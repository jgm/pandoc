{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{- |
   Module      : Text.Pandoc.Writers.Org
   Copyright   : © 2010-2015 Puneeth Chaganti <punchagan@gmail.com>
                   2010-2020 John MacFarlane <jgm@berkeley.edu>
                   2016-2020 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Emacs Org-Mode.

Org-Mode:  <http://orgmode.org>
-}
module Text.Pandoc.Writers.Org (writeOrg) where
import Control.Monad.State.Strict
import Data.Char (isAlphaNum)
import Data.List (intersect, intersperse, partition, transpose)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.DocLayout
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Shared

data WriterState =
  WriterState { stNotes   :: [[Block]]
              , stHasMath :: Bool
              , stOptions :: WriterOptions
              }

type Org = StateT WriterState

-- | Convert Pandoc to Org.
writeOrg :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeOrg opts document = do
  let st = WriterState { stNotes = [],
                         stHasMath = False,
                         stOptions = opts }
  evalStateT (pandocToOrg document) st

-- | Return Org representation of document.
pandocToOrg :: PandocMonad m => Pandoc -> Org m Text
pandocToOrg (Pandoc meta blocks) = do
  opts <- gets stOptions
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  metadata <- metaToContext opts
               blockListToOrg
               (fmap chomp . inlineListToOrg)
               meta
  body <- blockListToOrg blocks
  notes <- gets (reverse . stNotes) >>= notesToOrg
  hasMath <- gets stHasMath
  let main = body $+$ notes
  let context = defField "body" main
              . defField "math" hasMath
              $ metadata
  return $ render colwidth $
    case writerTemplate opts of
       Nothing  -> main
       Just tpl -> renderTemplate tpl context

-- | Return Org representation of notes.
notesToOrg :: PandocMonad m => [[Block]] -> Org m (Doc Text)
notesToOrg notes =
  vsep <$> zipWithM noteToOrg [1..] notes

-- | Return Org representation of a note.
noteToOrg :: PandocMonad m => Int -> [Block] -> Org m (Doc Text)
noteToOrg num note = do
  contents <- blockListToOrg note
  let marker = "[fn:" ++ show num ++ "] "
  return $ hang (length marker) (text marker) contents

-- | Escape special characters for Org.
escapeString :: Text -> Text
escapeString = escapeStringUsing
               [ ('\x2014',"---")
               , ('\x2013',"--")
               , ('\x2019',"'")
               , ('\x2026',"...")
               ]

isRawFormat :: Format -> Bool
isRawFormat f =
  f == Format "latex" || f == Format "tex" || f == Format "org"

-- | Convert Pandoc block element to Org.
blockToOrg :: PandocMonad m
           => Block         -- ^ Block element
           -> Org m (Doc Text)
blockToOrg Null = return empty
blockToOrg (Div attr bs) = divToOrg attr bs
blockToOrg (Plain inlines) = inlineListToOrg inlines
-- title beginning with fig: indicates that the image is a figure
blockToOrg (Para [Image attr txt (src,tgt)])
  | Just tit <- T.stripPrefix "fig:" tgt = do
      capt <- if null txt
              then return empty
              else ("#+CAPTION: " <>) `fmap` inlineListToOrg txt
      img <- inlineToOrg (Image attr txt (src,tit))
      return $ capt $$ img $$ blankline
blockToOrg (Para inlines) = do
  contents <- inlineListToOrg inlines
  return $ contents <> blankline
blockToOrg (LineBlock lns) = do
  let splitStanza [] = []
      splitStanza xs = case break (== mempty) xs of
        (l, [])  -> [l]
        (l, _:r) -> l : splitStanza r
  let joinWithLinefeeds  = nowrap . mconcat . intersperse cr
  let joinWithBlankLines = mconcat . intersperse blankline
  let prettifyStanza ls  = joinWithLinefeeds <$> mapM inlineListToOrg ls
  contents <- joinWithBlankLines <$> mapM prettifyStanza (splitStanza lns)
  return $ blankline $$ "#+BEGIN_VERSE" $$
           nest 2 contents $$ "#+END_VERSE" <> blankline
blockToOrg (RawBlock "html" str) =
  return $ blankline $$ "#+BEGIN_HTML" $$
           nest 2 (literal str) $$ "#+END_HTML" $$ blankline
blockToOrg b@(RawBlock f str)
  | isRawFormat f = return $ literal str
  | otherwise     = do
      report $ BlockNotRendered b
      return empty
blockToOrg HorizontalRule = return $ blankline $$ "--------------" $$ blankline
blockToOrg (Header level attr inlines) = do
  contents <- inlineListToOrg inlines
  let headerStr = text $ if level > 999 then " " else replicate level '*'
  let drawerStr = if attr == nullAttr
                  then empty
                  else cr <> nest (level + 1) (propertiesDrawer attr)
  return $ headerStr <> " " <> contents <> drawerStr <> blankline
blockToOrg (CodeBlock (_,classes,kvs) str) = do
  let startnum = maybe "" (\x -> " " <> trimr x) $ lookup "startFrom" kvs
  let numberlines = if "numberLines" `elem` classes
                      then if "continuedSourceBlock" `elem` classes
                             then " +n" <> startnum
                             else " -n" <> startnum
                      else ""
  let at = map pandocLangToOrg classes `intersect` orgLangIdentifiers
  let (beg, end) = case at of
                      []    -> ("#+BEGIN_EXAMPLE" <> numberlines, "#+END_EXAMPLE")
                      (x:_) -> ("#+BEGIN_SRC " <> x <> numberlines, "#+END_SRC")
  return $ literal beg $$ nest 2 (literal str) $$ text end $$ blankline
blockToOrg (BlockQuote blocks) = do
  contents <- blockListToOrg blocks
  return $ blankline $$ "#+BEGIN_QUOTE" $$
           nest 2 contents $$ "#+END_QUOTE" $$ blankline
blockToOrg (Table _ blkCapt specs thead tbody tfoot) =  do
  let (caption', _, _, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
  caption'' <- inlineListToOrg caption'
  let caption = if null caption'
                   then empty
                   else "#+CAPTION: " <> caption''
  headers' <- mapM blockListToOrg headers
  rawRows <- mapM (mapM blockListToOrg) rows
  let numChars = maximum . map offset
  -- FIXME: width is not being used.
  let widthsInChars =
       map numChars $ transpose (headers' : rawRows)
  -- FIXME: Org doesn't allow blocks with height more than 1.
  let hpipeBlocks blocks = hcat [beg, middle, end]
        where sep'   = vfill " | "
              beg    = vfill "| "
              end    = vfill " |"
              middle = hcat $ intersperse sep' blocks
  let makeRow = hpipeBlocks . zipWith lblock widthsInChars
  let head' = makeRow headers'
  rows' <- mapM (\row -> do cols <- mapM blockListToOrg row
                            return $ makeRow cols) rows
  let border ch = char '|' <> char ch <>
                  (hcat . intersperse (char ch <> char '+' <> char ch) $
                          map (\l -> text $ replicate l ch) widthsInChars) <>
                  char ch <> char '|'
  let body = vcat rows'
  let head'' = if all null headers
                  then empty
                  else head' $$ border '-'
  return $ head'' $$ body $$ caption $$ blankline
blockToOrg (BulletList items) = do
  contents <- mapM bulletListItemToOrg items
  -- ensure that sublists have preceding blank line
  return $ blankline $$
           (if isTightList items then vcat else vsep) contents $$
           blankline
blockToOrg (OrderedList (start, _, delim) items) = do
  let delim' = case delim of
                    TwoParens -> OneParen
                    x         -> x
  let markers = take (length items) $ orderedListMarkers
                                      (start, Decimal, delim')
  let maxMarkerLength = maximum $ map T.length markers
  let markers' = map (\m -> let s = maxMarkerLength - T.length m
                            in  m <> T.replicate s " ") markers
  contents <- zipWithM orderedListItemToOrg markers' items
  -- ensure that sublists have preceding blank line
  return $ blankline $$
           (if isTightList items then vcat else vsep) contents $$
           blankline
blockToOrg (DefinitionList items) = do
  contents <- mapM definitionListItemToOrg items
  return $ vcat contents $$ blankline

-- | Convert bullet list item (list of blocks) to Org.
bulletListItemToOrg :: PandocMonad m => [Block] -> Org m (Doc Text)
bulletListItemToOrg items = do
  contents <- blockListToOrg items
  return $ hang 2 "- " contents $$
          if endsWithPlain items
             then cr
             else blankline


-- | Convert ordered list item (a list of blocks) to Org.
orderedListItemToOrg :: PandocMonad m
                     => Text   -- ^ marker for list item
                     -> [Block]  -- ^ list item (list of blocks)
                     -> Org m (Doc Text)
orderedListItemToOrg marker items = do
  contents <- blockListToOrg items
  return $ hang (T.length marker + 1) (literal marker <> space) contents $$
          if endsWithPlain items
             then cr
             else blankline

-- | Convert definition list item (label, list of blocks) to Org.
definitionListItemToOrg :: PandocMonad m
                        => ([Inline], [[Block]]) -> Org m (Doc Text)
definitionListItemToOrg (label, defs) = do
  label' <- inlineListToOrg label
  contents <- vcat <$> mapM blockListToOrg defs
  return $ hang 2 "- " (label' <> " :: " <> contents) $$
      if isTightList defs
         then cr
         else blankline

-- | Convert list of key/value pairs to Org :PROPERTIES: drawer.
propertiesDrawer :: Attr -> Doc Text
propertiesDrawer (ident, classes, kv) =
  let
    drawerStart = text ":PROPERTIES:"
    drawerEnd   = text ":END:"
    kv'  = if classes == mempty then kv  else ("CLASS", T.unwords classes):kv
    kv'' = if ident == mempty   then kv' else ("CUSTOM_ID", ident):kv'
    properties = vcat $ map kvToOrgProperty kv''
  in
    drawerStart <> cr <> properties <> cr <> drawerEnd
 where
   kvToOrgProperty :: (Text, Text) -> Doc Text
   kvToOrgProperty (key, value) =
     text ":" <> literal key <> text ": " <> literal value <> cr

-- | The different methods to represent a Div block.
data DivBlockType
  = GreaterBlock Text Attr   -- ^ Greater block like @center@ or @quote@.
  | Drawer Text Attr         -- ^ Org drawer with of given name; keeps
                             --   key-value pairs.
  | UnwrappedWithAnchor Text -- ^ Not mapped to other type, only
                             --   identifier is retained (if any).

-- | Gives the most suitable method to render a list of blocks
-- with attributes.
divBlockType :: Attr-> DivBlockType
divBlockType (ident, classes, kvs)
  -- if any class is named "drawer", then output as org :drawer:
  | ([_], drawerName:classes') <- partition (== "drawer") classes
  = Drawer drawerName (ident, classes', kvs)
  -- if any class is either @center@ or @quote@, then use a org block.
  | (blockName:classes'', classes') <- partition isGreaterBlockClass classes
  = GreaterBlock blockName (ident, classes' <> classes'', kvs)
  -- if no better method is found, unwrap div and set anchor
  | otherwise
  = UnwrappedWithAnchor ident
 where
  isGreaterBlockClass :: Text -> Bool
  isGreaterBlockClass = (`elem` ["center", "quote"]) . T.toLower

-- | Converts a Div to an org-mode element.
divToOrg :: PandocMonad m
         => Attr -> [Block] -> Org m (Doc Text)
divToOrg attr bs = do
  contents <- blockListToOrg bs
  case divBlockType attr of
    GreaterBlock blockName attr' ->
      -- Write as greater block. The ID, if present, is added via
      -- the #+NAME keyword; other classes and key-value pairs
      -- are kept as #+ATTR_HTML attributes.
      return $ blankline $$ attrHtml attr'
            $$ "#+BEGIN_" <> literal blockName
            $$ contents
            $$ "#+END_" <> literal blockName $$ blankline
    Drawer drawerName (_,_,kvs) -> do
      -- Write as drawer. Only key-value pairs are retained.
      let keys = vcat $ map (\(k,v) ->
                               ":" <> literal k <> ":"
                              <> space <> literal v) kvs
      return $ ":" <> literal drawerName <> ":" $$ cr
            $$ keys $$ blankline
            $$ contents $$ blankline
            $$ text ":END:" $$ blankline
    UnwrappedWithAnchor ident -> do
      -- Unwrap the div. All attributes are discarded, except for
      -- the identifier, which is added as an anchor before the
      -- div contents.
      let contents' = if T.null ident
                      then contents
                      else  "<<" <> literal ident <> ">>" $$ contents
      return (blankline $$ contents' $$ blankline)

attrHtml :: Attr -> Doc Text
attrHtml (""   , []     , []) = mempty
attrHtml (ident, classes, kvs) =
  let
    name = if T.null ident then mempty else "#+NAME: " <> literal ident <> cr
    keyword = "#+ATTR_HTML"
    classKv = ("class", T.unwords classes)
    kvStrings = map (\(k,v) -> ":" <> k <> " " <> v) (classKv:kvs)
  in name <> keyword <> ": " <> literal (T.unwords kvStrings) <> cr

-- | Convert list of Pandoc block elements to Org.
blockListToOrg :: PandocMonad m
               => [Block]       -- ^ List of block elements
               -> Org m (Doc Text)
blockListToOrg blocks = vcat <$> mapM blockToOrg blocks

-- | Convert list of Pandoc inline elements to Org.
inlineListToOrg :: PandocMonad m
                => [Inline]
                -> Org m (Doc Text)
inlineListToOrg lst = hcat <$> mapM inlineToOrg (fixMarkers lst)
  where fixMarkers [] = []  -- prevent note refs and list markers from wrapping, see #4171
        fixMarkers (Space : x : rest) | shouldFix x =
          Str " " : x : fixMarkers rest
        fixMarkers (SoftBreak : x : rest) | shouldFix x =
          Str " " : x : fixMarkers rest
        fixMarkers (x : rest) = x : fixMarkers rest

        shouldFix Note{} = True -- Prevent footnotes
        shouldFix (Str "-") = True -- Prevent bullet list items
        -- TODO: prevent ordered list items
        shouldFix _ = False

-- | Convert Pandoc inline element to Org.
inlineToOrg :: PandocMonad m => Inline -> Org m (Doc Text)
inlineToOrg (Span (uid, [], []) []) =
  return $ "<<" <> literal uid <> ">>"
inlineToOrg (Span _ lst) =
  inlineListToOrg lst
inlineToOrg (Emph lst) = do
  contents <- inlineListToOrg lst
  return $ "/" <> contents <> "/"
inlineToOrg (Underline lst) = do
  contents <- inlineListToOrg lst
  return $ "_" <> contents <> "_"
inlineToOrg (Strong lst) = do
  contents <- inlineListToOrg lst
  return $ "*" <> contents <> "*"
inlineToOrg (Strikeout lst) = do
  contents <- inlineListToOrg lst
  return $ "+" <> contents <> "+"
inlineToOrg (Superscript lst) = do
  contents <- inlineListToOrg lst
  return $ "^{" <> contents <> "}"
inlineToOrg (Subscript lst) = do
  contents <- inlineListToOrg lst
  return $ "_{" <> contents <> "}"
inlineToOrg (SmallCaps lst) = inlineListToOrg lst
inlineToOrg (Quoted SingleQuote lst) = do
  contents <- inlineListToOrg lst
  return $ "'" <> contents <> "'"
inlineToOrg (Quoted DoubleQuote lst) = do
  contents <- inlineListToOrg lst
  return $ "\"" <> contents <> "\""
inlineToOrg (Cite _  lst) = inlineListToOrg lst
inlineToOrg (Code _ str) = return $ "=" <> literal str <> "="
inlineToOrg (Str str) = return . literal $ escapeString str
inlineToOrg (Math t str) = do
  modify $ \st -> st{ stHasMath = True }
  return $ if t == InlineMath
              then "$" <> literal str <> "$"
              else "$$" <> literal str <> "$$"
inlineToOrg il@(RawInline f str)
  | isRawFormat f = return $ literal str
  | otherwise     = do
      report $ InlineNotRendered il
      return empty
inlineToOrg LineBreak = return (text "\\\\" <> cr)
inlineToOrg Space = return space
inlineToOrg SoftBreak = do
  wrapText <- gets (writerWrapText . stOptions)
  case wrapText of
       WrapPreserve -> return cr
       WrapAuto     -> return space
       WrapNone     -> return space
inlineToOrg (Link _ txt (src, _)) =
  case txt of
        [Str x] | escapeURI x == src ->  -- autolink
             return $ "[[" <> literal (orgPath x) <> "]]"
        _ -> do contents <- inlineListToOrg txt
                return $ "[[" <> literal (orgPath src) <> "][" <> contents <> "]]"
inlineToOrg (Image _ _ (source, _)) =
  return $ "[[" <> literal (orgPath source) <> "]]"
inlineToOrg (Note contents) = do
  -- add to notes in state
  notes <- gets stNotes
  modify $ \st -> st { stNotes = contents:notes }
  let ref = tshow $ length notes + 1
  return $ "[fn:" <> literal ref <> "]"

orgPath :: Text -> Text
orgPath src = case T.uncons src of
  Nothing            -> ""             -- wiki link
  Just ('#', _)      -> src            -- internal link
  _ | isUrl src      -> src
  _ | isFilePath src -> src
  _                  -> "file:" <> src
  where
    isFilePath :: Text -> Bool
    isFilePath cs = any (`T.isPrefixOf` cs) ["/", "./", "../", "file:"]

    isUrl :: Text -> Bool
    isUrl cs =
      let (scheme, path) = T.break (== ':') cs
      in T.all (\c -> isAlphaNum c || c `elemText` ".-") scheme
         && not (T.null path)

-- | Translate from pandoc's programming language identifiers to those used by
-- org-mode.
pandocLangToOrg :: Text -> Text
pandocLangToOrg cs =
  case cs of
    "c"          -> "C"
    "cpp"        -> "C++"
    "commonlisp" -> "lisp"
    "r"          -> "R"
    "bash"       -> "sh"
    _            -> cs

-- | List of language identifiers recognized by org-mode.
orgLangIdentifiers :: [Text]
orgLangIdentifiers =
  [ "asymptote", "awk", "C", "C++", "clojure", "css", "d", "ditaa", "dot"
  , "calc", "emacs-lisp", "fortran", "gnuplot", "haskell", "java", "js"
  , "latex", "ledger", "lisp", "lilypond", "matlab", "mscgen", "ocaml"
  , "octave", "org", "oz", "perl", "plantuml", "processing", "python", "R"
  , "ruby", "sass", "scheme", "screen", "sed", "sh", "sql", "sqlite"
  ]
