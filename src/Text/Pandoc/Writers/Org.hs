{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{- |
   Module      : Text.Pandoc.Writers.Org
   Copyright   : © 2010-2015 Puneeth Chaganti <punchagan@gmail.com>
                   2010-2022 John MacFarlane <jgm@berkeley.edu>
                   2016-2022 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Emacs Org-Mode.

Org-Mode:  <http://orgmode.org>
-}
module Text.Pandoc.Writers.Org (writeOrg) where
import Control.Monad.State.Strict
import Data.Char (isAlphaNum, isDigit)
import Data.List (intersect, intersperse, partition, transpose)
import Data.List.NonEmpty (nonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.DocLayout
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Citeproc.Locator (parseLocator, LocatorMap(..), LocatorInfo(..))
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
escapeString t
  | T.all (\c -> c < '\x2013' || c > '\x2026') t = t
  | otherwise = T.concatMap escChar t
  where
   escChar '\x2013' = "--"
   escChar '\x2014' = "---"
   escChar '\x2019' = "'"
   escChar '\x2026' = "..."
   escChar c        = T.singleton c

isRawFormat :: Format -> Bool
isRawFormat f =
  f == Format "latex" || f == Format "tex" || f == Format "org"

-- | Convert Pandoc block element to Org.
blockToOrg :: PandocMonad m
           => Block         -- ^ Block element
           -> Org m (Doc Text)
blockToOrg Null = return empty
blockToOrg (Div attr@(ident,_,_) bs) = do
  opts <- gets stOptions
  -- Strip off bibliography if citations enabled
  if ident == "refs" && isEnabled Ext_citations opts
     then return mempty
     else divToOrg attr bs
blockToOrg (Plain inlines) = inlineListToOrg inlines
blockToOrg (SimpleFigure attr txt (src, tit)) = do
      capt <- if null txt
              then return empty
              else ("#+caption: " <>) `fmap` inlineListToOrg txt
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
  return $ blankline $$ "#+begin_verse" $$
           nest 2 contents $$ "#+end_verse" <> blankline
blockToOrg (RawBlock "html" str) =
  return $ blankline $$ "#+begin_html" $$
           nest 2 (literal str) $$ "#+end_html" $$ blankline
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
  return $ headerStr <> " " <> contents <> drawerStr <> cr
blockToOrg (CodeBlock (_,classes,kvs) str) = do
  let startnum = maybe "" (\x -> " " <> trimr x) $ lookup "startFrom" kvs
  let numberlines = if "numberLines" `elem` classes
                      then if "continuedSourceBlock" `elem` classes
                             then " +n" <> startnum
                             else " -n" <> startnum
                      else ""
  let at = map pandocLangToOrg classes `intersect` orgLangIdentifiers
  let (beg, end) = case at of
                      []    -> ("#+begin_example" <> numberlines, "#+end_example")
                      (x:_) -> ("#+begin_src " <> x <> numberlines, "#+end_src")
  return $ literal beg $$ literal str $$ text end $$ blankline
blockToOrg (BlockQuote blocks) = do
  contents <- blockListToOrg blocks
  return $ blankline $$ "#+begin_quote" $$
           nest 2 contents $$ "#+end_quote" $$ blankline
blockToOrg (Table _ blkCapt specs thead tbody tfoot) =  do
  let (caption', _, _, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
  caption'' <- inlineListToOrg caption'
  let caption = if null caption'
                   then empty
                   else "#+caption: " <> caption''
  headers' <- mapM blockListToOrg headers
  rawRows <- mapM (mapM blockListToOrg) rows
  let numChars = maybe 0 maximum . nonEmpty . map offset
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
  return $ (if isTightList items then vcat else vsep) contents $$
           blankline
blockToOrg (OrderedList (start, _, delim) items) = do
  let delim' = case delim of
                    TwoParens -> OneParen
                    x         -> x
  let markers = take (length items) $ orderedListMarkers
                                      (start, Decimal, delim')
      counters = (case start of 1 -> Nothing; n -> Just n) : repeat Nothing
  contents <- zipWithM (\x f -> f x) items $
              zipWith orderedListItemToOrg markers counters
  return $ (if isTightList items then vcat else vsep) contents $$
           blankline
blockToOrg (DefinitionList items) = do
  contents <- mapM definitionListItemToOrg items
  return $ vcat contents $$ blankline

-- | Convert bullet list item (list of blocks) to Org.
bulletListItemToOrg :: PandocMonad m => [Block] -> Org m (Doc Text)
bulletListItemToOrg items = do
  exts <- gets $ writerExtensions . stOptions
  contents <- blockListToOrg (taskListItemToOrg exts items)
  -- if list item starts with non-paragraph, it must go on
  -- the next line:
  let contents' = (case items of
                    Plain{}:_ -> mempty
                    Para{}:_ -> mempty
                    _ -> cr) <> chomp contents
  return $ hang 2 "- " contents' $$
          if null items || endsWithPlain items
             then cr
             else blankline

-- | Convert ordered list item (a list of blocks) to Org.
orderedListItemToOrg :: PandocMonad m
                     => Text   -- ^ marker for list item
                     -> Maybe Int -- ^ maybe number for a counter cookie
                     -> [Block]  -- ^ list item (list of blocks)
                     -> Org m (Doc Text)
orderedListItemToOrg marker counter items = do
  exts <- gets $ writerExtensions . stOptions
  contents <- blockListToOrg (taskListItemToOrg exts items)
  -- if list item starts with non-paragraph, it must go on
  -- the next line:
  let contents' = (case items of
                    Plain{}:_ -> mempty
                    Para{}:_ -> mempty
                    _ -> cr) <> chomp contents
  let cookie = maybe empty
               (\n -> space <> literal "[@" <> literal (tshow n) <> literal "]")
               counter
  return $ hang (T.length marker + 1)
                (literal marker <> cookie <> space) contents' $$
          if null items || endsWithPlain items
             then cr
             else blankline

-- | Convert a list item containing text starting with @U+2610 BALLOT BOX@
-- or @U+2612 BALLOT BOX WITH X@ to org checkbox syntax (e.g. @[X]@).
taskListItemToOrg :: Extensions -> [Block] -> [Block]
taskListItemToOrg = handleTaskListItem toOrg
  where
    toOrg (Str "☐" : Space : is) = Str "[ ]" : Space : is
    toOrg (Str "☒" : Space : is) = Str "[X]" : Space : is
    toOrg is = is

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
      -- the #+name keyword; other classes and key-value pairs
      -- are kept as #+attr_html attributes.
      return $ blankline $$ attrHtml attr'
            $$ "#+begin_" <> literal blockName
            $$ contents
            $$ "#+end_" <> literal blockName $$ blankline
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
    name = if T.null ident then mempty else "#+name: " <> literal ident <> cr
    keyword = "#+attr_html"
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
  where -- Prevent note refs and list markers from wrapping, see #4171
        -- and #7132.
        fixMarkers [] = []
        fixMarkers (Space : x : rest) | shouldFix x =
          Str " " : x : fixMarkers rest
        fixMarkers (SoftBreak : x : rest) | shouldFix x =
          Str " " : x : fixMarkers rest
        fixMarkers (x : rest) = x : fixMarkers rest

        shouldFix Note{} = True    -- Prevent footnotes
        shouldFix (Str "-") = True -- Prevent bullet list items
        shouldFix (Str x)          -- Prevent ordered list items
          | Just (cs, c) <- T.unsnoc x = T.all isDigit cs &&
                                         (c == '.' || c == ')')
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
inlineToOrg (Cite cs lst) = do
  opts <- gets stOptions
  if isEnabled Ext_citations opts
     then do
       let renderCiteItem c = do
             citePref <- inlineListToOrg (citationPrefix c)
             let (locinfo, suffix) = parseLocator locmap (citationSuffix c)
             citeSuff <- inlineListToOrg suffix
             let locator = case locinfo of
                            Just info -> literal $
                              T.replace "\160" " " $
                              T.replace "{" "" $
                              T.replace "}" "" $ locatorRaw info
                            Nothing -> mempty
             return $ hsep [ citePref
                           , ("@" <> literal (citationId c))
                           , locator
                           , citeSuff ]
       citeItems <- mconcat . intersperse "; " <$> mapM renderCiteItem cs
       let sty = case cs of
                   (d:_)
                     | citationMode d == AuthorInText
                     -> literal "/t"
                   [d]
                     | citationMode d == SuppressAuthor
                     -> literal "/na"
                   _ -> mempty
       return $ "[cite" <> sty <> ":" <> citeItems <> "]"
     else inlineListToOrg lst
inlineToOrg (Code _ str) = return $ "=" <> literal str <> "="
inlineToOrg (Str str) = return . literal $ escapeString str
inlineToOrg (Math t str) = do
  modify $ \st -> st{ stHasMath = True }
  return $ if t == InlineMath
              then "\\(" <> literal str <> "\\)"
              else "\\[" <> literal str <> "\\]"
inlineToOrg il@(RawInline f str)
  | elem f ["tex", "latex"] && T.isPrefixOf "\\begin" str =
    return $ cr <> literal str <> cr
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
    "commonlisp" -> "lisp"
    "r"          -> "R"
    "bash"       -> "sh"
    _            -> cs

-- | List of language identifiers recognized by org-mode.
-- See <https://orgmode.org/manual/Languages.html>.
orgLangIdentifiers :: [Text]
orgLangIdentifiers =
  [ "asymptote"
  , "lisp"
  , "awk"
  , "lua"
  , "C"
  , "matlab"
  , "C++"
  , "mscgen"
  , "clojure"
  , "ocaml"
  , "css"
  , "octave"
  , "D"
  , "org"
  , "ditaa"
  , "oz"
  , "calc"
  , "perl"
  , "emacs-lisp"
  , "plantuml"
  , "eshell"
  , "processing"
  , "fortran"
  , "python"
  , "gnuplot"
  , "R"
  , "screen"
  , "ruby"
  , "dot"
  , "sass"
  , "haskell"
  , "scheme"
  , "java"
  , "sed"
  , "js"
  , "sh"
  , "latex"
  , "sql"
  , "ledger"
  , "sqlite"
  , "lilypond"
  , "vala" ]

-- taken from oc-csl.el in the org source tree:
locmap :: LocatorMap
locmap = LocatorMap $ M.fromList
  [ ("bk."       , "book")
  , ("bks."      , "book")
  , ("book"      , "book")
  , ("chap."     , "chapter")
  , ("chaps."    , "chapter")
  , ("chapter"   , "chapter")
  , ("col."      , "column")
  , ("cols."     , "column")
  , ("column"    , "column")
  , ("figure"    , "figure")
  , ("fig."      , "figure")
  , ("figs."     , "figure")
  , ("folio"     , "folio")
  , ("fol."      , "folio")
  , ("fols."     , "folio")
  , ("number"    , "number")
  , ("no."       , "number")
  , ("nos."      , "number")
  , ("line"      , "line")
  , ("l."        , "line")
  , ("ll."       , "line")
  , ("note"      , "note")
  , ("n."        , "note")
  , ("nn."       , "note")
  , ("opus"      , "opus")
  , ("op."       , "opus")
  , ("opp."      , "opus")
  , ("page"      , "page")
  , ("p"         , "page")
  , ("p."        , "page")
  , ("pp."       , "page")
  , ("paragraph" , "paragraph")
  , ("para."     , "paragraph")
  , ("paras."    , "paragraph")
  , ("¶"         , "paragraph")
  , ("¶¶"        , "paragraph")
  , ("part"      , "part")
  , ("pt."       , "part")
  , ("pts."      , "part")
  , ("§"         , "section")
  , ("§§"        , "section")
  , ("section"   , "section")
  , ("sec."      , "section")
  , ("secs."     , "section")
  , ("sub verbo" , "sub verbo")
  , ("s.v."      , "sub verbo")
  , ("s.vv."     , "sub verbo")
  , ("verse"     , "verse")
  , ("v."        , "verse")
  , ("vv."       , "verse")
  , ("volume"    , "volume")
  , ("vol."      , "volume")
  , ("vols."     , "volume") ]
