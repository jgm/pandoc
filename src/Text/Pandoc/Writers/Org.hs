{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2010-2015 Puneeth Chaganti <punchagan@gmail.com>
                        Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>,
                        and John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Writers.Org
   Copyright   : Copyright (C) 2010-2015 Puneeth Chaganti and John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Puneeth Chaganti <punchagan@gmail.com>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Emacs Org-Mode.

Org-Mode:  <http://orgmode.org>
-}
module Text.Pandoc.Writers.Org ( writeOrg) where
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Pretty
import Text.Pandoc.Templates (renderTemplate')
import Data.Char ( isAlphaNum, toLower )
import Data.List ( isPrefixOf, intersect, intersperse, partition, transpose )
import Control.Monad.State

data WriterState =
  WriterState { stNotes     :: [[Block]]
              , stLinks     :: Bool
              , stImages    :: Bool
              , stHasMath   :: Bool
              , stOptions   :: WriterOptions
              }

-- | Convert Pandoc to Org.
writeOrg :: WriterOptions -> Pandoc -> String
writeOrg opts document =
  let st = WriterState { stNotes = [], stLinks = False,
                         stImages = False, stHasMath = False,
                         stOptions = opts }
  in evalState (pandocToOrg document) st

-- | Return Org representation of document.
pandocToOrg :: Pandoc -> State WriterState String
pandocToOrg (Pandoc meta blocks) = do
  opts <- liftM stOptions get
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  metadata <- metaToJSON opts
               (fmap (render colwidth) . blockListToOrg)
               (fmap (render colwidth) . inlineListToOrg)
               meta
  body <- blockListToOrg blocks
  notes <- liftM (reverse . stNotes) get >>= notesToOrg
  -- note that the notes may contain refs, so we do them first
  hasMath <- liftM stHasMath get
  let main = render colwidth $ foldl ($+$) empty $ [body, notes]
  let context = defField "body" main
              $ defField "math" hasMath
              $ metadata
  if writerStandalone opts
     then return $ renderTemplate' (writerTemplate opts) context
     else return main

-- | Return Org representation of notes.
notesToOrg :: [[Block]] -> State WriterState Doc
notesToOrg notes =
  mapM (\(num, note) -> noteToOrg num note) (zip [1..] notes) >>=
  return . vsep

-- | Return Org representation of a note.
noteToOrg :: Int -> [Block] -> State WriterState Doc
noteToOrg num note = do
  contents <- blockListToOrg note
  let marker = "[" ++ show num ++ "] "
  return $ hang (length marker) (text marker) contents

-- | Escape special characters for Org.
escapeString :: String -> String
escapeString = escapeStringUsing $
               [ ('\x2014',"---")
               , ('\x2013',"--")
               , ('\x2019',"'")
               , ('\x2026',"...")
               ] ++ backslashEscapes "^_"

isRawFormat :: Format -> Bool
isRawFormat f =
  f == Format "latex" || f == Format "tex" || f == Format "org"

-- | Convert Pandoc block element to Org.
blockToOrg :: Block         -- ^ Block element
           -> State WriterState Doc
blockToOrg Null = return empty
blockToOrg (Div (_,classes@(cls:_),kvs) bs) | "drawer" `elem` classes = do
  contents <- blockListToOrg bs
  let drawerNameTag = ":" <> text cls <> ":"
  let keys = vcat $ map (\(k,v) ->
                       ":" <> text k <> ":"
                       <> space <> text v) kvs
  let drawerEndTag = text ":END:"
  return $ drawerNameTag $$ cr $$ keys $$
           blankline $$ contents $$
           blankline $$ drawerEndTag $$
           blankline
blockToOrg (Div attrs bs) = do
  contents <- blockListToOrg bs
  let isGreaterBlockClass = (`elem` ["center", "quote"]) . map toLower
  return $ case attrs of
    ("", [], []) ->
      -- nullAttr, treat contents as if it wasn't wrapped
      blankline $$ contents $$ blankline
    (ident, [], []) ->
      -- only an id: add id as an anchor, unwrap the rest
      blankline $$ "<<" <> text ident <> ">>" $$ contents $$ blankline
    (ident, classes, kv) ->
      -- if one class looks like the name of a greater block then output as
      -- such: The ID, if present, is added via the #+NAME keyword; other
      -- classes and key-value pairs are kept as #+ATTR_HTML attributes.
      let
        (blockTypeCand, classes') = partition isGreaterBlockClass classes
      in case blockTypeCand of
        (blockType:classes'') ->
          blankline $$ attrHtml (ident, classes'' <> classes', kv) $$
          "#+BEGIN_" <> text blockType $$ contents $$
          "#+END_" <> text blockType $$ blankline
        _                     ->
          -- fallback: wrap in div tags
          let
            startTag = tagWithAttrs "div" attrs
            endTag = text "</div>"
          in blankline $$ "#+BEGIN_HTML" $$
             nest 2 startTag $$ "#+END_HTML" $$ blankline $$
             contents $$ blankline $$ "#+BEGIN_HTML" $$
             nest 2 endTag $$ "#+END_HTML" $$ blankline
blockToOrg (Plain inlines) = inlineListToOrg inlines
-- title beginning with fig: indicates that the image is a figure
blockToOrg (Para [Image attr txt (src,'f':'i':'g':':':tit)]) = do
  capt <- if null txt
             then return empty
             else ("#+CAPTION: " <>) `fmap` inlineListToOrg txt
  img <- inlineToOrg (Image attr txt (src,tit))
  return $ capt $$ img $$ blankline
blockToOrg (Para inlines) = do
  contents <- inlineListToOrg inlines
  return $ contents <> blankline
blockToOrg (RawBlock "html" str) =
  return $ blankline $$ "#+BEGIN_HTML" $$
           nest 2 (text str) $$ "#+END_HTML" $$ blankline
blockToOrg (RawBlock f str) | isRawFormat f =
  return $ text str
blockToOrg (RawBlock _ _) = return empty
blockToOrg HorizontalRule = return $ blankline $$ "--------------" $$ blankline
blockToOrg (Header level attr inlines) = do
  contents <- inlineListToOrg inlines
  let headerStr = text $ if level > 999 then " " else replicate level '*'
  let drawerStr = if attr == nullAttr
                  then empty
                  else cr <> nest (level + 1) (propertiesDrawer attr)
  return $ headerStr <> " " <> contents <> drawerStr <> blankline
blockToOrg (CodeBlock (_,classes,_) str) = do
  opts <- stOptions <$> get
  let tabstop = writerTabStop opts
  let at = map pandocLangToOrg classes `intersect` orgLangIdentifiers
  let (beg, end) = case at of
                      []    -> ("#+BEGIN_EXAMPLE", "#+END_EXAMPLE")
                      (x:_) -> ("#+BEGIN_SRC " ++ x, "#+END_SRC")
  return $ text beg $$ nest tabstop (text str) $$ text end $$ blankline
blockToOrg (BlockQuote blocks) = do
  contents <- blockListToOrg blocks
  return $ blankline $$ "#+BEGIN_QUOTE" $$
           nest 2 contents $$ "#+END_QUOTE" $$ blankline
blockToOrg (Table caption' _ _ headers rows) =  do
  caption'' <- inlineListToOrg caption'
  let caption = if null caption'
                   then empty
                   else ("#+CAPTION: " <> caption'')
  headers' <- mapM blockListToOrg headers
  rawRows <- mapM (mapM blockListToOrg) rows
  let numChars = maximum . map offset
  -- FIXME: width is not being used.
  let widthsInChars =
       map ((+2) . numChars) $ transpose (headers' : rawRows)
  -- FIXME: Org doesn't allow blocks with height more than 1.
  let hpipeBlocks blocks = hcat [beg, middle, end]
        where h      = maximum (1 : map height blocks)
              sep'   = lblock 3 $ vcat (map text $ replicate h " | ")
              beg    = lblock 2 $ vcat (map text $ replicate h "| ")
              end    = lblock 2 $ vcat (map text $ replicate h " |")
              middle = hcat $ intersperse sep' blocks
  let makeRow = hpipeBlocks . zipWith lblock widthsInChars
  let head' = makeRow headers'
  rows' <- mapM (\row -> do cols <- mapM blockListToOrg row
                            return $ makeRow cols) rows
  let border ch = char '|' <> char ch <>
                  (hcat $ intersperse (char ch <> char '+' <> char ch) $
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
  return $ blankline $+$ vcat contents $$ blankline
blockToOrg (OrderedList (start, _, delim) items) = do
  let delim' = case delim of
                    TwoParens -> OneParen
                    x         -> x
  let markers = take (length items) $ orderedListMarkers
                                      (start, Decimal, delim')
  let maxMarkerLength = maximum $ map length markers
  let markers' = map (\m -> let s = maxMarkerLength - length m
                            in  m ++ replicate s ' ') markers
  contents <- mapM (\(item, num) -> orderedListItemToOrg item num) $
              zip markers' items
  -- ensure that sublists have preceding blank line
  return $ blankline $$ vcat contents $$ blankline
blockToOrg (DefinitionList items) = do
  contents <- mapM definitionListItemToOrg items
  return $ vcat contents $$ blankline

-- | Convert bullet list item (list of blocks) to Org.
bulletListItemToOrg :: [Block] -> State WriterState Doc
bulletListItemToOrg items = do
  contents <- blockListToOrg items
  return $ hang 3 "-  " (contents <> cr)

-- | Convert ordered list item (a list of blocks) to Org.
orderedListItemToOrg :: String   -- ^ marker for list item
                     -> [Block]  -- ^ list item (list of blocks)
                     -> State WriterState Doc
orderedListItemToOrg marker items = do
  contents <- blockListToOrg items
  return $ hang (length marker + 1) (text marker <> space) (contents <> cr)

-- | Convert defintion list item (label, list of blocks) to Org.
definitionListItemToOrg :: ([Inline], [[Block]]) -> State WriterState Doc
definitionListItemToOrg (label, defs) = do
  label' <- inlineListToOrg label
  contents <- liftM vcat $ mapM blockListToOrg defs
  return $ hang 3 "-  " $ label' <> " :: " <> (contents <> cr)

-- | Convert list of key/value pairs to Org :PROPERTIES: drawer.
propertiesDrawer :: Attr -> Doc
propertiesDrawer (ident, classes, kv) =
  let
    drawerStart = text ":PROPERTIES:"
    drawerEnd   = text ":END:"
    kv'  = if (classes == mempty) then kv  else ("CLASS", unwords classes):kv
    kv'' = if (ident == mempty)   then kv' else ("CUSTOM_ID", ident):kv'
    properties = vcat $ map kvToOrgProperty kv''
  in
    drawerStart <> cr <> properties <> cr <> drawerEnd
 where
   kvToOrgProperty :: (String, String) -> Doc
   kvToOrgProperty (key, value) =
     text ":" <> text key <> text ": " <> text value <> cr

attrHtml :: Attr -> Doc
attrHtml (""   , []     , []) = mempty
attrHtml (ident, classes, kvs) =
  let
    name = if (null ident) then mempty else "#+NAME: " <> text ident <> cr
    keyword = "#+ATTR_HTML"
    classKv = ("class", unwords classes)
    kvStrings = map (\(k,v) -> ":" <> k <> " " <> v) (classKv:kvs)
  in name <> keyword <> ": " <> text (unwords kvStrings) <> cr

-- | Convert list of Pandoc block elements to Org.
blockListToOrg :: [Block]       -- ^ List of block elements
               -> State WriterState Doc
blockListToOrg blocks = mapM blockToOrg blocks >>= return . vcat

-- | Convert list of Pandoc inline elements to Org.
inlineListToOrg :: [Inline] -> State WriterState Doc
inlineListToOrg lst = mapM inlineToOrg lst >>= return . hcat

-- | Convert Pandoc inline element to Org.
inlineToOrg :: Inline -> State WriterState Doc
inlineToOrg (Span (uid, [], []) []) =
  return $ "<<" <> text uid <> ">>"
inlineToOrg (Span _ lst) =
  inlineListToOrg lst
inlineToOrg (Emph lst) = do
  contents <- inlineListToOrg lst
  return $ "/" <> contents <> "/"
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
inlineToOrg (Code _ str) = return $ "=" <> text str <> "="
inlineToOrg (Str str) = return $ text $ escapeString str
inlineToOrg (Math t str) = do
  modify $ \st -> st{ stHasMath = True }
  return $ if t == InlineMath
              then "$" <> text str <> "$"
              else "$$" <> text str <> "$$"
inlineToOrg (RawInline f@(Format f') str) =
  return $ if isRawFormat f
           then text str
           else "@@" <> text f' <> ":" <> text str <> "@@"
inlineToOrg (LineBreak) = return (text "\\\\" <> cr)
inlineToOrg Space = return space
inlineToOrg SoftBreak = do
  wrapText <- gets (writerWrapText . stOptions)
  case wrapText of
       WrapPreserve   -> return cr
       WrapAuto       -> return space
       WrapNone       -> return space
inlineToOrg (Link _ txt (src, _)) = do
  case txt of
        [Str x] | escapeURI x == src ->  -- autolink
             do modify $ \s -> s{ stLinks = True }
                return $ "[[" <> text (orgPath x) <> "]]"
        _ -> do contents <- inlineListToOrg txt
                modify $ \s -> s{ stLinks = True }
                return $ "[[" <> text (orgPath src) <> "][" <> contents <> "]]"
inlineToOrg (Image _ _ (source, _)) = do
  modify $ \s -> s{ stImages = True }
  return $ "[[" <> text (orgPath source) <> "]]"
inlineToOrg (Note contents) = do
  -- add to notes in state
  notes <- get >>= (return . stNotes)
  modify $ \st -> st { stNotes = contents:notes }
  let ref = show $ (length notes) + 1
  return $ " [" <> text ref <> "]"

orgPath :: String -> String
orgPath src =
  case src of
    []                 -> mempty         -- wiki link
    ('#':xs)           -> xs             -- internal link
    _ | isUrl src      -> src
    _ | isFilePath src -> src
    _                  -> "file:" <> src
 where
   isFilePath :: String -> Bool
   isFilePath cs = any (`isPrefixOf` cs) ["/", "./", "../", "file:"]

   isUrl :: String -> Bool
   isUrl cs =
     let (scheme, path) = break (== ':') cs
     in all (\c -> isAlphaNum c || c `elem` (".-"::String)) scheme
          && not (null path)

-- | Translate from pandoc's programming language identifiers to those used by
-- org-mode.
pandocLangToOrg :: String -> String
pandocLangToOrg cs =
  case cs of
    "c"          -> "C"
    "cpp"        -> "C++"
    "commonlisp" -> "lisp"
    "r"          -> "R"
    "bash"       -> "sh"
    _            -> cs

-- | List of language identifiers recognized by org-mode.
orgLangIdentifiers :: [String]
orgLangIdentifiers =
  [ "asymptote", "awk", "C", "C++", "clojure", "css", "d", "ditaa", "dot"
  , "calc", "emacs-lisp", "fortran", "gnuplot", "haskell", "java", "js"
  , "latex", "ledger", "lisp", "lilypond", "matlab", "mscgen", "ocaml"
  , "octave", "org", "oz", "perl", "plantuml", "processing", "python", "R"
  , "ruby", "sass", "scheme", "screen", "sed", "sh", "sql", "sqlite"
  ]
