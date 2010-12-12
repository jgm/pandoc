{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.Markdown 
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to markdown-formatted plain text.

Markdown:  <http://daringfireball.net/projects/markdown/>
-}
module Text.Pandoc.Writers.Markdown (writeMarkdown, writePlain) where
import Text.Pandoc.Definition
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Shared
import Text.Pandoc.Parsing
import Text.Pandoc.Blocks
import Text.ParserCombinators.Parsec ( runParser, GenParser )
import Data.List ( group, isPrefixOf, find, intersperse, transpose )
import Text.PrettyPrint.HughesPJ hiding ( Str )
import Control.Monad.State

type Notes = [[Block]]
type Refs = [([Inline], Target)]
data WriterState = WriterState { stNotes :: Notes
                               , stRefs :: Refs
                               , stPlain :: Bool }

-- | Convert Pandoc to Markdown.
writeMarkdown :: WriterOptions -> Pandoc -> String
writeMarkdown opts document = 
  evalState (pandocToMarkdown opts document) WriterState{ stNotes = []
                                                        , stRefs  = []
                                                        , stPlain = False }

-- | Convert Pandoc to plain text (like markdown, but without links,
-- pictures, or inline formatting).
writePlain :: WriterOptions -> Pandoc -> String
writePlain opts document =
  evalState (pandocToMarkdown opts document') WriterState{ stNotes = []
                                                         , stRefs  = []
                                                         , stPlain = True }
    where document' = plainify document

plainify :: Pandoc -> Pandoc
plainify = processWith go
  where go :: [Inline] -> [Inline]
        go (Emph xs : ys) = go xs ++ go ys
        go (Strong xs : ys) = go xs ++ go ys
        go (Strikeout xs : ys) = go xs ++ go ys
        go (Superscript xs : ys) = go xs ++ go ys
        go (Subscript xs : ys) = go xs ++ go ys
        go (SmallCaps xs : ys) = go xs ++ go ys
        go (Code s : ys) = Str s : go ys
        go (Math _ s : ys) = Str s : go ys
        go (TeX _ : ys) = Str "" : go ys
        go (HtmlInline _ : ys) = Str "" : go ys
        go (Link xs _ : ys) = go xs ++ go ys
        go (Image _ _ : ys) = go ys
        go (Cite _ cits : ys) = go cits ++ go ys
        go (x : ys) = x : go ys
        go [] = []

-- | Return markdown representation of document.
pandocToMarkdown :: WriterOptions -> Pandoc -> State WriterState String
pandocToMarkdown opts (Pandoc (Meta title authors date) blocks) = do
  title' <- inlineListToMarkdown opts title
  authors' <- mapM (inlineListToMarkdown opts) authors
  date' <- inlineListToMarkdown opts date
  let titleblock = not $ null title && null authors && null date
  let headerBlocks = filter isHeaderBlock blocks
  let toc = if writerTableOfContents opts 
               then tableOfContents opts headerBlocks
               else empty
  body <- blockListToMarkdown opts blocks
  st <- get
  notes' <- notesToMarkdown opts (reverse $ stNotes st)
  st' <- get  -- note that the notes may contain refs
  refs' <- refsToMarkdown opts (reverse $ stRefs st')
  let main = render $ foldl ($+$) empty $ [body, notes', refs']
  let context  = writerVariables opts ++
                 [ ("toc", render toc)
                 , ("body", main)
                 , ("title", render title')
                 , ("date", render date')
                 ] ++
                 [ ("titleblock", "yes") | titleblock ] ++
                 [ ("author", render a) | a <- authors' ]
  if writerStandalone opts
     then return $ renderTemplate context $ writerTemplate opts
     else return main

-- | Return markdown representation of reference key table.
refsToMarkdown :: WriterOptions -> Refs -> State WriterState Doc
refsToMarkdown opts refs = mapM (keyToMarkdown opts) refs >>= return . vcat
 
-- | Return markdown representation of a reference key. 
keyToMarkdown :: WriterOptions 
              -> ([Inline], (String, String)) 
              -> State WriterState Doc
keyToMarkdown opts (label, (src, tit)) = do
  label' <- inlineListToMarkdown opts label
  let tit' = if null tit then empty else text $ " \"" ++ tit ++ "\""
  return $ text "  " <> char '[' <> label' <> char ']' <> text ": " <>
           text src <> tit' 

-- | Return markdown representation of notes.
notesToMarkdown :: WriterOptions -> [[Block]] -> State WriterState Doc
notesToMarkdown opts notes = 
  mapM (\(num, note) -> noteToMarkdown opts num note) (zip [1..] notes) >>= 
  return . vcat

-- | Return markdown representation of a note.
noteToMarkdown :: WriterOptions -> Int -> [Block] -> State WriterState Doc
noteToMarkdown opts num blocks = do
  contents  <- blockListToMarkdown opts blocks
  let marker = text "[^" <> text (show num) <> text "]:"
  return $ hang' marker (writerTabStop opts) contents 

-- | Escape special characters for Markdown.
escapeString :: String -> String
escapeString = escapeStringUsing markdownEscapes
  where markdownEscapes = backslashEscapes "\\`*_>#~^"

-- | Construct table of contents from list of header blocks.
tableOfContents :: WriterOptions -> [Block] -> Doc 
tableOfContents opts headers =
  let opts' = opts { writerIgnoreNotes = True }
      contents = BulletList $ map elementToListItem $ hierarchicalize headers
  in  evalState (blockToMarkdown opts' contents) WriterState{ stNotes = []
                                                            , stRefs  = []
                                                            , stPlain = False }

-- | Converts an Element to a list item for a table of contents,
elementToListItem :: Element -> [Block]
elementToListItem (Blk _) = []
elementToListItem (Sec _ _ _ headerText subsecs) = [Plain headerText] ++ 
  if null subsecs
     then []
     else [BulletList $ map elementToListItem subsecs]

-- | Ordered list start parser for use in Para below.
olMarker :: GenParser Char ParserState Char
olMarker = do (start, style', delim) <- anyOrderedListMarker
              if delim == Period && 
                          (style' == UpperAlpha || (style' == UpperRoman &&
                          start `elem` [1, 5, 10, 50, 100, 500, 1000]))
                          then spaceChar >> spaceChar
                          else spaceChar

-- | True if string begins with an ordered list marker
beginsWithOrderedListMarker :: String -> Bool
beginsWithOrderedListMarker str = 
  case runParser olMarker defaultParserState "para start" str of
         Left  _  -> False 
         Right _  -> True

wrappedMarkdown :: WriterOptions -> [Inline] -> State WriterState Doc
wrappedMarkdown opts inlines = do
  let chunks  = splitBy LineBreak inlines
  let chunks' = if null chunks
                   then []
                   else (map (++ [Str "  "]) $ init chunks) ++ [last chunks]
  lns <- mapM (wrapIfNeeded opts (inlineListToMarkdown opts)) chunks'
  return $ vcat lns

-- | Convert Pandoc block element to markdown.
blockToMarkdown :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState Doc 
blockToMarkdown _ Null = return empty
blockToMarkdown opts (Plain inlines) = 
  wrappedMarkdown opts inlines
blockToMarkdown opts (Para inlines) = do
  contents <- wrappedMarkdown opts inlines
  -- escape if para starts with ordered list marker
  let esc = if (not (writerStrictMarkdown opts)) && 
               beginsWithOrderedListMarker (render contents)
               then char '\\'
               else empty 
  return $ esc <> contents <> text "\n"
blockToMarkdown _ (RawHtml str) = do
  st <- get
  if stPlain st
     then return empty
     else return $ text str
blockToMarkdown _ HorizontalRule = return $ text "\n* * * * *\n"
blockToMarkdown opts (Header level inlines) = do
  contents <- inlineListToMarkdown opts inlines
  st <- get
  -- use setext style headers if in literate haskell mode.
  -- ghc interprets '#' characters in column 1 as line number specifiers.
  if writerLiterateHaskell opts || stPlain st
     then let len = length $ render contents
          in  return $ contents <> text "\n" <>
                       case level of
                            1  -> text $ replicate len '=' ++ "\n"
                            2  -> text $ replicate len '-' ++ "\n"
                            _  -> empty
     else return $ text ((replicate level '#') ++ " ") <> contents <> text "\n"
blockToMarkdown opts (CodeBlock (_,classes,_) str) | "haskell" `elem` classes &&
                                                     "literate" `elem` classes &&
                                                     writerLiterateHaskell opts =
  return $ (vcat $ map (text "> " <>) $ map text (lines str)) <> text "\n"
blockToMarkdown opts (CodeBlock _ str) = return $
  (nest (writerTabStop opts) $ vcat $ map text (lines str)) <> text "\n"
blockToMarkdown opts (BlockQuote blocks) = do
  st <- get
  -- if we're writing literate haskell, put a space before the bird tracks
  -- so they won't be interpreted as lhs...
  let leader = if writerLiterateHaskell opts
                  then text . (" > " ++)
                  else if stPlain st
                          then text . ("  " ++)
                          else text . ("> " ++)
  contents <- blockListToMarkdown opts blocks
  return $ (vcat $ map leader $ lines $ render contents) <> 
           text "\n"
blockToMarkdown opts (Table caption aligns widths headers rows) =  do
  caption' <- inlineListToMarkdown opts caption
  let caption'' = if null caption
                     then empty
                     else text "" $+$ (text ": " <> caption')
  headers' <- mapM (blockListToMarkdown opts) headers
  let alignHeader alignment = case alignment of
                                AlignLeft    -> leftAlignBlock
                                AlignCenter  -> centerAlignBlock
                                AlignRight   -> rightAlignBlock
                                AlignDefault -> leftAlignBlock  
  rawRows <- mapM (mapM (blockListToMarkdown opts)) rows
  let isSimple = all (==0) widths
  let numChars = maximum . map (length . render)
  let widthsInChars =
       if isSimple
          then map ((+2) . numChars) $ transpose (headers' : rawRows)
          else map (floor . (78 *)) widths
  let makeRow = hsepBlocks . (zipWith alignHeader aligns) . 
                (zipWith docToBlock widthsInChars)
  let rows' = map makeRow rawRows
  let head' = makeRow headers'
  let maxRowHeight = maximum $ map heightOfBlock (head':rows')
  let underline = hsep $ 
                  map (\width -> text $ replicate width '-') widthsInChars
  let border = if maxRowHeight > 1
                  then text $ replicate (sum widthsInChars + (length widthsInChars - 1)) '-'
                  else if all null headers
                          then underline
                          else empty
  let head'' = if all null headers
                  then empty
                  else border $+$ blockToDoc head'
  let spacer = if maxRowHeight > 1
                  then text ""
                  else empty
  let body = vcat $ intersperse spacer $ map blockToDoc rows'
  let bottom = if all null headers
                  then underline
                  else border
  return $ (nest 2 $ head'' $+$ underline $+$ body $+$ 
                     bottom $+$ caption'') <> text "\n"
blockToMarkdown opts (BulletList items) = do
  contents <- mapM (bulletListItemToMarkdown opts) items
  return $ (vcat contents) <> text "\n"
blockToMarkdown opts (OrderedList attribs items) = do
  let markers  = orderedListMarkers attribs
  let markers' = map (\m -> if length m < 3
                               then m ++ replicate (3 - length m) ' '
                               else m) markers 
  contents <- mapM (\(item, num) -> orderedListItemToMarkdown opts item num) $
              zip markers' items  
  return $ (vcat contents) <> text "\n"
blockToMarkdown opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToMarkdown opts) items
  return $ (vcat contents) <> text "\n"

-- | Convert bullet list item (list of blocks) to markdown.
bulletListItemToMarkdown :: WriterOptions -> [Block] -> State WriterState Doc
bulletListItemToMarkdown opts items = do
  contents <- blockListToMarkdown opts items
  return $ hang' (text "-  ") (writerTabStop opts) contents

-- | Convert ordered list item (a list of blocks) to markdown.
orderedListItemToMarkdown :: WriterOptions -- ^ options
                          -> String        -- ^ list item marker
                          -> [Block]       -- ^ list item (list of blocks)
                          -> State WriterState Doc
orderedListItemToMarkdown opts marker items = do
  contents <- blockListToMarkdown opts items
  return $ hsep [nest (min (3 - length marker) 0) (text marker),
                nest (writerTabStop opts) contents]

-- | Convert definition list item (label, list of blocks) to markdown.
definitionListItemToMarkdown :: WriterOptions
                             -> ([Inline],[[Block]]) 
                             -> State WriterState Doc
definitionListItemToMarkdown opts (label, defs) = do
  labelText <- inlineListToMarkdown opts label
  let tabStop = writerTabStop opts
  st <- get
  let leader  = if stPlain st then empty else text "  ~"
  contents <- liftM vcat $
    mapM (liftM ((leader $$) . nest tabStop . vcat) . mapM (blockToMarkdown opts))           defs
  return $ labelText $+$ contents

-- | Convert list of Pandoc block elements to markdown.
blockListToMarkdown :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState Doc 
blockListToMarkdown opts blocks =
  mapM (blockToMarkdown opts) blocks >>= return . vcat

-- | Get reference for target; if none exists, create unique one and return.
--   Prefer label if possible; otherwise, generate a unique key.
getReference :: [Inline] -> Target -> State WriterState [Inline]
getReference label (src, tit) = do
  st <- get
  case find ((== (src, tit)) . snd) (stRefs st) of
    Just (ref, _) -> return ref
    Nothing       -> do
      let label' = case find ((== label) . fst) (stRefs st) of
                      Just _ -> -- label is used; generate numerical label
                                 case find (\n -> not (any (== [Str (show n)])
                                           (map fst (stRefs st)))) [1..(10000 :: Integer)] of
                                      Just x  -> [Str (show x)]
                                      Nothing -> error "no unique label"
                      Nothing -> label
      modify (\s -> s{ stRefs = (label', (src,tit)) : stRefs st })
      return label'

-- | Convert list of Pandoc inline elements to markdown.
inlineListToMarkdown :: WriterOptions -> [Inline] -> State WriterState Doc
inlineListToMarkdown opts lst =
  mapM (inlineToMarkdown opts) lst >>= return . hcat

-- | Convert Pandoc inline element to markdown.
inlineToMarkdown :: WriterOptions -> Inline -> State WriterState Doc
inlineToMarkdown opts (Emph lst) = do 
  contents <- inlineListToMarkdown opts lst
  return $ char '*' <> contents <> char '*'
inlineToMarkdown opts (Strong lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ text "**" <> contents <> text "**"
inlineToMarkdown opts (Strikeout lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ text "~~" <> contents <> text "~~"
inlineToMarkdown opts (Superscript lst) = do
  contents <- inlineListToMarkdown opts lst
  let contents' = text $ substitute " " "\\ " $ render contents
  return $ char '^' <> contents' <> char '^'
inlineToMarkdown opts (Subscript lst) = do
  contents <- inlineListToMarkdown opts lst
  let contents' = text $ substitute " " "\\ " $ render contents
  return $ char '~' <> contents' <> char '~'
inlineToMarkdown opts (SmallCaps lst) = inlineListToMarkdown opts lst
inlineToMarkdown opts (Quoted SingleQuote lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ char '‘' <> contents <> char '’'
inlineToMarkdown opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ char '“' <> contents <> char '”'
inlineToMarkdown _ EmDash = return $ char '\8212'
inlineToMarkdown _ EnDash = return $ char '\8211'
inlineToMarkdown _ Apostrophe = return $ char '\8217'
inlineToMarkdown _ Ellipses = return $ char '\8230'
inlineToMarkdown _ (Code str) =
  let tickGroups = filter (\s -> '`' `elem` s) $ group str 
      longest    = if null tickGroups
                     then 0
                     else maximum $ map length tickGroups 
      marker     = replicate (longest + 1) '`' 
      spacer     = if (longest == 0) then "" else " " in
  return $ text (marker ++ spacer ++ str ++ spacer ++ marker)
inlineToMarkdown _ (Str str) = do
  st <- get
  if stPlain st
     then return $ text str
     else return $ text $ escapeString str
inlineToMarkdown _ (Math InlineMath str) = return $ char '$' <> text str <> char '$'
inlineToMarkdown _ (Math DisplayMath str) = return $ text "$$" <> text str <> text "$$"
inlineToMarkdown _ (TeX str) = return $ text str
inlineToMarkdown _ (HtmlInline str) = return $ text str 
inlineToMarkdown _ (LineBreak) = return $ text "  \n"
inlineToMarkdown _ Space = return $ char ' '
inlineToMarkdown opts (Cite (c:cs) lst)
  | writerCiteMethod opts == Citeproc = inlineListToMarkdown opts lst
  | citationMode c == AuthorInText = do
    suffs <- inlineListToMarkdown opts $ citationSuffix c
    rest <- mapM convertOne cs
    let inbr = suffs <+> joincits rest
        br   = if isEmpty inbr then empty else brackets inbr
    return $ text ("@" ++ citationId c) <+> br
  | otherwise = do
    cits <- mapM convertOne (c:cs)
    return $ text "[" <> joincits cits <> text "]"
  where
        joincits = hcat . punctuate (text "; ") . filter (not . isEmpty)
        convertOne Citation { citationId      = k
                            , citationPrefix  = pinlines
                            , citationSuffix  = sinlines
                            , citationMode    = m }
                               = do
           pdoc <- inlineListToMarkdown opts pinlines
           sdoc <- inlineListToMarkdown opts sinlines
           let k' = text (modekey m ++ "@" ++ k)
               r = case sinlines of
                        Str (y:_):_ | y `elem` ",;]@" -> k' <> sdoc
                        _                             -> k' <+> sdoc
           return $ pdoc <+> r
        modekey SuppressAuthor = "-"
        modekey _              = ""
inlineToMarkdown _ (Cite _ _) = return $ text ""
inlineToMarkdown opts (Link txt (src', tit)) = do
  linktext <- inlineListToMarkdown opts txt
  let linktitle = if null tit then empty else text $ " \"" ++ tit ++ "\""
  let src = unescapeURI src'
  let srcSuffix = if isPrefixOf "mailto:" src then drop 7 src else src
  let useRefLinks = writerReferenceLinks opts
  let useAuto = null tit && txt == [Code srcSuffix]
  ref <- if useRefLinks then getReference txt (src, tit) else return []
  reftext <- inlineListToMarkdown opts ref
  return $ if useAuto
              then char '<' <> text srcSuffix <> char '>' 
              else if useRefLinks
                      then let first  = char '[' <> linktext <> char ']'
                               second = if txt == ref
                                           then text "[]"
                                           else char '[' <> reftext <> char ']'
                           in  first <> second
                      else char '[' <> linktext <> char ']' <> 
                           char '(' <> text src <> linktitle <> char ')' 
inlineToMarkdown opts (Image alternate (source, tit)) = do
  let txt = if (null alternate) || (alternate == [Str ""]) || 
               (alternate == [Str source]) -- to prevent autolinks
               then [Str "image"]
               else alternate
  linkPart <- inlineToMarkdown opts (Link txt (unescapeURI source, tit)) 
  return $ char '!' <> linkPart
inlineToMarkdown _ (Note contents) = do 
  modify (\st -> st{ stNotes = contents : stNotes st })
  st <- get
  let ref = show $ (length $ stNotes st)
  return $ text "[^" <> text ref <> char ']'
