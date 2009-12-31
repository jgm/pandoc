{-
Copyright (C) 2006-7 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-7 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to markdown-formatted plain text.

Markdown:  <http://daringfireball.net/projects/markdown/>
-}
module Text.Pandoc.Writers.Markdown ( writeMarkdown) where
import Text.Pandoc.Definition
import Text.Pandoc.Templates
import Text.Pandoc.Shared 
import Text.Pandoc.Blocks
import Text.ParserCombinators.Parsec ( parse, GenParser )
import Data.List ( group, isPrefixOf, drop, find, intersperse, intercalate, transpose )
import Text.PrettyPrint.HughesPJ hiding ( Str )
import Control.Monad.State

type Notes = [[Block]]
type Refs = KeyTable
type WriterState = (Notes, Refs)

-- | Convert Pandoc to Markdown.
writeMarkdown :: WriterOptions -> Pandoc -> String
writeMarkdown opts document = 
  evalState (pandocToMarkdown opts document) ([],[]) 

-- | Return markdown representation of document.
pandocToMarkdown :: WriterOptions -> Pandoc -> State WriterState String
pandocToMarkdown opts (Pandoc meta blocks) = do
  metaBlock <- metaToMarkdown opts meta
  let head' = if writerStandalone opts
                then metaBlock
                else empty
  let headerBlocks = filter isHeaderBlock blocks
  let toc = if writerTableOfContents opts 
               then tableOfContents opts headerBlocks
               else empty
  body <- blockListToMarkdown opts blocks
  let before = if null (writerIncludeBefore opts)
                  then empty
                  else text $ writerIncludeBefore opts
  let after = if null (writerIncludeAfter opts)
                  then empty
                  else text $ writerIncludeAfter opts
  (notes, _) <- get
  notes' <- notesToMarkdown opts (reverse notes)
  (_, refs) <- get  -- note that the notes may contain refs
  refs' <- keyTableToMarkdown opts (reverse refs)
  let main = render $ before $+$ body $+$ text "" $+$ notes' $+$ text "" $+$ refs' $+$ after
  let context  = writerVariables opts ++
                 [ ("toc", render toc)
                 , ("body", main)
                 , ("titleblock", render head')
                 ]
  if writerStandalone opts
     then return $ renderTemplate context $ writerTemplate opts
     else return main

-- | Return markdown representation of reference key table.
keyTableToMarkdown :: WriterOptions -> KeyTable -> State WriterState Doc
keyTableToMarkdown opts refs = mapM (keyToMarkdown opts) refs >>= return . vcat
 
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

-- | Convert bibliographic information into Markdown header.
metaToMarkdown :: WriterOptions -> Meta -> State WriterState Doc
metaToMarkdown _ (Meta [] [] []) = return empty
metaToMarkdown opts (Meta title authors date) = do
  title'   <- titleToMarkdown opts title
  authors' <- authorsToMarkdown opts authors
  date'    <- dateToMarkdown opts date
  return $ title' $+$ authors' $+$ date' $+$ text ""

titleToMarkdown :: WriterOptions -> [Inline] -> State WriterState Doc
titleToMarkdown _    []  = return empty
titleToMarkdown opts lst = do
  contents <- inlineListToMarkdown opts lst
  return $ text "% " <> contents 

authorsToMarkdown :: WriterOptions -> [[Inline]] -> State WriterState Doc
authorsToMarkdown opts [] = return empty
authorsToMarkdown opts lst = do
  authors <- mapM (inlineListToMarkdown opts) lst 
  return $ text "% " <> (hcat $ intersperse (text ", ") authors)

dateToMarkdown :: WriterOptions -> [Inline] -> State WriterState Doc
dateToMarkdown opts [] = return empty
dateToMarkdown opts str = do
  date <- inlineListToMarkdown opts str
  return $ text "% " <> date

-- | Construct table of contents from list of header blocks.
tableOfContents :: WriterOptions -> [Block] -> Doc 
tableOfContents opts headers =
  let opts' = opts { writerIgnoreNotes = True }
      contents = BulletList $ map elementToListItem $ hierarchicalize headers
  in  evalState (blockToMarkdown opts' contents) ([],[])

-- | Converts an Element to a list item for a table of contents,
elementToListItem :: Element -> [Block]
elementToListItem (Blk _) = []
elementToListItem (Sec _ _ _ headerText subsecs) = [Plain headerText] ++ 
  if null subsecs
     then []
     else [BulletList $ map elementToListItem subsecs]

-- | Ordered list start parser for use in Para below.
olMarker :: GenParser Char st Char
olMarker = do (start, style', delim) <- anyOrderedListMarker
              if delim == Period && 
                          (style' == UpperAlpha || (style' == UpperRoman &&
                          start `elem` [1, 5, 10, 50, 100, 500, 1000]))
                          then spaceChar >> spaceChar
                          else spaceChar

-- | True if string begins with an ordered list marker
beginsWithOrderedListMarker :: String -> Bool
beginsWithOrderedListMarker str = 
  case parse olMarker "para start" str of
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
blockToMarkdown _ (RawHtml str) = return $ text str
blockToMarkdown _ HorizontalRule = return $ text "\n* * * * *\n"
blockToMarkdown opts (Header level inlines) = do
  contents <- inlineListToMarkdown opts inlines
  -- use setext style headers if in literate haskell mode.
  -- ghc interprets '#' characters in column 1 as line number specifiers.
  if writerLiterateHaskell opts
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
  -- if we're writing literate haskell, put a space before the bird tracks
  -- so they won't be interpreted as lhs...
  let leader = if writerLiterateHaskell opts
                  then text . (" > " ++)
                  else text . ("> " ++)
  contents <- blockListToMarkdown opts blocks
  return $ (vcat $ map leader $ lines $ render contents) <> 
           text "\n"
blockToMarkdown opts (Table caption aligns widths headers rows) =  do
  caption' <- inlineListToMarkdown opts caption
  let caption'' = if null caption
                     then empty
                     else text "" $+$ (text "Table: " <> caption')
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
  let head' = makeRow headers'
  let rows' = map makeRow rawRows
  let maxRowHeight = maximum $ map heightOfBlock (head':rows')
  let underline = hsep $ 
                  map (\width -> text $ replicate width '-') widthsInChars
  let border = if maxRowHeight > 1
                  then text $ replicate (sum widthsInChars + (length widthsInChars - 1)) '-'
                  else empty
  let spacer = if maxRowHeight > 1
                  then text ""
                  else empty
  let body = vcat $ intersperse spacer $ map blockToDoc rows'
  return $ (nest 2 $ border $+$ (blockToDoc head') $+$ underline $+$ body $+$ 
                     border $+$ caption'') <> text "\n"
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
  let leader  = char ':'
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
  (_,refs) <- get
  case find ((== (src, tit)) . snd) refs of
    Just (ref, _) -> return ref
    Nothing       -> do
      let label' = case find ((== label) . fst) refs of
                      Just _ -> -- label is used; generate numerical label
                                 case find (\n -> not (any (== [Str (show n)])
                                           (map fst refs))) [1..(10000 :: Integer)] of
                                      Just x  -> [Str (show x)]
                                      Nothing -> error "no unique label"
                      Nothing -> label
      modify (\(notes, refs') -> (notes, (label', (src,tit)):refs'))
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
  return $ char '\'' <> contents <> char '\''
inlineToMarkdown opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ char '"' <> contents <> char '"'
inlineToMarkdown _ EmDash = return $ text "--"
inlineToMarkdown _ EnDash = return $ char '-'
inlineToMarkdown _ Apostrophe = return $ char '\''
inlineToMarkdown _ Ellipses = return $ text "..."
inlineToMarkdown _ (Code str) =
  let tickGroups = filter (\s -> '`' `elem` s) $ group str 
      longest    = if null tickGroups
                     then 0
                     else maximum $ map length tickGroups 
      marker     = replicate (longest + 1) '`' 
      spacer     = if (longest == 0) then "" else " " in
  return $ text (marker ++ spacer ++ str ++ spacer ++ marker)
inlineToMarkdown _ (Str str) = return $ text $ escapeString str
inlineToMarkdown _ (Math InlineMath str) = return $ char '$' <> text str <> char '$'
inlineToMarkdown _ (Math DisplayMath str) = return $ text "$$" <> text str <> text "$$"
inlineToMarkdown _ (TeX str) = return $ text str
inlineToMarkdown _ (HtmlInline str) = return $ text str 
inlineToMarkdown _ (LineBreak) = return $ text "  \n"
inlineToMarkdown _ Space = return $ char ' '
inlineToMarkdown _ (Cite cits _ ) = do
  let format (a,b) xs = text a <>
                        (if b /= [] then char '@' else empty) <>
                        text b <> 
                        (if isEmpty xs then empty else text "; ") <>
                        xs
  return $ char '[' <> foldr format empty cits <> char ']'
inlineToMarkdown opts (Link txt (src, tit)) = do
  linktext <- inlineListToMarkdown opts txt
  let linktitle = if null tit then empty else text $ " \"" ++ tit ++ "\""
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
  linkPart <- inlineToMarkdown opts (Link txt (source, tit)) 
  return $ char '!' <> linkPart
inlineToMarkdown _ (Note contents) = do 
  modify (\(notes, refs) -> (contents:notes, refs)) -- add to notes in state
  (notes, _) <- get
  let ref = show $ (length notes)
  return $ text "[^" <> text ref <> char ']'
