{-# LANGUAGE OverloadedStrings #-}
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
import Text.Pandoc.Generic
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Shared
import Text.Pandoc.Parsing hiding (blankline)
import Text.ParserCombinators.Parsec ( runParser, GenParser )
import Data.List ( group, isPrefixOf, find, intersperse, transpose )
import Text.Pandoc.Pretty
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
  evalState (pandocToMarkdown opts{writerStrictMarkdown = True}
              document') WriterState{ stNotes = []
                                    , stRefs  = []
                                    , stPlain = True }
    where document' = plainify document

plainify :: Pandoc -> Pandoc
plainify = bottomUp go
  where go :: Inline -> Inline
        go (Emph xs) = SmallCaps xs
        go (Strong xs) = SmallCaps xs
        go (Strikeout xs) = SmallCaps xs
        go (Superscript xs) = SmallCaps xs
        go (Subscript xs) = SmallCaps xs
        go (SmallCaps xs) = SmallCaps xs
        go (Code _ s) = Str s
        go (Math _ s) = Str s
        go (RawInline _ _) = Str ""
        go (Link xs _) = SmallCaps xs
        go (Image xs _) = SmallCaps $ [Str "["] ++ xs ++ [Str "]"]
        go (Cite _ cits) = SmallCaps cits
        go x = x

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
  let colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
  let main = render colwidth $ body <>
               (if isEmpty notes' then empty else blankline <> notes') <>
               (if isEmpty refs' then empty else blankline <> refs')
  let context  = writerVariables opts ++
                 [ ("toc", render colwidth toc)
                 , ("body", main)
                 , ("title", render colwidth title')
                 , ("date", render colwidth date')
                 ] ++
                 [ ("titleblock", "yes") | titleblock ] ++
                 [ ("author", render colwidth a) | a <- authors' ]
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
  let tit' = if null tit
                then empty
                else space <> "\"" <> text tit <> "\""
  return $ nest 2 $ hang 2
            ("[" <> label' <> "]:" <> space) (text src <> tit')

-- | Return markdown representation of notes.
notesToMarkdown :: WriterOptions -> [[Block]] -> State WriterState Doc
notesToMarkdown opts notes = 
  mapM (\(num, note) -> noteToMarkdown opts num note) (zip [1..] notes) >>=
  return . vsep

-- | Return markdown representation of a note.
noteToMarkdown :: WriterOptions -> Int -> [Block] -> State WriterState Doc
noteToMarkdown opts num blocks = do
  contents  <- blockListToMarkdown opts blocks
  let num' = text $ show num
  let marker = text "[^" <> num' <> text "]:"
  let markerSize = 4 + offset num'
  let spacer = case writerTabStop opts - markerSize of
                     n | n > 0  -> text $ replicate n ' '
                     _          -> text " "
  return $ hang (writerTabStop opts) (marker <> spacer) contents

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

attrsToMarkdown :: Attr -> Doc
attrsToMarkdown attribs = braces $ hsep [attribId, attribClasses, attribKeys]
        where attribId = case attribs of
                                ([],_,_) -> empty
                                (i,_,_)  -> "#" <> text i
              attribClasses = case attribs of
                                (_,[],_) -> empty
                                (_,cs,_) -> hsep $
                                            map (text . ('.':))
                                            cs
              attribKeys = case attribs of
                                (_,_,[]) -> empty
                                (_,_,ks) -> hsep $
                                            map (\(k,v) -> text k
                                              <> "=\"" <> text v <> "\"") ks

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
  case runParser olMarker defaultParserState "para start" (take 10 str) of
         Left  _  -> False
         Right _  -> True

-- | Convert Pandoc block element to markdown.
blockToMarkdown :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState Doc 
blockToMarkdown _ Null = return empty
blockToMarkdown opts (Plain inlines) = do
  contents <- inlineListToMarkdown opts inlines
  return $ contents <> cr
blockToMarkdown opts (Para inlines) = do
  contents <- inlineListToMarkdown opts inlines
  -- escape if para starts with ordered list marker
  st <- get
  let esc = if (not (writerStrictMarkdown opts)) &&
               not (stPlain st) &&
               beginsWithOrderedListMarker (render Nothing contents)
               then text "\\"
               else empty
  return $ esc <> contents <> blankline
blockToMarkdown _ (RawBlock f str)
  | f == "html" || f == "latex" || f == "tex" || f == "markdown" = do
    st <- get
    if stPlain st
       then return empty
       else return $ text str <> text "\n"
blockToMarkdown _ (RawBlock _ _) = return empty
blockToMarkdown _ HorizontalRule =
  return $ blankline <> text "* * * * *" <> blankline
blockToMarkdown opts (Header level inlines) = do
  contents <- inlineListToMarkdown opts inlines
  st <- get
  -- use setext style headers if in literate haskell mode.
  -- ghc interprets '#' characters in column 1 as line number specifiers.
  if writerLiterateHaskell opts || stPlain st
     then let len = offset contents
          in  return $ contents <> cr <>
                       (case level of
                             1  -> text $ replicate len '='
                             2  -> text $ replicate len '-'
                             _  -> empty) <> blankline
     else return $
       text ((replicate level '#') ++ " ") <> contents <> blankline
blockToMarkdown opts (CodeBlock (_,classes,_) str)
  | "haskell" `elem` classes && "literate" `elem` classes &&
    writerLiterateHaskell opts =
  return $ prefixed "> " (text str) <> blankline
blockToMarkdown opts (CodeBlock attribs str) = return $
  if writerStrictMarkdown opts || attribs == nullAttr
     then nest (writerTabStop opts) (text str) <> blankline
     else -- use delimited code block
          flush (tildes <> space <> attrs <> cr <> text str <>
                  cr <> tildes) <> blankline
            where tildes  = text "~~~~"
                  attrs = attrsToMarkdown attribs
blockToMarkdown opts (BlockQuote blocks) = do
  st <- get
  -- if we're writing literate haskell, put a space before the bird tracks
  -- so they won't be interpreted as lhs...
  let leader = if writerLiterateHaskell opts
                  then " > "
                  else if stPlain st
                          then "  "
                          else "> "
  contents <- blockListToMarkdown opts blocks
  return $ (prefixed leader contents) <> blankline
blockToMarkdown opts (Table caption aligns widths headers rows) =  do
  caption' <- inlineListToMarkdown opts caption
  let caption'' = if null caption
                     then empty
                     else blankline <> ": " <> caption' <> blankline
  headers' <- mapM (blockListToMarkdown opts) headers
  let alignHeader alignment = case alignment of
                                AlignLeft    -> lblock
                                AlignCenter  -> cblock
                                AlignRight   -> rblock
                                AlignDefault -> lblock
  rawRows <- mapM (mapM (blockListToMarkdown opts)) rows
  let isSimple = all (==0) widths
  let numChars = maximum . map offset
  let widthsInChars =
       if isSimple
          then map ((+2) . numChars) $ transpose (headers' : rawRows)
          else map (floor . (fromIntegral (writerColumns opts) *)) widths
  let makeRow = hcat . intersperse (lblock 1 (text " ")) .
                   (zipWith3 alignHeader aligns widthsInChars)
  let rows' = map makeRow rawRows
  let head' = makeRow headers'
  let maxRowHeight = maximum $ map height (head':rows')
  let underline = cat $ intersperse (text " ") $
                  map (\width -> text (replicate width '-')) widthsInChars
  let border = if maxRowHeight > 1
                  then text (replicate (sum widthsInChars +
                          length widthsInChars - 1) '-')
                  else if all null headers
                          then underline
                          else empty
  let head'' = if all null headers
                  then empty
                  else border <> cr <> head'
  let body = if maxRowHeight > 1
                then vsep rows'
                else vcat rows'
  let bottom = if all null headers
                  then underline
                  else border
  return $ nest 2 $ head'' $$ underline $$ body $$
              bottom $$ blankline $$ caption'' $$ blankline
blockToMarkdown opts (BulletList items) = do
  contents <- mapM (bulletListItemToMarkdown opts) items
  return $ cat contents <> blankline
blockToMarkdown opts (OrderedList attribs items) = do
  let markers  = orderedListMarkers attribs
  let markers' = map (\m -> if length m < 3
                               then m ++ replicate (3 - length m) ' '
                               else m) markers
  contents <- mapM (\(item, num) -> orderedListItemToMarkdown opts item num) $
              zip markers' items
  return $ cat contents <> blankline
blockToMarkdown opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToMarkdown opts) items
  return $ cat contents <> blankline

-- | Convert bullet list item (list of blocks) to markdown.
bulletListItemToMarkdown :: WriterOptions -> [Block] -> State WriterState Doc
bulletListItemToMarkdown opts items = do
  contents <- blockListToMarkdown opts items
  let sps = replicate (writerTabStop opts - 2) ' '
  let start = text ('-' : ' ' : sps)
  return $ hang (writerTabStop opts) start $ contents <> cr

-- | Convert ordered list item (a list of blocks) to markdown.
orderedListItemToMarkdown :: WriterOptions -- ^ options
                          -> String        -- ^ list item marker
                          -> [Block]       -- ^ list item (list of blocks)
                          -> State WriterState Doc
orderedListItemToMarkdown opts marker items = do
  contents <- blockListToMarkdown opts items
  let sps = case length marker - writerTabStop opts of
                   n | n > 0 -> text $ replicate n ' '
                   _         -> text " "
  let start = text marker <> sps
  return $ hang (writerTabStop opts) start $ contents <> cr

-- | Convert definition list item (label, list of blocks) to markdown.
definitionListItemToMarkdown :: WriterOptions
                             -> ([Inline],[[Block]]) 
                             -> State WriterState Doc
definitionListItemToMarkdown opts (label, defs) = do
  labelText <- inlineListToMarkdown opts label
  let tabStop = writerTabStop opts
  st <- get
  let leader  = if stPlain st then "   " else "  ~"
  let sps = case writerTabStop opts - 3 of
                 n | n > 0   -> text $ replicate n ' '
                 _           -> text " "
  defs' <- mapM (mapM (blockToMarkdown opts)) defs
  let contents = vcat $ map (\d -> hang tabStop (leader <> sps) $ vcat d <> cr) defs'
  return $ labelText <> cr <> contents <> cr

-- | Convert list of Pandoc block elements to markdown.
blockListToMarkdown :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState Doc 
blockListToMarkdown opts blocks =
  mapM (blockToMarkdown opts) (fixBlocks blocks) >>= return . cat
    -- insert comment between list and indented code block, or the
    -- code block will be treated as a list continuation paragraph
    where fixBlocks (b : CodeBlock attr x : rest)
            | (writerStrictMarkdown opts || attr == nullAttr) && isListBlock b =
               b : RawBlock "html" "<!-- -->\n" : CodeBlock attr x :
                  fixBlocks rest
          fixBlocks (x : xs)             = x : fixBlocks xs
          fixBlocks []                   = []
          isListBlock (BulletList _)     = True
          isListBlock (OrderedList _ _)  = True
          isListBlock (DefinitionList _) = True
          isListBlock _                  = False

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
  mapM (inlineToMarkdown opts) lst >>= return . cat

escapeSpaces :: Inline -> Inline
escapeSpaces (Str s) = Str $ substitute " " "\\ " s
escapeSpaces Space = Str "\\ "
escapeSpaces x = x

-- | Convert Pandoc inline element to markdown.
inlineToMarkdown :: WriterOptions -> Inline -> State WriterState Doc
inlineToMarkdown opts (Emph lst) = do 
  contents <- inlineListToMarkdown opts lst
  return $ "*" <> contents <> "*"
inlineToMarkdown opts (Strong lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ "**" <> contents <> "**"
inlineToMarkdown opts (Strikeout lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ "~~" <> contents <> "~~"
inlineToMarkdown opts (Superscript lst) = do
  let lst' = bottomUp escapeSpaces lst
  contents <- inlineListToMarkdown opts lst'
  return $ "^" <> contents <> "^"
inlineToMarkdown opts (Subscript lst) = do
  let lst' = bottomUp escapeSpaces lst
  contents <- inlineListToMarkdown opts lst'
  return $ "~" <> contents <> "~"
inlineToMarkdown opts (SmallCaps lst) = inlineListToMarkdown opts lst
inlineToMarkdown opts (Quoted SingleQuote lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ "‘" <> contents <> "’"
inlineToMarkdown opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ "“" <> contents <> "”"
inlineToMarkdown _ EmDash = return "\8212"
inlineToMarkdown _ EnDash = return "\8211"
inlineToMarkdown _ Apostrophe = return "\8217"
inlineToMarkdown _ Ellipses = return "\8230"
inlineToMarkdown opts (Code attr str) =
  let tickGroups = filter (\s -> '`' `elem` s) $ group str 
      longest    = if null tickGroups
                     then 0
                     else maximum $ map length tickGroups 
      marker     = replicate (longest + 1) '`' 
      spacer     = if (longest == 0) then "" else " "
      attrs      = if writerStrictMarkdown opts || attr == nullAttr
                      then empty
                      else attrsToMarkdown attr
  in  return $ text (marker ++ spacer ++ str ++ spacer ++ marker) <> attrs
inlineToMarkdown _ (Str str) = do
  st <- get
  if stPlain st
     then return $ text str
     else return $ text $ escapeString str
inlineToMarkdown _ (Math InlineMath str) =
  return $ "$" <> text str <> "$"
inlineToMarkdown _ (Math DisplayMath str) =
  return $ "$$" <> text str <> "$$"
inlineToMarkdown _ (RawInline f str)
  | f == "html" || f == "latex" || f == "tex" || f == "markdown" =
    return $ text str
inlineToMarkdown _ (RawInline _ _) = return empty
inlineToMarkdown opts (LineBreak) = return $
  if writerStrictMarkdown opts
     then "  " <> cr
     else "\\" <> cr
inlineToMarkdown _ Space = return space
inlineToMarkdown opts (Cite (c:cs) lst)
  | writerCiteMethod opts == Citeproc = inlineListToMarkdown opts lst
  | citationMode c == AuthorInText = do
    suffs <- inlineListToMarkdown opts $ citationSuffix c
    rest <- mapM convertOne cs
    let inbr = suffs <+> joincits rest
        br   = if isEmpty inbr then empty else char '[' <> inbr <> char ']'
    return $ text ("@" ++ citationId c) <+> br
  | otherwise = do
    cits <- mapM convertOne (c:cs)
    return $ text "[" <> joincits cits <> text "]"
  where
        joincits = hcat . intersperse (text "; ") . filter (not . isEmpty)
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
  let linktitle = if null tit
                     then empty
                     else text $ " \"" ++ tit ++ "\""
  let src = unescapeURI src'
  let srcSuffix = if isPrefixOf "mailto:" src then drop 7 src else src
  let useRefLinks = writerReferenceLinks opts
  let useAuto = case (tit,txt) of
                      ("", [Code _ s]) | s == srcSuffix -> True
                      _                                 -> False
  ref <- if useRefLinks then getReference txt (src, tit) else return []
  reftext <- inlineListToMarkdown opts ref
  return $ if useAuto
              then "<" <> text srcSuffix <> ">"
              else if useRefLinks
                      then let first  = "[" <> linktext <> "]"
                               second = if txt == ref
                                           then "[]"
                                           else "[" <> reftext <> "]"
                           in  first <> second
                      else "[" <> linktext <> "](" <> 
                           text src <> linktitle <> ")"
inlineToMarkdown opts (Image alternate (source, tit)) = do
  let txt = if (null alternate) || (alternate == [Str ""]) || 
               (alternate == [Str source]) -- to prevent autolinks
               then [Str "image"]
               else alternate
  linkPart <- inlineToMarkdown opts (Link txt (source, tit))
  return $ "!" <> linkPart
inlineToMarkdown _ (Note contents) = do 
  modify (\st -> st{ stNotes = contents : stNotes st })
  st <- get
  let ref = show $ (length $ stNotes st)
  return $ "[^" <> text ref <> "]"
