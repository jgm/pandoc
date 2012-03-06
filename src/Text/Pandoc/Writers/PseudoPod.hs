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
   Module      : Text.Pandoc.Writers.PseudoPod 
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to PseudoPod-formatted plain text.

PseudoPod:  <http://daringfireball.net/projects/PseudoPod/>
-}
module Text.Pandoc.Writers.PseudoPod (writePseudoPod) where
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Shared
import Text.Pandoc.Parsing hiding (blankline)
import Text.ParserCombinators.Parsec ( runParser, GenParser )
import Data.List ( intersperse )
import Text.Pandoc.Pretty
import Control.Monad.State

type Refs = [([Inline], Target)]
data WriterState = WriterState { stRefs :: Refs
                               , stPlain :: Bool }

-- | Convert Pandoc to PseudoPod.
writePseudoPod :: WriterOptions -> Pandoc -> String
writePseudoPod opts document = 
  evalState (pandocToPseudoPod opts document) WriterState{stRefs  = []
                                                        , stPlain = False }

-- | Return PseudoPod representation of document.
pandocToPseudoPod :: WriterOptions -> Pandoc -> State WriterState String
pandocToPseudoPod opts (Pandoc (Meta title authors date) blocks) = do
  title' <- inlineListToPseudoPod opts title
  authors' <- mapM (inlineListToPseudoPod opts) authors
  date' <- inlineListToPseudoPod opts date
  let titleblock = not $ null title && null authors && null date
  let headerBlocks = filter isHeaderBlock blocks
  let toc = if writerTableOfContents opts 
               then tableOfContents opts headerBlocks
               else empty
  body <- blockListToPseudoPod opts blocks
  st' <- get  -- note that the notes may contain refs
  refs' <- refsToPseudoPod opts (reverse $ stRefs st')
  let colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
  let main = render colwidth $ text("=encoding utf8\n\n") <> body <>
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

-- | Return PseudoPod representation of reference key table.
refsToPseudoPod :: WriterOptions -> Refs -> State WriterState Doc
refsToPseudoPod opts refs = mapM (keyToPseudoPod opts) refs >>= return . vcat

-- | Return PseudoPod representation of a reference key. 
keyToPseudoPod :: WriterOptions 
              -> ([Inline], (String, String)) 
              -> State WriterState Doc
keyToPseudoPod opts (label, (src, tit)) = do
  label' <- inlineListToPseudoPod opts label
  let tit' = if null tit
                then empty
                else space <> "\"" <> text tit <> "\""
  return $ nest 2 $ hang 2
            ("[" <> label' <> "]:" <> space) (text src <> tit')

-- | Escape special characters for PseudoPod.
escapeString :: String -> String
escapeString = escapeStringUsing pseudopodEscapes
  where pseudopodEscapes = entityEscapes

-- | pod-ish entity escapes, E<>
entityEscapes :: [(Char,String)]
entityEscapes = [('>', "E<gt>"), ('<', "E<lt>"), ('|', "E<verbar>"), ('/', "E<sol>")]

-- | Construct table of contents from list of header blocks.
tableOfContents :: WriterOptions -> [Block] -> Doc 
tableOfContents opts headers =
  let opts' = opts { writerIgnoreNotes = True }
      contents = BulletList $ map elementToListItem $ hierarchicalize headers
  in  evalState (blockToPseudoPod opts' contents) WriterState{  stRefs  = []
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
  case runParser olMarker defaultParserState "para start" (take 10 str) of
         Left  _  -> False
         Right _  -> True

-- | Convert Pandoc block element to PseudoPod.
blockToPseudoPod :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState Doc 
blockToPseudoPod _ Null = return empty
blockToPseudoPod opts (Plain inlines) = do
  contents <- inlineListToPseudoPod opts inlines
  return $ contents


blockToPseudoPod opts (Para inlines) = do
  contents <- inlineListToPseudoPod opts inlines
  -- escape if para starts with ordered list marker
  st <- get
  let esc = if (not (writerStrictMarkdown opts)) &&
               not (stPlain st) &&
               beginsWithOrderedListMarker (render Nothing contents)
               then text "\\"
               else empty
  return $ esc <> contents

blockToPseudoPod _ (RawBlock f str)
  | f == "html" || f == "latex" || f == "tex" || f == "PseudoPod" = do
    st <- get
    if stPlain st
       then return empty
       else return $ text str <> text "\n"
blockToPseudoPod _ (RawBlock _ _) = return empty

-- | No horizontal rules, leave a space
blockToPseudoPod _ HorizontalRule =
  return $ blankline <> blankline

-- | =headN <content> - DONE
blockToPseudoPod opts (Header level inlines) = do
  contents <- inlineListToPseudoPod opts inlines
  return $ "=head" <> text (show level) <> " "  <> contents

{-
blockToPseudoPod opts (CodeBlock (_,classes,_) str)
  | "haskell" `elem` classes && "literate" `elem` classes &&
    writerLiterateHaskell opts =
  return $ prefixed "> " (text str) <> blankline
-}

-- | indent 3 spaces - DONE
blockToPseudoPod _ (CodeBlock _ str) = return $
  nest 3 (text str)

{-
  if writerStrictMarkdown opts || attribs == nullAttr
     then nest (writerTabStop opts) (text str) <> blankline
     else -- use delimited code block
          flush (tildes <> space <> attrs <> cr <> text str <>
                  cr <> tildes) <> blankline
            where tildes  = text "~~~~"
                  attrs = attrsToPseudoPod attribs
-}

-- | =begin blockquote ?
blockToPseudoPod opts (BlockQuote blocks) = do
  contents <- blockListToPseudoPod opts blocks
  return $ "=begin blockquote" <> blankline <> contents <> blankline <> "=end blockquote"

-- | =begin table <caption> / =headrow / =row / =cell / =bodyrows / =row / =cell / =end table
blockToPseudoPod opts (Table caption _ _ headers rows) =  do
  caption' <- inlineListToPseudoPod opts caption
  headers' <- mapM (blockListToPseudoPod opts) headers

  rawRows <- mapM (mapM (blockListToPseudoPod opts)) rows
  let makeRow = hcat . intersperse (text("\n\n=cell "))
  let makeRowBlock = hcat . intersperse (text("\n\n=row\n\n=cell "))

  let rows' = map makeRow rawRows
  let rows'' = makeRowBlock rows'
  let rows''' = "=row" <> blankline <> "=cell " <> rows''

  let head' = makeRow headers'
  let head'' = if all null headers
                  then empty
                  else "=headrow" <> blankline <> "=row" <> blankline <> "=cell " <> head'

  return $ "=begin table " <> caption' <> blankline <> head'' <> blankline <> "=bodyrows" <> blankline <> rows''' <> blankline <> "=end table"

-- | =over / =item * / =back
blockToPseudoPod opts (BulletList items) = do
  contents <- mapM (bulletListItemToPseudoPod opts) items
  return $ "=over" <> blankline <> cat contents <> blankline <> "=back"


-- | =over / =item N. / =back
blockToPseudoPod opts (OrderedList attribs items) = do
  let markers  = orderedListMarkers attribs
  let markers' = map (\m -> if length m < 3
                               then m ++ replicate (3 - length m) ' '
                               else m) markers
  contents <- mapM (\(item, num) -> orderedListItemToPseudoPod opts item num) $
              zip markers' items
  return $ "=over" <> blankline <> cat contents <> blankline <> "=back"
--  return $ cat contents <> blankline

blockToPseudoPod opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToPseudoPod opts) items
  return $ "=over" <> blankline <> cat contents <> blankline <> "=back"

-- | bullet list -> =item *
bulletListItemToPseudoPod :: WriterOptions -> [Block] -> State WriterState Doc
bulletListItemToPseudoPod opts items = do
  contents <- blockListToPseudoPod opts items
  return $ "=item *" <> blankline <> contents <> blankline

-- | Convert ordered list item (a list of blocks) to PseudoPod.
orderedListItemToPseudoPod :: WriterOptions -- ^ options
                          -> String        -- ^ list item marker
                          -> [Block]       -- ^ list item (list of blocks)
                          -> State WriterState Doc
orderedListItemToPseudoPod opts marker items = do
  contents <- blockListToPseudoPod opts items
  return $ "=item " <> (text marker) <> blankline <> contents <> blankline

{-
  let sps = case length marker - writerTabStop opts of
                   n | n > 0 -> text $ replicate n ' '
                   _         -> text " "
  let start = text marker <> sps
  return $ hang (writerTabStop opts) start $ contents <> cr
-}

{-
-- | Convert definition list item (label, list of blocks) to markdown.
definitionListItemToPseudoPod :: WriterOptions
                             -> ([Inline],[[Block]]) 
                             -> State WriterState Doc
definitionListItemToPseudoPod opts (label, defs) = do
  labelText <- inlineListToPseudoPod opts label
  let tabStop = writerTabStop opts
  st <- get
  let leader  = if stPlain st then "   " else "  ~"
  let sps = case writerTabStop opts - 3 of
                 n | n > 0   -> text $ replicate n ' '
                 _           -> text " "
  defs' <- mapM (mapM (blockToPseudoPod opts)) defs
  let contents = vcat $ map (\d -> hang tabStop (leader <> sps) $ vcat d <> cr) defs'
  return $ labelText <> cr <> contents <> cr
-}

-- | Convert definition list item (label, list of blocks) to PseudoPod.
-- | =over / =item FOO / para / =back
definitionListItemToPseudoPod :: WriterOptions
                             -> ([Inline],[[Block]]) 
                             -> State WriterState Doc
definitionListItemToPseudoPod opts (label, defs) = do
  labelText <- inlineListToPseudoPod opts label
  -- each call to definitonListItemToPseudoPod is one label, which can have multiple defintions.  (Each definition is a [Block].)

  defs' <- mapM (blockListToPseudoPod opts) defs
--  let contents = vcat $ map (\d -> "=item") $ vcat d <> cr) defs'
  return $ "=item " <> labelText <> blankline <> vcat defs' <> blankline


-- | Convert list of Pandoc block elements to PseudoPod.
blockListToPseudoPod :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState Doc 
blockListToPseudoPod opts blocks = do
  contents <-  mapM (blockToPseudoPod opts) (blocks)
  let makeBlank = hcat . intersperse (text("\n\n"))
  let contents' = makeBlank contents
  return $ contents'
-- >>= return . cat . intersperse (blankline)

-- | Convert list of Pandoc inline elements to PseudoPod.
inlineListToPseudoPod :: WriterOptions -> [Inline] -> State WriterState Doc
inlineListToPseudoPod opts lst =
  mapM (inlineToPseudoPod opts) lst >>= return . cat

escapeSpaces :: Inline -> Inline
escapeSpaces (Str s) = Str $ substitute " " "\\ " s
escapeSpaces Space = Str "\\ "
escapeSpaces x = x

-- | Convert Pandoc inline element to PseudoPod.
inlineToPseudoPod :: WriterOptions -> Inline -> State WriterState Doc

-- | I< > - DONE
inlineToPseudoPod opts (Emph lst) = do 
  contents <- inlineListToPseudoPod opts lst
  return $ "I<" <> contents <> ">"

-- | B< > - DONE
inlineToPseudoPod opts (Strong lst) = do
  contents <- inlineListToPseudoPod opts lst
  return $ "B<" <> contents <> ">"

inlineToPseudoPod opts (Strikeout lst) = inlineListToPseudoPod opts lst
{-
inlineToPseudoPod opts (Strikeout lst) = do
  contents <- inlineListToPseudoPod opts lst
  return $ "~~" <> contents <> "~~"
-}

-- | G<>
inlineToPseudoPod opts (Superscript lst) = do
  let lst' = bottomUp escapeSpaces lst
  contents <- inlineListToPseudoPod opts lst'
  return $ "G<" <> contents <> ">"

-- | H<>
inlineToPseudoPod opts (Subscript lst) = do
  let lst' = bottomUp escapeSpaces lst
  contents <- inlineListToPseudoPod opts lst'
  return $ "H<" <> contents <> ">"

-- | FIXME - doesnt exist
inlineToPseudoPod opts (SmallCaps lst) = inlineListToPseudoPod opts lst

inlineToPseudoPod opts (Quoted SingleQuote lst) = do
  contents <- inlineListToPseudoPod opts lst
  return $ "‘" <> contents <> "’"
inlineToPseudoPod opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToPseudoPod opts lst
  return $ "“" <> contents <> "”"

-- | C<>
inlineToPseudoPod _ (Code _ str) =
  return $ "C<< " <> text (escapeString str) <> " >>"

inlineToPseudoPod _ (Str str) = do
  st <- get
  if stPlain st
     then return $ text str
     else return $ text $ escapeString str

-- | No Math
-- inlineToPseudoPod opts (Math _ str) = inlineToPseudoPod opts Inline str

inlineToPseudoPod _ (Math InlineMath str) =
  return $ "$" <> text str <> "$"
inlineToPseudoPod _ (Math DisplayMath str) =
  return $ "$$" <> text str <> "$$"

inlineToPseudoPod _ (RawInline f str)
  | f == "html" || f == "latex" || f == "tex" || f == "PseudoPod" =
    return $ text str
inlineToPseudoPod _ (RawInline _ _) = return empty
inlineToPseudoPod opts (LineBreak) = return $
  if writerStrictMarkdown opts
     then "  " <> cr
     else "\\" <> cr
inlineToPseudoPod _ Space = return space
inlineToPseudoPod opts (Cite (c:cs) lst)
  | writerCiteMethod opts == Citeproc = inlineListToPseudoPod opts lst
  | citationMode c == AuthorInText = do
    suffs <- inlineListToPseudoPod opts $ citationSuffix c
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
           pdoc <- inlineListToPseudoPod opts pinlines
           sdoc <- inlineListToPseudoPod opts sinlines
           let k' = text (modekey m ++ "@" ++ k)
               r = case sinlines of
                        Str (y:_):_ | y `elem` ",;]@" -> k' <> sdoc
                        _                             -> k' <+> sdoc
           return $ pdoc <+> r
        modekey SuppressAuthor = "-"
        modekey _              = ""
inlineToPseudoPod _ (Cite _ _) = return $ text ""

-- | L<> - DONE
inlineToPseudoPod opts (Link txt (src, _)) = do
  label <- inlineListToPseudoPod opts txt
  let linktext = if null txt
                   then empty
                   else label <> (text "|")
  return $ "L<" <> linktext <> (text src) <> ">"

inlineToPseudoPod opts (Image alternate (source, tit)) = do
  let txt = if (null alternate) || (alternate == [Str ""]) || 
               (alternate == [Str source]) -- to prevent autolinks
               then [Str "image"]
               else alternate
  linkPart <- inlineToPseudoPod opts (Link txt (source, tit))
  return $ "!" <> linkPart

-- | N<>
-- inlineToPseudoPod :: WriterOptions -> Inline -> State WriterState Doc
-- We need the converted blocks not to end in a blankline, else we get blankline ">", which the pseudopod parser does not like
inlineToPseudoPod opts (Note (block:_)) = do 
--  contents <- mapM (blockToPseudoPod opts) blocks
  contents <- blockToPseudoPod opts block
  return $ "N<" <> contents <> ">"
--  return $ "N<" <> cat contents <> ">"
 