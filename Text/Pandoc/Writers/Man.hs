{-
Copyright (C) 2007 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.Man 
   Copyright   : Copyright (C) 2007 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to groff man page format.

-}
module Text.Pandoc.Writers.Man ( writeMan) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Readers.TeXMath
import Text.Printf ( printf )
import Data.List ( isPrefixOf, drop, nub, intersperse )
import Text.PrettyPrint.HughesPJ hiding ( Str )
import Control.Monad.State

type Notes = [[Block]]
type Preprocessors = [String] -- e.g. "t" for tbl
type WriterState = (Notes, Preprocessors)

-- | Convert Pandoc to Man.
writeMan :: WriterOptions -> Pandoc -> String
writeMan opts document = render $ evalState (pandocToMan opts document) ([],[]) 

-- | Return groff man representation of document.
pandocToMan :: WriterOptions -> Pandoc -> State WriterState Doc
pandocToMan opts (Pandoc meta blocks) = do
  let before  = writerIncludeBefore opts
  let after   = writerIncludeAfter opts
  let before' = if null before then empty else text before
  let after'  = if null after then empty else text after
  (head, foot) <- metaToMan opts meta
  body <- blockListToMan opts blocks
  (notes, preprocessors) <- get
  let preamble = if null preprocessors || not (writerStandalone opts)
                    then empty
                    else text $ ".\\\" " ++ concat (nub preprocessors)
  notes' <- notesToMan opts (reverse notes)
  return $ preamble $$ head $$ before' $$ body $$ notes' $$ foot $$ after'

-- | Insert bibliographic information into Man header and footer.
metaToMan :: WriterOptions -- ^ Options, including Man header
          -> Meta          -- ^ Meta with bibliographic information
          -> State WriterState (Doc, Doc)
metaToMan options (Meta title authors date) = do
  titleText <- inlineListToMan options title
  let (cmdName, rest) = break (== ' ') $ render titleText
  let (title', section) = case reverse cmdName of
                            (')':d:'(':xs) | d `elem` ['0'..'9'] -> 
                                  (text (reverse xs), char d)
                            xs -> (text (reverse xs), doubleQuotes empty)                    
  let extras = map (doubleQuotes . text . removeLeadingTrailingSpace) $
               splitBy '|' rest
  let head = (text ".TH") <+> title' <+> section <+> 
             doubleQuotes (text date) <+> hsep extras
  let foot = case length authors of
                0 -> empty
                1 -> text ".SH AUTHOR" $$ (text $ joinWithSep ", " authors)
                2 -> text ".SH AUTHORS" $$ (text $ joinWithSep ", " authors)
  return $ if writerStandalone options
              then (head, foot)
              else (empty, empty)

-- | Return man representation of notes.
notesToMan :: WriterOptions -> [[Block]] -> State WriterState Doc
notesToMan opts notes =
  if null notes
     then return empty
     else mapM (\(num, note) -> noteToMan opts num note) (zip [1..] notes) >>= 
          return . (text ".SH NOTES" $$) . vcat

-- | Return man representation of a note.
noteToMan :: WriterOptions -> Int -> [Block] -> State WriterState Doc
noteToMan opts num note = do
  contents <- blockListToMan opts note
  let marker = text "\n.SS [" <> text (show num) <> char ']'
  return $ marker $$ contents 

-- | Association list of characters to escape.
manEscapes :: [(Char, String)]
manEscapes = [('\160', "\\ "), ('\'', "\\[aq]")] ++ backslashEscapes "\".@\\"

-- | Escape special characters for Man.
escapeString :: String -> String
escapeString = escapeStringUsing manEscapes

-- | Escape a literal (code) section for Man.
escapeCode :: String -> String
escapeCode = escapeStringUsing (manEscapes ++ backslashEscapes "\t ")

-- | Convert Pandoc block element to man.
blockToMan :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState Doc 
blockToMan opts Null = return empty
blockToMan opts (Plain inlines) = 
  wrapIfNeeded opts (inlineListToMan opts) inlines
blockToMan opts (Para inlines) = do
  contents <- wrapIfNeeded opts (inlineListToMan opts) inlines
  return $ text ".PP" $$ contents 
blockToMan opts (RawHtml str) = return $ text str
blockToMan opts HorizontalRule = return $ text $ ".PP\n   *   *   *   *   *"
blockToMan opts (Header level inlines) = do
  contents <- inlineListToMan opts inlines
  let heading = case level of
                  1 -> ".SH "
                  _ -> ".SS "
  return $ text heading <> contents 
blockToMan opts (CodeBlock str) = return $
  text ".PP" $$ text "\\f[CR]" $$ 
  text ((unlines . map ("      " ++) . lines) (escapeCode str)) <> text "\\f[]"
blockToMan opts (BlockQuote blocks) = do  
  contents <- blockListToMan opts blocks
  return $ text ".RS" $$ contents $$ text ".RE"
blockToMan opts (Table caption alignments widths headers rows) = 
  let aligncode AlignLeft    = "l"
      aligncode AlignRight   = "r"
      aligncode AlignCenter  = "c"
      aligncode AlignDefault = "l"
  in do
  caption' <- inlineListToMan opts caption
  modify (\(notes, preprocessors) -> (notes, "t":preprocessors))
  let iwidths = map (printf "w(%0.2fn)" . (70 *)) widths 
  -- 78n default width - 8n indent = 70n
  let coldescriptions = text $ joinWithSep " " 
                        (zipWith (\align width -> aligncode align ++ width) 
                        alignments iwidths) ++ "."
  colheadings <- mapM (blockListToMan opts) headers
  let makeRow cols = text "T{" $$ 
                     (vcat $ intersperse (text "T}@T{") cols) $$ 
                     text "T}"
  let colheadings' = makeRow colheadings
  body <- mapM (\row -> do 
                         cols <- mapM (blockListToMan opts) row
                         return $ makeRow cols) rows
  return $ text ".PP" $$ caption' $$ 
           text ".TS" $$ text "tab(@);" $$ coldescriptions $$ 
           colheadings' $$ char '_' $$ vcat body $$ text ".TE"

blockToMan opts (BulletList items) = do
  contents <- mapM (bulletListItemToMan opts) items
  return (vcat contents) 
blockToMan opts (OrderedList attribs items) = do
  let markers = take (length items) $ orderedListMarkers attribs  
  let indent = 1 + (maximum $ map length markers)
  contents <- mapM (\(num, item) -> orderedListItemToMan opts num indent item) $
              zip markers items  
  return (vcat contents)
blockToMan opts (DefinitionList items) = do  
  contents <- mapM (definitionListItemToMan opts) items
  return (vcat contents)

-- | Convert bullet list item (list of blocks) to man.
bulletListItemToMan :: WriterOptions -> [Block] -> State WriterState Doc
bulletListItemToMan opts [] = return empty
bulletListItemToMan opts ((Para first):rest) = 
  bulletListItemToMan opts ((Plain first):rest)
bulletListItemToMan opts ((Plain first):rest) = do
  first' <- blockToMan opts (Plain first) 
  rest' <- blockListToMan opts rest
  let first'' = text ".IP \\[bu] 2" $$ first'
  let rest''  = if null rest
                   then empty
                   else text ".RS 2" $$ rest' $$ text ".RE"
  return (first'' $$ rest'') 
bulletListItemToMan opts (first:rest) = do
  first' <- blockToMan opts first
  rest' <- blockListToMan opts rest
  return $ text "\\[bu] .RS 2" $$ first' $$ rest' $$ text ".RE"
 
-- | Convert ordered list item (a list of blocks) to man.
orderedListItemToMan :: WriterOptions -- ^ options
                          -> String   -- ^ order marker for list item
                          -> Int      -- ^ number of spaces to indent
                          -> [Block]  -- ^ list item (list of blocks)
                          -> State WriterState Doc
orderedListItemToMan _ _ _ [] = return empty
orderedListItemToMan opts num indent ((Para first):rest) = 
  orderedListItemToMan opts num indent ((Plain first):rest)
orderedListItemToMan opts num indent (first:rest) = do
  first' <- blockToMan opts first
  rest' <- blockListToMan opts rest
  let num' = printf ("%" ++ show (indent - 1) ++ "s") num
  let first'' = text (".IP \"" ++ num' ++ "\" " ++ show indent) $$ first'
  let rest''  = if null rest
                   then empty
                   else text ".RS 4" $$ rest' $$ text ".RE"
  return $ first'' $$ rest'' 

-- | Convert definition list item (label, list of blocks) to man.
definitionListItemToMan :: WriterOptions
                             -> ([Inline],[Block]) 
                             -> State WriterState Doc
definitionListItemToMan opts (label, items) = do
  labelText <- inlineListToMan opts label
  contents <- if null items
                 then return empty
                 else do 
                        let (first, rest) = case items of
                             ((Para x):y) -> (Plain x,y)
                             (x:y)        -> (x,y)
                        rest' <- mapM (\item -> blockToMan opts item)
                                 rest >>= (return . vcat)
                        first' <- blockToMan opts first
                        return $ first' $$ text ".RS" $$ rest' $$ text ".RE"
  return $ text ".TP\n.B " <> labelText $+$ contents

-- | Convert list of Pandoc block elements to man.
blockListToMan :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState Doc 
blockListToMan opts blocks =
  mapM (blockToMan opts) blocks >>= (return . vcat)

-- | Convert list of Pandoc inline elements to man.
inlineListToMan :: WriterOptions -> [Inline] -> State WriterState Doc
inlineListToMan opts lst = mapM (inlineToMan opts) lst >>= (return . hcat)

-- | Convert Pandoc inline element to man.
inlineToMan :: WriterOptions -> Inline -> State WriterState Doc
inlineToMan opts (Emph lst) = do 
  contents <- inlineListToMan opts lst
  return $ text "\\f[I]" <> contents <> text "\\f[]"
inlineToMan opts (Strong lst) = do
  contents <- inlineListToMan opts lst
  return $ text "\\f[B]" <> contents <> text "\\f[]"
inlineToMan opts (Strikeout lst) = do
  contents <- inlineListToMan opts lst
  return $ text "[STRIKEOUT:" <> contents <> char ']'
inlineToMan opts (Superscript lst) = do
  contents <- inlineListToMan opts lst
  return $ char '^' <> contents <> char '^'
inlineToMan opts (Subscript lst) = do
  contents <- inlineListToMan opts lst
  return $ char '~' <> contents <> char '~'
inlineToMan opts (Quoted SingleQuote lst) = do
  contents <- inlineListToMan opts lst
  return $ char '`' <> contents <> char '\''
inlineToMan opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToMan opts lst
  return $ text "\\[lq]" <> contents <> text "\\[rq]"
inlineToMan opts EmDash = return $ text "\\[em]"
inlineToMan opts EnDash = return $ text "\\[en]"
inlineToMan opts Apostrophe = return $ char '\''
inlineToMan opts Ellipses = return $ text "\\&..."
inlineToMan opts (Code str) =
  return $ text $ "\\f[B]" ++ escapeCode str ++ "\\f[]"
inlineToMan opts (Str str) = return $ text $ escapeString str
inlineToMan opts (Math str) = inlineToMan opts (Code str)
inlineToMan opts (TeX str) = return empty
inlineToMan opts (HtmlInline str) = return $ text $ escapeCode str 
inlineToMan opts (LineBreak) = return $ text "\n.PD 0\n.P\n.PD\n"
inlineToMan opts Space = return $ char ' '
inlineToMan opts (Link txt (src, _)) = do
  linktext <- inlineListToMan opts txt
  let srcSuffix = if isPrefixOf "mailto:" src then drop 7 src else src
  return $ if txt == [Code srcSuffix]
              then char '<' <> text srcSuffix <> char '>' 
              else linktext <> text " (" <> text src <> char ')' 
inlineToMan opts (Image alternate (source, tit)) = do
  let txt = if (null alternate) || (alternate == [Str ""]) || 
               (alternate == [Str source]) -- to prevent autolinks
               then [Str "image"]
               else alternate
  linkPart <- inlineToMan opts (Link txt (source, tit)) 
  return $ char '[' <> text "IMAGE: " <> linkPart <> char ']'
inlineToMan opts (Note contents) = do 
  modify (\(notes, prep) -> (contents:notes, prep)) -- add to notes in state
  (notes, _) <- get
  let ref = show $ (length notes)
  return $ char '[' <> text ref <> char ']'

