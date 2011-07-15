{-
Copyright (C) 2007-2010 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2007-2010 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to groff man page format.

-}
module Text.Pandoc.Writers.Man ( writeMan) where
import Text.Pandoc.Definition
import Text.Pandoc.Templates
import Text.Pandoc.Shared
import Text.Pandoc.Readers.TeXMath
import Text.Printf ( printf )
import Data.List ( isPrefixOf, intersperse, intercalate )
import Text.Pandoc.Pretty
import Control.Monad.State

type Notes = [[Block]]
data WriterState = WriterState { stNotes  :: Notes
                               , stHasTables :: Bool }

-- | Convert Pandoc to Man.
writeMan :: WriterOptions -> Pandoc -> String
writeMan opts document = evalState (pandocToMan opts document) (WriterState [] False) 

-- | Return groff man representation of document.
pandocToMan :: WriterOptions -> Pandoc -> State WriterState String
pandocToMan opts (Pandoc (Meta title authors date) blocks) = do
  titleText <- inlineListToMan opts title
  authors' <- mapM (inlineListToMan opts) authors
  date' <- inlineListToMan opts date 
  let colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
  let render' = render colwidth
  let (cmdName, rest) = break (== ' ') $ render' titleText
  let (title', section) = case reverse cmdName of
                            (')':d:'(':xs) | d `elem` ['0'..'9'] -> 
                                  (text (reverse xs), char d)
                            xs -> (text (reverse xs), doubleQuotes empty)
  let description = hsep $
                    map (doubleQuotes . text . removeLeadingTrailingSpace) $
                    splitBy (== '|') rest
  body <- blockListToMan opts blocks
  notes <- liftM stNotes get
  notes' <- notesToMan opts (reverse notes)
  let main = render' $ body $$ notes' $$ text ""
  hasTables <- liftM stHasTables get
  let context  = writerVariables opts ++
                 [ ("body", main)
                 , ("title", render' title')
                 , ("section", render' section)
                 , ("date", render' date')
                 , ("description", render' description) ] ++
                 [ ("has-tables", "yes") | hasTables ] ++
                 [ ("author", render' a) | a <- authors' ]
  if writerStandalone opts
     then return $ renderTemplate context $ writerTemplate opts
     else return main

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
  let marker = cr <> text ".SS " <> brackets (text (show num))
  return $ marker $$ contents 

-- | Association list of characters to escape.
manEscapes :: [(Char, String)]
manEscapes = [('\160', "\\ "), ('\'', "\\[aq]")] ++ backslashEscapes "@\\"

-- | Escape special characters for Man.
escapeString :: String -> String
escapeString = escapeStringUsing manEscapes

-- | Escape a literal (code) section for Man.
escapeCode :: String -> String
escapeCode = escapeStringUsing (manEscapes ++ backslashEscapes "\t ")

-- We split inline lists into sentences, and print one sentence per
-- line.  groff/troff treats the line-ending period differently.
-- See http://code.google.com/p/pandoc/issues/detail?id=148.

-- | Returns the first sentence in a list of inlines, and the rest.
breakSentence :: [Inline] -> ([Inline], [Inline])
breakSentence [] = ([],[])
breakSentence xs =
  let isSentenceEndInline (Str ".") = True
      isSentenceEndInline (Str "?") = True
      isSentenceEndInline _         = False
      (as, bs) = break isSentenceEndInline xs
  in  case bs of
           []             -> (as, [])
           [c]            -> (as ++ [c], [])
           (c:Space:cs)   -> (as ++ [c], cs)
           (Str ".":Str ")":cs) -> (as ++ [Str ".", Str ")"], cs)
           (c:cs)         -> (as ++ [c] ++ ds, es)
              where (ds, es) = breakSentence cs

-- | Split a list of inlines into sentences.
splitSentences :: [Inline] -> [[Inline]]
splitSentences xs =
  let (sent, rest) = breakSentence xs
  in  if null rest then [sent] else sent : splitSentences rest

-- | Convert Pandoc block element to man.
blockToMan :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState Doc 
blockToMan _ Null = return empty
blockToMan opts (Plain inlines) = 
  liftM vcat $ mapM (inlineListToMan opts) $ splitSentences inlines
blockToMan opts (Para inlines) = do
  contents <- liftM vcat $ mapM (inlineListToMan opts) $
    splitSentences inlines
  return $ text ".PP" $$ contents 
blockToMan _ (RawBlock "man" str) = return $ text str
blockToMan _ (RawBlock _ _) = return empty
blockToMan _ HorizontalRule = return $ text ".PP" $$ text "   *   *   *   *   *"
blockToMan opts (Header level inlines) = do
  contents <- inlineListToMan opts inlines
  let heading = case level of
                  1 -> ".SH "
                  _ -> ".SS "
  return $ text heading <> contents 
blockToMan _ (CodeBlock _ str) = return $
  text ".IP" $$
  text ".nf" $$
  text "\\f[C]" $$
  text (escapeCode str) $$
  text "\\f[]" $$
  text ".fi"
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
  modify $ \st -> st{ stHasTables = True }
  let iwidths = if all (== 0) widths
                   then repeat ""
                   else map (printf "w(%0.2fn)" . (70 *)) widths
  -- 78n default width - 8n indent = 70n
  let coldescriptions = text $ intercalate " "
                        (zipWith (\align width -> aligncode align ++ width) 
                        alignments iwidths) ++ "."
  colheadings <- mapM (blockListToMan opts) headers
  let makeRow cols = text "T{" $$ 
                     (vcat $ intersperse (text "T}@T{") cols) $$ 
                     text "T}"
  let colheadings' = if all null headers
                        then empty
                        else makeRow colheadings $$ char '_'
  body <- mapM (\row -> do 
                         cols <- mapM (blockListToMan opts) row
                         return $ makeRow cols) rows
  return $ text ".PP" $$ caption' $$ 
           text ".TS" $$ text "tab(@);" $$ coldescriptions $$ 
           colheadings' $$ vcat body $$ text ".TE"

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
bulletListItemToMan _ [] = return empty
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
                             -> ([Inline],[[Block]]) 
                             -> State WriterState Doc
definitionListItemToMan opts (label, defs) = do
  labelText <- inlineListToMan opts label
  contents <- if null defs 
                 then return empty
                 else liftM vcat $ forM defs $ \blocks -> do 
                        let (first, rest) = case blocks of
                              ((Para x):y) -> (Plain x,y)
                              (x:y)        -> (x,y)
                              []           -> error "blocks is null"
                        rest' <- liftM vcat $
                                  mapM (\item -> blockToMan opts item) rest
                        first' <- blockToMan opts first
                        return $ first' $$ text ".RS" $$ rest' $$ text ".RE"
  return $ text ".TP" $$ text ".B " <> labelText $$ contents

-- | Convert list of Pandoc block elements to man.
blockListToMan :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState Doc 
blockListToMan opts blocks =
  mapM (blockToMan opts) blocks >>= (return . vcat)

-- | Convert list of Pandoc inline elements to man.
inlineListToMan :: WriterOptions -> [Inline] -> State WriterState Doc
-- if list starts with ., insert a zero-width character \& so it
-- won't be interpreted as markup if it falls at the beginning of a line.
inlineListToMan opts lst@(Str "." : _) = mapM (inlineToMan opts) lst >>=
  (return . (text "\\&" <>)  . hcat)
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
inlineToMan opts (SmallCaps lst) = inlineListToMan opts lst -- not supported
inlineToMan opts (Quoted SingleQuote lst) = do
  contents <- inlineListToMan opts lst
  return $ char '`' <> contents <> char '\''
inlineToMan opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToMan opts lst
  return $ text "\\[lq]" <> contents <> text "\\[rq]"
inlineToMan opts (Cite _ lst) =
  inlineListToMan opts lst
inlineToMan _ EmDash = return $ text "\\[em]"
inlineToMan _ EnDash = return $ text "\\[en]"
inlineToMan _ Apostrophe = return $ char '\''
inlineToMan _ Ellipses = return $ text "\\&..."
inlineToMan _ (Code _ str) =
  return $ text $ "\\f[C]" ++ escapeCode str ++ "\\f[]"
inlineToMan _ (Str str) = return $ text $ escapeString str
inlineToMan opts (Math InlineMath str) = inlineListToMan opts $ readTeXMath str
inlineToMan opts (Math DisplayMath str) = do
  contents <- inlineListToMan opts $ readTeXMath str
  return $ cr <> text ".RS" $$ contents $$ text ".RE"
inlineToMan _ (RawInline "man" str) = return $ text str
inlineToMan _ (RawInline _ _) = return empty
inlineToMan _ (LineBreak) = return $
  cr <> text ".PD 0" $$ text ".P" $$ text ".PD" <> cr
inlineToMan _ Space = return space
inlineToMan opts (Link txt (src, _)) = do
  linktext <- inlineListToMan opts txt
  let srcSuffix = if isPrefixOf "mailto:" src then drop 7 src else src
  return $ case txt of
           [Code _ s]
             | s == srcSuffix -> char '<' <> text srcSuffix <> char '>' 
           _                  -> linktext <> text " (" <> text src <> char ')'
inlineToMan opts (Image alternate (source, tit)) = do
  let txt = if (null alternate) || (alternate == [Str ""]) || 
               (alternate == [Str source]) -- to prevent autolinks
               then [Str "image"]
               else alternate
  linkPart <- inlineToMan opts (Link txt (source, tit)) 
  return $ char '[' <> text "IMAGE: " <> linkPart <> char ']'
inlineToMan _ (Note contents) = do 
  -- add to notes in state
  modify $ \st -> st{ stNotes = contents : stNotes st }
  notes <- liftM stNotes get
  let ref = show $ (length notes)
  return $ char '[' <> text ref <> char ']'

