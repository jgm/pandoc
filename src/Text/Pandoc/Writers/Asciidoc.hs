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
   Module      : Text.Pandoc.Writers.Asciidoc
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to asciidoc.

Note that some information may be lost in conversion, due to
expressive limitations of asciidoc.  Footnotes and table cells with
paragraphs (or other block items) are not possible in asciidoc.
If pandoc encounters one of these, it will insert a message indicating
that it has omitted the construct.

Asciidoc:  <http://www.methods.co.nz/asciidoc/>
-}
module Text.Pandoc.Writers.Asciidoc (writeAsciidoc) where
import Text.Pandoc.Definition
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Shared
import Text.Pandoc.Parsing hiding (blankline)
import Text.ParserCombinators.Parsec ( runParser, GenParser )
import Data.List ( isPrefixOf, intersperse, intercalate )
import Text.Pandoc.Pretty
import Control.Monad.State

data WriterState = WriterState { defListMarker :: String
                               , orderedListLevel :: Int
                               , bulletListLevel  :: Int
                               }

-- | Convert Pandoc to Asciidoc.
writeAsciidoc :: WriterOptions -> Pandoc -> String
writeAsciidoc opts document =
  evalState (pandocToAsciidoc opts document) WriterState{
      defListMarker = "::"
    , orderedListLevel = 1
    , bulletListLevel = 1
    }

-- | Return markdown representation of document.
pandocToAsciidoc :: WriterOptions -> Pandoc -> State WriterState String
pandocToAsciidoc opts (Pandoc (Meta title authors date) blocks) = do
  title' <- inlineListToAsciidoc opts title
  let title'' = title' $$ text (replicate (offset title') '=')
  authors' <- mapM (inlineListToAsciidoc opts) authors
  -- asciidoc only allows a singel author
  date' <- inlineListToAsciidoc opts date
  let titleblock = not $ null title && null authors && null date
  body <- blockListToAsciidoc opts blocks
  let colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
  let main = render colwidth body
  let context  = writerVariables opts ++
                 [ ("body", main)
                 , ("title", render colwidth title'')
                 , ("date", render colwidth date')
                 ] ++
                 [ ("toc", "yes") | writerTableOfContents opts &&
                                    writerStandalone opts ] ++
                 [ ("titleblock", "yes") | titleblock ] ++
                 [ ("author", render colwidth a) | a <- authors' ]
  if writerStandalone opts
     then return $ renderTemplate context $ writerTemplate opts
     else return main

-- | Escape special characters for Asciidoc.
escapeString :: String -> String
escapeString = escapeStringUsing markdownEscapes
  where markdownEscapes = backslashEscapes "\\`*_>#~^{+"

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
blockToAsciidoc :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState Doc
blockToAsciidoc _ Null = return empty
blockToAsciidoc opts (Plain inlines) = do
  contents <- inlineListToAsciidoc opts inlines
  return $ contents <> cr
blockToAsciidoc opts (Para inlines) = do
  contents <- inlineListToAsciidoc opts inlines
  -- escape if para starts with ordered list marker
  let esc = if beginsWithOrderedListMarker (render Nothing contents)
               then text "\\"
               else empty
  return $ esc <> contents <> blankline
blockToAsciidoc _ (RawBlock f str) | f == "html" || f == "docbook" = do
    return $ blankline $$ "+++" $$ text str $$ "+++" <> blankline
blockToAsciidoc _ (RawBlock _ _) = return empty
blockToAsciidoc _ HorizontalRule =
  return $ blankline <> text "'''''" <> blankline
blockToAsciidoc opts (Header level inlines) = do
  contents <- inlineListToAsciidoc opts inlines
  let len = offset contents
  return $ contents <> cr <>
         (case level of
               1  -> text $ replicate len '-'
               2  -> text $ replicate len '~'
               3  -> text $ replicate len '^'
               4  -> text $ replicate len '+'
               _  -> empty) <> blankline
blockToAsciidoc _ (CodeBlock (_,classes,_) str) = return $
  flush (attrs <> dashes <> space <> attrs <> cr <> text str <>
           cr <> dashes) <> blankline
     where dashes  = text $ replicate (maximum $ map length $ lines str) '-'
           attrs = if null classes
                      then empty
                      else text $ intercalate "," $ "code" : classes
blockToAsciidoc opts (BlockQuote blocks) = do
  contents <- blockListToAsciidoc opts blocks
  let isBlock (BlockQuote _) = True
      isBlock _              = False
  -- if there are nested block quotes, put in an open block
  let contents' = if any isBlock blocks
                     then "--" $$ contents $$ "--"
                     else contents
  let cols = offset contents'
  let bar = text $ replicate cols '_'
  return $ bar $$ chomp contents' $$ bar <> blankline
blockToAsciidoc opts (Table caption aligns widths headers rows) =  do
  caption' <- inlineListToAsciidoc opts caption
  let caption'' = if null caption
                     then empty
                     else "." <> caption' <> cr
  let isSimple = all (== 0) widths
  let relativePercentWidths = if isSimple
                                 then widths
                                 else map (/ (sum widths)) widths
  let widths'' :: [Integer]
      widths'' = map (floor . (* 100)) relativePercentWidths
  -- ensure that the widths sum to 100
  let widths' = case widths'' of
                     _ | isSimple -> widths''
                     (w:ws) | sum (w:ws) < 100
                               -> (100 - sum ws) : ws
                     ws        -> ws
  let totalwidth :: Integer
      totalwidth = floor $ sum widths * 100
  let colspec al wi = (case al of
                         AlignLeft    -> "<"
                         AlignCenter  -> "^"
                         AlignRight   -> ">"
                         AlignDefault -> "") ++
                      if wi == 0 then "" else (show wi ++ "%")
  let headerspec = if all null headers
                      then empty
                      else text "options=\"header\","
  let widthspec = if totalwidth == 0
                     then empty
                     else text "width="
                          <> doubleQuotes (text $ show totalwidth ++ "%")
                          <> text ","
  let tablespec = text "["
         <> widthspec
         <> text "cols="
         <> doubleQuotes (text $ intercalate ","
             $ zipWith colspec aligns widths')
         <> text ","
         <> headerspec <> text "]"
  let makeCell [Plain x] = do d <- blockListToAsciidoc opts [Plain x]
                              return $ text "|" <> chomp d
      makeCell [Para x]  = makeCell [Plain x]
      makeCell _         = return $ text "|" <> "[multiblock cell omitted]"
  let makeRow cells = hsep `fmap` mapM makeCell cells
  rows' <- mapM makeRow rows
  head' <- makeRow headers
  let head'' = if all null headers then empty else head'
  let colwidth = if writerWrapText opts
                    then writerColumns opts
                    else 100000
  let maxwidth = maximum $ map offset (head':rows')
  let body = if maxwidth > colwidth then vsep rows' else vcat rows'
  let border = text $ "|" ++ replicate ((min maxwidth colwidth) - 1) '='
  return $
    caption'' $$ tablespec $$ border $$ head'' $$ body $$ border $$ blankline
blockToAsciidoc opts (BulletList items) = do
  contents <- mapM (bulletListItemToAsciidoc opts) items
  return $ cat contents <> blankline
blockToAsciidoc opts (OrderedList (start, sty, _delim) items) = do
  let markers  = orderedListMarkers (start, sty, Period)
  let markers' = map (\m -> if length m < 3
                               then m ++ replicate (3 - length m) ' '
                               else m) markers
  contents <- mapM (\(item, num) -> orderedListItemToAsciidoc opts item num) $
              zip markers' items
  return $ cat contents <> blankline
blockToAsciidoc opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToAsciidoc opts) items
  return $ cat contents <> blankline

-- | Convert bullet list item (list of blocks) to markdown.
bulletListItemToAsciidoc :: WriterOptions -> [Block] -> State WriterState Doc
bulletListItemToAsciidoc opts blocks = do
  let addBlock :: Doc -> Block -> State WriterState Doc
      addBlock d b | isEmpty d    = chomp `fmap` blockToAsciidoc opts b
      addBlock d b@(BulletList _) = do x <- blockToAsciidoc opts b
                                       return $ d <> cr <> chomp x
      addBlock d b@(OrderedList _ _) = do x <- blockToAsciidoc opts b
                                          return $ d <> cr <> chomp x
      addBlock d b = do x <- blockToAsciidoc opts b
                        return $ d <> cr <> text "+" <> cr <> chomp x
  lev <- bulletListLevel `fmap` get
  modify $ \s -> s{ bulletListLevel = lev + 1 }
  contents <- foldM addBlock empty blocks
  modify $ \s -> s{ bulletListLevel = lev }
  let marker = text (replicate lev '*')
  return $ marker <> space <> contents <> cr

-- | Convert ordered list item (a list of blocks) to markdown.
orderedListItemToAsciidoc :: WriterOptions -- ^ options
                          -> String        -- ^ list item marker
                          -> [Block]       -- ^ list item (list of blocks)
                          -> State WriterState Doc
orderedListItemToAsciidoc opts marker blocks = do
  let addBlock :: Doc -> Block -> State WriterState Doc
      addBlock d b | isEmpty d    = chomp `fmap` blockToAsciidoc opts b
      addBlock d b@(BulletList _) = do x <- blockToAsciidoc opts b
                                       return $ d <> cr <> chomp x
      addBlock d b@(OrderedList _ _) = do x <- blockToAsciidoc opts b
                                          return $ d <> cr <> chomp x
      addBlock d b = do x <- blockToAsciidoc opts b
                        return $ d <> cr <> text "+" <> cr <> chomp x
  lev <- orderedListLevel `fmap` get
  modify $ \s -> s{ orderedListLevel = lev + 1 }
  contents <- foldM addBlock empty blocks
  modify $ \s -> s{ orderedListLevel = lev }
  return $ text marker <> space <> contents <> cr

-- | Convert definition list item (label, list of blocks) to markdown.
definitionListItemToAsciidoc :: WriterOptions
                             -> ([Inline],[[Block]])
                             -> State WriterState Doc
definitionListItemToAsciidoc opts (label, defs) = do
  labelText <- inlineListToAsciidoc opts label
  marker <- defListMarker `fmap` get
  if marker == "::"
     then modify (\st -> st{ defListMarker = ";;"})
     else modify (\st -> st{ defListMarker = "::"})
  let divider = cr <> text "+" <> cr
  let defsToAsciidoc :: [Block] -> State WriterState Doc
      defsToAsciidoc ds = (vcat . intersperse divider . map chomp)
           `fmap` mapM (blockToAsciidoc opts) ds
  defs' <- mapM defsToAsciidoc defs
  modify (\st -> st{ defListMarker = marker })
  let contents = nest 2 $ vcat $ intersperse divider $ map chomp defs'
  return $ labelText <> text marker <> cr <> contents <> cr

-- | Convert list of Pandoc block elements to markdown.
blockListToAsciidoc :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState Doc
blockListToAsciidoc opts blocks = cat `fmap` mapM (blockToAsciidoc opts) blocks

-- | Convert list of Pandoc inline elements to markdown.
inlineListToAsciidoc :: WriterOptions -> [Inline] -> State WriterState Doc
inlineListToAsciidoc opts lst =
  mapM (inlineToAsciidoc opts) lst >>= return . cat

-- | Convert Pandoc inline element to markdown.
inlineToAsciidoc :: WriterOptions -> Inline -> State WriterState Doc
inlineToAsciidoc opts (Emph lst) = do
  contents <- inlineListToAsciidoc opts lst
  return $ "_" <> contents <> "_"
inlineToAsciidoc opts (Strong lst) = do
  contents <- inlineListToAsciidoc opts lst
  return $ "*" <> contents <> "*"
inlineToAsciidoc opts (Strikeout lst) = do
  contents <- inlineListToAsciidoc opts lst
  return $ "[line-through]*" <> contents <> "*"
inlineToAsciidoc opts (Superscript lst) = do
  contents <- inlineListToAsciidoc opts lst
  return $ "^" <> contents <> "^"
inlineToAsciidoc opts (Subscript lst) = do
  contents <- inlineListToAsciidoc opts lst
  return $ "~" <> contents <> "~"
inlineToAsciidoc opts (SmallCaps lst) = inlineListToAsciidoc opts lst
inlineToAsciidoc opts (Quoted SingleQuote lst) = do
  contents <- inlineListToAsciidoc opts lst
  return $ "`" <> contents <> "'"
inlineToAsciidoc opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToAsciidoc opts lst
  return $ "``" <> contents <> "''"
inlineToAsciidoc _ EmDash = return "\8212"
inlineToAsciidoc _ EnDash = return "\8211"
inlineToAsciidoc _ Apostrophe = return "\8217"
inlineToAsciidoc _ Ellipses = return "\8230"
inlineToAsciidoc _ (Code _ str) = return $
  text "`" <> text (escapeStringUsing (backslashEscapes "`") str) <> "`"
inlineToAsciidoc _ (Str str) = return $ text $ escapeString str
inlineToAsciidoc _ (Math InlineMath str) =
  return $ "latexmath:[$" <> text str <> "$]"
inlineToAsciidoc _ (Math DisplayMath str) =
  return $ "latexmath:[$$" <> text str <> "$$]"
inlineToAsciidoc _ (RawInline _ _) = return empty
inlineToAsciidoc _ (LineBreak) = return $ " +" <> cr
inlineToAsciidoc _ Space = return space
inlineToAsciidoc opts (Cite _ lst) = inlineListToAsciidoc opts lst
inlineToAsciidoc opts (Link txt (src', _tit)) = do
-- relative:  link:downloads/foo.zip[download foo.zip]
-- abs:  http://google.cod[Google]
-- or my@email.com[email john]
  linktext <- inlineListToAsciidoc opts txt
  let src = unescapeURI src'
  let isRelative = ':' `notElem` src
  let prefix = if isRelative
                  then text "link:"
                  else empty
  let srcSuffix = if isPrefixOf "mailto:" src then drop 7 src else src
  let useAuto = case txt of
                      [Code _ s] | s == srcSuffix -> True
                      _                           -> False
  return $ if useAuto
              then text srcSuffix
              else prefix <> text src <> "[" <> linktext <> "]"
inlineToAsciidoc opts (Image alternate (src', tit)) = do
-- image:images/logo.png[Company logo, title="blah"]
  let txt = if (null alternate) || (alternate == [Str ""])
               then [Str "image"]
               else alternate
  linktext <- inlineListToAsciidoc opts txt
  let linktitle = if null tit
                     then empty
                     else text $ ",title=\"" ++ tit ++ "\""
  let src = unescapeURI src'
  return $ "image:" <> text src <> "[" <> linktext <> linktitle <> "]"
inlineToAsciidoc opts (Note [Para inlines]) =
  inlineToAsciidoc opts (Note [Plain inlines])
inlineToAsciidoc opts (Note [Plain inlines]) = do
  contents  <- inlineListToAsciidoc opts inlines
  return $ text "footnote:[" <> contents <> "]"
-- asciidoc can't handle blank lines in notes
inlineToAsciidoc _ (Note _) = return "[multiblock footnote omitted]"
