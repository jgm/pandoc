{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2006-2015 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.AsciiDoc
   Copyright   : Copyright (C) 2006-2015 John MacFarlane
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

AsciiDoc:  <http://www.methods.co.nz/asciidoc/>
-}
module Text.Pandoc.Writers.AsciiDoc (writeAsciiDoc) where
import Text.Pandoc.Definition
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (blankline, space)
import Data.Maybe (fromMaybe)
import Data.List ( stripPrefix, intersperse, intercalate )
import Text.Pandoc.Pretty
import Text.Pandoc.ImageSize
import Control.Monad.State
import qualified Data.Map as M
import Data.Aeson (Value(String), fromJSON, toJSON, Result(..))
import qualified Data.Text as T

data WriterState = WriterState { defListMarker :: String
                               , orderedListLevel :: Int
                               , bulletListLevel  :: Int
                               , intraword        :: Bool
                               }

-- | Convert Pandoc to AsciiDoc.
writeAsciiDoc :: WriterOptions -> Pandoc -> String
writeAsciiDoc opts document =
  evalState (pandocToAsciiDoc opts document) WriterState{
      defListMarker = "::"
    , orderedListLevel = 1
    , bulletListLevel = 1
    , intraword = False
    }

-- | Return asciidoc representation of document.
pandocToAsciiDoc :: WriterOptions -> Pandoc -> State WriterState String
pandocToAsciiDoc opts (Pandoc meta blocks) = do
  let titleblock = not $ null (docTitle meta) && null (docAuthors meta) &&
                         null (docDate meta)
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  metadata <- metaToJSON opts
              (fmap (render colwidth) . blockListToAsciiDoc opts)
              (fmap (render colwidth) . inlineListToAsciiDoc opts)
              meta
  let addTitleLine (String t) = String $
         t <> "\n" <> T.replicate (T.length t) "="
      addTitleLine x = x
  let metadata' = case fromJSON metadata of
                        Success m  -> toJSON $ M.adjust addTitleLine
                                                 ("title" :: T.Text) m
                        _          -> metadata
  body <- blockListToAsciiDoc opts blocks
  let main = render colwidth body
  let context  = defField "body" main
               $ defField "toc"
                  (writerTableOfContents opts && writerStandalone opts)
               $ defField "titleblock" titleblock
               $ metadata'
  if writerStandalone opts
     then return $ renderTemplate' (writerTemplate opts) context
     else return main

-- | Escape special characters for AsciiDoc.
escapeString :: String -> String
escapeString = escapeStringUsing escs
  where escs = backslashEscapes "{"

-- | Ordered list start parser for use in Para below.
olMarker :: Parser [Char] ParserState Char
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

-- | Convert Pandoc block element to asciidoc.
blockToAsciiDoc :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState Doc
blockToAsciiDoc _ Null = return empty
blockToAsciiDoc opts (Plain inlines) = do
  contents <- inlineListToAsciiDoc opts inlines
  return $ contents <> blankline
blockToAsciiDoc opts (Para [Image attr alt (src,'f':'i':'g':':':tit)]) = do
  blockToAsciiDoc opts (Para [Image attr alt (src,tit)])
blockToAsciiDoc opts (Para inlines) = do
  contents <- inlineListToAsciiDoc opts inlines
  -- escape if para starts with ordered list marker
  let esc = if beginsWithOrderedListMarker (render Nothing contents)
               then text "\\"
               else empty
  return $ esc <> contents <> blankline
blockToAsciiDoc _ (RawBlock f s)
  | f == "asciidoc" = return $ text s
  | otherwise       = return empty
blockToAsciiDoc _ HorizontalRule =
  return $ blankline <> text "'''''" <> blankline
blockToAsciiDoc opts (Header level (ident,_,_) inlines) = do
  contents <- inlineListToAsciiDoc opts inlines
  let len = offset contents
  -- ident seem to be empty most of the time and asciidoc will generate them automatically
  -- so lets make them not show up when null
  let identifier = if (null ident) then empty else ("[[" <> text ident <> "]]")
  let setext = writerSetextHeaders opts
  return $
         (if setext
            then
              identifier $$ contents $$
              (case level of
               1  -> text $ replicate len '-'
               2  -> text $ replicate len '~'
               3  -> text $ replicate len '^'
               4  -> text $ replicate len '+'
               _  -> empty) <> blankline
            else
              identifier $$ text (replicate level '=') <> space <> contents <> blankline)
blockToAsciiDoc _ (CodeBlock (_,classes,_) str) = return $ (flush $
  if null classes
     then "...." $$ text str $$ "...."
     else attrs $$ "----" $$ text str $$ "----")
  <> blankline
    where attrs = "[" <> text (intercalate "," ("source" : classes)) <> "]"
blockToAsciiDoc opts (BlockQuote blocks) = do
  contents <- blockListToAsciiDoc opts blocks
  let isBlock (BlockQuote _) = True
      isBlock _              = False
  -- if there are nested block quotes, put in an open block
  let contents' = if any isBlock blocks
                     then "--" $$ contents $$ "--"
                     else contents
  let cols = offset contents'
  let bar = text $ replicate cols '_'
  return $ bar $$ chomp contents' $$ bar <> blankline
blockToAsciiDoc opts (Table caption aligns widths headers rows) =  do
  caption' <- inlineListToAsciiDoc opts caption
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
  let makeCell [Plain x] = do d <- blockListToAsciiDoc opts [Plain x]
                              return $ text "|" <> chomp d
      makeCell [Para x]  = makeCell [Plain x]
      makeCell []        = return $ text "|"
      makeCell bs        = do d <- blockListToAsciiDoc opts bs
                              return $ text "a|" $$ d
  let makeRow cells = hsep `fmap` mapM makeCell cells
  rows' <- mapM makeRow rows
  head' <- makeRow headers
  let head'' = if all null headers then empty else head'
  let colwidth = if writerWrapText opts == WrapAuto
                    then writerColumns opts
                    else 100000
  let maxwidth = maximum $ map offset (head':rows')
  let body = if maxwidth > colwidth then vsep rows' else vcat rows'
  let border = text $ "|" ++ replicate (max 5 (min maxwidth colwidth) - 1) '='
  return $
    caption'' $$ tablespec $$ border $$ head'' $$ body $$ border $$ blankline
blockToAsciiDoc opts (BulletList items) = do
  contents <- mapM (bulletListItemToAsciiDoc opts) items
  return $ cat contents <> blankline
blockToAsciiDoc opts (OrderedList (_start, sty, _delim) items) = do
  let sty' = case sty of
                  UpperRoman -> UpperAlpha
                  LowerRoman -> LowerAlpha
                  x          -> x
  let markers  = orderedListMarkers (1, sty', Period)  -- start num not used
  let markers' = map (\m -> if length m < 3
                               then m ++ replicate (3 - length m) ' '
                               else m) markers
  contents <- mapM (\(item, num) -> orderedListItemToAsciiDoc opts item num) $
              zip markers' items
  return $ cat contents <> blankline
blockToAsciiDoc opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToAsciiDoc opts) items
  return $ cat contents <> blankline
blockToAsciiDoc opts (Div (ident,_,_) bs) = do
  let identifier = if (null ident) then empty else ("[[" <> text ident <> "]]")
  contents <- blockListToAsciiDoc opts bs
  return $ identifier $$ contents

-- | Convert bullet list item (list of blocks) to asciidoc.
bulletListItemToAsciiDoc :: WriterOptions -> [Block] -> State WriterState Doc
bulletListItemToAsciiDoc opts blocks = do
  let addBlock :: Doc -> Block -> State WriterState Doc
      addBlock d b | isEmpty d    = chomp `fmap` blockToAsciiDoc opts b
      addBlock d b@(BulletList _) = do x <- blockToAsciiDoc opts b
                                       return $ d <> cr <> chomp x
      addBlock d b@(OrderedList _ _) = do x <- blockToAsciiDoc opts b
                                          return $ d <> cr <> chomp x
      addBlock d b = do x <- blockToAsciiDoc opts b
                        return $ d <> cr <> text "+" <> cr <> chomp x
  lev <- bulletListLevel `fmap` get
  modify $ \s -> s{ bulletListLevel = lev + 1 }
  contents <- foldM addBlock empty blocks
  modify $ \s -> s{ bulletListLevel = lev }
  let marker = text (replicate lev '*')
  return $ marker <> text " " <> contents <> cr

-- | Convert ordered list item (a list of blocks) to asciidoc.
orderedListItemToAsciiDoc :: WriterOptions -- ^ options
                          -> String        -- ^ list item marker
                          -> [Block]       -- ^ list item (list of blocks)
                          -> State WriterState Doc
orderedListItemToAsciiDoc opts marker blocks = do
  let addBlock :: Doc -> Block -> State WriterState Doc
      addBlock d b | isEmpty d    = chomp `fmap` blockToAsciiDoc opts b
      addBlock d b@(BulletList _) = do x <- blockToAsciiDoc opts b
                                       return $ d <> cr <> chomp x
      addBlock d b@(OrderedList _ _) = do x <- blockToAsciiDoc opts b
                                          return $ d <> cr <> chomp x
      addBlock d b = do x <- blockToAsciiDoc opts b
                        return $ d <> cr <> text "+" <> cr <> chomp x
  lev <- orderedListLevel `fmap` get
  modify $ \s -> s{ orderedListLevel = lev + 1 }
  contents <- foldM addBlock empty blocks
  modify $ \s -> s{ orderedListLevel = lev }
  return $ text marker <> text " " <> contents <> cr

-- | Convert definition list item (label, list of blocks) to asciidoc.
definitionListItemToAsciiDoc :: WriterOptions
                             -> ([Inline],[[Block]])
                             -> State WriterState Doc
definitionListItemToAsciiDoc opts (label, defs) = do
  labelText <- inlineListToAsciiDoc opts label
  marker <- defListMarker `fmap` get
  if marker == "::"
     then modify (\st -> st{ defListMarker = ";;"})
     else modify (\st -> st{ defListMarker = "::"})
  let divider = cr <> text "+" <> cr
  let defsToAsciiDoc :: [Block] -> State WriterState Doc
      defsToAsciiDoc ds = (vcat . intersperse divider . map chomp)
           `fmap` mapM (blockToAsciiDoc opts) ds
  defs' <- mapM defsToAsciiDoc defs
  modify (\st -> st{ defListMarker = marker })
  let contents = nest 2 $ vcat $ intersperse divider $ map chomp defs'
  return $ labelText <> text marker <> cr <> contents <> cr

-- | Convert list of Pandoc block elements to asciidoc.
blockListToAsciiDoc :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState Doc
blockListToAsciiDoc opts blocks = cat `fmap` mapM (blockToAsciiDoc opts) blocks

-- | Convert list of Pandoc inline elements to asciidoc.
inlineListToAsciiDoc :: WriterOptions -> [Inline] -> State WriterState Doc
inlineListToAsciiDoc opts lst = do
  oldIntraword <- gets intraword
  setIntraword False
  result <- go lst
  setIntraword oldIntraword
  return result
 where go [] = return empty
       go (y:x:xs)
         | not (isSpacy y) = do
           y' <- if isSpacy x
                    then inlineToAsciiDoc opts y
                    else withIntraword $ inlineToAsciiDoc opts y
           x' <- withIntraword $ inlineToAsciiDoc opts x
           xs' <- go xs
           return (y' <> x' <> xs')
         | not (isSpacy x) = do
           y' <- withIntraword $ inlineToAsciiDoc opts y
           xs' <- go (x:xs)
           return (y' <> xs')
       go (x:xs) = do
           x' <- inlineToAsciiDoc opts x
           xs' <- go xs
           return (x' <> xs')
       isSpacy Space = True
       isSpacy LineBreak = True
       isSpacy SoftBreak = True
       isSpacy _ = False

setIntraword :: Bool -> State WriterState ()
setIntraword b = modify $ \st -> st{ intraword = b }

withIntraword :: State WriterState a -> State WriterState a
withIntraword p = setIntraword True *> p <* setIntraword False

-- | Convert Pandoc inline element to asciidoc.
inlineToAsciiDoc :: WriterOptions -> Inline -> State WriterState Doc
inlineToAsciiDoc opts (Emph lst) = do
  contents <- inlineListToAsciiDoc opts lst
  isIntraword <- gets intraword
  let marker = if isIntraword then "__" else "_"
  return $ marker <> contents <> marker
inlineToAsciiDoc opts (Strong lst) = do
  contents <- inlineListToAsciiDoc opts lst
  isIntraword <- gets intraword
  let marker = if isIntraword then "**" else "*"
  return $ marker <> contents <> marker
inlineToAsciiDoc opts (Strikeout lst) = do
  contents <- inlineListToAsciiDoc opts lst
  return $ "[line-through]*" <> contents <> "*"
inlineToAsciiDoc opts (Superscript lst) = do
  contents <- inlineListToAsciiDoc opts lst
  return $ "^" <> contents <> "^"
inlineToAsciiDoc opts (Subscript lst) = do
  contents <- inlineListToAsciiDoc opts lst
  return $ "~" <> contents <> "~"
inlineToAsciiDoc opts (SmallCaps lst) = inlineListToAsciiDoc opts lst
inlineToAsciiDoc opts (Quoted SingleQuote lst) =
  inlineListToAsciiDoc opts (Str "`" : lst ++ [Str "'"])
inlineToAsciiDoc opts (Quoted DoubleQuote lst) =
  inlineListToAsciiDoc opts (Str "``" : lst ++ [Str "''"])
inlineToAsciiDoc _ (Code _ str) = return $
  text "`" <> text (escapeStringUsing (backslashEscapes "`") str) <> "`"
inlineToAsciiDoc _ (Str str) = return $ text $ escapeString str
inlineToAsciiDoc _ (Math InlineMath str) =
  return $ "latexmath:[$" <> text str <> "$]"
inlineToAsciiDoc _ (Math DisplayMath str) =
  return $ "latexmath:[\\[" <> text str <> "\\]]"
inlineToAsciiDoc _ (RawInline f s)
  | f == "asciidoc" = return $ text s
  | otherwise       = return empty
inlineToAsciiDoc _ (LineBreak) = return $ " +" <> cr
inlineToAsciiDoc _ Space = return space
inlineToAsciiDoc opts SoftBreak =
  case writerWrapText opts of
       WrapAuto -> return space
       WrapPreserve -> return cr
       WrapNone -> return space
inlineToAsciiDoc opts (Cite _ lst) = inlineListToAsciiDoc opts lst
inlineToAsciiDoc opts (Link _ txt (src, _tit)) = do
-- relative:  link:downloads/foo.zip[download foo.zip]
-- abs:  http://google.cod[Google]
-- or my@email.com[email john]
  linktext <- inlineListToAsciiDoc opts txt
  let isRelative = ':' `notElem` src
  let prefix = if isRelative
                  then text "link:"
                  else empty
  let srcSuffix = fromMaybe src (stripPrefix "mailto:" src)
  let useAuto = case txt of
                      [Str s] | escapeURI s == srcSuffix -> True
                      _                                  -> False
  return $ if useAuto
              then text srcSuffix
              else prefix <> text src <> "[" <> linktext <> "]"
inlineToAsciiDoc opts (Image attr alternate (src, tit)) = do
-- image:images/logo.png[Company logo, title="blah"]
  let txt = if (null alternate) || (alternate == [Str ""])
               then [Str "image"]
               else alternate
  linktext <- inlineListToAsciiDoc opts txt
  let linktitle = if null tit
                     then empty
                     else ",title=\"" <> text tit <> "\""
      showDim dir = case (dimension dir attr) of
                      Just (Percent a) ->
                        ["scaledwidth=" <> text (show (Percent a))]
                      Just dim         ->
                        [text (show dir) <> "=" <> text (showInPixel opts dim)]
                      Nothing          ->
                        []
      dimList = showDim Width ++ showDim Height
      dims = if null dimList
                then empty
                else "," <> cat (intersperse "," dimList)
  return $ "image:" <> text src <> "[" <> linktext <> linktitle <> dims <> "]"
inlineToAsciiDoc opts (Note [Para inlines]) =
  inlineToAsciiDoc opts (Note [Plain inlines])
inlineToAsciiDoc opts (Note [Plain inlines]) = do
  contents  <- inlineListToAsciiDoc opts inlines
  return $ text "footnote:[" <> contents <> "]"
-- asciidoc can't handle blank lines in notes
inlineToAsciiDoc _ (Note _) = return "[multiblock footnote omitted]"
inlineToAsciiDoc opts (Span (ident,_,_) ils) = do
  let identifier = if (null ident) then empty else ("[[" <> text ident <> "]]")
  contents <- inlineListToAsciiDoc opts ils
  return $ identifier <> contents

