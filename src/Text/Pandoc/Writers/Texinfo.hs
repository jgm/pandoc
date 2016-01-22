{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2008-2015 John MacFarlane and Peter Wang

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
   Module      : Text.Pandoc.Writers.Texinfo
   Copyright   : Copyright (C) 2008-2015 John MacFarlane and Peter Wang
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' format into Texinfo.
-}
module Text.Pandoc.Writers.Texinfo ( writeTexinfo ) where
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Templates (renderTemplate')
import Text.Printf ( printf )
import Data.List ( transpose, maximumBy )
import Data.Ord ( comparing )
import Data.Char ( chr, ord )
import Control.Monad.State
import Text.Pandoc.Pretty
import Text.Pandoc.ImageSize
import Network.URI ( isURI, unEscapeString )
import System.FilePath
import qualified Data.Set as Set

data WriterState =
  WriterState { stStrikeout   :: Bool  -- document contains strikeout
              , stSuperscript :: Bool -- document contains superscript
              , stSubscript   :: Bool -- document contains subscript
              , stEscapeComma :: Bool -- in a context where we need @comma
              , stIdentifiers :: Set.Set String -- header ids used already
              , stOptions     :: WriterOptions -- writer options
              }

{- TODO:
 - internal cross references a la HTML
 - generated .texi files don't work when run through texi2dvi
 -}

-- | Convert Pandoc to Texinfo.
writeTexinfo :: WriterOptions -> Pandoc -> String
writeTexinfo options document =
  evalState (pandocToTexinfo options $ wrapTop document) $
  WriterState { stStrikeout = False, stSuperscript = False,
                stEscapeComma = False, stSubscript = False,
                stIdentifiers = Set.empty, stOptions = options}

-- | Add a "Top" node around the document, needed by Texinfo.
wrapTop :: Pandoc -> Pandoc
wrapTop (Pandoc meta blocks) =
  Pandoc meta (Header 0 nullAttr (docTitle meta) : blocks)

pandocToTexinfo :: WriterOptions -> Pandoc -> State WriterState String
pandocToTexinfo options (Pandoc meta blocks) = do
  let titlePage = not $ all null
                      $ docTitle meta : docDate meta : docAuthors meta
  let colwidth = if writerWrapText options == WrapAuto
                    then Just $ writerColumns options
                    else Nothing
  metadata <- metaToJSON options
              (fmap (render colwidth) . blockListToTexinfo)
              (fmap (render colwidth) . inlineListToTexinfo)
              meta
  main <- blockListToTexinfo blocks
  st <- get
  let body = render colwidth main
  let context = defField "body" body
              $ defField "toc" (writerTableOfContents options)
              $ defField "titlepage" titlePage
              $ defField "subscript" (stSubscript st)
              $ defField "superscript" (stSuperscript st)
              $ defField "strikeout" (stStrikeout st)
              $ metadata
  if writerStandalone options
     then return $ renderTemplate' (writerTemplate options) context
     else return body

-- | Escape things as needed for Texinfo.
stringToTexinfo :: String -> String
stringToTexinfo = escapeStringUsing texinfoEscapes
  where texinfoEscapes = [ ('{', "@{")
                         , ('}', "@}")
                         , ('@', "@@")
                         , ('\160', "@ ")
                         , ('\x2014', "---")
                         , ('\x2013', "--")
                         , ('\x2026', "@dots{}")
                         , ('\x2019', "'")
                         ]

escapeCommas :: State WriterState Doc -> State WriterState Doc
escapeCommas parser = do
  oldEscapeComma <- gets stEscapeComma
  modify $ \st -> st{ stEscapeComma = True }
  res <- parser
  modify $ \st -> st{ stEscapeComma = oldEscapeComma }
  return res

-- | Puts contents into Texinfo command.
inCmd :: String -> Doc -> Doc
inCmd cmd contents = char '@' <> text cmd <> braces contents

-- | Convert Pandoc block element to Texinfo.
blockToTexinfo :: Block     -- ^ Block to convert
               -> State WriterState Doc

blockToTexinfo Null = return empty

blockToTexinfo (Div _ bs) = blockListToTexinfo bs

blockToTexinfo (Plain lst) =
  inlineListToTexinfo lst

-- title beginning with fig: indicates that the image is a figure
blockToTexinfo (Para [Image attr txt (src,'f':'i':'g':':':tit)]) = do
  capt <- if null txt
             then return empty
             else (\c -> text "@caption" <> braces c) `fmap`
                    inlineListToTexinfo txt
  img  <- inlineToTexinfo (Image attr txt (src,tit))
  return $ text "@float" $$ img $$ capt $$ text "@end float"

blockToTexinfo (Para lst) =
  inlineListToTexinfo lst    -- this is handled differently from Plain in blockListToTexinfo

blockToTexinfo (BlockQuote lst) = do
  contents <- blockListToTexinfo lst
  return $ text "@quotation" $$
           contents $$
           text "@end quotation"

blockToTexinfo (CodeBlock _ str) = do
  return $ blankline $$
           text "@verbatim" $$
           flush (text str) $$
           text "@end verbatim" <> blankline

blockToTexinfo (RawBlock f str)
  | f == "texinfo" = return $ text str
  | f == "latex" || f == "tex" =
                      return $ text "@tex" $$ text str $$ text "@end tex"
  | otherwise      = return empty

blockToTexinfo (BulletList lst) = do
  items <- mapM listItemToTexinfo lst
  return $ text "@itemize" $$
           vcat items $$
           text "@end itemize" <> blankline

blockToTexinfo (OrderedList (start, numstyle, _) lst) = do
  items <- mapM listItemToTexinfo lst
  return $ text "@enumerate " <> exemplar $$
           vcat items $$
           text "@end enumerate" <> blankline
  where
    exemplar = case numstyle of
                DefaultStyle -> decimal
                Decimal      -> decimal
                Example      -> decimal
                UpperRoman   -> decimal   -- Roman numerals not supported
                LowerRoman   -> decimal
                UpperAlpha   -> upperAlpha
                LowerAlpha   -> lowerAlpha
    decimal = if start == 1
                 then empty
                 else text (show start)
    upperAlpha = text [chr $ ord 'A' + start - 1]
    lowerAlpha = text [chr $ ord 'a' + start - 1]

blockToTexinfo (DefinitionList lst) = do
  items <- mapM defListItemToTexinfo lst
  return $ text "@table @asis" $$
           vcat items $$
           text "@end table" <> blankline

blockToTexinfo HorizontalRule =
    -- XXX can't get the equivalent from LaTeX.hs to work
    return $ text "@iftex" $$
             text "@bigskip@hrule@bigskip" $$
             text "@end iftex" $$
             text "@ifnottex" $$
             text (take 72 $ repeat '-') $$
             text "@end ifnottex"

blockToTexinfo (Header 0 _ lst) = do
  txt <- if null lst
            then return $ text "Top"
            else inlineListToTexinfo lst
  return $ text "@node Top" $$
           text "@top " <> txt <> blankline

blockToTexinfo (Header level _ lst) = do
  node <- inlineListForNode lst
  txt <- inlineListToTexinfo lst
  idsUsed <- gets stIdentifiers
  let id' = uniqueIdent lst idsUsed
  modify $ \st -> st{ stIdentifiers = Set.insert id' idsUsed }
  return $ if (level > 0) && (level <= 4)
              then blankline <> text "@node " <> node $$
                   text (seccmd level) <> txt $$
                   text "@anchor" <> braces (text $ '#':id')
              else txt
  where
    seccmd 1 = "@chapter "
    seccmd 2 = "@section "
    seccmd 3 = "@subsection "
    seccmd 4 = "@subsubsection "
    seccmd _ = error "illegal seccmd level"

blockToTexinfo (Table caption aligns widths heads rows) = do
  headers <- if all null heads
                then return empty
                else tableHeadToTexinfo aligns heads
  captionText <- inlineListToTexinfo caption
  rowsText <- mapM (tableRowToTexinfo aligns) rows
  colDescriptors <-
    if all (== 0) widths
       then do -- use longest entry instead of column widths
            cols <- mapM (mapM (liftM (render Nothing . hcat) . mapM blockToTexinfo)) $
                        transpose $ heads : rows
            return $ concatMap ((\x -> "{"++x++"} ") .  maximumBy (comparing length)) cols
       else return $ "@columnfractions " ++ concatMap (printf "%.2f ") widths
  let tableBody = text ("@multitable " ++ colDescriptors) $$
                  headers $$
                  vcat rowsText $$
                  text "@end multitable"
  return $ if isEmpty captionText
              then tableBody <> blankline
              else text "@float" $$
                   tableBody $$
                   inCmd "caption" captionText $$
                   text "@end float"

tableHeadToTexinfo :: [Alignment]
                   -> [[Block]]
                   -> State WriterState Doc
tableHeadToTexinfo = tableAnyRowToTexinfo "@headitem "

tableRowToTexinfo :: [Alignment]
                  -> [[Block]]
                  -> State WriterState Doc
tableRowToTexinfo = tableAnyRowToTexinfo "@item "

tableAnyRowToTexinfo :: String
                     -> [Alignment]
                     -> [[Block]]
                     -> State WriterState Doc
tableAnyRowToTexinfo itemtype aligns cols =
  zipWithM alignedBlock aligns cols >>=
  return . (text itemtype $$) . foldl (\row item -> row $$
  (if isEmpty row then empty else text " @tab ") <> item) empty

alignedBlock :: Alignment
             -> [Block]
             -> State WriterState Doc
-- XXX @flushleft and @flushright text won't get word wrapped.  Since word
-- wrapping is more important than alignment, we ignore the alignment.
alignedBlock _ = blockListToTexinfo
{-
alignedBlock AlignLeft col = do
  b <- blockListToTexinfo col
  return $ text "@flushleft" $$ b $$ text "@end flushleft"
alignedBlock AlignRight col = do
  b <- blockListToTexinfo col
  return $ text "@flushright" $$ b $$ text "@end flushright"
alignedBlock _ col = blockListToTexinfo col
-}

-- | Convert Pandoc block elements to Texinfo.
blockListToTexinfo :: [Block]
                   -> State WriterState Doc
blockListToTexinfo [] = return empty
blockListToTexinfo (x:xs) = do
  x' <- blockToTexinfo x
  case x of
    Header level _ _ -> do
      -- We need need to insert a menu for this node.
      let (before, after) = break isHeaderBlock xs
      before' <- blockListToTexinfo before
      let menu = if level < 4
                    then collectNodes (level + 1) after
                    else []
      lines' <- mapM makeMenuLine menu
      let menu' = if null lines'
                    then empty
                    else text "@menu" $$
                         vcat lines' $$
                         text "@end menu"
      after' <- blockListToTexinfo after
      return $ x' $$ before' $$ menu' $$ after'
    Para _ -> do
      xs' <- blockListToTexinfo xs
      case xs of
           ((CodeBlock _ _):_) -> return $ x' $$ xs'
           _                   -> return $ x' $+$ xs'
    _ -> do
      xs' <- blockListToTexinfo xs
      return $ x' $$ xs'

collectNodes :: Int -> [Block] -> [Block]
collectNodes _ [] = []
collectNodes level (x:xs) =
  case x of
    (Header hl _ _) ->
      if hl < level
         then []
         else if hl == level
                 then x : collectNodes level xs
                 else collectNodes level xs
    _ ->
      collectNodes level xs

makeMenuLine :: Block
             -> State WriterState Doc
makeMenuLine (Header _ _ lst) = do
  txt <- inlineListForNode lst
  return $ text "* " <> txt <> text "::"
makeMenuLine _ = error "makeMenuLine called with non-Header block"

listItemToTexinfo :: [Block]
                  -> State WriterState Doc
listItemToTexinfo lst = do
  contents <- blockListToTexinfo lst
  let spacer = case reverse lst of
                    (Para{}:_) -> blankline
                    _          -> empty
  return $ text "@item" $$ contents <> spacer

defListItemToTexinfo :: ([Inline], [[Block]])
                     -> State WriterState Doc
defListItemToTexinfo (term, defs) = do
    term' <- inlineListToTexinfo term
    let defToTexinfo bs = do d <- blockListToTexinfo bs
                             case reverse bs of
                                  (Para{}:_) -> return $ d <> blankline
                                  _          -> return d
    defs' <- mapM defToTexinfo defs
    return $ text "@item " <> term' $+$ vcat defs'

-- | Convert list of inline elements to Texinfo.
inlineListToTexinfo :: [Inline]  -- ^ Inlines to convert
                  -> State WriterState Doc
inlineListToTexinfo lst = mapM inlineToTexinfo lst >>= return . hcat

-- | Convert list of inline elements to Texinfo acceptable for a node name.
inlineListForNode :: [Inline]  -- ^ Inlines to convert
                  -> State WriterState Doc
inlineListForNode = return . text . stringToTexinfo .
                    filter (not . disallowedInNode) . stringify

-- periods, commas, colons, and parentheses are disallowed in node names
disallowedInNode :: Char -> Bool
disallowedInNode c = c `elem` (".,:()" :: String)

-- | Convert inline element to Texinfo
inlineToTexinfo :: Inline    -- ^ Inline to convert
                -> State WriterState Doc

inlineToTexinfo (Span _ lst) =
  inlineListToTexinfo lst

inlineToTexinfo (Emph lst) =
  inlineListToTexinfo lst >>= return . inCmd "emph"

inlineToTexinfo (Strong lst) =
  inlineListToTexinfo lst >>= return . inCmd "strong"

inlineToTexinfo (Strikeout lst) = do
  modify $ \st -> st{ stStrikeout = True }
  contents <- inlineListToTexinfo lst
  return $ text "@textstrikeout{" <> contents <> text "}"

inlineToTexinfo (Superscript lst) = do
  modify $ \st -> st{ stSuperscript = True }
  contents <- inlineListToTexinfo lst
  return $ text "@textsuperscript{" <> contents <> char '}'

inlineToTexinfo (Subscript lst) = do
  modify $ \st -> st{ stSubscript = True }
  contents <- inlineListToTexinfo lst
  return $ text "@textsubscript{" <> contents <> char '}'

inlineToTexinfo (SmallCaps lst) =
  inlineListToTexinfo lst >>= return . inCmd "sc"

inlineToTexinfo (Code _ str) = do
  return $ text $ "@code{" ++ stringToTexinfo str ++ "}"

inlineToTexinfo (Quoted SingleQuote lst) = do
  contents <- inlineListToTexinfo lst
  return $ char '`' <> contents <> char '\''

inlineToTexinfo (Quoted DoubleQuote lst) = do
  contents <- inlineListToTexinfo lst
  return $ text "``" <> contents <> text "''"

inlineToTexinfo (Cite _ lst) =
  inlineListToTexinfo lst
inlineToTexinfo (Str str) = return $ text (stringToTexinfo str)
inlineToTexinfo (Math _ str) = return $ inCmd "math" $ text str
inlineToTexinfo (RawInline f str)
  | f == "latex" || f == "tex" =
                      return $ text "@tex" $$ text str $$ text "@end tex"
  | f == "texinfo" =  return $ text str
  | otherwise      =  return empty
inlineToTexinfo (LineBreak) = return $ text "@*" <> cr
inlineToTexinfo SoftBreak = do
  wrapText <- gets (writerWrapText . stOptions)
  case wrapText of
      WrapAuto     -> return space
      WrapNone     -> return space
      WrapPreserve -> return cr
inlineToTexinfo Space = return space

inlineToTexinfo (Link _ txt (src@('#':_), _)) = do
  contents <- escapeCommas $ inlineListToTexinfo txt
  return $ text "@ref" <>
           braces (text (stringToTexinfo src) <> text "," <> contents)
inlineToTexinfo (Link _ txt (src, _)) = do
  case txt of
        [Str x] | escapeURI x == src ->  -- autolink
             do return $ text $ "@url{" ++ x ++ "}"
        _ -> do contents <- escapeCommas $ inlineListToTexinfo txt
                let src1 = stringToTexinfo src
                return $ text ("@uref{" ++ src1 ++ ",") <> contents <>
                         char '}'

inlineToTexinfo (Image attr alternate (source, _)) = do
  content <- escapeCommas $ inlineListToTexinfo alternate
  opts <- gets stOptions
  let showDim dim = case (dimension dim attr) of
                      (Just (Pixel a))   -> showInInch opts (Pixel a) ++ "in"
                      (Just (Percent _)) -> ""
                      (Just d)           -> show d
                      Nothing            -> ""
  return $ text ("@image{" ++ base ++ ',':(showDim Width) ++ ',':(showDim Height) ++ ",")
           <> content <> text "," <> text (ext ++ "}")
  where
    ext     = drop 1 $ takeExtension source'
    base    = dropExtension source'
    source' = if isURI source
                 then source
                 else unEscapeString source

inlineToTexinfo (Note contents) = do
  contents' <- blockListToTexinfo contents
  return $ text "@footnote" <> braces contents'
