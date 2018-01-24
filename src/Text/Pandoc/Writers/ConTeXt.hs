{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright (C) 2007-2018 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.ConTeXt
   Copyright   : Copyright (C) 2007-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' format into ConTeXt.
-}
module Text.Pandoc.Writers.ConTeXt ( writeConTeXt ) where
import Control.Monad.State.Strict
import Data.Char (ord, isDigit)
import Data.List (intercalate, intersperse)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Network.URI (unEscapeString)
import Text.Pandoc.BCP47
import Text.Pandoc.Class (PandocMonad, report, toLang)
import Text.Pandoc.Definition
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Pretty
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Walk (query)
import Text.Pandoc.Writers.Shared
import Text.Printf (printf)

data WriterState =
  WriterState { stNextRef          :: Int  -- number of next URL reference
              , stOrderedListLevel :: Int  -- level of ordered list
              , stOptions          :: WriterOptions -- writer options
              }

data Tabl = Xtb | Ntb deriving (Show, Eq)

orderedListStyles :: [Char]
orderedListStyles = cycle "narg"

-- | Convert Pandoc to ConTeXt.
writeConTeXt :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeConTeXt options document =
  let defaultWriterState = WriterState { stNextRef = 1
                                       , stOrderedListLevel = 0
                                       , stOptions = options
                                       }
  in evalStateT (pandocToConTeXt options document) defaultWriterState

type WM = StateT WriterState

pandocToConTeXt :: PandocMonad m => WriterOptions -> Pandoc -> WM m Text
pandocToConTeXt options (Pandoc meta blocks) = do
  let colwidth = if writerWrapText options == WrapAuto
                    then Just $ writerColumns options
                    else Nothing
  let render' :: Doc -> Text
      render' = render colwidth
  metadata <- metaToJSON options
              (fmap render' . blockListToConTeXt)
              (fmap render' . inlineListToConTeXt)
              meta
  body <- mapM (elementToConTeXt options) $ hierarchicalize blocks
  let main = (render' . vcat) body
  let layoutFromMargins = intercalate [','] $ mapMaybe (\(x,y) ->
                                ((x ++ "=") ++) <$> getField y metadata)
                              [("leftmargin","margin-left")
                              ,("rightmargin","margin-right")
                              ,("top","margin-top")
                              ,("bottom","margin-bottom")
                              ]
  mblang <- fromBCP47 (getLang options meta)
  let context =   defField "toc" (writerTableOfContents options)
                $ defField "placelist" (intercalate ("," :: String) $
                     take (writerTOCDepth options +
                           case writerTopLevelDivision options of
                             TopLevelPart    -> 0
                             TopLevelChapter -> 0
                             _               -> 1)
                       ["chapter","section","subsection","subsubsection",
                        "subsubsubsection","subsubsubsubsection"])
                $ defField "body" main
                $ defField "layout" layoutFromMargins
                $ defField "number-sections" (writerNumberSections options)
                $ maybe id (defField "context-lang") mblang
                $ (case getField "papersize" metadata of
                        Just (('a':d:ds) :: String)
                          | all isDigit (d:ds) -> resetField "papersize"
                                                     (('A':d:ds) :: String)
                        _                     -> id) metadata
  let context' = defField "context-dir" (toContextDir
                                         $ getField "dir" context) context
  case writerTemplate options of
       Nothing  -> return main
       Just tpl -> renderTemplate' tpl context'

toContextDir :: Maybe String -> String
toContextDir (Just "rtl") = "r2l"
toContextDir (Just "ltr") = "l2r"
toContextDir _            = ""

-- | escape things as needed for ConTeXt
escapeCharForConTeXt :: WriterOptions -> Char -> String
escapeCharForConTeXt opts ch =
 let ligatures = isEnabled Ext_smart opts in
 case ch of
    '{'      -> "\\{"
    '}'      -> "\\}"
    '\\'     -> "\\letterbackslash{}"
    '$'      -> "\\$"
    '|'      -> "\\letterbar{}"
    '%'      -> "\\letterpercent{}"
    '~'      -> "\\lettertilde{}"
    '#'      -> "\\#"
    '['      -> "{[}"
    ']'      -> "{]}"
    '\160'   -> "~"
    '\x2014' | ligatures -> "---"
    '\x2013' | ligatures -> "--"
    '\x2019' | ligatures -> "'"
    '\x2026' -> "\\ldots{}"
    x        -> [x]

-- | Escape string for ConTeXt
stringToConTeXt :: WriterOptions -> String -> String
stringToConTeXt opts = concatMap (escapeCharForConTeXt opts)

-- | Sanitize labels
toLabel :: String -> String
toLabel z = concatMap go z
 where go x
         | x `elem` ("\\#[]\",{}%()|=" :: String) = "ux" ++ printf "%x" (ord x)
         | otherwise = [x]

-- | Convert Elements to ConTeXt
elementToConTeXt :: PandocMonad m => WriterOptions -> Element -> WM m Doc
elementToConTeXt _ (Blk block) = blockToConTeXt block
elementToConTeXt opts (Sec level _ attr title' elements) = do
  header' <- sectionHeader attr level title'
  footer' <- sectionFooter attr level
  innerContents <- mapM (elementToConTeXt opts) elements
  return $ header' $$ vcat innerContents $$ footer'

-- | Convert Pandoc block element to ConTeXt.
blockToConTeXt :: PandocMonad m => Block -> WM m Doc
blockToConTeXt Null = return empty
blockToConTeXt (Plain lst) = inlineListToConTeXt lst
-- title beginning with fig: indicates that the image is a figure
blockToConTeXt (Para [Image attr txt (src,'f':'i':'g':':':_)]) = do
  capt <- inlineListToConTeXt txt
  img  <- inlineToConTeXt (Image attr txt (src, ""))
  let (ident, _, _) = attr
      label = if null ident
                 then empty
                 else "[]" <> brackets (text $ toLabel ident)
  return $ blankline $$ "\\placefigure" <> label <> braces capt <> img <> blankline
blockToConTeXt (Para lst) = do
  contents <- inlineListToConTeXt lst
  return $ contents <> blankline
blockToConTeXt (LineBlock lns) = do
  doclines <- nowrap . vcat <$> mapM inlineListToConTeXt lns
  return $ "\\startlines" $$ doclines $$ "\\stoplines" <> blankline
blockToConTeXt (BlockQuote lst) = do
  contents <- blockListToConTeXt lst
  return $ "\\startblockquote" $$ nest 0 contents $$ "\\stopblockquote" <> blankline
blockToConTeXt (CodeBlock _ str) =
  return $ flush ("\\starttyping" <> cr <> text str <> cr <> "\\stoptyping") $$ blankline
  -- blankline because \stoptyping can't have anything after it, inc. '}'
blockToConTeXt (RawBlock "context" str) = return $ text str <> blankline
blockToConTeXt b@(RawBlock _ _ ) = do
  report $ BlockNotRendered b
  return empty
blockToConTeXt (Div (ident,_,kvs) bs) = do
  let align dir txt = "\\startalignment[" <> dir <> "]" $$ txt $$ "\\stopalignment"
  mblang <- fromBCP47 (lookup "lang" kvs)
  let wrapRef txt = if null ident
                       then txt
                       else ("\\reference" <> brackets (text $ toLabel ident) <>
                              braces empty <> "%") $$ txt
      wrapDir = case lookup "dir" kvs of
                  Just "rtl" -> align "righttoleft"
                  Just "ltr" -> align "lefttoright"
                  _          -> id
      wrapLang txt = case mblang of
                       Just lng -> "\\start\\language["
                                     <> text lng <> "]" $$ txt $$ "\\stop"
                       Nothing  -> txt
      wrapBlank txt = blankline <> txt <> blankline
  (wrapBlank . wrapLang . wrapDir . wrapRef) <$> blockListToConTeXt bs
blockToConTeXt (BulletList lst) = do
  contents <- mapM listItemToConTeXt lst
  return $ ("\\startitemize" <> if isTightList lst
                                   then brackets "packed"
                                   else empty) $$
    vcat contents $$ text "\\stopitemize" <> blankline
blockToConTeXt (OrderedList (start, style', delim) lst) = do
    st <- get
    let level = stOrderedListLevel st
    put st {stOrderedListLevel = level + 1}
    contents <- mapM listItemToConTeXt lst
    put st {stOrderedListLevel = level}
    let start' = if start == 1 then "" else "start=" ++ show start
    let delim' = case delim of
                        DefaultDelim -> ""
                        Period       -> "stopper=."
                        OneParen     -> "stopper=)"
                        TwoParens    -> "left=(,stopper=)"
    let width = maximum $ map length $ take (length contents)
                          (orderedListMarkers (start, style', delim))
    let width' = (toEnum width + 1) / 2
    let width'' = if width' > (1.5 :: Double)
                     then "width=" ++ show width' ++ "em"
                     else ""
    let specs2Items = filter (not . null) [start', delim', width'']
    let specs2 = if null specs2Items
                    then ""
                    else "[" ++ intercalate "," specs2Items ++ "]"
    let style'' = '[': (case style' of
                          DefaultStyle -> orderedListStyles !! level
                          Decimal      -> 'n'
                          Example      -> 'n'
                          LowerRoman   -> 'r'
                          UpperRoman   -> 'R'
                          LowerAlpha   -> 'a'
                          UpperAlpha   -> 'A') :
                       if isTightList lst then ",packed]" else "]"
    let specs = style'' ++ specs2
    return $ "\\startitemize" <> text specs $$ vcat contents $$
             "\\stopitemize" <> blankline
blockToConTeXt (DefinitionList lst) =
  liftM vcat $ mapM defListItemToConTeXt lst
blockToConTeXt HorizontalRule = return $ "\\thinrule" <> blankline
-- If this is ever executed, provide a default for the reference identifier.
blockToConTeXt (Header level attr lst) = sectionHeader attr level lst
blockToConTeXt (Table caption aligns widths heads rows) = do
    opts <- gets stOptions
    let tabl = if isEnabled Ext_ntb opts
          then Ntb
          else Xtb
    captionText <- inlineListToConTeXt caption
    headers <- if all null heads
               then return empty
               else tableRowToConTeXt tabl aligns widths heads
    rows' <- mapM (tableRowToConTeXt tabl aligns widths) rows
    body <- tableToConTeXt tabl headers rows'
    return $ "\\startplacetable" <> brackets (
      if null caption
        then "location=none"
        else "title=" <> braces captionText
      ) $$ body $$ "\\stopplacetable" <> blankline

tableToConTeXt :: PandocMonad m => Tabl -> Doc -> [Doc] -> WM m Doc
tableToConTeXt Xtb heads rows =
  return $ "\\startxtable" $$
    (if isEmpty heads
      then empty
      else "\\startxtablehead[head]" $$ heads $$ "\\stopxtablehead") $$
    (if null rows
      then empty
      else "\\startxtablebody[body]" $$ vcat (init rows) $$ "\\stopxtablebody" $$
           "\\startxtablefoot[foot]" $$ last rows $$ "\\stopxtablefoot") $$
    "\\stopxtable"
tableToConTeXt Ntb heads rows =
  return $ "\\startTABLE" $$
    (if isEmpty heads
      then empty
      else "\\startTABLEhead" $$ heads $$ "\\stopTABLEhead") $$
    (if null rows
      then empty
      else "\\startTABLEbody" $$ vcat (init rows) $$ "\\stopTABLEbody" $$
           "\\startTABLEfoot" $$ last rows $$ "\\stopTABLEfoot") $$
    "\\stopTABLE"

tableRowToConTeXt :: PandocMonad m => Tabl -> [Alignment] -> [Double] -> [[Block]] -> WM m Doc
tableRowToConTeXt Xtb aligns widths cols = do
  cells <- mapM (tableColToConTeXt Xtb) $ zip3 aligns widths cols
  return $ "\\startxrow" $$ vcat cells $$ "\\stopxrow"
tableRowToConTeXt Ntb aligns widths cols = do
  cells <- mapM (tableColToConTeXt Ntb) $ zip3 aligns widths cols
  return $ vcat cells $$ "\\NC\\NR"

tableColToConTeXt :: PandocMonad m => Tabl -> (Alignment, Double, [Block]) -> WM m Doc
tableColToConTeXt tabl (align, width, blocks) = do
  cellContents <- blockListToConTeXt blocks
  let colwidth = if width == 0
        then empty
        else "width=" <> braces (text (printf "%.2f\\textwidth" width))
  let halign = alignToConTeXt align
  let options = (if keys == empty
                 then empty
                 else brackets keys) <> space
        where keys = hcat $ intersperse "," $ filter (empty /=) [halign, colwidth]
  tableCellToConTeXt tabl options cellContents

tableCellToConTeXt :: PandocMonad m => Tabl -> Doc -> Doc -> WM m Doc
tableCellToConTeXt Xtb options cellContents =
  return $ "\\startxcell" <> options <> cellContents <> " \\stopxcell"
tableCellToConTeXt Ntb options cellContents =
  return $ "\\NC" <> options <> cellContents

alignToConTeXt :: Alignment -> Doc
alignToConTeXt align = case align of
                         AlignLeft    -> "align=right"
                         AlignRight   -> "align=left"
                         AlignCenter  -> "align=middle"
                         AlignDefault -> empty

listItemToConTeXt :: PandocMonad m => [Block] -> WM m Doc
listItemToConTeXt list = blockListToConTeXt list >>=
  return . ("\\item" $$) . nest 2

defListItemToConTeXt :: PandocMonad m => ([Inline], [[Block]]) -> WM m Doc
defListItemToConTeXt (term, defs) = do
  term' <- inlineListToConTeXt term
  def'  <- liftM vsep $ mapM blockListToConTeXt defs
  return $ "\\startdescription" <> braces term' $$ nest 2 def' $$
           "\\stopdescription" <> blankline

-- | Convert list of block elements to ConTeXt.
blockListToConTeXt :: PandocMonad m => [Block] -> WM m Doc
blockListToConTeXt lst = liftM vcat $ mapM blockToConTeXt lst

-- | Convert list of inline elements to ConTeXt.
inlineListToConTeXt :: PandocMonad m
                    => [Inline]  -- ^ Inlines to convert
                    -> WM m Doc
inlineListToConTeXt lst = liftM hcat $ mapM inlineToConTeXt $ addStruts lst
  -- We add a \strut after a line break that precedes a space,
  -- or the space gets swallowed
  where addStruts (LineBreak : s : xs) | isSpacey s =
           LineBreak : RawInline (Format "context") "\\strut " : s :
             addStruts xs
        addStruts (x:xs) = x : addStruts xs
        addStruts [] = []
        isSpacey Space            = True
        isSpacey (Str ('\160':_)) = True
        isSpacey _                = False

-- | Convert inline element to ConTeXt
inlineToConTeXt :: PandocMonad m
                => Inline    -- ^ Inline to convert
                -> WM m Doc
inlineToConTeXt (Emph lst) = do
  contents <- inlineListToConTeXt lst
  return $ braces $ "\\em " <> contents
inlineToConTeXt (Strong lst) = do
  contents <- inlineListToConTeXt lst
  return $ braces $ "\\bf " <> contents
inlineToConTeXt (Strikeout lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\overstrikes" <> braces contents
inlineToConTeXt (Superscript lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\high" <> braces contents
inlineToConTeXt (Subscript lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\low" <> braces contents
inlineToConTeXt (SmallCaps lst) = do
  contents <- inlineListToConTeXt lst
  return $ braces $ "\\sc " <> contents
inlineToConTeXt (Code _ str) | not ('{' `elem` str || '}' `elem` str) =
  return $ "\\type" <> braces (text str)
inlineToConTeXt (Code _ str) = do
  opts <- gets stOptions
  return $ "\\mono" <> braces (text $ stringToConTeXt opts str)
inlineToConTeXt (Quoted SingleQuote lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\quote" <> braces contents
inlineToConTeXt (Quoted DoubleQuote lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\quotation" <> braces contents
inlineToConTeXt (Cite _ lst) = inlineListToConTeXt lst
inlineToConTeXt (Str str) = do
  opts <- gets stOptions
  return $ text $ stringToConTeXt opts str
inlineToConTeXt (Math InlineMath str) =
  return $ char '$' <> text str <> char '$'
inlineToConTeXt (Math DisplayMath str) =
  return $ text "\\startformula "  <> text str <> text " \\stopformula" <> space
inlineToConTeXt (RawInline "context" str) = return $ text str
inlineToConTeXt (RawInline "tex" str) = return $ text str
inlineToConTeXt il@(RawInline _ _) = do
  report $ InlineNotRendered il
  return empty
inlineToConTeXt LineBreak = return $ text "\\crlf" <> cr
inlineToConTeXt SoftBreak = do
  wrapText <- gets (writerWrapText . stOptions)
  return $ case wrapText of
               WrapAuto     -> space
               WrapNone     -> space
               WrapPreserve -> cr
inlineToConTeXt Space = return space
-- Handle HTML-like internal document references to sections
inlineToConTeXt (Link _ txt ('#' : ref, _)) = do
  opts <- gets stOptions
  contents <-  inlineListToConTeXt txt
  let ref' = toLabel $ stringToConTeXt opts ref
  return $ text "\\goto"
           <> braces contents
           <> brackets (text ref')

inlineToConTeXt (Link _ txt (src, _)) = do
  let isAutolink = txt == [Str (unEscapeString src)]
  st <- get
  let next = stNextRef st
  put $ st {stNextRef = next + 1}
  let ref = "url" ++ show next
  contents <-  inlineListToConTeXt txt
  return $ "\\useURL"
           <> brackets (text ref)
           <> brackets (text $ escapeStringUsing [('#',"\\#"),('%',"\\%")] src)
           <> (if isAutolink
                  then empty
                  else brackets empty <> brackets contents)
           <> "\\from"
           <> brackets (text ref)
inlineToConTeXt (Image attr@(_,cls,_) _ (src, _)) = do
  opts <- gets stOptions
  let showDim dir = let d = text (show dir) <> "="
                    in case dimension dir attr of
                         Just (Pixel a)   ->
                           [d <> text (showInInch opts (Pixel a)) <> "in"]
                         Just (Percent a) ->
                           [d <> text (showFl (a / 100)) <> "\\textwidth"]
                         Just dim         ->
                           [d <> text (show dim)]
                         Nothing          ->
                           []
      dimList = showDim Width ++ showDim Height
      dims = if null dimList
                then empty
                else brackets $ cat (intersperse "," dimList)
      clas = if null cls
                then empty
                else brackets $ text $ toLabel $ head cls
      src' = if isURI src
                then src
                else unEscapeString src
  return $ braces $ "\\externalfigure" <> brackets (text src') <> dims <> clas
inlineToConTeXt (Note contents) = do
  contents' <- blockListToConTeXt contents
  let codeBlock x@(CodeBlock _ _) = [x]
      codeBlock _                 = []
  let codeBlocks = query codeBlock contents
  return $ if null codeBlocks
              then text "\\footnote{" <> nest 2 contents' <> char '}'
              else text "\\startbuffer " <> nest 2 contents' <>
                   text "\\stopbuffer\\footnote{\\getbuffer}"
inlineToConTeXt (Span (_,_,kvs) ils) = do
  mblang <- fromBCP47 (lookup "lang" kvs)
  let wrapDir txt = case lookup "dir" kvs of
                      Just "rtl" -> braces $ "\\righttoleft " <> txt
                      Just "ltr" -> braces $ "\\lefttoright " <> txt
                      _          -> txt
      wrapLang txt = case mblang of
                       Just lng -> "\\start\\language[" <> text lng
                                      <> "]" <> txt <> "\\stop "
                       Nothing -> txt
  (wrapLang . wrapDir) <$> inlineListToConTeXt ils

-- | Craft the section header, inserting the section reference, if supplied.
sectionHeader :: PandocMonad m
              => Attr
              -> Int
              -> [Inline]
              -> WM m Doc
sectionHeader (ident,classes,kvs) hdrLevel lst = do
  opts <- gets stOptions
  contents <- inlineListToConTeXt lst
  levelText <- sectionLevelToText opts (ident,classes,kvs) hdrLevel
  let ident' = if null ident
               then empty
               else "reference=" <> braces (text (toLabel ident))
  let contents' = if contents == empty
                  then empty
                  else "title=" <> braces contents
  let options = if keys == empty || levelText == empty
                then empty
                else brackets keys
        where keys = hcat $ intersperse "," $ filter (empty /=) [contents', ident']
  let starter = if writerSectionDivs opts
                then "\\start"
                else "\\"
  return $ starter <> levelText <> options <> blankline

-- | Craft the section footer
sectionFooter :: PandocMonad m => Attr -> Int -> WM m Doc
sectionFooter attr hdrLevel = do
  opts <- gets stOptions
  levelText <- sectionLevelToText opts attr hdrLevel
  return $ if writerSectionDivs opts
           then "\\stop" <> levelText <> blankline
           else empty

-- | Generate a textual representation of the section level
sectionLevelToText :: PandocMonad m => WriterOptions -> Attr -> Int -> WM m Doc
sectionLevelToText opts (_,classes,_) hdrLevel = do
  let level' = case writerTopLevelDivision opts of
                 TopLevelPart    -> hdrLevel - 2
                 TopLevelChapter -> hdrLevel - 1
                 TopLevelSection -> hdrLevel
                 TopLevelDefault -> hdrLevel
  let (section, chapter) = if "unnumbered" `elem` classes
                              then (text "subject", text "title")
                              else (text "section", text "chapter")
  return $ case level' of
             -1         -> text "part"
             0          -> chapter
             n | n >= 1 -> text (concat (replicate (n - 1) "sub"))
                           <> section
             _          -> empty -- cannot happen

fromBCP47 :: PandocMonad m => Maybe String -> WM m (Maybe String)
fromBCP47 mbs = fromBCP47' <$> toLang mbs

-- Takes a list of the constituents of a BCP 47 language code
-- and irons out ConTeXt's exceptions
-- https://tools.ietf.org/html/bcp47#section-2.1
-- http://wiki.contextgarden.net/Language_Codes
fromBCP47' :: Maybe Lang -> Maybe String
fromBCP47' (Just (Lang "ar" _ "SY" _)     ) = Just "ar-sy"
fromBCP47' (Just (Lang "ar" _ "IQ" _)     ) = Just "ar-iq"
fromBCP47' (Just (Lang "ar" _ "JO" _)     ) = Just "ar-jo"
fromBCP47' (Just (Lang "ar" _ "LB" _)     ) = Just "ar-lb"
fromBCP47' (Just (Lang "ar" _ "DZ" _)     ) = Just "ar-dz"
fromBCP47' (Just (Lang "ar" _ "MA" _)     ) = Just "ar-ma"
fromBCP47' (Just (Lang "de" _ _ ["1901"]) ) = Just "deo"
fromBCP47' (Just (Lang "de" _ "DE" _)     ) = Just "de-de"
fromBCP47' (Just (Lang "de" _ "AT" _)     ) = Just "de-at"
fromBCP47' (Just (Lang "de" _ "CH" _)     ) = Just "de-ch"
fromBCP47' (Just (Lang "el" _ _ ["poly"]) ) = Just "agr"
fromBCP47' (Just (Lang "en" _ "US" _)     ) = Just "en-us"
fromBCP47' (Just (Lang "en" _ "GB" _)     ) = Just "en-gb"
fromBCP47' (Just (Lang "grc"_  _ _)       ) = Just "agr"
fromBCP47' (Just (Lang "el" _ _ _)        ) = Just "gr"
fromBCP47' (Just (Lang "eu" _ _ _)        ) = Just "ba"
fromBCP47' (Just (Lang "he" _ _ _)        ) = Just "il"
fromBCP47' (Just (Lang "jp" _ _ _)        ) = Just "ja"
fromBCP47' (Just (Lang "uk" _ _ _)        ) = Just "ua"
fromBCP47' (Just (Lang "vi" _ _ _)        ) = Just "vn"
fromBCP47' (Just (Lang "zh" _ _ _)        ) = Just "cn"
fromBCP47' (Just (Lang l _ _ _)           ) = Just l
fromBCP47' Nothing                          = Nothing
