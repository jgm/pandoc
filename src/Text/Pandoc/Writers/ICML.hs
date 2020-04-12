{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{- |
   Module      : Text.Pandoc.Writers.ICML
   Copyright   : Copyright (C) 2013-2020 github.com/mb21
   License     : GNU GPL, version 2 or above

   Stability   : alpha

Conversion of 'Pandoc' documents to Adobe InCopy ICML, a stand-alone XML format
which is a subset of the zipped IDML format for which the documentation is
available here: http://wwwimages.adobe.com/www.adobe.com/content/dam/Adobe/en/devnet/indesign/sdk/cs6/idml/idml-specification.pdf
InCopy is the companion word-processor to Adobe InDesign and ICML documents can be integrated
into InDesign with File -> Place.
-}
module Text.Pandoc.Writers.ICML (writeICML) where
import Control.Monad.Except (catchError)
import Control.Monad.State.Strict
import Data.List (intersperse)
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import Text.Pandoc.Class.PandocMonad (PandocMonad, fetchItem, report)
import Text.Pandoc.Definition
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.DocLayout
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Math (texMathToInlines)
import Text.Pandoc.Writers.Shared
import Text.Pandoc.XML

type Style = [Text]
type Hyperlink = [(Int, Text)]

data WriterState = WriterState{
    blockStyles  :: Set.Set Text
  , inlineStyles :: Set.Set Text
  , links        :: Hyperlink
  , listDepth    :: Int
  , maxListDepth :: Int
  }

type WS m = StateT WriterState m

defaultWriterState :: WriterState
defaultWriterState = WriterState{
    blockStyles  = Set.empty
  , inlineStyles = Set.empty
  , links        = []
  , listDepth    = 1
  , maxListDepth = 0
  }

-- inline names (appear in InDesign's character styles pane)
emphName        :: Text
underlineName   :: Text
strongName      :: Text
strikeoutName   :: Text
superscriptName :: Text
subscriptName   :: Text
smallCapsName   :: Text
codeName        :: Text
linkName        :: Text
emphName        = "Italic"
underlineName   = "Underline"
strongName      = "Bold"
strikeoutName   = "Strikeout"
superscriptName = "Superscript"
subscriptName   = "Subscript"
smallCapsName   = "SmallCaps"
codeName        = "Code"
linkName        = "Link"

-- block element names (appear in InDesign's paragraph styles pane)
paragraphName     :: Text
figureName        :: Text
imgCaptionName    :: Text
codeBlockName     :: Text
blockQuoteName    :: Text
orderedListName   :: Text
bulletListName    :: Text
defListTermName   :: Text
defListDefName    :: Text
headerName        :: Text
tableName         :: Text
tableHeaderName   :: Text
tableCaptionName  :: Text
alignLeftName     :: Text
alignRightName    :: Text
alignCenterName   :: Text
firstListItemName :: Text
beginsWithName    :: Text
lowerRomanName    :: Text
upperRomanName    :: Text
lowerAlphaName    :: Text
upperAlphaName    :: Text
subListParName    :: Text
footnoteName      :: Text
citeName          :: Text
paragraphName     = "Paragraph"
figureName        = "Figure"
imgCaptionName    = "Caption"
codeBlockName     = "CodeBlock"
blockQuoteName    = "Blockquote"
orderedListName   = "NumList"
bulletListName    = "BulList"
defListTermName   = "DefListTerm"
defListDefName    = "DefListDef"
headerName        = "Header"
tableName         = "TablePar"
tableHeaderName   = "TableHeader"
tableCaptionName  = "TableCaption"
alignLeftName     = "LeftAlign"
alignRightName    = "RightAlign"
alignCenterName   = "CenterAlign"
firstListItemName = "first"
beginsWithName    = "beginsWith-"
lowerRomanName    = "lowerRoman"
upperRomanName    = "upperRoman"
lowerAlphaName    = "lowerAlpha"
upperAlphaName    = "upperAlpha"
subListParName    = "subParagraph"
footnoteName      = "Footnote"
citeName          = "Cite"

-- | Convert Pandoc document to string in ICML format.
writeICML :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeICML opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
      renderMeta f s = fst <$> runStateT (f opts [] s) defaultWriterState
  metadata <- metaToContext opts
             (renderMeta blocksToICML)
             (renderMeta inlinesToICML)
             meta
  (main, st) <- runStateT (blocksToICML opts [] blocks) defaultWriterState
  let context = defField "body" main
              $ defField "charStyles" (charStylesToDoc st)
              $ defField "parStyles"  (parStylesToDoc st)
              $ defField "hyperlinks" (hyperlinksToDoc $ links st) metadata
  return $ render colwidth $
    (if writerPreferAscii opts then fmap toEntities else id) $
    case writerTemplate opts of
       Nothing  -> main
       Just tpl -> renderTemplate tpl context

-- | Auxiliary functions for parStylesToDoc and charStylesToDoc.
contains :: Text -> (Text, (Text, Text)) -> [(Text, Text)]
contains s rule =
  [snd rule | fst rule `Text.isInfixOf` s]

-- | The monospaced font to use as default.
monospacedFont :: Doc Text
monospacedFont = inTags False "AppliedFont" [("type", "string")] $ text "Courier New"

-- | How much to indent blockquotes etc.
defaultIndent :: Int
defaultIndent = 20

-- | How much to indent numbered lists before the number.
defaultListIndent :: Int
defaultListIndent = 10

-- other constants
lineSeparator :: Text
lineSeparator = "&#x2028;"

-- | Convert a WriterState with its block styles to the ICML listing of Paragraph Styles.
parStylesToDoc :: WriterState -> Doc Text
parStylesToDoc st = vcat $ map makeStyle $ Set.toAscList $ blockStyles st
  where
    makeStyle s =
      let countSubStrs sub str = length $ Text.breakOnAll sub str
          attrs = concatMap (contains s) [
                               (defListTermName, ("BulletsAndNumberingListType", "BulletList"))
                             , (defListTermName, ("FontStyle", "Bold"))
                             , (tableHeaderName, ("FontStyle", "Bold"))
                             , (alignLeftName,   ("Justification", "LeftAlign"))
                             , (alignRightName,  ("Justification", "RightAlign"))
                             , (alignCenterName, ("Justification", "CenterAlign"))
                             , (headerName<>"1", ("PointSize", "36"))
                             , (headerName<>"2", ("PointSize", "30"))
                             , (headerName<>"3", ("PointSize", "24"))
                             , (headerName<>"4", ("PointSize", "18"))
                             , (headerName<>"5", ("PointSize", "14"))
                             ]
          -- what is the most nested list type, if any?
          (isBulletList, isOrderedList) = findList $ reverse $ splitTextBy (==' ') s
            where
              findList [] = (False, False)
              findList (x:xs) | x == bulletListName  = (True, False)
                              | x == orderedListName = (False, True)
                              | otherwise = findList xs
          nBuls = countSubStrs bulletListName s
          nOrds = countSubStrs orderedListName s
          attrs' = numbering <> listType <> indent <> attrs
            where
              numbering | isOrderedList = [("NumberingExpression", "^#.^t"), ("NumberingLevel", tshow nOrds)]
                        | otherwise     = []
              listType | isOrderedList && not (subListParName `Text.isInfixOf` s)
                           = [("BulletsAndNumberingListType", "NumberedList")]
                       | isBulletList && not (subListParName `Text.isInfixOf` s)
                           = [("BulletsAndNumberingListType", "BulletList")]
                       | otherwise = []
              indent = [("LeftIndent", tshow indt)]
                where
                  nBlockQuotes = countSubStrs blockQuoteName s
                  nDefLists = countSubStrs defListDefName s
                  indt = max 0 $ defaultListIndent*(nBuls + nOrds - 1) + defaultIndent*(nBlockQuotes + nDefLists)
          props = inTags True "Properties" [] (basedOn $$ tabList $$ numbForm)
            where
              font = if codeBlockName `Text.isInfixOf` s
                        then monospacedFont
                        else empty
              basedOn = inTags False "BasedOn" [("type", "object")] (text "$ID/NormalParagraphStyle") $$ font
              tabList = if isBulletList
                           then inTags True "TabList" [("type","list")] $ inTags True "ListItem" [("type","record")]
                                $ vcat [
                                    inTags False "Alignment" [("type","enumeration")] $ text "LeftAlign"
                                  , inTags False "AlignmentCharacter" [("type","string")] $ text "."
                                  , selfClosingTag "Leader" [("type","string")]
                                  , inTags False "Position" [("type","unit")] $ text
                                      $ show $ defaultListIndent * (nBuls + nOrds)
                                  ]
                           else empty
              makeNumb name = inTags False "NumberingFormat" [("type", "string")] (text name)
              numbForm | Text.isInfixOf lowerRomanName s = makeNumb "i, ii, iii, iv..."
                       | Text.isInfixOf upperRomanName s = makeNumb "I, II, III, IV..."
                       | Text.isInfixOf lowerAlphaName s = makeNumb "a, b, c, d..."
                       | Text.isInfixOf upperAlphaName s = makeNumb "A, B, C, D..."
                       | otherwise = empty
      in  inTags True "ParagraphStyle" ([("Self", "ParagraphStyle/"<>s), ("Name", s)] ++ attrs') props

-- | Convert a WriterState with its inline styles to the ICML listing of Character Styles.
charStylesToDoc :: WriterState -> Doc Text
charStylesToDoc st = vcat $ map makeStyle $ Set.toAscList $ inlineStyles st
  where
    makeStyle s =
      let attrs = concatMap (contains s) [
                               (strikeoutName,   ("StrikeThru", "true"))
                             , (superscriptName, ("Position", "Superscript"))
                             , (subscriptName,   ("Position", "Subscript"))
                             , (smallCapsName,   ("Capitalization", "SmallCaps"))
                             ]
          attrs' | Text.isInfixOf emphName s && Text.isInfixOf strongName s
                                               = ("FontStyle", "Bold Italic") : attrs
                 | Text.isInfixOf strongName s = ("FontStyle", "Bold") : attrs
                 | Text.isInfixOf emphName s   = ("FontStyle", "Italic") : attrs
                 | otherwise                   = attrs
          props = inTags True "Properties" [] $
                    inTags False "BasedOn" [("type", "object")] (text "$ID/NormalCharacterStyle") $$ font
                  where
                    font =
                      if codeName `Text.isInfixOf` s
                         then monospacedFont
                         else empty
      in  inTags True "CharacterStyle" ([("Self", "CharacterStyle/"<>s), ("Name", s)] ++ attrs') props

-- | Escape colon characters as %3a
escapeColons :: Text -> Text
escapeColons = Text.concatMap $ \x -> case x of
  ':' -> "%3a"
  _   -> Text.singleton x

-- | Convert a list of (identifier, url) pairs to the ICML listing of hyperlinks.
hyperlinksToDoc :: Hyperlink -> Doc Text
hyperlinksToDoc []     = empty
hyperlinksToDoc (x:xs) = hyp x $$ hyperlinksToDoc xs
  where
    hyp (ident, url) = hdest $$ hlink
      where
        hdest = selfClosingTag "HyperlinkURLDestination"
                  [("Self", "HyperlinkURLDestination/"<>escapeColons url), ("Name","link"), ("DestinationURL",url), ("DestinationUniqueKey","1")] -- HyperlinkURLDestination with more than one colon crashes CS6
        hlink = inTags True "Hyperlink" [("Self","uf-"<>tshow ident),  ("Name",url),
                    ("Source","htss-"<>tshow ident), ("Visible","true"), ("DestinationUniqueKey","1")]
                  $ inTags True "Properties" []
                  $ inTags False "BorderColor" [("type","enumeration")] (text "Black")
                  $$ inTags False "Destination" [("type","object")] (literal $ "HyperlinkURLDestination/"<>escapeColons (escapeStringForXML url)) -- HyperlinkURLDestination with more than one colon crashes CS6

-- | Key for specifying user-defined styles
dynamicStyleKey :: Text
dynamicStyleKey = "custom-style"

-- | Convert a list of Pandoc blocks to ICML.
blocksToICML :: PandocMonad m => WriterOptions -> Style -> [Block] -> WS m (Doc Text)
blocksToICML opts style lst = do
  docs <- mapM (blockToICML opts style) lst
  return $ intersperseBrs docs

-- | Convert a Pandoc block element to ICML.
blockToICML :: PandocMonad m => WriterOptions -> Style -> Block -> WS m (Doc Text)
blockToICML opts style (Plain lst) = parStyle opts style lst
-- title beginning with fig: indicates that the image is a figure
blockToICML opts style (Para img@[Image _ txt (_,Text.stripPrefix "fig:" -> Just _)]) = do
  figure  <- parStyle opts (figureName:style) img
  caption <- parStyle opts (imgCaptionName:style) txt
  return $ intersperseBrs [figure, caption]
blockToICML opts style (Para lst) = parStyle opts (paragraphName:style) lst
blockToICML opts style (LineBlock lns) =
  blockToICML opts style $ linesToPara lns
blockToICML opts style (CodeBlock _ str) = parStyle opts (codeBlockName:style) [Str str]
blockToICML _ _ b@(RawBlock f str)
  | f == Format "icml" = return $ literal str
  | otherwise          = do
      report $ BlockNotRendered b
      return empty
blockToICML opts style (BlockQuote blocks) = blocksToICML opts (blockQuoteName:style) blocks
blockToICML opts style (OrderedList attribs lst) = listItemsToICML opts orderedListName style (Just attribs) lst
blockToICML opts style (BulletList lst) = listItemsToICML opts bulletListName style Nothing lst
blockToICML opts style (DefinitionList lst) = intersperseBrs `fmap` mapM (definitionListItemToICML opts style) lst
blockToICML opts style (Header lvl (_, cls, _) lst) =
  let stl = (headerName <> tshow lvl <> unnumbered):style
      unnumbered = if "unnumbered" `elem` cls
                   then " (unnumbered)"
                   else ""
  in parStyle opts stl lst
blockToICML _ _ HorizontalRule = return empty -- we could insert a page break instead
blockToICML opts style (Table _ blkCapt specs thead tbody tfoot) =
  let (caption, aligns, widths, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
      style' = tableName : style
      noHeader  = all null headers
      nrHeaders = if noHeader
                     then "0"
                     else "1"
      nrRows = length rows
      nrCols = if null rows
                  then 0
                  else length $ head rows
      rowsToICML [] _ = return empty
      rowsToICML (col:rest) rowNr =
        liftM2 ($$) (colsToICML col aligns rowNr (0::Int)) $ rowsToICML rest (rowNr+1)
      colsToICML [] _ _ _ = return empty
      colsToICML _ [] _ _ = return empty
      colsToICML (cell:rest) (alig:restAligns) rowNr colNr = do
        let stl  = if rowNr == 0 && not noHeader
                      then tableHeaderName:style'
                      else style'
            stl' | alig == AlignLeft = alignLeftName : stl
                 | alig == AlignRight = alignRightName : stl
                 | alig == AlignCenter = alignCenterName : stl
                 | otherwise = stl
        c <- blocksToICML opts stl' cell
        let cl = return $ inTags True "Cell"
                   [("Name", tshow colNr <>":"<> tshow rowNr), ("AppliedCellStyle","CellStyle/Cell")] c
        liftM2 ($$) cl $ colsToICML rest restAligns rowNr (colNr+1)
  in  do
      let tabl = if noHeader
                    then rows
                    else headers:rows
      cells <- rowsToICML tabl (0::Int)
      let colWidths w =
            [("SingleColumnWidth",tshow $ 500 * w) | w > 0]
      let tupToDoc tup = selfClosingTag "Column" $ ("Name",tshow $ fst tup) : colWidths (snd tup)
      let colDescs = vcat $ zipWith (curry tupToDoc) [0..nrCols-1] widths
      let tableDoc = return $ inTags True "Table" [
                         ("AppliedTableStyle","TableStyle/Table")
                       , ("HeaderRowCount", nrHeaders)
                       , ("BodyRowCount", tshow nrRows)
                       , ("ColumnCount", tshow nrCols)
                       ] (colDescs $$ cells)
      liftM2 ($$) tableDoc $ parStyle opts (tableCaptionName:style) caption
blockToICML opts style (Div (_, _, kvs) lst) =
  let dynamicStyle = maybeToList $ lookup dynamicStyleKey kvs
  in  blocksToICML opts (dynamicStyle <> style) lst
blockToICML _ _ Null = return empty

-- | Convert a list of lists of blocks to ICML list items.
listItemsToICML :: PandocMonad m => WriterOptions -> Text -> Style -> Maybe ListAttributes -> [[Block]] -> WS m (Doc Text)
listItemsToICML _ _ _ _ [] = return empty
listItemsToICML opts listType style attribs (first:rest) = do
  st <- get
  put st{ listDepth = 1 + listDepth st}
  let stl = listType:style
  let f = listItemToICML opts stl True attribs first
  let r = map (listItemToICML opts stl False attribs) rest
  docs <- sequence $ f:r
  s    <- get
  let maxD = max (maxListDepth s) (listDepth s)
  put s{ listDepth = 1, maxListDepth = maxD }
  return $ intersperseBrs docs

-- | Convert a list of blocks to ICML list items.
listItemToICML :: PandocMonad m => WriterOptions -> Style -> Bool-> Maybe ListAttributes -> [Block] -> WS m (Doc Text)
listItemToICML opts style isFirst attribs item =
  let makeNumbStart (Just (beginsWith, numbStl, _)) =
        let doN DefaultStyle = []
            doN LowerRoman   = [lowerRomanName]
            doN UpperRoman   = [upperRomanName]
            doN LowerAlpha   = [lowerAlphaName]
            doN UpperAlpha   = [upperAlphaName]
            doN _            = []
            bw =
              [beginsWithName <> tshow beginsWith | beginsWith > 1]
        in  doN numbStl ++ bw
      makeNumbStart Nothing = []
      stl = if isFirst
               then firstListItemName:style
               else style
      stl' = makeNumbStart attribs ++ stl
  in  if length item > 1
         then do
           let insertTab (Para lst) = blockToICML opts (subListParName:style) $ Para $ Str "\t":lst
               insertTab block      = blockToICML opts style block
           f <- blockToICML opts stl' $ head item
           r <- mapM insertTab $ tail item
           return $ intersperseBrs (f : r)
         else blocksToICML opts stl' item

definitionListItemToICML :: PandocMonad m => WriterOptions -> Style -> ([Inline],[[Block]]) -> WS m (Doc Text)
definitionListItemToICML opts style (term,defs) = do
  term' <- parStyle opts (defListTermName:style) term
  defs' <- mapM (blocksToICML opts (defListDefName:style)) defs
  return $ intersperseBrs (term' : defs')


-- | Convert a list of inline elements to ICML.
inlinesToICML :: PandocMonad m => WriterOptions -> Style -> [Inline] -> WS m (Doc Text)
inlinesToICML opts style lst = vcat `fmap` mapM (inlineToICML opts style) (mergeStrings opts lst)

-- | Convert an inline element to ICML.
inlineToICML :: PandocMonad m => WriterOptions -> Style -> Inline -> WS m (Doc Text)
inlineToICML _    style (Str str) = charStyle style $ literal $ escapeStringForXML str
inlineToICML opts style (Emph lst) = inlinesToICML opts (emphName:style) lst
inlineToICML opts style (Underline lst) = inlinesToICML opts (underlineName:style) lst
inlineToICML opts style (Strong lst) = inlinesToICML opts (strongName:style) lst
inlineToICML opts style (Strikeout lst) = inlinesToICML opts (strikeoutName:style) lst
inlineToICML opts style (Superscript lst) = inlinesToICML opts (superscriptName:style) lst
inlineToICML opts style (Subscript lst) = inlinesToICML opts (subscriptName:style) lst
inlineToICML opts style (SmallCaps lst) = inlinesToICML opts (smallCapsName:style) lst
inlineToICML opts style (Quoted SingleQuote lst) = inlinesToICML opts style $
  mergeStrings opts $ [Str "‘"] ++ lst ++ [Str "’"]
inlineToICML opts style (Quoted DoubleQuote lst) = inlinesToICML opts style $
  mergeStrings opts $ [Str "“"] ++ lst ++ [Str "”"]
inlineToICML opts style (Cite _ lst) = inlinesToICML opts (citeName:style) lst
inlineToICML _    style (Code _ str) = charStyle (codeName:style) $ literal $ escapeStringForXML str
inlineToICML _    style Space = charStyle style space
inlineToICML opts style SoftBreak =
  case writerWrapText opts of
       WrapAuto     -> charStyle style space
       WrapNone     -> charStyle style space
       WrapPreserve -> charStyle style cr
inlineToICML _ style LineBreak = charStyle style $ literal lineSeparator
inlineToICML opts style (Math mt str) =
  lift (texMathToInlines mt str) >>=
    (fmap mconcat . mapM (inlineToICML opts style))
inlineToICML _ _ il@(RawInline f str)
  | f == Format "icml" = return $ literal str
  | otherwise          = do
      report $ InlineNotRendered il
      return empty
inlineToICML opts style (Link _ lst (url, title)) = do
  content <- inlinesToICML opts (linkName:style) lst
  state $ \st ->
            let ident = if null $ links st
                           then 1::Int
                           else 1 + fst (head $ links st)
                newst = st{ links = (ident, url):links st }
                cont  = inTags True "HyperlinkTextSource"
                         [("Self","htss-"<>tshow ident), ("Name",title), ("Hidden","false")] content
            in  (cont, newst)
inlineToICML opts style (Image attr _ target) = imageICML opts style attr target
inlineToICML opts style (Note lst) = footnoteToICML opts style lst
inlineToICML opts style (Span (_, _, kvs) lst) =
  let dynamicStyle = maybeToList $ lookup dynamicStyleKey kvs
  in  inlinesToICML opts (dynamicStyle <> style) lst

-- | Convert a list of block elements to an ICML footnote.
footnoteToICML :: PandocMonad m => WriterOptions -> Style -> [Block] -> WS m (Doc Text)
footnoteToICML opts style lst =
  let insertTab (Para ls) = blockToICML opts (footnoteName:style) $ Para $ Str "\t":ls
      insertTab block     = blockToICML opts (footnoteName:style) block
  in  do
    contents <- mapM insertTab lst
    let number = inTags True "ParagraphStyleRange" [] $
                   inTags True "CharacterStyleRange" [] $ inTagsSimple "Content" "<?ACE 4?>"
    return $ inTags True "CharacterStyleRange"
      [("AppliedCharacterStyle","$ID/NormalCharacterStyle"), ("Position","Superscript")]
      $ inTags True "Footnote" [] $ number $$ intersperseBrs contents

-- | Auxiliary function to merge Space elements into the adjacent Strs.
mergeStrings :: WriterOptions -> [Inline] -> [Inline]
mergeStrings opts = mergeStrings' . map spaceToStr
  where spaceToStr Space = Str " "
        spaceToStr SoftBreak = case writerWrapText opts of
                                    WrapPreserve  -> Str "\n"
                                    _             -> Str " "
        spaceToStr x = x

        mergeStrings' (Str x : Str y : zs) = mergeStrings' (Str (x <> y) : zs)
        mergeStrings' (x : xs) = x : mergeStrings' xs
        mergeStrings' []       = []

-- | Intersperse line breaks
intersperseBrs :: [Doc Text] -> Doc Text
intersperseBrs = vcat . intersperse (selfClosingTag "Br" []) . filter (not . isEmpty)

-- | Wrap a list of inline elements in an ICML Paragraph Style
parStyle :: PandocMonad m => WriterOptions -> Style -> [Inline] -> WS m (Doc Text)
parStyle opts style lst =
  let slipIn x y = if Text.null y
                      then x
                      else x <> " > " <> y
      stlStr = foldr slipIn "" $ reverse style
      stl    = if Text.null stlStr
                  then ""
                  else "ParagraphStyle/" <> stlStr
      attrs  = ("AppliedParagraphStyle", stl)
      attrs' =  if firstListItemName `elem` style
                   then let ats = attrs : [("NumberingContinue", "false")]
                            begins = filter (Text.isPrefixOf beginsWithName) style
                        in  if null begins
                               then ats
                               else let i = fromMaybe "" $ Text.stripPrefix beginsWithName
                                                         $ head begins
                                    in  ("NumberingStartAt", i) : ats
                   else [attrs]
  in  do
      content <- inlinesToICML opts [] lst
      let cont = inTags True "ParagraphStyleRange" attrs' content
      state $ \st -> (cont, st{ blockStyles = Set.insert stlStr $ blockStyles st })

-- | Wrap a Doc in an ICML Character Style.
charStyle :: PandocMonad m => Style -> Doc Text -> WS m (Doc Text)
charStyle style content =
  let (stlStr, attrs) = styleToStrAttr style
      doc = inTags True "CharacterStyleRange" attrs $ inTagsSimple "Content" $ flush content
  in
      state $ \st ->
    let styles = if Text.null stlStr
                    then st
                    else st{ inlineStyles = Set.insert stlStr $ inlineStyles st }
    in  (doc, styles)

-- | Transform a Style to a tuple of String (eliminating duplicates and ordered) and corresponding attribute.
styleToStrAttr :: Style -> (Text, [(Text, Text)])
styleToStrAttr style =
  let stlStr = Text.unwords $ Set.toAscList $ Set.fromList style
      stl    = if null style
                  then "$ID/NormalCharacterStyle"
                  else "CharacterStyle/" <> stlStr
      attrs = [("AppliedCharacterStyle", stl)]
  in  (stlStr, attrs)

-- | Assemble an ICML Image.
imageICML :: PandocMonad m => WriterOptions -> Style -> Attr -> Target -> WS m (Doc Text)
imageICML opts style attr (src, _) = do
  imgS <- catchError
          (do (img, _) <- fetchItem src
              case imageSize opts img of
                Right size -> return size
                Left msg   -> do
                  report $ CouldNotDetermineImageSize src msg
                  return def)
           (\e -> do
               report $ CouldNotFetchResource src $ tshow e
               return def)
  let (ow, oh) = sizeInPoints imgS
      (imgWidth, imgHeight) = desiredSizeInPoints opts attr imgS
      hw = showFl $ ow / 2
      hh = showFl $ oh / 2
      scale = showFl (imgWidth / ow) <> " 0 0 " <> showFl (imgHeight / oh)
      src' = if isURI src then src else "file:" <> src
      (stlStr, attrs) = styleToStrAttr style
      props  = inTags True "Properties" [] $ inTags True "PathGeometry" []
                 $ inTags True "GeometryPathType" [("PathOpen","false")]
                 $ inTags True "PathPointArray" []
                 $ vcat [
                     selfClosingTag "PathPointType" [("Anchor", "-"<>hw<>" -"<>hh),
                       ("LeftDirection", "-"<>hw<>" -"<>hh), ("RightDirection", "-"<>hw<>" -"<>hh)]
                   , selfClosingTag "PathPointType" [("Anchor", "-"<>hw<>" "<>hh),
                       ("LeftDirection", "-"<>hw<>" "<>hh), ("RightDirection", "-"<>hw<>" "<>hh)]
                   , selfClosingTag "PathPointType" [("Anchor", hw<>" "<>hh),
                       ("LeftDirection", hw<>" "<>hh), ("RightDirection", hw<>" "<>hh)]
                   , selfClosingTag "PathPointType" [("Anchor", hw<>" -"<>hh),
                       ("LeftDirection", hw<>" -"<>hh), ("RightDirection", hw<>" -"<>hh)]
                   ]
      image  = inTags True "Image"
                   [("Self","ue6"), ("ItemTransform", scale<>" -"<>hw<>" -"<>hh)]
                 $ vcat [
                     inTags True "Properties" [] $ inTags True "Profile" [("type","string")] $ text "$ID/Embedded"
                   , selfClosingTag "Link" [("Self", "ueb"), ("LinkResourceURI", src')]
                   ]
      doc    = inTags True "CharacterStyleRange" attrs
                 $ inTags True "Rectangle" [("Self","uec"), ("StrokeWeight", "0"),
                     ("ItemTransform", scale<>" "<>hw<>" -"<>hh)] (props $$ image)
  state $ \st -> (doc, st{ inlineStyles = Set.insert stlStr $ inlineStyles st } )
