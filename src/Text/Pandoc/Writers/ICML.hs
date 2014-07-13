{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : Text.Pandoc.Writers.ICML
   Copyright   : Copyright (C) 2013 github.com/mb21
   License     : GNU GPL, version 2 or above

   Stability   : alpha

Conversion of 'Pandoc' documents to Adobe InCopy ICML, a stand-alone XML format
which is a subset of the zipped IDML format for which the documentation is
available here: http://wwwimages.adobe.com/www.adobe.com/content/dam/Adobe/en/devnet/indesign/sdk/cs6/idml/idml-specification.pdf
InCopy is the companion word-processor to Adobe InDesign and ICML documents can be integrated
into InDesign with File -> Place.
-}
module Text.Pandoc.Writers.ICML (writeICML) where
import Text.Pandoc.Definition
import Text.Pandoc.XML
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Shared (splitBy)
import Text.Pandoc.Options
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Pretty
import Data.List (isPrefixOf, isInfixOf, stripPrefix)
import Data.Text as Text (breakOnAll, pack)
import Data.Monoid (mappend)
import Control.Monad.State
import qualified Data.Set as Set

type Style = [String]
type Hyperlink = [(Int, String)]

data WriterState = WriterState{
    blockStyles  :: Set.Set String
  , inlineStyles :: Set.Set String
  , links        :: Hyperlink
  , listDepth    :: Int
  , maxListDepth :: Int
  }

type WS a = State WriterState a

defaultWriterState :: WriterState
defaultWriterState = WriterState{
    blockStyles  = Set.empty
  , inlineStyles = Set.empty
  , links        = []
  , listDepth    = 1
  , maxListDepth = 0
  }

-- inline names (appear in InDesign's character styles pane)
emphName        :: String
strongName      :: String
strikeoutName   :: String
superscriptName :: String
subscriptName   :: String
smallCapsName   :: String
codeName        :: String
linkName        :: String
emphName        = "Italic"
strongName      = "Bold"
strikeoutName   = "Strikeout"
superscriptName = "Superscript"
subscriptName   = "Subscript"
smallCapsName   = "SmallCaps"
codeName        = "Code"
linkName        = "Link"

-- block element names (appear in InDesign's paragraph styles pane)
paragraphName     :: String
codeBlockName     :: String
rawBlockName      :: String
blockQuoteName    :: String
orderedListName   :: String
bulletListName    :: String
defListTermName   :: String
defListDefName    :: String
headerName        :: String
tableName         :: String
tableHeaderName   :: String
tableCaptionName  :: String
alignLeftName     :: String
alignRightName    :: String
alignCenterName   :: String
firstListItemName :: String
beginsWithName    :: String
lowerRomanName    :: String
upperRomanName    :: String
lowerAlphaName    :: String
upperAlphaName    :: String
subListParName    :: String
footnoteName      :: String
paragraphName     = "Paragraph"
codeBlockName     = "CodeBlock"
rawBlockName      = "Rawblock"
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


-- | Convert Pandoc document to string in ICML format.
writeICML :: WriterOptions -> Pandoc -> String
writeICML opts (Pandoc meta blocks) =
  let colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
      render' = render colwidth
      renderMeta f s = Just $ render' $ fst $ runState (f opts [] s) defaultWriterState
      Just metadata = metaToJSON opts
                 (renderMeta blocksToICML)
                 (renderMeta inlinesToICML)
                 meta
      (doc, st) = runState (blocksToICML opts [] blocks) defaultWriterState
      main    = render' doc
      context = defField "body" main
              $ defField "charStyles" (render' $ charStylesToDoc st)
              $ defField "parStyles"  (render' $ parStylesToDoc st)
              $ defField "hyperlinks" (render' $ hyperlinksToDoc $ links st)
              $ metadata
  in  if writerStandalone opts
         then renderTemplate' (writerTemplate opts) context
         else main

-- | Auxilary functions for parStylesToDoc and charStylesToDoc.
contains :: String -> (String, (String, String)) -> [(String, String)]
contains s rule =
  if isInfixOf (fst rule) s
     then [snd rule]
     else []

-- | The monospaced font to use as default.
monospacedFont :: Doc
monospacedFont = inTags False "AppliedFont" [("type", "string")] $ text "Courier New"

-- | How much to indent blockquotes etc.
defaultIndent :: Int
defaultIndent = 20

-- | How much to indent numbered lists before the number.
defaultListIndent :: Int
defaultListIndent = 10

-- other constants
lineSeparator :: String
lineSeparator = "&#x2028;"

-- | Convert a WriterState with its block styles to the ICML listing of Paragraph Styles.
parStylesToDoc :: WriterState -> Doc
parStylesToDoc st = vcat $ map makeStyle $ Set.toAscList $ blockStyles st
  where
    makeStyle s =
      let countSubStrs sub str = length $ Text.breakOnAll (Text.pack sub) (Text.pack str)
          attrs = concat $ map (contains s) $ [
                               (defListTermName, ("BulletsAndNumberingListType", "BulletList"))
                             , (defListTermName, ("FontStyle", "Bold"))
                             , (tableHeaderName, ("FontStyle", "Bold"))
                             , (alignLeftName,   ("Justification", "LeftAlign"))
                             , (alignRightName,  ("Justification", "RightAlign"))
                             , (alignCenterName, ("Justification", "CenterAlign"))
                             , (headerName++"1", ("PointSize", "36"))
                             , (headerName++"2", ("PointSize", "30"))
                             , (headerName++"3", ("PointSize", "24"))
                             , (headerName++"4", ("PointSize", "18"))
                             , (headerName++"5", ("PointSize", "14"))
                             ]
          -- what is the most nested list type, if any?
          (isBulletList, isOrderedList) = findList $ reverse $ splitBy (==' ') s
            where
              findList [] = (False, False)
              findList (x:xs) | x == bulletListName  = (True, False)
                              | x == orderedListName = (False, True)
                              | otherwise = findList xs
          nBuls = countSubStrs bulletListName s
          nOrds = countSubStrs orderedListName s
          attrs' = numbering ++ listType ++ indent ++ attrs
            where
              numbering | isOrderedList = [("NumberingExpression", "^#.^t"), ("NumberingLevel", show nOrds)]
                        | otherwise     = []
              listType | isOrderedList && (not $ isInfixOf subListParName s)
                           = [("BulletsAndNumberingListType", "NumberedList")]
                       | isBulletList && (not $ isInfixOf subListParName s)
                           = [("BulletsAndNumberingListType", "BulletList")]
                       | otherwise = []
              indent = [("LeftIndent", show indt)]
                where
                  nBlockQuotes = countSubStrs blockQuoteName s
                  nDefLists = countSubStrs defListDefName s
                  indt = max 0 $ defaultListIndent*(nBuls + nOrds - 1) + defaultIndent*(nBlockQuotes + nDefLists)
          props = inTags True "Properties" [] $ (basedOn $$ tabList $$ numbForm)
            where
              font = if isInfixOf codeBlockName s
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
              numbForm | isInfixOf lowerRomanName s = makeNumb "i, ii, iii, iv..."
                       | isInfixOf upperRomanName s = makeNumb "I, II, III, IV..."
                       | isInfixOf lowerAlphaName s = makeNumb "a, b, c, d..."
                       | isInfixOf upperAlphaName s = makeNumb "A, B, C, D..."
                       | otherwise = empty
      in  inTags True "ParagraphStyle" ([("Self", "ParagraphStyle/"++s), ("Name", s)] ++ attrs') props

-- | Convert a WriterState with its inline styles to the ICML listing of Character Styles.
charStylesToDoc :: WriterState -> Doc
charStylesToDoc st = vcat $ map makeStyle $ Set.toAscList $ inlineStyles st
  where
    makeStyle s =
      let attrs = concat $ map (contains s) [
                               (strikeoutName,   ("StrikeThru", "true"))
                             , (superscriptName, ("Position", "Superscript"))
                             , (subscriptName,   ("Position", "Subscript"))
                             , (smallCapsName,   ("Capitalization", "SmallCaps"))
                             ]
          attrs' | isInfixOf emphName s && isInfixOf strongName s = ("FontStyle", "Bold Italic") : attrs
                 | isInfixOf strongName s                         = ("FontStyle", "Bold") : attrs
                 | isInfixOf emphName s                           = ("FontStyle", "Italic") : attrs
                 | otherwise                                      = attrs
          props = inTags True "Properties" [] $
                    inTags False "BasedOn" [("type", "object")] (text "$ID/NormalCharacterStyle") $$ font
                  where
                    font =
                      if isInfixOf codeName s
                         then monospacedFont
                         else empty
      in  inTags True "CharacterStyle" ([("Self", "CharacterStyle/"++s), ("Name", s)] ++ attrs') props

-- | Convert a list of (identifier, url) pairs to the ICML listing of hyperlinks.
hyperlinksToDoc :: Hyperlink -> Doc
hyperlinksToDoc []     = empty
hyperlinksToDoc (x:xs) = hyp x $$ hyperlinksToDoc xs
  where
    hyp (ident, url) = hdest $$ hlink
      where
        hdest = selfClosingTag "HyperlinkURLDestination"
                  [("Self", "HyperlinkURLDestination/"++url), ("Name","link"), ("DestinationURL",url), ("DestinationUniqueKey","1")]
        hlink = inTags True "Hyperlink" [("Self","uf-"++show ident),  ("Name",url),
                    ("Source","htss-"++show ident), ("Visible","true"), ("DestinationUniqueKey","1")]
                  $ inTags True "Properties" []
                  $ inTags False "BorderColor" [("type","enumeration")] (text "Black")
                  $$ (inTags False "Destination" [("type","object")]
                  $ text $ "HyperlinkURLDestination/"++(escapeStringForXML url))


-- | Convert a list of Pandoc blocks to ICML.
blocksToICML :: WriterOptions -> Style -> [Block] -> WS Doc
blocksToICML opts style lst = vcat `fmap` mapM (blockToICML opts style) lst

-- | Convert a Pandoc block element to ICML.
blockToICML :: WriterOptions -> Style -> Block -> WS Doc
blockToICML opts style (Plain lst) = parStyle opts style lst
blockToICML opts style (Para lst) = parStyle opts (paragraphName:style) lst
blockToICML opts style (CodeBlock _ str) = parStyle opts (codeBlockName:style) $ [Str str]
blockToICML opts style (RawBlock _ str) = parStyle opts (rawBlockName:style) $ [Str str]
blockToICML opts style (BlockQuote blocks) = blocksToICML opts (blockQuoteName:style) blocks
blockToICML opts style (OrderedList attribs lst) = listItemsToICML opts orderedListName style (Just attribs) lst
blockToICML opts style (BulletList lst) = listItemsToICML opts bulletListName style Nothing lst
blockToICML opts style (DefinitionList lst) = vcat `fmap` mapM (definitionListItemToICML opts style) lst
blockToICML opts style (Header lvl _ lst) =
  let stl = (headerName ++ show lvl):style
  in parStyle opts stl lst
blockToICML _ _ HorizontalRule = return empty -- we could insert a page break instead
blockToICML opts style (Table caption aligns widths headers rows) =
  let style' = tableName : style
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
        liftM2 ($$) (colsToICML col rowNr (0::Int)) $ rowsToICML rest (rowNr+1)
      colsToICML [] _ _ = return empty
      colsToICML (cell:rest) rowNr colNr = do
        let stl  = if rowNr == 0 && not noHeader
                      then tableHeaderName:style'
                      else style'
            alig = aligns !! colNr
            stl' | alig == AlignLeft = alignLeftName : stl
                 | alig == AlignRight = alignRightName : stl
                 | alig == AlignCenter = alignCenterName : stl
                 | otherwise = stl
        c <- blocksToICML opts stl' cell
        let cl = return $ inTags True "Cell"
                   [("Name", show colNr ++":"++ show rowNr), ("AppliedCellStyle","CellStyle/Cell")] c
        liftM2 ($$) cl $ colsToICML rest rowNr (colNr+1)
  in  do
      let tabl = if noHeader
                    then rows
                    else headers:rows
      cells <- rowsToICML tabl (0::Int)
      let colWidths w = if w > 0
                           then [("SingleColumnWidth",show $ 500 * w)]
                           else []
      let tupToDoc tup = selfClosingTag "Column" $ [("Name",show $ fst tup)] ++ (colWidths $ snd tup)
      let colDescs = vcat $ map tupToDoc $ zip [0..nrCols-1] widths
      let tableDoc = return $ inTags True "Table" [
                         ("AppliedTableStyle","TableStyle/Table")
                       , ("HeaderRowCount", nrHeaders)
                       , ("BodyRowCount", show nrRows)
                       , ("ColumnCount", show nrCols)
                       ] (colDescs $$ cells)
      liftM2 ($$) tableDoc $ parStyle opts (tableCaptionName:style) caption
blockToICML opts style (Div _ lst) = blocksToICML opts style lst
blockToICML _ _ Null = return empty

-- | Convert a list of lists of blocks to ICML list items.
listItemsToICML :: WriterOptions -> String -> Style -> Maybe ListAttributes -> [[Block]] -> WS Doc
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
  return $ vcat docs

-- | Convert a list of blocks to ICML list items.
listItemToICML :: WriterOptions -> Style -> Bool-> Maybe ListAttributes -> [Block] -> WS Doc
listItemToICML opts style isFirst attribs item =
  let makeNumbStart (Just (beginsWith, numbStl, _)) =
        let doN DefaultStyle = []
            doN LowerRoman = [lowerRomanName]
            doN UpperRoman = [upperRomanName]
            doN LowerAlpha = [lowerAlphaName]
            doN UpperAlpha = [upperAlphaName]
            doN _ = []
            bw = if beginsWith > 1
                    then [beginsWithName ++ show beginsWith]
                    else []
        in  doN numbStl ++ bw
      makeNumbStart Nothing = []
      stl = if isFirst
               then firstListItemName:style
               else style
      stl' = makeNumbStart attribs ++ stl
  in  if length item > 1
         then do
           let insertTab (Para lst) = blockToICML opts (subListParName:style) $ Para $ (Str "\t"):lst
               insertTab block      = blockToICML opts style block
           f <- blockToICML opts stl' $ head item
           r <- fmap vcat $ mapM insertTab $ tail item
           return $ f $$ r
         else blocksToICML opts stl' item

definitionListItemToICML :: WriterOptions -> Style -> ([Inline],[[Block]]) -> WS Doc
definitionListItemToICML opts style (term,defs) = do
  term' <- parStyle opts (defListTermName:style) term
  defs' <- vcat `fmap` mapM (blocksToICML opts (defListDefName:style)) defs
  return $ term' $$ defs'


-- | Convert a list of inline elements to ICML.
inlinesToICML :: WriterOptions -> Style -> [Inline] -> WS Doc
inlinesToICML opts style lst = vcat `fmap` mapM (inlineToICML opts style) (mergeSpaces lst)

-- | Convert an inline element to ICML.
inlineToICML :: WriterOptions -> Style -> Inline -> WS Doc
inlineToICML _    style (Str str) = charStyle style $ text $ escapeStringForXML str
inlineToICML opts style (Emph lst) = inlinesToICML opts (emphName:style) lst
inlineToICML opts style (Strong lst) = inlinesToICML opts (strongName:style) lst
inlineToICML opts style (Strikeout lst) = inlinesToICML opts (strikeoutName:style) lst
inlineToICML opts style (Superscript lst) = inlinesToICML opts (superscriptName:style) lst
inlineToICML opts style (Subscript lst) = inlinesToICML opts (subscriptName:style) lst
inlineToICML opts style (SmallCaps lst) = inlinesToICML opts (smallCapsName:style) lst
inlineToICML opts style (Quoted SingleQuote lst) = inlinesToICML opts style $ [Str "‘"] ++ lst ++ [Str "’"]
inlineToICML opts style (Quoted DoubleQuote lst) = inlinesToICML opts style $ [Str "“"] ++ lst ++ [Str "”"]
inlineToICML opts style (Cite _ lst) = footnoteToICML opts style [Para lst]
inlineToICML _    style (Code _ str) = charStyle (codeName:style) $ text $ escapeStringForXML str
inlineToICML _    style Space = charStyle style space
inlineToICML _ style LineBreak = charStyle style $ text lineSeparator
inlineToICML _ style (Math _ str) = charStyle style $ text $ escapeStringForXML str --InDesign doesn't really do math
inlineToICML _ style (RawInline _ str) = charStyle style $ text $ escapeStringForXML str
inlineToICML opts style (Link lst (url, title)) = do
  content <- inlinesToICML opts (linkName:style) lst
  state $ \st ->
            let ident = if null $ links st
                           then 1::Int
                           else 1 + (fst $ head $ links st)
                newst = st{ links = (ident, url):(links st) }
                cont  = inTags True "HyperlinkTextSource"
                         [("Self","htss-"++show ident), ("Name",title), ("Hidden","false")] content
            in  (cont, newst)
inlineToICML opts style (Image alt target) = imageICML opts style alt target
inlineToICML opts style (Note lst) = footnoteToICML opts style lst
inlineToICML opts style (Span _ lst) = inlinesToICML opts style lst

-- | Convert a list of block elements to an ICML footnote.
footnoteToICML :: WriterOptions -> Style -> [Block] -> WS Doc
footnoteToICML opts style lst =
  let insertTab (Para ls) = blockToICML opts (footnoteName:style) $ Para $ (Str "\t"):ls
      insertTab block     = blockToICML opts (footnoteName:style) block
  in  do
    contents <- mapM insertTab lst
    let number = inTags True "ParagraphStyleRange" [] $
                   inTags True "CharacterStyleRange" [] $ inTagsSimple "Content" "<?ACE 4?>"
    return $ inTags True "CharacterStyleRange"
      [("AppliedCharacterStyle","$ID/NormalCharacterStyle"), ("Position","Superscript")]
      $ inTags True "Footnote" [] $ number $$ vcat contents

-- | Auxiliary function to merge Space elements into the adjacent Strs.
mergeSpaces :: [Inline] -> [Inline]
mergeSpaces ((Str s):(Space:((Str s'):xs))) = mergeSpaces $ Str(s++" "++s') : xs
mergeSpaces (Space:((Str s):xs)) = mergeSpaces $ Str (" "++s) : xs
mergeSpaces ((Str s):(Space:xs)) = mergeSpaces $ Str (s++" ") : xs
mergeSpaces (x:xs) = x : (mergeSpaces xs)
mergeSpaces []     = []

-- | Wrap a list of inline elements in an ICML Paragraph Style
parStyle :: WriterOptions -> Style -> [Inline] -> WS Doc
parStyle opts style lst =
  let slipIn x y = if null y
                      then x
                      else x ++ " > " ++ y
      stlStr = foldr slipIn [] $ reverse style
      stl    = if null stlStr
                  then ""
                  else "ParagraphStyle/" ++ stlStr
      attrs  = ("AppliedParagraphStyle", stl)
      attrs' =  if firstListItemName `elem` style
                   then let ats = attrs : [("NumberingContinue", "false")]
                            begins = filter (isPrefixOf beginsWithName) style
                        in  if null begins
                               then ats
                               else let i = maybe "" id $ stripPrefix beginsWithName $ head begins
                                    in  ("NumberingStartAt", i) : ats
                   else [attrs]
  in  do
      content <- inlinesToICML opts [] lst
      let cont = inTags True "ParagraphStyleRange" attrs'
                   $ mappend content $ selfClosingTag "Br" []
      state $ \st -> (cont, st{ blockStyles = Set.insert stlStr $ blockStyles st })

-- | Wrap a Doc in an ICML Character Style.
charStyle :: Style -> Doc -> WS Doc
charStyle style content =
  let (stlStr, attrs) = styleToStrAttr style
      doc = inTags True "CharacterStyleRange" attrs $ inTagsSimple "Content" $ flush content
  in  do
      state $ \st ->
        let styles = if null stlStr
                        then st
                        else st{ inlineStyles = Set.insert stlStr $ inlineStyles st }
        in  (doc, styles)

-- | Transform a Style to a tuple of String (eliminating duplicates and ordered) and corresponding attribute.
styleToStrAttr :: Style -> (String, [(String, String)])
styleToStrAttr style =
  let stlStr = unwords $ Set.toAscList $ Set.fromList style
      stl    = if null style
                  then "$ID/NormalCharacterStyle"
                  else "CharacterStyle/" ++ stlStr
      attrs = [("AppliedCharacterStyle", stl)]
  in  (stlStr, attrs)

-- | Assemble an ICML Image.
imageICML :: WriterOptions -> Style -> [Inline] -> Target -> WS Doc
imageICML _ style _ (linkURI, _) =
  let imgWidth  = 300::Int --TODO: set width, height dynamically as in Docx.hs
      imgHeight = 200::Int
      scaleFact = show (1::Double) --TODO: set scaling factor so image is scaled exactly to imgWidth x imgHeight
      hw = show $ imgWidth  `div` 2
      hh = show $ imgHeight `div` 2
      qw = show $ imgWidth  `div` 4
      qh = show $ imgHeight `div` 4
      (stlStr, attrs) = styleToStrAttr style
      props  = inTags True "Properties" [] $ inTags True "PathGeometry" []
                 $ inTags True "GeometryPathType" [("PathOpen","false")]
                 $ inTags True "PathPointArray" []
                 $ vcat [
                     selfClosingTag "PathPointType" [("Anchor", "-"++qw++" -"++qh),
                       ("LeftDirection", "-"++qw++" -"++qh), ("RightDirection", "-"++qw++" -"++qh)]
                   , selfClosingTag "PathPointType" [("Anchor", "-"++qw++" "++qh),
                       ("LeftDirection", "-"++qw++" "++qh), ("RightDirection", "-"++qw++" "++qh)]
                   , selfClosingTag "PathPointType" [("Anchor", qw++" "++qh),
                       ("LeftDirection", qw++" "++qh), ("RightDirection", qw++" "++qh)]
                   , selfClosingTag "PathPointType" [("Anchor", qw++" -"++qh),
                       ("LeftDirection", qw++" -"++qh), ("RightDirection", qw++" -"++qh)]
                   ]
      image  = inTags True "Image"
                   [("Self","ue6"), ("ItemTransform", scaleFact++" 0 0 "++scaleFact++" -"++qw++" -"++qh)]
                 $ vcat [
                     inTags True "Properties" [] $ inTags True "Profile" [("type","string")] $ text "$ID/Embedded"
                       $$ selfClosingTag "GraphicBounds" [("Left","0"), ("Top","0"), ("Right", hw), ("Bottom", hh)]
                   , selfClosingTag "Link" [("Self", "ueb"), ("LinkResourceURI", linkURI)]
                   ]
      doc    = inTags True "CharacterStyleRange" attrs
                 $ inTags True "Rectangle" [("Self","uec"), ("ItemTransform", "1 0 0 1 "++qw++" -"++qh)]
                 $ (props $$ image)
  in  do
      state $ \st -> (doc, st{ inlineStyles = Set.insert stlStr $ inlineStyles st } )
