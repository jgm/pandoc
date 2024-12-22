{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{- |
   Module      : Text.Pandoc.Writers.Docx
   Copyright   : Copyright (C) 2012-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to docx.
-}
module Text.Pandoc.Writers.Docx.OpenXML ( writeOpenXML, maxListLevel ) where

import Control.Monad (when, unless)
import Control.Applicative ((<|>))
import Control.Monad.Except (catchError)
import Crypto.Hash (hashWith, SHA1(SHA1))
import qualified Data.ByteString.Lazy as BL
import Data.Char (isLetter, isSpace)
import Text.Pandoc.Char (isCJK)
import Data.Ord (comparing)
import Data.String (fromString)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, maybeToList, isJust)
import Control.Monad.State ( gets, modify, MonadTrans(lift) )
import Control.Monad.Reader ( asks, MonadReader(local) )
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Skylighting
import Text.DocLayout (hcat, vcat, literal, render)
import Text.Pandoc.Class (PandocMonad, report, getMediaBag)
import Text.Pandoc.Translations (Term(Abstract), translateTerm)
import Text.Pandoc.MediaBag (lookupMedia, MediaItem(..))
import qualified Text.Pandoc.Translations as Term
import qualified Text.Pandoc.Class.PandocMonad as P
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.UTF8 (fromText)
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting (highlight)
import Text.Pandoc.Templates (compileDefaultTemplate, renderTemplate)
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.MIME (extensionFromMimeType, getMimeType)
import Text.Pandoc.Options
import Text.Pandoc.Writers.Docx.StyleMap
import Text.Pandoc.Writers.Docx.Table as Table
import Text.Pandoc.Writers.Docx.Types
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import qualified Text.Pandoc.Writers.GridTable as Grid
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import Text.TeXMath
import Text.Pandoc.Writers.OOXML
import Text.Pandoc.XML.Light as XML
import Data.List (sortBy, intercalate, groupBy)

-- from wml.xsd EG_RPrBase
rPrTagOrder :: M.Map Text Int
rPrTagOrder =
  M.fromList
  (zip [ "rStyle"
    , "rFonts"
    , "b"
    , "bCs"
    , "i"
    , "iCs"
    , "caps"
    , "smallCaps"
    , "strike"
    , "dstrike"
    , "outline"
    , "shadow"
    , "emboss"
    , "imprint"
    , "noProof"
    , "snapToGrid"
    , "vanish"
    , "webHidden"
    , "color"
    , "spacing"
    , "w"
    , "kern"
    , "position"
    , "sz"
    , "szCs"
    , "highlight"
    , "u"
    , "effect"
    , "bdr"
    , "shd"
    , "fitText"
    , "vertAlign"
    , "rtl"
    , "cs"
    , "em"
    , "lang"
    , "eastAsianLayout"
    , "specVanish"
    , "oMath"
    ] [0..])

sortSquashed :: [Element] -> [Element]
sortSquashed l =
  sortBy (comparing tagIndex) l
  where
    tagIndex :: Element -> Int
    tagIndex el =
      fromMaybe 0 (M.lookup tag rPrTagOrder)
      where tag = (qName . elName) el

squashProps :: EnvProps -> [Element]
squashProps (EnvProps Nothing es) = sortSquashed es
squashProps (EnvProps (Just e) es) = sortSquashed (e : es)

-- | Certain characters are invalid in XML even if escaped.
-- See #1992
stripInvalidChars :: Text -> Text
stripInvalidChars = T.filter isValidChar

-- | See XML reference
isValidChar :: Char -> Bool
isValidChar '\t' = True
isValidChar '\n' = True
isValidChar '\r' = True
isValidChar '\xFFFE' = False
isValidChar '\xFFFF' = False
isValidChar c = (' ' <= c && c <= '\xD7FF') || ('\xE000' <= c)

-- this is the lowest number used for a list numId
baseListId :: Int
baseListId = 1000

getNumId :: (PandocMonad m) => WS m Int
getNumId = gets (((baseListId - 1) +) . length . stLists)

makeTOC :: (PandocMonad m) => WriterOptions -> WS m [Element]
makeTOC opts = do
  let depth = "1-" <> tshow (writerTOCDepth opts)
  let tocCmd = "TOC \\o \"" <> depth <> "\" \\h \\z \\u"
  tocTitle <- gets stTocTitle
  title <- withParaPropM (pStyleM "TOC Heading") (blocksToOpenXML opts [Para tocTitle])
  return
    [mknode "w:sdt" [] [
      mknode "w:sdtPr" [] (
        mknode "w:docPartObj" []
          [mknode "w:docPartGallery" [("w:val","Table of Contents")] (),
          mknode "w:docPartUnique" [] ()]
         -- w:docPartObj
      ), -- w:sdtPr
      mknode "w:sdtContent" [] (title ++ [ Elem $
        mknode "w:p" [] (
          mknode "w:r" [] [
            mknode "w:fldChar" [("w:fldCharType","begin"),("w:dirty","true")] (),
            mknode "w:instrText" [("xml:space","preserve")] tocCmd,
            mknode "w:fldChar" [("w:fldCharType","separate")] (),
            mknode "w:fldChar" [("w:fldCharType","end")] ()
          ] -- w:r
        ) -- w:p
      ])
    ]] -- w:sdt

makeLOF :: (PandocMonad m) => WriterOptions -> WS m [Element]
makeLOF opts = do
  let lofCmd = "TOC \\h \\z \\t \"Image Caption\" \\c" :: Text
  lofTitle <- B.toList <$> B.text <$> translateTerm Term.ListOfFigures
  title <- withParaPropM (pStyleM "TOC Heading") (blocksToOpenXML opts [Para lofTitle])
  return
    [mknode "w:sdt" [] [
      mknode "w:sdtPr" [] (
        mknode "w:docPartObj" []
          [mknode "w:docPartGallery" [("w:val","List of Figures")] (),
          mknode "w:docPartUnique" [] ()]
         -- w:docPartObj
      ), -- w:sdtPr
      mknode "w:sdtContent" [] (title ++ [ Elem $
        mknode "w:p" [] (
          mknode "w:r" [] [
            mknode "w:fldChar" [("w:fldCharType","begin"),("w:dirty","true")] (),
            mknode "w:instrText" [("xml:space","preserve")] lofCmd,
            mknode "w:fldChar" [("w:fldCharType","separate")] (),
            mknode "w:fldChar" [("w:fldCharType","end")] ()
          ] -- w:r
        ) -- w:p
      ]) -- w:sdtContent
    ]] -- w:sdt

makeLOT :: (PandocMonad m) => WriterOptions -> WS m [Element]
makeLOT opts = do
  let lotCmd = "TOC \\h \\z \\t \"Table Caption\" \\c" :: Text
  lotTitle <- B.toList <$> B.text <$> translateTerm Term.ListOfTables
  title <- withParaPropM (pStyleM "TOC Heading") (blocksToOpenXML opts [Para lotTitle])
  return
    [mknode "w:sdt" [] [
      mknode "w:sdtPr" [] (
        mknode "w:docPartObj" []
          [mknode "w:docPartGallery" [("w:val","List of Tables")] (),
          mknode "w:docPartUnique" [] ()]
         -- w:docPartObj
      ), -- w:sdtPr
      mknode "w:sdtContent" [] (title ++ [ Elem $
        mknode "w:p" [] (
          mknode "w:r" [] [
            mknode "w:fldChar" [("w:fldCharType","begin"),("w:dirty","true")] (),
            mknode "w:instrText" [("xml:space","preserve")] lotCmd,
            mknode "w:fldChar" [("w:fldCharType","separate")] (),
            mknode "w:fldChar" [("w:fldCharType","end")] ()
          ] -- w:r
        ) -- w:p
      ]) -- w:sdtContent
    ]] -- w:sdt

-- | Convert Pandoc document to rendered document contents plus two lists of
-- OpenXML elements (footnotes and comments).
writeOpenXML :: PandocMonad m
             => WriterOptions -> Pandoc
             -> WS m (Text, [Element], [Element])
writeOpenXML opts (Pandoc meta blocks) = do
  setupTranslations meta
  let includeTOC = writerTableOfContents opts || lookupMetaBool "toc" meta
  let includeLOF = writerListOfFigures opts || lookupMetaBool "lof" meta
  let includeLOT = writerListOfTables opts || lookupMetaBool "lot" meta
  abstractTitle <- case lookupMeta "abstract-title" meta of
      Just (MetaBlocks bs)   -> pure $ stringify bs
      Just (MetaInlines ils) -> pure $ stringify ils
      Just (MetaString s)    -> pure s
      _                      -> translateTerm Abstract
  abstract <-
    case lookupMetaBlocks "abstract" meta of
      [] -> return mempty
      xs -> vcat . map (literal . showContent) <$>
              withParaPropM (pStyleM "Abstract") (blocksToOpenXML opts xs)

  let toInlineMeta field = hcat . map (literal . showContent) <$>
         inlinesToOpenXML opts (lookupMetaInlines field meta)

  title <- toInlineMeta "title"
  subtitle <- toInlineMeta "subtitle"
  date <- toInlineMeta "date"

  author <- mapM
             (fmap (hcat . map (literal . showContent)) . inlinesToOpenXML opts)
             (docAuthors meta)

  doc' <- setFirstPara >> blocksToOpenXML opts blocks
  let body = vcat $ map (literal . showContent) doc'
  notes' <- gets (reverse . stFootnotes)
  comments <- gets (reverse . stComments)
  let toComment (kvs, ils) = do
        annotation <- inlinesToOpenXML opts ils
        return $
          mknode "w:comment" [("w:" <> k, v) | (k,v) <- kvs]
            [ mknode "w:p" [] $
              map Elem
              [ mknode "w:pPr" []
                [ mknode "w:pStyle" [("w:val", "CommentText")] () ]
              , mknode "w:r" []
                [ mknode "w:rPr" []
                  [ mknode "w:rStyle" [("w:val", "CommentReference")] ()
                  ]
                  , mknode "w:annotationRef" [] ()
                ]
              ] ++ annotation
            ]
  comments' <- mapM toComment comments
  toc <- if includeTOC
            then makeTOC opts
            else return []
  lof <- if includeLOF
            then makeLOF opts
            else return []
  lot <- if includeLOT
            then makeLOT opts
            else return []
  metadata <- metaToContext opts
                 (fmap (vcat . map (literal . showContent)) . blocksToOpenXML opts)
                 (fmap (hcat . map (literal . showContent)) . inlinesToOpenXML opts)
                 meta
  cStyleMap <- gets (smParaStyle . stStyleMaps)
  let styleIdOf name = fromStyleId $ getStyleIdFromName name cStyleMap
  let context = resetField "body" body
              . resetField "toc"
                   (vcat (map (literal . showElement) toc))
              . resetField "lof"
                   (vcat (map (literal . showElement) lof))
              . resetField "lot"
                   (vcat (map (literal . showElement) lot))
              . resetField "title" title
              . resetField "subtitle" subtitle
              . resetField "author" author
              . resetField "date" date
              . resetField "abstract-title" abstractTitle
              . resetField "abstract" abstract
              . resetField "title-style-id" (styleIdOf "Title")
              . resetField "subtitle-style-id" (styleIdOf "Subtitle")
              . resetField "author-style-id" (styleIdOf "Author")
              . resetField "date-style-id" (styleIdOf "Date")
              . resetField "abstract-title-style-id" (styleIdOf "AbstractTitle")
              . resetField "abstract-style-id" (styleIdOf "Abstract")
              $ metadata
  tpl <- maybe (lift $ compileDefaultTemplate "openxml") pure $ writerTemplate opts
  let rendered = render Nothing $ renderTemplate tpl context
  return (rendered, notes', comments')

-- | Convert a list of Pandoc blocks to OpenXML.
blocksToOpenXML :: (PandocMonad m) => WriterOptions -> [Block] -> WS m [Content]
blocksToOpenXML opts = fmap concat . mapM (blockToOpenXML opts) . separateTables . filter (not . isForeignRawBlock)

isForeignRawBlock :: Block -> Bool
isForeignRawBlock (RawBlock format _) = format /= "openxml"
isForeignRawBlock _                   = False

-- Word combines adjacent tables unless you put an empty paragraph between
-- them.  See #4315.
separateTables :: [Block] -> [Block]
separateTables [] = []
separateTables (x@Table{}:xs@(Table{}:_)) =
  x : RawBlock (Format "openxml") "<w:p />" : separateTables xs
separateTables (x:xs) = x : separateTables xs

rStyleM :: (PandocMonad m) => CharStyleName -> WS m XML.Element
rStyleM styleName = do
  cStyleMap <- gets (smCharStyle . stStyleMaps)
  let sty' = getStyleIdFromName styleName cStyleMap
  return $ mknode "w:rStyle" [("w:val", fromStyleId sty')] ()

getUniqueId :: (PandocMonad m) => WS m Text
getUniqueId = do
  n <- gets stCurId
  modify $ \st -> st{stCurId = n + 1}
  return $ tshow n

-- | Key for specifying user-defined docx styles.
dynamicStyleKey :: Text
dynamicStyleKey = "custom-style"

-- | Convert a Pandoc block element to OpenXML.
blockToOpenXML :: (PandocMonad m) => WriterOptions -> Block -> WS m [Content]
blockToOpenXML opts blk = withDirection $ blockToOpenXML' opts blk

blockToOpenXML' :: (PandocMonad m) => WriterOptions -> Block -> WS m [Content]
blockToOpenXML' opts (Div (ident,_classes,kvs) bs) = do
  stylemod <- case lookup dynamicStyleKey kvs of
                   Just (fromString . T.unpack -> sty) -> do
                      modify $ \s ->
                        s{stDynamicParaProps = Set.insert sty
                             (stDynamicParaProps s)}
                      return $ withParaPropM (pStyleM sty)
                   _ -> return id
  dirmod <- case lookup "dir" kvs of
                 Just "rtl" -> return $ local (\env -> env { envRTL = True })
                 Just "ltr" -> return $ local (\env -> env { envRTL = False })
                 _ -> return id
  let (hs, bs') = if ident == "refs"
                     then span isHeaderBlock bs
                     else ([], bs)
  let bibmod = if ident == "refs"
                  then withParaPropM (pStyleM "Bibliography")
                  else id
  let langmod = case lookup "lang" kvs of
                  Nothing -> id
                  Just lang -> local (\env -> env{envLang = Just lang})
  header <- dirmod $ stylemod $ blocksToOpenXML opts hs
  contents <- dirmod $ bibmod $ stylemod $ langmod $ blocksToOpenXML opts bs'
  wrapBookmark ident $ header <> contents
blockToOpenXML' opts (Header lev (ident,_,kvs) lst) = do
  setFirstPara
  let isChapter = lev == 1 && writerTopLevelDivision opts == TopLevelChapter
  paraProps <- withParaPropM (pStyleM (fromString $ "Heading "++show lev)) $
                    getParaProps False
  number <-
        if writerNumberSections opts
           then
             case lookup "number" kvs of
                Just n -> do
                   num <- withTextPropM (rStyleM "SectionNumber")
                            (inlineToOpenXML opts (Str n))
                   return $ num ++ [Elem $ mknode "w:r" [] [mknode "w:tab" [] ()]]
                Nothing -> return []
           else return []
  contents <- (number ++) <$> inlinesToOpenXML opts lst
  let addSectionBreak
       | isChapter = (Elem (mknode "w:p" []
                            (mknode "w:pPr" []
                             [mknode "w:sectPr" [] ()])) :)
       | otherwise = id
  addSectionBreak <$>
    if T.null ident
       then return [Elem $ mknode "w:p" [] (map Elem paraProps ++ contents)]
       else do
         let bookmarkName = ident
         modify $ \s -> s{ stSectionIds = Set.insert bookmarkName
                                        $ stSectionIds s }
         bookmarkedContents <- wrapBookmark bookmarkName contents
         return [Elem $ mknode "w:p" [] (map Elem paraProps ++ bookmarkedContents)]
blockToOpenXML' opts (Plain lst) = do
  isInTable <- gets stInTable
  isInList <- gets stInList
  let block = blockToOpenXML opts (Para lst)
  prop <- pStyleM "Compact"
  if isInTable || isInList
     then withParaProp prop block
     else block
blockToOpenXML' opts (Para lst)
  | null lst && not (isEnabled Ext_empty_paragraphs opts) = return []
  | otherwise = do
      isFirstPara <- gets stFirstPara
      let displayMathPara = case lst of
                                 [x] -> isDisplayMath x
                                 _   -> False
      paraProps <- getParaProps displayMathPara
      bodyTextStyle <- pStyleM $ if isFirstPara
                       then "First Paragraph"
                       else "Body Text"
      let paraProps' = case paraProps of
            []               -> [mknode "w:pPr" [] [bodyTextStyle]]
            ps               -> ps
      modify $ \s -> s { stFirstPara = False }
      contents <- inlinesToOpenXML opts lst
      return [Elem $ mknode "w:p" [] (map Elem paraProps' ++ contents)]
blockToOpenXML' opts (LineBlock lns) = blockToOpenXML opts $ linesToPara lns
blockToOpenXML' _ b@(RawBlock format str)
  | format == Format "openxml" = return [
        Text (CData CDataRaw str Nothing)
      ]
  | otherwise                  = do
      report $ BlockNotRendered b
      return []
blockToOpenXML' opts (BlockQuote blocks) = do
  inNote <- asks envInNote
  p <- withParaPropM (pStyleM
                       (if inNote
                           then "Footnote Block Text"
                           else "Block Text"))
       $ blocksToOpenXML opts blocks
  setFirstPara
  return p
blockToOpenXML' opts (CodeBlock attrs@(ident, _, _) str) = do
  p <- withParaPropM (pStyleM "Source Code") (blockToOpenXML opts $ Para [Code attrs str])
  setFirstPara
  wrapBookmark ident p
blockToOpenXML' _ HorizontalRule = do
  setFirstPara
  return [ Elem $
    mknode "w:p" [] $ mknode "w:r" [] $ mknode "w:pict" []
    $ mknode "v:rect" [("style","width:0;height:1.5pt"),
                       ("o:hralign","center"),
                       ("o:hrstd","t"),("o:hr","t")] () ]
blockToOpenXML' opts (Table attr caption colspecs thead tbodies tfoot) = do
  -- Remove extra paragraph indentation due to list items (#5947).
  -- This means that tables in lists will not be indented, but it
  -- avoids unwanted indentation in each cell.
  content <- tableToOpenXML opts
              (local (\env -> env{ envListLevel = -1 }) . blocksToOpenXML opts)
                 (Grid.toTable attr caption colspecs thead tbodies tfoot)
  let (tableId, _, _) = attr
  wrapBookmark tableId content
blockToOpenXML' opts el
  | BulletList lst <- el
    = case mapM toTaskListItem lst of
      Just items -> mconcat <$>
        mapM (\(checked, bs) -> addOpenXMLList (CheckboxMarker checked) [bs]) items
      Nothing -> addOpenXMLList BulletMarker lst
  | OrderedList (start, numstyle, numdelim) lst <- el
    = addOpenXMLList (NumberMarker numstyle numdelim start) lst
  where
    addOpenXMLList marker items = do
      addList marker
      numid <- getNumId
      exampleid <- case marker of
                     NumberMarker Example _ _ -> gets stExampleId
                     _ -> return Nothing
      l <- asList $ concat <$>
             mapM (listItemToOpenXML opts $ fromMaybe numid exampleid) items
      setFirstPara
      return l
blockToOpenXML' opts (DefinitionList items) = do
  l <- concat `fmap` mapM (definitionListItemToOpenXML opts) items
  setFirstPara
  return l
blockToOpenXML' opts (Figure (ident, _, _) (Caption _ longcapt) body) = do
  setFirstPara
  fignum <- gets stNextFigureNum
  unless (null longcapt) $ modify $ \st -> st{ stNextFigureNum = fignum + 1 }
  let refid = if T.null ident
              then "ref_fig" <> tshow fignum
              else "ref_" <> ident
  figname <- translateTerm Term.Figure
  prop <- pStyleM $
    if null longcapt
    then "Figure"
    else "Captioned Figure"
  paraProps <- local
    (\env -> env { envParaProperties = EnvProps (Just prop) [] <>
                                       envParaProperties env })
    (getParaProps False)

  -- Figure contents
  let simpleImage x = do
        imgXML <- inlineToOpenXML opts x
        pure $ Elem (mknode "w:p" [] (map Elem paraProps ++ imgXML))
  contentsNode <- case body of
    [Plain [img@Image {}]] -> simpleImage img
    [Para  [img@Image {}]] -> simpleImage img
    _                      -> toFigureTable opts body
  -- Caption
  let imageCaption = withParaPropM (pStyleM "Image Caption")
                   . blocksToOpenXML opts
  let fstCaptionPara inlns = Para $
        if not $ isEnabled Ext_native_numbering opts
        then inlns
        else let rawfld = RawInline (Format "openxml") $ mconcat
                          [ "<w:fldSimple w:instr=\"SEQ Figure"
                          , " \\* ARABIC \"><w:r><w:t>"
                          , tshow fignum
                          , "</w:t></w:r></w:fldSimple>"
                          ]
             in Span (refid,[],[]) [Str (figname <> "\160") , rawfld]
                : Str ": " : inlns
  captionNode <- case longcapt of
    []              -> return []
    (Para xs  : bs) -> imageCaption (fstCaptionPara xs : bs)
    (Plain xs : bs) -> imageCaption (fstCaptionPara xs : bs)
    _               -> imageCaption longcapt
  wrapBookmark ident $
    case writerFigureCaptionPosition opts of
      CaptionBelow -> contentsNode : captionNode
      CaptionAbove -> captionNode ++ [contentsNode]

toFigureTable :: PandocMonad m
              => WriterOptions -> [Block] -> WS m Content
toFigureTable opts blks = do
  modify $ \s -> s { stInTable = True }
  let ncols = length blks
  let textwidth = 7920  -- 5.5 in in twips       (1 twip == 1/20 pt)
  let cellfrac = 1 / fromIntegral ncols
  let colwidth = tshow @Integer $ floor (textwidth * cellfrac) -- twips
  let gridCols = replicate ncols $ mknode "w:gridCol" [("w:w", colwidth)] ()
  let scaleImage = \case
        Image attr@(ident, classes, attribs) alt tgt ->
          let dimWidth  = case dimension Width attr of
                            Nothing -> Percent (cellfrac * 100)
                            Just d  -> scaleDimension cellfrac d
              dimHeight = scaleDimension cellfrac <$> dimension Height attr
              attribs' = (tshow Width, tshow dimWidth) :
                         (case dimHeight of
                            Nothing -> id
                            Just h  -> ((tshow Height, tshow h) :))
                         [ (k, v) | (k, v) <- attribs
                                  , k `notElem` ["width", "height"]
                                  ]
          in Image (ident, classes, attribs') alt tgt
        x -> x
  let blockToCell = Table.OOXMLCell nullAttr AlignCenter 1 1 . (:[])
                  . walk scaleImage
  tblBody <- Table.rowToOpenXML (blocksToOpenXML opts) .
             Table.OOXMLRow Table.BodyRow nullAttr $
             map blockToCell blks
  let tbl = mknode "w:tbl" []
        ( mknode "w:tblPr" []
          [ mknode "w:tblStyle" [("w:val","FigureTable")] (),
            mknode "w:tblW" [ ("w:type", "auto"), ("w:w", "0") ] (),
            mknode "w:jc" [("w:val","center")] (),
            mknode "w:tblLook" [ ("w:firstRow", "0")
                               , ("w:lastRow", "0")
                               , ("w:firstColumn", "0")
                               , ("w:lastColumn", "0")
                               ] ()
          ]
          : mknode "w:tblGrid" [] gridCols
          : maybeToList tblBody
        )
  modify $ \s -> s { stInTable = False }
  return $ Elem tbl


definitionListItemToOpenXML  :: (PandocMonad m)
                             => WriterOptions -> ([Inline],[[Block]])
                             -> WS m [Content]
definitionListItemToOpenXML opts (term,defs) = do
  term' <- withParaPropM (pStyleM "Definition Term")
           $ blockToOpenXML opts (Para term)
  defs' <- withParaPropM (pStyleM "Definition")
           $ concat `fmap` mapM (blocksToOpenXML opts) defs
  return $ term' ++ defs'

addList :: (PandocMonad m) => ListMarker -> WS m ()
addList marker = do
  lists <- gets stLists
  lastExampleId <- gets stExampleId
  modify $ \st -> st{ stLists = lists ++ case marker of
                                         -- Use only first occurrence of Example for list declaration to avoid overhead
                                         NumberMarker Example _ _ | isJust lastExampleId -> []
                                         _ -> [marker]
                    , stExampleId = case marker of
                                         -- Reuse the same identifier for all other occurrences of Example
                                         NumberMarker Example _ _ -> lastExampleId <|> Just (baseListId + length lists)
                                         _ -> lastExampleId
                  }

listItemToOpenXML :: (PandocMonad m)
                  => WriterOptions
                  -> Int -> [Block]
                  -> WS m [Content]
listItemToOpenXML opts numid bs = do
  oldInList <- gets stInList
  modify $ \st -> st{ stInList = True }
  let isListBlock = \case
        BulletList{}  -> True
        OrderedList{} -> True
        _             -> False
  -- Prepend an empty string if the first entry is another
  -- list. Otherwise the outer bullet will disappear.
  let bs' = case bs of
                 [] -> []
                 x:xs -> if isListBlock x
                               then Plain [Str ""]:x:xs
                               else x:xs
  modify $ \st -> st{ stNumIdUsed = False }
  contents <- withNumId numid $ blocksToOpenXML opts bs'
  modify $ \st -> st{ stInList = oldInList }
  return contents

-- | Convert a list of inline elements to OpenXML.
inlinesToOpenXML :: PandocMonad m => WriterOptions -> [Inline] -> WS m [Content]
inlinesToOpenXML opts lst = concat `fmap` mapM (inlineToOpenXML opts) (convertSpace lst)

withNumId :: (PandocMonad m) => Int -> WS m a -> WS m a
withNumId numid = local $ \env -> env{ envListNumId = numid }

asList :: (PandocMonad m) => WS m a -> WS m a
asList = local $ \env -> env{ envListLevel = envListLevel env + 1 }

getTextProps :: (PandocMonad m) => WS m [Element]
getTextProps = do
  props <- asks envTextProperties
  mblang <- asks envLang
  let langnode = case mblang of
                   Nothing -> mempty
                   Just l  -> EnvProps Nothing
                               [mknode "w:lang" [("w:val", l)] ()]
  let squashed = squashProps (props <> langnode)
  return [mknode "w:rPr" [] squashed | (not . null) squashed]

withTextProp :: PandocMonad m => Element -> WS m a -> WS m a
withTextProp d p =
  local (\env -> env {envTextProperties = ep <> envTextProperties env}) p
  where ep = if isStyle d then EnvProps (Just d) [] else EnvProps Nothing [d]

withTextPropM :: PandocMonad m => WS m Element -> WS m a -> WS m a
withTextPropM md p = do
  d <- md
  withTextProp d p

getParaProps :: PandocMonad m => Bool -> WS m [Element]
getParaProps displayMathPara = do
  props <- asks envParaProperties
  listLevel <- asks envListLevel
  numid <- asks envListNumId
  numIdUsed <- gets stNumIdUsed
  -- clear numId after first use to support multiple paragraphs in the same bullet
  -- baseListId is the code for no list marker
  let numid' = if numIdUsed then baseListId else numid
  modify $ \st -> st{ stNumIdUsed = True }
  let listPr = [mknode "w:numPr" []
                [ mknode "w:ilvl" [("w:val",tshow listLevel)] ()
                , mknode "w:numId" [("w:val",tshow numid')] () ] | listLevel >= 0 && not displayMathPara]
  return $ case squashProps (EnvProps Nothing listPr <> props) of
                [] -> []
                ps -> [mknode "w:pPr" [] ps]

formattedString :: PandocMonad m => Text -> WS m [Element]
formattedString str =
  -- properly handle soft hyphens
  case splitTextBy (=='\173') str of
      [w] -> formattedString' w
      ws  -> do
         sh <- formattedRun [mknode "w:softHyphen" [] ()]
         intercalate [sh] <$> mapM formattedString' ws

formattedString' :: PandocMonad m => Text -> WS m [Element]
formattedString' str = do
  inDel <- asks envInDel
  let mkrun s =
        (if T.any isCJK s
            then withTextProp (mknode "w:rFonts" [("w:hint","eastAsia")] ())
            else id) $ formattedRun
                       [ mktnode (if inDel then "w:delText" else "w:t")
                          [("xml:space","preserve")] $ s ]
  mapM mkrun $ breakIntoChunks $ stripInvalidChars str

-- For motivation see #9817.
breakIntoChunks :: Text -> [Text]
breakIntoChunks t
  | T.null t = []
  | T.any isCJK t
    = let cs = T.groupBy (\c d -> (isSpace c && isSpace d) ||
                                  not (isSpace c || isSpace d)) t
          css = groupBy (\x y -> not (T.any isCJK x || T.any isCJK y)
                                  || (T.all isSpace x && not (T.any isCJK y))
                                  || (T.all isSpace y && not (T.any isCJK x)))
                        cs
       in map mconcat css
  | otherwise = [t]

formattedRun :: PandocMonad m => [Element] -> WS m Element
formattedRun els = do
  props <- getTextProps
  return $ mknode "w:r" [] $ props ++ els

-- | Convert an inline element to OpenXML.
inlineToOpenXML :: PandocMonad m => WriterOptions -> Inline -> WS m [Content]
inlineToOpenXML opts il = withDirection $ inlineToOpenXML' opts il

inlineToOpenXML' :: PandocMonad m => WriterOptions -> Inline -> WS m [Content]
inlineToOpenXML' _ (Str str) =
  map Elem <$> formattedString str
inlineToOpenXML' opts Space = inlineToOpenXML opts (Str " ")
inlineToOpenXML' opts SoftBreak = inlineToOpenXML opts (Str " ")
inlineToOpenXML' opts (Span ("",["mark"],[]) ils) =
  withTextProp (mknode "w:highlight" [("w:val","yellow")] ()) $
    inlinesToOpenXML opts ils
inlineToOpenXML' opts (Span ("",["csl-block"],[]) ils) =
  inlinesToOpenXML opts ils
inlineToOpenXML' opts (Span ("",["csl-left-margin"],[]) ils) =
  inlinesToOpenXML opts ils
inlineToOpenXML' opts (Span ("",["csl-right-inline"],[]) ils) =
   ([Elem $
     mknode "w:r" []
     (mknode "w:t"
       [("xml:space","preserve")]
       ("\t" :: Text))] ++)
    <$> inlinesToOpenXML opts ils
inlineToOpenXML' opts (Span ("",["csl-indent"],[]) ils) =
  inlinesToOpenXML opts ils
inlineToOpenXML' _ (Span (ident,["comment-start"],kvs) ils) = do
  -- prefer the "id" in kvs, since that is the one produced by the docx
  -- reader.
  let ident' = fromMaybe ident (lookup "id" kvs)
      kvs' = filter (("id" /=) . fst) kvs
  modify $ \st -> st{ stComments = (("id",ident'):kvs', ils) : stComments st }
  return [ Elem $ mknode "w:commentRangeStart" [("w:id", ident')] () ]
inlineToOpenXML' _ (Span (ident,["comment-end"],kvs) _) =
  -- prefer the "id" in kvs, since that is the one produced by the docx
  -- reader.
  let ident' = fromMaybe ident (lookup "id" kvs)
  in return . map Elem $
     [ mknode "w:commentRangeEnd" [("w:id", ident')] ()
     , mknode "w:r" []
       [ mknode "w:rPr" []
         [ mknode "w:rStyle" [("w:val", "CommentReference")] () ]
       , mknode "w:commentReference" [("w:id", ident')] () ]
     ]
inlineToOpenXML' opts (Span (ident,classes,kvs) ils) = do
  stylemod <- case lookup dynamicStyleKey kvs of
                   Just (fromString . T.unpack -> sty) -> do
                      modify $ \s ->
                        s{stDynamicTextProps = Set.insert sty
                              (stDynamicTextProps s)}
                      return $ withTextPropM (rStyleM sty)
                   _ -> return id
  let dirmod = case lookup "dir" kvs of
                 Just "rtl" -> local (\env -> env { envRTL = True })
                 Just "ltr" -> local (\env -> env { envRTL = False })
                 _          -> id
      off x = withTextProp (mknode x [("w:val","0")] ())
      pmod =  (if "csl-no-emph" `elem` classes then off "w:i" else id) .
              (if "csl-no-strong" `elem` classes then off "w:b" else id) .
              (if "csl-no-smallcaps" `elem` classes
                  then off "w:smallCaps"
                  else id)
      getChangeAuthorDate = do
        defaultAuthor <- asks envChangesAuthor
        let author = fromMaybe defaultAuthor (lookup "author" kvs)
        let mdate = lookup "date" kvs
        return $ ("w:author", author) :
                   maybe [] (\date -> [("w:date", date)]) mdate
  insmod <- if "insertion" `elem` classes
               then do
                 changeAuthorDate <- getChangeAuthorDate
                 insId <- gets stInsId
                 modify $ \s -> s{stInsId = insId + 1}
                 return $ \f -> do
                   x <- f
                   return [Elem $
                           mknode "w:ins"
                             (("w:id", tshow insId) : changeAuthorDate) x]
               else return id
  delmod <- if "deletion" `elem` classes
               then do
                 changeAuthorDate <- getChangeAuthorDate
                 delId <- gets stDelId
                 modify $ \s -> s{stDelId = delId + 1}
                 return $ \f -> local (\env->env{envInDel=True}) $ do
                   x <- f
                   return [Elem $ mknode "w:del"
                             (("w:id", tshow delId) : changeAuthorDate) x]
               else return id
  let langmod = case lookup "lang" kvs of
                  Nothing -> id
                  Just lang -> local (\env -> env{envLang = Just lang})
  contents <- insmod $ delmod $ dirmod $ stylemod $ pmod $
              langmod $ inlinesToOpenXML opts ils
  wrapBookmark ident contents
inlineToOpenXML' opts (Strong lst) =
  withTextProp (mknode "w:bCs" [] ()) $ -- needed for LTR, #6911
  withTextProp (mknode "w:b" [] ()) $
  inlinesToOpenXML opts lst
inlineToOpenXML' opts (Emph lst) =
  withTextProp (mknode "w:iCs" [] ()) $  -- needed for LTR, #6911
  withTextProp (mknode "w:i" [] ()) $
  inlinesToOpenXML opts lst
inlineToOpenXML' opts (Underline lst) =
  withTextProp (mknode "w:u" [("w:val","single")] ()) $
    inlinesToOpenXML opts lst
inlineToOpenXML' opts (Subscript lst) =
  withTextProp (mknode "w:vertAlign" [("w:val","subscript")] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML' opts (Superscript lst) =
  withTextProp (mknode "w:vertAlign" [("w:val","superscript")] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML' opts (SmallCaps lst) =
  withTextProp (mknode "w:smallCaps" [] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML' opts (Strikeout lst) =
  withTextProp (mknode "w:strike" [] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML' _ LineBreak = return [Elem br]
inlineToOpenXML' _ il@(RawInline f str)
  | f == Format "openxml" = return
                            [Text (CData CDataRaw str Nothing)]
  | otherwise             = do
      report $ InlineNotRendered il
      return []
inlineToOpenXML' opts (Quoted quoteType lst) =
  inlinesToOpenXML opts $ [Str open] ++ lst ++ [Str close]
    where (open, close) = case quoteType of
                            SingleQuote -> ("\x2018", "\x2019")
                            DoubleQuote -> ("\x201C", "\x201D")
inlineToOpenXML' opts (Math mathType str) = do
  when (mathType == DisplayMath) setFirstPara
  res <- (lift . lift) (convertMath writeOMML mathType str)
  case res of
       Right r -> return [Elem $ fromXLElement r]
       Left il -> inlineToOpenXML' opts il
inlineToOpenXML' opts (Cite _ lst) = inlinesToOpenXML opts lst
inlineToOpenXML' opts (Code attrs str) = do
  let alltoktypes = [KeywordTok ..]
  tokTypesMap <- mapM (\tt -> (,) tt <$> rStyleM (fromString $ show tt)) alltoktypes
  let unhighlighted = (map Elem . intercalate [br]) `fmap`
                       mapM formattedString (T.lines str)
      formatOpenXML _fmtOpts = intercalate [br] . map (map toHlTok)
      toHlTok (toktype,tok) =
        mknode "w:r" []
          [ mknode "w:rPr" [] $
            maybeToList (lookup toktype tokTypesMap)
            , mknode "w:t" [("xml:space","preserve")] tok ]
  withTextPropM (rStyleM "Verbatim Char")
    $ if isNothing (writerHighlightStyle opts)
          then unhighlighted
          else case highlight (writerSyntaxMap opts)
                      formatOpenXML attrs str of
                    Right h  -> return (map Elem h)
                    Left msg -> do
                      unless (T.null msg) $ report $ CouldNotHighlight msg
                      unhighlighted
inlineToOpenXML' opts (Note bs) = do
  notes <- gets stFootnotes
  notenum <- getUniqueId
  footnoteStyle <- rStyleM "Footnote Reference"
  let notemarker = mknode "w:r" []
                   [ mknode "w:rPr" [] footnoteStyle
                   , mknode "w:footnoteRef" [] () ]
  let notemarkerXml = RawInline (Format "openxml") $ ppElement notemarker
  let insertNoteRef (Plain ils : xs) = Plain (notemarkerXml : Space : ils) : xs
      insertNoteRef (Para ils  : xs) = Para  (notemarkerXml : Space : ils) : xs
      insertNoteRef xs               = Para [notemarkerXml] : xs

  contents <- local (\env -> env{ envListLevel = -1
                                , envParaProperties = mempty
                                , envTextProperties = mempty
                                , envInNote = True })
              (withParaPropM (pStyleM "Footnote Text") $
               blocksToOpenXML opts $ insertNoteRef bs)
  let newnote = mknode "w:footnote" [("w:id", notenum)] contents
  modify $ \s -> s{ stFootnotes = newnote : notes }
  return [ Elem $ mknode "w:r" []
           [ mknode "w:rPr" [] footnoteStyle
           , mknode "w:footnoteReference" [("w:id", notenum)] () ] ]
-- internal link:
inlineToOpenXML' opts (Link _ txt (T.uncons -> Just ('#', xs),_)) = do
  contents <- withTextPropM (rStyleM "Hyperlink") $ inlinesToOpenXML opts txt
  return
    [ Elem $ mknode "w:hyperlink" [("w:anchor", toBookmarkName xs)] contents ]
-- external link:
inlineToOpenXML' opts (Link _ txt (src,_)) = do
  contents <- withTextPropM (rStyleM "Hyperlink") $ inlinesToOpenXML opts txt
  extlinks <- gets stExternalLinks
  id' <- case M.lookup src extlinks of
            Just i   -> return i
            Nothing  -> do
              i <- ("rId" <>) <$> getUniqueId
              modify $ \st -> st{ stExternalLinks =
                        M.insert src i extlinks }
              return i
  return [ Elem $ mknode "w:hyperlink" [("r:id",id')] contents ]
inlineToOpenXML' opts (Image attr@(imgident, _, _) alt (src, title)) = do
  pageWidth <- asks envPrintWidth
  imgs <- gets stImages
  let
    stImage = M.lookup (T.unpack src) imgs
    generateImgElt (ident, _fp, mt, img) = do
      docprid <- getUniqueId
      nvpicprid <- getUniqueId
      (blipAttrs, blipContents) <-
        case T.takeWhile (/=';') <$> mt of
          Just "image/svg+xml" -> do
            -- get fallback png
            mediabag <- getMediaBag
            mbFallback <-
              case lookupMedia (T.unpack (src <> ".png")) mediabag of
                Just item -> do
                  id' <- T.unpack . ("rId" <>) <$> getUniqueId
                  let fp' = "media/" <> id' <> ".png"
                  let imgdata = (id',
                                 fp',
                                 Just (mediaMimeType item),
                                 BL.toStrict $ mediaContents item)
                  modify $ \st -> st { stImages =
                            M.insert fp' imgdata $ stImages st }
                  return $ Just id'
                Nothing -> return Nothing
            let extLst = mknode "a:extLst" []
                            [ mknode "a:ext"
                              [("uri","{28A0092B-C50C-407E-A947-70E740481C1C}")]
                              [ mknode "a14:useLocalDpi"
                                [("xmlns:a14","http://schemas.microsoft.com/office/drawing/2010/main"),
                                 ("val","0")] () ]
                            , mknode "a:ext"
                              [("uri","{96DAC541-7B7A-43D3-8B79-37D633B846F1}")]
                              [ mknode "asvg:svgBlip"
                                [("xmlns:asvg", "http://schemas.microsoft.com/office/drawing/2016/SVG/main"),
                                 ("r:embed",T.pack ident)] () ]
                            ]
            return (maybe [] (\id'' -> [("r:embed", T.pack id'')]) mbFallback,
                    [extLst])
          _ -> return ([("r:embed", T.pack ident)], [])
      let
        (xpt,ypt) = desiredSizeInPoints opts attr
               (either (const def) id (imageSize opts img))
        -- 12700 emu = 1 pt
        pageWidthPt = case dimension Width attr of
                        Just (Percent a) -> pageWidth * floor (a * 127)
                        _                -> pageWidth * 12700
        (xemu,yemu) = fitToPage (xpt * 12700, ypt * 12700) pageWidthPt
        cNvPicPr = mknode "pic:cNvPicPr" [] $
                         mknode "a:picLocks" [("noChangeArrowheads","1")
                                             ,("noChangeAspect","1")] ()
        nvPicPr  = mknode "pic:nvPicPr" []
                        [ mknode "pic:cNvPr"
                            [("descr",src)
                            ,("id", nvpicprid)
                            ,("name","Picture")] ()
                        , cNvPicPr ]
        blipFill = mknode "pic:blipFill" []
          [ mknode "a:blip" blipAttrs blipContents
          , mknode "a:stretch" [] $
              mknode "a:fillRect" [] ()
          ]
        xfrm =    mknode "a:xfrm" []
                        [ mknode "a:off" [("x","0"),("y","0")] ()
                        , mknode "a:ext" [("cx",tshow xemu)
                                         ,("cy",tshow yemu)] () ]
        prstGeom = mknode "a:prstGeom" [("prst","rect")] $
                         mknode "a:avLst" [] ()
        ln =      mknode "a:ln" [("w","9525")]
                        [ mknode "a:noFill" [] ()
                        , mknode "a:headEnd" [] ()
                        , mknode "a:tailEnd" [] () ]
        spPr =    mknode "pic:spPr" [("bwMode","auto")]
                        [xfrm, prstGeom, mknode "a:noFill" [] (), ln]
        graphic = mknode "a:graphic" [] $
          mknode "a:graphicData"
            [("uri","http://schemas.openxmlformats.org/drawingml/2006/picture")]
            [ mknode "pic:pic" []
              [ nvPicPr
              , blipFill
              , spPr
              ]
            ]
        imgElt = mknode "w:r" [] $
          mknode "w:drawing" [] $
            mknode "wp:inline" []
              [ mknode "wp:extent" [("cx",tshow xemu),("cy",tshow yemu)] ()
              , mknode "wp:effectExtent"
                [("b","0"),("l","0"),("r","0"),("t","0")] ()
              , mknode "wp:docPr"
                [ ("descr", stringify alt)
                , ("title", title)
                , ("id", docprid)
                , ("name","Picture")
                ] ()
              , graphic
              ]
      return [Elem imgElt]

  wrapBookmark imgident =<< case stImage of
    Just imgData -> generateImgElt imgData
    Nothing -> ( do --try
      (img, mt) <- P.fetchItem src
      ident <- ("rId" <>) <$> getUniqueId

      let
        imgext = case mt >>= extensionFromMimeType of
          Just x    -> "." <> x
          Nothing   -> case imageType img of
            Just Png  -> ".png"
            Just Jpeg -> ".jpeg"
            Just Gif  -> ".gif"
            Just Pdf  -> ".pdf"
            Just Eps  -> ".eps"
            Just Svg  -> ".svg"
            Just Emf  -> ".emf"
            Just Tiff -> ".tiff"
            Just Webp -> ".webp"
            Nothing   -> ""
        imgpath = "media/" <> ident <> imgext
        mbMimeType = mt <|> getMimeType (T.unpack imgpath)

        imgData = (T.unpack ident, T.unpack imgpath, mbMimeType, img)

      if T.null imgext
         then -- without an extension there is no rule for content type
           inlinesToOpenXML opts alt -- return alt to avoid corrupted docx
         else do
           -- insert mime type to use in constructing [Content_Types].xml
           modify $ \st -> st { stImages = M.insert (T.unpack src) imgData $ stImages st }
           generateImgElt imgData
      )
      `catchError` ( \e -> do
        report $ CouldNotFetchResource src $ T.pack (show e)
        -- emit alt text
        inlinesToOpenXML opts alt
      )

br :: Element
br = mknode "w:r" [] [mknode "w:br" [] ()]


withDirection :: PandocMonad m => WS m a -> WS m a
withDirection x = do
  isRTL <- asks envRTL
  paraProps <- asks envParaProperties
  textProps <- asks envTextProperties
  -- We want to clean all bidirection (bidi) and right-to-left (rtl)
  -- properties from the props first. This is because we don't want
  -- them to stack up.
  let paraProps' = filter (\e -> (qName . elName) e /= "bidi") (otherElements paraProps)
      textProps' = filter (\e -> (qName . elName) e /= "rtl") (otherElements textProps)
      paraStyle = styleElement paraProps
      textStyle = styleElement textProps
  if isRTL
    -- if we are going right-to-left, we (re?)add the properties.
    then flip local x $
         \env -> env { envParaProperties = EnvProps paraStyle $ mknode "w:bidi" [] () : paraProps'
                     , envTextProperties = EnvProps textStyle $ mknode "w:rtl" [] () : textProps'
                     }
    else flip local x $ \env -> env { envParaProperties = EnvProps paraStyle paraProps'
                                    , envTextProperties = EnvProps textStyle textProps'
                                    }

wrapBookmark :: (PandocMonad m) => Text -> [Content] -> WS m [Content]
wrapBookmark "" contents = return contents
wrapBookmark ident contents = do
  id' <- getUniqueId
  let bookmarkStart = mknode "w:bookmarkStart"
                       [("w:id", id')
                       ,("w:name", toBookmarkName ident)] ()
      bookmarkEnd = mknode "w:bookmarkEnd" [("w:id", id')] ()
  return $ Elem bookmarkStart : contents ++ [Elem bookmarkEnd]

-- Word imposes a 40 character limit on bookmark names and requires
-- that they begin with a letter.  So we just use a hash of the
-- identifier when otherwise we'd have an illegal bookmark name.
toBookmarkName :: Text -> Text
toBookmarkName s
  | Just (c, _) <- T.uncons s
  , isLetter c
  , T.length s <= 40 = s
  | otherwise = T.pack $ 'X' : drop 1 (show (hashWith SHA1 (fromText s)))

maxListLevel :: Int
maxListLevel = 8

convertSpace :: [Inline] -> [Inline]
convertSpace (Str x : Space : Str y : xs) = convertSpace (Str (x <> " " <> y) : xs)
convertSpace (Str x : Str y : xs)         = convertSpace (Str (x <> y) : xs)
convertSpace (x:xs)                       = x : convertSpace xs
convertSpace []                           = []
