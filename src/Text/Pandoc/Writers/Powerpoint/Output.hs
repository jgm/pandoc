{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{- |
   Module      : Text.Pandoc.Writers.Powerpoint.Output
   Copyright   : Copyright (C) 2017-2020 Jesse Rosenthal
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Conversion of Presentation datatype (defined in
Text.Pandoc.Writers.Powerpoint.Presentation) to a zip archive.
-}

module Text.Pandoc.Writers.Powerpoint.Output ( presentationToArchive
                                             ) where

import Control.Monad.Except (throwError, catchError)
import Control.Monad.Reader
import Control.Monad.State
import Codec.Archive.Zip
import Data.List (intercalate, stripPrefix, nub, union, isPrefixOf, intersperse)
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read
import Data.Time (formatTime, defaultTimeLocale)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import System.FilePath.Posix (splitDirectories, splitExtension, takeExtension)
import Text.Pandoc.XML.Light as XML
import Text.Pandoc.Definition
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Error (PandocError(..))
import qualified Text.Pandoc.Class.PandocMonad as P
import Text.Pandoc.Options
import Text.Pandoc.MIME
import qualified Data.ByteString.Lazy as BL
import Text.Pandoc.Writers.Shared (metaToContext)
import Text.Pandoc.Writers.OOXML
import qualified Data.Map as M
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe, maybeToList, catMaybes, isJust)
import Text.Pandoc.ImageSize
import Control.Applicative ((<|>))
import System.FilePath.Glob
import Text.DocTemplates (FromContext(lookupContext), Context)
import Text.DocLayout (literal)
import Text.TeXMath
import Text.Pandoc.Writers.Math (convertMath)
import Text.Pandoc.Writers.Powerpoint.Presentation
import Text.Pandoc.Shared (tshow, stringify)
import Skylighting (fromColor)
import Data.List.NonEmpty (nonEmpty)

-- |The 'EMU' type is used to specify sizes in English Metric Units.
type EMU = Integer

-- |The 'pixelsToEmu' function converts a size in pixels to one
-- in English Metric Units. It assumes a DPI of 72.
pixelsToEmu :: Pixels -> EMU
pixelsToEmu = (12700 *)

-- This populates the global ids map with images already in the
-- template, so the ids won't be used by images introduced by the
-- user.
initialGlobalIds :: Archive -> Archive -> M.Map FilePath Int
initialGlobalIds refArchive distArchive =
  let archiveFiles = filesInArchive refArchive `union` filesInArchive distArchive
      mediaPaths = filter (isPrefixOf "ppt/media/image") archiveFiles

      go :: FilePath -> Maybe (FilePath, Int)
      go fp = do
        s <- stripPrefix "ppt/media/image" $ fst $ splitExtension fp
        (n, _) <- listToMaybe $ reads s
        return (fp, n)
  in
    M.fromList $ mapMaybe go mediaPaths

getPresentationSize :: Archive -> Archive -> Maybe (Integer, Integer)
getPresentationSize refArchive distArchive = do
  entry <- findEntryByPath "ppt/presentation.xml" refArchive  `mplus`
           findEntryByPath "ppt/presentation.xml" distArchive
  presElement <- either (const Nothing) return $
                   parseXMLElement $ UTF8.toTextLazy $ fromEntry entry
  let ns = elemToNameSpaces presElement
  sldSize <- findChild (elemName ns "p" "sldSz") presElement
  cxS <- findAttr (QName "cx" Nothing Nothing) sldSize
  cyS <- findAttr (QName "cy" Nothing Nothing) sldSize
  cx <- readTextAsInteger cxS
  cy <- readTextAsInteger cyS
  return (cx `div` 12700, cy `div` 12700)

readTextAsInteger :: Text -> Maybe Integer
readTextAsInteger = either (const Nothing) (Just . fst) . Data.Text.Read.decimal

data WriterEnv = WriterEnv { envRefArchive :: Archive
                           , envDistArchive :: Archive
                           , envUTCTime :: UTCTime
                           , envOpts :: WriterOptions
                           , envContext :: Context Text
                           , envPresentationSize :: (Integer, Integer)
                           , envSlideHasHeader :: Bool
                           , envInList :: Bool
                           , envInNoteSlide :: Bool
                           , envCurSlideId :: Int
                           -- the difference between the number at
                           -- the end of the slide file name and
                           -- the rId number
                           , envSlideIdOffset :: Int
                           , envContentType :: ContentType
                           , envSlideIdMap :: M.Map SlideId Int
                           -- maps the slide number to the
                           -- corresponding notes id number. If there
                           -- are no notes for a slide, there will be
                           -- no entry in the map for it.
                           , envSpeakerNotesIdMap :: M.Map Int Int
                           , envInSpeakerNotes :: Bool
                           }
                 deriving (Show)

instance Default WriterEnv where
  def = WriterEnv { envRefArchive = emptyArchive
                  , envDistArchive = emptyArchive
                  , envUTCTime = posixSecondsToUTCTime 0
                  , envOpts = def
                  , envContext = mempty
                  , envPresentationSize = (720, 540)
                  , envSlideHasHeader = False
                  , envInList = False
                  , envInNoteSlide = False
                  , envCurSlideId = 1
                  , envSlideIdOffset = 1
                  , envContentType = NormalContent
                  , envSlideIdMap = mempty
                  , envSpeakerNotesIdMap = mempty
                  , envInSpeakerNotes = False
                  }

data ContentType = NormalContent
                 | TwoColumnLeftContent
                 | TwoColumnRightContent
                 deriving (Show, Eq)

data MediaInfo = MediaInfo { mInfoFilePath :: FilePath
                           , mInfoLocalId  :: Int
                           , mInfoGlobalId :: Int
                           , mInfoMimeType :: Maybe MimeType
                           , mInfoExt      :: Maybe T.Text
                           , mInfoCaption  :: Bool
                           } deriving (Show, Eq)

data WriterState = WriterState { stLinkIds :: M.Map Int (M.Map Int LinkTarget)
                               -- (FP, Local ID, Global ID, Maybe Mime)
                               , stMediaIds :: M.Map Int [MediaInfo]
                               , stMediaGlobalIds :: M.Map FilePath Int
                               } deriving (Show, Eq)

instance Default WriterState where
  def = WriterState { stLinkIds = mempty
                    , stMediaIds = mempty
                    , stMediaGlobalIds = mempty
                    }

type P m = ReaderT WriterEnv (StateT WriterState m)

runP :: Monad m => WriterEnv -> WriterState -> P m a -> m a
runP env st p = evalStateT (runReaderT p env) st

--------------------------------------------------------------------

monospaceFont :: Monad m => P m T.Text
monospaceFont = do
  vars <- asks envContext
  case lookupContext "monofont" vars of
    Just s -> return s
    Nothing -> return "Courier"

fontSizeAttributes :: Monad m => RunProps -> P m [(Text, Text)]
fontSizeAttributes RunProps { rPropForceSize = Just sz } =
  return [("sz", tshow $ sz * 100)]
fontSizeAttributes _ = return []

copyFileToArchive :: PandocMonad m => Archive -> FilePath -> P m Archive
copyFileToArchive arch fp = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  case findEntryByPath fp refArchive `mplus` findEntryByPath fp distArchive of
    Nothing -> throwError $ PandocSomeError
                          $ T.pack
                          $ fp <> " missing in reference file"
    Just e -> return $ addEntryToArchive e arch

alwaysInheritedPatterns :: [Pattern]
alwaysInheritedPatterns =
  map compile [ "docProps/app.xml"
              , "ppt/slideLayouts/slideLayout*.xml"
              , "ppt/slideLayouts/_rels/slideLayout*.xml.rels"
              , "ppt/slideMasters/slideMaster1.xml"
              , "ppt/slideMasters/_rels/slideMaster1.xml.rels"
              , "ppt/theme/theme1.xml"
              , "ppt/theme/_rels/theme1.xml.rels"
              , "ppt/presProps.xml"
              , "ppt/tableStyles.xml"
              , "ppt/media/image*"
              ]

-- We only look for these under special conditions
contingentInheritedPatterns :: Presentation -> [Pattern]
contingentInheritedPatterns pres = [] <>
  if presHasSpeakerNotes pres
  then map compile [ "ppt/notesMasters/notesMaster*.xml"
                   , "ppt/notesMasters/_rels/notesMaster*.xml.rels"
                   , "ppt/theme/theme2.xml"
                   , "ppt/theme/_rels/theme2.xml.rels"
                   ]
  else []

inheritedPatterns :: Presentation -> [Pattern]
inheritedPatterns pres =
  alwaysInheritedPatterns <> contingentInheritedPatterns pres

patternToFilePaths :: PandocMonad m => Pattern -> P m [FilePath]
patternToFilePaths pat = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive

  let archiveFiles = filesInArchive refArchive `union` filesInArchive distArchive
  return $ filter (match pat) archiveFiles

patternsToFilePaths :: PandocMonad m => [Pattern] -> P m [FilePath]
patternsToFilePaths pats = concat <$> mapM patternToFilePaths pats

-- Here are the files we'll require to make a Powerpoint document. If
-- any of these are missing, we should error out of our build.
requiredFiles :: [FilePath]
requiredFiles = [ "docProps/app.xml"
                , "ppt/presProps.xml"
                , "ppt/slideLayouts/slideLayout1.xml"
                , "ppt/slideLayouts/_rels/slideLayout1.xml.rels"
                , "ppt/slideLayouts/slideLayout2.xml"
                , "ppt/slideLayouts/_rels/slideLayout2.xml.rels"
                , "ppt/slideLayouts/slideLayout3.xml"
                , "ppt/slideLayouts/_rels/slideLayout3.xml.rels"
                , "ppt/slideLayouts/slideLayout4.xml"
                , "ppt/slideLayouts/_rels/slideLayout4.xml.rels"
                , "ppt/slideMasters/slideMaster1.xml"
                , "ppt/slideMasters/_rels/slideMaster1.xml.rels"
                , "ppt/theme/theme1.xml"
                , "ppt/tableStyles.xml"
                ]

presentationToArchiveP :: PandocMonad m => Presentation -> P m Archive
presentationToArchiveP p@(Presentation docProps slides) = do
  filePaths <- patternsToFilePaths $ inheritedPatterns p

  -- make sure all required files are available:
  let missingFiles = filter (`notElem` filePaths) requiredFiles
  unless (null missingFiles)
    (throwError $
      PandocSomeError $
      "The following required files are missing:\n" <>
      T.unlines (map (T.pack . ("  " <>)) missingFiles)
    )

  newArch' <- foldM copyFileToArchive emptyArchive filePaths
  -- we make a modified ppt/viewProps.xml out of the presentation viewProps
  viewPropsEntry <- makeViewPropsEntry
  -- we make a docProps/core.xml entry out of the presentation docprops
  docPropsEntry <- docPropsToEntry docProps
  -- we make a docProps/custom.xml entry out of the custom properties
  docCustomPropsEntry <- docCustomPropsToEntry docProps
  -- we make this ourself in case there's something unexpected in the
  -- one in the reference doc.
  relsEntry <- topLevelRelsEntry
  -- presentation entry and rels. We have to do the rels first to make
  -- sure we know the correct offset for the rIds.
  presEntry <- presentationToPresEntry p
  presRelsEntry <- presentationToRelsEntry p
  slideEntries <- mapM slideToEntry slides
  slideRelEntries <- mapM slideToSlideRelEntry slides
  spkNotesEntries <- catMaybes <$> mapM slideToSpeakerNotesEntry slides
  spkNotesRelEntries <- catMaybes <$> mapM slideToSpeakerNotesRelEntry slides
  -- These have to come after everything, because they need the info
  -- built up in the state.
  mediaEntries <- makeMediaEntries
  contentTypesEntry <- presentationToContentTypes p >>= contentTypesToEntry
  -- fold everything into our inherited archive and return it.
  return $ foldr addEntryToArchive newArch' $
    slideEntries <>
    slideRelEntries <>
    spkNotesEntries <>
    spkNotesRelEntries <>
    mediaEntries <>
    [contentTypesEntry, docPropsEntry, docCustomPropsEntry, relsEntry,
     presEntry, presRelsEntry, viewPropsEntry]

makeSlideIdMap :: Presentation -> M.Map SlideId Int
makeSlideIdMap (Presentation _ slides) =
  M.fromList $ map slideId slides `zip` [1..]

makeSpeakerNotesMap :: Presentation -> M.Map Int Int
makeSpeakerNotesMap (Presentation _ slides) =
  M.fromList $
    mapMaybe f (slides `zip` [1..]) `zip` [1..]
  where f (Slide _ _ notes, n) = if notes == mempty
                                 then Nothing
                                 else Just n

presentationToArchive :: PandocMonad m
                      => WriterOptions -> Meta -> Presentation -> m Archive
presentationToArchive opts meta pres = do
  distArchive <- toArchive . BL.fromStrict <$>
                      P.readDefaultDataFile "reference.pptx"
  refArchive <- case writerReferenceDoc opts of
                     Just f  -> toArchive <$> P.readFileLazy f
                     Nothing -> toArchive . BL.fromStrict <$>
                        P.readDataFile "reference.pptx"

  utctime <- P.getTimestamp

  presSize <- case getPresentationSize refArchive distArchive of
                Just sz -> return sz
                Nothing -> throwError $
                           PandocSomeError
                           "Could not determine presentation size"

  -- note, we need writerTemplate to be Just _ or metaToContext does
  -- nothing
  context <- metaToContext opts{ writerTemplate =
                                  writerTemplate opts <|> Just mempty }
                (return . literal . stringify)
                (return . literal . stringify) meta

  let env = def { envRefArchive = refArchive
                , envDistArchive = distArchive
                , envUTCTime = utctime
                , envOpts = opts
                , envContext = context
                , envPresentationSize = presSize
                , envSlideIdMap = makeSlideIdMap pres
                , envSpeakerNotesIdMap = makeSpeakerNotesMap pres
                }

  let st = def { stMediaGlobalIds = initialGlobalIds refArchive distArchive
               }

  runP env st $ presentationToArchiveP pres



--------------------------------------------------

-- Check to see if the presentation has speaker notes. This will
-- influence whether we import the notesMaster template.
presHasSpeakerNotes :: Presentation -> Bool
presHasSpeakerNotes (Presentation _ slides) =
  not $ all ((mempty ==) . slideSpeakerNotes) slides

curSlideHasSpeakerNotes :: PandocMonad m => P m Bool
curSlideHasSpeakerNotes =
  M.member <$> asks envCurSlideId <*> asks envSpeakerNotesIdMap

--------------------------------------------------

getLayout :: PandocMonad m => Layout -> P m Element
getLayout layout = do
  let layoutpath = case layout of
        MetadataSlide{}  -> "ppt/slideLayouts/slideLayout1.xml"
        TitleSlide{}     -> "ppt/slideLayouts/slideLayout3.xml"
        ContentSlide{}   -> "ppt/slideLayouts/slideLayout2.xml"
        TwoColumnSlide{} -> "ppt/slideLayouts/slideLayout4.xml"
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  parseXml refArchive distArchive layoutpath

shapeHasId :: NameSpaces -> T.Text -> Element -> Bool
shapeHasId ns ident element
  | Just nvSpPr <- findChild (elemName ns "p" "nvSpPr") element
  , Just cNvPr <- findChild (elemName ns "p" "cNvPr") nvSpPr
  , Just nm <- findAttr (QName "id" Nothing Nothing) cNvPr =
      nm == ident
  | otherwise = False

getContentShape :: PandocMonad m => NameSpaces -> Element -> P m Element
getContentShape ns spTreeElem
  | isElem ns "p" "spTree" spTreeElem = do
      contentType <- asks envContentType
      let contentShapes = getShapesByPlaceHolderType ns spTreeElem ObjType
      case contentType of
        NormalContent | (sp : _) <- contentShapes -> return sp
        TwoColumnLeftContent | (sp : _) <- contentShapes -> return sp
        TwoColumnRightContent | (_ : sp : _) <- contentShapes -> return sp
        _ -> throwError $ PandocSomeError
             "Could not find shape for Powerpoint content"
getContentShape _ _ = throwError $ PandocSomeError
                      "Attempted to find content on non shapeTree"

getShapeDimensions :: NameSpaces
                   -> Element
                   -> Maybe ((Integer, Integer), (Integer, Integer))
getShapeDimensions ns element
  | isElem ns "p" "sp" element = do
      spPr <- findChild (elemName ns "p" "spPr") element
      xfrm <- findChild (elemName ns "a" "xfrm") spPr
      off <- findChild (elemName ns "a" "off") xfrm
      xS <- findAttr (QName "x" Nothing Nothing) off
      yS <- findAttr (QName "y" Nothing Nothing) off
      ext <- findChild (elemName ns "a" "ext") xfrm
      cxS <- findAttr (QName "cx" Nothing Nothing) ext
      cyS <- findAttr (QName "cy" Nothing Nothing) ext
      x <- readTextAsInteger xS
      y <- readTextAsInteger yS
      cx <- readTextAsInteger cxS
      cy <- readTextAsInteger cyS
      return ((x `div` 12700, y `div` 12700),
              (cx `div` 12700, cy `div` 12700))
  | otherwise = Nothing


getMasterShapeDimensionsById :: T.Text
                             -> Element
                             -> Maybe ((Integer, Integer), (Integer, Integer))
getMasterShapeDimensionsById ident master = do
  let ns = elemToNameSpaces master
  cSld <- findChild (elemName ns "p" "cSld") master
  spTree <- findChild (elemName ns "p" "spTree") cSld
  sp <- filterChild (\e -> isElem ns "p" "sp" e && shapeHasId ns ident e) spTree
  getShapeDimensions ns sp

getContentShapeSize :: PandocMonad m
                    => NameSpaces
                    -> Element
                    -> Element
                    -> P m ((Integer, Integer), (Integer, Integer))
getContentShapeSize ns layout master
  | isElem ns "p" "sldLayout" layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld = do
      sp  <- getContentShape ns spTree
      case getShapeDimensions ns sp of
        Just sz -> return sz
        Nothing -> do let mbSz =
                            findChild (elemName ns "p" "nvSpPr") sp >>=
                            findChild (elemName ns "p" "cNvPr") >>=
                            findAttr (QName "id" Nothing Nothing) >>=
                            flip getMasterShapeDimensionsById master
                      case mbSz of
                        Just sz' -> return sz'
                        Nothing -> throwError $ PandocSomeError
                                   "Couldn't find necessary content shape size"
getContentShapeSize _ _ _ = throwError $ PandocSomeError
                            "Attempted to find content shape size in non-layout"

buildSpTree :: NameSpaces -> Element -> [Content] -> Element
buildSpTree ns spTreeElem newShapes =
  emptySpTreeElem { elContent = newContent }
  where newContent = elContent emptySpTreeElem <> newShapes
        emptySpTreeElem = spTreeElem { elContent = filter fn (elContent spTreeElem) }
        fn :: Content -> Bool
        fn (Elem e) = isElem ns "p" "nvGrpSpPr" e ||
                      isElem ns "p" "grpSpPr" e
        fn _        = True

replaceNamedChildren :: NameSpaces
                     -> Text
                     -> Text
                     -> [Element]
                     -> Element
                     -> Element
replaceNamedChildren ns prefix name newKids element =
  element { elContent = concat $ fun True $ elContent element }
  where
    fun :: Bool -> [Content] -> [[Content]]
    fun _ [] = []
    fun switch (Elem e : conts) | isElem ns prefix name e =
                                      if switch
                                      then map Elem newKids : fun False conts
                                      else fun False conts
    fun switch (cont : conts) = [cont] : fun switch conts

----------------------------------------------------------------

registerLink :: PandocMonad m => LinkTarget -> P m Int
registerLink link = do
  curSlideId <- asks envCurSlideId
  linkReg <- gets stLinkIds
  mediaReg <- gets stMediaIds
  hasSpeakerNotes <- curSlideHasSpeakerNotes
  let maxLinkId = case M.lookup curSlideId linkReg >>= nonEmpty . M.keys of
        Just xs -> maximum xs
        Nothing
          | hasSpeakerNotes -> 2
          | otherwise       -> 1
      maxMediaId = case M.lookup curSlideId mediaReg >>= nonEmpty of
        Just mInfos -> maximum $ fmap mInfoLocalId mInfos
        Nothing
          | hasSpeakerNotes -> 2
          | otherwise       -> 1
      maxId = max maxLinkId maxMediaId
      slideLinks = case M.lookup curSlideId linkReg of
        Just mp -> M.insert (maxId + 1) link mp
        Nothing -> M.singleton (maxId + 1) link
  modify $ \st -> st{ stLinkIds = M.insert curSlideId slideLinks linkReg}
  return $ maxId + 1

registerMedia :: PandocMonad m => FilePath -> [ParaElem] -> P m MediaInfo
registerMedia fp caption = do
  curSlideId <- asks envCurSlideId
  linkReg <- gets stLinkIds
  mediaReg <- gets stMediaIds
  globalIds <- gets stMediaGlobalIds
  hasSpeakerNotes <- curSlideHasSpeakerNotes
  let maxLinkId = case M.lookup curSlideId linkReg >>= nonEmpty . M.keys of
          Just ks -> maximum ks
          Nothing
            | hasSpeakerNotes -> 2
            | otherwise       -> 1
      maxMediaId = case M.lookup curSlideId mediaReg >>= nonEmpty of
          Just mInfos -> maximum $ fmap mInfoLocalId mInfos
          Nothing
            | hasSpeakerNotes -> 2
            | otherwise       -> 1
      maxLocalId = max maxLinkId maxMediaId

      maxGlobalId = maybe 0 maximum $ nonEmpty $ M.elems globalIds

  (imgBytes, mbMt) <- P.fetchItem $ T.pack fp
  let imgExt = (mbMt >>= extensionFromMimeType >>= (\x -> return $ "." <> x))
               <|>
               case imageType imgBytes of
                 Just Png  -> Just ".png"
                 Just Jpeg -> Just ".jpeg"
                 Just Gif  -> Just ".gif"
                 Just Pdf  -> Just ".pdf"
                 Just Eps  -> Just ".eps"
                 Just Svg  -> Just ".svg"
                 Just Emf  -> Just ".emf"
                 Just Tiff -> Just ".tiff"
                 Nothing   -> Nothing

  let newGlobalId = fromMaybe (maxGlobalId + 1) (M.lookup fp globalIds)

  let newGlobalIds = M.insert fp newGlobalId globalIds

  let mediaInfo = MediaInfo { mInfoFilePath = fp
                            , mInfoLocalId = maxLocalId + 1
                            , mInfoGlobalId = newGlobalId
                            , mInfoMimeType = mbMt
                            , mInfoExt = imgExt
                            , mInfoCaption = (not . null) caption
                            }

  let slideMediaInfos = case M.lookup curSlideId mediaReg of
        Just minfos -> mediaInfo : minfos
        Nothing     -> [mediaInfo]


  modify $ \st -> st{ stMediaIds = M.insert curSlideId slideMediaInfos mediaReg
                    , stMediaGlobalIds = newGlobalIds
                    }
  return mediaInfo

makeMediaEntry :: PandocMonad m => MediaInfo -> P m Entry
makeMediaEntry mInfo = do
  epochtime <- floor . utcTimeToPOSIXSeconds <$> asks envUTCTime
  (imgBytes, _) <- P.fetchItem (T.pack $ mInfoFilePath mInfo)
  let ext = fromMaybe "" (mInfoExt mInfo)
  let fp = "ppt/media/image" <>
          show (mInfoGlobalId mInfo) <> T.unpack ext
  return $ toEntry fp epochtime $ BL.fromStrict imgBytes

makeMediaEntries :: PandocMonad m => P m [Entry]
makeMediaEntries = do
  mediaInfos <- gets stMediaIds
  let allInfos = mconcat $ M.elems mediaInfos
  mapM makeMediaEntry allInfos

-- -- | Scales the image to fit the page
-- -- sizes are passed in emu
-- fitToPage' :: (Double, Double)  -- image size in emu
--            -> Integer           -- pageWidth
--            -> Integer           -- pageHeight
--            -> (Integer, Integer) -- imagesize
-- fitToPage' (x, y) pageWidth pageHeight
--   -- Fixes width to the page width and scales the height
--   | x <= fromIntegral pageWidth && y <= fromIntegral pageHeight =
--       (floor x, floor y)
--   | x / fromIntegral pageWidth > y / fromIntegral pageWidth =
--       (pageWidth, floor $ ((fromIntegral pageWidth) / x) * y)
--   | otherwise =
--       (floor $ ((fromIntegral pageHeight) / y) * x, pageHeight)

-- positionImage :: (Double, Double) -> Integer -> Integer -> (Integer, Integer)
-- positionImage (x, y) pageWidth pageHeight =
--   let (x', y') = fitToPage' (x, y) pageWidth pageHeight
--   in
--     ((pageWidth - x') `div` 2, (pageHeight - y') `div`  2)

getMaster :: PandocMonad m => P m Element
getMaster = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  parseXml refArchive distArchive "ppt/slideMasters/slideMaster1.xml"

-- We want to get the header dimensions, so we can make sure that the
-- image goes underneath it. We only use this in a content slide if it
-- has a header.

-- getHeaderSize :: PandocMonad m => P m ((Integer, Integer), (Integer, Integer))
-- getHeaderSize = do
--   master <- getMaster
--   let ns = elemToNameSpaces master
--       sps = [master] >>=
--             findChildren (elemName ns "p" "cSld") >>=
--             findChildren (elemName ns "p" "spTree") >>=
--             findChildren (elemName ns "p" "sp")
--       mbXfrm =
--         listToMaybe (filter (shapeHasName ns "Title Placeholder 1") sps) >>=
--         findChild (elemName ns "p" "spPr") >>=
--         findChild (elemName ns "a" "xfrm")
--       xoff = mbXfrm >>=
--              findChild (elemName ns "a" "off") >>=
--              findAttr (QName "x" Nothing Nothing) >>=
--              (listToMaybe . (\s -> reads s :: [(Integer, String)]))
--       yoff = mbXfrm >>=
--              findChild (elemName ns "a" "off") >>=
--              findAttr (QName "y" Nothing Nothing) >>=
--              (listToMaybe . (\s -> reads s :: [(Integer, String)]))
--       xext = mbXfrm >>=
--              findChild (elemName ns "a" "ext") >>=
--              findAttr (QName "cx" Nothing Nothing) >>=
--              (listToMaybe . (\s -> reads s :: [(Integer, String)]))
--       yext = mbXfrm >>=
--              findChild (elemName ns "a" "ext") >>=
--              findAttr (QName "cy" Nothing Nothing) >>=
--              (listToMaybe . (\s -> reads s :: [(Integer, String)]))
--       off = case xoff of
--               Just (xoff', _) | Just (yoff',_) <- yoff -> (xoff', yoff')
--               _                               -> (1043490, 1027664)
--       ext = case xext of
--               Just (xext', _) | Just (yext',_) <- yext -> (xext', yext')
--               _                               -> (7024744, 1143000)
--   return $ (off, ext)

-- Hard-coded for now
-- captionPosition :: ((Integer, Integer), (Integer, Integer))
-- captionPosition = ((457200, 6061972), (8229600, 527087))

captionHeight :: Integer
captionHeight = 40

createCaption :: PandocMonad m
              => ((Integer, Integer), (Integer, Integer))
              -> [ParaElem]
              -> P m Element
createCaption contentShapeDimensions paraElements = do
  let para = Paragraph def{pPropAlign = Just AlgnCenter} paraElements
  elements <- mapM paragraphToElement [para]
  let ((x, y), (cx, cy)) = contentShapeDimensions
  let txBody = mknode "p:txBody" [] $
               [mknode "a:bodyPr" [] (), mknode "a:lstStyle" [] ()] <> elements
  return $
    mknode "p:sp" [] [ mknode "p:nvSpPr" []
                       [ mknode "p:cNvPr" [("id","1"), ("name","TextBox 3")] ()
                       , mknode "p:cNvSpPr" [("txBox", "1")] ()
                       , mknode "p:nvPr" [] ()
                       ]
                     , mknode "p:spPr" []
                       [ mknode "a:xfrm" []
                         [ mknode "a:off" [("x", tshow $ 12700 * x),
                                           ("y", tshow $ 12700 * (y + cy - captionHeight))] ()
                         , mknode "a:ext" [("cx", tshow $ 12700 * cx),
                                           ("cy", tshow $ 12700 * captionHeight)] ()
                         ]
                       , mknode "a:prstGeom" [("prst", "rect")]
                         [ mknode "a:avLst" [] ()
                         ]
                       , mknode "a:noFill" [] ()
                       ]
                     , txBody
                     ]

makePicElements :: PandocMonad m
                => Element
                -> PicProps
                -> MediaInfo
                -> [ParaElem]
                -> P m [Element]
makePicElements layout picProps mInfo alt = do
  opts <- asks envOpts
  (pageWidth, pageHeight) <- asks envPresentationSize
  -- hasHeader <- asks envSlideHasHeader
  let hasCaption = mInfoCaption mInfo
  (imgBytes, _) <- P.fetchItem (T.pack $ mInfoFilePath mInfo)
  let (pxX, pxY) = case imageSize opts imgBytes of
        Right sz -> sizeInPixels sz
        Left _   -> sizeInPixels def
  master <- getMaster
  let ns = elemToNameSpaces layout
  ((x, y), (cx, cytmp)) <- getContentShapeSize ns layout master
                           `catchError`
                           (\_ -> return ((0, 0), (pageWidth, pageHeight)))

  let cy = if hasCaption then cytmp - captionHeight else cytmp

  let imgRatio = fromIntegral pxX / fromIntegral pxY :: Double
      boxRatio = fromIntegral cx / fromIntegral cy :: Double
      (dimX, dimY) = if imgRatio > boxRatio
                     then (fromIntegral cx, fromIntegral cx / imgRatio)
                     else (fromIntegral cy * imgRatio, fromIntegral cy)

      (dimX', dimY') = (round dimX * 12700, round dimY * 12700) :: (Integer, Integer)
      (xoff, yoff) = (fromIntegral x + (fromIntegral cx - dimX) / 2,
                      fromIntegral y + (fromIntegral cy - dimY) / 2)
      (xoff', yoff') = (round xoff * 12700, round yoff * 12700) :: (Integer, Integer)

  let cNvPicPr = mknode "p:cNvPicPr" [] $
                 mknode "a:picLocks" [("noGrp","1")
                                     ,("noChangeAspect","1")] ()
  -- cNvPr will contain the link information so we do that separately,
  -- and register the link if necessary.
  let cNvPrAttr = [("descr", T.pack $ mInfoFilePath mInfo),
                   ("id","0"),
                   ("name","Picture 1")]
  cNvPr <- case picPropLink picProps of
    Just link -> do idNum <- registerLink link
                    return $ mknode "p:cNvPr" cNvPrAttr $
                      mknode "a:hlinkClick" [("r:id", "rId" <> tshow idNum)] ()
    Nothing   -> return $ mknode "p:cNvPr" cNvPrAttr ()
  let nvPicPr  = mknode "p:nvPicPr" []
                 [ cNvPr
                 , cNvPicPr
                 , mknode "p:nvPr" [] ()]
  let blipFill = mknode "p:blipFill" []
                 [ mknode "a:blip" [("r:embed", "rId" <>
                     tshow (mInfoLocalId mInfo))] ()
                 , mknode "a:stretch" [] $
                   mknode "a:fillRect" [] () ]
  let xfrm =    mknode "a:xfrm" []
                [ mknode "a:off" [("x", tshow xoff'), ("y", tshow yoff')] ()
                , mknode "a:ext" [("cx", tshow dimX')
                                 ,("cy", tshow dimY')] () ]
  let prstGeom = mknode "a:prstGeom" [("prst","rect")] $
                 mknode "a:avLst" [] ()
  let ln =      mknode "a:ln" [("w","9525")]
                [ mknode "a:noFill" [] ()
                , mknode "a:headEnd" [] ()
                , mknode "a:tailEnd" [] () ]
  let spPr =    mknode "p:spPr" [("bwMode","auto")]
                [xfrm, prstGeom, mknode "a:noFill" [] (), ln]

  let picShape = mknode "p:pic" []
                 [ nvPicPr
                 , blipFill
                 , spPr ]

  -- And now, maybe create the caption:
  if hasCaption
    then do cap <- createCaption ((x, y), (cx, cytmp)) alt
            return [picShape, cap]
    else return [picShape]


paraElemToElements :: PandocMonad m => ParaElem -> P m [Content]
paraElemToElements Break = return [Elem $ mknode "a:br" [] ()]
paraElemToElements (Run rpr s) = do
  sizeAttrs <- fontSizeAttributes rpr
  let attrs = sizeAttrs <>
        (
        [("b", "1") | rPropBold rpr]) <>
        (
        [("i", "1") | rPropItalics rpr]) <>
        (
        [("u", "sng") | rPropUnderline rpr]) <>
        (case rStrikethrough rpr of
            Just NoStrike     -> [("strike", "noStrike")]
            Just SingleStrike -> [("strike", "sngStrike")]
            Just DoubleStrike -> [("strike", "dblStrike")]
            Nothing -> []) <>
        (case rBaseline rpr of
            Just n -> [("baseline", tshow n)]
            Nothing -> []) <>
        (case rCap rpr of
            Just NoCapitals -> [("cap", "none")]
            Just SmallCapitals -> [("cap", "small")]
            Just AllCapitals -> [("cap", "all")]
            Nothing -> []) <>
        []
  linkProps <- case rLink rpr of
                 Just link -> do
                   idNum <- registerLink link
                   -- first we have to make sure that if it's an
                   -- anchor, it's in the anchor map. If not, there's
                   -- no link.
                   return $ case link of
                     InternalTarget _ ->
                       let linkAttrs =
                             [ ("r:id", "rId" <> tshow idNum)
                             , ("action", "ppaction://hlinksldjump")
                             ]
                       in [mknode "a:hlinkClick" linkAttrs ()]
                     -- external
                     ExternalTarget _ ->
                       let linkAttrs =
                             [ ("r:id", "rId" <> tshow idNum)
                             ]
                       in [mknode "a:hlinkClick" linkAttrs ()]
                 Nothing -> return []
  let colorContents = case rSolidFill rpr of
                        Just color ->
                          case fromColor color of
                            '#':hx ->
                              [mknode "a:solidFill" []
                                [mknode "a:srgbClr"
                                  [("val", T.toUpper $ T.pack hx)] ()]]
                            _ -> []
                        Nothing -> []
  codeFont <- monospaceFont
  let codeContents =
        [mknode "a:latin" [("typeface", codeFont)] () | rPropCode rpr]
  let propContents = linkProps <> colorContents <> codeContents
  return [Elem $ mknode "a:r" [] [ mknode "a:rPr" attrs propContents
                                 , mknode "a:t" [] s
                                 ]]
paraElemToElements (MathElem mathType texStr) = do
  isInSpkrNotes <- asks envInSpeakerNotes
  if isInSpkrNotes
    then paraElemToElements $ Run def $ unTeXString texStr
    else do res <- convertMath writeOMML mathType (unTeXString texStr)
            case fromXLElement <$> res of
              Right r -> return [Elem $ mknode "a14:m" [] $ addMathInfo r]
              Left (Str s) -> paraElemToElements (Run def s)
              Left _       -> throwError $ PandocShouldNeverHappenError "non-string math fallback"
paraElemToElements (RawOOXMLParaElem str) = return
  [Text (CData CDataRaw str Nothing)]


-- This is a bit of a kludge -- really requires adding an option to
-- TeXMath, but since that's a different package, we'll do this one
-- step at a time.
addMathInfo :: Element -> Element
addMathInfo element =
  let mathspace =
        Attr { attrKey = QName "m" Nothing (Just "xmlns")
             , attrVal = "http://schemas.openxmlformats.org/officeDocument/2006/math"
             }
  in add_attr mathspace element

-- We look through the element to see if it contains an a14:m
-- element. If so, we surround it. This is a bit ugly, but it seems
-- more dependable than looking through shapes for math. Plus this is
-- an xml implementation detail, so it seems to make sense to do it at
-- the xml level.
surroundWithMathAlternate :: Element -> Element
surroundWithMathAlternate element =
  case findElement (QName "m" Nothing (Just "a14")) element of
    Just _ ->
      mknode "mc:AlternateContent"
         [("xmlns:mc", "http://schemas.openxmlformats.org/markup-compatibility/2006")
         ] [ mknode "mc:Choice"
             [ ("xmlns:a14", "http://schemas.microsoft.com/office/drawing/2010/main")
             , ("Requires", "a14")] [ element ]
           ]
    Nothing -> element

paragraphToElement :: PandocMonad m => Paragraph -> P m Element
paragraphToElement par = do
  let
    attrs = [("lvl", tshow $ pPropLevel $ paraProps par)] <>
            (case pPropMarginLeft (paraProps par) of
               Just px -> [("marL", tshow $ pixelsToEmu px)]
               Nothing -> []
            ) <>
            (case pPropIndent (paraProps par) of
               Just px -> [("indent", tshow $ pixelsToEmu px)]
               Nothing -> []
            ) <>
            (case pPropAlign (paraProps par) of
               Just AlgnLeft -> [("algn", "l")]
               Just AlgnRight -> [("algn", "r")]
               Just AlgnCenter -> [("algn", "ctr")]
               Nothing -> []
            )
    props = [] <>
            (case pPropSpaceBefore $ paraProps par of
               Just px -> [mknode "a:spcBef" [] [
                              mknode "a:spcPts" [("val", tshow $ 100 * px)] ()
                              ]
                          ]
               Nothing -> []
            ) <>
            (case pPropBullet $ paraProps par of
               Just Bullet -> []
               Just (AutoNumbering attrs') ->
                 [mknode "a:buAutoNum" (autoNumAttrs attrs') ()]
               Nothing -> [mknode "a:buNone" [] ()]
            )
  paras <- mapM paraElemToElements (paraElems par)
  return $ mknode "a:p" [] $
    [Elem $ mknode "a:pPr" attrs props] <> concat paras

shapeToElement :: PandocMonad m => Element -> Shape -> P m Element
shapeToElement layout (TextBox paras)
  | ns <- elemToNameSpaces layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld = do
      sp <- getContentShape ns spTree
      elements <- mapM paragraphToElement paras
      let txBody = mknode "p:txBody" [] $
                   [mknode "a:bodyPr" [] (), mknode "a:lstStyle" [] ()] <> elements
          emptySpPr = mknode "p:spPr" [] ()
      return
        . surroundWithMathAlternate
        . replaceNamedChildren ns "p" "txBody" [txBody]
        . replaceNamedChildren ns "p" "spPr" [emptySpPr]
        $ sp
-- GraphicFrame and Pic should never reach this.
shapeToElement _ _ = return $ mknode "p:sp" [] ()

shapeToElements :: PandocMonad m => Element -> Shape -> P m [Content]
shapeToElements layout (Pic picProps fp alt) = do
  mInfo <- registerMedia fp alt
  case mInfoExt mInfo of
    Just _ -> map Elem <$>
      makePicElements layout picProps mInfo alt
    Nothing -> shapeToElements layout $ TextBox [Paragraph def alt]
shapeToElements layout (GraphicFrame tbls cptn) = map Elem <$>
  graphicFrameToElements layout tbls cptn
shapeToElements _ (RawOOXMLShape str) = return
  [Text (CData CDataRaw str Nothing)]
shapeToElements layout shp = do
  element <- shapeToElement layout shp
  return [Elem element]

shapesToElements :: PandocMonad m => Element -> [Shape] -> P m [Content]
shapesToElements layout shps =
 concat <$> mapM (shapeToElements layout) shps

graphicFrameToElements :: PandocMonad m => Element -> [Graphic] -> [ParaElem] -> P m [Element]
graphicFrameToElements layout tbls caption = do
  -- get the sizing
  master <- getMaster
  (pageWidth, pageHeight) <- asks envPresentationSize
  let ns = elemToNameSpaces layout
  ((x, y), (cx, cytmp)) <- getContentShapeSize ns layout master
                           `catchError`
                           (\_ -> return ((0, 0), (pageWidth, pageHeight)))

  let cy = if not $ null caption then cytmp - captionHeight else cytmp

  elements <- mapM (graphicToElement cx) tbls
  let graphicFrameElts =
        mknode "p:graphicFrame" [] $
        [ mknode "p:nvGraphicFramePr" []
          [ mknode "p:cNvPr" [("id", "6"), ("name", "Content Placeholder 5")] ()
          , mknode "p:cNvGraphicFramePr" []
            [mknode "a:graphicFrameLocks" [("noGrp", "1")] ()]
          , mknode "p:nvPr" []
            [mknode "p:ph" [("idx", "1")] ()]
          ]
        , mknode "p:xfrm" []
          [ mknode "a:off" [("x", tshow $ 12700 * x),
                            ("y", tshow $ 12700 * y)] ()
          , mknode "a:ext" [("cx", tshow $ 12700 * cx),
                            ("cy", tshow $ 12700 * cy)] ()
          ]
        ] <> elements

  if not $ null caption
    then do capElt <- createCaption ((x, y), (cx, cytmp)) caption
            return [graphicFrameElts, capElt]
    else return [graphicFrameElts]

getDefaultTableStyle :: PandocMonad m => P m (Maybe T.Text)
getDefaultTableStyle = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  tblStyleLst <- parseXml refArchive distArchive "ppt/tableStyles.xml"
  return $ findAttr (QName "def" Nothing Nothing) tblStyleLst

graphicToElement :: PandocMonad m => Integer -> Graphic -> P m Element
graphicToElement tableWidth (Tbl tblPr hdrCells rows) = do
  let colWidths = if null hdrCells
                  then case rows of
                         r : _ | not (null r) -> replicate (length r) $
                                                 tableWidth `div` toInteger (length r)
                         -- satisfy the compiler. This is the same as
                         -- saying that rows is empty, but the compiler
                         -- won't understand that `[]` exhausts the
                         -- alternatives.
                         _ -> []
                  else replicate (length hdrCells) $
                       tableWidth `div` toInteger (length hdrCells)

  let cellToOpenXML paras =
        do elements <- mapM paragraphToElement paras
           let elements' = if null elements
                           then [mknode "a:p" [] [mknode "a:endParaRPr" [] ()]]
                           else elements

           return
             [mknode "a:txBody" [] $
               [ mknode "a:bodyPr" [] ()
               , mknode "a:lstStyle" [] ()]
               <> elements']
  headers' <- mapM cellToOpenXML hdrCells
  rows' <- mapM (mapM cellToOpenXML) rows
  let borderProps = mknode "a:tcPr" [] ()
  let emptyCell' = [mknode "a:p" [] [mknode "a:pPr" [] ()]]
  let mkcell border contents = mknode "a:tc" []
                            $ (if null contents
                               then emptyCell'
                               else contents) <> [ borderProps | border ]
  let mkrow border cells = mknode "a:tr" [("h", "0")] $ map (mkcell border) cells

  let mkgridcol w = mknode "a:gridCol"
                       [("w", tshow ((12700 * w) :: Integer))] ()
  let hasHeader = not (all null hdrCells)

  mbDefTblStyle <- getDefaultTableStyle
  let tblPrElt = mknode "a:tblPr"
                 [ ("firstRow", if tblPrFirstRow tblPr then "1" else "0")
                 , ("bandRow", if tblPrBandRow tblPr then "1" else "0")
                 ] (case mbDefTblStyle of
                      Nothing -> []
                      Just sty -> [mknode "a:tableStyleId" [] sty])

  return $ mknode "a:graphic" []
    [mknode "a:graphicData" [("uri", "http://schemas.openxmlformats.org/drawingml/2006/table")]
     [mknode "a:tbl" [] $
      [ tblPrElt
      , mknode "a:tblGrid" [] (if all (==0) colWidths
                               then []
                               else map mkgridcol colWidths)
      ]
      <> [ mkrow True headers' | hasHeader ] <> map (mkrow False) rows'
     ]
    ]


-- We get the shape by placeholder type. If there is NO type, it
-- defaults to a content placeholder.

data PHType = PHType T.Text | ObjType
  deriving (Show, Eq)

findPHType :: NameSpaces -> Element -> PHType -> Bool
findPHType ns spElem phType
  | isElem ns "p" "sp" spElem =
    let mbPHElem = (Just spElem >>=
                   findChild (elemName ns "p" "nvSpPr") >>=
                   findChild (elemName ns "p" "nvPr") >>=
                   findChild (elemName ns "p" "ph"))
    in
      case mbPHElem of
        -- if it's a named PHType, we want to check that the attribute
        -- value matches.
        Just phElem | (PHType tp) <- phType ->
                        case findAttr (QName "type" Nothing Nothing) phElem of
                          Just tp' -> tp == tp'
                          Nothing -> False
        -- if it's an ObjType, we want to check that there is NO
        -- "type" attribute. In other words, a lookup should return nothing.
        Just phElem | ObjType <- phType ->
                        case findAttr (QName "type" Nothing Nothing) phElem of
                          Just _ -> False
                          Nothing -> True
        Nothing -> False
findPHType _ _ _ = False

getShapesByPlaceHolderType :: NameSpaces -> Element -> PHType -> [Element]
getShapesByPlaceHolderType ns spTreeElem phType
  | isElem ns "p" "spTree" spTreeElem =
      filterChildren (\e -> findPHType ns e phType) spTreeElem
  | otherwise = []

getShapeByPlaceHolderType :: NameSpaces -> Element -> PHType -> Maybe Element
getShapeByPlaceHolderType ns spTreeElem phType =
  listToMaybe $ getShapesByPlaceHolderType ns spTreeElem phType

-- Like the above, but it tries a number of different placeholder types
getShapeByPlaceHolderTypes :: NameSpaces -> Element -> [PHType] -> Maybe Element
getShapeByPlaceHolderTypes _ _ [] = Nothing
getShapeByPlaceHolderTypes ns spTreeElem (s:ss) =
  case getShapeByPlaceHolderType ns spTreeElem s of
    Just element -> Just element
    Nothing -> getShapeByPlaceHolderTypes ns spTreeElem ss

nonBodyTextToElement :: PandocMonad m => Element -> [PHType] -> [ParaElem] -> P m Element
nonBodyTextToElement layout phTypes paraElements
  | ns <- elemToNameSpaces layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld
  , Just sp <- getShapeByPlaceHolderTypes ns spTree phTypes = do
      let hdrPara = Paragraph def paraElements
      element <- paragraphToElement hdrPara
      let txBody = mknode "p:txBody" [] $
                   [mknode "a:bodyPr" [] (), mknode "a:lstStyle" [] ()] <>
                   [element]
      return $ replaceNamedChildren ns "p" "txBody" [txBody] sp
  -- XXX: TODO
  | otherwise = return $ mknode "p:sp" [] ()

contentToElement :: PandocMonad m => Element -> [ParaElem] -> [Shape] -> P m Element
contentToElement layout hdrShape shapes
  | ns <- elemToNameSpaces layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld = do
      element <- nonBodyTextToElement layout [PHType "title"] hdrShape
      let hdrShapeElements = [Elem element | not (null hdrShape)]
      contentElements <- local
                         (\env -> env {envContentType = NormalContent})
                         (shapesToElements layout shapes)
      return $ buildSpTree ns spTree (hdrShapeElements <> contentElements)
contentToElement _ _ _ = return $ mknode "p:sp" [] ()

twoColumnToElement :: PandocMonad m => Element -> [ParaElem] -> [Shape] -> [Shape] -> P m Element
twoColumnToElement layout hdrShape shapesL shapesR
  | ns <- elemToNameSpaces layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld = do
      element <- nonBodyTextToElement layout [PHType "title"] hdrShape
      let hdrShapeElements = [Elem element | not (null hdrShape)]
      contentElementsL <- local
                          (\env -> env {envContentType =TwoColumnLeftContent})
                          (shapesToElements layout shapesL)
      contentElementsR <- local
                          (\env -> env {envContentType =TwoColumnRightContent})
                          (shapesToElements layout shapesR)
      -- let contentElementsL' = map (setIdx ns "1") contentElementsL
      --     contentElementsR' = map (setIdx ns "2") contentElementsR
      return $ buildSpTree ns spTree $
        hdrShapeElements <> contentElementsL <> contentElementsR
twoColumnToElement _ _ _ _= return $ mknode "p:sp" [] ()


titleToElement :: PandocMonad m => Element -> [ParaElem] -> P m Element
titleToElement layout titleElems
  | ns <- elemToNameSpaces layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld = do
      element <- nonBodyTextToElement layout [PHType "title", PHType "ctrTitle"] titleElems
      let titleShapeElements = [Elem element | not (null titleElems)]
      return $ buildSpTree ns spTree titleShapeElements
titleToElement _ _ = return $ mknode "p:sp" [] ()

metadataToElement :: PandocMonad m => Element -> [ParaElem] -> [ParaElem] -> [[ParaElem]] -> [ParaElem] -> P m Element
metadataToElement layout titleElems subtitleElems authorsElems dateElems
  | ns <- elemToNameSpaces layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld = do
      titleShapeElements <- if null titleElems
                            then return []
                            else sequence [nonBodyTextToElement layout [PHType "ctrTitle"] titleElems]
      let combinedAuthorElems = intercalate [Break] authorsElems
          subtitleAndAuthorElems = intercalate [Break, Break] [subtitleElems, combinedAuthorElems]
      subtitleShapeElements <- if null subtitleAndAuthorElems
                               then return []
                               else sequence [nonBodyTextToElement layout [PHType "subTitle"] subtitleAndAuthorElems]
      dateShapeElements <- if null dateElems
                           then return []
                           else sequence [nonBodyTextToElement layout [PHType "dt"] dateElems]
      return . buildSpTree ns spTree . map Elem $
        (titleShapeElements <> subtitleShapeElements <> dateShapeElements)
metadataToElement _ _ _ _ _ = return $ mknode "p:sp" [] ()

slideToElement :: PandocMonad m => Slide -> P m Element
slideToElement (Slide _ l@(ContentSlide hdrElems shapes) _ )= do
  layout <- getLayout l
  spTree <- local (\env -> if null hdrElems
                           then env
                           else env{envSlideHasHeader=True}) $
            contentToElement layout hdrElems shapes
  return $ mknode "p:sld"
    [ ("xmlns:a", "http://schemas.openxmlformats.org/drawingml/2006/main"),
      ("xmlns:r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships"),
      ("xmlns:p", "http://schemas.openxmlformats.org/presentationml/2006/main")
    ] [mknode "p:cSld" [] [spTree]]
slideToElement (Slide _ l@(TwoColumnSlide hdrElems shapesL shapesR) _) = do
  layout <- getLayout l
  spTree <- local (\env -> if null hdrElems
                           then env
                           else env{envSlideHasHeader=True}) $
            twoColumnToElement layout hdrElems shapesL shapesR
  return $ mknode "p:sld"
    [ ("xmlns:a", "http://schemas.openxmlformats.org/drawingml/2006/main"),
      ("xmlns:r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships"),
      ("xmlns:p", "http://schemas.openxmlformats.org/presentationml/2006/main")
    ] [mknode "p:cSld" [] [spTree]]
slideToElement (Slide _ l@(TitleSlide hdrElems) _) = do
  layout <- getLayout l
  spTree <- titleToElement layout hdrElems
  return $ mknode "p:sld"
    [ ("xmlns:a", "http://schemas.openxmlformats.org/drawingml/2006/main"),
      ("xmlns:r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships"),
      ("xmlns:p", "http://schemas.openxmlformats.org/presentationml/2006/main")
    ] [mknode "p:cSld" [] [spTree]]
slideToElement (Slide _ l@(MetadataSlide titleElems subtitleElems authorElems dateElems) _) = do
  layout <- getLayout l
  spTree <- metadataToElement layout titleElems subtitleElems authorElems dateElems
  return $ mknode "p:sld"
    [ ("xmlns:a", "http://schemas.openxmlformats.org/drawingml/2006/main"),
      ("xmlns:r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships"),
      ("xmlns:p", "http://schemas.openxmlformats.org/presentationml/2006/main")
    ] [mknode "p:cSld" [] [spTree]]


--------------------------------------------------------------------
-- Notes:

getNotesMaster :: PandocMonad m => P m Element
getNotesMaster = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  parseXml refArchive distArchive "ppt/notesMasters/notesMaster1.xml"

getSlideNumberFieldId :: PandocMonad m => Element -> P m T.Text
getSlideNumberFieldId notesMaster
  | ns <- elemToNameSpaces notesMaster
  , Just cSld <- findChild (elemName ns "p" "cSld") notesMaster
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld
  , Just sp <- getShapeByPlaceHolderType ns spTree (PHType "sldNum")
  , Just txBody <- findChild (elemName ns "p" "txBody") sp
  , Just p <- findChild (elemName ns "a" "p") txBody
  , Just fld <- findChild (elemName ns "a" "fld") p
  , Just fldId <- findAttr (QName "id" Nothing Nothing) fld =
      return fldId
  | otherwise = throwError $
                PandocSomeError
                "No field id for slide numbers in notesMaster.xml"

speakerNotesSlideImage :: Element
speakerNotesSlideImage =
  mknode "p:sp" []
  [ mknode "p:nvSpPr" []
    [ mknode "p:cNvPr" [ ("id", "2")
                       , ("name", "Slide Image Placeholder 1")
                       ] ()
    , mknode "p:cNvSpPr" []
      [ mknode "a:spLocks" [ ("noGrp", "1")
                           , ("noRot", "1")
                           , ("noChangeAspect", "1")
                           ] ()
      ]
    , mknode "p:nvPr" []
      [ mknode "p:ph" [("type", "sldImg")] ()]
    ]
  , mknode "p:spPr" [] ()
  ]

-- we want to wipe links from the speaker notes in the
-- paragraphs. Powerpoint doesn't allow you to input them, and it
-- would provide extra complications.
removeParaLinks :: Paragraph -> Paragraph
removeParaLinks paragraph = paragraph{paraElems = map f (paraElems paragraph)}
  where f (Run rProps s) = Run rProps{rLink=Nothing} s
        f pe             = pe

-- put an empty paragraph between paragraphs for more expected spacing.
spaceParas :: [Paragraph] -> [Paragraph]
spaceParas = intersperse (Paragraph def [])

speakerNotesBody :: PandocMonad m => [Paragraph] -> P m Element
speakerNotesBody paras = do
  elements <- local (\env -> env{envInSpeakerNotes = True}) $
              mapM paragraphToElement $ spaceParas $ map removeParaLinks paras
  let txBody = mknode "p:txBody" [] $
               [mknode "a:bodyPr" [] (), mknode "a:lstStyle" [] ()] <> elements
  return $
    mknode "p:sp" []
    [ mknode "p:nvSpPr" []
      [ mknode "p:cNvPr" [ ("id", "3")
                         , ("name", "Notes Placeholder 2")
                         ] ()
      , mknode "p:cNvSpPr" []
        [ mknode "a:spLocks" [("noGrp", "1")] ()]
      , mknode "p:nvPr" []
        [ mknode "p:ph" [("type", "body"), ("idx", "1")] ()]
      ]
    , mknode "p:spPr" [] ()
    , txBody
    ]

speakerNotesSlideNumber :: Int -> T.Text -> Element
speakerNotesSlideNumber pgNum fieldId =
  mknode "p:sp" []
  [ mknode "p:nvSpPr" []
    [ mknode "p:cNvPr" [ ("id", "4")
                       , ("name", "Slide Number Placeholder 3")
                       ] ()
    , mknode "p:cNvSpPr" []
      [ mknode "a:spLocks" [("noGrp", "1")] ()]
    , mknode "p:nvPr" []
      [ mknode "p:ph" [ ("type", "sldNum")
                      , ("sz", "quarter")
                      , ("idx", "10")
                      ] ()
      ]
    ]
  , mknode "p:spPr" [] ()
  , mknode "p:txBody" []
    [ mknode "a:bodyPr" [] ()
    , mknode "a:lstStyle" [] ()
    , mknode "a:p" []
      [ mknode "a:fld" [ ("id", fieldId)
                       , ("type", "slidenum")
                       ]
        [ mknode "a:rPr" [("lang", "en-US")] ()
        , mknode "a:t" [] (tshow pgNum)
        ]
      , mknode "a:endParaRPr" [("lang", "en-US")] ()
      ]
    ]
  ]

slideToSpeakerNotesElement :: PandocMonad m => Slide -> P m (Maybe Element)
slideToSpeakerNotesElement (Slide _ _ (SpeakerNotes [])) = return Nothing
slideToSpeakerNotesElement slide@(Slide _ _ (SpeakerNotes paras)) = do
  master <- getNotesMaster
  fieldId  <- getSlideNumberFieldId master
  num <- slideNum slide
  let imgShape = speakerNotesSlideImage
      sldNumShape = speakerNotesSlideNumber num fieldId
  bodyShape <- speakerNotesBody paras
  return $ Just $
    mknode "p:notes"
    [ ("xmlns:a", "http://schemas.openxmlformats.org/drawingml/2006/main")
    , ("xmlns:r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships")
    , ("xmlns:p", "http://schemas.openxmlformats.org/presentationml/2006/main")
    ] [ mknode "p:cSld" []
        [ mknode "p:spTree" []
          [ mknode "p:nvGrpSpPr" []
            [ mknode "p:cNvPr" [("id", "1"), ("name", "")] ()
            , mknode "p:cNvGrpSpPr" [] ()
            , mknode "p:nvPr" [] ()
            ]
          , mknode "p:grpSpPr" []
            [ mknode "a:xfrm" []
              [ mknode "a:off" [("x", "0"), ("y", "0")] ()
              , mknode "a:ext" [("cx", "0"), ("cy", "0")] ()
              , mknode "a:chOff" [("x", "0"), ("y", "0")] ()
              , mknode "a:chExt" [("cx", "0"), ("cy", "0")] ()
              ]
            ]
          , imgShape
          , bodyShape
          , sldNumShape
          ]
        ]
      ]

-----------------------------------------------------------------------

getSlideIdNum :: PandocMonad m => SlideId -> P m Int
getSlideIdNum sldId = do
  slideIdMap <- asks envSlideIdMap
  case  M.lookup sldId slideIdMap of
    Just n -> return n
    Nothing -> throwError $
               PandocShouldNeverHappenError $
               "Slide Id " <> tshow sldId <> " not found."

slideNum :: PandocMonad m => Slide -> P m Int
slideNum slide = getSlideIdNum $ slideId slide

idNumToFilePath :: Int -> FilePath
idNumToFilePath idNum = "slide" <> show idNum <> ".xml"

slideToFilePath :: PandocMonad m => Slide -> P m FilePath
slideToFilePath slide = do
  idNum <- slideNum slide
  return $ "slide" <> show idNum <> ".xml"

slideToRelId :: PandocMonad m => Slide -> P m T.Text
slideToRelId slide = do
  n <- slideNum slide
  offset <- asks envSlideIdOffset
  return $ "rId" <> tshow (n + offset)


data Relationship = Relationship { relId :: Int
                                 , relType :: MimeType
                                 , relTarget :: FilePath
                                 } deriving (Show, Eq)

elementToRel :: Element -> Maybe Relationship
elementToRel element
  | elName element == QName "Relationship" (Just "http://schemas.openxmlformats.org/package/2006/relationships") Nothing =
      do rId <- findAttr (QName "Id" Nothing Nothing) element
         numStr <- T.stripPrefix "rId" rId
         num <- fromIntegral <$> readTextAsInteger numStr
         type' <- findAttr (QName "Type" Nothing Nothing) element
         target <- findAttr (QName "Target" Nothing Nothing) element
         return $ Relationship num type' (T.unpack target)
  | otherwise = Nothing

slideToPresRel :: PandocMonad m => Slide -> P m Relationship
slideToPresRel slide = do
  idNum <- slideNum slide
  n <- asks envSlideIdOffset
  let rId = idNum + n
      fp = "slides/" <> idNumToFilePath idNum
  return $ Relationship { relId = rId
                        , relType = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slide"
                        , relTarget = fp
                        }

getRels :: PandocMonad m => P m [Relationship]
getRels = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  relsElem <- parseXml refArchive distArchive "ppt/_rels/presentation.xml.rels"
  let globalNS = "http://schemas.openxmlformats.org/package/2006/relationships"
  let relElems = findChildren (QName "Relationship" (Just globalNS) Nothing) relsElem
  return $ mapMaybe elementToRel relElems

presentationToRels :: PandocMonad m => Presentation -> P m [Relationship]
presentationToRels pres@(Presentation _ slides) = do
  mySlideRels <- mapM slideToPresRel slides
  let notesMasterRels =
        [Relationship { relId = length mySlideRels + 2
                         , relType = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/notesMaster"
                         , relTarget = "notesMasters/notesMaster1.xml"
                         } | presHasSpeakerNotes pres]
      insertedRels = mySlideRels <> notesMasterRels
  rels <- getRels
  -- we remove the slide rels and the notesmaster (if it's
  -- there). We'll put these back in ourselves, if necessary.
  let relsWeKeep = filter
                   (\r -> relType r /= "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slide" &&
                          relType r /= "http://schemas.openxmlformats.org/officeDocument/2006/relationships/notesMaster")
                   rels
  -- We want to make room for the slides in the id space. The slides
  -- will start at Id2 (since Id1 is for the slide master). There are
  -- two slides in the data file, but that might change in the future,
  -- so we will do this:
  --
  -- 1. We look to see what the minimum relWithoutSlide id (greater than 1) is.
  -- 2. We add the difference between this and the number of slides to
  -- all relWithoutSlide rels (unless they're 1)
  -- 3. If we have a notesmaster slide, we make space for that as well.

  let minRelNotOne = maybe 0 minimum $ nonEmpty
                                     $ filter (1 <) $ map relId relsWeKeep

      modifyRelNum :: Int -> Int
      modifyRelNum 1 = 1
      modifyRelNum n = n - minRelNotOne + 2 + length insertedRels

      relsWeKeep' = map (\r -> r{relId = modifyRelNum $ relId r}) relsWeKeep

  return $ insertedRels <> relsWeKeep'

-- We make this ourselves, in case there's a thumbnail in the one from
-- the template.
topLevelRels :: [Relationship]
topLevelRels =
  [ Relationship { relId = 1
                 , relType = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument"
                 , relTarget = "ppt/presentation.xml"
                 }
  , Relationship { relId = 2
                 , relType = "http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties"
                 , relTarget = "docProps/core.xml"
                 }
  , Relationship { relId = 3
                 , relType = "http://schemas.openxmlformats.org/package/2006/relationships/metadata/extended-properties"
                 , relTarget = "docProps/app.xml"
                 }
  , Relationship { relId = 4
                 , relType = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/custom-properties"
                 , relTarget = "docProps/custom.xml"
                 }
  ]

topLevelRelsEntry :: PandocMonad m => P m Entry
topLevelRelsEntry = elemToEntry "_rels/.rels" $ relsToElement topLevelRels

relToElement :: Relationship -> Element
relToElement rel = mknode "Relationship" [ ("Id", "rId" <> tshow (relId rel))
                                         , ("Type", relType rel)
                                         , ("Target", T.pack $ relTarget rel) ] ()

relsToElement :: [Relationship] -> Element
relsToElement rels = mknode "Relationships"
                     [("xmlns", "http://schemas.openxmlformats.org/package/2006/relationships")]
                     (map relToElement rels)

presentationToRelsEntry :: PandocMonad m => Presentation -> P m Entry
presentationToRelsEntry pres = do
  rels <- presentationToRels pres
  elemToEntry "ppt/_rels/presentation.xml.rels" $ relsToElement rels

elemToEntry :: PandocMonad m => FilePath -> Element -> P m Entry
elemToEntry fp element = do
  epochtime <- floor . utcTimeToPOSIXSeconds <$> asks envUTCTime
  return $ toEntry fp epochtime $ renderXml element

slideToEntry :: PandocMonad m => Slide -> P m Entry
slideToEntry slide = do
  idNum <- slideNum slide
  local (\env -> env{envCurSlideId = idNum}) $ do
    element <- slideToElement slide
    elemToEntry ("ppt/slides/" <> idNumToFilePath idNum) element

slideToSpeakerNotesEntry :: PandocMonad m => Slide -> P m (Maybe Entry)
slideToSpeakerNotesEntry slide = do
  idNum <- slideNum slide
  local (\env -> env{envCurSlideId = idNum}) $ do
    mbElement <- slideToSpeakerNotesElement slide
    mbNotesIdNum <- do mp <- asks envSpeakerNotesIdMap
                       return $ M.lookup idNum mp
    case mbElement of
      Just element | Just notesIdNum <- mbNotesIdNum ->
                       Just <$>
                       elemToEntry
                       ("ppt/notesSlides/notesSlide" <> show notesIdNum <>
                        ".xml")
                       element
      _ -> return Nothing

slideToSpeakerNotesRelElement :: PandocMonad m => Slide -> P m (Maybe Element)
slideToSpeakerNotesRelElement (Slide _ _ (SpeakerNotes [])) = return Nothing
slideToSpeakerNotesRelElement slide@Slide{} = do
  idNum <- slideNum slide
  return $ Just $
    mknode "Relationships"
    [("xmlns", "http://schemas.openxmlformats.org/package/2006/relationships")]
    [ mknode "Relationship" [ ("Id", "rId2")
                            , ("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slide")
                            , ("Target", "../slides/slide" <> tshow idNum <> ".xml")
                            ] ()
    , mknode "Relationship" [ ("Id", "rId1")
                            , ("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/notesMaster")
                            , ("Target", "../notesMasters/notesMaster1.xml")
                            ] ()
    ]


slideToSpeakerNotesRelEntry :: PandocMonad m => Slide -> P m (Maybe Entry)
slideToSpeakerNotesRelEntry slide = do
  idNum <- slideNum slide
  mbElement <- slideToSpeakerNotesRelElement slide
  mp <- asks envSpeakerNotesIdMap
  let mbNotesIdNum = M.lookup idNum mp
  case mbElement of
    Just element | Just notesIdNum <- mbNotesIdNum ->
      Just <$>
      elemToEntry
      ("ppt/notesSlides/_rels/notesSlide" <> show notesIdNum <> ".xml.rels")
      element
    _ -> return Nothing

slideToSlideRelEntry :: PandocMonad m => Slide -> P m Entry
slideToSlideRelEntry slide = do
  idNum <- slideNum slide
  element <- slideToSlideRelElement slide
  elemToEntry ("ppt/slides/_rels/" <> idNumToFilePath idNum <> ".rels") element

linkRelElement :: PandocMonad m => (Int, LinkTarget) -> P m Element
linkRelElement (rIdNum, InternalTarget targetId) = do
  targetIdNum <- getSlideIdNum targetId
  return $
    mknode "Relationship" [ ("Id", "rId" <> tshow rIdNum)
                          , ("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slide")
                          , ("Target", "slide" <> tshow targetIdNum <> ".xml")
                          ] ()
linkRelElement (rIdNum, ExternalTarget (url, _)) =
  return $
    mknode "Relationship" [ ("Id", "rId" <> tshow rIdNum)
                          , ("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink")
                          , ("Target", url)
                          , ("TargetMode", "External")
                          ] ()

linkRelElements :: PandocMonad m => M.Map Int LinkTarget -> P m [Element]
linkRelElements mp = mapM linkRelElement (M.toList mp)

mediaRelElement :: MediaInfo -> Element
mediaRelElement mInfo =
  let ext = fromMaybe "" (mInfoExt mInfo)
  in
    mknode "Relationship" [ ("Id", "rId" <>
      tshow (mInfoLocalId mInfo))
                          , ("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image")
                          , ("Target", "../media/image" <>
      tshow (mInfoGlobalId mInfo) <> ext)
                          ] ()

speakerNotesSlideRelElement :: PandocMonad m => Slide -> P m (Maybe Element)
speakerNotesSlideRelElement slide = do
  idNum <- slideNum slide
  mp <- asks envSpeakerNotesIdMap
  return $ case M.lookup idNum mp of
    Nothing -> Nothing
    Just n ->
      let target = "../notesSlides/notesSlide" <> tshow n <> ".xml"
      in Just $
         mknode "Relationship" [ ("Id", "rId2")
                               , ("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/notesSlide")
                               , ("Target", target)
                               ] ()

slideToSlideRelElement :: PandocMonad m => Slide -> P m Element
slideToSlideRelElement slide = do
  idNum <- slideNum slide
  let target =  case slide of
        (Slide _ MetadataSlide{} _)  -> "../slideLayouts/slideLayout1.xml"
        (Slide _ TitleSlide{} _)     -> "../slideLayouts/slideLayout3.xml"
        (Slide _ ContentSlide{} _)   -> "../slideLayouts/slideLayout2.xml"
        (Slide _ TwoColumnSlide{} _) -> "../slideLayouts/slideLayout4.xml"

  speakerNotesRels <- maybeToList <$> speakerNotesSlideRelElement slide

  linkIds <- gets stLinkIds
  mediaIds <- gets stMediaIds

  linkRels <- case M.lookup idNum linkIds of
                Just mp -> linkRelElements mp
                Nothing -> return []
  let mediaRels = case M.lookup idNum mediaIds of
                   Just mInfos -> map mediaRelElement mInfos
                   Nothing -> []

  return $
    mknode "Relationships"
    [("xmlns", "http://schemas.openxmlformats.org/package/2006/relationships")]
    ([mknode "Relationship" [ ("Id", "rId1")
                           , ("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideLayout")
                           , ("Target", target)] ()
    ] <> speakerNotesRels <> linkRels <> mediaRels)

slideToSldIdElement :: PandocMonad m => Slide -> P m Element
slideToSldIdElement slide = do
  n <- slideNum slide
  let id' = tshow $ n + 255
  rId <- slideToRelId slide
  return $ mknode "p:sldId" [("id", id'), ("r:id", rId)] ()

presentationToSldIdLst :: PandocMonad m => Presentation -> P m Element
presentationToSldIdLst (Presentation _ slides) = do
  ids <- mapM slideToSldIdElement slides
  return $ mknode "p:sldIdLst" [] ids

presentationToPresentationElement :: PandocMonad m => Presentation -> P m Element
presentationToPresentationElement pres@(Presentation _ slds) = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  element <- parseXml refArchive distArchive "ppt/presentation.xml"
  sldIdLst <- presentationToSldIdLst pres

  let modifySldIdLst :: Content -> Content
      modifySldIdLst (Elem e) = case elName e of
        (QName "sldIdLst" _ _) -> Elem sldIdLst
        _                      -> Elem e
      modifySldIdLst ct = ct

      notesMasterRId = length slds + 2

      notesMasterElem =  mknode "p:notesMasterIdLst" []
                         [ mknode
                           "p:NotesMasterId"
                           [("r:id", "rId" <> tshow notesMasterRId)]
                           ()
                         ]

      -- if there's a notesMasterIdLst in the presentation.xml file,
      -- we want to remove it. We then want to put our own, if
      -- necessary, after the slideMasterIdLst element. We also remove
      -- handouts master, since we don't want it.

      removeUnwantedMaster' :: Content -> [Content]
      removeUnwantedMaster' (Elem e) = case elName e of
        (QName "notesMasterIdLst" _ _) -> []
        (QName "handoutMasterIdLst" _ _) -> []
        _                              -> [Elem e]
      removeUnwantedMaster' ct = [ct]

      removeUnwantedMaster :: [Content] -> [Content]
      removeUnwantedMaster = concatMap removeUnwantedMaster'

      insertNotesMaster' :: Content -> [Content]
      insertNotesMaster' (Elem e) = case elName e of
        (QName "sldMasterIdLst" _ _) -> [Elem e, Elem notesMasterElem]
        _                            -> [Elem e]
      insertNotesMaster' ct = [ct]

      insertNotesMaster :: [Content] -> [Content]
      insertNotesMaster = if presHasSpeakerNotes pres
                          then concatMap insertNotesMaster'
                          else id

      newContent = insertNotesMaster $
                   removeUnwantedMaster $
                   map modifySldIdLst $
                   elContent element

  return $ element{elContent = newContent}

presentationToPresEntry :: PandocMonad m => Presentation -> P m Entry
presentationToPresEntry pres = presentationToPresentationElement pres >>=
  elemToEntry "ppt/presentation.xml"

-- adapted from the Docx writer
docPropsElement :: PandocMonad m => DocProps -> P m Element
docPropsElement docProps = do
  utctime <- asks envUTCTime
  let keywords = case dcKeywords docProps of
        Just xs -> T.intercalate ", " xs
        Nothing -> ""
  return $
    mknode "cp:coreProperties"
    [("xmlns:cp","http://schemas.openxmlformats.org/package/2006/metadata/core-properties")
    ,("xmlns:dc","http://purl.org/dc/elements/1.1/")
    ,("xmlns:dcterms","http://purl.org/dc/terms/")
    ,("xmlns:dcmitype","http://purl.org/dc/dcmitype/")
    ,("xmlns:xsi","http://www.w3.org/2001/XMLSchema-instance")]
    $
      mknode "dc:title" [] (fromMaybe "" $ dcTitle docProps)
    :
      mknode "dc:creator" [] (fromMaybe "" $ dcCreator docProps)
    :
      mknode "cp:keywords" [] keywords
    : ( [mknode "dc:subject" [] $ fromMaybe "" $ dcSubject docProps | isJust (dcSubject docProps)])
    <> ( [mknode "dc:description" [] $ fromMaybe "" $ dcDescription docProps | isJust (dcDescription docProps)])
    <> ( [mknode "cp:category" [] $ fromMaybe "" $ cpCategory docProps | isJust (cpCategory docProps)])
    <> (\x -> [ mknode "dcterms:created" [("xsi:type","dcterms:W3CDTF")] x
              , mknode "dcterms:modified" [("xsi:type","dcterms:W3CDTF")] x
              ]) (T.pack $ formatTime defaultTimeLocale "%FT%XZ" utctime)

docPropsToEntry :: PandocMonad m => DocProps -> P m Entry
docPropsToEntry docProps = docPropsElement docProps >>=
                           elemToEntry "docProps/core.xml"

-- adapted from the Docx writer
docCustomPropsElement :: PandocMonad m => DocProps -> P m Element
docCustomPropsElement docProps = do
  let mkCustomProp (k, v) pid = mknode "property"
         [("fmtid","{D5CDD505-2E9C-101B-9397-08002B2CF9AE}")
         ,("pid", tshow pid)
         ,("name", k)] $ mknode "vt:lpwstr" [] v
  return $ mknode "Properties"
          [("xmlns","http://schemas.openxmlformats.org/officeDocument/2006/custom-properties")
          ,("xmlns:vt","http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes")
          ] $ zipWith mkCustomProp (fromMaybe [] $ customProperties docProps) [(2 :: Int)..]

docCustomPropsToEntry :: PandocMonad m => DocProps -> P m Entry
docCustomPropsToEntry docProps = docCustomPropsElement docProps >>=
                           elemToEntry "docProps/custom.xml"

-- We read from the template, but we remove the lastView, so it always
-- opens on slide view. Templates will sometimes be open in master
-- view for editing.
viewPropsElement :: PandocMonad m => P m Element
viewPropsElement = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  viewPrElement <- parseXml refArchive distArchive "ppt/viewProps.xml"
  -- remove  "lastView" if it exists:
  let notLastView :: XML.Attr -> Bool
      notLastView attr =
          qName (attrKey attr) /= "lastView"
  return $
    viewPrElement {elAttribs = filter notLastView (elAttribs viewPrElement)}

makeViewPropsEntry :: PandocMonad m => P m Entry
makeViewPropsEntry = viewPropsElement >>= elemToEntry "ppt/viewProps.xml"

defaultContentTypeToElem :: DefaultContentType -> Element
defaultContentTypeToElem dct =
  mknode "Default"
  [("Extension", defContentTypesExt dct),
    ("ContentType", defContentTypesType dct)]
  ()

overrideContentTypeToElem :: OverrideContentType -> Element
overrideContentTypeToElem oct =
  mknode "Override"
  [("PartName", T.pack $ overrideContentTypesPart oct),
   ("ContentType", overrideContentTypesType oct)]
  ()

contentTypesToElement :: ContentTypes -> Element
contentTypesToElement ct =
  let ns = "http://schemas.openxmlformats.org/package/2006/content-types"
  in
    mknode "Types" [("xmlns", ns)] $

      map defaultContentTypeToElem (contentTypesDefaults ct) <>
      map overrideContentTypeToElem (contentTypesOverrides ct)

data DefaultContentType = DefaultContentType
                           { defContentTypesExt :: T.Text
                           , defContentTypesType:: MimeType
                           }
                         deriving (Show, Eq)

data OverrideContentType = OverrideContentType
                           { overrideContentTypesPart :: FilePath
                           , overrideContentTypesType :: MimeType
                           }
                          deriving (Show, Eq)

data ContentTypes = ContentTypes { contentTypesDefaults :: [DefaultContentType]
                                 , contentTypesOverrides :: [OverrideContentType]
                                 }
                    deriving (Show, Eq)

contentTypesToEntry :: PandocMonad m => ContentTypes -> P m Entry
contentTypesToEntry ct = elemToEntry "[Content_Types].xml" $ contentTypesToElement ct

pathToOverride :: FilePath -> Maybe OverrideContentType
pathToOverride fp = OverrideContentType ("/" <> fp) <$> getContentType fp

mediaFileContentType :: FilePath -> Maybe DefaultContentType
mediaFileContentType fp = case takeExtension fp of
  '.' : ext -> Just $
               DefaultContentType { defContentTypesExt = T.pack ext
                                  , defContentTypesType =
                                      fromMaybe "application/octet-stream" (getMimeType fp)
                                  }
  _ -> Nothing

mediaContentType :: MediaInfo -> Maybe DefaultContentType
mediaContentType mInfo
  | Just t <- mInfoExt mInfo
  , Just ('.', ext) <- T.uncons t =
      Just $ DefaultContentType { defContentTypesExt = ext
                                , defContentTypesType =
                                    fromMaybe "application/octet-stream" (mInfoMimeType mInfo)
                                }
  | otherwise = Nothing

getSpeakerNotesFilePaths :: PandocMonad m => P m [FilePath]
getSpeakerNotesFilePaths = do
  mp <- asks envSpeakerNotesIdMap
  let notesIdNums = M.elems mp
  return $ map (\n -> "ppt/notesSlides/notesSlide" <> show n <> ".xml")
               notesIdNums

presentationToContentTypes :: PandocMonad m => Presentation -> P m ContentTypes
presentationToContentTypes p@(Presentation _ slides) = do
  mediaInfos <- mconcat . M.elems <$> gets stMediaIds
  filePaths <- patternsToFilePaths $ inheritedPatterns p
  let mediaFps = filter (match (compile "ppt/media/image*")) filePaths
  let defaults = [ DefaultContentType "xml" "application/xml"
                 , DefaultContentType "rels" "application/vnd.openxmlformats-package.relationships+xml"
                 ]
      mediaDefaults = nub $
                      mapMaybe mediaContentType mediaInfos <>
                      mapMaybe mediaFileContentType mediaFps

      inheritedOverrides = mapMaybe pathToOverride filePaths
      createdOverrides = mapMaybe pathToOverride [ "docProps/core.xml"
                                                 , "docProps/custom.xml"
                                                 , "ppt/presentation.xml"
                                                 , "ppt/viewProps.xml"
                                                 ]
  relativePaths <- mapM slideToFilePath slides
  let slideOverrides = mapMaybe
                       (\fp -> pathToOverride $ "ppt/slides/" <> fp)
                       relativePaths
  speakerNotesOverrides <- mapMaybe pathToOverride <$> getSpeakerNotesFilePaths
  return $ ContentTypes
    (defaults <> mediaDefaults)
    (inheritedOverrides <> createdOverrides <> slideOverrides <> speakerNotesOverrides)

presML :: T.Text
presML = "application/vnd.openxmlformats-officedocument.presentationml"

noPresML :: T.Text
noPresML = "application/vnd.openxmlformats-officedocument"

getContentType :: FilePath -> Maybe MimeType
getContentType fp
  | fp == "ppt/presentation.xml" = Just $ presML <> ".presentation.main+xml"
  | fp == "ppt/presProps.xml" = Just $ presML <> ".presProps+xml"
  | fp == "ppt/viewProps.xml" = Just $ presML <> ".viewProps+xml"
  | fp == "ppt/tableStyles.xml" = Just $ presML <> ".tableStyles+xml"
  | fp == "docProps/core.xml" = Just "application/vnd.openxmlformats-package.core-properties+xml"
  | fp == "docProps/custom.xml" = Just "application/vnd.openxmlformats-officedocument.custom-properties+xml"
  | fp == "docProps/app.xml" = Just $ noPresML <> ".extended-properties+xml"
  | ["ppt", "slideMasters", f] <- splitDirectories fp
  , (_, ".xml") <- splitExtension f =
      Just $ presML <> ".slideMaster+xml"
  | ["ppt", "slides", f] <- splitDirectories fp
  , (_, ".xml") <- splitExtension f =
      Just $ presML <> ".slide+xml"
  | ["ppt", "notesMasters", f] <- splitDirectories fp
  , (_, ".xml") <- splitExtension f =
      Just $ presML <> ".notesMaster+xml"
  | ["ppt", "notesSlides", f] <- splitDirectories fp
  , (_, ".xml") <- splitExtension f =
      Just $ presML <> ".notesSlide+xml"
  | ["ppt", "theme", f] <- splitDirectories fp
  , (_, ".xml") <- splitExtension f =
      Just $ noPresML <> ".theme+xml"
  | ["ppt", "slideLayouts", _] <- splitDirectories fp=
      Just $ presML <> ".slideLayout+xml"
  | otherwise = Nothing

-- Kept as String for XML.Light
autoNumAttrs :: ListAttributes -> [(Text, Text)]
autoNumAttrs (startNum, numStyle, numDelim) =
  numAttr <> typeAttr
  where
    numAttr = [("startAt", tshow startNum) | startNum /= 1]
    typeAttr = [("type", typeString <> delimString)]
    typeString = case numStyle of
      Decimal -> "arabic"
      UpperAlpha -> "alphaUc"
      LowerAlpha -> "alphaLc"
      UpperRoman -> "romanUc"
      LowerRoman -> "romanLc"
      _          -> "arabic"
    delimString = case numDelim of
      Period -> "Period"
      OneParen -> "ParenR"
      TwoParens -> "ParenBoth"
      _         -> "Period"
