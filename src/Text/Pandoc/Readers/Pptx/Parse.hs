{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Pptx.Parse
   Copyright   : © 2025 Anton Antic
   License     : GNU GPL, version 2 or above

   Maintainer  : Anton Antic <anton@everworker.ai>
   Stability   : alpha
   Portability : portable

Parsing of PPTX archive to intermediate representation.
-}
module Text.Pandoc.Readers.Pptx.Parse
  ( Pptx(..)
  , PresentationDoc(..)
  , PptxSlide(..)
  , SlideId(..)
  , archiveToPptx
  ) where

import Codec.Archive.Zip (Archive, Entry, findEntryByPath, fromEntry)
import qualified Data.ByteString.Lazy as B
import Data.List (find)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.Text (Text)
import System.FilePath (splitFileName)
import Text.Pandoc.Readers.OOXML.Shared
import Text.Pandoc.XML.Light
import Text.Read (readMaybe)

-- | Slide identifier
newtype SlideId = SlideId Int deriving (Show, Eq, Ord)

-- | Complete PPTX document (intermediate representation)
data Pptx = Pptx
  { pptxPresentation :: PresentationDoc
  , pptxSlides       :: [PptxSlide]
  , pptxArchive      :: Archive
  } deriving (Show)

-- | Individual slide data
data PptxSlide = PptxSlide
  { slideId      :: SlideId
  , slidePath    :: FilePath
  , slideElement :: Element     -- The parsed p:sld element
  , slideRels    :: [(Text, Text)]  -- Slide relationships
  } deriving (Show)

-- | Presentation-level information from presentation.xml
data PresentationDoc = PresentationDoc
  { presNameSpaces   :: NameSpaces
  , presSlideSize    :: (Integer, Integer)  -- (width, height) in pixels
  , presSlideIds     :: [(SlideId, Text)]   -- (slideId, relationshipId)
  } deriving (Show)

-- | Parse PPTX archive to intermediate representation
archiveToPptx :: Archive -> Either Text Pptx
archiveToPptx archive = do
  -- Find and parse presentation.xml
  presPath <- getPresentationXmlPath archive
  presElem <- loadXMLFromArchive archive presPath
  presDoc <- elemToPresentation presElem

  -- Load presentation relationships to resolve slide paths
  presRelsPath <- getPresentationRelsPath archive presPath
  presRels <- loadRelationships archive presRelsPath

  -- Parse each slide
  slides <- mapM (parseSlide archive presRels) (presSlideIds presDoc)

  return $ Pptx presDoc slides archive

-- | Find presentation.xml via root relationships
getPresentationXmlPath :: Archive -> Either Text FilePath
getPresentationXmlPath archive = do
  -- Load _rels/.rels
  relsEntry <- maybeToEither "Missing _rels/.rels" $
               findEntryByPath "_rels/.rels" archive

  relsElem <- parseXMLFromEntry relsEntry

  -- The Relationships element has a default namespace, but Relationship children don't use prefix
  -- We need to look at all children regardless of namespace
  let relElems = onlyElems $ elContent relsElem

  -- Look for relationship containing "officeDocument" in Type attribute
  case find isOfficeDocRel relElems of
    Nothing -> Left $ "No presentation.xml relationship found. Found " <>
                     T.pack (show (length relElems)) <> " relationships."
    Just rel -> do
      target <- maybeToEither "Missing Target attribute" $
                findAttr (unqual "Target") rel
      return $ T.unpack target  -- Convert Text to FilePath

  where
    isOfficeDocRel el =
      case findAttr (unqual "Type") el of
        -- Must end with "/officeDocument" to avoid matching "/extended-properties"
        Just relType -> "/officeDocument" `T.isSuffixOf` relType
        Nothing -> False

-- | Load and parse XML from archive entry
loadXMLFromArchive :: Archive -> FilePath -> Either Text Element
loadXMLFromArchive archive path = do
  entry <- maybeToEither ("Entry not found: " <> T.pack path) $
           findEntryByPath path archive

  let xmlBytes = fromEntry entry
  parseXMLFromBS xmlBytes

-- | Parse XML from ByteString
parseXMLFromBS :: B.ByteString -> Either Text Element
parseXMLFromBS = parseXMLElement . TL.decodeUtf8

-- | Parse XML from Entry
parseXMLFromEntry :: Entry -> Either Text Element
parseXMLFromEntry = parseXMLFromBS . fromEntry

-- | Parse presentation.xml element to PresentationDoc
elemToPresentation :: Element -> Either Text PresentationDoc
elemToPresentation presElem = do
  let ns = elemToNameSpaces presElem

  -- Extract slide size (with defaults)
  let sizeElem = findChildByName ns "p" "sldSz" presElem
      (widthEMU, heightEMU) = case sizeElem of
        Just el ->
          let cx = readAttrInt "cx" el
              cy = readAttrInt "cy" el
           in (cx, cy)
        Nothing -> (9144000, 6858000)  -- Default 10" x 7.5"

  -- Convert EMUs to pixels (approximate for metadata)
  let width = widthEMU `div` emusPerInch
      height = heightEMU `div` emusPerInch

  -- Extract slide ID list (optional - some presentations may have no slides)
  let sldIdLstElem = findChildByName ns "p" "sldIdLst" presElem

  slideRefs <- case sldIdLstElem of
    Nothing -> return []  -- No slides is valid for templates/masters-only presentations
    Just el -> do
      let sldIdElems = findChildren (elemName ns "p" "sldId") el
      mapM (extractSlideRef ns) (zip [1..] sldIdElems)

  return $ PresentationDoc
    { presNameSpaces = ns
    , presSlideSize = (width, height)
    , presSlideIds = slideRefs
    }

-- | Extract slide ID and relationship ID from p:sldId element
extractSlideRef :: NameSpaces -> (Int, Element) -> Either Text (SlideId, Text)
extractSlideRef ns (idx, sldIdElem) = do
  relId <- maybeToEither ("Missing r:id in slide " <> T.pack (show idx)) $
           findAttrByName ns "r" "id" sldIdElem

  return (SlideId idx, relId)

-- | Safe read attribute as Integer (with default of 0)
readAttrInt :: Text -> Element -> Integer
readAttrInt attrName el =
  case findAttr (unqual attrName) el of
    Just str -> case readMaybe (T.unpack str) of
      Just n -> n
      Nothing -> 0
    Nothing -> 0

-- | Get presentation relationships path
getPresentationRelsPath :: Archive -> FilePath -> Either Text FilePath
getPresentationRelsPath _archive presPath =
  -- ppt/presentation.xml → ppt/_rels/presentation.xml.rels
  let (dir, file) = splitFileName presPath
      relsPath = dir ++ "/_rels/" ++ file ++ ".rels"
   in Right relsPath

-- | Load relationships from .rels file
loadRelationships :: Archive -> FilePath -> Either Text [(Text, Text)]
loadRelationships archive relsPath =
  case findEntryByPath relsPath archive of
    Nothing -> Right []  -- No relationships is OK
    Just entry -> do
      relsElem <- parseXMLFromEntry entry
      let relElems = onlyElems $ elContent relsElem
      return $ mapMaybe extractRelationship relElems
  where
    extractRelationship el = do
      relId <- findAttr (unqual "Id") el
      target <- findAttr (unqual "Target") el
      return (relId, target)

-- | Parse a single slide
parseSlide :: Archive -> [(Text, Text)] -> (SlideId, Text) -> Either Text PptxSlide
parseSlide archive rels (sid, relId) = do
  -- Resolve relationship to get slide path
  target <- maybeToEither ("Relationship not found: " <> relId) $
            lookup relId rels

  -- Resolve relative path: ppt/slides/slide1.xml
  let slidePath' = "ppt/" <> T.unpack target

  -- Load and parse slide XML
  slideElem <- loadXMLFromArchive archive slidePath'

  -- Load slide-specific relationships
  slideRelsPath <- getPresentationRelsPath archive slidePath'
  slideRels' <- loadRelationships archive slideRelsPath

  return $ PptxSlide sid slidePath' slideElem slideRels'

-- | Helper: Maybe a -> Either Text a
maybeToEither :: Text -> Maybe a -> Either Text a
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x
