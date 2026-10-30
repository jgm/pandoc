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
import qualified System.FilePath.Posix as Posix
import Text.Pandoc.Readers.OOXML.Shared
import Text.Pandoc.Shared (collapseFilePath)
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
  , slideNotes   :: Maybe Element   -- The parsed p:notes element, if any
  , slideLayout  :: Maybe Element   -- The slide's p:sldLayout
  , slideMaster  :: Maybe Element   -- Its layout's p:sldMaster
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

  -- Follow the slide's own notesSlide relationship: notesSlideN.xml need
  -- not belong to slideN.xml. A missing or unreadable notes part means
  -- the slide has no notes; it never fails the conversion.
  let notesElem = do
        notesTarget <- findRelTargetByType archive slideRelsPath "notesSlide"
        either (const Nothing) Just $
          loadXMLFromArchive archive (resolveTarget slidePath' notesTarget)

  -- The slide's layout and master, which the text of its placeholders
  -- inherits from; either may be missing.
  let related source typeName = do
        t <- findRelTargetByType archive (relsPathOf source) typeName
        let path = resolveTarget source t
        el <- either (const Nothing) Just $ loadXMLFromArchive archive path
        return (path, el)
      layout = related slidePath' "slideLayout"
      master = layout >>= \(layoutPath, _) -> related layoutPath "slideMaster"

  return $ PptxSlide sid slidePath' slideElem slideRels' notesElem
                     (snd <$> layout) (snd <$> master)

-- | The path of a part's relationships
relsPathOf :: FilePath -> FilePath
relsPathOf path =
  let (dir, file) = splitFileName path
   in dir ++ "/_rels/" ++ file ++ ".rels"

-- | Find the target of the first relationship in a .rels part whose Type
-- ends with the given name, such as @notesSlide@.
findRelTargetByType :: Archive -> FilePath -> Text -> Maybe Text
findRelTargetByType archive relsPath typeName = do
  entry <- findEntryByPath relsPath archive
  relsElem <- either (const Nothing) Just $ parseXMLFromEntry entry
  rel <- find hasType (onlyElems $ elContent relsElem)
  findAttr (unqual "Target") rel
  where
    hasType el = maybe False (("/" <> typeName) `T.isSuffixOf`) $
                 findAttr (unqual "Type") el

-- | Resolve a relationship target to an archive path. A target starting
-- with @/@ is relative to the package root; any other target is relative
-- to the folder of the part that holds the relationship.
resolveTarget :: FilePath -> Text -> FilePath
resolveTarget source target =
  case T.unpack target of
    '/':path -> path
    path -> collapseFilePath (Posix.takeDirectory source Posix.</> path)

-- | Helper: Maybe a -> Either Text a
maybeToEither :: Text -> Maybe a -> Either Text a
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x
