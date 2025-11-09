{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Pptx.SmartArt
   Copyright   : © 2025 Anton Antic
   License     : GNU GPL, version 2 or above

   Maintainer  : Anton Antic <anton@everworker.ai>
   Stability   : alpha
   Portability : portable

SmartArt diagram parsing and text extraction for PPTX.
-}
module Text.Pandoc.Readers.Pptx.SmartArt
  ( PptxDiagram(..)
  , parseDiagram
  , diagramToBlocks
  ) where

import Codec.Archive.Zip (Archive, findEntryByPath, fromEntry)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.Text (Text)
import Text.Pandoc.Definition
import Text.Pandoc.Readers.OOXML.Shared
import Text.Pandoc.XML.Light

-- | SmartArt diagram data
data PptxDiagram = PptxDiagram
  { diagramType :: Text               -- Layout type (chevron, cycle, etc.)
  , diagramNodes :: [(Text, [Text])]  -- (nodeText, childTexts)
  } deriving (Show)

-- | Parse SmartArt diagram from relationship IDs
parseDiagram :: Archive
             -> [(Text, Text)]  -- Slide relationships
             -> Text            -- data relationship ID
             -> Text            -- layout relationship ID
             -> Either Text PptxDiagram
parseDiagram archive rels dataRelId layoutRelId = do
  -- Resolve relationships to file paths
  dataTarget <- maybeToEither ("Relationship not found: " <> dataRelId) $
                lookup dataRelId rels
  layoutTarget <- maybeToEither ("Relationship not found: " <> layoutRelId) $
                  lookup layoutRelId rels

  -- Resolve relative paths (diagrams are in ../diagrams/ from slides/)
  let dataPath = resolveDiagramPath dataTarget
      layoutPath = resolveDiagramPath layoutTarget

  -- Load XML files
  dataElem <- loadXMLFromArchive archive dataPath
  layoutElem <- loadXMLFromArchive archive layoutPath

  -- Extract layout type
  layoutType <- extractLayoutType layoutElem

  -- Extract text nodes with hierarchy
  nodes <- extractDiagramNodes dataElem

  return $ PptxDiagram layoutType nodes

-- | Resolve diagram path (handle ../diagrams/ relative paths)
resolveDiagramPath :: Text -> FilePath
resolveDiagramPath target =
  if "../diagrams/" `T.isPrefixOf` target
    then "ppt/diagrams/" ++ T.unpack (T.drop 12 target)  -- "../diagrams/" = 12 chars
    else T.unpack target

-- | Load XML from archive
loadXMLFromArchive :: Archive -> FilePath -> Either Text Element
loadXMLFromArchive archive path =
  case findEntryByPath path archive of
    Nothing -> Left $ "File not found in archive: " <> T.pack path
    Just entry ->
      let xmlBytes = fromEntry entry
          lazyText = TL.decodeUtf8 xmlBytes
       in parseXMLElement lazyText

-- | Extract layout type from layout XML
extractLayoutType :: Element -> Either Text Text
extractLayoutType layoutElem = do
  -- Look for uniqueId attribute: "urn:.../layout/chevron2"
  case findAttr (unqual "uniqueId") layoutElem of
    Just uid ->
      -- Extract last part after last /
      let layoutName = T.takeWhileEnd (/= '/') uid
       in Right layoutName
    Nothing ->
      -- Fallback: look for title
      case findChildByName ns "dgm" "title" layoutElem >>=
           findAttr (unqual "val") of
        Just title -> Right title
        Nothing -> Right "unknown"
  where
    ns = elemToNameSpaces layoutElem

-- | Extract text nodes from diagram data
extractDiagramNodes :: Element -> Either Text [(Text, [Text])]
extractDiagramNodes dataElem = do
  let ns = elemToNameSpaces dataElem

  -- Find point list
  ptLst <- maybeToEither "Missing dgm:ptLst" $
           findChildByName ns "dgm" "ptLst" dataElem

  let ptElems = findChildrenByName ns "dgm" "pt" ptLst

  -- Build node map: modelId → text
  let nodeMap = M.fromList $ mapMaybe (extractNodeText ns) ptElems

  -- Parse connections
  let cxnLst = findChildByName ns "dgm" "cxnLst" dataElem
      connections = maybe [] (parseConnections ns) cxnLst

  -- Build parent-child map
  let parentMap = buildParentMap connections

  -- Find parent nodes (nodes that have children)
  let parentIds = M.keys parentMap

  -- Build hierarchy - only show nodes that are parents
  -- (children are shown under their parents)
  let hierarchy = map (buildNodeWithChildren nodeMap parentMap) parentIds
      -- Filter out nodes with empty text (presentation nodes)
      validHierarchy = filter (\(nodeText, _) -> not $ T.null nodeText) hierarchy

  return validHierarchy

-- | Extract text from a point element (returns Nothing if no text)
extractNodeText :: NameSpaces -> Element -> Maybe (Text, Text)
extractNodeText ns ptElem = do
  modelId <- findAttr (unqual "modelId") ptElem

  -- Extract text from dgm:t element (which contains a:p/a:r/a:t)
  let text = case findChildByName ns "dgm" "t" ptElem of
        Just tElem ->
          -- Recursively get ALL text content from all descendants
          getAllText tElem
        Nothing -> ""

  -- Only return nodes with actual text
  if T.null (T.strip text)
    then Nothing
    else return (modelId, text)

-- | Connection between nodes
data Connection = Connection
  { connType :: Text
  , connSrc  :: Text
  , connDest :: Text
  } deriving (Show)

-- | Parse connections
parseConnections :: NameSpaces -> Element -> [Connection]
parseConnections ns cxnLst =
  let cxnElems = findChildrenByName ns "dgm" "cxn" cxnLst
   in mapMaybe (parseConnection ns) cxnElems

parseConnection :: NameSpaces -> Element -> Maybe Connection
parseConnection _ns cxnElem = do
  let cxnType = maybe "" id $ findAttr (unqual "type") cxnElem  -- Empty if no type
  srcId <- findAttr (unqual "srcId") cxnElem
  destId <- findAttr (unqual "destId") cxnElem
  return $ Connection cxnType srcId destId

-- | Build parent-child map from connections
-- Use connections WITHOUT a type attribute (these are the data hierarchy)
buildParentMap :: [Connection] -> M.Map Text [Text]
buildParentMap connections =
  let dataConnections = filter (\c -> T.null (connType c)) connections
   in foldr addConn M.empty dataConnections
  where
    addConn conn m = M.insertWith (++) (connSrc conn) [connDest conn] m

-- | Build node with its children
buildNodeWithChildren :: M.Map Text Text -> M.Map Text [Text] -> Text -> (Text, [Text])
buildNodeWithChildren nodeMap parentMap nodeId =
  let nodeText = M.findWithDefault "" nodeId nodeMap
      childIds = M.findWithDefault [] nodeId parentMap
      -- Only include children that have text
      childTexts = filter (not . T.null) $
                   map (\cid -> M.findWithDefault "" cid nodeMap) childIds
   in (nodeText, childTexts)

-- | Convert diagram to Pandoc blocks
diagramToBlocks :: PptxDiagram -> [Block]
diagramToBlocks diagram =
  let nodes = diagramNodes diagram
      layoutType = diagramType diagram

      -- Build content blocks
      contentBlocks = concatMap nodeToBlocks nodes

   in [Div ("", ["smartart", layoutType], [("layout", layoutType)])
           contentBlocks]

-- | Convert node to blocks
nodeToBlocks :: (Text, [Text]) -> [Block]
nodeToBlocks (nodeText, childTexts) =
  if null childTexts
    then [Para [Strong [Str nodeText]]]
    else [ Para [Strong [Str nodeText]]
         , BulletList [[Plain [Str child]] | child <- childTexts]
         ]

-- | Recursively extract all text from an element and its descendants
getAllText :: Element -> Text
getAllText el =
  let textFromContent (Text cdata) = cdData cdata
      textFromContent (Elem e) = getAllText e
      textFromContent _ = ""
      texts = map textFromContent (elContent el)
   in T.unwords $ filter (not . T.null) texts

-- Helper functions
maybeToEither :: Text -> Maybe a -> Either Text a
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x
