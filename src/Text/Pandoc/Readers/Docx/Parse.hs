{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
 Module : Text.Pandoc.Readers.Docx.Parse
 Copyright : Copyright (C) 2014-2020 Jesse Rosenthal
                           2019 Nikolay Yakimov <root@livid.pp.ru>
 License : GNU GPL, version 2 or above

 Maintainer : Jesse Rosenthal <jrosenthal@jhu.edu>
 Stability : alpha
 Portability : portable

Conversion of docx archive into Docx haskell type
-}

module Text.Pandoc.Readers.Docx.Parse ( Docx(..)
                                      , Document(..)
                                      , Body(..)
                                      , BodyPart(..)
                                      , TblLook(..)
                                      , Extent
                                      , ParPart(..)
                                      , Run(..)
                                      , RunElem(..)
                                      , Notes
                                      , Numbering
                                      , Relationship
                                      , Media
                                      , RunStyle(..)
                                      , VertAlign(..)
                                      , ParIndentation(..)
                                      , Justification(..)
                                      , ParagraphStyle(..)
                                      , ParStyle
                                      , CharStyle(cStyleData)
                                      , Row(..)
                                      , TblHeader(..)
                                      , Align(..)
                                      , Cell(..)
                                      , VMerge(..)
                                      , TrackedChange(..)
                                      , ChangeType(..)
                                      , ChangeInfo(..)
                                      , FieldInfo(..)
                                      , IndexEntry(..)
                                      , Level(..)
                                      , ParaStyleName
                                      , CharStyleName
                                      , FromStyleName(..)
                                      , HasStyleName(..)
                                      , HasParentStyle(..)
                                      , archiveToDocx
                                      , archiveToDocxWithWarnings
                                      , getStyleNames
                                      , pHeading
                                      , pStyleIndentation
                                      , constructBogusParStyleData
                                      , leftBiasedMergeRunStyle
                                      , rowsToRowspans
                                      , extractTarget
                                      ) where
import Text.Pandoc.Readers.Docx.Parse.Styles
import Codec.Archive.Zip
import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bits ((.|.))
import qualified Data.ByteString.Lazy as B
import Data.Char (chr, ord, readLitChar)
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import System.FilePath
import Text.Pandoc.Readers.Docx.Util
import Text.Pandoc.Readers.Docx.Fields
import Text.Pandoc.Shared (filteredFilesFromArchive, safeRead)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.TeXMath (Exp)
import Text.TeXMath.Readers.OMML (readOMML)
import Text.Pandoc.Readers.Docx.Symbols (symbolMap, Font(..), textToFont)
import Text.Pandoc.XML.Light
    ( filterChild,
      findElement,
      strContent,
      showElement,
      findAttr,
      filterChild,
      filterChildrenName,
      filterElementName,
      filterElementsName,
      lookupAttrBy,
      parseXMLElement,
      elChildren,
      QName(QName, qName),
      Content(Elem),
      Element(..))

data ReaderEnv = ReaderEnv { envNotes         :: Notes
                           , envComments      :: Comments
                           , envNumbering     :: Numbering
                           , envRelationships :: [Relationship]
                           , envMedia         :: Media
                           , envFont          :: Maybe Font
                           , envCharStyles    :: CharStyleMap
                           , envParStyles     :: ParStyleMap
                           , envLocation      :: DocumentLocation
                           , envDocXmlPath    :: FilePath
                           }
               deriving Show

data ReaderState = ReaderState { stateWarnings :: [T.Text]
                               , stateFldCharState :: [FldCharState]
                               }
                 deriving Show

data FldCharState = FldCharOpen
                  | FldCharFieldInfo T.Text
                  | FldCharContent FieldInfo [ParPart]
                  deriving (Show)

data DocxError = DocxError
               | WrongElem
               deriving Show

type D = ExceptT DocxError (ReaderT ReaderEnv (State ReaderState))

runD :: D a -> ReaderEnv -> ReaderState -> (Either DocxError a, ReaderState)
runD dx re rs = runState (runReaderT (runExceptT dx) re) rs

maybeToD :: Maybe a -> D a
maybeToD (Just a) = return a
maybeToD Nothing  = throwError DocxError

eitherToD :: Either a b -> D b
eitherToD (Right b) = return b
eitherToD (Left _)  = throwError DocxError

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

-- This is similar to `mapMaybe`: it maps a function returning the D
-- monad over a list, and only keeps the non-erroring return values.
mapD :: (a -> D b) -> [a] -> D [b]
mapD f xs =
  let handler x = (f x >>= (\y-> return [y])) `catchError` (\_ -> return [])
  in
   concatMapM handler xs

isAltContentRun :: NameSpaces -> Element -> Bool
isAltContentRun ns element
  | isElem ns "w" "r" element
  , Just _altContentElem <- findChildByName ns "mc" "AlternateContent" element
  = True
  | otherwise
  = False

-- Elements such as <w:shape> are not always preferred
-- to be unwrapped. Only if they are part of an AlternateContent
-- element, they should be unwrapped.
-- This strategy prevents VML images breaking.
unwrapAlternateContentElement :: NameSpaces -> Element -> [Element]
unwrapAlternateContentElement ns element
  | isElem ns "mc" "AlternateContent" element
  || isElem ns "mc" "Fallback" element
  || isElem ns "w" "pict" element
  || isElem ns "v" "group" element
  || isElem ns "v" "rect" element
  || isElem ns "v" "roundrect" element
  || isElem ns "v" "shape" element
  || isElem ns "v" "textbox" element
  || isElem ns "w" "txbxContent" element
  = concatMap (unwrapAlternateContentElement ns) (elChildren element)
  | otherwise
  = unwrapElement ns element

unwrapElement :: NameSpaces -> Element -> [Element]
unwrapElement ns element
  | isElem ns "w" "sdt" element
  , Just sdtContent <- findChildByName ns "w" "sdtContent" element
  = concatMap (unwrapElement ns) (elChildren sdtContent)
  | isElem ns "w" "r" element
  , Just alternateContentElem <- findChildByName ns "mc" "AlternateContent" element
  = unwrapAlternateContentElement ns alternateContentElem
  | isElem ns "w" "smartTag" element
  = concatMap (unwrapElement ns) (elChildren element)
  | isElem ns "w" "p" element
  , Just (modified, altContentRuns) <- extractChildren element (isAltContentRun ns)
  = (unwrapElement ns modified) ++ concatMap (unwrapElement ns) altContentRuns
  | otherwise
  = [element{ elContent = concatMap (unwrapContent ns) (elContent element) }]

unwrapContent :: NameSpaces -> Content -> [Content]
unwrapContent ns (Elem element) = map Elem $ unwrapElement ns element
unwrapContent _ content = [content]

walkDocument :: NameSpaces -> Element -> Element
walkDocument ns element =
  element{ elContent = concatMap (unwrapContent ns) (elContent element) }

newtype Docx = Docx Document
          deriving Show

data Document = Document NameSpaces Body
          deriving Show

newtype Body = Body [BodyPart]
          deriving Show

type Media = [(FilePath, B.ByteString)]

type CharStyleMap = M.Map CharStyleId CharStyle

type ParStyleMap = M.Map ParaStyleId ParStyle

data Numbering = Numbering NameSpaces [Numb] [AbstractNumb]
                 deriving Show

data Numb = Numb T.Text T.Text [LevelOverride]
            deriving Show

--                                 ilvl    startOverride   lvl
data LevelOverride = LevelOverride T.Text (Maybe Integer) (Maybe Level)
  deriving Show

data AbstractNumb = AbstractNumb T.Text [Level]
                    deriving Show

--                 ilvl   format string  start
data Level = Level T.Text T.Text T.Text (Maybe Integer)
  deriving Show

data DocumentLocation = InDocument | InFootnote | InEndnote
                      deriving (Eq,Show)

data Relationship = Relationship DocumentLocation RelId Target
                  deriving Show

data Notes = Notes NameSpaces
             (Maybe (M.Map T.Text Element))
             (Maybe (M.Map T.Text Element))
           deriving Show

data Comments = Comments NameSpaces (M.Map T.Text Element)
              deriving Show

data ChangeType = Insertion | Deletion
                deriving Show

data ChangeInfo = ChangeInfo ChangeId Author (Maybe ChangeDate)
                deriving Show

data TrackedChange = TrackedChange ChangeType ChangeInfo
                   deriving Show

data Justification = JustifyBoth | JustifyLeft | JustifyRight | JustifyCenter
  deriving (Show, Eq)

data ParagraphStyle = ParagraphStyle { pStyle        :: [ParStyle]
                                     , indentation   :: Maybe ParIndentation
                                     , justification :: Maybe Justification
                                     , numbered      :: Bool
                                     , dropCap       :: Bool
                                     , pChange       :: Maybe TrackedChange
                                     , pBidi         :: Maybe Bool
                                     , pKeepNext     :: Bool
                                     }
                      deriving Show

defaultParagraphStyle :: ParagraphStyle
defaultParagraphStyle = ParagraphStyle { pStyle = []
                                       , indentation = Nothing
                                       , justification = Nothing
                                       , numbered = False
                                       , dropCap = False
                                       , pChange = Nothing
                                       , pBidi = Just False
                                       , pKeepNext = False
                                       }


data BodyPart = Paragraph ParagraphStyle [ParPart]
              | Heading Int ParaStyleName ParagraphStyle T.Text T.Text (Maybe Level)
                 [ParPart]
              | ListItem ParagraphStyle T.Text T.Text (Maybe Level) [ParPart]
              | Tbl (Maybe T.Text) T.Text TblGrid TblLook [Row]
              | Captioned ParagraphStyle [ParPart] BodyPart
              | HRule
              deriving Show

type TblGrid = [Integer]

newtype TblLook = TblLook {firstRowFormatting::Bool}
              deriving Show

defaultTblLook :: TblLook
defaultTblLook = TblLook{firstRowFormatting = False}

data Row = Row TblHeader [Cell] deriving Show

data TblHeader = HasTblHeader | NoTblHeader deriving (Show, Eq)

data Align = AlignDefault | AlignLeft | AlignRight | AlignCenter
  deriving (Show, Eq)

data Cell = Cell Align GridSpan VMerge [BodyPart]
            deriving Show

type GridSpan = Integer

data VMerge = Continue
            -- ^ This cell should be merged with the one above it
            | Restart
            -- ^ This cell should not be merged with the one above it
            deriving (Show, Eq)

rowsToRowspans :: [Row] -> [[(Int, Cell)]]
rowsToRowspans rows = let
  removeMergedCells = fmap (filter (\(_, Cell _ _ vmerge _) -> vmerge == Restart))
  in removeMergedCells (foldr f [] rows)
  where
    f :: Row -> [[(Int, Cell)]] -> [[(Int, Cell)]]
    f (Row _ cells) acc = let
      spans = g cells Nothing (listToMaybe acc)
      in spans : acc

    g :: [Cell] -- the current row
      -> Maybe Integer -- Number of columns left below
      -> Maybe [(Int, Cell)] -- (rowspan so far, cell) for the row below this one
      -> [(Int, Cell)] -- (rowspan so far, cell) for this row
    g cells _ Nothing = map (1,) cells
    g cells columnsLeftBelow (Just rowBelow) =
        case cells of
          [] -> []
          thisCell@(Cell _ thisGridSpan _ _) : restOfRow -> case rowBelow of
            [] -> map (1,) cells
            (spanSoFarBelow, Cell _ gridSpanBelow vmerge _) : _ ->
              let spanSoFar = case vmerge of
                    Restart -> 1
                    Continue -> 1 + spanSoFarBelow
                  columnsToDrop = thisGridSpan + (gridSpanBelow - fromMaybe gridSpanBelow columnsLeftBelow)
                  (newColumnsLeftBelow, restOfRowBelow) = dropColumns columnsToDrop rowBelow
              in (spanSoFar, thisCell) : g restOfRow (Just newColumnsLeftBelow) (Just restOfRowBelow)

    dropColumns :: Integer -> [(a, Cell)] -> (Integer, [(a, Cell)])
    dropColumns n [] = (n, [])
    dropColumns n cells@((_, Cell _ gridSpan _ _) : otherCells) =
      if n < gridSpan
      then (gridSpan - n, cells)
      else dropColumns (n - gridSpan) otherCells

leftBiasedMergeRunStyle :: RunStyle -> RunStyle -> RunStyle
leftBiasedMergeRunStyle a b = RunStyle
    { isBold = isBold a <|> isBold b
    , isBoldCTL = isBoldCTL a <|> isBoldCTL b
    , isItalic = isItalic a <|> isItalic b
    , isItalicCTL = isItalicCTL a <|> isItalicCTL b
    , isSmallCaps = isSmallCaps a <|> isSmallCaps b
    , isStrike = isStrike a <|> isStrike b
    , isRTL = isRTL a <|> isRTL b
    , isForceCTL = isForceCTL a <|> isForceCTL b
    , rHighlight = rHighlight a <|> rHighlight b
    , rVertAlign = rVertAlign a <|> rVertAlign b
    , rUnderline = rUnderline a <|> rUnderline b
    , rParentStyle = rParentStyle a
    }

-- (width, height) in EMUs
type Extent = Maybe (Double, Double)

data ParPart = PlainRun Run
             | ChangedRuns TrackedChange [ParPart]
             | CommentStart CommentId Author (Maybe CommentDate) [BodyPart]
             | CommentEnd CommentId
             | BookMark BookMarkId Anchor
             | InternalHyperLink Anchor [ParPart]
             | ExternalHyperLink URL [ParPart]
             | Drawing FilePath T.Text T.Text B.ByteString Extent -- title, alt
             | Chart                                              -- placeholder for now
             | Diagram                                            -- placeholder for now
             | PlainOMath [Exp]
             | OMathPara [Exp]
             | Field FieldInfo [ParPart]
             deriving Show

data Run = Run RunStyle [RunElem]
         | Footnote [BodyPart]
         | Endnote [BodyPart]
         | InlineDrawing FilePath T.Text T.Text B.ByteString Extent -- title, alt
         | InlineChart          -- placeholder
         | InlineDiagram        -- placeholder
           deriving Show

data RunElem = TextRun T.Text | LnBrk | Tab | SoftHyphen | NoBreakHyphen
             deriving Show

type Target = T.Text
type Anchor = T.Text
type URL = T.Text
type BookMarkId = T.Text
type RelId = T.Text
type ChangeId = T.Text
type CommentId = T.Text
type Author = T.Text
type ChangeDate = T.Text
type CommentDate = T.Text

archiveToDocx :: Archive -> Either DocxError Docx
archiveToDocx archive = fst <$> archiveToDocxWithWarnings archive

archiveToDocxWithWarnings :: Archive -> Either DocxError (Docx, [T.Text])
archiveToDocxWithWarnings archive = do
  docXmlPath <- case getDocumentXmlPath archive of
    Just fp -> Right fp
    Nothing -> Left DocxError
  let notes     = archiveToNotes archive
      comments  = archiveToComments archive
      numbering = archiveToNumbering archive
      rels      = archiveToRelationships archive docXmlPath
      media     = filteredFilesFromArchive archive filePathIsMedia
      (styles, parstyles) = archiveToStyles archive
      rEnv = ReaderEnv { envNotes = notes
                       , envComments = comments
                       , envNumbering = numbering
                       , envRelationships = rels
                       , envMedia = media
                       , envFont = Nothing
                       , envCharStyles = styles
                       , envParStyles = parstyles
                       , envLocation = InDocument
                       , envDocXmlPath = docXmlPath
                       }
      rState = ReaderState { stateWarnings = []
                           , stateFldCharState = []
                           }
      (eitherDoc, st) = runD (archiveToDocument archive) rEnv rState
  case eitherDoc of
    Right doc -> Right (Docx doc, stateWarnings st)
    Left e    -> Left e

parseXMLFromEntry :: Entry -> Maybe Element
parseXMLFromEntry entry =
  case parseXMLElement (UTF8.toTextLazy (fromEntry entry)) of
    Left _   -> Nothing
    Right el -> Just el

getDocumentXmlPath :: Archive -> Maybe FilePath
getDocumentXmlPath zf = do
  entry <- findEntryByPath "_rels/.rels" zf
  relsElem <- parseXMLFromEntry entry
  let rels = filterChildrenName (\n -> qName n == "Relationship") relsElem
  rel <- find (\e ->
                 case findAttr (QName "Type" Nothing Nothing) e of
                   Just u -> isNamespace "officeDocument" "relationships/officeDocument" u
                   Nothing -> False)
         rels
  fp <- findAttr (QName "Target" Nothing Nothing) rel
  -- sometimes there will be a leading slash, which windows seems to
  -- have trouble with.
  return $ case T.unpack fp of
    '/' : fp' -> fp'
    fp'       -> fp'

archiveToDocument :: Archive -> D Document
archiveToDocument zf = do
  docPath <- asks envDocXmlPath
  entry <- maybeToD $ findEntryByPath docPath zf
  docElem <- maybeToD $ parseXMLFromEntry entry
  let namespaces = elemToNameSpaces docElem
  bodyElem <- maybeToD $ findChildByName namespaces "w" "body" docElem
  let bodyElem' = walkDocument namespaces bodyElem
  body <- elemToBody namespaces bodyElem'
  return $ Document namespaces body

elemToBody :: NameSpaces -> Element -> D Body
elemToBody ns element | isElem ns "w" "body" element =
  Body . addCaptioned <$> mapD (elemToBodyPart ns) (elChildren element)
elemToBody _ _ = throwError WrongElem

archiveToStyles :: Archive -> (CharStyleMap, ParStyleMap)
archiveToStyles = archiveToStyles' getStyleId getStyleId

class HasParentStyle a where
  getParentStyle :: a -> Maybe a

instance HasParentStyle CharStyle where
  getParentStyle = rParentStyle . cStyleData

instance HasParentStyle ParStyle where
  getParentStyle = psParentStyle

getStyleNames :: (Functor t, HasStyleName a) => t a -> t (StyleName a)
getStyleNames = fmap getStyleName

constructBogusParStyleData :: ParaStyleName -> ParStyle
constructBogusParStyleData stName = ParStyle
  { headingLev = Nothing
  , indent = Nothing
  , numInfo = Nothing
  , psParentStyle = Nothing
  , pStyleName = stName
  , pStyleId = ParaStyleId . T.filter (/=' ') . fromStyleName $ stName
  }

archiveToNotes :: Archive -> Notes
archiveToNotes zf =
  let fnElem = findEntryByPath "word/footnotes.xml" zf
               >>= parseXMLFromEntry
      enElem = findEntryByPath "word/endnotes.xml" zf
               >>= parseXMLFromEntry
      fn_namespaces = maybe mempty elemToNameSpaces fnElem
      en_namespaces = maybe mempty elemToNameSpaces enElem
      ns = M.union fn_namespaces en_namespaces
      fn = fnElem >>= elemToNotes ns "footnote" . walkDocument ns
      en = enElem >>= elemToNotes ns "endnote" . walkDocument ns
  in
   Notes ns fn en

archiveToComments :: Archive -> Comments
archiveToComments zf =
  let cmtsElem = findEntryByPath "word/comments.xml" zf
               >>= parseXMLFromEntry
      cmts_namespaces = maybe mempty elemToNameSpaces cmtsElem
      cmts = elemToComments cmts_namespaces . walkDocument cmts_namespaces <$>
               cmtsElem
  in
    case cmts of
      Just c  -> Comments cmts_namespaces c
      Nothing -> Comments cmts_namespaces M.empty

filePathToRelType :: FilePath -> FilePath -> Maybe DocumentLocation
filePathToRelType "word/_rels/footnotes.xml.rels" _ = Just InFootnote
filePathToRelType "word/_rels/endnotes.xml.rels" _ = Just InEndnote
-- -- to see if it's a documentPath, we have to check against the dynamic
-- -- docPath specified in "_rels/.rels"
filePathToRelType path docXmlPath =
  if path == "word/_rels/" ++ takeFileName docXmlPath ++ ".rels"
  then Just InDocument
  else Nothing

relElemToRelationship :: FilePath -> DocumentLocation -> Element
                      -> Maybe Relationship
relElemToRelationship fp relType element | qName (elName element) == "Relationship" =
  do
    relId <- findAttr (QName "Id" Nothing Nothing) element
    target <- findAttr (QName "Target" Nothing Nothing) element
    -- target may be relative (media/image1.jpeg) or absolute
    -- (/word/media/image1.jpeg); we need to relativize it (see #7374)
    let frontOfFp = T.pack $ takeWhile (/= '_') fp
    let target' = fromMaybe target $
           T.stripPrefix frontOfFp $ T.dropWhile (== '/') target
    return $ Relationship relType relId target'
relElemToRelationship _ _ _ = Nothing

extractTarget :: Element -> Maybe Target
extractTarget element = do (Relationship _ _ target) <- relElemToRelationship "word/" InDocument element
                           return target

filePathToRelationships :: Archive -> FilePath -> FilePath ->  [Relationship]
filePathToRelationships ar docXmlPath fp
  | Just relType <- filePathToRelType fp docXmlPath
  , Just entry <- findEntryByPath fp ar
  , Just relElems <- parseXMLFromEntry entry =
  mapMaybe (relElemToRelationship fp relType) $ elChildren relElems
filePathToRelationships _ _ _ = []

archiveToRelationships :: Archive -> FilePath -> [Relationship]
archiveToRelationships archive docXmlPath =
  concatMap (filePathToRelationships archive docXmlPath) $ filesInArchive archive

filePathIsMedia :: FilePath -> Bool
filePathIsMedia fp =
  "media" `elem` splitDirectories (takeDirectory fp)

lookupLevel :: T.Text -> T.Text -> Numbering -> Maybe Level
lookupLevel numId ilvl (Numbering _ numbs absNumbs) = do
  (absNumId, ovrrides) <- lookup numId $
                          map (\(Numb nid absnumid ovrRides) -> (nid, (absnumid, ovrRides))) numbs
  lvls <- lookup absNumId $
    map (\(AbstractNumb aid ls) -> (aid, ls)) absNumbs
  -- this can be a maybe, so we do a let
  let lvlOverride = lookup ilvl $
                    map (\lo@(LevelOverride ilvl' _ _) -> (ilvl', lo)) ovrrides
  case lvlOverride of
    Just (LevelOverride _ _ (Just lvl')) -> Just lvl'
    Just (LevelOverride _ (Just strt) _) ->
      lookup ilvl $ map (\(Level i fmt s _) -> (i, Level i fmt s (Just strt))) lvls
    _ ->
      lookup ilvl $ map (\l@(Level i _ _ _) -> (i, l)) lvls

loElemToLevelOverride :: NameSpaces -> Element -> Maybe LevelOverride
loElemToLevelOverride ns element
  | isElem ns "w" "lvlOverride" element = do
      ilvl <- findAttrByName ns "w" "ilvl" element
      let startOverride = findChildByName ns "w" "startOverride" element
                          >>= findAttrByName ns "w" "val"
                          >>= stringToInteger
          lvl = findChildByName ns "w" "lvl" element
                >>= levelElemToLevel ns
      return $ LevelOverride ilvl startOverride lvl
loElemToLevelOverride _ _ = Nothing

numElemToNum :: NameSpaces -> Element -> Maybe Numb
numElemToNum ns element
  | isElem ns "w" "num" element = do
      numId <- findAttrByName ns "w" "numId" element
      absNumId <- findChildByName ns "w" "abstractNumId" element
                  >>= findAttrByName ns "w" "val"
      let lvlOverrides = mapMaybe
                         (loElemToLevelOverride ns)
                         (findChildrenByName ns "w" "lvlOverride" element)
      return $ Numb numId absNumId lvlOverrides
numElemToNum _ _ = Nothing

absNumElemToAbsNum :: NameSpaces -> Element -> Maybe AbstractNumb
absNumElemToAbsNum ns element
  | isElem ns "w" "abstractNum" element = do
      absNumId <- findAttrByName ns "w" "abstractNumId" element
      let levelElems = findChildrenByName ns "w" "lvl" element
          levels = mapMaybe (levelElemToLevel ns) levelElems
      return $ AbstractNumb absNumId levels
absNumElemToAbsNum _ _ = Nothing

levelElemToLevel :: NameSpaces -> Element -> Maybe Level
levelElemToLevel ns element
  | isElem ns "w" "lvl" element = do
      ilvl <- findAttrByName ns "w" "ilvl" element
      fmt <- findChildByName ns "w" "numFmt" element
             >>= findAttrByName ns "w" "val"
      txt <- findChildByName ns "w" "lvlText" element
             >>= findAttrByName ns "w" "val"
      let start = findChildByName ns "w" "start" element
                  >>= findAttrByName ns "w" "val"
                  >>= stringToInteger
      return (Level ilvl fmt txt start)
levelElemToLevel _ _ = Nothing

archiveToNumbering' :: Archive -> Maybe Numbering
archiveToNumbering' zf =
  case findEntryByPath "word/numbering.xml" zf of
    Nothing -> Just $ Numbering mempty [] []
    Just entry -> do
      numberingElem <- parseXMLFromEntry entry
      let namespaces = elemToNameSpaces numberingElem
          numElems = findChildrenByName namespaces "w" "num" numberingElem
          absNumElems = findChildrenByName namespaces "w" "abstractNum" numberingElem
          nums = mapMaybe (numElemToNum namespaces) numElems
          absNums = mapMaybe (absNumElemToAbsNum namespaces) absNumElems
      return $ Numbering namespaces nums absNums

archiveToNumbering :: Archive -> Numbering
archiveToNumbering archive =
  fromMaybe (Numbering mempty [] []) (archiveToNumbering' archive)

elemToNotes :: NameSpaces -> Text -> Element -> Maybe (M.Map T.Text Element)
elemToNotes ns notetype element
  | isElem ns "w" (notetype <> "s") element =
      let pairs = mapMaybe
                  (\e -> findAttrByName ns "w" "id" e >>=
                         (\a -> Just (a, e)))
                  (findChildrenByName ns "w" notetype element)
      in
       Just $
       M.fromList pairs
elemToNotes _ _ _ = Nothing

elemToComments :: NameSpaces -> Element -> M.Map T.Text Element
elemToComments ns element
  | isElem ns "w" "comments" element =
      let pairs = mapMaybe
                  (\e -> findAttrByName ns "w" "id" e >>=
                         (\a -> Just (a, e)))
                  (findChildrenByName ns "w" "comment" element)
      in
       M.fromList pairs
elemToComments _ _ = M.empty


---------------------------------------------
---------------------------------------------

elemToTblGrid :: NameSpaces -> Element -> D TblGrid
elemToTblGrid ns element | isElem ns "w" "tblGrid" element =
  let cols = findChildrenByName ns "w" "gridCol" element
  in
   mapD (\e -> maybeToD (findAttrByName ns "w" "w" e >>= stringToInteger))
   cols
elemToTblGrid _ _ = throwError WrongElem

elemToTblLook :: NameSpaces -> Element -> D TblLook
elemToTblLook ns element | isElem ns "w" "tblLook" element =
  let firstRow = findAttrByName ns "w" "firstRow" element
      val = findAttrByName ns "w" "val" element
      firstRowFmt =
        case firstRow of
          Just "1" -> True
          Just  _  -> False
          Nothing -> case val of
            Just bitMask -> testBitMask bitMask 0x020
            Nothing      -> False
  in
   return TblLook{firstRowFormatting = firstRowFmt}
elemToTblLook _ _ = throwError WrongElem

elemToRow :: NameSpaces -> Element -> D Row
elemToRow ns element | isElem ns "w" "tr" element =
  do
    let cellElems = findChildrenByName ns "w" "tc" element
    cells <- mapD (elemToCell ns) cellElems
    let hasTblHeader = maybe NoTblHeader (const HasTblHeader)
          (findChildByName ns "w" "trPr" element
           >>= findChildByName ns "w" "tblHeader")
    return $ Row hasTblHeader cells
elemToRow _ _ = throwError WrongElem

elemToCell :: NameSpaces -> Element -> D Cell
elemToCell ns element | isElem ns "w" "tc" element =
  do
    let properties = findChildByName ns "w" "tcPr" element
    let gridSpan = properties
                     >>= findChildByName ns "w" "gridSpan"
                     >>= findAttrByName ns "w" "val"
                     >>= stringToInteger
    let vMerge = case properties >>= findChildByName ns "w" "vMerge" of
                   Nothing -> Restart
                   Just e ->
                     fromMaybe Continue $ do
                       s <- findAttrByName ns "w" "val" e
                       case s of
                         "continue" -> Just Continue
                         "restart" -> Just Restart
                         _ -> Nothing
    cellContents <- mapD (elemToBodyPart ns) (elChildren element)
    let align = case cellContents of -- take alignment from first paragraph
                  Paragraph pstyle _ : _ ->
                    case justification pstyle of
                      Just JustifyBoth -> AlignLeft
                      Just JustifyLeft -> AlignLeft
                      Just JustifyRight -> AlignRight
                      Just JustifyCenter -> AlignCenter
                      Nothing -> AlignDefault
                  _ -> AlignDefault
    return $ Cell align (fromMaybe 1 gridSpan) vMerge cellContents
elemToCell _ _ = throwError WrongElem

testBitMask :: Text -> Int -> Bool
testBitMask bitMaskS n =
  case (reads ("0x" ++ T.unpack bitMaskS) :: [(Int, String)]) of
    []            -> False
    ((n', _) : _) -> (n' .|. n) /= 0

pHeading :: ParagraphStyle -> Maybe (ParaStyleName, Int)
pHeading = getParStyleField headingLev . pStyle

pNumInfo :: ParagraphStyle -> Maybe (T.Text, T.Text)
pNumInfo = getParStyleField numInfo . pStyle

mkListItem :: ParagraphStyle -> Text -> Text -> [ParPart] -> D BodyPart
mkListItem parstyle numId lvl parparts = do
  lvlInfo <- lookupLevel numId lvl <$> asks envNumbering
  return $ ListItem parstyle numId lvl lvlInfo parparts

pStyleIndentation :: ParagraphStyle -> Maybe ParIndentation
pStyleIndentation style = (getParStyleField indent . pStyle) style

addCaptioned :: [BodyPart] -> [BodyPart]
addCaptioned [] = []
addCaptioned (Paragraph parstyle parparts : x : xs)
  | hasCaptionStyle parstyle
  , isCaptionable x
    = Captioned parstyle parparts x : addCaptioned xs
addCaptioned (x : Paragraph parstyle parparts : xs)
  | hasCaptionStyle parstyle
  , not (pKeepNext parstyle)
  , isCaptionable x
    = Captioned parstyle parparts x : addCaptioned xs
addCaptioned (x:xs) = x : addCaptioned xs

isCaptionable :: BodyPart -> Bool
isCaptionable (Paragraph _ [Drawing{}]) = True
isCaptionable (Tbl{}) = True
isCaptionable _ = False

elemToBodyPart :: NameSpaces -> Element -> D BodyPart
elemToBodyPart ns element
  | isElem ns "m" "oMathPara" element = do
      expsLst <- eitherToD $ readOMML $ showElement element
      parstyle <- elemToParagraphStyle ns element
                  <$> asks envParStyles
                  <*> asks envNumbering
      return $ Paragraph parstyle [OMathPara expsLst]
elemToBodyPart ns element
  | isElem ns "w" "p" element
  , Just (numId, lvl) <- getNumInfo ns element = do
    lvlInfo <- lookupLevel numId lvl <$> asks envNumbering
    parstyle <- elemToParagraphStyle ns element
                <$> asks envParStyles
                <*> asks envNumbering
    parparts <- mconcat <$> mapD (elemToParPart ns) (elChildren element)
    case pHeading parstyle of
      Nothing -> mkListItem parstyle numId lvl parparts
      Just (parstylename, lev)
        -> return $ Heading lev parstylename parstyle numId lvl lvlInfo parparts
elemToBodyPart ns element
  | isElem ns "w" "p" element
  , [Elem ppr] <- elContent element
  , isElem ns "w" "pPr" ppr
  , [Elem pbdr] <- elContent ppr
  , isElem ns "w" "pBdr" pbdr
    = return HRule
      -- for this style of horizontal rule, see
      -- https://support.microsoft.com/en-us/office/insert-a-horizontal-line-9bf172f6-5908-4791-9bb9-2c952197b1a9
elemToBodyPart ns element -- pandoc-style horizontal rule
  | isElem ns "w" "p" element
  , [Elem r] <- elContent element
  , isElem ns "w" "r" r
  , [Elem pict] <- elContent r
  , isElem ns "w" "pict" pict
  , [Elem rect] <- elContent pict
  , isElem ns "v" "rect" rect
    = return HRule
elemToBodyPart ns element
  | isElem ns "w" "p" element = do
      parstyle <- elemToParagraphStyle ns element
                  <$> asks envParStyles
                  <*> asks envNumbering

      let children =
            (if hasCaptionStyle parstyle
                then stripCaptionLabel
                else id) (elChildren element)

      parparts' <- mconcat <$> mapD (elemToParPart ns) children
      fldCharState <- gets stateFldCharState
      modify $ \st -> st {stateFldCharState = emptyFldCharContents fldCharState}
      -- Word uses list enumeration for numbered headings, so we only
      -- want to infer a list from the styles if it is NOT a heading.
      let parparts = parparts' ++ openFldCharsToParParts fldCharState
      case pHeading parstyle of
        Nothing | Just (numId, lvl) <- pNumInfo parstyle -> do
                    mkListItem parstyle numId lvl parparts
        Just (parstylename, lev) -> do
          let (numId, lvl) = fromMaybe ("","") $ pNumInfo parstyle
          lvlInfo <- lookupLevel numId lvl <$> asks envNumbering
          return $ Heading lev parstylename parstyle numId lvl lvlInfo parparts
        Nothing -> return $ Paragraph parstyle parparts
elemToBodyPart ns element
  | isElem ns "w" "tbl" element = do
    let tblProperties = findChildByName ns "w" "tblPr" element
        caption = fromMaybe "" $ tblProperties
                   >>= findChildByName ns "w" "tblCaption"
                   >>= findAttrByName ns "w" "val"
        description = fromMaybe "" $ tblProperties
                       >>= findChildByName ns "w" "tblDescription"
                       >>= findAttrByName ns "w" "val"
        mbstyle = tblProperties
                       >>= findChildByName ns "w" "tblStyle"
                       >>= findAttrByName ns "w" "val"
        grid' = case findChildByName ns "w" "tblGrid" element of
          Just g  -> elemToTblGrid ns g
          Nothing -> return []
        tblLook' = case findChildByName ns "w" "tblPr" element >>=
                          findChildByName ns "w" "tblLook"
                     of
                       Just l  -> elemToTblLook ns l
                       Nothing -> return defaultTblLook

    grid <- grid'
    tblLook <- tblLook'
    rows <- mapD (elemToRow ns) (elChildren element)
    return $ Tbl mbstyle (caption <> description) grid tblLook rows
elemToBodyPart _ _ = throwError WrongElem

lookupRelationship :: DocumentLocation -> RelId -> [Relationship] -> Maybe Target
lookupRelationship docLocation relid rels =
  lookup (docLocation, relid) pairs
  where
    pairs = map (\(Relationship loc relid' target) -> ((loc, relid'), target)) rels

openFldCharsToParParts :: [FldCharState] -> [ParPart]
openFldCharsToParParts [] = []
openFldCharsToParParts (FldCharContent info children : ancestors) = case openFldCharsToParParts ancestors of
  Field parentInfo siblings : _ -> [Field parentInfo $ siblings ++ [Field info $ reverse children]]
  _ -> [Field info $ reverse children]
openFldCharsToParParts (_ : ancestors) = openFldCharsToParParts ancestors

emptyFldCharContents :: [FldCharState] -> [FldCharState]
emptyFldCharContents = map
  (\x -> case x of
    FldCharContent info _ -> FldCharContent info []
    _ -> x)

expandDrawingId :: T.Text -> D (FilePath, B.ByteString)
expandDrawingId s = do
  location <- asks envLocation
  target <- asks (fmap T.unpack . lookupRelationship location s . envRelationships)
  case target of
    Just filepath -> do
      media <- asks envMedia
      let filepath' = case filepath of
                        ('/':rest) -> rest
                        _ -> "word/" ++ filepath
      case lookup filepath' media of
        Just bs -> return (filepath, bs)
        Nothing -> throwError DocxError
    Nothing -> throwError DocxError

getTitleAndAlt :: NameSpaces -> Element -> (T.Text, T.Text)
getTitleAndAlt ns element =
  let mbDocPr = (findChildByName ns "wp" "inline" element <|>   -- Word
                 findChildByName ns "wp" "anchor" element) >>=  -- LibreOffice
                findChildByName ns "wp" "docPr"
      title = fromMaybe "" (mbDocPr >>= findAttrByName ns "" "title")
      alt = fromMaybe "" (mbDocPr >>= findAttrByName ns "" "descr")
  in (title, alt)

elemToParPart :: NameSpaces -> Element -> D [ParPart]
{-
The next one is a bit complicated. fldChar fields work by first
having a <w:fldChar fldCharType="begin"> in a run, then a run with
<w:instrText>, then a <w:fldChar fldCharType="separate"> run, then the
content runs, and finally a <w:fldChar fldCharType="end"> run. For
example (omissions and my comments in brackets):

      <w:r>
        [...]
        <w:fldChar w:fldCharType="begin"/>
      </w:r>
      <w:r>
        [...]
        <w:instrText xml:space="preserve"> HYPERLINK [hyperlink url] </w:instrText>
      </w:r>
      <w:r>
        [...]
        <w:fldChar w:fldCharType="separate"/>
      </w:r>
      <w:r w:rsidRPr=[...]>
        [...]
        <w:t>Foundations of Analysis, 2nd Edition</w:t>
      </w:r>
      <w:r>
        [...]
        <w:fldChar w:fldCharType="end"/>
      </w:r>

Note that there may be mulitple w:instrText elements in a row.
For example, you might first have ` XE "`, then `Kay, Alan`, then `"`.
The texts in all of them should be concatenated before it is processed!

So we do this in a number of steps. If we encounter the fldchar begin
tag, we start open a fldchar state variable (see state above). We add
the instrtext to it as FieldInfo. Then we close that and start adding
the children when we get to separate. Then when we get to end, we produce
the Field type with appropriate FieldInfo and ParParts.

Since there can be nested fields, the fldchar state needs to be a stack,
so we can have multiple fldchars open at the same time. When a fldchar is
closed, we either add the resulting field to its parent or we return it if
there is no parent.
-}
elemToParPart ns element
  | isElem ns "w" "r" element
  , Just fldChar <- findChildByName ns "w" "fldChar" element
  , Just fldCharType <- findAttrByName ns "w" "fldCharType" fldChar = do
      fldCharState <- gets stateFldCharState
      case fldCharState of
        _ | fldCharType == "begin" -> do
          modify $ \st -> st {stateFldCharState = FldCharOpen : fldCharState}
          return []
        FldCharFieldInfo t : ancestors | fldCharType == "separate" -> do
          info <- eitherToD $ parseFieldInfo t
          modify $ \st -> st {stateFldCharState = FldCharContent info [] : ancestors}
          return []
        -- Some fields have no content, e.g. index XE:
        FldCharFieldInfo t : ancestors | fldCharType == "end" -> do
          modify $ \st -> st {stateFldCharState = ancestors}
          info <- eitherToD $ parseFieldInfo t
          return [Field info []]
        [FldCharContent info children] | fldCharType == "end" -> do
          modify $ \st -> st {stateFldCharState = []}
          return [Field info $ reverse children]
        FldCharContent info children : FldCharContent parentInfo siblings : ancestors | fldCharType == "end" ->
          let parent = FldCharContent parentInfo $ (Field info (reverse children)) : siblings in do
            modify $ \st -> st {stateFldCharState = parent : ancestors}
            return []
        _ -> throwError WrongElem
elemToParPart ns element
  | isElem ns "w" "r" element
  , Just instrText <- findChildByName ns "w" "instrText" element = do
      fldCharState <- gets stateFldCharState
      case fldCharState of
        FldCharOpen : ancestors -> do
          modify $ \st -> st {stateFldCharState =
                               FldCharFieldInfo (strContent instrText) : ancestors}
          return []
        FldCharFieldInfo t : ancestors -> do
          modify $ \st -> st {stateFldCharState =
                               FldCharFieldInfo (t <> strContent instrText) :
                                ancestors}
          return []
        _ -> return []
{-
There is an open fldchar, so we calculate the element and add it to the
children. For this we need to first change the fldchar state to an empty
stack to avoid descendants of children simply being added to the state instead
of to their direct parent element. This would happen in the case of a
w:hyperlink element for example.
-}
elemToParPart ns element = do
  fldCharState <- gets stateFldCharState
  case fldCharState of
    FldCharContent info children : ancestors -> do
      modify $ \st -> st {stateFldCharState = []}
      parParts <- elemToParPart' ns element `catchError` \_ -> return []
      modify $ \st -> st{stateFldCharState = FldCharContent info (parParts ++ children) : ancestors}
      return []
    _ -> elemToParPart' ns element

elemToParPart' :: NameSpaces -> Element -> D [ParPart]
elemToParPart' ns element
  | isElem ns "w" "r" element
  , Just drawingElem <- findChildByName ns "w" "drawing" element
  , picElems <- filterElementsName
                 (matchQName "drawingml" "picture" (Just "pic") "pic") drawingElem
  = let (title, alt) = getTitleAndAlt ns drawingElem
        drawings = map (\el ->
                        ((findBlip el >>= findAttrByName ns "r" "embed"), el))
                       picElems
    in mapM (\case
                (Just s, el) -> do
                  (fp, bs) <- expandDrawingId s
                  let extent = elemToExtent el <|> elemToExtent element
                  return $ Drawing fp title alt bs extent
                (Nothing, _) -> throwError WrongElem)
            drawings
-- The two cases below are an attempt to deal with images in deprecated vml format.
-- Todo: check out title and attr for deprecated format.
elemToParPart' ns element
  | isElem ns "w" "r" element
  , Just _ <- findChildByName ns "w" "pict" element =
    let drawing = findElement (elemName ns "v" "imagedata") element
                  >>= findAttrByName ns "r" "id"
    in
     case drawing of
       Just s -> expandDrawingId s >>= (\(fp, bs) -> return [Drawing fp "" "" bs Nothing])
       Nothing -> throwError WrongElem
elemToParPart' ns element
  | isElem ns "w" "r" element
  , Just objectElem <- findChildByName ns "w" "object" element
  , Just shapeElem <- findChildByName ns "v" "shape" objectElem
  , Just imagedataElem <- findChildByName ns "v" "imagedata" shapeElem
  , Just drawingId <- findAttrByName ns "r" "id" imagedataElem
  = expandDrawingId drawingId >>= (\(fp, bs) -> return [Drawing fp "" "" bs Nothing])
-- Diagram
elemToParPart' ns element
  | isElem ns "w" "r" element
  , Just drawingElem <- findChildByName ns "w" "drawing" element
  , Just _ <- filterElementName
                 (matchQName "drawingml" "diagram" (Just "dgm") "relIds") drawingElem
  = return [Diagram]
-- Chart
elemToParPart' ns element
  | isElem ns "w" "r" element
  , Just drawingElem <- findChildByName ns "w" "drawing" element
  , Just _ <- filterElementName
                (matchQName "drawingml" "chart" (Just "c") "chart") drawingElem
  = return [Chart]
elemToParPart' ns element
  | isElem ns "w" "r" element = do
    runs <- elemToRun ns element
    return $ map PlainRun runs
elemToParPart' ns element
  | Just change <- getTrackedChange ns element = do
      runs <- mconcat <$> mapD (elemToParPart' ns) (elChildren element)
      return [ChangedRuns change runs]
elemToParPart' ns element
  | isElem ns "w" "bookmarkStart" element
  , Just bmId <- findAttrByName ns "w" "id" element
  , Just bmName <- findAttrByName ns "w" "name" element =
    return [BookMark bmId bmName]
elemToParPart' ns element
  | isElem ns "w" "hyperlink" element
  , Just relId <- findAttrByName ns "r" "id" element = do
    location <- asks envLocation
    children <- mconcat <$> mapD (elemToParPart ns) (elChildren element)
    rels <- asks envRelationships
    case lookupRelationship location relId rels of
      Just target ->
         case findAttrByName ns "w" "anchor" element of
             Just anchor -> return
               [ExternalHyperLink (target <> "#" <> anchor) children]
             Nothing -> return [ExternalHyperLink target children]
      Nothing     -> return [ExternalHyperLink "" children]
elemToParPart' ns element
  | isElem ns "w" "hyperlink" element
  , Just anchor <- findAttrByName ns "w" "anchor" element = do
    children <- mconcat <$> mapD (elemToParPart ns) (elChildren element)
    return [InternalHyperLink anchor children]
elemToParPart' ns element
  | isElem ns "w" "commentRangeStart" element
  , Just cmtId <- findAttrByName ns "w" "id" element = do
      (Comments _ commentMap) <- asks envComments
      case M.lookup cmtId commentMap of
        Just cmtElem -> elemToCommentStart ns cmtElem
        Nothing      -> throwError WrongElem
elemToParPart' ns element
  | isElem ns "w" "commentRangeEnd" element
  , Just cmtId <- findAttrByName ns "w" "id" element =
    return [CommentEnd cmtId]
elemToParPart' ns element
  | isElem ns "m" "oMath" element =
    fmap (return . PlainOMath) (eitherToD $ readOMML $ showElement element)
elemToParPart' ns element
  | isElem ns "m" "oMathPara" element =
    fmap (return . OMathPara) (eitherToD $ readOMML $ showElement element)
elemToParPart' _ _ = throwError WrongElem

elemToCommentStart :: NameSpaces -> Element -> D [ParPart]
elemToCommentStart ns element
  | isElem ns "w" "comment" element
  , Just cmtId <- findAttrByName ns "w" "id" element
  , Just cmtAuthor <- findAttrByName ns "w" "author" element
  , cmtDate <- findAttrByName ns "w" "date" element = do
      bps <- mapD (elemToBodyPart ns) (elChildren element)
      return [CommentStart cmtId cmtAuthor cmtDate bps]
elemToCommentStart _ _ = throwError WrongElem

lookupFootnote :: T.Text -> Notes -> Maybe Element
lookupFootnote s (Notes _ fns _) = fns >>= M.lookup s

lookupEndnote :: T.Text -> Notes -> Maybe Element
lookupEndnote s (Notes _ _ ens) = ens >>= M.lookup s

elemToExtent :: Element -> Extent
elemToExtent el =
  case (getDim "cx", getDim "cy") of
    (Just w, Just h) -> Just (w, h)
    _                -> Nothing
 where
  getDim at = filterElementName (\n -> qName n `elem` ["extent", "ext"]) el
              >>= findAttr (QName at Nothing Nothing) >>= safeRead


childElemToRun :: NameSpaces -> Element -> D [Run]
childElemToRun ns element
  | isElem ns "w" "drawing" element
  , picElems <- filterElementsName
                 (matchQName "drawingml" "picture" (Just "pic") "pic") element
  = let (title, alt) = getTitleAndAlt ns element
        drawings = map (\el ->
                         ((findBlip el >>= findAttrByName ns "r" "embed"), el))
                   picElems
    in mapM (\case
                (Just s, el) -> do
                  (fp, bs) <- expandDrawingId s
                  let extent = elemToExtent el <|> elemToExtent element
                  return $ InlineDrawing fp title alt bs extent
                (Nothing, _) -> throwError WrongElem)
       drawings
childElemToRun ns element
  | isElem ns "w" "drawing" element
  , Just _ <- filterElementName
                 (matchQName "drawingml" "chart" (Just "c") "chart") element
  = return [InlineChart]
childElemToRun ns element
  | isElem ns "w" "drawing" element
  , Just _ <- filterElementName
                 (matchQName "drawingml" "diagram" (Just "dgm") "relIds") element
  = return [InlineDiagram]
childElemToRun ns element
  | isElem ns "w" "footnoteReference" element
  , Just fnId <- findAttrByName ns "w" "id" element = do
    notes <- asks envNotes
    case lookupFootnote fnId notes of
      Just e -> do bps <- local (\r -> r {envLocation=InFootnote}) $ mapD (elemToBodyPart ns) (elChildren e)
                   return [Footnote bps]
      Nothing  -> return [Footnote []]
childElemToRun ns element
  | isElem ns "w" "endnoteReference" element
  , Just enId <- findAttrByName ns "w" "id" element = do
    notes <- asks envNotes
    case lookupEndnote enId notes of
      Just e -> do bps <- local (\r -> r {envLocation=InEndnote}) $ mapD (elemToBodyPart ns) (elChildren e)
                   return [Endnote bps]
      Nothing  -> return [Endnote []]
childElemToRun _ _ = throwError WrongElem

elemToRun :: NameSpaces -> Element -> D [Run]
elemToRun ns element
  | isElem ns "w" "r" element
  , Just altCont <- findChildByName ns "mc" "AlternateContent" element =
    do let choices = findChildrenByName ns "mc" "Choice" altCont
           choiceChildren = concatMap (take 1 . elChildren) choices
       outputs <- mapD (childElemToRun ns) choiceChildren
       case outputs of
         r : _ -> return r
         []    -> throwError WrongElem
elemToRun ns element
  | isElem ns "w" "r" element
  , Just drawingElem <- findChildByName ns "w" "drawing" element =
    childElemToRun ns drawingElem
elemToRun ns element
  | isElem ns "w" "r" element
  , Just ref <- findChildByName ns "w" "footnoteReference" element =
    childElemToRun ns ref
elemToRun ns element
  | isElem ns "w" "r" element
  , Just ref <- findChildByName ns "w" "endnoteReference" element =
    childElemToRun ns ref
elemToRun ns element
  | isElem ns "w" "r" element = do
    runElems <- elemToRunElems ns element
    runStyle <- elemToRunStyleD ns element
    return [Run runStyle runElems]
elemToRun _ _ = throwError WrongElem

getParentStyleValue :: (ParStyle -> Maybe a) -> ParStyle -> Maybe a
getParentStyleValue field style
  | Just value <- field style = Just value
  | Just parentStyle <- psParentStyle style
                      = getParentStyleValue field parentStyle
getParentStyleValue _ _ = Nothing

getParStyleField :: (ParStyle -> Maybe a) -> [ParStyle] -> Maybe a
getParStyleField field styles
  | (y:_) <- mapMaybe (getParentStyleValue field) styles
           = Just y
getParStyleField _ _ = Nothing

getTrackedChange :: NameSpaces -> Element -> Maybe TrackedChange
getTrackedChange ns element
  | isElem ns "w" "ins" element || isElem ns "w" "moveTo" element
  , Just cId <- findAttrByName ns "w" "id" element
  , Just cAuthor <- findAttrByName ns "w" "author" element
  , mcDate <- findAttrByName ns "w" "date" element =
      Just $ TrackedChange Insertion (ChangeInfo cId cAuthor mcDate)
getTrackedChange ns element
  | isElem ns "w" "del" element || isElem ns "w" "moveFrom" element
  , Just cId <- findAttrByName ns "w" "id" element
  , Just cAuthor <- findAttrByName ns "w" "author" element
  , mcDate <- findAttrByName ns "w" "date" element =
      Just $ TrackedChange Deletion (ChangeInfo cId cAuthor mcDate)
getTrackedChange _ _ = Nothing

elemToParagraphStyle :: NameSpaces -> Element
                     -> ParStyleMap
                     -> Numbering
                     -> ParagraphStyle
elemToParagraphStyle ns element sty numbering
  | Just pPr <- findChildByName ns "w" "pPr" element =
    let style =
          mapMaybe
          (fmap ParaStyleId . findAttrByName ns "w" "val")
          (findChildrenByName ns "w" "pStyle" pPr)
        pStyle' = mapMaybe (`M.lookup` sty) style
    in ParagraphStyle
      {pStyle = pStyle'
      , numbered = case getNumInfo ns element of
          Just (numId, lvl) -> isJust $ lookupLevel numId lvl numbering
          Nothing -> isJust $ getParStyleField numInfo pStyle'
      , justification =
          case findChildByName ns "w" "jc" pPr >>= findAttrByName ns "w" "val" of
            Nothing -> Nothing
            Just "both" -> Just JustifyBoth
            Just "center" -> Just JustifyCenter
            Just "left" -> Just JustifyLeft
            Just "right" -> Just JustifyRight
            _ -> Nothing
      , indentation =
          getIndentation ns element
      , dropCap =
          case
            findChildByName ns "w" "framePr" pPr >>=
            findAttrByName ns "w" "dropCap"
          of
            Just "none" -> False
            Just _      -> True
            Nothing     -> False
      , pChange     = findChildByName ns "w" "rPr" pPr >>=
                      filterChild (\e -> isElem ns "w" "ins" e ||
                                         isElem ns "w" "moveTo" e ||
                                         isElem ns "w" "del" e ||
                                         isElem ns "w" "moveFrom" e
                                  ) >>=
                      getTrackedChange ns
      , pBidi = checkOnOff ns pPr (elemName ns "w" "bidi")
      , pKeepNext = isJust $ findChildByName ns "w" "keepNext" pPr
      }
  | otherwise = defaultParagraphStyle

elemToRunStyleD :: NameSpaces -> Element -> D RunStyle
elemToRunStyleD ns element
  | Just rPr <- findChildByName ns "w" "rPr" element = do
    charStyles <- asks envCharStyles
    let parentSty =
          findChildByName ns "w" "rStyle" rPr >>=
          findAttrByName ns "w" "val" >>=
          flip M.lookup charStyles . CharStyleId
    return $ elemToRunStyle ns element parentSty
elemToRunStyleD _ _ = return defaultRunStyle

elemToRunElem :: NameSpaces -> Element -> D RunElem
elemToRunElem ns element
  | isElem ns "w" "t" element
    || isElem ns "w" "delText" element
    || isElem ns "m" "t" element = do
    let str = strContent element
    font <- asks envFont
    case font of
      Nothing -> return $ TextRun str
      Just f  -> return . TextRun $
                   T.map (\c -> fromMaybe c (getFontChar f c)) str
  | isElem ns "w" "br" element = return LnBrk
  | isElem ns "w" "tab" element = return Tab
  | isElem ns "w" "softHyphen" element = return SoftHyphen
  | isElem ns "w" "noBreakHyphen" element = return NoBreakHyphen
  | isElem ns "w" "sym" element = return (getSymChar ns element)
  | otherwise = throwError WrongElem

-- The char attribute is a hex string
getSymChar :: NameSpaces -> Element -> RunElem
getSymChar ns element
  | Just s <- getCodepoint
  , Just font <- getFont =
    case readLitChar ("\\x" ++ T.unpack s) of
         [(ch, _)] ->
              TextRun $ T.singleton $ fromMaybe ch $ getFontChar font ch
         _ -> TextRun ""
  where
    getCodepoint = findAttrByName ns "w" "char" element
    getFont = findAttrByName ns "w" "font" element >>= textToFont
getSymChar _ _ = TextRun ""

getFontChar :: Font -> Char -> Maybe Char
getFontChar font ch = chr <$> M.lookup (font, point) symbolMap
 where
   point  -- sometimes F000 is added to put char in private range:
      | ch >= '\xF000' = ord ch - 0xF000
      | otherwise = ord ch

elemToRunElems :: NameSpaces -> Element -> D [RunElem]
elemToRunElems ns element
  |  isElem ns "w" "r" element
     || isElem ns "m" "r" element = do
       let qualName = elemName ns "w"
       let font = do
                    fontElem <- findElement (qualName "rFonts") element
                    foldr ((<|>) . (flip findAttr fontElem . qualName))
                         Nothing ["ascii", "hAnsi"]
                      >>= textToFont
       local (setFont font) (mapD (elemToRunElem ns) (elChildren element))
elemToRunElems _ _ = throwError WrongElem

setFont :: Maybe Font -> ReaderEnv -> ReaderEnv
setFont f s = s{envFont = f}

findBlip :: Element -> Maybe Element
findBlip el = do
  blip <- filterElementName (matchQName "drawingml" "main" (Just "a") "blip") el
  -- return svg if present:
  filterElementName (\(QName tag _ _) -> tag == "svgBlip") el `mplus` pure blip

hasCaptionStyle :: ParagraphStyle -> Bool
hasCaptionStyle parstyle = any (isCaptionStyleName . pStyleName) (pStyle parstyle)
 where -- note that these are case insensitive:
   isCaptionStyleName "caption" = True
   isCaptionStyleName "table caption" = True
   isCaptionStyleName "image caption" = True
   isCaptionStyleName _ = False

stripCaptionLabel :: [Element] -> [Element]
stripCaptionLabel els =
  if any isNumberElt els
     then dropWhile (not . isNumberElt) els
     else els
  where
    isNumberElt el@(Element name attribs _ _) =
       (qName name == "fldSimple" &&
             case lookupAttrBy ((== "instr") . qName) attribs of
               Nothing -> False
               Just instr -> "Table" `elem` T.words instr ||
                             "Figure" `elem` T.words instr) ||
       (qName name == "instrText" &&
          let ws = T.words (strContent el)
          in  ("Table" `elem` ws || "Figure" `elem` ws))

isNamespace :: Text -> Text -> Text -> Bool
isNamespace primary secondary url =
  -- first try transitional:
  case T.stripPrefix "http://schemas.openxmlformats.org/" url of
    Just path -> path == primary <> "/2006/" <> secondary
    Nothing -> -- then try strict:
      case T.stripPrefix "http://purl.oclc.org/ooxml/" url of
        Just path -> path == primary <> "/" <> snakeToCamel secondary
        Nothing -> False
 where
   snakeToCamel "custom-properties" = "customProperties"
   snakeToCamel "extended-properties" = "extendedProperties"
   snakeToCamel x = x

matchQName :: Text -> Text -> Maybe Text -> Text -> QName -> Bool
matchQName primary secondary mbprefix name (QName name' mbns' mbprefix') =
  name == name' &&
  (isNothing mbprefix || mbprefix' == mbprefix) &&
  maybe True (isNamespace primary secondary) mbns'
