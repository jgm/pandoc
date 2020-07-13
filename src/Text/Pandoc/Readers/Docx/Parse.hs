{-# LANGUAGE ViewPatterns      #-}
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
                                      , ParagraphStyle(..)
                                      , ParStyle
                                      , CharStyle(cStyleData)
                                      , Row(..)
                                      , Cell(..)
                                      , TrackedChange(..)
                                      , ChangeType(..)
                                      , ChangeInfo(..)
                                      , FieldInfo(..)
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
                                      , constructBogusParStyleData
                                      , leftBiasedMergeRunStyle
                                      ) where
import Text.Pandoc.Readers.Docx.Parse.Styles
import Codec.Archive.Zip
import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bits ((.|.))
import qualified Data.ByteString.Lazy as B
import Data.Char (chr, ord, readLitChar)
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe
import System.FilePath
import Text.Pandoc.Readers.Docx.Util
import Text.Pandoc.Readers.Docx.Fields
import Text.Pandoc.Shared (filteredFilesFromArchive, safeRead)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.TeXMath (Exp)
import Text.TeXMath.Readers.OMML (readOMML)
import Text.TeXMath.Unicode.Fonts (Font (..), getUnicode, textToFont)
import Text.XML.Light
import qualified Text.XML.Light.Cursor as XMLC

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
                               , stateFldCharState :: FldCharState
                               }
                 deriving Show

data FldCharState = FldCharOpen
                  | FldCharFieldInfo FieldInfo
                  | FldCharContent FieldInfo [Run]
                  | FldCharClosed
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

unwrap :: NameSpaces -> Content -> [Content]
unwrap ns (Elem element)
  | isElem ns "w" "sdt" element
  , Just sdtContent <- findChildByName ns "w" "sdtContent" element
  = concatMap (unwrap ns . Elem) (elChildren sdtContent)
  | isElem ns "w" "smartTag" element
  = concatMap (unwrap ns . Elem) (elChildren element)
unwrap _ content = [content]

unwrapChild :: NameSpaces -> Content -> Content
unwrapChild ns (Elem element) =
  Elem $ element { elContent = concatMap (unwrap ns) (elContent element) }
unwrapChild _ content = content

walkDocument' :: NameSpaces -> XMLC.Cursor -> XMLC.Cursor
walkDocument' ns cur =
  let modifiedCur = XMLC.modifyContent (unwrapChild ns) cur
  in
    case XMLC.nextDF modifiedCur of
      Just cur' -> walkDocument' ns cur'
      Nothing   -> XMLC.root modifiedCur

walkDocument :: NameSpaces -> Element -> Maybe Element
walkDocument ns element =
  let cur = XMLC.fromContent (Elem element)
      cur' = walkDocument' ns cur
  in
    case XMLC.toTree cur' of
      Elem element' -> Just element'
      _             -> Nothing


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

data ParIndentation = ParIndentation { leftParIndent    :: Maybe Integer
                                     , rightParIndent   :: Maybe Integer
                                     , hangingParIndent :: Maybe Integer}
                      deriving Show

data ChangeType = Insertion | Deletion
                deriving Show

data ChangeInfo = ChangeInfo ChangeId Author ChangeDate
                deriving Show

data TrackedChange = TrackedChange ChangeType ChangeInfo
                   deriving Show

data ParagraphStyle = ParagraphStyle { pStyle      :: [ParStyle]
                                     , indentation :: Maybe ParIndentation
                                     , dropCap     :: Bool
                                     , pChange     :: Maybe TrackedChange
                                     , pBidi       :: Maybe Bool
                                     }
                      deriving Show

defaultParagraphStyle :: ParagraphStyle
defaultParagraphStyle = ParagraphStyle { pStyle = []
                                       , indentation = Nothing
                                       , dropCap     = False
                                       , pChange     = Nothing
                                       , pBidi       = Just False
                                       }


data BodyPart = Paragraph ParagraphStyle [ParPart]
              | ListItem ParagraphStyle T.Text T.Text (Maybe Level) [ParPart]
              | Tbl T.Text TblGrid TblLook [Row]
              | OMathPara [Exp]
              deriving Show

type TblGrid = [Integer]

newtype TblLook = TblLook {firstRowFormatting::Bool}
              deriving Show

defaultTblLook :: TblLook
defaultTblLook = TblLook{firstRowFormatting = False}

newtype Row = Row [Cell]
           deriving Show

newtype Cell = Cell [BodyPart]
            deriving Show

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
    , rVertAlign = rVertAlign a <|> rVertAlign b
    , rUnderline = rUnderline a <|> rUnderline b
    , rParentStyle = rParentStyle a
    }

-- (width, height) in EMUs
type Extent = Maybe (Double, Double)

data ParPart = PlainRun Run
             | ChangedRuns TrackedChange [Run]
             | CommentStart CommentId Author CommentDate [BodyPart]
             | CommentEnd CommentId
             | BookMark BookMarkId Anchor
             | InternalHyperLink Anchor [Run]
             | ExternalHyperLink URL [Run]
             | Drawing FilePath T.Text T.Text B.ByteString Extent -- title, alt
             | Chart                                              -- placeholder for now
             | PlainOMath [Exp]
             | Field FieldInfo [Run]
             | NullParPart      -- when we need to return nothing, but
                                -- not because of an error.
             deriving Show

data Run = Run RunStyle [RunElem]
         | Footnote [BodyPart]
         | Endnote [BodyPart]
         | InlineDrawing FilePath T.Text T.Text B.ByteString Extent -- title, alt
         | InlineChart          -- placeholder
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
                           , stateFldCharState = FldCharClosed
                           }
      (eitherDoc, st) = runD (archiveToDocument archive) rEnv rState
  case eitherDoc of
    Right doc -> Right (Docx doc, stateWarnings st)
    Left e    -> Left e

getDocumentXmlPath :: Archive -> Maybe FilePath
getDocumentXmlPath zf = do
  entry <- findEntryByPath "_rels/.rels" zf
  relsElem <- (parseXMLDoc . UTF8.toStringLazy . fromEntry) entry
  let rels = filterChildrenName (\n -> qName n == "Relationship") relsElem
  rel <- find (\e -> findAttr (QName "Type" Nothing Nothing) e ==
                       Just "http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument")
         rels
  fp <- findAttr (QName "Target" Nothing Nothing) rel
  -- sometimes there will be a leading slash, which windows seems to
  -- have trouble with.
  return $ case fp of
    '/' : fp' -> fp'
    _         -> fp

archiveToDocument :: Archive -> D Document
archiveToDocument zf = do
  docPath <- asks envDocXmlPath
  entry <- maybeToD $ findEntryByPath docPath zf
  docElem <- maybeToD $ (parseXMLDoc . UTF8.toStringLazy . fromEntry) entry
  let namespaces = elemToNameSpaces docElem
  bodyElem <- maybeToD $ findChildByName namespaces "w" "body" docElem
  let bodyElem' = fromMaybe bodyElem (walkDocument namespaces bodyElem)
  body <- elemToBody namespaces bodyElem'
  return $ Document namespaces body

elemToBody :: NameSpaces -> Element -> D Body
elemToBody ns element | isElem ns "w" "body" element =
  fmap Body (mapD (elemToBodyPart ns) (elChildren element))
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
  , numInfo = Nothing
  , psParentStyle = Nothing
  , pStyleName = stName
  , pStyleId = ParaStyleId . T.filter (/=' ') . fromStyleName $ stName
  }

archiveToNotes :: Archive -> Notes
archiveToNotes zf =
  let fnElem = findEntryByPath "word/footnotes.xml" zf
               >>= (parseXMLDoc . UTF8.toStringLazy . fromEntry)
      enElem = findEntryByPath "word/endnotes.xml" zf
               >>= (parseXMLDoc . UTF8.toStringLazy . fromEntry)
      fn_namespaces = case fnElem of
        Just e  -> elemToNameSpaces e
        Nothing -> []
      en_namespaces = case enElem of
        Just e  -> elemToNameSpaces e
        Nothing -> []
      ns = unionBy (\x y -> fst x == fst y) fn_namespaces en_namespaces
      fn = fnElem >>= walkDocument ns >>= elemToNotes ns "footnote"
      en = enElem >>= walkDocument ns >>= elemToNotes ns "endnote"
  in
   Notes ns fn en

archiveToComments :: Archive -> Comments
archiveToComments zf =
  let cmtsElem = findEntryByPath "word/comments.xml" zf
               >>= (parseXMLDoc . UTF8.toStringLazy . fromEntry)
      cmts_namespaces = case cmtsElem of
        Just e  -> elemToNameSpaces e
        Nothing -> []
      cmts = elemToComments cmts_namespaces <$> (cmtsElem >>= walkDocument cmts_namespaces)
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

relElemToRelationship :: DocumentLocation -> Element -> Maybe Relationship
relElemToRelationship relType element | qName (elName element) == "Relationship" =
  do
    relId <- findAttrText (QName "Id" Nothing Nothing) element
    target <- findAttrText (QName "Target" Nothing Nothing) element
    return $ Relationship relType relId target
relElemToRelationship _ _ = Nothing

filePathToRelationships :: Archive -> FilePath -> FilePath ->  [Relationship]
filePathToRelationships ar docXmlPath fp
  | Just relType <- filePathToRelType fp docXmlPath
  , Just entry <- findEntryByPath fp ar
  , Just relElems <- (parseXMLDoc . UTF8.toStringLazy . fromEntry) entry =
  mapMaybe (relElemToRelationship relType) $ elChildren relElems
filePathToRelationships _ _ _ = []

archiveToRelationships :: Archive -> FilePath -> [Relationship]
archiveToRelationships archive docXmlPath =
  concatMap (filePathToRelationships archive docXmlPath) $ filesInArchive archive

filePathIsMedia :: FilePath -> Bool
filePathIsMedia fp =
  let (dir, _) = splitFileName fp
  in
   (dir == "word/media/")

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
      ilvl <- findAttrTextByName ns "w" "ilvl" element
      let startOverride = findChildByName ns "w" "startOverride" element
                          >>= findAttrByName ns "w" "val"
                          >>= (\s -> listToMaybe (map fst (reads s :: [(Integer, String)])))
          lvl = findChildByName ns "w" "lvl" element
                >>= levelElemToLevel ns
      return $ LevelOverride ilvl startOverride lvl
loElemToLevelOverride _ _ = Nothing

numElemToNum :: NameSpaces -> Element -> Maybe Numb
numElemToNum ns element
  | isElem ns "w" "num" element = do
      numId <- findAttrTextByName ns "w" "numId" element
      absNumId <- findChildByName ns "w" "abstractNumId" element
                  >>= findAttrTextByName ns "w" "val"
      let lvlOverrides = mapMaybe
                         (loElemToLevelOverride ns)
                         (findChildrenByName ns "w" "lvlOverride" element)
      return $ Numb numId absNumId lvlOverrides
numElemToNum _ _ = Nothing

absNumElemToAbsNum :: NameSpaces -> Element -> Maybe AbstractNumb
absNumElemToAbsNum ns element
  | isElem ns "w" "abstractNum" element = do
      absNumId <- findAttrTextByName ns "w" "abstractNumId" element
      let levelElems = findChildrenByName ns "w" "lvl" element
          levels = mapMaybe (levelElemToLevel ns) levelElems
      return $ AbstractNumb absNumId levels
absNumElemToAbsNum _ _ = Nothing

levelElemToLevel :: NameSpaces -> Element -> Maybe Level
levelElemToLevel ns element
  | isElem ns "w" "lvl" element = do
      ilvl <- findAttrTextByName ns "w" "ilvl" element
      fmt <- findChildByName ns "w" "numFmt" element
             >>= findAttrTextByName ns "w" "val"
      txt <- findChildByName ns "w" "lvlText" element
             >>= findAttrTextByName ns "w" "val"
      let start = findChildByName ns "w" "start" element
                  >>= findAttrByName ns "w" "val"
                  >>= (\s -> listToMaybe (map fst (reads s :: [(Integer, String)])))
      return (Level ilvl fmt txt start)
levelElemToLevel _ _ = Nothing

archiveToNumbering' :: Archive -> Maybe Numbering
archiveToNumbering' zf =
  case findEntryByPath "word/numbering.xml" zf of
    Nothing -> Just $ Numbering [] [] []
    Just entry -> do
      numberingElem <- (parseXMLDoc . UTF8.toStringLazy . fromEntry) entry
      let namespaces = elemToNameSpaces numberingElem
          numElems = findChildrenByName namespaces "w" "num" numberingElem
          absNumElems = findChildrenByName namespaces "w" "abstractNum" numberingElem
          nums = mapMaybe (numElemToNum namespaces) numElems
          absNums = mapMaybe (absNumElemToAbsNum namespaces) absNumElems
      return $ Numbering namespaces nums absNums

archiveToNumbering :: Archive -> Numbering
archiveToNumbering archive =
  fromMaybe (Numbering [] [] []) (archiveToNumbering' archive)

elemToNotes :: NameSpaces -> String -> Element -> Maybe (M.Map T.Text Element)
elemToNotes ns notetype element
  | isElem ns "w" (notetype <> "s") element =
      let pairs = mapMaybe
                  (\e -> findAttrTextByName ns "w" "id" e >>=
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
                  (\e -> findAttrTextByName ns "w" "id" e >>=
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
   mapD (\e -> maybeToD (findAttrByName ns "w" "val" e >>= stringToInteger))
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
    return $ Row cells
elemToRow _ _ = throwError WrongElem

elemToCell :: NameSpaces -> Element -> D Cell
elemToCell ns element | isElem ns "w" "tc" element =
  do
    cellContents <- mapD (elemToBodyPart ns) (elChildren element)
    return $ Cell cellContents
elemToCell _ _ = throwError WrongElem

elemToParIndentation :: NameSpaces -> Element -> Maybe ParIndentation
elemToParIndentation ns element | isElem ns "w" "ind" element =
 Just ParIndentation {
    leftParIndent =
       findAttrByName ns "w" "left" element >>=
       stringToInteger
    , rightParIndent =
      findAttrByName ns "w" "right" element >>=
      stringToInteger
    , hangingParIndent =
      findAttrByName ns "w" "hanging" element >>=
      stringToInteger}
elemToParIndentation _ _ = Nothing

testBitMask :: String -> Int -> Bool
testBitMask bitMaskS n =
  case (reads ("0x" ++ bitMaskS) :: [(Int, String)]) of
    []            -> False
    ((n', _) : _) -> (n' .|. n) /= 0

pHeading :: ParagraphStyle -> Maybe (ParaStyleName, Int)
pHeading = getParStyleField headingLev . pStyle

pNumInfo :: ParagraphStyle -> Maybe (T.Text, T.Text)
pNumInfo = getParStyleField numInfo . pStyle

elemToBodyPart :: NameSpaces -> Element -> D BodyPart
elemToBodyPart ns element
  | isElem ns "w" "p" element
  , (c:_) <- findChildrenByName ns "m" "oMathPara" element =
      do
        expsLst <- eitherToD $ readOMML $ T.pack $ showElement c
        return $ OMathPara expsLst
elemToBodyPart ns element
  | isElem ns "w" "p" element
  , Just (numId, lvl) <- getNumInfo ns element = do
    parstyle <- elemToParagraphStyle ns element <$> asks envParStyles
    parparts <- mapD (elemToParPart ns) (elChildren element)
    levelInfo <- lookupLevel numId lvl <$> asks envNumbering
    return $ ListItem parstyle numId lvl levelInfo parparts
elemToBodyPart ns element
  | isElem ns "w" "p" element = do
      parstyle <- elemToParagraphStyle ns element <$> asks envParStyles
      parparts <- mapD (elemToParPart ns) (elChildren element)
      -- Word uses list enumeration for numbered headings, so we only
      -- want to infer a list from the styles if it is NOT a heading.
      case pHeading parstyle of
        Nothing | Just (numId, lvl) <- pNumInfo parstyle -> do
                    levelInfo <- lookupLevel numId lvl <$> asks envNumbering
                    return $ ListItem parstyle numId lvl levelInfo parparts
        _ -> return $ Paragraph parstyle parparts
elemToBodyPart ns element
  | isElem ns "w" "tbl" element = do
    let caption' = findChildByName ns "w" "tblPr" element
                   >>= findChildByName ns "w" "tblCaption"
                   >>= findAttrTextByName ns "w" "val"
        caption = fromMaybe "" caption'
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
    return $ Tbl caption grid tblLook rows
elemToBodyPart _ _ = throwError WrongElem

lookupRelationship :: DocumentLocation -> RelId -> [Relationship] -> Maybe Target
lookupRelationship docLocation relid rels =
  lookup (docLocation, relid) pairs
  where
    pairs = map (\(Relationship loc relid' target) -> ((loc, relid'), target)) rels

expandDrawingId :: T.Text -> D (FilePath, B.ByteString)
expandDrawingId s = do
  location <- asks envLocation
  target <- asks (fmap T.unpack . lookupRelationship location s . envRelationships)
  case target of
    Just filepath -> do
      bytes <- asks (lookup ("word/" ++ filepath) . envMedia)
      case bytes of
        Just bs -> return (filepath, bs)
        Nothing -> throwError DocxError
    Nothing -> throwError DocxError

getTitleAndAlt :: NameSpaces -> Element -> (T.Text, T.Text)
getTitleAndAlt ns element =
  let mbDocPr = findChildByName ns "wp" "inline" element >>=
                findChildByName ns "wp" "docPr"
      title = fromMaybe "" (mbDocPr >>= findAttrTextByName ns "" "title")
      alt = fromMaybe "" (mbDocPr >>= findAttrTextByName ns "" "descr")
  in (title, alt)

elemToParPart :: NameSpaces -> Element -> D ParPart
elemToParPart ns element
  | isElem ns "w" "r" element
  , Just drawingElem <- findChildByName ns "w" "drawing" element
  , pic_ns <- "http://schemas.openxmlformats.org/drawingml/2006/picture"
  , Just picElem <- findElement (QName "pic" (Just pic_ns) (Just "pic")) drawingElem
  = let (title, alt) = getTitleAndAlt ns drawingElem
        a_ns = "http://schemas.openxmlformats.org/drawingml/2006/main"
        drawing = findElement (QName "blip" (Just a_ns) (Just "a")) picElem
                  >>= findAttrTextByName ns "r" "embed"
    in
     case drawing of
       Just s -> expandDrawingId s >>= (\(fp, bs) -> return $ Drawing fp title alt bs $ elemToExtent drawingElem)
       Nothing -> throwError WrongElem
-- The below is an attempt to deal with images in deprecated vml format.
elemToParPart ns element
  | isElem ns "w" "r" element
  , Just _ <- findChildByName ns "w" "pict" element =
    let drawing = findElement (elemName ns "v" "imagedata") element
                  >>= findAttrTextByName ns "r" "id"
    in
     case drawing of
       -- Todo: check out title and attr for deprecated format.
       Just s -> expandDrawingId s >>= (\(fp, bs) -> return $ Drawing fp "" "" bs Nothing)
       Nothing -> throwError WrongElem
-- Chart
elemToParPart ns element
  | isElem ns "w" "r" element
  , Just drawingElem <- findChildByName ns "w" "drawing" element
  , c_ns <- "http://schemas.openxmlformats.org/drawingml/2006/chart"
  , Just _ <- findElement (QName "chart" (Just c_ns) (Just "c")) drawingElem
  = return Chart
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

So we do this in a number of steps. If we encounter the fldchar begin
tag, we start open a fldchar state variable (see state above). We add
the instrtext to it as FieldInfo. Then we close that and start adding
the runs when we get to separate. Then when we get to end, we produce
the Field type with appropriate FieldInfo and Runs.
-}
elemToParPart ns element
  | isElem ns "w" "r" element
  , Just fldChar <- findChildByName ns "w" "fldChar" element
  , Just fldCharType <- findAttrByName ns "w" "fldCharType" fldChar = do
      fldCharState <- gets stateFldCharState
      case fldCharState of
        FldCharClosed | fldCharType == "begin" -> do
          modify $ \st -> st {stateFldCharState = FldCharOpen}
          return NullParPart
        FldCharFieldInfo info | fldCharType == "separate" -> do
          modify $ \st -> st {stateFldCharState = FldCharContent info []}
          return NullParPart
        FldCharContent info runs | fldCharType == "end" -> do
          modify $ \st -> st {stateFldCharState = FldCharClosed}
          return $ Field info $ reverse runs
        _ -> throwError WrongElem
elemToParPart ns element
  | isElem ns "w" "r" element
  , Just instrText <- findChildByName ns "w" "instrText" element = do
      fldCharState <- gets stateFldCharState
      case fldCharState of
        FldCharOpen -> do
          info <- eitherToD $ parseFieldInfo $ T.pack $ strContent instrText
          modify $ \st -> st{stateFldCharState = FldCharFieldInfo info}
          return NullParPart
        _ -> return NullParPart
elemToParPart ns element
  | isElem ns "w" "r" element = do
    run <- elemToRun ns element
    -- we check to see if we have an open FldChar in state that we're
    -- recording.
    fldCharState <- gets stateFldCharState
    case fldCharState of
      FldCharContent info runs -> do
        modify $ \st -> st{stateFldCharState = FldCharContent info (run : runs)}
        return NullParPart
      _ -> return $ PlainRun run
elemToParPart ns element
  | Just change <- getTrackedChange ns element = do
      runs <- mapD (elemToRun ns) (elChildren element)
      return $ ChangedRuns change runs
elemToParPart ns element
  | isElem ns "w" "bookmarkStart" element
  , Just bmId <- findAttrTextByName ns "w" "id" element
  , Just bmName <- findAttrTextByName ns "w" "name" element =
    return $ BookMark bmId bmName
elemToParPart ns element
  | isElem ns "w" "hyperlink" element
  , Just relId <- findAttrTextByName ns "r" "id" element = do
    location <- asks envLocation
    runs <- mapD (elemToRun ns) (elChildren element)
    rels <- asks envRelationships
    case lookupRelationship location relId rels of
      Just target ->
         case findAttrTextByName ns "w" "anchor" element of
             Just anchor -> return $ ExternalHyperLink (target <> "#" <> anchor) runs
             Nothing -> return $ ExternalHyperLink target runs
      Nothing     -> return $ ExternalHyperLink "" runs
elemToParPart ns element
  | isElem ns "w" "hyperlink" element
  , Just anchor <- findAttrTextByName ns "w" "anchor" element = do
    runs <- mapD (elemToRun ns) (elChildren element)
    return $ InternalHyperLink anchor runs
elemToParPart ns element
  | isElem ns "w" "commentRangeStart" element
  , Just cmtId <- findAttrTextByName ns "w" "id" element = do
      (Comments _ commentMap) <- asks envComments
      case M.lookup cmtId commentMap of
        Just cmtElem -> elemToCommentStart ns cmtElem
        Nothing      -> throwError WrongElem
elemToParPart ns element
  | isElem ns "w" "commentRangeEnd" element
  , Just cmtId <- findAttrTextByName ns "w" "id" element =
    return $ CommentEnd cmtId
elemToParPart ns element
  | isElem ns "m" "oMath" element =
    fmap PlainOMath (eitherToD $ readOMML $ T.pack $ showElement element)
elemToParPart _ _ = throwError WrongElem

elemToCommentStart :: NameSpaces -> Element -> D ParPart
elemToCommentStart ns element
  | isElem ns "w" "comment" element
  , Just cmtId <- findAttrTextByName ns "w" "id" element
  , Just cmtAuthor <- findAttrTextByName ns "w" "author" element
  , Just cmtDate <- findAttrTextByName ns "w" "date" element = do
      bps <- mapD (elemToBodyPart ns) (elChildren element)
      return $ CommentStart cmtId cmtAuthor cmtDate bps
elemToCommentStart _ _ = throwError WrongElem

lookupFootnote :: T.Text -> Notes -> Maybe Element
lookupFootnote s (Notes _ fns _) = fns >>= M.lookup s

lookupEndnote :: T.Text -> Notes -> Maybe Element
lookupEndnote s (Notes _ _ ens) = ens >>= M.lookup s

elemToExtent :: Element -> Extent
elemToExtent drawingElem =
  case (getDim "cx", getDim "cy") of
    (Just w, Just h) -> Just (w, h)
    _                -> Nothing
    where
      wp_ns  = "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
      getDim at = findElement (QName "extent" (Just wp_ns) (Just "wp")) drawingElem
                    >>= findAttr (QName at Nothing Nothing) >>= safeRead . T.pack


childElemToRun :: NameSpaces -> Element -> D Run
childElemToRun ns element
  | isElem ns "w" "drawing" element
  , pic_ns <- "http://schemas.openxmlformats.org/drawingml/2006/picture"
  , Just picElem <- findElement (QName "pic" (Just pic_ns) (Just "pic")) element
  = let (title, alt) = getTitleAndAlt ns element
        a_ns = "http://schemas.openxmlformats.org/drawingml/2006/main"
        drawing = findElement (QName "blip" (Just a_ns) (Just "a")) picElem
                  >>= findAttrText (QName "embed" (lookup "r" ns) (Just "r"))
    in
     case drawing of
       Just s -> expandDrawingId s >>=
                 (\(fp, bs) -> return $ InlineDrawing fp title alt bs $ elemToExtent element)
       Nothing -> throwError WrongElem
childElemToRun ns element
  | isElem ns "w" "drawing" element
  , c_ns <- "http://schemas.openxmlformats.org/drawingml/2006/chart"
  , Just _ <- findElement (QName "chart" (Just c_ns) (Just "c")) element
  = return InlineChart
childElemToRun ns element
  | isElem ns "w" "footnoteReference" element
  , Just fnId <- findAttrTextByName ns "w" "id" element = do
    notes <- asks envNotes
    case lookupFootnote fnId notes of
      Just e -> do bps <- local (\r -> r {envLocation=InFootnote}) $ mapD (elemToBodyPart ns) (elChildren e)
                   return $ Footnote bps
      Nothing  -> return $ Footnote []
childElemToRun ns element
  | isElem ns "w" "endnoteReference" element
  , Just enId <- findAttrTextByName ns "w" "id" element = do
    notes <- asks envNotes
    case lookupEndnote enId notes of
      Just e -> do bps <- local (\r -> r {envLocation=InEndnote}) $ mapD (elemToBodyPart ns) (elChildren e)
                   return $ Endnote bps
      Nothing  -> return $ Endnote []
childElemToRun _ _ = throwError WrongElem

elemToRun :: NameSpaces -> Element -> D Run
elemToRun ns element
  | isElem ns "w" "r" element
  , Just altCont <- findChildByName ns "mc" "AlternateContent" element =
    do let choices = findChildrenByName ns "mc" "Choice" altCont
           choiceChildren = map head $ filter (not . null) $ map elChildren choices
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
    return $ Run runStyle runElems
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
  , Just cId <- findAttrTextByName ns "w" "id" element
  , Just cAuthor <- findAttrTextByName ns "w" "author" element
  , Just cDate <- findAttrTextByName ns "w" "date" element =
      Just $ TrackedChange Insertion (ChangeInfo cId cAuthor cDate)
getTrackedChange ns element
  | isElem ns "w" "del" element || isElem ns "w" "moveFrom" element
  , Just cId <- findAttrTextByName ns "w" "id" element
  , Just cAuthor <- findAttrTextByName ns "w" "author" element
  , Just cDate <- findAttrTextByName ns "w" "date" element =
      Just $ TrackedChange Deletion (ChangeInfo cId cAuthor cDate)
getTrackedChange _ _ = Nothing

elemToParagraphStyle :: NameSpaces -> Element -> ParStyleMap -> ParagraphStyle
elemToParagraphStyle ns element sty
  | Just pPr <- findChildByName ns "w" "pPr" element =
    let style =
          mapMaybe
          (fmap ParaStyleId . findAttrTextByName ns "w" "val")
          (findChildrenByName ns "w" "pStyle" pPr)
    in ParagraphStyle
      {pStyle = mapMaybe (`M.lookup` sty) style
      , indentation =
          findChildByName ns "w" "ind" pPr >>=
          elemToParIndentation ns
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
      }
elemToParagraphStyle _ _ _ =  defaultParagraphStyle

elemToRunStyleD :: NameSpaces -> Element -> D RunStyle
elemToRunStyleD ns element
  | Just rPr <- findChildByName ns "w" "rPr" element = do
    charStyles <- asks envCharStyles
    let parentSty =
          findChildByName ns "w" "rStyle" rPr >>=
          findAttrTextByName ns "w" "val" >>=
          flip M.lookup charStyles . CharStyleId
    return $ elemToRunStyle ns element parentSty
elemToRunStyleD _ _ = return defaultRunStyle

elemToRunElem :: NameSpaces -> Element -> D RunElem
elemToRunElem ns element
  | isElem ns "w" "t" element
    || isElem ns "w" "delText" element
    || isElem ns "m" "t" element = do
    let str = T.pack $ strContent element
    font <- asks envFont
    case font of
      Nothing -> return $ TextRun str
      Just f  -> return . TextRun $
                  T.map (\x -> fromMaybe x . getUnicode f . lowerFromPrivate $ x) str
  | isElem ns "w" "br" element = return LnBrk
  | isElem ns "w" "tab" element = return Tab
  | isElem ns "w" "softHyphen" element = return SoftHyphen
  | isElem ns "w" "noBreakHyphen" element = return NoBreakHyphen
  | isElem ns "w" "sym" element = return (getSymChar ns element)
  | otherwise = throwError WrongElem
  where
    lowerFromPrivate (ord -> c)
      | c >= ord '\xF000' = chr $ c - ord '\xF000'
      | otherwise = chr c

-- The char attribute is a hex string
getSymChar :: NameSpaces -> Element -> RunElem
getSymChar ns element
  | Just s <- lowerFromPrivate <$> getCodepoint
  , Just font <- getFont =
    case readLitChar ("\\x" ++ s) of
         [(char, _)] -> TextRun . maybe "" T.singleton $ getUnicode font char
         _           -> TextRun ""
  where
    getCodepoint = findAttrByName ns "w" "char" element
    getFont = textToFont . T.pack =<< findAttrByName ns "w" "font" element
    lowerFromPrivate ('F':xs) = '0':xs
    lowerFromPrivate xs       = xs
getSymChar _ _ = TextRun ""

elemToRunElems :: NameSpaces -> Element -> D [RunElem]
elemToRunElems ns element
  |  isElem ns "w" "r" element
     || isElem ns "m" "r" element = do
       let qualName = elemName ns "w"
       let font = do
                    fontElem <- findElement (qualName "rFonts") element
                    textToFont . T.pack =<<
                       foldr ((<|>) . (flip findAttr fontElem . qualName)) Nothing ["ascii", "hAnsi"]
       local (setFont font) (mapD (elemToRunElem ns) (elChildren element))
elemToRunElems _ _ = throwError WrongElem

setFont :: Maybe Font -> ReaderEnv -> ReaderEnv
setFont f s = s{envFont = f}
