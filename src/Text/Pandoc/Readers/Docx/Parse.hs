{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE ViewPatterns      #-}

{-
Copyright (C) 2014-2018 Jesse Rosenthal <jrosenthal@jhu.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
-}

{- |
 Module : Text.Pandoc.Readers.Docx.Parse
 Copyright : Copyright (C) 2014-2018 Jesse Rosenthal
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
                                      , Row(..)
                                      , Cell(..)
                                      , TrackedChange(..)
                                      , ChangeType(..)
                                      , ChangeInfo(..)
                                      , FieldInfo(..)
                                      , Level(..)
                                      , archiveToDocx
                                      , archiveToDocxWithWarnings
                                      ) where
import Prelude
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
import Data.Maybe
import System.FilePath
import Text.Pandoc.Readers.Docx.Util
import Text.Pandoc.Readers.Docx.Fields
import Text.Pandoc.Shared (filteredFilesFromArchive, safeRead)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.TeXMath (Exp)
import Text.TeXMath.Readers.OMML (readOMML)
import Text.TeXMath.Unicode.Fonts (Font (..), getUnicode, stringToFont)
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
                           }
               deriving Show

data ReaderState = ReaderState { stateWarnings :: [String]
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
  = concatMap ((unwrap ns) . Elem) (elChildren sdtContent)
  | isElem ns "w" "smartTag" element
  = concatMap ((unwrap ns) . Elem) (elChildren element)
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


data Docx = Docx Document
          deriving Show

data Document = Document NameSpaces Body
          deriving Show

data Body = Body [BodyPart]
          deriving Show

type Media = [(FilePath, B.ByteString)]

type CharStyle = (String, RunStyle)

type ParStyle = (String, ParStyleData)

type CharStyleMap = M.Map String RunStyle

type ParStyleMap = M.Map String ParStyleData

data Numbering = Numbering NameSpaces [Numb] [AbstractNumb]
                 deriving Show

data Numb = Numb String String [LevelOverride]
            deriving Show

-- ilvl, startOverride, lvl
data LevelOverride = LevelOverride String (Maybe String) (Maybe Level)
  deriving Show

data AbstractNumb = AbstractNumb String [Level]
                    deriving Show

-- (ilvl, format, string, start)
data Level = Level String String String (Maybe Integer)
  deriving Show

data DocumentLocation = InDocument | InFootnote | InEndnote
                      deriving (Eq,Show)

data Relationship = Relationship DocumentLocation RelId Target
                  deriving Show

data Notes = Notes NameSpaces
             (Maybe (M.Map String Element))
             (Maybe (M.Map String Element))
           deriving Show

data Comments = Comments NameSpaces (M.Map String Element)
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

data ParagraphStyle = ParagraphStyle { pStyle      :: [String]
                                     , indentation :: Maybe ParIndentation
                                     , dropCap     :: Bool
                                     , pHeading    :: Maybe (String, Int)
                                     , pNumInfo    :: Maybe (String, String)
                                     , pBlockQuote :: Maybe Bool
                                     , pChange     :: Maybe TrackedChange
                                     }
                      deriving Show

defaultParagraphStyle :: ParagraphStyle
defaultParagraphStyle = ParagraphStyle { pStyle = []
                                       , indentation = Nothing
                                       , dropCap     = False
                                       , pHeading    = Nothing
                                       , pNumInfo    = Nothing
                                       , pBlockQuote = Nothing
                                       , pChange     = Nothing
                                       }


data BodyPart = Paragraph ParagraphStyle [ParPart]
              | ListItem ParagraphStyle String String (Maybe Level) [ParPart]
              | Tbl String TblGrid TblLook [Row]
              | OMathPara [Exp]
              deriving Show

type TblGrid = [Integer]

data TblLook = TblLook {firstRowFormatting::Bool}
              deriving Show

defaultTblLook :: TblLook
defaultTblLook = TblLook{firstRowFormatting = False}

data Row = Row [Cell]
           deriving Show

data Cell = Cell [BodyPart]
            deriving Show

-- (width, height) in EMUs
type Extent = Maybe (Double, Double)

data ParPart = PlainRun Run
             | ChangedRuns TrackedChange [Run]
             | CommentStart CommentId Author CommentDate [BodyPart]
             | CommentEnd CommentId
             | BookMark BookMarkId Anchor
             | InternalHyperLink Anchor [Run]
             | ExternalHyperLink URL [Run]
             | Drawing FilePath String String B.ByteString Extent -- title, alt
             | Chart                                              -- placeholder for now
             | PlainOMath [Exp]
             | Field FieldInfo [Run]
             | NullParPart      -- when we need to return nothing, but
                                -- not because of an error.
             deriving Show

data Run = Run RunStyle [RunElem]
         | Footnote [BodyPart]
         | Endnote [BodyPart]
         | InlineDrawing FilePath String String B.ByteString Extent -- title, alt
         | InlineChart          -- placeholder
           deriving Show

data RunElem = TextRun String | LnBrk | Tab | SoftHyphen | NoBreakHyphen
             deriving Show

data VertAlign = BaseLn | SupScrpt | SubScrpt
               deriving Show

data RunStyle = RunStyle { isBold      :: Maybe Bool
                         , isItalic    :: Maybe Bool
                         , isSmallCaps :: Maybe Bool
                         , isStrike    :: Maybe Bool
                         , rVertAlign  :: Maybe VertAlign
                         , rUnderline  :: Maybe String
                         , rStyle      :: Maybe CharStyle}
                deriving Show

data ParStyleData = ParStyleData { headingLev   :: Maybe (String, Int)
                                 , isBlockQuote :: Maybe Bool
                                 , numInfo      :: Maybe (String, String)
                                 , psStyle      :: Maybe ParStyle}
                    deriving Show

defaultRunStyle :: RunStyle
defaultRunStyle = RunStyle { isBold = Nothing
                           , isItalic = Nothing
                           , isSmallCaps = Nothing
                           , isStrike = Nothing
                           , rVertAlign = Nothing
                           , rUnderline = Nothing
                           , rStyle = Nothing}

type Target = String
type Anchor = String
type URL = String
type BookMarkId = String
type RelId = String
type ChangeId = String
type CommentId = String
type Author = String
type ChangeDate = String
type CommentDate = String

archiveToDocx :: Archive -> Either DocxError Docx
archiveToDocx archive = fst <$> archiveToDocxWithWarnings archive

archiveToDocxWithWarnings :: Archive -> Either DocxError (Docx, [String])
archiveToDocxWithWarnings archive = do
  let notes     = archiveToNotes archive
      comments  = archiveToComments archive
      numbering = archiveToNumbering archive
      rels      = archiveToRelationships archive
      media     = filteredFilesFromArchive archive filePathIsMedia
      (styles, parstyles) = archiveToStyles archive
      rEnv =
        ReaderEnv notes comments numbering rels media Nothing styles parstyles InDocument
      rState = ReaderState { stateWarnings = []
                           , stateFldCharState = FldCharClosed
                           }
      (eitherDoc, st) = runD (archiveToDocument archive) rEnv rState
  case eitherDoc of
    Right doc -> Right (Docx doc, stateWarnings st)
    Left e    -> Left e



archiveToDocument :: Archive -> D Document
archiveToDocument zf = do
  entry <- maybeToD $ findEntryByPath "word/document.xml" zf
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
archiveToStyles zf =
  let stylesElem = findEntryByPath "word/styles.xml" zf >>=
                   (parseXMLDoc . UTF8.toStringLazy . fromEntry)
  in
   case stylesElem of
     Nothing -> (M.empty, M.empty)
     Just styElem ->
       let namespaces = elemToNameSpaces styElem
       in
        ( M.fromList $ buildBasedOnList namespaces styElem
            (Nothing :: Maybe CharStyle),
          M.fromList $ buildBasedOnList namespaces styElem
            (Nothing :: Maybe ParStyle) )

isBasedOnStyle :: (ElemToStyle a) => NameSpaces -> Element -> Maybe a -> Bool
isBasedOnStyle ns element parentStyle
  | isElem ns "w" "style" element
  , Just styleType <- findAttrByName ns "w" "type" element
  , styleType == cStyleType parentStyle
  , Just basedOnVal <- findChildByName ns "w" "basedOn" element >>=
                       findAttrByName ns "w" "val"
  , Just ps <- parentStyle = basedOnVal == getStyleId ps
  | isElem ns "w" "style" element
  , Just styleType <- findAttrByName ns "w" "type" element
  , styleType == cStyleType parentStyle
  , Nothing <- findChildByName ns "w" "basedOn" element
  , Nothing <- parentStyle = True
  | otherwise = False

class ElemToStyle a where
  cStyleType  :: Maybe a -> String
  elemToStyle :: NameSpaces -> Element -> Maybe a -> Maybe a
  getStyleId     :: a -> String

instance ElemToStyle CharStyle where
  cStyleType _ = "character"
  elemToStyle ns element parentStyle
    | isElem ns "w" "style" element
    , Just "character" <- findAttrByName ns "w" "type" element
    , Just styleId <- findAttrByName ns "w" "styleId" element =
      Just (styleId, elemToRunStyle ns element parentStyle)
    | otherwise = Nothing
  getStyleId s = fst s

instance ElemToStyle ParStyle where
  cStyleType _ = "paragraph"
  elemToStyle ns element parentStyle
    | isElem ns "w" "style" element
    , Just "paragraph" <- findAttrByName ns "w" "type" element
    , Just styleId <- findAttrByName ns "w" "styleId" element =
      Just (styleId, elemToParStyleData ns element parentStyle)
    | otherwise = Nothing
  getStyleId s = fst s

getStyleChildren :: (ElemToStyle a) => NameSpaces -> Element -> Maybe a -> [a]
getStyleChildren ns element parentStyle
  | isElem ns "w" "styles" element =
    mapMaybe (\e -> elemToStyle ns e parentStyle) $
    filterChildren (\e' -> isBasedOnStyle ns e' parentStyle) element
  | otherwise = []

buildBasedOnList :: (ElemToStyle a) => NameSpaces -> Element -> Maybe a -> [a]
buildBasedOnList ns element rootStyle =
  case getStyleChildren ns element rootStyle of
    [] -> []
    stys -> stys ++
            concatMap (buildBasedOnList ns element . Just) stys

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
      fn = fnElem >>= elemToNotes ns "footnote"
      en = enElem >>= elemToNotes ns "endnote"
  in
   Notes ns fn en

archiveToComments :: Archive -> Comments
archiveToComments zf =
  let cmtsElem = findEntryByPath "word/comments.xml" zf
               >>= (parseXMLDoc . UTF8.toStringLazy . fromEntry)
      cmts_namespaces = case cmtsElem of
        Just e  -> elemToNameSpaces e
        Nothing -> []
      cmts = elemToComments cmts_namespaces <$> cmtsElem
  in
    case cmts of
      Just c  -> Comments cmts_namespaces c
      Nothing -> Comments cmts_namespaces M.empty

filePathToRelType :: FilePath -> Maybe DocumentLocation
filePathToRelType "word/_rels/document.xml.rels"  = Just InDocument
filePathToRelType "word/_rels/footnotes.xml.rels" = Just InFootnote
filePathToRelType "word/_rels/endnotes.xml.rels"  = Just InEndnote
filePathToRelType _                               = Nothing

relElemToRelationship :: DocumentLocation -> Element -> Maybe Relationship
relElemToRelationship relType element | qName (elName element) == "Relationship" =
  do
    relId <- findAttr (QName "Id" Nothing Nothing) element
    target <- findAttr (QName "Target" Nothing Nothing) element
    return $ Relationship relType relId target
relElemToRelationship _ _ = Nothing

filePathToRelationships :: Archive -> FilePath -> [Relationship]
filePathToRelationships ar fp | Just relType <- filePathToRelType fp
                              , Just entry <- findEntryByPath fp ar
                              , Just relElems <- (parseXMLDoc . UTF8.toStringLazy . fromEntry) entry =
  mapMaybe (relElemToRelationship relType) $ elChildren relElems
filePathToRelationships _ _ = []

archiveToRelationships :: Archive -> [Relationship]
archiveToRelationships archive =
  concatMap (filePathToRelationships archive) $ filesInArchive archive

filePathIsMedia :: FilePath -> Bool
filePathIsMedia fp =
  let (dir, _) = splitFileName fp
  in
   (dir == "word/media/")

lookupLevel :: String -> String -> Numbering -> Maybe Level
lookupLevel numId ilvl (Numbering _ numbs absNumbs) = do
  absNumId <- lookup numId $ map (\(Numb nid absnumid _) -> (nid, absnumid)) numbs
  lvls <- lookup absNumId $ map (\(AbstractNumb aid ls) -> (aid, ls)) absNumbs
  lookup ilvl $ map (\l@(Level i _ _ _) -> (i, l)) lvls

loElemToLevelOverride :: NameSpaces -> Element -> Maybe LevelOverride
loElemToLevelOverride ns element
  | isElem ns "w" "lvlOverride" element = do
      ilvl <- findAttrByName ns "w" "ilvl" element
      let startOverride = findChildByName ns "w" "startOverride" element
                          >>= findAttrByName ns "w" "val"
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

elemToNotes :: NameSpaces -> String -> Element -> Maybe (M.Map String Element)
elemToNotes ns notetype element
  | isElem ns "w" (notetype ++ "s") element =
      let pairs = mapMaybe
                  (\e -> findAttrByName ns "w" "id" e >>=
                         (\a -> Just (a, e)))
                  (findChildrenByName ns "w" notetype element)
      in
       Just $
       M.fromList pairs
elemToNotes _ _ _ = Nothing

elemToComments :: NameSpaces -> Element -> M.Map String Element
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

stringToInteger :: String -> Maybe Integer
stringToInteger s = listToMaybe $ map fst (reads s :: [(Integer, String)])

elemToBodyPart :: NameSpaces -> Element -> D BodyPart
elemToBodyPart ns element
  | isElem ns "w" "p" element
  , (c:_) <- findChildrenByName ns "m" "oMathPara" element =
      do
        expsLst <- eitherToD $ readOMML $ showElement c
        return $ OMathPara expsLst
elemToBodyPart ns element
  | isElem ns "w" "p" element
  , Just (numId, lvl) <- getNumInfo ns element = do
    sty <- asks envParStyles
    let parstyle = elemToParagraphStyle ns element sty
    parparts <- mapD (elemToParPart ns) (elChildren element)
    num <- asks envNumbering
    let levelInfo = lookupLevel numId lvl num
    return $ ListItem parstyle numId lvl levelInfo parparts
elemToBodyPart ns element
  | isElem ns "w" "p" element = do
      sty <- asks envParStyles
      let parstyle = elemToParagraphStyle ns element sty
      parparts <- mapD (elemToParPart ns) (elChildren element)
      -- Word uses list enumeration for numbered headings, so we only
      -- want to infer a list from the styles if it is NOT a heading.
      case pHeading parstyle of
        Nothing | Just (numId, lvl) <- pNumInfo parstyle -> do
                    num <- asks envNumbering
                    let levelInfo = lookupLevel numId lvl num
                    return $ ListItem parstyle numId lvl levelInfo parparts
        _ -> return $ Paragraph parstyle parparts
elemToBodyPart ns element
  | isElem ns "w" "tbl" element = do
    let caption' = findChildByName ns "w" "tblPr" element
                   >>= findChildByName ns "w" "tblCaption"
                   >>= findAttrByName ns "w" "val"
        caption = (fromMaybe "" caption')
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

expandDrawingId :: String -> D (FilePath, B.ByteString)
expandDrawingId s = do
  location <- asks envLocation
  target <- asks (lookupRelationship location s . envRelationships)
  case target of
    Just filepath -> do
      bytes <- asks (lookup ("word/" ++ filepath) . envMedia)
      case bytes of
        Just bs -> return (filepath, bs)
        Nothing -> throwError DocxError
    Nothing -> throwError DocxError

getTitleAndAlt :: NameSpaces -> Element -> (String, String)
getTitleAndAlt ns element =
  let mbDocPr = findChildByName ns "wp" "inline" element >>=
                findChildByName ns "wp" "docPr"
      title = fromMaybe "" (mbDocPr >>= findAttrByName ns "" "title")
      alt = fromMaybe "" (mbDocPr >>= findAttrByName ns "" "descr")
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
                  >>= findAttrByName ns "r" "embed"
    in
     case drawing of
       Just s -> expandDrawingId s >>= (\(fp, bs) -> return $ Drawing fp title alt bs $ elemToExtent drawingElem)
       Nothing -> throwError WrongElem
-- The below is an attempt to deal with images in deprecated vml format.
elemToParPart ns element
  | isElem ns "w" "r" element
  , Just _ <- findChildByName ns "w" "pict" element =
    let drawing = findElement (elemName ns "v" "imagedata") element
                  >>= findAttrByName ns "r" "id"
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
          info <- eitherToD $ parseFieldInfo $ strContent instrText
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
  , Just bmId <- findAttrByName ns "w" "id" element
  , Just bmName <- findAttrByName ns "w" "name" element =
    return $ BookMark bmId bmName
elemToParPart ns element
  | isElem ns "w" "hyperlink" element
  , Just relId <- findAttrByName ns "r" "id" element = do
    location <- asks envLocation
    runs <- mapD (elemToRun ns) (elChildren element)
    rels <- asks envRelationships
    case lookupRelationship location relId rels of
      Just target ->
         case findAttrByName ns "w" "anchor" element of
             Just anchor -> return $ ExternalHyperLink (target ++ '#':anchor) runs
             Nothing -> return $ ExternalHyperLink target runs
      Nothing     -> return $ ExternalHyperLink "" runs
elemToParPart ns element
  | isElem ns "w" "hyperlink" element
  , Just anchor <- findAttrByName ns "w" "anchor" element = do
    runs <- mapD (elemToRun ns) (elChildren element)
    return $ InternalHyperLink anchor runs
elemToParPart ns element
  | isElem ns "w" "commentRangeStart" element
  , Just cmtId <- findAttrByName ns "w" "id" element = do
      (Comments _ commentMap) <- asks envComments
      case M.lookup cmtId commentMap of
        Just cmtElem -> elemToCommentStart ns cmtElem
        Nothing      -> throwError WrongElem
elemToParPart ns element
  | isElem ns "w" "commentRangeEnd" element
  , Just cmtId <- findAttrByName ns "w" "id" element =
    return $ CommentEnd cmtId
elemToParPart ns element
  | isElem ns "m" "oMath" element =
    fmap PlainOMath (eitherToD $ readOMML $ showElement element)
elemToParPart _ _ = throwError WrongElem

elemToCommentStart :: NameSpaces -> Element -> D ParPart
elemToCommentStart ns element
  | isElem ns "w" "comment" element
  , Just cmtId <- findAttrByName ns "w" "id" element
  , Just cmtAuthor <- findAttrByName ns "w" "author" element
  , Just cmtDate <- findAttrByName ns "w" "date" element = do
      bps <- mapD (elemToBodyPart ns) (elChildren element)
      return $ CommentStart cmtId cmtAuthor cmtDate bps
elemToCommentStart _ _ = throwError WrongElem

lookupFootnote :: String -> Notes -> Maybe Element
lookupFootnote s (Notes _ fns _) = fns >>= M.lookup s

lookupEndnote :: String -> Notes -> Maybe Element
lookupEndnote s (Notes _ _ ens) = ens >>= M.lookup s

elemToExtent :: Element -> Extent
elemToExtent drawingElem =
  case (getDim "cx", getDim "cy") of
    (Just w, Just h) -> Just (w, h)
    _                -> Nothing
    where
      wp_ns  = "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
      getDim at = findElement (QName "extent" (Just wp_ns) (Just "wp")) drawingElem
                    >>= findAttr (QName at Nothing Nothing) >>= safeRead


childElemToRun :: NameSpaces -> Element -> D Run
childElemToRun ns element
  | isElem ns "w" "drawing" element
  , pic_ns <- "http://schemas.openxmlformats.org/drawingml/2006/picture"
  , Just picElem <- findElement (QName "pic" (Just pic_ns) (Just "pic")) element
  = let (title, alt) = getTitleAndAlt ns element
        a_ns = "http://schemas.openxmlformats.org/drawingml/2006/main"
        drawing = findElement (QName "blip" (Just a_ns) (Just "a")) picElem
                  >>= findAttr (QName "embed" (lookup "r" ns) (Just "r"))
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
  , Just fnId <- findAttrByName ns "w" "id" element = do
    notes <- asks envNotes
    case lookupFootnote fnId notes of
      Just e -> do bps <- local (\r -> r {envLocation=InFootnote}) $ mapD (elemToBodyPart ns) (elChildren e)
                   return $ Footnote bps
      Nothing  -> return $ Footnote []
childElemToRun ns element
  | isElem ns "w" "endnoteReference" element
  , Just enId <- findAttrByName ns "w" "id" element = do
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

getParentStyleValue :: (ParStyleData -> Maybe a) -> ParStyleData -> Maybe a
getParentStyleValue field style
  | Just value <- field style = Just value
  | Just parentStyle <- psStyle style
                      = getParentStyleValue field (snd parentStyle)
getParentStyleValue _ _ = Nothing

getParStyleField :: (ParStyleData -> Maybe a) -> ParStyleMap -> [String] ->
                                                                        Maybe a
getParStyleField field stylemap styles
  | x     <- mapMaybe (\x -> M.lookup x stylemap) styles
  , (y:_) <- mapMaybe (getParentStyleValue field) x
           = Just y
getParStyleField _ _ _ = Nothing

getTrackedChange :: NameSpaces -> Element -> Maybe TrackedChange
getTrackedChange ns element
  | isElem ns "w" "ins" element || isElem ns "w" "moveTo" element
  , Just cId <- findAttrByName ns "w" "id" element
  , Just cAuthor <- findAttrByName ns "w" "author" element
  , Just cDate <- findAttrByName ns "w" "date" element =
      Just $ TrackedChange Insertion (ChangeInfo cId cAuthor cDate)
getTrackedChange ns element
  | isElem ns "w" "del" element || isElem ns "w" "moveFrom" element
  , Just cId <- findAttrByName ns "w" "id" element
  , Just cAuthor <- findAttrByName ns "w" "author" element
  , Just cDate <- findAttrByName ns "w" "date" element =
      Just $ TrackedChange Deletion (ChangeInfo cId cAuthor cDate)
getTrackedChange _ _ = Nothing

elemToParagraphStyle :: NameSpaces -> Element -> ParStyleMap -> ParagraphStyle
elemToParagraphStyle ns element sty
  | Just pPr <- findChildByName ns "w" "pPr" element =
    let style =
          mapMaybe
          (findAttrByName ns "w" "val")
          (findChildrenByName ns "w" "pStyle" pPr)
    in ParagraphStyle
      {pStyle = style
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
      , pHeading = getParStyleField headingLev sty style
      , pNumInfo = getParStyleField numInfo sty style
      , pBlockQuote = getParStyleField isBlockQuote sty style
      , pChange     = findChildByName ns "w" "rPr" pPr >>=
                      filterChild (\e -> isElem ns "w" "ins" e ||
                                         isElem ns "w" "moveTo" e ||
                                         isElem ns "w" "del" e ||
                                         isElem ns "w" "moveFrom" e
                                  ) >>=
                      getTrackedChange ns
      }
elemToParagraphStyle _ _ _ =  defaultParagraphStyle

checkOnOff :: NameSpaces -> Element -> QName -> Maybe Bool
checkOnOff ns rPr tag
  | Just t <-  findChild tag rPr
  , Just val <- findAttrByName ns "w" "val" t =
    Just $ case val of
      "true"  -> True
      "false" -> False
      "on"    -> True
      "off"   -> False
      "1"     -> True
      "0"     -> False
      _       -> False
  | Just _ <- findChild tag rPr = Just True
checkOnOff _ _ _ = Nothing

elemToRunStyleD :: NameSpaces -> Element -> D RunStyle
elemToRunStyleD ns element
  | Just rPr <- findChildByName ns "w" "rPr" element = do
    charStyles <- asks envCharStyles
    let parentSty = case
          findChildByName ns "w" "rStyle" rPr >>=
          findAttrByName ns "w" "val"
          of
            Just styName | Just style <- M.lookup styName charStyles ->
              Just (styName, style)
            _            -> Nothing
    return $ elemToRunStyle ns element parentSty
elemToRunStyleD _ _ = return defaultRunStyle

elemToRunStyle :: NameSpaces -> Element -> Maybe CharStyle -> RunStyle
elemToRunStyle ns element parentStyle
  | Just rPr <- findChildByName ns "w" "rPr" element =
    RunStyle
      {
        isBold = checkOnOff ns rPr (elemName ns "w" "b") `mplus`
                 checkOnOff ns rPr (elemName ns "w" "bCs")
      , isItalic = checkOnOff ns rPr (elemName ns "w" "i") `mplus`
                   checkOnOff ns rPr (elemName ns "w" "iCs")
      , isSmallCaps = checkOnOff ns rPr (elemName ns "w" "smallCaps")
      , isStrike = checkOnOff ns rPr (elemName ns "w" "strike")
      , rVertAlign =
           findChildByName ns "w" "vertAlign" rPr >>=
           findAttrByName ns "w" "val" >>=
           \v -> Just $ case v of
             "superscript" -> SupScrpt
             "subscript"   -> SubScrpt
             _             -> BaseLn
      , rUnderline =
          findChildByName ns "w" "u" rPr >>=
          findAttrByName ns "w" "val"
      , rStyle = parentStyle
        }
elemToRunStyle _ _ _ = defaultRunStyle

getHeaderLevel :: NameSpaces -> Element -> Maybe (String,Int)
getHeaderLevel ns element
  | Just styleId <- findAttrByName ns "w" "styleId" element
  , Just index   <- stripPrefix "Heading" styleId
  , Just n       <- stringToInteger index
  , n > 0 = Just (styleId, fromInteger n)
  | Just styleId <- findAttrByName ns "w" "styleId" element
  , Just index   <- findChildByName ns "w" "name" element >>=
                    findAttrByName ns "w" "val" >>=
                    stripPrefix "heading "
  , Just n <- stringToInteger index
  , n > 0 = Just (styleId, fromInteger n)
getHeaderLevel _ _ = Nothing

blockQuoteStyleIds :: [String]
blockQuoteStyleIds = ["Quote", "BlockQuote", "BlockQuotation"]

blockQuoteStyleNames :: [String]
blockQuoteStyleNames = ["Quote", "Block Text"]

getBlockQuote :: NameSpaces -> Element -> Maybe Bool
getBlockQuote ns element
  | Just styleId <- findAttrByName ns "w" "styleId" element
  , styleId `elem` blockQuoteStyleIds = Just True
  | Just styleName <- findChildByName ns "w" "name" element >>=
                      findAttrByName ns "w" "val"
  , styleName `elem` blockQuoteStyleNames = Just True
getBlockQuote _ _ = Nothing

getNumInfo :: NameSpaces -> Element -> Maybe (String, String)
getNumInfo ns element = do
  let numPr = findChildByName ns "w" "pPr" element >>=
              findChildByName ns "w" "numPr"
      lvl = fromMaybe "0" (numPr >>=
                           findChildByName ns "w" "ilvl" >>=
                           findAttrByName ns "w" "val")
  numId <- numPr >>=
           findChildByName ns "w" "numId" >>=
           findAttrByName ns "w" "val"
  return (numId, lvl)


elemToParStyleData :: NameSpaces -> Element -> Maybe ParStyle -> ParStyleData
elemToParStyleData ns element parentStyle =
    ParStyleData
      {
        headingLev = getHeaderLevel ns element
      , isBlockQuote = getBlockQuote ns element
      , numInfo = getNumInfo ns element
      , psStyle = parentStyle
        }

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
                  map (\x -> fromMaybe x . getUnicode f . lowerFromPrivate $ x) str
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
         [(char, _)] -> TextRun . maybe "" (:[]) $ getUnicode font char
         _           -> TextRun ""
  where
    getCodepoint = findAttrByName ns "w" "char" element
    getFont = stringToFont =<< findAttrByName ns "w" "font" element
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
                    stringToFont =<<
                       foldr ((<|>) . (flip findAttr fontElem . qualName)) Nothing ["ascii", "hAnsi"]
       local (setFont font) (mapD (elemToRunElem ns) (elChildren element))
elemToRunElems _ _ = throwError WrongElem

setFont :: Maybe Font -> ReaderEnv -> ReaderEnv
setFont f s = s{envFont = f}
