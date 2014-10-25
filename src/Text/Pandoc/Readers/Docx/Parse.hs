{-# LANGUAGE PatternGuards, ViewPatterns, FlexibleInstances #-}

{-
Copyright (C) 2014 Jesse Rosenthal <jrosenthal@jhu.edu>

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
 Copyright : Copyright (C) 2014 Jesse Rosenthal
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
                                      , archiveToDocx
                                      ) where
import Codec.Archive.Zip
import Text.XML.Light
import Data.Maybe
import Data.List
import System.FilePath
import Data.Bits ((.|.))
import qualified Data.ByteString.Lazy as B
import qualified Text.Pandoc.UTF8 as UTF8
import Control.Monad.Reader
import Control.Applicative ((<$>), (<|>))
import qualified Data.Map as M
import Text.Pandoc.Compat.Except
import Text.TeXMath.Readers.OMML (readOMML)
import Text.Pandoc.Readers.Docx.Fonts (getUnicode, Font(..))
import Text.TeXMath (Exp)
import Data.Char (readLitChar, ord, chr, isDigit)

data ReaderEnv = ReaderEnv { envNotes         :: Notes
                           , envNumbering     :: Numbering
                           , envRelationships :: [Relationship]
                           , envMedia         :: Media
                           , envFont          :: Maybe Font
                           , envCharStyles    :: CharStyleMap
                           , envParStyles     :: ParStyleMap
                           }
               deriving Show

data DocxError = DocxError | WrongElem
               deriving Show

instance Error DocxError where
  noMsg = WrongElem

type D = ExceptT DocxError (Reader ReaderEnv)

runD :: D a -> ReaderEnv -> Either DocxError a
runD dx re = runReader (runExceptT dx ) re

maybeToD :: Maybe a -> D a
maybeToD (Just a) = return a
maybeToD Nothing = throwError DocxError

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

type NameSpaces = [(String, String)]

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

data Numb = Numb String String           -- right now, only a key to an abstract num
            deriving Show

data AbstractNumb = AbstractNumb String [Level]
                    deriving Show

-- (ilvl, format, string, start)
type Level = (String, String, String, Maybe Integer)

data Relationship = Relationship (RelId, Target)
                  deriving Show

data Notes = Notes NameSpaces
             (Maybe (M.Map String Element))
             (Maybe (M.Map String Element))
           deriving Show

data ParIndentation = ParIndentation { leftParIndent :: Maybe Integer
                                     , rightParIndent :: Maybe Integer
                                     , hangingParIndent :: Maybe Integer}
                      deriving Show

data ParagraphStyle = ParagraphStyle { pStyle :: [String]
                                     , indentation :: Maybe ParIndentation
                                     , dropCap     :: Bool
                                     , pHeading    :: Maybe (String, Int)
                                     , pBlockQuote :: Maybe Bool
                                     }
                      deriving Show

defaultParagraphStyle :: ParagraphStyle
defaultParagraphStyle = ParagraphStyle { pStyle = []
                                       , indentation = Nothing
                                       , dropCap     = False
                                       , pHeading    = Nothing
                                       , pBlockQuote = Nothing
                                       }


data BodyPart = Paragraph ParagraphStyle [ParPart]
              | ListItem ParagraphStyle String String Level [ParPart]
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

data ParPart = PlainRun Run
             | Insertion ChangeId Author ChangeDate [Run]
             | Deletion ChangeId Author ChangeDate [Run]
             | BookMark BookMarkId Anchor
             | InternalHyperLink Anchor [Run]
             | ExternalHyperLink URL [Run]
             | Drawing FilePath B.ByteString
             | PlainOMath [Exp]
             deriving Show

data Run = Run RunStyle [RunElem]
         | Footnote [BodyPart]
         | Endnote [BodyPart]
         | InlineDrawing FilePath B.ByteString
           deriving Show

data RunElem = TextRun String | LnBrk | Tab
             deriving Show

data VertAlign = BaseLn | SupScrpt | SubScrpt
               deriving Show

data RunStyle = RunStyle { isBold :: Maybe Bool
                         , isItalic :: Maybe Bool
                         , isSmallCaps :: Maybe Bool
                         , isStrike :: Maybe Bool
                         , rVertAlign :: Maybe VertAlign
                         , rUnderline :: Maybe String
                         , rStyle :: Maybe CharStyle}
                deriving Show

data ParStyleData = ParStyleData { headingLev :: Maybe (String, Int)
                                 , isBlockQuote :: Maybe Bool
                                 , psStyle :: Maybe ParStyle}
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
type Author = String
type ChangeDate = String

attrToNSPair :: Attr -> Maybe (String, String)
attrToNSPair (Attr (QName s _ (Just "xmlns")) val) = Just (s, val)
attrToNSPair _ = Nothing

archiveToDocx :: Archive -> Either DocxError Docx
archiveToDocx archive = do
  let notes     = archiveToNotes archive
      numbering = archiveToNumbering archive
      rels      = archiveToRelationships archive
      media     = archiveToMedia archive
      (styles, parstyles) = archiveToStyles archive
      rEnv = ReaderEnv notes numbering rels media Nothing styles parstyles
  doc <- runD (archiveToDocument archive) rEnv
  return $ Docx doc


archiveToDocument :: Archive -> D Document
archiveToDocument zf = do
  entry <- maybeToD $ findEntryByPath "word/document.xml" zf
  docElem <- maybeToD $ (parseXMLDoc . UTF8.toStringLazy . fromEntry) entry
  let namespaces = mapMaybe attrToNSPair (elAttribs docElem)
  bodyElem <- maybeToD $ findChild (elemName namespaces "w" "body") docElem
  body <- elemToBody namespaces bodyElem
  return $ Document namespaces body

elemToBody :: NameSpaces -> Element -> D Body
elemToBody ns element | isElem ns "w" "body" element =
  mapD (elemToBodyPart ns) (elChildren element) >>=
  (\bps -> return $ Body bps)
elemToBody _ _ = throwError WrongElem

archiveToStyles :: Archive -> (CharStyleMap, ParStyleMap)
archiveToStyles zf =
  let stylesElem = findEntryByPath "word/styles.xml" zf >>=
                   (parseXMLDoc . UTF8.toStringLazy . fromEntry)
  in
   case stylesElem of
     Nothing -> (M.empty, M.empty)
     Just styElem ->
       let namespaces = mapMaybe attrToNSPair (elAttribs styElem)
       in
        ( M.fromList $ buildBasedOnList namespaces styElem
            (Nothing :: Maybe CharStyle),
          M.fromList $ buildBasedOnList namespaces styElem
            (Nothing :: Maybe ParStyle) )

isBasedOnStyle :: (ElemToStyle a) => NameSpaces -> Element -> Maybe a -> Bool
isBasedOnStyle ns element parentStyle
  | isElem ns "w" "style" element
  , Just styleType <- findAttr (elemName ns "w" "type") element
  , styleType == cStyleType parentStyle
  , Just basedOnVal <- findChild (elemName ns "w" "basedOn") element >>=
                       findAttr (elemName ns "w" "val")
  , Just ps <- parentStyle = (basedOnVal == getStyleId ps)
  | isElem ns "w" "style" element
  , Just styleType <- findAttr (elemName ns "w" "type") element
  , styleType == cStyleType parentStyle
  , Nothing <- findChild (elemName ns "w" "basedOn") element
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
    , Just "character" <- findAttr (elemName ns "w" "type") element
    , Just styleId <- findAttr (elemName ns "w" "styleId") element =
      Just (styleId, elemToRunStyle ns element parentStyle)
    | otherwise = Nothing
  getStyleId s = fst s

instance ElemToStyle ParStyle where
  cStyleType _ = "paragraph"
  elemToStyle ns element parentStyle
    | isElem ns "w" "style" element
    , Just "paragraph" <- findAttr (elemName ns "w" "type") element
    , Just styleId <- findAttr (elemName ns "w" "styleId") element =
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
  case (getStyleChildren ns element rootStyle) of
    [] -> []
    stys -> stys ++
            (concatMap (\s -> buildBasedOnList ns element (Just s)) stys)

archiveToNotes :: Archive -> Notes
archiveToNotes zf =
  let fnElem = findEntryByPath "word/footnotes.xml" zf
               >>= (parseXMLDoc . UTF8.toStringLazy . fromEntry)
      enElem = findEntryByPath "word/endnotes.xml" zf
               >>= (parseXMLDoc . UTF8.toStringLazy . fromEntry)
      fn_namespaces = case fnElem of
        Just e -> mapMaybe attrToNSPair (elAttribs e)
        Nothing -> []
      en_namespaces = case enElem of
        Just e -> mapMaybe attrToNSPair (elAttribs e)
        Nothing -> []
      ns = unionBy (\x y -> fst x == fst y) fn_namespaces en_namespaces
      fn = fnElem >>= (elemToNotes ns "footnote")
      en = enElem >>= (elemToNotes ns "endnote")
  in
   Notes ns fn en

filePathIsRel :: FilePath -> Bool
filePathIsRel fp =
  let (dir, name) = splitFileName fp
  in
   (dir == "word/_rels/") && ((takeExtension name) == ".rels")

relElemToRelationship :: Element -> Maybe Relationship
relElemToRelationship element | qName (elName element) == "Relationship" =
  do
    relId <- findAttr (QName "Id" Nothing Nothing) element
    target <- findAttr (QName "Target" Nothing Nothing) element
    return $ Relationship (relId, target)
relElemToRelationship _ = Nothing


archiveToRelationships :: Archive -> [Relationship]
archiveToRelationships archive =
  let relPaths = filter filePathIsRel (filesInArchive archive)
      entries  = mapMaybe (\f -> findEntryByPath f archive) relPaths
      relElems = mapMaybe (parseXMLDoc . UTF8.toStringLazy . fromEntry) entries
      rels =     mapMaybe relElemToRelationship $ concatMap elChildren relElems
  in
   rels

filePathIsMedia :: FilePath -> Bool
filePathIsMedia fp =
  let (dir, _) = splitFileName fp
  in
   (dir == "word/media/")

getMediaPair :: Archive -> FilePath -> Maybe (FilePath, B.ByteString)
getMediaPair zf fp =
  case findEntryByPath fp zf of
    Just e -> Just (fp, fromEntry e)
    Nothing -> Nothing

archiveToMedia :: Archive -> Media
archiveToMedia zf =
  mapMaybe (getMediaPair zf) (filter filePathIsMedia (filesInArchive zf))

lookupLevel :: String -> String -> Numbering -> Maybe Level
lookupLevel numId ilvl (Numbering _ numbs absNumbs) = do
  absNumId <- lookup numId $ map (\(Numb nid absnumid) -> (nid, absnumid)) numbs
  lvls <- lookup absNumId $ map (\(AbstractNumb aid ls) -> (aid, ls)) absNumbs
  lvl  <- lookup ilvl $ map (\l@(i, _, _, _) -> (i, l)) lvls
  return lvl

numElemToNum :: NameSpaces -> Element -> Maybe Numb
numElemToNum ns element |
  qName (elName element) == "num" &&
  qURI (elName element) == (lookup "w" ns) = do
    numId <- findAttr (QName "numId" (lookup "w" ns) (Just "w")) element
    absNumId <- findChild (QName "abstractNumId" (lookup "w" ns) (Just "w")) element
                >>= findAttr (QName "val" (lookup "w" ns) (Just "w"))
    return $ Numb numId absNumId
numElemToNum _ _ = Nothing

absNumElemToAbsNum :: NameSpaces -> Element -> Maybe AbstractNumb
absNumElemToAbsNum ns element |
  qName (elName element) == "abstractNum" &&
  qURI (elName element) == (lookup "w" ns) = do
    absNumId <- findAttr
                (QName "abstractNumId" (lookup "w" ns) (Just "w"))
                element
    let levelElems = findChildren
                 (QName "lvl" (lookup "w" ns) (Just "w"))
                 element
        levels = mapMaybe (levelElemToLevel ns) levelElems
    return $ AbstractNumb absNumId levels
absNumElemToAbsNum _ _ = Nothing

levelElemToLevel :: NameSpaces -> Element -> Maybe Level
levelElemToLevel ns element |
    qName (elName element) == "lvl" &&
    qURI (elName element) == (lookup "w" ns) = do
      ilvl <- findAttr (QName "ilvl" (lookup "w" ns) (Just "w")) element
      fmt <- findChild (QName "numFmt" (lookup "w" ns) (Just "w")) element
             >>= findAttr (QName "val" (lookup "w" ns) (Just "w"))
      txt <- findChild (QName "lvlText" (lookup "w" ns) (Just "w")) element
             >>= findAttr (QName "val" (lookup "w" ns) (Just "w"))
      let start = findChild (QName "start" (lookup "w" ns) (Just "w")) element
                  >>= findAttr (QName "val" (lookup "w" ns) (Just "w"))
                  >>= (\s -> listToMaybe (map fst (reads s :: [(Integer, String)])))
      return (ilvl, fmt, txt, start)
levelElemToLevel _ _ = Nothing

archiveToNumbering' :: Archive -> Maybe Numbering
archiveToNumbering' zf = do
  case findEntryByPath "word/numbering.xml" zf of
    Nothing -> Just $ Numbering [] [] []
    Just entry -> do
      numberingElem <- (parseXMLDoc . UTF8.toStringLazy . fromEntry) entry
      let namespaces = mapMaybe attrToNSPair (elAttribs numberingElem)
          numElems = findChildren
                     (QName "num" (lookup "w" namespaces) (Just "w"))
                     numberingElem
          absNumElems = findChildren
                        (QName "abstractNum" (lookup "w" namespaces) (Just "w"))
                        numberingElem
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
                  (\e -> findAttr (elemName ns "w" "id") e >>=
                         (\a -> Just (a, e)))
                  (findChildren (elemName ns "w" notetype) element)
      in
       Just $ M.fromList $ pairs
elemToNotes _ _ _ = Nothing

---------------------------------------------
---------------------------------------------

elemName :: NameSpaces -> String -> String -> QName
elemName ns prefix name = (QName name (lookup prefix ns) (Just prefix))

isElem :: NameSpaces -> String -> String -> Element -> Bool
isElem ns prefix name element =
  qName (elName element) == name &&
  qURI (elName element) == (lookup prefix ns)


elemToTblGrid :: NameSpaces -> Element -> D TblGrid
elemToTblGrid ns element | isElem ns "w" "tblGrid" element =
  let cols = findChildren (elemName ns "w" "gridCol") element
  in
   mapD (\e -> maybeToD (findAttr (elemName ns "w" "val") e >>= stringToInteger))
   cols
elemToTblGrid _ _ = throwError WrongElem

elemToTblLook :: NameSpaces -> Element -> D TblLook
elemToTblLook ns element | isElem ns "w" "tblLook" element =
  let firstRow = findAttr (elemName ns "w" "firstRow") element
      val = findAttr (elemName ns "w" "val") element
      firstRowFmt =
        case firstRow of
          Just "1" -> True
          Just  _  -> False
          Nothing -> case val of
            Just bitMask -> testBitMask bitMask 0x020
            Nothing      -> False
  in
   return $ TblLook{firstRowFormatting = firstRowFmt}
elemToTblLook _ _ = throwError WrongElem

elemToRow :: NameSpaces -> Element -> D Row
elemToRow ns element | isElem ns "w" "tr" element =
  do
    let cellElems = findChildren (elemName ns "w" "tc") element
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
  Just $ ParIndentation {
    leftParIndent =
       findAttr (QName "left" (lookup "w" ns) (Just "w")) element >>=
       stringToInteger
    , rightParIndent =
      findAttr (QName "right" (lookup "w" ns) (Just "w")) element >>=
      stringToInteger
    , hangingParIndent =
      findAttr (QName "hanging" (lookup "w" ns) (Just "w")) element >>=
      stringToInteger}
elemToParIndentation _ _ = Nothing


elemToNumInfo :: NameSpaces -> Element -> Maybe (String, String)
elemToNumInfo ns element  | isElem ns "w" "p" element = do
  let pPr = findChild (elemName ns "w" "pPr") element
      numPr = pPr >>= findChild (elemName ns "w" "numPr")
  lvl <- numPr >>=
         findChild (elemName ns "w" "ilvl") >>=
         findAttr (elemName ns "w" "val")
  numId <- numPr >>=
           findChild (elemName ns "w" "numId") >>=
           findAttr (elemName ns "w" "val")
  return (numId, lvl)
elemToNumInfo _ _ = Nothing

testBitMask :: String -> Int -> Bool
testBitMask bitMaskS n =
  case (reads ("0x" ++ bitMaskS) :: [(Int, String)]) of
    []            -> False
    ((n', _) : _) -> ((n' .|. n) /= 0)

stringToInteger :: String -> Maybe Integer
stringToInteger s = listToMaybe $ map fst (reads s :: [(Integer, String)])

elemToBodyPart :: NameSpaces -> Element -> D BodyPart
elemToBodyPart ns element
  | isElem ns "w" "p" element
  , (c:_) <- findChildren (elemName ns "m" "oMathPara") element =
      do
        expsLst <- eitherToD $ readOMML $ showElement c
        return $ OMathPara expsLst
elemToBodyPart ns element
  | isElem ns "w" "p" element
  , Just (numId, lvl) <- elemToNumInfo ns element = do
    sty <- asks envParStyles
    let parstyle = elemToParagraphStyle ns element sty
    parparts <- mapD (elemToParPart ns) (elChildren element)
    num <- asks envNumbering
    case lookupLevel numId lvl num of
      Just levelInfo -> return $ ListItem parstyle numId lvl levelInfo parparts
      Nothing         -> throwError WrongElem
elemToBodyPart ns element
  | isElem ns "w" "p" element = do
    sty <- asks envParStyles
    let parstyle = elemToParagraphStyle ns element sty
    parparts <- mapD (elemToParPart ns) (elChildren element)
    return $ Paragraph parstyle parparts
elemToBodyPart ns element
  | isElem ns "w" "tbl" element = do
    let caption' = findChild (elemName ns "w" "tblPr") element
                   >>= findChild (elemName ns "w" "tblCaption")
                   >>= findAttr (elemName ns "w" "val")
        caption = (fromMaybe "" caption')
        grid' = case findChild (elemName ns "w" "tblGrid") element of
          Just g -> elemToTblGrid ns g
          Nothing -> return []
        tblLook' = case findChild (elemName ns "w" "tblPr") element >>=
                          findChild (elemName ns "w" "tblLook")
                     of
                       Just l -> elemToTblLook ns l
                       Nothing -> return defaultTblLook

    grid <- grid'
    tblLook <- tblLook'
    rows <- mapD (elemToRow ns) (elChildren element)
    return $ Tbl caption grid tblLook rows
elemToBodyPart _ _ = throwError WrongElem

lookupRelationship :: RelId -> [Relationship] -> Maybe Target
lookupRelationship relid rels =
  lookup relid (map (\(Relationship pair) -> pair) rels)

expandDrawingId :: String -> D (FilePath, B.ByteString)
expandDrawingId s = do
  target <- asks (lookupRelationship s . envRelationships)
  case target of
    Just filepath -> do
      bytes <- asks (lookup ("word/" ++ filepath) . envMedia)
      case bytes of
        Just bs -> return (filepath, bs)
        Nothing -> throwError DocxError
    Nothing -> throwError DocxError

elemToParPart :: NameSpaces -> Element -> D ParPart
elemToParPart ns element
  | isElem ns "w" "r" element
  , Just _ <- findChild (elemName ns "w" "drawing") element =
    let a_ns = "http://schemas.openxmlformats.org/drawingml/2006/main"
        drawing = findElement (QName "blip" (Just a_ns) (Just "a")) element
                  >>= findAttr (QName "embed" (lookup "r" ns) (Just "r"))
    in
     case drawing of
       Just s -> expandDrawingId s >>= (\(fp, bs) -> return $ Drawing fp bs)
       Nothing -> throwError WrongElem
elemToParPart ns element
  | isElem ns "w" "r" element =
    elemToRun ns element >>= (\r -> return $ PlainRun r)
elemToParPart ns element
  | isElem ns "w" "ins" element
  , Just cId <- findAttr (elemName ns "w" "id") element
  , Just cAuthor <- findAttr (elemName ns "w" "author") element
  , Just cDate <- findAttr (elemName ns "w" "date") element = do
    runs <- mapD (elemToRun ns) (elChildren element)
    return $ Insertion cId cAuthor cDate runs
elemToParPart ns element
  | isElem ns "w" "del" element
  , Just cId <- findAttr (elemName ns "w" "id") element
  , Just cAuthor <- findAttr (elemName ns "w" "author") element
  , Just cDate <- findAttr (elemName ns "w" "date") element = do
    runs <- mapD (elemToRun ns) (elChildren element)
    return $ Deletion cId cAuthor cDate runs
elemToParPart ns element
  | isElem ns "w" "bookmarkStart" element
  , Just bmId <- findAttr (elemName ns "w" "id") element
  , Just bmName <- findAttr (elemName ns "w" "name") element =
    return $ BookMark bmId bmName
elemToParPart ns element
  | isElem ns "w" "hyperlink" element
  , Just anchor <- findAttr (elemName ns "w" "anchor") element = do
    runs <- mapD (elemToRun ns) (elChildren element)
    return $ InternalHyperLink anchor runs
elemToParPart ns element
  | isElem ns "w" "hyperlink" element
  , Just relId <- findAttr (elemName ns "r" "id") element = do
    runs <- mapD (elemToRun ns) (elChildren element)
    rels <- asks envRelationships
    return $ case lookupRelationship relId rels of
      Just target -> ExternalHyperLink target runs
      Nothing     -> ExternalHyperLink "" runs
elemToParPart ns element
  | isElem ns "m" "oMath" element =
    (eitherToD $ readOMML $ showElement element) >>= (return . PlainOMath)
elemToParPart _ _ = throwError WrongElem

lookupFootnote :: String -> Notes -> Maybe Element
lookupFootnote s (Notes _ fns _) = fns >>= (M.lookup s)

lookupEndnote :: String -> Notes -> Maybe Element
lookupEndnote s (Notes _ _ ens) = ens >>= (M.lookup s)

elemToRun :: NameSpaces -> Element -> D Run
elemToRun ns element
  | isElem ns "w" "r" element
  , Just drawingElem <- findChild (elemName ns "w" "drawing") element =
    let a_ns = "http://schemas.openxmlformats.org/drawingml/2006/main"
        drawing = findElement (QName "blip" (Just a_ns) (Just "a")) drawingElem
                  >>= findAttr (QName "embed" (lookup "r" ns) (Just "r"))
    in
     case drawing of
       Just s -> expandDrawingId s >>=
                 (\(fp, bs) -> return $ InlineDrawing fp bs)
       Nothing -> throwError WrongElem
elemToRun ns element
  | isElem ns "w" "r" element
  , Just ref <- findChild (elemName ns "w" "footnoteReference") element
  , Just fnId <- findAttr (elemName ns "w" "id") ref = do
    notes <- asks envNotes
    case lookupFootnote fnId notes of
      Just e -> do bps <- mapD (elemToBodyPart ns) (elChildren e)
                   return $ Footnote bps
      Nothing  -> return $ Footnote []
elemToRun ns element
  | isElem ns "w" "r" element
  , Just ref <- findChild (elemName ns "w" "endnoteReference") element
  , Just enId <- findAttr (elemName ns "w" "id") ref = do
    notes <- asks envNotes
    case lookupEndnote enId notes of
      Just e -> do bps <- mapD (elemToBodyPart ns) (elChildren e)
                   return $ Endnote bps
      Nothing  -> return $ Endnote []
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

elemToParagraphStyle :: NameSpaces -> Element -> ParStyleMap -> ParagraphStyle
elemToParagraphStyle ns element sty
  | Just pPr <- findChild (elemName ns "w" "pPr") element =
    let style =
          mapMaybe
          (findAttr (elemName ns "w" "val"))
          (findChildren (elemName ns "w" "pStyle") pPr)
    in ParagraphStyle
      {pStyle = style
      , indentation =
          findChild (elemName ns "w" "ind") pPr >>=
          elemToParIndentation ns
      , dropCap =
          case
            findChild (elemName ns "w" "framePr") pPr >>=
            findAttr (elemName ns "w" "dropCap")
          of
            Just "none" -> False
            Just _      -> True
            Nothing     -> False
      , pHeading = getParStyleField headingLev sty style
      , pBlockQuote = getParStyleField isBlockQuote sty style
      }
elemToParagraphStyle _ _ _ =  defaultParagraphStyle

checkOnOff :: NameSpaces -> Element -> QName -> Maybe Bool
checkOnOff ns rPr tag
  | Just t <-  findChild tag rPr
  , Just val <- findAttr (elemName ns "w" "val") t =
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
  | Just rPr <- findChild (elemName ns "w" "rPr") element = do
    charStyles <- asks envCharStyles
    let parentSty = case
          findChild (elemName ns "w" "rStyle") rPr >>=
          findAttr (elemName ns "w" "val")
          of
            Just styName | Just style <- M.lookup styName charStyles ->
              Just (styName, style)
            _            -> Nothing
    return $ elemToRunStyle ns element parentSty
elemToRunStyleD _ _ = return defaultRunStyle

elemToRunStyle :: NameSpaces -> Element -> Maybe CharStyle -> RunStyle
elemToRunStyle ns element parentStyle
  | Just rPr <- findChild (elemName ns "w" "rPr") element =
    RunStyle
      {
        isBold = checkOnOff ns rPr (elemName ns "w" "b")
      , isItalic = checkOnOff ns rPr (elemName ns "w" "i")
      , isSmallCaps = checkOnOff ns rPr (elemName ns "w" "smallCaps")
      , isStrike = checkOnOff ns rPr (elemName ns "w" "strike")
      , rVertAlign =
           findChild (elemName ns "w" "vertAlign") rPr >>=
           findAttr (elemName ns "w" "val") >>=
           \v -> Just $ case v of
             "superscript" -> SupScrpt
             "subscript"   -> SubScrpt
             _             -> BaseLn
      , rUnderline =
          findChild (elemName ns "w" "u") rPr >>=
          findAttr (elemName ns "w" "val")
      , rStyle = parentStyle
        }
elemToRunStyle _ _ _ = defaultRunStyle

isNumericNotNull :: String -> Bool
isNumericNotNull str = (str /= []) && (all isDigit str)

getHeaderLevel :: NameSpaces -> Element -> Maybe (String,Int)
getHeaderLevel ns element
  | Just styleId <- findAttr (elemName ns "w" "styleId") element
  , Just index   <- stripPrefix "Heading" styleId
  , isNumericNotNull index = Just (styleId, read index)
  | Just styleId <- findAttr (elemName ns "w" "styleId") element
  , Just index   <- findChild (elemName ns "w" "name") element >>=
                    findAttr (elemName ns "w" "val") >>=
                    stripPrefix "heading "
  , isNumericNotNull index = Just (styleId, read index)
getHeaderLevel _ _ = Nothing

blockQuoteStyleIds :: [String]
blockQuoteStyleIds = ["Quote", "BlockQuote", "BlockQuotation"]

blockQuoteStyleNames :: [String]
blockQuoteStyleNames = ["Quote", "Block Text"]

getBlockQuote :: NameSpaces -> Element -> Maybe Bool
getBlockQuote ns element
  | Just styleId <- findAttr (elemName ns "w" "styleId") element
  , styleId `elem` blockQuoteStyleIds = Just True
  | Just styleName <- findChild (elemName ns "w" "name") element >>=
                      findAttr (elemName ns "w" "val")
  , styleName `elem` blockQuoteStyleNames = Just True
getBlockQuote _ _ = Nothing

elemToParStyleData :: NameSpaces -> Element -> Maybe ParStyle -> ParStyleData
elemToParStyleData ns element parentStyle =
    ParStyleData
      {
        headingLev = getHeaderLevel ns element
      , isBlockQuote = getBlockQuote ns element
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
  let [(char, _)] = readLitChar ("\\x" ++ s) in
    TextRun . maybe "" (:[]) $ getUnicode font char
  where
    getCodepoint = findAttr (elemName ns "w" "char") element
    getFont = stringToFont =<< findAttr (elemName ns "w" "font") element
    lowerFromPrivate ('F':xs) = '0':xs
    lowerFromPrivate xs = xs
getSymChar _ _ = TextRun ""

stringToFont :: String -> Maybe Font
stringToFont "Symbol" = Just Symbol
stringToFont _ = Nothing

elemToRunElems :: NameSpaces -> Element -> D [RunElem]
elemToRunElems ns element
  |  isElem ns "w" "r" element
     || isElem ns "m" "r" element = do
       let qualName = elemName ns "w"
       let font = do
                    fontElem <- findElement (qualName "rFonts") element
                    stringToFont =<<
                      (foldr (<|>) Nothing $
                        map (flip findAttr fontElem . qualName) ["ascii", "hAnsi"])
       local (setFont font) (mapD (elemToRunElem ns) (elChildren element))
elemToRunElems _ _ = throwError WrongElem

setFont :: Maybe Font -> ReaderEnv -> ReaderEnv
setFont f s = s{envFont = f}
