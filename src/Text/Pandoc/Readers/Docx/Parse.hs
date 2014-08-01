{-# LANGUAGE PatternGuards #-}

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
                                      , OMath(..)
                                      , OMathElem(..)
                                      , Base(..)
                                      , TopBottom(..)
                                      , AccentStyle(..)
                                      , BarStyle(..)
                                      , NAryStyle(..)
                                      , DelimStyle(..)
                                      , GroupStyle(..)
                                      , Run(..)
                                      , RunElem(..)
                                      , Notes
                                      , Numbering
                                      , Relationship
                                      , Media
                                      , RunStyle(..)
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
import qualified Data.Map as M
import Text.Pandoc.Compat.Except

data ReaderEnv = ReaderEnv { envNotes         :: Notes
                           , envNumbering     :: Numbering
                           , envRelationships :: [Relationship]
                           , envMedia         :: Media
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

mapD :: (a -> D b) -> [a] -> D [b]
mapD _ [] = return []
mapD f (x:xs) = do
  y <- (f x >>= (\z -> return [z])) `catchError` (\_ -> return [])
  ys <- mapD f xs
  return $ y ++ ys


type NameSpaces = [(String, String)]

data Docx = Docx Document
          deriving Show

data Document = Document NameSpaces Body
          deriving Show

data Body = Body [BodyPart]
          deriving Show

type Media = [(FilePath, B.ByteString)]

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
                                     }
                      deriving Show

defaultParagraphStyle :: ParagraphStyle
defaultParagraphStyle = ParagraphStyle { pStyle = []
                                       , indentation = Nothing
                                       }


data BodyPart = Paragraph ParagraphStyle [ParPart]
              | ListItem ParagraphStyle String String Level [ParPart]
              | Tbl String TblGrid TblLook [Row]
              | OMathPara OMathParaStyle [OMath]
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
             | PlainOMath OMath
             deriving Show

data OMath = OMath [OMathElem]
          deriving Show

data OMathElem = Accent AccentStyle Base
              | Bar BarStyle Base
              | Box Base
              | BorderBox Base
              | Delimiter DelimStyle [Base]
              | EquationArray [Base]
              | Fraction [OMathElem] [OMathElem]
              | Function [OMathElem] Base
              | Group GroupStyle Base
              | LowerLimit Base [OMathElem]
              | UpperLimit Base [OMathElem]
              | Matrix [[Base]]
              | NAry NAryStyle [OMathElem] [OMathElem] Base
              | Phantom Base
              | Radical [OMathElem] Base
              | PreSubSuper [OMathElem] [OMathElem] Base
              | Sub Base [OMathElem]
              | SubSuper Base [OMathElem] [OMathElem]
              | Super Base [OMathElem]
              | OMathRun OMathRunStyle Run
              deriving Show

data Base = Base [OMathElem]
          deriving Show

-- placeholders
type OMathParaStyle = [String]

data TopBottom = Top | Bottom
               deriving Show

data AccentStyle = AccentStyle { accentChar :: Maybe Char }
                 deriving Show

data BarStyle = BarStyle { barPos :: TopBottom}
              deriving Show

data NAryStyle = NAryStyle { nAryChar :: Maybe Char
                           , nAryLimLoc :: LimLoc}
               deriving Show

defaultNAryStyle :: NAryStyle
defaultNAryStyle = NAryStyle { nAryChar = Nothing -- integral, in practice
                             , nAryLimLoc = SubSup }

data LimLoc = SubSup | UnderOver deriving Show

data DelimStyle = DelimStyle { delimBegChar :: Maybe Char
                             , delimSepChar :: Maybe Char
                             , delimEndChar :: Maybe Char}
                  deriving Show

defaultDelimStyle :: DelimStyle
defaultDelimStyle = DelimStyle { delimBegChar = Nothing
                               , delimSepChar = Nothing
                               , delimEndChar = Nothing }

data GroupStyle = GroupStyle { groupChr :: Maybe Char
                             , groupPos :: Maybe TopBottom }
                  deriving Show

defaultGroupStyle :: GroupStyle
defaultGroupStyle = GroupStyle {groupChr = Nothing, groupPos = Nothing}

type OMathRunStyle = [String]


data Run = Run RunStyle [RunElem]
         | Footnote [BodyPart]
         | Endnote [BodyPart]
           deriving Show

data RunElem = TextRun String | LnBrk | Tab
             deriving Show

data RunStyle = RunStyle { isBold :: Bool
                         , isItalic :: Bool
                         , isSmallCaps :: Bool
                         , isStrike :: Bool
                         , isSuperScript :: Bool
                         , isSubScript :: Bool
                         , rUnderline :: Maybe String
                         , rStyle :: Maybe String }
                deriving Show

defaultRunStyle :: RunStyle
defaultRunStyle = RunStyle { isBold = False
                           , isItalic = False
                           , isSmallCaps = False
                           , isStrike = False
                           , isSuperScript = False
                           , isSubScript = False
                           , rUnderline = Nothing
                           , rStyle = Nothing
                           }


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
      rEnv = ReaderEnv notes numbering rels media
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
        let style = []  -- placeholder
        maths <- mapD (elemToMath ns) (elChildren c)
        return $ OMathPara style maths
elemToBodyPart ns element
  | isElem ns "w" "p" element
  , Just (numId, lvl) <- elemToNumInfo ns element = do
    let parstyle = elemToParagraphStyle ns element
    parparts <- mapD (elemToParPart ns) (elChildren element)
    num <- asks envNumbering
    case lookupLevel numId lvl num of
      Just levelInfo -> return $ ListItem parstyle numId lvl levelInfo parparts
      Nothing         -> throwError WrongElem
elemToBodyPart ns element
  | isElem ns "w" "p" element = do
    let parstyle = elemToParagraphStyle ns element
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

elemToMath :: NameSpaces -> Element -> D OMath
elemToMath ns element  | isElem ns "m" "oMath" element =
    mapD (elemToMathElem ns) (elChildren element) >>=
    (\es -> return $ OMath es)
elemToMath _ _ = throwError WrongElem

elemToBase :: NameSpaces -> Element -> D Base
elemToBase ns element | isElem ns "m" "e" element =
  mapD (elemToMathElem ns) (elChildren element) >>=
  (\es -> return $ Base es)
elemToBase _ _ = throwError WrongElem

elemToNAryStyle :: NameSpaces -> Element -> NAryStyle
elemToNAryStyle ns element
  | Just narypr <- findChild (QName "naryPr" (lookup "m" ns) (Just "m")) element =
  let
    chr = findChild (QName "chr" (lookup "m" ns) (Just "m")) narypr >>=
          findAttr (QName "val" (lookup "m" ns) (Just "m")) >>=
          Just . head
    limLoc = findChild (QName "limLoc" (lookup "m" ns) (Just "m")) narypr >>=
             findAttr (QName "val" (lookup "m" ns) (Just "m"))
    limLoc' = case limLoc of
      Just "undOver" -> UnderOver
      Just "subSup"  -> SubSup
      _              -> SubSup
  in
   NAryStyle { nAryChar = chr, nAryLimLoc = limLoc'}
elemToNAryStyle _ _ = defaultNAryStyle

elemToDelimStyle :: NameSpaces -> Element -> DelimStyle
elemToDelimStyle ns element
  | Just dPr <- findChild (QName "dPr" (lookup "m" ns) (Just "m")) element =
    let begChr = findChild (QName "begChr" (lookup "m" ns) (Just "m")) dPr >>=
                 findAttr (QName "val" (lookup "m" ns) (Just "m")) >>=
                 (\c -> if null c then Nothing else (Just $ head c))
        sepChr = findChild (QName "sepChr" (lookup "m" ns) (Just "m")) dPr >>=
                 findAttr (QName "val" (lookup "m" ns) (Just "m")) >>=
                 (\c -> if null c then Nothing else (Just $ head c))
        endChr = findChild (QName "endChr" (lookup "m" ns) (Just "m")) dPr >>=
                 findAttr (QName "val" (lookup "m" ns) (Just "m")) >>=
                 (\c -> if null c then Nothing else (Just $ head c))
    in
     DelimStyle { delimBegChar = begChr
                , delimSepChar = sepChr
                , delimEndChar = endChr}
elemToDelimStyle _ _ = defaultDelimStyle

elemToGroupStyle :: NameSpaces -> Element -> GroupStyle
elemToGroupStyle ns element
  | Just gPr <- findChild (QName "groupChrPr" (lookup "m" ns) (Just "m")) element =
    let chr = findChild (QName "chr" (lookup "m" ns) (Just "m")) gPr >>=
              findAttr (QName "val" (lookup "m" ns) (Just "m")) >>=
              Just . head
        pos = findChild (QName "pos" (lookup "m" ns) (Just "m")) gPr >>=
              findAttr (QName "val" (lookup "m" ns) (Just "m")) >>=
              (\s -> Just $ if s == "top" then Top else Bottom)
    in
     GroupStyle { groupChr = chr, groupPos = pos }
elemToGroupStyle _ _ = defaultGroupStyle

elemToMathElem :: NameSpaces -> Element -> D OMathElem
elemToMathElem ns element | isElem ns "m" "acc" element = do
  let accChar =
        findChild (QName "accPr" (lookup "m" ns) (Just "m")) element >>=
        findChild (QName "chr" (lookup "m" ns) (Just "m")) >>=
        findAttr (QName "val" (lookup "m" ns) (Just "m")) >>=
        Just . head
      accPr = AccentStyle { accentChar = accChar}
  base <-(maybeToD $ findChild (elemName ns "m" "e") element) >>=
         elemToBase ns
  return $ Accent accPr base
elemToMathElem ns element | isElem ns "m" "bar" element = do
  barPr <- maybeToD $
          findChild (QName "barPr" (lookup "m" ns) (Just "m")) element >>=
          findChild (QName "pos" (lookup "m" ns) (Just "m")) >>=
          findAttr (QName "val" (lookup "m" ns) (Just "m")) >>=
          (\s ->
            Just $ BarStyle {
              barPos = (if s == "bot" then Bottom else Top)
              })
  base <-maybeToD (findChild (QName "e" (lookup "m" ns) (Just "m")) element) >>=
         elemToBase ns
  return $ Bar barPr base
elemToMathElem ns element | isElem ns "m" "box" element =
  maybeToD (findChild (elemName ns "m" "e") element) >>=
  elemToBase ns >>=
  (\b -> return $ Box b)
elemToMathElem ns element | isElem ns "m" "borderBox" element =
  maybeToD (findChild (elemName ns "m" "e") element) >>=
  elemToBase ns >>=
  (\b -> return $ BorderBox b)
elemToMathElem ns element | isElem ns "m" "d" element =
  let style = elemToDelimStyle ns element
  in
   mapD (elemToBase ns) (elChildren element) >>=
   (\es -> return $ Delimiter style es)
elemToMathElem ns element | isElem ns "m" "eqArr" element =
  mapD (elemToBase ns) (elChildren element) >>=
  (\es -> return $ EquationArray es)
elemToMathElem ns element | isElem ns "m" "f" element = do
  num <- maybeToD $ findChild (elemName ns "m" "num") element
  den <- maybeToD $ findChild (elemName ns "m" "den") element
  numElems <- mapD (elemToMathElem ns) (elChildren num)
  denElems <- mapD (elemToMathElem ns) (elChildren den)
  return $ Fraction numElems denElems
elemToMathElem ns element | isElem ns "m" "func" element = do
  fName <- maybeToD $ findChild (elemName ns "m" "fName") element
  base <- maybeToD (findChild (elemName ns "m" "e") element) >>=
          elemToBase ns
  fnElems <- mapD (elemToMathElem ns) (elChildren fName)
  return $ Function fnElems base
elemToMathElem ns element | isElem ns "m" "groupChr" element =
  let style = elemToGroupStyle ns element
  in
   maybeToD (findChild (elemName ns "m" "e") element) >>=
   elemToBase ns >>=
   (\b -> return $ Group style b)
elemToMathElem ns element | isElem ns "m" "limLow" element = do
  base <- maybeToD (findChild (elemName ns "m" "e") element)
          >>= elemToBase ns
  lim <- maybeToD $ findChild (elemName ns "m" "lim") element
  limElems <- mapD (elemToMathElem ns) (elChildren lim)
  return $ LowerLimit base limElems
elemToMathElem ns element | isElem ns "m" "limUpp" element = do
  base <- maybeToD (findChild (elemName ns "m" "e") element)
          >>= elemToBase ns
  lim <- maybeToD $ findChild (elemName ns "m" "lim") element
  limElems <- mapD (elemToMathElem ns) (elChildren lim)
  return $ UpperLimit base limElems
elemToMathElem ns element | isElem ns "m" "m" element = do
  let rows = findChildren (elemName ns "m" "mr") element
  bases <- mapD (\mr -> mapD (elemToBase ns) (elChildren mr)) rows
  return $ Matrix bases
elemToMathElem ns element | isElem ns "m" "nary" element = do
  let style = elemToNAryStyle ns element
  sub <- maybeToD (findChild (elemName ns "m" "sub") element) >>=
         (\e -> mapD (elemToMathElem ns) (elChildren e))
  sup <- maybeToD (findChild (elemName ns "m" "sup") element) >>=
         (\e -> mapD (elemToMathElem ns) (elChildren e))
  base <- maybeToD (findChild (elemName ns "m" "e") element) >>=
          elemToBase ns
  return $ NAry style sub sup base
elemToMathElem ns element | isElem ns "m" "rad" element = do
  deg <- maybeToD (findChild (elemName ns "m" "deg") element) >>=
         (\e -> mapD (elemToMathElem ns) (elChildren e))
  base <- maybeToD (findChild (elemName ns "m" "e") element) >>=
          elemToBase ns
  return $ Radical deg base
elemToMathElem ns element | isElem ns "m" "phant" element = do
  base <- maybeToD (findChild (elemName ns "m" "e") element) >>=
          elemToBase ns
  return $ Phantom base
elemToMathElem ns element | isElem ns "m" "sPre" element = do
  sub <- maybeToD (findChild (elemName ns "m" "sub") element) >>=
         (\e -> mapD (elemToMathElem ns) (elChildren e))
  sup <- maybeToD (findChild (elemName ns "m" "sup") element) >>=
         (\e -> mapD (elemToMathElem ns) (elChildren e))
  base <- maybeToD (findChild (elemName ns "m" "e") element) >>=
          elemToBase ns
  return $ PreSubSuper sub sup base
elemToMathElem ns element | isElem ns "m" "sSub" element = do
  base <- maybeToD (findChild (elemName ns "m" "e") element) >>=
          elemToBase ns
  sub <- maybeToD (findChild (elemName ns "m" "sub") element) >>=
         (\e -> mapD (elemToMathElem ns) (elChildren e))
  return $ Sub base sub
elemToMathElem ns element | isElem ns "m" "sSubSup" element = do
  base <- maybeToD (findChild (elemName ns "m" "e") element) >>=
          elemToBase ns
  sub <- maybeToD (findChild (elemName ns "m" "sub") element) >>=
         (\e -> mapD (elemToMathElem ns) (elChildren e))
  sup <- maybeToD (findChild (elemName ns "m" "sup") element) >>=
         (\e -> mapD (elemToMathElem ns) (elChildren e))
  return $ SubSuper base sub sup
elemToMathElem ns element | isElem ns "m" "sSup" element = do
  base <- maybeToD (findChild (elemName ns "m" "e") element) >>=
          elemToBase ns
  sup <- maybeToD (findChild (elemName ns "m" "sup") element) >>=
         (\e -> mapD (elemToMathElem ns) (elChildren e))
  return $ Sub base sup
elemToMathElem ns element | isElem ns "m" "r" element = do
  let style = []            -- placeholder
      rstyle = elemToRunStyle ns element
  relems <- elemToRunElems ns element
  return $ OMathRun style $ Run rstyle relems
elemToMathElem _ _ = throwError WrongElem

lookupRelationship :: RelId -> [Relationship] -> Maybe Target
lookupRelationship relid rels =
  lookup relid (map (\(Relationship pair) -> pair) rels)

expandDrawingId :: String -> D ParPart
expandDrawingId s = do
  target <- asks (lookupRelationship s . envRelationships)
  case target of
    Just filepath -> do
      bytes <- asks (lookup (combine "word" filepath) . envMedia)
      case bytes of
        Just bs -> return $ Drawing filepath bs
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
       Just s -> expandDrawingId s
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
elemToParPart _ _ = throwError WrongElem

lookupFootnote :: String -> Notes -> Maybe Element
lookupFootnote s (Notes _ fns _) = fns >>= (M.lookup s)

lookupEndnote :: String -> Notes -> Maybe Element
lookupEndnote s (Notes _ _ ens) = ens >>= (M.lookup s)

elemToRun :: NameSpaces -> Element -> D Run
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
    return $ Run (elemToRunStyle ns element) runElems
elemToRun _ _ = throwError WrongElem

elemToParagraphStyle :: NameSpaces -> Element -> ParagraphStyle
elemToParagraphStyle ns element
  | Just pPr <- findChild (elemName ns "w" "pPr") element =
    ParagraphStyle
      {pStyle =
          mapMaybe
          (findAttr (elemName ns "w" "val"))
          (findChildren (elemName ns "w" "pStyle") pPr)
      , indentation =
            findChild (elemName ns "w" "ind") pPr >>=
            elemToParIndentation ns
      }
elemToParagraphStyle _ _ =  defaultParagraphStyle


elemToRunStyle :: NameSpaces -> Element -> RunStyle
elemToRunStyle ns element
  | Just rPr <- findChild (elemName ns "w" "rPr") element =
    RunStyle
      {
        isBold = isJust $ findChild (QName "b" (lookup "w" ns) (Just "w")) rPr
      , isItalic = isJust $ findChild (QName "i" (lookup "w" ns) (Just "w")) rPr
      , isSmallCaps = isJust $ findChild (QName "smallCaps" (lookup "w" ns) (Just "w")) rPr
      , isStrike = isJust $ findChild (QName "strike" (lookup "w" ns) (Just "w")) rPr
      , isSuperScript =
        (Just "superscript" ==
        (findChild (QName "vertAlign" (lookup "w" ns) (Just "w")) rPr >>=
         findAttr (QName "val" (lookup "w" ns) (Just "w"))))
      , isSubScript =
        (Just "subscript" ==
        (findChild (QName "vertAlign" (lookup "w" ns) (Just "w")) rPr >>=
         findAttr (QName "val" (lookup "w" ns) (Just "w"))))
      , rUnderline =
        findChild (QName "u" (lookup "w" ns) (Just "w")) rPr >>=
        findAttr (QName "val" (lookup "w" ns) (Just "w"))
      , rStyle =
        findChild (QName "rStyle" (lookup "w" ns) (Just "w")) rPr >>=
        findAttr (QName "val" (lookup "w" ns) (Just "w"))
        }
elemToRunStyle _ _ = defaultRunStyle

elemToRunElem :: NameSpaces -> Element -> D RunElem
elemToRunElem ns element
  | isElem ns "w" "t" element || isElem ns "w" "delText" element =
    return $ TextRun $ strContent element
  | isElem ns "w" "br" element = return LnBrk
  | isElem ns "w" "tab" element = return Tab
  | otherwise = throwError WrongElem

elemToRunElems :: NameSpaces -> Element -> D [RunElem]
elemToRunElems ns element
  |  isElem ns "w" "r" element = mapD (elemToRunElem ns) (elChildren element)
elemToRunElems _ _ = throwError WrongElem










