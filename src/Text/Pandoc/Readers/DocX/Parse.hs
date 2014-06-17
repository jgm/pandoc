{-
Copyright (C) 2014 Jesse Rosenthal <jrosenthal@jhu.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Readers.DocX.Parse
   Copyright   : Copyright (C) 2014 Jesse Rosenthal
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Conversion of DocX archive into DocX haskell type
-}


module Text.Pandoc.Readers.DocX.Parse (  DocX(..)
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
                                       , ParagraphStyle(..)
                                       , Row(..)
                                       , Cell(..)
                                       , getFootNote
                                       , getEndNote
                                       , lookupLevel
                                       , lookupRelationship
                                       , archiveToDocX
                                       ) where
import Codec.Archive.Zip
import Text.XML.Light
import Data.Maybe
import Data.List
import System.FilePath
import Data.Bits ((.|.))
import qualified Data.ByteString.Lazy as B
import qualified Text.Pandoc.UTF8 as UTF8

attrToNSPair :: Attr -> Maybe (String, String)
attrToNSPair (Attr (QName s _ (Just "xmlns")) val) = Just (s, val)
attrToNSPair _ = Nothing


type NameSpaces = [(String, String)]

data DocX = DocX Document Notes Numbering [Relationship] Media
          deriving Show

archiveToDocX :: Archive -> Maybe DocX
archiveToDocX archive = do
  let notes = archiveToNotes archive
      rels = archiveToRelationships archive
      media = archiveToMedia archive
  doc <- archiveToDocument archive
  numbering <- archiveToNumbering archive
  return $ DocX doc notes numbering rels media

data Document = Document NameSpaces Body 
          deriving Show

archiveToDocument :: Archive -> Maybe Document
archiveToDocument zf = do
  entry <- findEntryByPath "word/document.xml" zf
  docElem <- (parseXMLDoc . UTF8.toStringLazy . fromEntry) entry
  let namespaces = mapMaybe attrToNSPair (elAttribs docElem) 
  bodyElem <- findChild (QName "body" (lookup "w" namespaces) Nothing) docElem
  body <- elemToBody namespaces bodyElem
  return $ Document namespaces body

type Media = [(FilePath, B.ByteString)]

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

data Numbering = Numbering NameSpaces [Numb] [AbstractNumb]
                 deriving Show

data Numb = Numb String String           -- right now, only a key to an abstract num
            deriving Show

data AbstractNumb = AbstractNumb String [Level]
                    deriving Show

-- (ilvl, format, string, start)
type Level = (String, String, String, Maybe Integer)

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
        levels = mapMaybe id $ map (levelElemToLevel ns) levelElems
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

archiveToNumbering :: Archive -> Maybe Numbering
archiveToNumbering zf =
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
          nums = mapMaybe id $ map (numElemToNum namespaces) numElems
          absNums = mapMaybe id $ map (absNumElemToAbsNum namespaces) absNumElems
      return $ Numbering namespaces nums absNums

data Notes = Notes NameSpaces (Maybe [(String, [BodyPart])]) (Maybe [(String, [BodyPart])])
           deriving Show

noteElemToNote :: NameSpaces -> Element -> Maybe (String, [BodyPart])
noteElemToNote ns element
  | qName (elName element) `elem` ["endnote", "footnote"] &&
    qURI (elName element) == (lookup "w" ns) =
      do
        noteId <- findAttr (QName "id" (lookup "w" ns) (Just "w")) element
        let bps = map fromJust
                  $ filter isJust
                  $ map (elemToBodyPart ns)
                  $ filterChildrenName (isParOrTbl ns) element
        return $ (noteId, bps)
noteElemToNote _ _ = Nothing

getFootNote :: String -> Notes -> Maybe [BodyPart]
getFootNote s (Notes _ fns _) = fns >>= (lookup s)

getEndNote :: String -> Notes -> Maybe [BodyPart]
getEndNote s (Notes _ _ ens) = ens >>= (lookup s)

elemToNotes :: NameSpaces -> String -> Element -> Maybe [(String, [BodyPart])]
elemToNotes ns notetype element
  | qName (elName element) == (notetype ++ "s") &&
    qURI (elName element) == (lookup "w" ns) =
      Just $ map fromJust
      $ filter isJust
      $ map (noteElemToNote ns)
      $ findChildren (QName notetype (lookup "w" ns) (Just "w")) element
elemToNotes _ _ _ = Nothing

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


data Relationship = Relationship (RelId, Target)
                  deriving Show

lookupRelationship :: RelId -> [Relationship] -> Maybe Target
lookupRelationship relid rels =
  lookup relid (map (\(Relationship pair) -> pair) rels)

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
      entries  = map fromJust $ filter isJust $ map (\f -> findEntryByPath f archive) relPaths
      relElems = map fromJust $ filter isJust $ map (parseXMLDoc . UTF8.toStringLazy . fromEntry) entries
      rels = map fromJust $ filter isJust $ map relElemToRelationship $ concatMap elChildren relElems
  in
   rels
   
data Body = Body [BodyPart]
          deriving Show

isParOrTbl :: NameSpaces -> QName -> Bool
isParOrTbl ns q = qName q `elem` ["p", "tbl"] &&
                  qURI q == (lookup "w" ns)

elemToBody :: NameSpaces -> Element ->  Maybe Body
elemToBody ns element | qName (elName element) == "body" && qURI (elName element) == (lookup "w" ns) =
  Just $ Body
  $ map fromJust
  $ filter isJust
  $ map (elemToBodyPart ns) $ filterChildrenName (isParOrTbl ns) element
elemToBody _ _ = Nothing

isRunOrLinkOrBookmark :: NameSpaces -> QName ->  Bool
isRunOrLinkOrBookmark ns q = qName q `elem` ["r", "hyperlink", "bookmarkStart"] &&
                   qURI q == (lookup "w" ns)

elemToNumInfo :: NameSpaces -> Element -> Maybe (String, String)
elemToNumInfo ns element
  | qName (elName element) == "p" &&
    qURI (elName element) == (lookup "w" ns) =
      do
        pPr <- findChild (QName "pPr" (lookup "w" ns) (Just "w")) element
        numPr <- findChild (QName "numPr" (lookup "w" ns) (Just "w")) pPr
        lvl <- findChild (QName "ilvl" (lookup "w" ns) (Just "w")) numPr >>=
                findAttr (QName "val" (lookup "w" ns) (Just "w"))
        numId <- findChild (QName "numId" (lookup "w" ns) (Just "w")) numPr >>=
                 findAttr (QName "val" (lookup "w" ns) (Just "w"))
        return (numId, lvl)
elemToNumInfo _ _ = Nothing

-- isBookMarkTag :: NameSpaces -> QName -> Bool
-- isBookMarkTag ns q = qName q `elem` ["bookmarkStart", "bookmarkEnd"] &&
--                      qURI q == (lookup "w" ns)

-- parChildrenToBookmark :: NameSpaces -> [Element] -> BookMark
-- parChildrenToBookmark ns (bms : bme : _)
--   | qName (elName bms) == "bookmarkStart" &&
--     qURI (elName bms) == (lookup "w" ns) &&
--     qName (elName bme) == "bookmarkEnd" &&
--     qURI (elName bme) == (lookup "w" ns) = do
--       bmId <- findAttr (QName "id" (lookup "w" ns) (Just "w")) bms
--       bmName <- findAttr (QName "name" (lookup "w" ns) (Just "w")) bms
--       return $ (bmId, bmName)
-- parChildrenToBookmark _ _ = Nothing

elemToBodyPart :: NameSpaces -> Element ->  Maybe BodyPart
elemToBodyPart ns element
  | qName (elName element) == "p" &&
    qURI (elName element) == (lookup "w" ns) =
      let parstyle = elemToParagraphStyle ns element
          parparts = mapMaybe id
                     $ map (elemToParPart ns)
                     $ filterChildrenName (isRunOrLinkOrBookmark ns) element
      in
       case elemToNumInfo ns element of
         Just (numId, lvl) -> Just $ ListItem parstyle numId lvl parparts
         Nothing -> Just $ Paragraph parstyle parparts
  | qName (elName element) == "tbl" &&
    qURI (elName element) == (lookup "w" ns) =
      let
        caption = findChild (QName "tblPr" (lookup "w" ns) (Just "w")) element
                  >>= findChild (QName "tblCaption" (lookup "w" ns) (Just "w"))
                  >>= findAttr (QName "val" (lookup "w" ns) (Just "w"))
        grid = case
          findChild (QName "tblGrid" (lookup "w" ns) (Just "w")) element
          of
            Just g -> elemToTblGrid ns g
            Nothing -> []
        tblLook = findChild (QName "tblPr" (lookup "w" ns) (Just "w")) element
                  >>= findChild (QName "tblLook" (lookup "w" ns) (Just "w"))
                  >>= elemToTblLook ns
      in
       Just $ Tbl
       (fromMaybe "" caption)
       grid
       (fromMaybe defaultTblLook tblLook)
       (mapMaybe (elemToRow ns) (elChildren element))
  | otherwise = Nothing

elemToTblLook :: NameSpaces -> Element -> Maybe TblLook
elemToTblLook ns element
  | qName (elName element) == "tblLook" &&
    qURI (elName element) == (lookup "w" ns) =
      let firstRow = findAttr (QName "firstRow" (lookup "w" ns) (Just "w")) element
          val = findAttr (QName "val" (lookup "w" ns) (Just "w")) element
          firstRowFmt = 
            case firstRow of
              Just "1" -> True
              Just  _  -> False
              Nothing -> case val of
                Just bitMask -> testBitMask bitMask 0x020
                Nothing      -> False
      in
       Just $ TblLook{firstRowFormatting = firstRowFmt}
elemToTblLook _ _ = Nothing

testBitMask :: String -> Int -> Bool
testBitMask bitMaskS n =
  case (reads ("0x" ++ bitMaskS) :: [(Int, String)]) of
    []            -> False
    ((n', _) : _) -> ((n' .|. n) /= 0)

data ParagraphStyle = ParagraphStyle { pStyle :: [String]
                                     , indent :: Maybe Integer
                                     }
                      deriving Show

defaultParagraphStyle :: ParagraphStyle
defaultParagraphStyle = ParagraphStyle { pStyle = []
                                       , indent = Nothing
                                       }

elemToParagraphStyle :: NameSpaces -> Element -> ParagraphStyle
elemToParagraphStyle ns element =
  case findChild (QName "pPr" (lookup "w" ns) (Just "w")) element of
    Just pPr ->
      ParagraphStyle
      {pStyle =
          mapMaybe id $
          map
          (findAttr (QName "val" (lookup "w" ns) (Just "w")))
          (findChildren (QName "pStyle" (lookup "w" ns) (Just "w")) pPr)
      , indent =
        findChild (QName "ind" (lookup "w" ns) (Just "w")) pPr >>=
        findAttr (QName "left" (lookup "w" ns) (Just "w")) >>=
        stringToInteger
        }
    Nothing -> defaultParagraphStyle


data BodyPart = Paragraph ParagraphStyle [ParPart]
              | ListItem ParagraphStyle String String [ParPart]
              | Tbl String TblGrid TblLook [Row]

              deriving Show

type TblGrid = [Integer]

data TblLook = TblLook {firstRowFormatting::Bool}
              deriving Show

defaultTblLook :: TblLook
defaultTblLook = TblLook{firstRowFormatting = False}

stringToInteger :: String -> Maybe Integer
stringToInteger s = listToMaybe $ map fst (reads s :: [(Integer, String)])

elemToTblGrid :: NameSpaces -> Element -> TblGrid
elemToTblGrid ns element
  | qName (elName element) == "tblGrid" &&
    qURI (elName element) == (lookup "w" ns) =
      let
        cols = findChildren (QName "gridCol" (lookup "w" ns) (Just "w")) element
      in
       mapMaybe (\e ->
                  findAttr (QName "val" (lookup "w" ns) (Just ("w"))) e
                  >>= stringToInteger
                )
       cols
elemToTblGrid _ _ = []

data Row = Row [Cell]
           deriving Show


elemToRow :: NameSpaces -> Element -> Maybe Row
elemToRow ns element
  | qName (elName element) == "tr" &&
    qURI (elName element) == (lookup "w" ns) =
      let 
        cells = findChildren (QName "tc" (lookup "w" ns) (Just "w")) element
      in
       Just $ Row (mapMaybe (elemToCell ns) cells)
elemToRow _ _ = Nothing

data Cell = Cell [BodyPart]
            deriving Show

elemToCell :: NameSpaces -> Element -> Maybe Cell
elemToCell ns element
  | qName (elName element) == "tc" &&
    qURI (elName element) == (lookup "w" ns) =
      Just $ Cell (mapMaybe (elemToBodyPart ns) (elChildren element))
elemToCell _ _ = Nothing

data ParPart = PlainRun Run
             | BookMark BookMarkId Anchor
             | InternalHyperLink Anchor [Run]
             | ExternalHyperLink RelId [Run]
             | Drawing String
             deriving Show

data Run = Run RunStyle [RunElem]
         | Footnote String 
         | Endnote String
           deriving Show

data RunElem = TextRun String | LnBrk
             deriving Show

data RunStyle = RunStyle { isBold :: Bool
                         , isItalic :: Bool
                         , isSmallCaps :: Bool
                         , isStrike :: Bool
                         , isSuperScript :: Bool
                         , isSubScript :: Bool
                         , underline :: Maybe String
                         , rStyle :: Maybe String }
                deriving Show

defaultRunStyle :: RunStyle
defaultRunStyle = RunStyle { isBold = False
                           , isItalic = False
                           , isSmallCaps = False
                           , isStrike = False
                           , isSuperScript = False
                           , isSubScript = False
                           , underline = Nothing
                           , rStyle = Nothing
                           } 

elemToRunStyle :: NameSpaces -> Element -> RunStyle
elemToRunStyle ns element =
  case findChild (QName "rPr" (lookup "w" ns) (Just "w")) element of
    Just rPr ->
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
      , underline =
        findChild (QName "u" (lookup "w" ns) (Just "w")) rPr >>=
        findAttr (QName "val" (lookup "w" ns) (Just "w"))
      , rStyle =
        findChild (QName "rStyle" (lookup "w" ns) (Just "w")) rPr >>=
        findAttr (QName "val" (lookup "w" ns) (Just "w"))
        }
    Nothing -> defaultRunStyle

elemToRun :: NameSpaces -> Element -> Maybe Run
elemToRun ns element
  | qName (elName element) == "r" &&
    qURI (elName element) == (lookup "w" ns) =
      case
        findChild (QName "footnoteReference" (lookup "w" ns) (Just "w")) element >>=
        findAttr (QName "id" (lookup "w" ns) (Just "w"))
      of
        Just s -> Just $ Footnote s
        Nothing ->
          case
            findChild (QName "endnoteReference" (lookup "w" ns) (Just "w")) element >>=
            findAttr (QName "id" (lookup "w" ns) (Just "w"))
          of
            Just s -> Just $ Endnote s 
            Nothing ->  Just $
                        Run (elemToRunStyle ns element)
                        (elemToRunElems ns element)
elemToRun _ _ = Nothing

elemToRunElem :: NameSpaces -> Element -> Maybe RunElem
elemToRunElem ns element
  | qName (elName element) == "t" &&
    qURI (elName element) == (lookup "w" ns) =
      Just $ TextRun (strContent element)
  | qName (elName element) == "br" &&
    qURI (elName element) == (lookup "w" ns) =
      Just $ LnBrk
  | otherwise = Nothing


elemToRunElems :: NameSpaces -> Element -> [RunElem]
elemToRunElems ns element
  | qName (elName element) == "r" &&
    qURI (elName element) == (lookup "w" ns) =
      mapMaybe (elemToRunElem ns) (elChildren element)
  | otherwise = []

elemToDrawing :: NameSpaces -> Element -> Maybe ParPart
elemToDrawing ns element
  | qName (elName element) == "drawing" &&
    qURI (elName element) == (lookup "w" ns) =
      let a_ns = "http://schemas.openxmlformats.org/drawingml/2006/main"
      in
       findElement (QName "blip" (Just a_ns) (Just "a")) element
       >>= findAttr (QName "embed" (lookup "r" ns) (Just "r"))
       >>= (\s -> Just $ Drawing s)
elemToDrawing _ _ = Nothing


elemToParPart :: NameSpaces -> Element -> Maybe ParPart
elemToParPart ns element
  | qName (elName element) == "r" &&
    qURI (elName element) == (lookup "w" ns) =
      case findChild (QName "drawing" (lookup "w" ns) (Just "w")) element of
        Just drawingElem -> elemToDrawing ns drawingElem
        Nothing -> do
          r <- elemToRun ns element
          return $ PlainRun r
elemToParPart ns element
  | qName (elName element) == "bookmarkStart" &&
    qURI (elName element) == (lookup "w" ns) = do
      bmId <- findAttr (QName "id" (lookup "w" ns) (Just "w")) element
      bmName <- findAttr (QName "name" (lookup "w" ns) (Just "w")) element
      return $ BookMark bmId bmName
elemToParPart ns element
  | qName (elName element) == "hyperlink" &&
    qURI (elName element) == (lookup "w" ns) =
      let runs = map fromJust $ filter isJust $ map (elemToRun ns)
                 $ findChildren (QName "r" (lookup "w" ns) (Just "w")) element
      in
       case findAttr (QName "anchor" (lookup "w" ns) (Just "w")) element of
         Just anchor ->
          Just $ InternalHyperLink anchor runs
         Nothing ->
           case findAttr (QName "id" (lookup "r" ns) (Just "r")) element of
             Just relId -> Just $ ExternalHyperLink relId runs
             Nothing    -> Nothing
elemToParPart _ _ = Nothing

type Target = String
type Anchor = String
type BookMarkId = String
type RelId = String
               
