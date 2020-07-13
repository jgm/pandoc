{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
 Module : Text.Pandoc.Readers.Docx.Parse.Styles
 Copyright : Copyright (C) 2014-2020 Jesse Rosenthal
                           2019 Nikolay Yakimov <root@livid.pp.ru>
 License : GNU GPL, version 2 or above

 Maintainer : Jesse Rosenthal <jrosenthal@jhu.edu>
 Stability : alpha
 Portability : portable

Type machinery and code for extraction and manipulation of docx styles
-}

module Text.Pandoc.Readers.Docx.Parse.Styles (
    CharStyleId(..)
  , CharStyle
  , ParaStyleId(..)
  , ParStyle(..)
  , RunStyle(..)
  , HasStyleName
  , StyleName
  , ParaStyleName
  , CharStyleName
  , FromStyleName
  , VertAlign(..)
  , StyleId
  , HasStyleId
  , archiveToStyles'
  , getStyleId
  , getStyleName
  , cStyleData
  , fromStyleName
  , fromStyleId
  , stringToInteger
  , getNumInfo
  , elemToRunStyle
  , defaultRunStyle
  , checkOnOff
  ) where
import Codec.Archive.Zip
import Control.Applicative ((<|>))
import Data.Function (on)
import Data.String (IsString(..))
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe
import Data.Coerce
import Text.Pandoc.Readers.Docx.Util
import qualified Text.Pandoc.UTF8 as UTF8
import Text.XML.Light

newtype CharStyleId   = CharStyleId T.Text
  deriving (Show, Eq, Ord, IsString, FromStyleId)
newtype ParaStyleId   = ParaStyleId T.Text
  deriving (Show, Eq, Ord, IsString, FromStyleId)

newtype CharStyleName = CharStyleName CIString
  deriving (Show, Eq, Ord, IsString, FromStyleName)
newtype ParaStyleName = ParaStyleName CIString
  deriving (Show, Eq, Ord, IsString, FromStyleName)

-- Case-insensitive comparisons
newtype CIString = CIString T.Text deriving (Show, IsString, FromStyleName)

class FromStyleName a where
  fromStyleName :: a -> T.Text

instance FromStyleName String where
  fromStyleName = T.pack

instance FromStyleName T.Text where
  fromStyleName = id

class FromStyleId a where
  fromStyleId :: a -> T.Text

instance FromStyleId String where
  fromStyleId = T.pack

instance FromStyleId T.Text where
  fromStyleId = id

instance Eq CIString where
   (==) = (==) `on` T.toCaseFold . coerce

instance Ord CIString where
  compare = compare `on` T.toCaseFold . coerce

data VertAlign = BaseLn | SupScrpt | SubScrpt
               deriving Show

data CharStyle = CharStyle { cStyleId   :: CharStyleId
                           , cStyleName :: CharStyleName
                           , cStyleData :: RunStyle
                           } deriving (Show)

data RunStyle = RunStyle { isBold       :: Maybe Bool
                         , isBoldCTL    :: Maybe Bool
                         , isItalic     :: Maybe Bool
                         , isItalicCTL  :: Maybe Bool
                         , isSmallCaps  :: Maybe Bool
                         , isStrike     :: Maybe Bool
                         , isRTL        :: Maybe Bool
                         , isForceCTL   :: Maybe Bool
                         , rVertAlign   :: Maybe VertAlign
                         , rUnderline   :: Maybe String
                         , rParentStyle :: Maybe CharStyle
                         }
                deriving Show

data ParStyle = ParStyle { headingLev    :: Maybe (ParaStyleName, Int)
                         , numInfo       :: Maybe (T.Text, T.Text)
                         , psParentStyle :: Maybe ParStyle
                         , pStyleName    :: ParaStyleName
                         , pStyleId      :: ParaStyleId
                         }
                    deriving Show

defaultRunStyle :: RunStyle
defaultRunStyle = RunStyle { isBold = Nothing
                           , isBoldCTL = Nothing
                           , isItalic = Nothing
                           , isItalicCTL = Nothing
                           , isSmallCaps = Nothing
                           , isStrike = Nothing
                           , isRTL = Nothing
                           , isForceCTL = Nothing
                           , rVertAlign = Nothing
                           , rUnderline = Nothing
                           , rParentStyle = Nothing
                           }

archiveToStyles' :: (Ord k1, Ord k2, ElemToStyle a1, ElemToStyle a2) =>
                    (a1 -> k1) -> (a2 -> k2) -> Archive -> (M.Map k1 a1, M.Map k2 a2)
archiveToStyles' conv1 conv2 zf =
  let stylesElem = findEntryByPath "word/styles.xml" zf >>=
                   (parseXMLDoc . UTF8.toStringLazy . fromEntry)
  in
   case stylesElem of
     Nothing -> (M.empty, M.empty)
     Just styElem ->
       let namespaces = elemToNameSpaces styElem
       in
        ( M.fromList $ map (\r -> (conv1 r, r)) $ buildBasedOnList namespaces styElem Nothing,
          M.fromList $ map (\p -> (conv2 p, p)) $ buildBasedOnList namespaces styElem Nothing)

isBasedOnStyle :: (ElemToStyle a, FromStyleId (StyleId a)) => NameSpaces -> Element -> Maybe a -> Bool
isBasedOnStyle ns element parentStyle
  | isElem ns "w" "style" element
  , Just styleType <- findAttrByName ns "w" "type" element
  , styleType == cStyleType parentStyle
  , Just basedOnVal <- findChildByName ns "w" "basedOn" element >>=
                       findAttrTextByName ns "w" "val"
  , Just ps <- parentStyle = basedOnVal == fromStyleId (getStyleId ps)
  | isElem ns "w" "style" element
  , Just styleType <- findAttrByName ns "w" "type" element
  , styleType == cStyleType parentStyle
  , Nothing <- findChildByName ns "w" "basedOn" element
  , Nothing <- parentStyle = True
  | otherwise = False

class HasStyleId a => ElemToStyle a where
  cStyleType  :: Maybe a -> String
  elemToStyle :: NameSpaces -> Element -> Maybe a -> Maybe a

class FromStyleId (StyleId a) => HasStyleId a where
  type StyleId a
  getStyleId :: a -> StyleId a

class FromStyleName (StyleName a) => HasStyleName a where
  type StyleName a
  getStyleName :: a -> StyleName a

instance ElemToStyle CharStyle where
  cStyleType _ = "character"
  elemToStyle ns element parentStyle
    | isElem ns "w" "style" element
    , Just "character" <- findAttrByName ns "w" "type" element =
      elemToCharStyle ns element parentStyle
    | otherwise = Nothing

instance HasStyleId CharStyle where
  type StyleId CharStyle = CharStyleId
  getStyleId = cStyleId

instance HasStyleName CharStyle where
  type StyleName CharStyle = CharStyleName
  getStyleName = cStyleName

instance ElemToStyle ParStyle where
  cStyleType _ = "paragraph"
  elemToStyle ns element parentStyle
    | isElem ns "w" "style" element
    , Just "paragraph" <- findAttrByName ns "w" "type" element
    = elemToParStyleData ns element parentStyle
    | otherwise = Nothing

instance HasStyleId ParStyle where
  type StyleId ParStyle = ParaStyleId
  getStyleId = pStyleId

instance HasStyleName ParStyle where
  type StyleName ParStyle = ParaStyleName
  getStyleName = pStyleName

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

stringToInteger :: String -> Maybe Integer
stringToInteger s = listToMaybe $ map fst (reads s :: [(Integer, String)])

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

elemToCharStyle :: NameSpaces
                -> Element -> Maybe CharStyle -> Maybe CharStyle
elemToCharStyle ns element parentStyle
  = CharStyle <$> (CharStyleId <$> findAttrTextByName ns "w" "styleId" element)
              <*> getElementStyleName ns element
              <*> Just (elemToRunStyle ns element parentStyle)

elemToRunStyle :: NameSpaces -> Element -> Maybe CharStyle -> RunStyle
elemToRunStyle ns element parentStyle
  | Just rPr <- findChildByName ns "w" "rPr" element =
    RunStyle
      {
        isBold = checkOnOff ns rPr (elemName ns "w" "b")
      , isBoldCTL = checkOnOff ns rPr (elemName ns "w" "bCs")
      , isItalic = checkOnOff ns rPr (elemName ns "w" "i")
      , isItalicCTL = checkOnOff ns rPr (elemName ns "w" "iCs")
      , isSmallCaps = checkOnOff ns rPr (elemName ns "w" "smallCaps")
      , isStrike = checkOnOff ns rPr (elemName ns "w" "strike")
      , isRTL = checkOnOff ns rPr (elemName ns "w" "rtl")
      , isForceCTL = checkOnOff ns rPr (elemName ns "w" "cs")
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
      , rParentStyle = parentStyle
      }
elemToRunStyle _ _ _ = defaultRunStyle

getHeaderLevel :: NameSpaces -> Element -> Maybe (ParaStyleName, Int)
getHeaderLevel ns element
  | Just styleName <- getElementStyleName ns element
  , Just n <- stringToInteger . T.unpack =<<
              (T.stripPrefix "heading " . T.toLower $
                fromStyleName styleName)
  , n > 0 = Just (styleName, fromInteger n)
getHeaderLevel _ _ = Nothing

getElementStyleName :: Coercible T.Text a => NameSpaces -> Element -> Maybe a
getElementStyleName ns el = coerce <$>
  ((findChildByName ns "w" "name" el >>= findAttrTextByName ns "w" "val")
  <|> findAttrTextByName ns "w" "styleId" el)

getNumInfo :: NameSpaces -> Element -> Maybe (T.Text, T.Text)
getNumInfo ns element = do
  let numPr = findChildByName ns "w" "pPr" element >>=
              findChildByName ns "w" "numPr"
      lvl = fromMaybe "0" (numPr >>=
                           findChildByName ns "w" "ilvl" >>=
                           findAttrTextByName ns "w" "val")
  numId <- numPr >>=
           findChildByName ns "w" "numId" >>=
           findAttrTextByName ns "w" "val"
  return (numId, lvl)

elemToParStyleData :: NameSpaces -> Element -> Maybe ParStyle -> Maybe ParStyle
elemToParStyleData ns element parentStyle
  | Just styleId <- findAttrTextByName ns "w" "styleId" element
  , Just styleName <- getElementStyleName ns element
  = Just $ ParStyle
      {
        headingLev = getHeaderLevel ns element
      , numInfo = getNumInfo ns element
      , psParentStyle = parentStyle
      , pStyleName = styleName
      , pStyleId = ParaStyleId styleId
      }
elemToParStyleData _ _ _ = Nothing
