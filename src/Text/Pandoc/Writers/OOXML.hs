{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.OOXML
   Copyright   : Copyright (C) 2012-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions common to OOXML writers (Docx and Powerpoint)
-}
module Text.Pandoc.Writers.OOXML ( mknode
                                 , mktnode
                                 , nodename
                                 , toLazy
                                 , renderXml
                                 , parseXml
                                 , elemToNameSpaces
                                 , elemName
                                 , isElem
                                 , NameSpaces
                                 , fitToPage
                                 ) where

import Codec.Archive.Zip
import Control.Monad.Reader
import Control.Monad.Except (throwError)
import Text.Pandoc.Error
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.XML.Light as XML

mknode :: Node t => String -> [(String,String)] -> t -> Element
mknode s attrs =
  add_attrs (map (\(k,v) -> Attr (nodename k) v) attrs) .  node (nodename s)

mktnode :: String -> [(String,String)] -> T.Text -> Element
mktnode s attrs = mknode s attrs . T.unpack

nodename :: String -> QName
nodename s = QName{ qName = name, qURI = Nothing, qPrefix = prefix }
 where (name, prefix) = case break (==':') s of
                             (xs,[])    -> (xs, Nothing)
                             (ys, _:zs) -> (zs, Just ys)

toLazy :: B.ByteString -> BL.ByteString
toLazy = BL.fromChunks . (:[])

renderXml :: Element -> BL.ByteString
renderXml elt = BL8.pack "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" <>
  UTF8.fromStringLazy (showElement elt)

parseXml :: PandocMonad m => Archive -> Archive -> String -> m Element
parseXml refArchive distArchive relpath =
  case findEntryByPath relpath refArchive `mplus`
         findEntryByPath relpath distArchive of
            Nothing -> throwError $ PandocSomeError $
                        T.pack relpath <> " missing in reference file"
            Just e  -> case parseXMLDoc . UTF8.toStringLazy . fromEntry $ e of
                       Nothing -> throwError $ PandocSomeError $
                                   T.pack relpath <> " corrupt in reference file"
                       Just d  -> return d

-- Copied from Util

attrToNSPair :: XML.Attr -> Maybe (String, String)
attrToNSPair (XML.Attr (QName s _ (Just "xmlns")) val) = Just (s, val)
attrToNSPair _                                     = Nothing


elemToNameSpaces :: Element -> NameSpaces
elemToNameSpaces = mapMaybe attrToNSPair . elAttribs

elemName :: NameSpaces -> String -> String -> QName
elemName ns prefix name =
  QName name (lookup prefix ns) (if null prefix then Nothing else Just prefix)

isElem :: NameSpaces -> String -> String -> Element -> Bool
isElem ns prefix name element =
  let ns' = ns ++ elemToNameSpaces element
  in qName (elName element) == name &&
     qURI (elName element) == lookup prefix ns'

type NameSpaces = [(String, String)]

-- | Scales the image to fit the page
-- sizes are passed in emu
fitToPage :: (Double, Double) -> Integer -> (Integer, Integer)
fitToPage (x, y) pageWidth
  -- Fixes width to the page width and scales the height
  | x > fromIntegral pageWidth =
    (pageWidth, floor $ (fromIntegral pageWidth / x) * y)
  | otherwise = (floor x, floor y)
