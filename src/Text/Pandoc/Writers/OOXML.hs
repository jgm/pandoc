module Text.Pandoc.Writers.OOXML ( mknode
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
--import Control.Applicative ((<|>))
-- import Control.Monad.Except (catchError)
import Control.Monad.Reader
-- import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
-- import Data.Char (isSpace, ord, toLower)
-- import Data.List (intercalate, isPrefixOf, isSuffixOf)
-- import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
-- import qualified Data.Set as Set
-- import qualified Data.Text as T
-- import Data.Time.Clock.POSIX
-- import Skylighting
-- import System.Random (randomR)
import Text.Pandoc.Class (PandocMonad)
-- import qualified Text.Pandoc.Class as P
-- import Text.Pandoc.Compat.Time
-- import Text.Pandoc.Definition
-- import Text.Pandoc.Generic
-- import Text.Pandoc.Highlighting (highlight)
-- import Text.Pandoc.ImageSize
-- import Text.Pandoc.Logging
-- import Text.Pandoc.MIME (MimeType, extensionFromMimeType, getMimeType,
--                          getMimeTypeDef)
-- import Text.Pandoc.Options
-- import Text.Pandoc.Readers.Docx.StyleMap
-- import Text.Pandoc.Shared hiding (Element)
import qualified Text.Pandoc.UTF8 as UTF8
-- import Text.Pandoc.Walk
-- import Text.Pandoc.Writers.Math
-- import Text.Pandoc.Writers.Shared (fixDisplayMath)
-- import Text.Printf (printf)
-- import Text.TeXMath
import Text.XML.Light as XML
-- import Text.XML.Light.Cursor as XMLC


mknode :: Node t => String -> [(String,String)] -> t -> Element
mknode s attrs =
  add_attrs (map (\(k,v) -> Attr (nodename k) v) attrs) .  node (nodename s)

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

parseXml :: (PandocMonad m) => Archive -> Archive -> String -> m Element
parseXml refArchive distArchive relpath =
  case findEntryByPath relpath refArchive `mplus`
         findEntryByPath relpath distArchive of
            Nothing -> fail $ relpath ++ " missing in reference file"
            Just e  -> case parseXMLDoc . UTF8.toStringLazy . fromEntry $ e of
                       Nothing -> fail $ relpath ++ " corrupt in reference file"
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
    (pageWidth, floor $ ((fromIntegral pageWidth) / x) * y)
  | otherwise = (floor x, floor y)

