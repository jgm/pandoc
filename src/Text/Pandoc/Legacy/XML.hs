module Text.Pandoc.Legacy.XML ( escapeCharForXML,
                         escapeStringForXML,
                         inTags,
                         selfClosingTag,
                         inTagsSimple,
                         inTagsIndented,
                         TP.toEntities,
                         TP.toHtml5Entities,
                         fromEntities ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Pandoc.XML as TP
import Data.String (IsString)
import Text.DocLayout

taround :: (Text -> Text) -> String -> String
taround f = T.unpack . f . T.pack

tl :: [(String, String)] -> [(T.Text, T.Text)]
tl = fmap $ uncurry $ \x y -> (T.pack x, T.pack y)

ton :: (Text -> a) -> String -> a
ton f = f . T.pack

escapeCharForXML :: Char -> String
escapeCharForXML = T.unpack . TP.escapeCharForXML

escapeStringForXML :: String -> String
escapeStringForXML = taround TP.escapeStringForXML

inTags :: (HasChars a, IsString a)
      => Bool -> String -> [(String, String)] -> Doc a -> Doc a
inTags x y = TP.inTags x (T.pack y) . fmap go
  where
    go (a, b) = (T.pack a, T.pack b)

selfClosingTag :: (HasChars a, IsString a)
               => String -> [(String, String)] -> Doc a
selfClosingTag x = TP.selfClosingTag (T.pack x) . tl

inTagsSimple :: (HasChars a, IsString a)
             => String -> Doc a -> Doc a
inTagsSimple = ton TP.inTagsSimple

inTagsIndented :: (HasChars a, IsString a)
               => String -> Doc a -> Doc a
inTagsIndented = ton TP.inTagsIndented

fromEntities :: String -> String
fromEntities = taround TP.fromEntities
