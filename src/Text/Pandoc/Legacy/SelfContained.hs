module Text.Pandoc.Legacy.SelfContained ( makeDataURI, makeSelfContained ) where

import Prelude
import Data.ByteString (ByteString)
import Text.Pandoc.Legacy.Class (PandocMonad (..))
import Text.Pandoc.Legacy.MIME (MimeType)

import qualified Text.Pandoc.SelfContained as TP
import qualified Data.Text as T

makeDataURI :: (MimeType, ByteString) -> String
makeDataURI (mime, raw) = T.unpack $ TP.makeDataURI (T.pack mime, raw)

makeSelfContained :: PandocMonad m => String -> m String
makeSelfContained = fmap T.unpack . TP.makeSelfContained . T.pack
