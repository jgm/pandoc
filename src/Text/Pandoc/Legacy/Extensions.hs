module Text.Pandoc.Legacy.Extensions
  ( TP.Extension(..)
  , TP.Extensions
  , TP.emptyExtensions
  , TP.extensionsFromList
  , parseFormatSpec
  , TP.extensionEnabled
  , TP.enableExtension
  , TP.disableExtension
  , getDefaultExtensions
  , getAllExtensions
  , TP.pandocExtensions
  , TP.plainExtensions
  , TP.strictExtensions
  , TP.phpMarkdownExtraExtensions
  , TP.githubMarkdownExtensions
  , TP.multimarkdownExtensions )
where

import qualified Text.Pandoc.Extensions as TP
import qualified Data.Text as T
import Text.Parsec (ParseError)

parseFormatSpec :: String
                -> Either ParseError (String, [TP.Extension], [TP.Extension])
parseFormatSpec = either Left (Right . go) . TP.parseFormatSpec . T.pack
  where
    go (x, y, z) = (T.unpack x, y, z)

getDefaultExtensions :: String -> TP.Extensions
getDefaultExtensions = TP.getDefaultExtensions . T.pack

getAllExtensions :: String -> TP.Extensions
getAllExtensions = TP.getAllExtensions . T.pack
