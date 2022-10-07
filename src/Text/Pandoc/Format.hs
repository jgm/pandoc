{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Format
   Copyright   : © 2022 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <pandoc@tarleb.com>

Handling of format specifiers for input and output.
-}
module Text.Pandoc.Format
  ( FlavoredFormat (..)
  , ExtensionsConfig (..)
  , ExtensionsDiff (..)
  , parseFlavoredFormat
  , applyExtensionsDiff
  , getExtensionsConfig
  ) where

import Control.Monad.Except (throwError)
import Data.List (foldl')
import System.FilePath (splitExtension)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.Extensions
  ( Extension
  , Extensions
  , disableExtension
  , enableExtension
  , extensionEnabled
  , getAllExtensions
  , getDefaultExtensions
  , readExtension
  )
import Text.Parsec
import qualified Data.Text as T

-- | Format specifier with the format's name and the lists of extensions
-- to be enabled or disabled.
data FlavoredFormat = FlavoredFormat
  { formatName     :: T.Text
  , formatExtsDiff :: ExtensionsDiff
  }

-- | Changes to a set of extensions, i.e., list of extensions to be
-- enabled or disabled.
data ExtensionsDiff = ExtensionsDiff
  { extsToEnable  :: [Extension]
  , extsToDisable :: [Extension]
  }

-- | Describes the properties of a format.
data ExtensionsConfig = ExtensionsConfig
  { extsDefault   :: Extensions -- ^ Extensions enabled by default
  , extsSupported :: Extensions -- ^ Extensions that can be enabled or disabled.
  }

-- | Returns the extensions configuration of a format.
getExtensionsConfig :: T.Text -> ExtensionsConfig
getExtensionsConfig fmt = ExtensionsConfig
  { extsDefault = getDefaultExtensions fmt
  , extsSupported = getAllExtensions fmt
  }

-- | Apply the extension changes in the format spec to the extensions
-- given in the format's extensions configuration. Throws an error in
-- case of an unknown or unsupported extension.
applyExtensionsDiff :: PandocMonad m
                    => ExtensionsConfig
                    -> FlavoredFormat
                    -> m Extensions
applyExtensionsDiff extConf (FlavoredFormat fname extsDiff) = do
  let unsupported =
        filter (\ext -> not $ extensionEnabled ext (extsSupported extConf))
               (extsToEnable extsDiff ++ extsToDisable extsDiff)
  case unsupported of
    ext:_ -> throwError $ PandocUnsupportedExtensionError
             (T.drop 4 . T.pack $ show ext) fname
    []    -> let enabled = foldr enableExtension
                                 (extsDefault extConf)
                                 (extsToEnable extsDiff)
             in pure $ foldr disableExtension enabled (extsToDisable extsDiff)

-- | Parse a format-specifying string into a markup format and the
-- change set to the format's extensions. Throws an error if the spec
-- cannot be parsed or contains an unknown extension.
parseFlavoredFormat :: PandocMonad m
                    => T.Text
                    -> m FlavoredFormat
parseFlavoredFormat spec =
  -- Paths like `latex-foo-bar.lua` or `latex-smart-citations.lua`
  -- should be parsed as the format name. The `-` (or `+`) in the
  -- filename would confuse the extensions parser, so, if `spec` looks
  -- like a filename, the file's basename is split off into the prefix.
  -- Only the remaining part is parsed, and the prefix is appended back
  -- to the format after parsing.
  case parse (fixSourcePos *> formatSpec) "" spec' of
    Right (fname, extsDiff) -> pure (FlavoredFormat (prefix <> fname) extsDiff)
    Left err -> throwError $ PandocFormatError spec (T.pack $ show err)
  where
    fixSourcePos = do
      pos <- statePos <$> getParserState
      setPosition (incSourceColumn pos (T.length prefix))
    formatSpec = do
      name <- parseFormatName
      extsDiff <- pExtensionsDiff
      return ( T.pack name, extsDiff )
    parseFormatName = many1 $ noneOf "-+"
    (prefix, spec') = case splitExtension (T.unpack spec) of
                        (_, "") -> ("", T.toLower $ spec) -- no extension
                        (p,s)   -> (T.pack p, T.pack s)

pExtensionsDiff :: Stream s m Char => ParsecT s u m ExtensionsDiff
pExtensionsDiff = foldl' (flip ($)) (ExtensionsDiff [] []) <$> many extMod
  where
    extMod = do
      polarity <- oneOf "-+"
      name <- many $ noneOf "-+"
      ext <- case readExtension name of
               Just n  -> return n
               Nothing -> unexpected $ "unknown extension: " ++ name
      return $ \extsDiff ->
        case polarity of
          '+' -> extsDiff{extsToEnable  = (ext : extsToEnable extsDiff)}
          _   -> extsDiff{extsToDisable = (ext : extsToDisable extsDiff)}
