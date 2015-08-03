{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Format
   Copyright   : © 2022-2024 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Handling of format specifiers for input and output.
-}
module Text.Pandoc.Format
  ( FlavoredFormat (..)
  , ExtensionsConfig (..)
  , ExtensionsDiff (..)
  , diffExtensions
  , parseFlavoredFormat
  , applyExtensionsDiff
  , getExtensionsConfig
  , formatFromFilePaths
  ) where

import Control.Monad.Except (throwError)
import Data.Char (toLower)
import Data.Foldable (asum)
import Data.List (foldl')
import System.FilePath (splitExtension, takeExtension)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.Extensions
  ( Extension (Ext_literate_haskell)
  , Extensions
  , disableExtensions
  , enableExtension
  , extensionsFromList
  , extensionsToList
  , getAllExtensions
  , getDefaultExtensions
  , showExtension
  , readExtension
  )
import Network.URI (URI (..), parseURI)
import Text.Pandoc.Parsing
import qualified Data.Text as T

-- | Format specifier with the format's name and the lists of extensions
-- to be enabled or disabled.
data FlavoredFormat = FlavoredFormat
  { formatName     :: T.Text
  , formatExtsDiff :: ExtensionsDiff
  } deriving (Show)

-- | Changes to a set of extensions, i.e., list of extensions to be
-- enabled or disabled.
data ExtensionsDiff = ExtensionsDiff
  { extsToEnable  :: Extensions
  , extsToDisable :: Extensions
  } deriving (Show)

instance Semigroup ExtensionsDiff where
  ExtensionsDiff enA disA <> ExtensionsDiff enB disB =
    ExtensionsDiff
    ((enA `disableExtensions` disB) <> enB)
    ((disA `disableExtensions` enB) <> disB)

instance Monoid ExtensionsDiff where
  mempty = ExtensionsDiff mempty mempty
  mappend = (<>)

-- | Calculate the change set to get from one set of extensions to
-- another.
diffExtensions :: Extensions -> Extensions -> ExtensionsDiff
diffExtensions def actual = ExtensionsDiff
  { extsToEnable = actual `disableExtensions` def
  , extsToDisable = def `disableExtensions` actual
  }

-- | Describes the properties of a format.
data ExtensionsConfig = ExtensionsConfig
  { extsDefault   :: Extensions -- ^ Extensions enabled by default
  , extsSupported :: Extensions -- ^ Extensions that can be enabled or disabled.
  } deriving (Show)

-- | Returns the extensions configuration of a format.
getExtensionsConfig :: T.Text -> ExtensionsConfig
getExtensionsConfig fmt = ExtensionsConfig
  { extsDefault = getDefaultExtensions fmt
  , extsSupported = getAllExtensions fmt
  }

instance Semigroup ExtensionsConfig where
  ExtensionsConfig x1 y1 <> ExtensionsConfig x2 y2 =
    ExtensionsConfig (x1 <> x2) (y1 <> y2)

instance Monoid ExtensionsConfig where
  mappend = (<>)
  mempty = ExtensionsConfig mempty mempty

-- | Apply the extension changes in the format spec to the extensions
-- given in the format's extensions configuration. Throws an error in
-- case of an unknown or unsupported extension.
applyExtensionsDiff :: PandocMonad m
                    => ExtensionsConfig
                    -> FlavoredFormat
                    -> m Extensions
applyExtensionsDiff extConf (FlavoredFormat fname extsDiff) = do
  let extsInDiff  = extsToEnable extsDiff <> extsToDisable extsDiff
  let unsupported = extsInDiff `disableExtensions` (extsSupported extConf)
  case extensionsToList unsupported of
    ext:_ -> throwError $ PandocUnsupportedExtensionError
             (showExtension ext) fname
    []    -> pure ((extsDefault extConf `disableExtensions`
                    extsToDisable extsDiff) <> extsToEnable extsDiff)

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
      pos <- getPosition
      setPosition (incSourceColumn pos (T.length prefix))
    formatSpec = do
      name <- parseFormatName
      extsDiff <- pExtensionsDiff
      return ( T.pack name, extsDiff )
    parseFormatName = many1 $ noneOf "-+"
    (prefix, spec') = case splitExtension (T.unpack spec) of
                        (_, "") -> ("", T.toLower spec) -- no extension
                        (p,s)   -> (T.pack p, T.pack s)

pExtensionsDiff :: (UpdateSourcePos s Char, Stream s m Char)
                => ParsecT s u m ExtensionsDiff
pExtensionsDiff = foldl' (flip ($)) mempty <$> many extMod
  where
    extMod = do
      polarity <- oneOf "-+"
      name <- many $ noneOf "-+"
      let ext = readExtension name
      return $ \extsDiff ->
        case polarity of
          '+' -> extsDiff{extsToEnable  = enableExtension ext $
                                          extsToEnable extsDiff}
          _   -> extsDiff{extsToDisable = enableExtension ext $
                                          extsToDisable extsDiff}

-- | Determines default format based on file extensions; uses the format
-- of the first extension that's associated with a format.
--
-- Examples:
--
-- > formatFromFilePaths ["text.unknown", "no-extension"]
-- Nothing
--
-- > formatFromFilePaths ["my.md", "other.rst"]
-- Just "markdown"
formatFromFilePaths :: [FilePath] -> (Maybe FlavoredFormat)
formatFromFilePaths = asum . map formatFromFilePath

-- | Determines format based on file extension.
formatFromFilePath :: FilePath -> Maybe FlavoredFormat
formatFromFilePath x =
  case takeExtension (map toLower fpath) of
    ".Rmd"      -> defFlavor "markdown"
    ".adoc"     -> defFlavor "asciidoc"
    ".asciidoc" -> defFlavor "asciidoc"
    ".bib"      -> defFlavor "biblatex"
    ".context"  -> defFlavor "context"
    ".csv"      -> defFlavor "csv"
    ".ctx"      -> defFlavor "context"
    ".db"       -> defFlavor "docbook"
    ".dj"       -> defFlavor "djot"
    ".doc"      -> defFlavor "doc"  -- so we get an "unknown reader" error
    ".docx"     -> defFlavor "docx"
    ".dokuwiki" -> defFlavor "dokuwiki"
    ".epub"     -> defFlavor "epub"
    ".fb2"      -> defFlavor "fb2"
    ".htm"      -> defFlavor "html"
    ".html"     -> defFlavor "html"
    ".icml"     -> defFlavor "icml"
    ".ipynb"    -> defFlavor "ipynb"
    ".json"     -> defFlavor "json"
    ".latex"    -> defFlavor "latex"
    ".lhs"      -> defFlavor "markdown" `withExtension` Ext_literate_haskell
    ".ltx"      -> defFlavor "latex"
    ".markdown" -> defFlavor "markdown"
    ".markua"   -> defFlavor "markua"
    ".md"       -> defFlavor "markdown"
    ".mdown"    -> defFlavor "markdown"
    ".mdwn"     -> defFlavor "markdown"
    ".mkd"      -> defFlavor "markdown"
    ".mkdn"     -> defFlavor "markdown"
    ".ms"       -> defFlavor "ms"
    ".muse"     -> defFlavor "muse"
    ".native"   -> defFlavor "native"
    ".odt"      -> defFlavor "odt"
    ".opml"     -> defFlavor "opml"
    ".org"      -> defFlavor "org"
    ".pdf"      -> defFlavor "pdf"  -- so we get an "unknown reader" error
    ".pptx"     -> defFlavor "pptx"
    ".ris"      -> defFlavor "ris"
    ".roff"     -> defFlavor "ms"
    ".rst"      -> defFlavor "rst"
    ".rtf"      -> defFlavor "rtf"
    ".s5"       -> defFlavor "s5"
    ".sil"      -> defFlavor "sile"
    ".t2t"      -> defFlavor "t2t"
    ".tei"      -> defFlavor "tei"
    ".tex"      -> defFlavor "latex"
    ".texi"     -> defFlavor "texinfo"
    ".texinfo"  -> defFlavor "texinfo"
    ".text"     -> defFlavor "markdown"
    ".textile"  -> defFlavor "textile"
    ".tsv"      -> defFlavor "tsv"
    ".txt"      -> defFlavor "markdown"
    ".typ"      -> defFlavor "typst"
    ".wiki"     -> defFlavor "mediawiki"
    ".xhtml"    -> defFlavor "html"
    ['.',y]     | y `elem` ['1'..'9'] -> defFlavor "man"
    _           -> Nothing
 where
  defFlavor f = Just (FlavoredFormat f mempty)
  withExtension Nothing _ = Nothing
  withExtension (Just (FlavoredFormat f ed)) ext = Just $
    FlavoredFormat f (ed <> ExtensionsDiff (extensionsFromList [ext]) mempty)
  fpath = case parseURI x of
            Nothing -> x
            Just URI{ uriPath = "" } -> "index.html"
            Just URI{ uriPath = "/" } -> "index.html"
            Just URI{ uriPath = up } -> up
