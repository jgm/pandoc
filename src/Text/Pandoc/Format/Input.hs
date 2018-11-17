{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Text.Pandoc.Format.Input
  ( InputFormat (..)
  , allInputFormats
  , name
  , fromName
  , defaultFlavor
  , flavoredFromSpec
  , flavoredFormatFromFilePath
  , flavoredFormatFromFilePaths
  , module Text.Pandoc.Format.Flavored
  ) where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Data.Char (toLower)
import Data.Data (Data)
import Data.Map.Strict (Map)
import Data.Monoid (First (..))
import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.FilePath (takeExtension)
import Text.Pandoc.Error
import Text.Pandoc.Format.Flavored
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Formats that pandoc knows how to parse.
data InputFormat
  = BibLaTeX
  | BibTeX
  | CSV
  | CommonMark
  | CommonMark_X
  | Creole
  | CslJson
  | DocBook
  | Docx
  | DokuWiki
  | EndNoteXML
  | EPUB
  | FB2
  | GFM
  | HTML
  | Haddock
  | Ipynb
  | JATS
  | JSON
  | Jira
  | LaTeX
  | Man
  | Markdown
  | Markdown_GitHub
  | Markdown_MMD
  | Markdown_PHPExtra
  | Markdown_strict
  | MediaWiki
  | Muse
  | Native
  | ODT
  | OPML
  | Org
  | RIS
  | RST
  | TSV
  | TWiki
  | Textile
  | TikiWiki
  | T2T       -- txt2tags
  | Vimwiki
  deriving (Bounded, Enum, Eq, Data, Generic, Ord, Read, Show, Typeable)

name :: InputFormat -> Text
name = T.toLower . T.pack . show

-- | List of all formats of which pandoc is aware.
allInputFormats :: Set InputFormat
allInputFormats = Set.fromAscList [minBound .. maxBound]

names :: Map Text InputFormat
names = Map.fromList $
  map (\f -> (T.toLower (T.pack $ show f), f)) (Set.toList allInputFormats)

-- | Get a format from a string identifier.
fromName :: Text -> Maybe InputFormat
fromName = flip Map.lookup names

-- | Returns the default flavor of an input format.
defaultFlavor :: InputFormat -> Flavored InputFormat
defaultFlavor f = KnownFormat f (getDefaultExtensions $ name f)

-- | Determine format based on file extensions
flavoredFormatFromFilePaths :: [FilePath] -> Maybe (Flavored InputFormat)
flavoredFormatFromFilePaths = getFirst .
  foldMap (First . flavoredFormatFromFilePath)

-- | Determine format based on file extension
flavoredFormatFromFilePath :: FilePath -> Maybe (Flavored InputFormat)
flavoredFormatFromFilePath fp =
  let
    defaultExts = return . defaultFlavor
    modifiedExts f extsMod = return $
      KnownFormat f (extsMod . getDefaultExtensions $ name f)
  in case takeExtension (map toLower fp) of
      ".db"       -> defaultExts DocBook
      ".docx"     -> defaultExts Docx
      ".dokuwiki" -> defaultExts DokuWiki
      ".epub"     -> defaultExts EPUB
      ".fb2"      -> defaultExts FB2
      ".htm"      -> defaultExts HTML
      ".html"     -> defaultExts HTML
      ".json"     -> defaultExts JSON
      ".latex"    -> defaultExts LaTeX
      ".lhs"      -> modifiedExts Markdown (enableExtension Ext_literate_haskell)
      ".ltx"      -> defaultExts LaTeX
      ".markdown" -> defaultExts Markdown
      ".md"       -> defaultExts Markdown
      ".muse"     -> defaultExts Muse
      ".native"   -> defaultExts Native
      ".opml"     -> defaultExts OPML
      ".org"      -> defaultExts Org
      ".rst"      -> defaultExts RST
      ".t2t"      -> defaultExts T2T
      ".tex"      -> defaultExts LaTeX
      ".text"     -> defaultExts Markdown
      ".textile"  -> defaultExts Textile
      ".txt"      -> defaultExts Markdown
      ".wiki"     -> defaultExts MediaWiki
      ".xhtml"    -> defaultExts HTML
      ['.',y]     | y `elem` ['1'..'9'] -> defaultExts Man
      _           -> Nothing

-- | Parses an input format spec.
flavoredFromSpec :: PandocMonad m => Text -> m (Flavored InputFormat)
flavoredFromSpec s =
  case parseFormatSpec s of
    Left e  -> throwError $ PandocAppError $ mconcat
               [ "Error parsing reader format "
               , T.pack (show s)
               , ": "
               , T.pack (show e)
               ]
    Right (readerName, extsToEnable, extsToDisable) ->
      case fromName readerName of
        Nothing           -> if ".lua" `T.isSuffixOf` readerName
                             then return $ CustomFormat (T.unpack readerName)
                             else throwError $ PandocUnknownReaderError readerName
        Just inputFormat  -> do
          let allExts = getAllExtensions readerName
          let exts = foldr disableExtension
                (foldr enableExtension
                  (getDefaultExtensions readerName)
                        extsToEnable) extsToDisable
          mapM_ (\ext ->
                   unless (extensionEnabled ext allExts) $
                     throwError $
                        PandocUnsupportedExtensionError
                        (T.drop 4 $ T.pack $ show ext) readerName)
               (extsToEnable ++ extsToDisable)
          return $ KnownFormat inputFormat exts
