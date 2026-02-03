{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : Main
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Exposes wasm functions to convert documents and get information
from pandoc.
-}
module PandocWasm where
import qualified Data.Map as M
import qualified Control.Exception as E
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Text.Pandoc.App ( convertWithOpts, Opt(..), defaultOpts )
import Text.Pandoc (Verbosity(ERROR), pandocVersion, Reader, Writer, PandocIO,
                    readers, writers, runIO, setUserDataDir)
import Text.Pandoc.Highlighting (highlightingStyles)
import Text.Pandoc.Templates (getDefaultTemplate)
import Skylighting (defaultSyntaxMap, Syntax(..))
import Text.Pandoc.Extensions (extensionsToList, extensionEnabled, getAllExtensions,
                               getDefaultExtensions)
import Text.Pandoc.Error
import Text.Pandoc.Scripting (ScriptingEngine(..), customTemplate)
import System.FilePath (splitExtension)
import PandocCLI.Lua
import Control.Exception
import Foreign
import Foreign.C
import Data.Aeson as Aeson
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Version (showVersion)

foreign export ccall "convert" convert :: Ptr CChar -> Int -> IO ()

-- | Do a pandoc conversion. The parameters are a pointer and length
-- for a C string containing JSON-encoded pandoc options (isomorphic
-- to a defaults file). The calling program should set up a virtual
-- file system containing @/stdin@ (input), @/stdout@ (output), and
-- @/warnings@ (output). @/stdin@ can be used to pass input to pandoc.
convert :: Ptr CChar -> Int -> IO ()
convert ptr len =
  E.catch act (\(err :: SomeException) ->
                 writeFile "/stderr" ("ERROR: " <> displayException err))
  where
    act = do
      args <- getCString ptr len
      engine <- getEngine
      let aesonRes = Aeson.eitherDecode (UTF8.fromStringLazy args)
      case aesonRes of
        Left e -> error e
        Right (f :: Opt -> Opt) -> do
          let opts = f defaultOpts
          let opts' = opts{ optInputFiles =
                             Just $ fromMaybe ["/stdin"] (optInputFiles opts)
                          , optOutputFile =
                             Just $ fromMaybe "/stdout" (optOutputFile opts)
                          , optLogFile =
                             Just $ fromMaybe "/warnings" (optLogFile opts)
                          , optVerbosity = ERROR -- only show errors to stderr
                          }
          convertWithOpts engine opts'

foreign export ccall "query" query :: Ptr CChar -> Int -> IO ()

-- | Retrieve information from pandoc. The parameters are a pointer
-- and length for a C string containing a JSON-encoded query.
-- The calling program should set up a virtual file system containing
-- @/stdout@, which will be used to hold the output, and @/stderr@,
-- which will be used to hold any error messages.
-- The following queries can be used:
-- * @{"query":"version"}@ returns the pandoc version as a JSON-encoded
--   string.
-- * @{"query:"input-formats"}@ returns a JSON list of supported input
--   formats.
-- * @{"query:"output-formats"}@ returns a JSON list of supported output
--   formats.
-- * @{"query:"highlight-languages"}@ returns a JSON list of languages
--   for which syntax highlighting is defined.
-- * @{"query:"highlight-styles"}@ returns a JSON list of defined syntax
--   highlighting styles.
-- * @{"query":"default-template","format": FORMAT}@ returns
--   the default template for the format as a JSON-encoded string.
-- * @{"query":"extensions-for-format","format": FORMAT}@ returns
--   an object mapping extension names to true or false for all the
--   extensions relevant for the format.
query :: Ptr CChar -> Int -> IO ()
query ptr len =
  E.catch act (\(err :: SomeException) ->
                 writeFile "/stderr" ("ERROR: " <> displayException err))
  where
    jsonOut :: forall a . ToJSON a => a -> IO ()
    jsonOut = BL.writeFile "/stdout" . Aeson.encode
    act = do
      args <- getCString ptr len
      case Aeson.eitherDecode (UTF8.fromStringLazy args) of
        Left e -> error e
        Right PandocVersion -> jsonOut $ showVersion pandocVersion
        Right HighlightStyles -> jsonOut $ map fst highlightingStyles
        Right HighlightLanguages -> jsonOut $ sort $
          [ T.toLower (sShortname s)
            | s <- M.elems defaultSyntaxMap
            , sShortname s `notElem` ["Alert", "Alert_indent"]
          ]
        Right (DefaultTemplate format) -> do
          templ <- runIO $
                     case splitExtension (T.unpack format) of
                        (_, "") -> do
                          -- built-in format
                          setUserDataDir Nothing
                          getDefaultTemplate format
                        _ -> do
                          -- format looks like a filepath => custom writer
                          engine <- getEngine
                          components <- engineLoadCustom engine (T.unpack format)
                          case customTemplate components of
                            Just t  -> pure t
                            Nothing -> E.throw $ PandocNoTemplateError format
          case templ of
               Right t
                 | T.null t -> -- e.g. for docx, odt, json:
                     E.throwIO $ PandocCouldNotFindDataFileError $ T.pack
                       ("templates/default." ++ T.unpack format)
                 | otherwise -> jsonOut t
               Left e  -> E.throwIO e
        Right InputFormats -> jsonOut readersNames
        Right OutputFormats -> jsonOut writersNames
        Right (ExtensionsForFormat format) -> do
          let allExts = getAllExtensions format
          let defExts = getDefaultExtensions format
          let addExt x = M.insert (drop 4 (show x))
                              (extensionEnabled x defExts)
          jsonOut $ foldr addExt mempty (extensionsToList allExts)
    readersNames = sort (map fst (readers :: [(T.Text, Reader PandocIO)]))
    writersNames = sort
      ("pdf" : map fst (writers :: [(T.Text, Writer PandocIO)]))


data Query =
    PandocVersion
  | InputFormats
  | OutputFormats
  | HighlightLanguages
  | HighlightStyles
  | ExtensionsForFormat T.Text
  | DefaultTemplate T.Text
  deriving (Show)

instance FromJSON Query where
  parseJSON = withObject "Query" $ \o -> do
    queryType <- o .: "query"
    case queryType of
      "version" -> pure PandocVersion
      "input-formats" -> pure InputFormats
      "output-formats" -> pure OutputFormats
      "highlight-languages" -> pure HighlightLanguages
      "highlight-styles" -> pure HighlightStyles
      "default-template" -> DefaultTemplate <$> o .: "format"
      "extensions-for-format" -> ExtensionsForFormat <$> o .: "format"
      _ -> fail $ "Unknown query type " <> queryType

getCString :: Ptr CChar -> Int -> IO String
getCString ptr len = peekCStringLen (ptr, len) <* free ptr
