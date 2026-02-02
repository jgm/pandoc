{-# LANGUAGE ScopedTypeVariables #-}

{- |
   Module      : Main
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Parses command-line options and calls the appropriate readers and
writers (wasm version).
-}
module Main where
import qualified Data.Map as M
import Text.Read (readMaybe)
import qualified Control.Exception as E
import Data.Maybe (fromMaybe)
import Text.Pandoc.App ( convertWithOpts, Opt(..), defaultOpts )
import Text.Pandoc (Verbosity(ERROR))
import Text.Pandoc.Extensions (extensionsToList, extensionEnabled, getAllExtensions,
                               getDefaultExtensions)
import PandocCLI.Lua
import Control.Exception
import Foreign
import Foreign.C
import qualified Data.Aeson as Aeson
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BL

foreign export ccall "wasm_main" wasm_main :: Ptr CChar -> Int -> IO ()

-- | The parameters are a pointer and length for a C string
-- containing JSON-encoded pandoc options (isomorphic to a defaults
-- file).  The calling program should set up a virtual file system
-- containing @/stdin@ (input), @/stdout@ (output), and @/warnings@ (output).
-- @/stdin@ can be used to pass input to pandoc.
wasm_main :: Ptr CChar -> Int -> IO ()
wasm_main ptr len =
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

foreign export ccall "get_extensions_for_format" get_extensions_for_format
   :: Ptr CChar -> Int -> IO ()

-- | Given a C string with the format name, returnsa  JSON object
-- mapping extensions relevant for this format to true or false,
-- depending on whether they are enabled by default.
get_extensions_for_format :: Ptr CChar -> Int -> IO ()
get_extensions_for_format ptr len = do
  formatName <- readMaybe <$> getCString ptr len
  case formatName of
    Just fmt -> do
       let allExts = getAllExtensions fmt
       let defExts = getDefaultExtensions fmt
       let addExt x = M.insert (drop 4 (show x)) (extensionEnabled x defExts)
       BL.writeFile "/stdout" $ Aeson.encode $ foldr addExt mempty (extensionsToList allExts)
    Nothing -> writeFile "/stdout" "{}"

getCString :: Ptr CChar -> Int -> IO String
getCString ptr len = peekCStringLen (ptr, len) <* free ptr

-- This must be included or we get an error:
main :: IO ()
main = pure ()
