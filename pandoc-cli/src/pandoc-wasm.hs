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

wasm_main :: Ptr CChar -> Int -> IO ()
wasm_main raw_args_ptr raw_args_len =
  E.catch act (\(err :: SomeException) ->
                 writeFile "/stderr" ("ERROR: " <> displayException err))
  where
    act = do
      args <- peekCStringLen (raw_args_ptr, raw_args_len)
      free raw_args_ptr
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

foreign export ccall "get_extensions_for_format" getExtensionsForFormat :: Ptr CChar -> Int -> IO ()

getExtensionsForFormat :: Ptr CChar -> Int -> IO ()
getExtensionsForFormat raw_fmt_ptr raw_fmt_len = do
  formatName <- readMaybe <$> peekCStringLen (raw_fmt_ptr, raw_fmt_len)
  free raw_fmt_ptr
  case formatName of
    Just fmt -> do
       let allExts = getAllExtensions fmt
       let defExts = getDefaultExtensions fmt
       let addExt x = M.insert (drop 4 (show x)) (extensionEnabled x defExts)
       BL.writeFile "/stdout" $ Aeson.encode $ foldr addExt mempty (extensionsToList allExts)
    Nothing -> writeFile "/stdout" "{}"

-- This must be included or we get an error:
main :: IO ()
main = pure ()
