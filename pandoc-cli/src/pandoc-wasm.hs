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
import qualified Control.Exception as E
import Data.Maybe (fromMaybe)
import Text.Pandoc.App ( convertWithOpts, Opt(..) )
import PandocCLI.Lua
import Control.Exception
import Foreign
import Foreign.C
import qualified Data.Aeson as Aeson
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc (Verbosity(ERROR))

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
        Right opts -> do
          let opts' = opts{ optInputFiles =
                             Just $ fromMaybe ["/stdin"] (optInputFiles opts)
                          , optOutputFile =
                             Just $ fromMaybe "/stdout" (optOutputFile opts)
                          , optLogFile =
                             Just $ fromMaybe "/warnings" (optLogFile opts)
                          , optVerbosity = ERROR -- only show errors to stderr
                          }
          convertWithOpts engine opts'

-- This must be included or we get an error:
main :: IO ()
main = pure ()
