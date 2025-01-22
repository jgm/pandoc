{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
   Module      : Main
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Parses command-line options and calls the appropriate readers and
writers.
-}
module Main where
import qualified Control.Exception as E
import System.Environment (getArgs, getProgName)
import Text.Pandoc.App ( convertWithOpts, defaultOpts, options
                       , parseOptionsFromArgs, handleOptInfo )
import Text.Pandoc.Error (handleError)
import System.Exit (exitSuccess)
import Data.Monoid (Any(..))
import PandocCLI.Lua
import PandocCLI.Server
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Version (pandocVersion)
import Text.Pandoc.Data (defaultUserDataDir)
import Text.Pandoc.Scripting (ScriptingEngine(..))
import Data.Version (showVersion)
import qualified Data.Text as T

#ifdef NIGHTLY
import qualified Language.Haskell.TH as TH
import Data.Time
#endif

#if defined(wasm32_HOST_ARCH)
import Control.Exception
import Foreign
import Foreign.C
import System.IO
#endif

#ifdef NIGHTLY
versionSuffix :: String
versionSuffix = "-nightly-" ++
  $(TH.stringE =<<
    TH.runIO (formatTime defaultTimeLocale "%F" <$> Data.Time.getCurrentTime))
#else
versionSuffix :: String
versionSuffix = ""
#endif

#if defined(wasm32_HOST_ARCH)

foreign export ccall "wasm_main" wasm_main :: Ptr CChar -> Int -> IO ()

wasm_main :: Ptr CChar -> Int -> IO ()
wasm_main raw_args_ptr raw_args_len =
  catch act (\(err :: SomeException) -> hPrint stderr err)
  where
    act = do
      args <- words <$> peekCStringLen (raw_args_ptr, raw_args_len)
      free raw_args_ptr
      engine <- getEngine
      res <- parseOptionsFromArgs options defaultOpts "pandoc.wasm" $ args <> ["/in", "-o", "/out"]
      case res of
        Left e -> handleOptInfo engine e
        Right opts -> convertWithOpts engine opts
#endif

main :: IO ()
main = E.handle (handleError . Left) $ do
  prg <- getProgName
  rawArgs <- getArgs
  let hasVersion = getAny $ foldMap
         (\s -> Any (s == "-v" || s == "--version"))
         (takeWhile (/= "--") rawArgs)
  let versionOr action = if hasVersion then versionInfo else action
  case prg of
    "pandoc-server.cgi" -> versionOr runCGI
    "pandoc-server"     -> versionOr $ runServer rawArgs
    "pandoc-lua"        -> runLuaInterpreter prg rawArgs
    _ ->
      case rawArgs of
        "lua" : args   -> runLuaInterpreter "pandoc lua" args
        "server": args -> versionOr $ runServer args
        args           -> versionOr $ do
          engine <- getEngine
          res <- parseOptionsFromArgs options defaultOpts prg args
          case res of
            Left e -> handleOptInfo engine e
            Right opts -> convertWithOpts engine opts

copyrightMessage :: String
copyrightMessage =
 "Copyright (C) 2006-2024 John MacFarlane. Web: https://pandoc.org\n"
 ++
 "This is free software; see the source for copying conditions. There is no\n"
 ++
 "warranty, not even for merchantability or fitness for a particular purpose."

flagSettings :: String
flagSettings = "Features: " ++
#ifdef VERSION_pandoc_server
  "+server"
#else
  "-server"
#endif
  ++ " " ++
#ifdef VERSION_hslua_cli
  "+lua"
#else
  "-lua"
#endif

versionInfo :: IO ()
versionInfo = do
  progname <- getProgName
  defaultDatadir <- defaultUserDataDir
  scriptingEngine <- getEngine
  UTF8.putStr $ T.unlines $ map T.pack
   [ progname ++ " " ++ showVersion pandocVersion ++ versionSuffix
   , flagSettings
   , "Scripting engine: " ++ T.unpack (engineName scriptingEngine)
   , "User data directory: " ++ defaultDatadir
   , copyrightMessage
   ]
  exitSuccess
