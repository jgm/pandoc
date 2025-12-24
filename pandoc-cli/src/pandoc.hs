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
import Data.Maybe (fromMaybe)
import Text.Pandoc.App ( convertWithOpts, defaultOpts, options, Opt(..)
                       , parseOptionsFromArgs, handleOptInfo, versionInfo )
import Text.Pandoc.Error (handleError)
import Data.Monoid (Any(..))
import PandocCLI.Lua
import PandocCLI.Server
import Text.Pandoc.Scripting (ScriptingEngine(..))
import qualified Data.Text as T

#ifdef NIGHTLY
import qualified Language.Haskell.TH as TH
import Data.Time
#endif

#if defined(wasm32_HOST_ARCH)
import Control.Exception
import Foreign
import Foreign.C
import qualified Data.Aeson as Aeson
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BL
import Text.Pandoc.Error (renderError, PandocError)
import Text.Pandoc.Logging (showLogMessage)
#else
import System.Exit
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
  catch act (\(err :: SomeException) -> writeFile "/stderr" (show err))
  where
    act = do
      args <- peekCStringLen (raw_args_ptr, raw_args_len)
      free raw_args_ptr
      engine <- getEngine
      let aesonRes = Aeson.eitherDecode (UTF8.fromStringLazy args)
      case aesonRes of
        Left e -> error e
        Right opts -> do
          let opts' = opts{ optInputFiles = Just $ fromMaybe ["/stdin"] (optInputFiles opts)
                          , optOutputFile = Just $ fromMaybe "/stdout" (optOutputFile opts)
                          , optLogFile = Just $ fromMaybe "/warnings" (optLogFile opts)
                          }
          E.catch (convertWithOpts engine opts') $
            \(e :: PandocError) -> TIO.writeFile "/stderr" (renderError e)
          res <- Aeson.eitherDecode <$> BL.readFile "/warnings"
          case res of
            Left e -> writeFile "/stderr" e
            Right msgs -> TIO.writeFile "/stderr" (T.unlines $ map showLogMessage msgs)

#endif

main :: IO ()
main = E.handle (handleError . Left) $ do
  prg <- getProgName
  rawArgs <- getArgs
  let hasVersion = getAny $ foldMap
         (\s -> Any (s == "-v" || s == "--version"))
         (takeWhile (/= "--") rawArgs)
  let versionOr action = if hasVersion then versionInfoCLI else action
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


getFeatures :: [String]
getFeatures = [
#ifdef VERSION_pandoc_server
  "+server"
#else
  "-server"
#endif
  ,
#ifdef VERSION_hslua_cli
  "+lua"
#else
  "-lua"
#endif
  ]

versionInfoCLI :: IO ()
versionInfoCLI = do
  scriptingEngine <- getEngine
  versionInfo getFeatures
              (Just $ T.unpack (engineName scriptingEngine))
              versionSuffix
