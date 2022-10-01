{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{- |
   Module      : Main
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
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
                       , parseOptionsFromArgs )
import Text.Pandoc.Error (handleError)
import qualified Text.Pandoc.UTF8 as UTF8
import System.Exit (exitSuccess)
import Data.Monoid (Any(..))
import Control.Monad (when)
import PandocCLI.Lua
import PandocCLI.Server
import Text.Pandoc.Shared (pandocVersion, defaultUserDataDir)
import Text.Pandoc.Scripting (ScriptingEngine(..))
import Data.Version (showVersion)
import qualified Data.Text as T

#ifdef NIGHTLY
import qualified Language.Haskell.TH as TH
import Data.Time
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

main :: IO ()
main = E.handle (handleError . Left) $ do
  prg <- getProgName
  rawArgs <- map UTF8.decodeArg <$> getArgs
  let hasVersion = getAny $ foldMap
         (\s -> Any (s == "-v" || s == "--version"))
         (takeWhile (/= "--") rawArgs)
  when hasVersion versionInfo
  case prg of
    "pandoc-server.cgi" -> runCGI
    "pandoc-server"     -> runServer rawArgs
    "pandoc-lua"        -> runLuaInterpreter prg rawArgs
    _ ->
      case rawArgs of
        "lua" : args   -> runLuaInterpreter "pandoc lua" args
        "server": args -> runServer args
        _              -> do
          engine <- getEngine
          let cliOpts = options engine
          opts <- parseOptionsFromArgs cliOpts defaultOpts prg rawArgs
          convertWithOpts engine opts

copyrightMessage :: String
copyrightMessage =
 "Copyright (C) 2006-2022 John MacFarlane. Web:  https://pandoc.org\n"
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
  putStr $ unlines
   [ progname ++ " " ++ showVersion pandocVersion ++ versionSuffix
   , flagSettings
   , "Scripting engine: " ++ T.unpack (engineName scriptingEngine)
   , "User data directory: " ++ defaultDatadir
   , copyrightMessage
   ]
  exitSuccess
