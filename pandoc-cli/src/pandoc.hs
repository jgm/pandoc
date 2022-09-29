{-# LANGUAGE OverloadedStrings #-}
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
                       , parseOptionsFromArgs)
import Text.Pandoc.Error (handleError)
import qualified Text.Pandoc.UTF8 as UTF8
import PandocCLI.Lua
import PandocCLI.Server

main :: IO ()
main = E.handle (handleError . Left) $ do
  prg <- getProgName
  rawArgs <- map UTF8.decodeArg <$> getArgs
  case prg of
    "pandoc-server.cgi" -> runCGI
    "pandoc-server"     -> runServer rawArgs
    "pandoc-lua"        -> runLuaInterpreter prg rawArgs
    _ ->
      case rawArgs of
        "lua" : args   -> runLuaInterpreter "pandoc lua" args
        "serve" : args -> runServer args
        _              -> do
          engine <- getEngine
          let cliOpts = options engine
          opts <- parseOptionsFromArgs cliOpts defaultOpts prg rawArgs
          convertWithOpts engine opts
