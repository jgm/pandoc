{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.CLI
   Copyright   : © 2022 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Command line helpers
-}
module Text.Pandoc.Lua.Module.CLI
  ( documentedModule
  ) where

import Control.Applicative ((<|>))
import HsLua ( Field (..), Module (..), (###), (<#>), (=#>), (#?)
             , defun, failLua, functionResult, liftIO, parameter, pop
             , pushViaJSON, rawgeti, top)
import HsLua.Marshalling (lastly, liftLua, peekList, peekString)
import Text.Pandoc.App (defaultOpts, options, parseOptionsFromArgs)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.PandocLua ()
import qualified Data.Text as T

-- | Push the pandoc.types module on the Lua stack.
documentedModule :: Module PandocError
documentedModule = Module
  { moduleName = "pandoc.cli"
  , moduleDescription =
      "Command line options and argument parsing."
  , moduleFields =
      [ Field
        { fieldName = "default_options"
        , fieldDescription = "Default CLI options, using a JSON-like " <>
          "representation."
        , fieldPushValue = pushViaJSON defaultOpts
        }
      ]
  , moduleFunctions =
      [ defun "parse_options"
        ### parseOptions
        <#> parameter peekArgs "{string,...}" "args"
              "list of command line arguments"
        =#> functionResult pushViaJSON "table"
              "parsed options, using their JSON-like representation."
        #? T.unlines
           [ "Parses command line arguments into pandoc options."
           , "Typically this function will be used in stand-alone pandoc Lua"
           , "scripts, taking the list of arguments from the global `arg`."
           ]
      ]
  , moduleOperations = []
  }
 where
  peekArgs idx =
    (,)
    <$> (liftLua (rawgeti idx 0) *> (peekString top <|> pure "") `lastly` pop 1)
    <*> peekList peekString idx

  parseOptions (prg, args) =
    liftIO (parseOptionsFromArgs options defaultOpts prg args) >>=
    \case
      Left e     -> failLua $ "Cannot process info option: " ++ show e
      Right opts -> pure opts
