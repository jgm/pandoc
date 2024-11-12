{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.CLI
   Copyright   : © 2022-2024 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Command line helpers
-}
module Text.Pandoc.Lua.Module.CLI
  ( documentedModule
  ) where

import Control.Applicative ((<|>))
import Data.Version (makeVersion)
import HsLua
import HsLua.REPL (defaultConfig, replWithEnv, setup)
import Text.Pandoc.App (defaultOpts, options, parseOptionsFromArgs)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.PandocLua ()
import qualified Data.Text as T

-- | Push the pandoc.types module on the Lua stack.
documentedModule :: Module PandocError
documentedModule = defmodule "pandoc.cli"
  `withDescription`
      "Command line options and argument parsing."
  `withFields`
      [ deffield "default_options"
        `withType` "table"
        `withDescription`
          "Default CLI options, using a JSON-like representation."
        `withValue` pushViaJSON defaultOpts
      ]
  `withFunctions`
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
        `since` makeVersion [3, 0]

      , repl `since` makeVersion [3, 1, 2]
      ]
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

-- | Starts a REPL.
repl :: DocumentedFunction PandocError
repl = defun "repl"
  ### (\menvIdx -> do
          let repl' = case menvIdx of
                        Nothing -> replWithEnv Nothing
                        Just envIdx -> do
                          settop envIdx
                          fillWithGlobals envIdx
                          replWithEnv . Just =<< ref registryindex
          setup defaultConfig
          repl')
  <#> opt (parameter (typeChecked "table" istable pure) "table" "env"
           ("Extra environment; the global environment is merged into this" <>
           " table."))
  =?> T.unlines
      [ "The result(s) of the last evaluated input, or nothing if the last"
      , "input resulted in an error."
      ]
  #? T.unlines
  [ "Starts a read-eval-print loop (REPL). The function returns all"
  , "values of the last evaluated input. Exit the REPL by pressing"
  , "`ctrl-d` or `ctrl-c`; press `F1` to get a list of all key"
  , "bindings."
  , ""
  , "The REPL is started in the global namespace, unless the `env`"
  , "parameter is specified. In that case, the global namespace is"
  , "merged into the given table and the result is used as `_ENV` value"
  , "for the repl."
  , ""
  , "Specifically, local variables *cannot* be accessed, unless they"
  , "are explicitly passed via the `env` parameter; e.g."
  , ""
  , "    function Pandoc (doc)"
  , "      -- start repl, allow to access the `doc` parameter"
  , "      -- in the repl"
  , "      return pandoc.cli.repl{ doc = doc }"
  , "    end"
  , ""
  , "**Note**: it seems that the function exits immediately on Windows,"
  , "without prompting for user input."
  ]
 where
  fillWithGlobals idx = do
    -- Copy all global values into the table
    pushglobaltable
    pushnil
    let copyval = next (nth 2) >>= \case
          False -> return ()
          True -> do
            pushvalue (nth 2)
            insert (nth 2)
            rawset idx
            copyval
    copyval
    pop 1  -- global table
