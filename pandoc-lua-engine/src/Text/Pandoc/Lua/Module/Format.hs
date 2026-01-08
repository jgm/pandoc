{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Format
   Copyright   : © 2022-2024 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Lua module to handle pandoc templates.
-}
module Text.Pandoc.Lua.Module.Format
  ( documentedModule
  ) where

import Data.Version (makeVersion)
import HsLua
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Extensions (getAllExtensions, getDefaultExtensions)
import Text.Pandoc.Format (formatFromFilePaths, formatName, getExtensionsConfig)
import Text.Pandoc.Lua.Marshal.Format (pushExtensions, pushExtensionsConfig)
import Text.Pandoc.Lua.PandocLua ()

import qualified Data.Text as T

-- | The "pandoc.format" module.
documentedModule :: Module PandocError
documentedModule = defmodule "pandoc.format"
  `withDescription` T.unlines
    [ "Information about the formats supported by pandoc."
    ]
  `withFunctions` functions

-- | Extension module functions.
functions :: [DocumentedFunction PandocError]
functions =
  [ defun "all_extensions"
     ### liftPure getAllExtensions
     <#> parameter peekText "string" "format" "format name"
     =#> functionResult pushExtensions "FormatExtensions"
           "all extensions supported for `format`"
     #? T.unlines
        [ "Returns the list of all valid extensions for a format."
        , "No distinction is made between input and output; an extension"
        , "can have an effect when reading a format but not when"
        , "writing it, or *vice versa*."
        ]
     `since` makeVersion [3,0]

  , defun "default_extensions"
     ### liftPure getDefaultExtensions
     <#> parameter peekText "string" "format" "format name"
     =#> functionResult pushExtensions "FormatExtensions"
           "default extensions enabled for `format`"
     #? T.unlines
        [ "Returns the list of default extensions of the given format; this"
        , "function does not check if the format is supported, it will return"
        , "a fallback list of extensions even for unknown formats."
        ]
     `since` makeVersion [3,0]

  , defun "extensions"
     ### liftPure getExtensionsConfig
     <#> textParam "format" "format identifier"
     =#> functionResult pushExtensionsConfig "table" "extensions config"
     #? T.unlines
        [ "Returns the extension configuration for the given format."
        , "The configuration is represented as a table with all supported"
        , "extensions as keys and their default status as value, with"
        , "`true` indicating that the extension is enabled by default,"
        , "while `false` marks a supported extension that's disabled."
        , ""
        , "This function can be used to assign a value to the `Extensions`"
        , "global in custom readers and writers."
        ]
     `since` makeVersion [3,0]

  , defun "from_path"
      ### liftPure formatFromFilePaths
      <#> parameter (choice [ fmap (:[]) . peekString, peekList peekString])
            "string|{string,...}" "path" "file path, or list of paths"
      =#> functionResult (maybe pushnil (pushText . formatName))
            "string|nil"
            "format determined by heuristic"
      `since` makeVersion [3,1,2]
  ]
