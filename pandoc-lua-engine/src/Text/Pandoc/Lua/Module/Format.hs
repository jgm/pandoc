{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Format
   Copyright   : © 2022 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Lua module to handle pandoc templates.
-}
module Text.Pandoc.Lua.Module.Format
  ( documentedModule
  ) where

import HsLua
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Extensions (getAllExtensions, getDefaultExtensions)
import Text.Pandoc.Lua.Marshal.Format (pushExtensions)
import Text.Pandoc.Lua.PandocLua ()

import qualified Data.Text as T

-- | The "pandoc.format" module.
documentedModule :: Module PandocError
documentedModule = Module
  { moduleName = "pandoc.format"
  , moduleDescription = T.unlines
    [ "Pandoc formats and their extensions."
    ]
  , moduleFields = []
  , moduleOperations = []
  , moduleFunctions = functions
  }

-- | Extension module functions.
functions :: [DocumentedFunction PandocError]
functions =
  [ defun "default_extensions"
     ### liftPure getDefaultExtensions
     <#> parameter peekText "string" "format" "format name"
     =#> functionResult pushExtensions "FormatExtensions"
           "default extensions enabled for `format`"
     #? T.unlines
        [ "Returns the list of default extensions of the given format; this"
        , "function does not check if the format is supported, it will return"
        , "a fallback list of extensions even for unknown formats."
        ]

  , defun "all_extensions"
     ### liftPure getAllExtensions
     <#> parameter peekText "string" "format" "format name"
     =#> functionResult pushExtensions "FormatExtensions"
           "all extensions supported for `format`"
     #? T.unlines
        [ "Returns the list of all valid extensions for a format."
        , "No distinction is made between input and output, and an"
        , "extension have an effect when reading a format but not when"
        , "writing it, or *vice versa*."
        ]
  ]
