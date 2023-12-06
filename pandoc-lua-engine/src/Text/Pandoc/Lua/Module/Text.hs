{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Text.Pandoc.Lua.Module.Text
Copyright   : © 2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert@hslua.org>

Lua module to work with UTF-8 strings.
-}
module Text.Pandoc.Lua.Module.Text
  ( documentedModule
  ) where

import Data.Version (makeVersion)
import HsLua
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.PandocLua ()

import qualified Data.Text as T
import qualified HsLua.Module.Text as TM

-- | The @aeson@ module specification.
documentedModule :: Module PandocError
documentedModule = TM.documentedModule
  { moduleName = "pandoc.text"
  , moduleFunctions =
    [ TM.fromencoding `since` v[3,0]
    , TM.len          `since` v[2,0,3]
    , TM.lower        `since` v[2,0,3]
    , TM.reverse      `since` v[2,0,3]
    , TM.sub          `since` v[2,0,3]
    , TM.toencoding   `since` v[3,0]
    , TM.upper        `since` v[2,0,3]
    ]
  , moduleDescription = T.unlines
    [ "UTF-8 aware text manipulation functions, implemented in Haskell."
    , ""
    , "The text module can also be loaded under the name `text`, although"
    , "this is discouraged and deprecated."
    , ""
    , "``` lua"
    , "-- uppercase all regular text in a document:"
    , "function Str (s)"
    , "  s.text = pandoc.text.upper(s.text)"
    , "  return s"
    , "end"
    , "```"
    ]
  }
 where
  v = makeVersion
