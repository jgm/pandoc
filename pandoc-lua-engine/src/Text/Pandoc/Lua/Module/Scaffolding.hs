{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Scaffolding
   Copyright   : Copyright © 2022-2026 Albert Krewinkel, John MacFarlane
   License     : GNU GPL, version 2 or above
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Scaffolding for custom Writers.
-}
module Text.Pandoc.Lua.Module.Scaffolding
  ( documentedModule
  ) where

import HsLua
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Writer.Scaffolding (pushWriterScaffolding)
import qualified Data.Text as T

-- | The "pandoc.template" module.
documentedModule :: Module PandocError
documentedModule = defmodule "pandoc.scaffolding"
  `withDescription` "Scaffolding for custom writers."
  `withFields` [writerScaffolding]

-- | Template module functions.
writerScaffolding :: Field PandocError
writerScaffolding = deffield "Writer"
  `withType` "table"
  `withDescription` T.unlines
    [ "An object to be used as a `Writer` function; the construct handles"
    , "most of the boilerplate, expecting only render functions for all"
    , "AST elements"
    ]
  `withValue` do
      pushWriterScaffolding
      -- pretend that it's a submodule so we get better error messages
      getfield registryindex loaded
      pushvalue (nth 2)
      setfield (nth 2) (submod "Writer")
      -- same for fields "Block" and "Inline"
      getfield (nth 2) "Inline" *> setfield (nth 2) (submod "Writer.Inline")
      getfield (nth 2) "Block" *> setfield (nth 2) (submod "Writer.Block")

      pop 1 -- remove "LOADED_TABLE"
 where submod x = moduleName documentedModule <> "." <> x
