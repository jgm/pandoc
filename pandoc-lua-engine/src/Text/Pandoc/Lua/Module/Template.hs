{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Template
   Copyright   : Copyright © 2022 Albert Krewinkel, John MacFarlane
   License     : GNU GPL, version 2 or above
   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Lua module to handle pandoc templates.
-}
module Text.Pandoc.Lua.Module.Template
  ( documentedModule
  ) where

import HsLua
import HsLua.Module.DocLayout (peekDoc, pushDoc)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Marshal.AST (peekMeta, pushBlocks, pushInlines)
import Text.Pandoc.Lua.Marshal.Context (peekContext, pushContext)
import Text.Pandoc.Lua.Marshal.Template (peekTemplate, pushTemplate)
import Text.Pandoc.Lua.PandocLua (PandocLua (unPandocLua), liftPandocLua)
import Text.Pandoc.Writers.Shared (metaToContext')
import Text.Pandoc.Templates
  ( compileTemplate, getDefaultTemplate, renderTemplate
  , runWithPartials, runWithDefaultPartials )

import qualified Data.Text as T

-- | The "pandoc.template" module.
documentedModule :: Module PandocError
documentedModule = Module
  { moduleName = "pandoc.template"
  , moduleDescription = T.unlines
    [ "Lua functions for pandoc templates."
    ]
  , moduleFields = []
  , moduleOperations = []
  , moduleFunctions = functions
  }

-- | Template module functions.
functions :: [DocumentedFunction PandocError]
functions =
  [ defun "apply"
     ### liftPure2 renderTemplate
     <#> parameter peekTemplate "Template" "template" "template to apply"
     <#> parameter peekContext "table" "context" "variable values"
     =#> functionResult pushDoc "Doc" "rendered template"
     #? T.unlines
     [ "Applies a context with variable assignments to a template,"
     , "returning the rendered template. The `context` parameter must be a"
     , "table with variable names as keys and [Doc], string, boolean, or"
     , "table as values, where the table can be either be a list of the"
     , "aforementioned types, or a nested context."
     ]

  , defun "compile"
     ### (\template mfilepath -> unPandocLua $
           case mfilepath of
             Just fp -> runWithPartials (compileTemplate fp template)
             Nothing -> runWithDefaultPartials
                        (compileTemplate "templates/default" template))
     <#> parameter peekText "string" "template" "template string"
     <#> opt (stringParam "templ_path" "template path")
     =#> functionResult (either failLua pushTemplate) "pandoc Template"
           "compiled template"

  , defun "default"
     ### (\mformat -> unPandocLua $ do
           let getFORMAT = liftPandocLua $ do
                 getglobal "FORMAT"
                 forcePeek $ peekText top `lastly` pop 1
           format <- maybe getFORMAT pure mformat
           getDefaultTemplate format)
     <#> opt (textParam "writer"
              "writer for which the template should be returned.")
     =#> functionResult pushText "string"
           "string representation of the writer's default template"

  , defun "meta_to_context"
     ### (\meta blockWriterIdx inlineWriterIdx -> unPandocLua $ do
             let blockWriter blks = liftPandocLua $ do
                   pushvalue blockWriterIdx
                   pushBlocks blks
                   callTrace 1 1
                   forcePeek $ peekDoc top
             let inlineWriter blks = liftPandocLua $ do
                   pushvalue inlineWriterIdx
                   pushInlines blks
                   callTrace 1 1
                   forcePeek $ peekDoc top
             metaToContext' blockWriter inlineWriter meta)
     <#> parameter peekMeta "Meta" "meta" "document metadata"
     <#> parameter pure "function" "blocks_writer"
           "converter from Blocks to Doc values"
     <#> parameter pure "function" "inlines_writer"
           "converter from Inlines to Doc values"
     =#> functionResult pushContext "table" "template context"
     #? T.unlines
     [ "Creates template context from the document's [Meta]{#type-meta}"
     , "data, using the given functions to convert [Blocks] and [Inlines]"
     , "to [Doc] values."
     ]
  ]
