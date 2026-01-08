{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Template
   Copyright   : Copyright © 2022-2026 Albert Krewinkel, John MacFarlane
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Lua module to handle pandoc templates.
-}
module Text.Pandoc.Lua.Module.Template
  ( documentedModule
  ) where

import Data.Version (makeVersion)
import HsLua
import HsLua.Module.DocLayout (peekDoc, pushDoc)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Marshal.AST (peekMeta, pushBlocks, pushInlines)
import Text.Pandoc.Lua.Marshal.Context (peekContext, pushContext)
import Text.Pandoc.Lua.Marshal.Template (typeTemplate, peekTemplate, pushTemplate)
import Text.Pandoc.Lua.PandocLua (PandocLua (unPandocLua), liftPandocLua)
import Text.Pandoc.Writers.Shared (metaToContext')
import Text.Pandoc.Templates
  ( compileTemplate, getDefaultTemplate, getTemplate, renderTemplate
  , runWithPartials, runWithDefaultPartials )

import qualified Data.Text as T

-- | The "pandoc.template" module.
documentedModule :: Module PandocError
documentedModule = defmodule "pandoc.template"
  `withDescription` "Handle pandoc templates."
  `withFunctions` functions
  `associateType` typeTemplate

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
     , "table with variable names as keys and [[Doc]], string, boolean, or"
     , "table as values, where the table can be either be a list of the"
     , "aforementioned types, or a nested context."
     ]
    `since` makeVersion [3,0]

  , defun "compile"
     ### (\template mfilepath -> unPandocLua $
           case mfilepath of
             Just fp -> runWithPartials (compileTemplate fp template)
             Nothing -> runWithDefaultPartials
                        (compileTemplate "templates/default" template))
     <#> parameter peekText "string" "template" "template string"
     <#> opt (stringParam "templates_path"
              ("parameter to determine a default path and extension for " <>
               "partials; uses the data files templates path by default."))
     =#> functionResult (either failLua pushTemplate) "Template"
           "compiled template"
     #? T.unlines
     [ "Compiles a template string into a [[Template]] object usable by"
     , "pandoc."
     , ""
     , "If the `templates_path` parameter is specified, then it should be the"
     , "file path associated with the template. It is used when checking"
     , "for partials. Partials will be taken only from the default data"
     , "files if this parameter is omitted."
     , ""
     , "An error is raised if compilation fails."
     ]
    `since` makeVersion [2,17]

  , defun "default"
     ### (\mformat -> unPandocLua $ do
           let getFORMAT = liftPandocLua $ do
                 getglobal "FORMAT"
                 forcePeek $ peekText top `lastly` pop 1
           format <- maybe getFORMAT pure mformat
           getDefaultTemplate format)
     <#> opt (textParam "writer"
              ("name of the writer for which the template should be " <>
               "retrieved; defaults to the global `FORMAT`."))
     =#> functionResult pushText "string" "raw template"
    #? T.unlines
    [ "Returns the default template for a given writer as a string. An"
    , "error is thrown if no such template can be found."
    ]
    `since` makeVersion [2,17]

  , defun "get"
     ### (unPandocLua . getTemplate)
     <#> stringParam "filename" "name of the template"
     =#> textResult "content of template file"
     #? T.unlines
     [ "Retrieve text for a template."
     , ""
     , "This function first checks the resource paths for a file of this"
     , "name; if none is found, the `templates` directory in the user data"
     , "directory is checked.  Returns the content of the file, or throws"
     , "an error if no file is found."
     ]
    `since` makeVersion [3,2,1]

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
           "converter from [[Blocks]] to [[Doc]] values"
     <#> parameter pure "function" "inlines_writer"
           "converter from [[Inlines]] to [[Doc]] values"
     =#> functionResult pushContext "table" "template context"
     #? T.unlines
     [ "Creates template context from the document's [[Meta]] data, using the"
     , "given functions to convert [[Blocks]] and [[Inlines]] to [[Doc]]"
     , "values."
     ]
    `since` makeVersion [3,0]
  ]
