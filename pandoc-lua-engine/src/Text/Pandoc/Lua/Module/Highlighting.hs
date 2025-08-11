{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Text.Pandoc.Lua.Module.Highlighting
Copyright   : © 2025 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Lua module for basic image operations.
-}
module Text.Pandoc.Lua.Module.Highlighting (
  -- * Module
    documentedModule

  -- ** Functions
  , style
  )
where

import Prelude hiding (null)
import Control.Applicative ((<|>))
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Version (makeVersion)
import HsLua.Aeson (peekViaJSON, pushViaJSON)
import HsLua.Core
import HsLua.Marshalling
import HsLua.Packaging
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Pandoc.Definition (Block(CodeBlock), Inline(Code))
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Highlighting
  ( Style
  , formatANSI
  , formatConTeXtBlock
  , formatConTeXtInline
  , formatHtmlBlock
  , formatHtmlInline
  , formatLaTeXBlock
  , formatLaTeXInline
  , highlightingStyles
  , lookupHighlightingStyle
  , pygments
  , styleToConTeXt
  , styleToCss
  , styleToLaTeX
  )
import Text.Pandoc.Lua.Marshal.AST (peekBlockFuzzy, peekInlineFuzzy)
import Text.Pandoc.Lua.Marshal.WriterOptions (peekWriterOptions)
import Text.Pandoc.Lua.PandocLua (unPandocLua)
import Text.Pandoc.Options
  ( WriterOptions (writerHighlightStyle, writerSyntaxMap) )

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.Pandoc.Highlighting as HL

-- | The @pandoc.image@ module specification.
documentedModule :: Module PandocError
documentedModule = Module
  { moduleName = "pandoc.highlighting"
  , moduleDescription = "Code highlighting"
  , moduleFields = fields
  , moduleFunctions =
    [ definitions `since` makeVersion [3, 8]
    , highlight   `since` makeVersion [3, 8]
    , style       `since` makeVersion [3, 8]
    ]
  , moduleOperations = []
  , moduleTypeInitializers = []
  }

--
-- Fields
--

-- | Exported fields.
fields :: LuaError e => [Field e]
fields =
  [ Field
    { fieldName = "styles"
    , fieldType = "{string,...}"
    , fieldDescription = "List of known code highlighting styles."
    , fieldPushValue = pushList (pushText . fst) highlightingStyles
    }
  ]

--
-- Functions
--

-- | Gets a highlighting style of the given name.
style :: DocumentedFunction PandocError
style = defun "style"
  ### (unPandocLua . lookupHighlightingStyle)
  <#> stringParam "name" "style name or path to theme file"
  =#> functionResult pushViaJSON "table" "style"
  #? "Returns the style definitions for a given style name.\
     \\
     \ If the name is a standard style, it is loaded amd returned;\
     \ if it ends in `.theme`, attemts to load a KDE theme from the \
     \ file path specified."

definitions :: DocumentedFunction PandocError
definitions = defun "definitions"
  ### (\sty format -> case T.toLower format of
          "context" -> pure $ styleToConTeXt sty
          "css"     -> pure $ T.pack $ styleToCss sty
          "latex"   -> pure $ styleToLaTeX sty
          _ -> failLua $ "Unsupported format: " <> T.unpack format)
  <#> parameter peekStyle "table|string" "style" "style table or style name"
  <#> textParam "format" "`'context'`, `'css'`, or `'latex'`"
  =#> functionResult pushText "string" "style definitions"
  #? "Generate highlighting definitions for the given format.\
     \ For example, to generate CSS definitions for the *espresso* style,\
     \ run `pandoc.highlighting.toformat('espresso', 'css')`."

highlight :: DocumentedFunction PandocError
highlight = defun "highlight"
  ### (\codeElement format mwopts -> do
          (attr, code, inline) <-
            case codeElement of
              Left (Code a c)       -> pure (a, c, True)
              Right (CodeBlock a c) -> pure (a, c, False)
              _                     -> failLua "Cannot highlight element"
          let wopts = fromMaybe def mwopts
          let sty = fromMaybe pygments (writerHighlightStyle wopts)
          (inlineFormatter, blockFormatter) <- case T.toLower format of
            "ansi"    -> pure ( \opts lns -> formatANSI opts sty lns
                              , \opts lns -> formatANSI opts sty lns )
            "context" -> pure (formatConTeXtInline, formatConTeXtBlock)
            "html"    -> let htmlToText fn = \opts src ->
                               TL.toStrict $ renderHtml (fn opts src)
                         in pure ( htmlToText formatHtmlInline
                                 , htmlToText formatHtmlBlock )
            "latex"   -> pure (formatLaTeXInline, formatLaTeXBlock)
            _ -> failLua $
                 "Unsupported highlighting format: " <> T.unpack format
          let syntaxMap = writerSyntaxMap wopts
          let formatter = if inline then inlineFormatter else blockFormatter
          case HL.highlight syntaxMap formatter attr code of
            Left err -> failLua $ T.unpack err
            Right result -> pure result)
  <#> parameter
        (\idx ->
           (Left <$> peekInlineFuzzy idx) <|>
           (Right <$> peekBlockFuzzy idx))
        "Inline|Block" "code element" "element that will be highlighted"
  <#> textParam "format"
        "target format (`'ansi'`, `'context'`, `'html'`, or `'latex'`')"
  <#> opt (parameter peekWriterOptions "WriterOptions" "wopts" "")
  =#> functionResult pushText "string" "highlighted code"
  #? "Highlight code in the given format."

-- | Retrieves a highlighting style; accepts a string, themepath, or style
-- table.
peekStyle :: Peeker PandocError Style
peekStyle idx = do
  liftLua (ltype idx) >>= \case
    TypeTable -> peekViaJSON idx
    TypeString -> do
      name <- peekString idx
      liftLua $ unPandocLua $ lookupHighlightingStyle name
    _type -> failPeek "Can't retrieve style."
