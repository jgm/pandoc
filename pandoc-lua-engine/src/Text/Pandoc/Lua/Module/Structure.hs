{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Structure
   Copyright   : © 2023-2026 Albert Krewinkel <albert+pandoc@tarleb.com>
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Command line helpers
-}
module Text.Pandoc.Lua.Module.Structure
  ( documentedModule
  ) where

import Control.Applicative ((<|>), optional)
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Data.Version (makeVersion)
import HsLua ( DocumentedFunction, LuaError, Module (..), Peeker
             , (###), (<#>), (=#>), (#?), defmodule
             , defun, functionResult, getfield, isnil, lastly, liftLua
             , opt, liftPure, parameter , peekBool, peekIntegral
             , peekFieldRaw, peekSet, peekText, pop, pushIntegral
             , pushText, since, top, withDescription, withFunctions )
import Text.Pandoc.Chunks ( ChunkedDoc (..), PathTemplate (..)
                          , tocToList, splitIntoChunks )
import Text.Pandoc.Definition (Pandoc (..), Block)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.PandocLua ()
import Text.Pandoc.Lua.Marshal.AST ( peekBlocksFuzzy, peekInlinesFuzzy
                                   , peekPandoc, pushBlock, pushBlocks )
import Text.Pandoc.Lua.Marshal.Chunks
import Text.Pandoc.Lua.Marshal.Format (peekExtensions)
import Text.Pandoc.Lua.Marshal.WriterOptions ( peekWriterOptions )
import Text.Pandoc.Options (WriterOptions (writerTOCDepth,
                                           writerNumberSections))
import Text.Pandoc.Slides (getSlideLevel, prepSlides)
import Text.Pandoc.Writers.Shared (toTableOfContents)
import qualified Data.Text as T
import qualified Text.Pandoc.Shared as Shared

-- | Push the pandoc.structure module on the Lua stack.
documentedModule :: Module PandocError
documentedModule = defmodule "pandoc.structure"
  `withDescription`
    "Access to the higher-level document structure, including " <>
    "hierarchical sections and the table of contents."
  `withFunctions`
      [ make_sections     `since` makeVersion [3,0]
      , slide_level       `since` makeVersion [3,0]
      , split_into_chunks `since` makeVersion [3,0]
      , table_of_contents `since` makeVersion [3,0]
      , unique_identifier `since` makeVersion [3,8]
      ]

make_sections :: LuaError e => DocumentedFunction e
make_sections = defun "make_sections"
  ### (\blks mopts ->
         let (numSects, baseLevel, mslideLevel) =
               fromMaybe (defNumSec, Nothing, Nothing) mopts
             blks' = case mslideLevel of
                       Just l | l <= 0 -> prepSlides (getSlideLevel blks) blks
                       Just sl -> prepSlides sl blks
                       Nothing -> blks
         in pure $ Shared.makeSections numSects baseLevel blks')
  <#> parameter peekBodyBlocks "Blocks|Pandoc" "blocks"
        "document blocks to process"
  <#> opt (parameter peekOpts "table" "opts" "options")
  =#> functionResult pushBlocks "Blocks"
        "processed blocks"
  #? T.unlines
     [ "Puts [[Blocks]] into a hierarchical structure: a list of sections"
     , "(each a Div with class \"section\" and first element a Header)."
     , ""
     , "The optional `opts` argument can be a table; two settings are"
     , "recognized: If `number_sections` is true, a `number` attribute"
     , "containing the section number will be added to each `Header`. If"
     , "`base_level` is an integer, then `Header` levels will be"
     , "reorganized so that there are no gaps, with numbering levels"
     , "shifted by the given value. Finally, an integer `slide_level`"
     , "value triggers the creation of slides at that heading level."
     , ""
     , "Note that a [[WriterOptions]] object can be passed as the opts"
     , "table; this will set the `number_section` and `slide_level` values"
     , "to those defined on the command line."
     , ""
     , "Usage:"
     , ""
     , "    local blocks = {"
     , "      pandoc.Header(2, pandoc.Str 'first'),"
     , "      pandoc.Header(2, pandoc.Str 'second'),"
     , "    }"
     , "    local opts = PANDOC_WRITER_OPTIONS"
     , "    local newblocks = pandoc.structure.make_sections(blocks, opts)"
     ]
  where
    defNumSec = False
    peekOpts idx = do
      numberSections <- fromMaybe defNumSec <$> do
        liftLua $ getfield idx "number_sections"
        optional (peekBool top `lastly` pop 1)
      baseLevel <- do
        liftLua $ getfield idx "base_level"
        optional (peekIntegral top `lastly` pop 1)
      slideLevel <- do
        liftLua $ getfield idx "slide_level"
        optional (peekIntegral top `lastly` pop 1)
      return (numberSections, baseLevel, slideLevel)

slide_level :: LuaError e => DocumentedFunction e
slide_level = defun "slide_level"
  ### liftPure getSlideLevel
  <#> parameter peekBodyBlocks "Blocks|Pandoc" "blocks" "document body"
  =#> functionResult pushIntegral "integer" "slide level"
  #? T.unlines
  [ "Find level of header that starts slides (defined as the least"
  , "header level that occurs before a non-header/non-hrule in the"
  , "blocks)."
  ]

-- | Split 'Pandoc' into 'Chunk's.
split_into_chunks :: LuaError e => DocumentedFunction e
split_into_chunks = defun "split_into_chunks"
  ### (\doc mopts -> pure $
          let defOpts = (defPathTmpl, defNumSects, Nothing, defLvl)
              (pathTempl, numberSect, mbBaseLevel, chunkLevel) =
                fromMaybe defOpts mopts
          in splitIntoChunks pathTempl numberSect mbBaseLevel chunkLevel doc)
  <#> parameter peekPandoc "Pandoc" "doc" "document to split"
  <#> opt (parameter peekSplitOpts "table" "opts" optionsDescr)
  =#> functionResult pushChunkedDoc "ChunkedDoc" ""
  #? T.unlines
     [ "Converts a [[Pandoc]] document into a [[ChunkedDoc]]." ]
 where
  defPathTmpl = PathTemplate "chunk-%n"
  defNumSects = False
  defLvl = 1
  peekSplitOpts idx = (,,,)
    <$> peekFieldRaw ((fmap PathTemplate . peekText) `orDefault` defPathTmpl)
                     "path_template" idx
    <*> peekFieldRaw (peekBool `orDefault` defNumSects) "number_sections" idx
    <*> peekFieldRaw (optional . peekIntegral) "base_heading_level" idx
    <*> peekFieldRaw (peekIntegral `orDefault` defLvl) "chunk_level" idx
  orDefault p defaultValue idx' = liftLua (isnil idx') >>= \case
    True  -> pure defaultValue
    False -> p idx'
  optionsDescr = T.unlines
    [ "Splitting options."
    , ""
    , "The following options are supported:"
    , ""
    , "    `path_template`"
    , "    :   template used to generate the chunks' filepaths"
    , "        `%n` will be replaced with the chunk number (padded with"
    , "        leading 0s to 3 digits), `%s` with the section number of"
    , "        the heading, `%h` with the (stringified) heading text,"
    , "        `%i` with the section identifier. For example,"
    , "        `\"section-%s-%i.html\"` might be resolved to"
    , "        `\"section-1.2-introduction.html\"`."
    , ""
    , "        Default is `\"chunk-%n\"` (string)"
    , ""
    , "    `number_sections`"
    , "    :   whether sections should be numbered; default is `false`"
    , "        (boolean)"
    , ""
    , "    `chunk_level`"
    , "    :   The heading level the document should be split into"
    , "        chunks. The default is to split at the top-level, i.e.,"
    , "        `1`. (integer)"
    , ""
    , "    `base_heading_level`"
    , "    :   The base level to be used for numbering. Default is `nil`"
    , "        (integer|nil)"
    ]

-- | Generate a table of contents.
table_of_contents :: DocumentedFunction PandocError
table_of_contents = defun "table_of_contents"
  ### (\tocSource mwriterOpts -> pure $
          let writerOpts = fromMaybe def mwriterOpts
          in case tocSource of
               Left blks  -> toTableOfContents writerOpts blks
               Right tree -> tocToList (writerNumberSections writerOpts)
                                       (writerTOCDepth writerOpts) tree
      )
  <#> parameter peekTocSource "Blocks|Pandoc|ChunkedDoc" "toc_source"
        "list of command line arguments"
  <#> opt (parameter peekWriterOptions "WriterOptions" "opts" "options")
  =#> functionResult pushBlock "Block"
        "Table of contents as a BulletList object"
  #? T.unlines
     [ "Generates a table of contents for the given object." ]
 where
  peekTocSource idx =
    (Left <$> peekBodyBlocks idx) <|>
    (Right . chunkedTOC <$> peekChunkedDoc idx)

-- | Generate a unique ID from a list of inlines.
unique_identifier :: LuaError e => DocumentedFunction e
unique_identifier = defun "unique_identifier"
  ### (\inlns mUsedIdents mExts -> do
          let usedIdents = fromMaybe mempty mUsedIdents
          let exts       = fromMaybe mempty mExts
          pure $ Shared.uniqueIdent exts inlns usedIdents)
  <#> parameter peekInlinesFuzzy "Inlines" "inlines" "base for identifier"
  <#> opt (parameter (peekSet peekText) "table" "used"
           "set of identifiers (string keys, boolean values) that\
           \ have already been used.")
  <#> opt (parameter peekExtensions "{string,...}" "exts"
           "list of format extensions")
  =#> functionResult pushText "string" "unique identifier"
  #? "Generates a unique identifier from a list of inlines, similar to\
     \ what's generated by the `auto_identifiers` extension.\n\
     \\n\
     \ The method used to generated identifiers can be modified through\
     \ `ext`, which is a list of format extensions.\n\
     \\n\
     \ It can be used to generate IDs similar to what the `auto_identifiers`\
     \ extension provides.\n\
     \\n\
     \ Example:\n\
     \\n\
     \     local used_ids = {}\n\
     \     function Header (h)\n\
     \       local id =\n\
     \         pandoc.structure.unique_identifier(h.content, used_ids)\n\
     \       used_ids[id] = true\n\
     \       h.identifier = id\n\
     \       return h\n\
     \     end"

-- | Retrieves the body blocks of a 'Pandoc' object or from a list of
-- blocks.
peekBodyBlocks :: LuaError e => Peeker e [Block]
peekBodyBlocks idx =
  ((\(Pandoc _ blks) -> blks) <$> peekPandoc idx) <|>
  peekBlocksFuzzy idx
