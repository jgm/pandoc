{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Utils
   Copyright   : Copyright © 2017-2026 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>
   Stability   : alpha

Utility module for Lua, exposing internal helper functions.
-}
module Text.Pandoc.Lua.Module.Utils
  ( documentedModule
  , sha1
  ) where

import Control.Applicative ((<|>))
import Control.Monad ((<$!>))
import Crypto.Hash (hashWith, SHA1(SHA1))
import Data.Data (showConstr, toConstr)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Version (Version, makeVersion)
import HsLua as Lua
import HsLua.Module.Version (peekVersionFuzzy, pushVersion)
import Text.Pandoc.Citeproc (getReferences, processCitations)
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Filter (applyJSONFilter)
import Text.Pandoc.Lua.Filter (runFilterFile')
import Text.Pandoc.Lua.Marshal.AST
import Text.Pandoc.Lua.Marshal.Reference
import Text.Pandoc.Lua.PandocLua (PandocLua (unPandocLua))

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.Shared as Shared
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Text.Pandoc.Writers.Shared as Shared

-- | Push the "pandoc.utils" module to the Lua stack.
documentedModule :: Module PandocError
documentedModule = defmodule "pandoc.utils"
  `withDescription` T.unlines
    [ "This module exposes internal pandoc functions and utility"
    , "functions."
    ]
  `withFunctions`
    [ blocks_to_inlines `since` v[2,2,3]
    , citeproc          `since` v[2,19,1]
    , equals            `since` v[2,5]
    , from_simple_table `since` v[2,11]
    , make_sections     `since` v[2,8]
    , normalize_date    `since` v[2,0,6]
    , references        `since` v[2,17]
    , run_json_filter   `since` v[2,1,1]
    , run_lua_filter    `since` v[3,2,1]
    , sha1              `since` v[2,0,6]
    , stringify         `since` v[2,0,6]
    , to_roman_numeral  `since` v[2,0,6]
    , to_simple_table   `since` v[2,11]
    , type'             `since` v[2,17]

    , defun "Version"
      ### liftPure (id @Version)
      <#> parameter peekVersionFuzzy
            "version string, list of integers, or integer"
            "v" "version description"
      =#> functionResult pushVersion "Version" "new Version object"
      #? "Creates a Version object."
    ]
 where
  v = makeVersion

blocks_to_inlines :: LuaError e => DocumentedFunction e
blocks_to_inlines = defun "blocks_to_inlines"
  ### (\blks mSep -> do
          let sep = maybe Shared.defaultBlocksSeparator B.fromList mSep
          return $ B.toList (Shared.blocksToInlinesWithSep sep blks))
  <#> parameter (peekList peekBlock) "Blocks"
        "blocks"
        "List of [[Block]] elements to be flattened."
  <#> opt (parameter (peekList peekInline) "Inlines" "sep"
           ("List of [[Inline]] elements inserted as separator between\n" <>
            "two consecutive blocks; defaults to `{pandoc.LineBreak()}`."))
  =#> functionResult pushInlines "Inlines" ""
  #? T.unlines
  [ "Squash a list of blocks into a list of inlines."
  , ""
  , "Usage"
  , ""
  , "    local blocks = {"
  , "      pandoc.Para{ pandoc.Str 'Paragraph1' },"
  , "      pandoc.Para{ pandoc.Emph 'Paragraph2' }"
  , "    }"
  , "    local inlines = pandoc.utils.blocks_to_inlines(blocks)"
  , "    assert("
  , "      inlines == pandoc.Inlines {"
  , "        pandoc.Str 'Paragraph1',"
  , "        pandoc.Linebreak(),"
  , "        pandoc.Emph{ pandoc.Str 'Paragraph2' }"
  , "      }"
  , "    )"
  ]

citeproc :: DocumentedFunction PandocError
citeproc = defun "citeproc"
  ### unPandocLua . processCitations
  <#> parameter peekPandoc "Pandoc" "doc" "document"
  =#> functionResult pushPandoc "Pandoc" "processed document"
  #?  T.unlines
      [ "Process the citations in the file, replacing them with "
      , "rendered citations and adding a bibliography. "
      , "See the manual section on citation rendering for details."
      , ""
      , "Usage:"
      , ""
      , "    -- Lua filter that behaves like `--citeproc`"
      , "    function Pandoc (doc)"
      , "      return pandoc.utils.citeproc(doc)"
      , "    end"
      ]

equals :: LuaError e => DocumentedFunction e
equals = defun "equals"
  ### equal
  <#> parameter pure "any" "element1" ""
  <#> parameter pure "any" "element2" ""
  =#> functionResult pushBool "boolean"
        "Whether the two objects represent the same element"
  #? T.unlines
  [ "Test equality of AST elements. Elements in Lua are considered"
  , "equal if and only if the objects obtained by unmarshaling are"
  , "equal."
  , ""
  , "**This function is deprecated.** Use the normal Lua `==` equality"
  , "operator instead."
  ]

-- | Converts an old/simple table into a normal table block element.
from_simple_table :: LuaError e => DocumentedFunction e
from_simple_table = defun "from_simple_table"
  ### liftPure
      (\(SimpleTable capt aligns widths head' body) ->
          Table
          nullAttr
          (Caption Nothing [Plain capt | not (null capt)])
          (zipWith (\a w -> (a, toColWidth w)) aligns widths)
          (TableHead nullAttr [blockListToRow head' | not (null head') ])
          [TableBody nullAttr 0 [] $ map blockListToRow body | not (null body)]
          (TableFoot nullAttr []))
  <#> parameter peekSimpleTable "SimpleTable" "simple_tbl" ""
  =#> functionResult pushBlock "Block" "table block element"
  #? T.unlines
  [ "Creates a [[Table]] block element from a [[SimpleTable]]. This is"
  , "useful for dealing with legacy code which was written for pandoc"
  , "versions older than 2.10."
  , ""
  , "Usage:"
  , ""
  , "    local simple = pandoc.SimpleTable(table)"
  , "    -- modify, using pre pandoc 2.10 methods"
  , "    simple.caption = pandoc.SmallCaps(simple.caption)"
  , "    -- create normal table block again"
  , "    table = pandoc.utils.from_simple_table(simple)"
  ]
 where
  blockListToRow :: [[Block]] -> Row
  blockListToRow = Row nullAttr . map (B.simpleCell . B.fromList)

  toColWidth :: Double -> ColWidth
  toColWidth 0 = ColWidthDefault
  toColWidth w = ColWidth w

make_sections :: LuaError e => DocumentedFunction e
make_sections = defun "make_sections"
  ### liftPure3 Shared.makeSections
  <#> parameter peekBool "boolean" "number_sections"
      ("whether section divs should get an additional `number`\n" <>
       "attribute containing the section number.")
  <#> parameter (\i -> (Nothing <$ peekNil i) <|> (Just <$!> peekIntegral i))
        "integer|nil" "baselevel"
        "shift top-level headings to this level"
  <#> parameter (peekList peekBlock) "Blocks"
        "blocks" "list of blocks to process"
  =#> functionResult pushBlocks "Blocks"
        "blocks with sections"
  #? T.unlines
  [ "Converts a list of [[Block]] elements into sections."
  , "`Div`s will be created beginning at each `Header`"
  , "and containing following content until the next `Header`"
  , "of comparable level.  If `number_sections` is true,"
  , "a `number` attribute will be added to each `Header`"
  , "containing the section number. If `base_level` is"
  , "non-null, `Header` levels will be reorganized so"
  , "that there are no gaps, and so that the base level"
  , "is the level specified."
  ]

normalize_date :: DocumentedFunction e
normalize_date = defun "normalize_date"
  ### liftPure Shared.normalizeDate
  <#> parameter peekText "string" "date" "the date string"
  =#> functionResult (maybe pushnil pushText) "string or nil"
        "normalized date, or nil if normalization failed."
  #? T.unwords
  [ "Parse a date and convert (if possible) to \"YYYY-MM-DD\" format. We"
  , "limit years to the range 1601-9999 (ISO 8601 accepts greater than"
  , "or equal to 1583, but MS Word only accepts dates starting 1601)."
  , "Returns nil instead of a string if the conversion failed."
  ]

-- | List of references in CSL format.
references :: DocumentedFunction PandocError
references = defun "references"
  ### (unPandocLua . getReferences Nothing)
  <#> parameter peekPandoc "Pandoc" "doc" "document"
  =#> functionResult (pushPandocList pushReference) "table"
       "lift of references."
  #? T.unlines
  [ "Get references defined inline in the metadata and via an external"
  , "bibliography. Only references that are actually cited in the"
  , "document (either with a genuine citation or with `nocite`) are"
  , "returned. URL variables are converted to links."
  , ""
  , "The structure used represent reference values corresponds to that"
  , "used in CSL JSON; the return value can be use as `references`"
  , "metadata, which is one of the values used by pandoc and citeproc"
  , "when generating bibliographies."
  , ""
  , "Usage:"
  , ""
  , "    -- Include all cited references in document"
  , "    function Pandoc (doc)"
  , "      doc.meta.references = pandoc.utils.references(doc)"
  , "      doc.meta.bibliography = nil"
  , "      return doc"
  , "    end"
  ]

-- | Run a filter from a file.
run_lua_filter :: DocumentedFunction PandocError
run_lua_filter = defun "run_lua_filter"
  ### (\doc fp mbenv -> do
         envIdx <- maybe copyOfGlobalTable pure mbenv
         runFilterFile' envIdx fp doc)
  <#> parameter peekPandoc "Pandoc" "doc" "the Pandoc document to filter"
  <#> parameter peekString "string" "filter" "filepath of the filter to run"
  <#> opt (parameter (typeChecked "table" istable pure) "table" "env"
            "environment to load and run the filter in")
  =#> functionResult pushPandoc "Pandoc" "filtered document"
  #? ( "Filter the given doc by passing it through a Lua filter." <>
       "\n\nThe filter will be run in the current Lua process." <>
       "\n"
     )
  where
    copynext :: LuaError e => StackIndex -> LuaE e StackIndex
    copynext to =
      Lua.next (nth 2) >>= \case
        False -> pure to
        True -> do
          pushvalue (nth 2)
          insert (nth 2)
          rawset to
          copynext to
    copyOfGlobalTable :: LuaError e => LuaE e StackIndex
    copyOfGlobalTable = do
      newtable
      pushglobaltable
      pushnil
      (copynext =<< absindex (nth 3)) <* pop 1 -- pop source table

-- | Process the document with a JSON filter.
run_json_filter :: DocumentedFunction PandocError
run_json_filter = defun "run_json_filter"
  ### (\doc filterPath margs -> do
          args <- case margs of
                    Just xs -> return xs
                    Nothing -> do
                      Lua.getglobal "FORMAT"
                      (forcePeek ((:[]) <$!> peekString top) <* pop 1)
          applyJSONFilter def args filterPath doc
      )
  <#> parameter peekPandoc "Pandoc" "doc" "the Pandoc document to filter"
  <#> parameter peekString "string" "filter" "filter to run"
  <#> opt (parameter (peekList peekString) "{string,...}" "args"
           "list of arguments passed to the filter. Defaults to `{FORMAT}`.")
  =#> functionResult pushPandoc "Pandoc" "filtered document"
  #? "Filter the given doc by passing it through a JSON filter."

-- | Documented Lua function to compute the hash of a string.
sha1 :: DocumentedFunction e
sha1 = defun "sha1"
  ### liftPure (show . hashWith SHA1)
  <#> parameter peekByteString "string" "input" ""
  =#> functionResult pushString "string" "hexadecimal hash value"
  #? "Computes the SHA1 hash of the given string input."

-- | Convert pandoc structure to a string with formatting removed.
-- Footnotes are skipped (since we don't want their contents in link
-- labels).
stringify :: LuaError e => DocumentedFunction e
stringify = defun "stringify"
  ### (\idx ->
         forcePeek . retrieving "stringifyable element" $
         choice
         [ (fmap Shared.stringify . peekPandoc)
         , (fmap Shared.stringify . peekInline)
         , (fmap Shared.stringify . peekBlock)
         , (fmap Shared.stringify . peekCaption)
         , (fmap Shared.stringify . peekCell)
         , (fmap Shared.stringify . peekCitation)
         , (fmap Shared.stringify . peekTableHead)
         , (fmap Shared.stringify . peekTableFoot)
         , (fmap stringifyMetaValue . peekMetaValue)
         , (fmap (const "") . peekAttr)
         , (fmap (const "") . peekListAttributes)
         ] idx)
  <#> parameter pure "AST element" "element" "some pandoc AST element"
  =#> functionResult pushText "string"
        "A plain string representation of the given element."
  #? T.unlines
  [ "Converts the given element (Pandoc, Meta, Block, or Inline) into"
  , "a string with all formatting removed."
  ]
 where
  stringifyMetaValue :: MetaValue -> T.Text
  stringifyMetaValue mv = case mv of
    MetaBool b   -> T.toLower $ T.pack (show b)
    MetaString s -> s
    MetaList xs  -> mconcat $ map stringifyMetaValue xs
    MetaMap m    -> mconcat $ map (stringifyMetaValue . snd) (Map.toList m)
    _            -> Shared.stringify mv


to_roman_numeral :: LuaError e => DocumentedFunction e
to_roman_numeral = defun "to_roman_numeral"
  ### liftPure Shared.toRomanNumeral
  <#> parameter (peekIntegral @Int) "integer" "n"
        "positive integer below 4000"
  =#> functionResult pushText "string" "A roman numeral."
  #? T.unlines
  [ "Converts an integer < 4000 to uppercase roman numeral."
  , ""
  , "Usage:"
  , ""
  , "    local to_roman_numeral = pandoc.utils.to_roman_numeral"
  , "    local pandoc_birth_year = to_roman_numeral(2006)"
  , "    -- pandoc_birth_year == 'MMVI'"
  ]

-- | Converts a table into an old/simple table.
to_simple_table :: DocumentedFunction PandocError
to_simple_table = defun "to_simple_table"
  ### (\case
          Table _attr caption specs thead tbodies tfoot -> do
            let (capt, aligns, widths, headers, rows) =
                  Shared.toLegacyTable caption specs thead tbodies tfoot
            return $ SimpleTable capt aligns widths headers rows
          blk -> Lua.failLua $ mconcat
                 [ "Expected Table, got ", showConstr (toConstr blk), "." ])
  <#> parameter peekTable "Block" "tbl" "a table"
  =#> functionResult pushSimpleTable "SimpleTable" "SimpleTable object"
  #? T.unlines
  [ "Converts a table into an old/simple table."
  , ""
  , "Usage:"
  , ""
  , "    local simple = pandoc.utils.to_simple_table(table)"
  , "    -- modify, using pre pandoc 2.10 methods"
  , "    simple.caption = pandoc.SmallCaps(simple.caption)"
  , "    -- create normal table block again"
  , "    table = pandoc.utils.from_simple_table(simple)"
  ]
 where
  peekTable :: LuaError e => Peeker e Block
  peekTable idx = peekBlock idx >>= \case
    t@(Table {}) -> return t
    b -> Lua.failPeek $ mconcat
         [ "Expected Table, got "
         , UTF8.fromString $ showConstr (toConstr b)
         , "." ]

type' :: DocumentedFunction e
type' = defun "type"
  ### (\idx -> getmetafield idx "__name" >>= \case
          TypeString -> fromMaybe mempty <$> tostring top
          _ -> ltype idx >>= typename)
  <#> parameter pure "any" "value" "any Lua value"
  =#> functionResult pushByteString "string" "type of the given value"
  #? T.unlines
  [ "Pandoc-friendly version of Lua's default `type` function, returning"
  , "type information similar to what is presented in the manual."
  , ""
  , "The function works by checking the metafield `__name`. If the"
  , "argument has a string-valued metafield `__name`, then it returns"
  , "that string. Otherwise it behaves just like the normal `type`"
  , "function."
  , ""
  , "Usage:"
  , ""
  , "    -- Prints one of 'string', 'boolean', 'Inlines', 'Blocks',"
  , "    -- 'table', and 'nil', corresponding to the Haskell constructors"
  , "    -- MetaString, MetaBool, MetaInlines, MetaBlocks, MetaMap,"
  , "    -- and an unset value, respectively."
  , ""
  , "    function Meta (meta)"
  , "      print('type of metavalue `author`:', pandoc.utils.type(meta.author))"
  , "    end"
  ]
