{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Utils
   Copyright   : Copyright © 2017-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Utility module for Lua, exposing internal helper functions.
-}
module Text.Pandoc.Lua.Module.Utils
  ( documentedModule
  , sha1
  ) where

import Control.Applicative ((<|>))
import Control.Monad ((<$!>))
import Data.Data (showConstr, toConstr)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Version (Version)
import HsLua as Lua
import HsLua.Module.Version (peekVersionFuzzy, pushVersion)
import Text.Pandoc.Citeproc (getReferences, processCitations)
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Filter (applyJSONFilter)
import Text.Pandoc.Lua.Marshal.AST
import Text.Pandoc.Lua.Marshal.Reference
import Text.Pandoc.Lua.PandocLua (PandocLua (unPandocLua))

import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.Shared as Shared
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Text.Pandoc.Writers.Shared as Shared

-- | Push the "pandoc.utils" module to the Lua stack.
documentedModule :: Module PandocError
documentedModule = Module
  { moduleName = "pandoc.utils"
  , moduleDescription = "pandoc utility functions"
  , moduleFields = []
  , moduleOperations = []
  , moduleFunctions =
    [ defun "blocks_to_inlines"
      ### (\blks mSep -> do
              let sep = maybe Shared.defaultBlocksSeparator B.fromList mSep
              return $ B.toList (Shared.blocksToInlinesWithSep sep blks))
      <#> parameter (peekList peekBlock) "list of blocks"
            "blocks" ""
      <#> opt (parameter (peekList peekInline) "list of inlines" "inline" "")
      =#> functionResult pushInlines "list of inlines" ""

    , defun "citeproc"
      ### unPandocLua . processCitations
      <#> parameter peekPandoc "Pandoc" "doc" "document"
      =#> functionResult pushPandoc "Pandoc" "processed document"
      #?  T.unwords
          [ "Process the citations in the file, replacing them with "
          , "rendered citations and adding a bibliography. "
          , "See the manual section on citation rendering for details."
          ]

    , defun "equals"
      ### equal
      <#> parameter pure "AST element" "elem1" ""
      <#> parameter pure "AST element" "elem2" ""
      =#> functionResult pushBool "boolean" "true iff elem1 == elem2"

    , defun "make_sections"
      ### liftPure3 Shared.makeSections
      <#> parameter peekBool "boolean" "numbering" "add header numbers"
      <#> parameter (\i -> (Nothing <$ peekNil i) <|> (Just <$!> peekIntegral i))
                    "integer or nil" "baselevel" ""
      <#> parameter (peekList peekBlock) "list of blocks"
            "blocks" "document blocks to process"
      =#> functionResult pushBlocks "list of Blocks"
            "processes blocks"

    , defun "normalize_date"
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

    , sha1

    , defun "Version"
      ### liftPure (id @Version)
      <#> parameter peekVersionFuzzy
            "version string, list of integers, or integer"
            "v" "version description"
      =#> functionResult pushVersion "Version" "new Version object"
      #? "Creates a Version object."

    , defun "references"
      ### (unPandocLua . getReferences Nothing)
      <#> parameter peekPandoc "Pandoc" "doc" "document"
      =#> functionResult (pushPandocList pushReference) "table"
            "lift of references"
      #? mconcat
         [ "Get references defined inline in the metadata and via an external "
         , "bibliography.  Only references that are actually cited in the "
         , "document (either with a genuine citation or with `nocite`) are "
         , "returned. URL variables are converted to links."
         ]

    , defun "run_json_filter"
      ### (\doc filterPath margs -> do
              args <- case margs of
                        Just xs -> return xs
                        Nothing -> do
                          Lua.getglobal "FORMAT"
                          (forcePeek ((:[]) <$!> peekString top) <* pop 1)
              applyJSONFilter def args filterPath doc
          )
      <#> parameter peekPandoc "Pandoc" "doc" "input document"
      <#> parameter peekString "filepath" "filter_path" "path to filter"
      <#> opt (parameter (peekList peekString) "list of strings"
               "args" "arguments to pass to the filter")
      =#> functionResult pushPandoc "Pandoc" "filtered document"

    , defun "stringify"
      ### stringify
      <#> parameter pure "AST element" "elem" "some pandoc AST element"
      =#> functionResult pushText "string" "stringified element"

    , defun "from_simple_table"
      ### from_simple_table
      <#> parameter peekSimpleTable "SimpleTable" "simple_tbl" ""
      =?> "Simple table"

    , defun "to_roman_numeral"
      ### liftPure Shared.toRomanNumeral
      <#> parameter (peekIntegral @Int) "integer" "n" "number smaller than 4000"
      =#> functionResult pushText "string" "roman numeral"
      #? "Converts a number < 4000 to uppercase roman numeral."

    , defun "to_simple_table"
      ### to_simple_table
      <#> parameter peekTable "Block" "tbl" "a table"
      =#> functionResult pushSimpleTable "SimpleTable" "SimpleTable object"
      #? "Converts a table into an old/simple table."

    , defun "type"
      ### (\idx -> getmetafield idx "__name" >>= \case
              TypeString -> fromMaybe mempty <$> tostring top
              _ -> ltype idx >>= typename)
      <#> parameter pure "any" "object" ""
      =#> functionResult pushByteString "string" "type of the given value"
    #? ("Pandoc-friendly version of Lua's default `type` function, " <>
        "returning the type of a value. If the argument has a " <>
        "string-valued metafield `__name`, then it gives that string. " <>
        "Otherwise it behaves just like the normal `type` function.")
    ]
  }

-- | Documented Lua function to compute the hash of a string.
sha1 :: DocumentedFunction e
sha1 = defun "sha1"
  ### liftPure (SHA.showDigest . SHA.sha1)
  <#> parameter (fmap BSL.fromStrict . peekByteString) "string" "input" ""
  =#> functionResult pushString "string" "hexadecimal hash value"
  #? "Compute the hash of the given string value."


-- | Convert pandoc structure to a string with formatting removed.
-- Footnotes are skipped (since we don't want their contents in link
-- labels).
stringify :: LuaError e => StackIndex -> LuaE e T.Text
stringify idx = forcePeek . retrieving "stringifyable element" $
  choice
  [ (fmap Shared.stringify . peekPandoc)
  , (fmap Shared.stringify . peekInline)
  , (fmap Shared.stringify . peekBlock)
  , (fmap Shared.stringify . peekCitation)
  , (fmap stringifyMetaValue . peekMetaValue)
  , (fmap (const "") . peekAttr)
  , (fmap (const "") . peekListAttributes)
  ] idx
 where
  stringifyMetaValue :: MetaValue -> T.Text
  stringifyMetaValue mv = case mv of
    MetaBool b   -> T.toLower $ T.pack (show b)
    MetaString s -> s
    MetaList xs  -> mconcat $ map stringifyMetaValue xs
    MetaMap m    -> mconcat $ map (stringifyMetaValue . snd) (Map.toList m)
    _            -> Shared.stringify mv

-- | Converts an old/simple table into a normal table block element.
from_simple_table :: SimpleTable -> LuaE PandocError NumResults
from_simple_table (SimpleTable capt aligns widths head' body) = do
  Lua.push $ Table
    nullAttr
    (Caption Nothing [Plain capt | not (null capt)])
    (zipWith (\a w -> (a, toColWidth w)) aligns widths)
    (TableHead nullAttr [blockListToRow head' | not (null head') ])
    [TableBody nullAttr 0 [] $ map blockListToRow body | not (null body)]
    (TableFoot nullAttr [])
  return (NumResults 1)
  where
    blockListToRow :: [[Block]] -> Row
    blockListToRow = Row nullAttr . map (B.simpleCell . B.fromList)

    toColWidth :: Double -> ColWidth
    toColWidth 0 = ColWidthDefault
    toColWidth w = ColWidth w

-- | Converts a table into an old/simple table.
to_simple_table :: Block -> LuaE PandocError SimpleTable
to_simple_table = \case
  Table _attr caption specs thead tbodies tfoot -> do
    let (capt, aligns, widths, headers, rows) =
          Shared.toLegacyTable caption specs thead tbodies tfoot
    return $ SimpleTable capt aligns widths headers rows
  blk -> Lua.failLua $ mconcat
         [ "Expected Table, got ", showConstr (toConstr blk), "." ]

peekTable :: LuaError e => Peeker e Block
peekTable idx = peekBlock idx >>= \case
  t@(Table {}) -> return t
  b -> Lua.failPeek $ mconcat
       [ "Expected Table, got "
       , UTF8.fromString $ showConstr (toConstr b)
       , "." ]
