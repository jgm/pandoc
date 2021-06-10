{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Utils
   Copyright   : Copyright © 2017-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Utility module for Lua, exposing internal helper functions.
-}
module Text.Pandoc.Lua.Module.Utils
  ( pushModule
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Catch (try)
import Data.Data (showConstr, toConstr)
import Data.Default (def)
import Data.Version (Version)
import Foreign.Lua (Peekable, Lua, NumResults (..))
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Marshaling ()
import Text.Pandoc.Lua.Marshaling.SimpleTable
  ( SimpleTable (..)
  , pushSimpleTable
  )
import Text.Pandoc.Lua.PandocLua (PandocLua, addFunction, liftPandocLua)

import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.Filter.JSON as JSONFilter
import qualified Text.Pandoc.Shared as Shared
import qualified Text.Pandoc.Writers.Shared as Shared

-- | Push the "pandoc.utils" module to the Lua stack.
pushModule :: PandocLua NumResults
pushModule = do
  liftPandocLua Lua.newtable
  addFunction "blocks_to_inlines" blocksToInlines
  addFunction "equals" equals
  addFunction "from_simple_table" from_simple_table
  addFunction "make_sections" makeSections
  addFunction "normalize_date" normalizeDate
  addFunction "run_json_filter" runJSONFilter
  addFunction "sha1" sha1
  addFunction "stringify" stringify
  addFunction "to_roman_numeral" toRomanNumeral
  addFunction "to_simple_table" to_simple_table
  addFunction "Version" (return :: Version -> Lua Version)
  return 1

-- | Squashes a list of blocks into inlines.
blocksToInlines :: [Block] -> Lua.Optional [Inline] -> PandocLua [Inline]
blocksToInlines blks optSep = liftPandocLua $ do
  let sep = maybe Shared.defaultBlocksSeparator B.fromList
            $ Lua.fromOptional optSep
  return $ B.toList (Shared.blocksToInlinesWithSep sep blks)

-- | Convert list of Pandoc blocks into sections using Divs.
makeSections :: Bool -> Lua.Optional Int -> [Block] -> Lua [Block]
makeSections number baselevel =
  return . Shared.makeSections number (Lua.fromOptional baselevel)

-- | Parse a date and convert (if possible) to "YYYY-MM-DD" format. We
-- limit years to the range 1601-9999 (ISO 8601 accepts greater than
-- or equal to 1583, but MS Word only accepts dates starting 1601).
-- Returns nil instead of a string if the conversion failed.
normalizeDate :: T.Text -> Lua (Lua.Optional T.Text)
normalizeDate = return . Lua.Optional . Shared.normalizeDate

-- | Run a JSON filter on the given document.
runJSONFilter :: Pandoc
              -> FilePath
              -> Lua.Optional [String]
              -> PandocLua Pandoc
runJSONFilter doc filterFile optArgs = do
  args <- case Lua.fromOptional optArgs of
            Just x -> return x
            Nothing -> liftPandocLua $ do
              Lua.getglobal "FORMAT"
              (:[]) <$> Lua.popValue
  JSONFilter.apply def args filterFile doc

-- | Calculate the hash of the given contents.
sha1 :: BSL.ByteString
     -> Lua T.Text
sha1 = return . T.pack . SHA.showDigest . SHA.sha1

-- | Convert pandoc structure to a string with formatting removed.
-- Footnotes are skipped (since we don't want their contents in link
-- labels).
stringify :: AstElement -> PandocLua T.Text
stringify el = return $ case el of
  PandocElement pd -> Shared.stringify pd
  InlineElement i  -> Shared.stringify i
  BlockElement b   -> Shared.stringify b
  MetaElement m    -> Shared.stringify m
  CitationElement c  -> Shared.stringify c
  MetaValueElement m -> stringifyMetaValue m
  _                  -> mempty

stringifyMetaValue :: MetaValue -> T.Text
stringifyMetaValue mv = case mv of
  MetaBool b   -> T.toLower $ T.pack (show b)
  MetaString s -> s
  _            -> Shared.stringify mv

equals :: AstElement -> AstElement -> PandocLua Bool
equals e1 e2 = return (e1 == e2)

data AstElement
  = PandocElement Pandoc
  | MetaElement Meta
  | BlockElement Block
  | InlineElement Inline
  | MetaValueElement MetaValue
  | AttrElement Attr
  | ListAttributesElement ListAttributes
  | CitationElement Citation
  deriving (Eq, Show)

instance Peekable AstElement where
  peek idx  = do
    res <- try $  (PandocElement <$> Lua.peek idx)
              <|> (InlineElement <$> Lua.peek idx)
              <|> (BlockElement <$> Lua.peek idx)
              <|> (AttrElement <$> Lua.peek idx)
              <|> (ListAttributesElement <$> Lua.peek idx)
              <|> (MetaElement <$> Lua.peek idx)
              <|> (MetaValueElement <$> Lua.peek idx)
    case res of
      Right x -> return x
      Left (_ :: PandocError) -> Lua.throwMessage
        "Expected an AST element, but could not parse value as such."

-- | Converts an old/simple table into a normal table block element.
from_simple_table :: SimpleTable -> Lua NumResults
from_simple_table (SimpleTable capt aligns widths head' body) = do
  Lua.push $ Table
    nullAttr
    (Caption Nothing [Plain capt])
    (zipWith (\a w -> (a, toColWidth w)) aligns widths)
    (TableHead nullAttr [blockListToRow head' | not (null head') ])
    [TableBody nullAttr 0 [] $ map blockListToRow body]
    (TableFoot nullAttr [])
  return (NumResults 1)
  where
    blockListToRow :: [[Block]] -> Row
    blockListToRow = Row nullAttr . map (B.simpleCell . B.fromList)

    toColWidth :: Double -> ColWidth
    toColWidth 0 = ColWidthDefault
    toColWidth w = ColWidth w

-- | Converts a table into an old/simple table.
to_simple_table :: Block -> Lua NumResults
to_simple_table = \case
  Table _attr caption specs thead tbodies tfoot -> do
    let (capt, aligns, widths, headers, rows) =
          Shared.toLegacyTable caption specs thead tbodies tfoot
    pushSimpleTable $ SimpleTable capt aligns widths headers rows
    return (NumResults 1)
  blk ->
    Lua.throwMessage $
      "Expected Table, got " <> showConstr (toConstr blk) <> "."

-- | Convert a number < 4000 to uppercase roman numeral.
toRomanNumeral :: Lua.Integer -> PandocLua T.Text
toRomanNumeral = return . Shared.toRomanNumeral . fromIntegral
