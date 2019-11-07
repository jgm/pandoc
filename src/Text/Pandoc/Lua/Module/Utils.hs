{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-} -- TODO text: remove
{- |
   Module      : Text.Pandoc.Lua.Module.Utils
   Copyright   : Copyright © 2017-2019 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Utility module for lua, exposing internal helper functions.
-}
module Text.Pandoc.Lua.Module.Utils
  ( pushModule
  ) where

import Prelude
import Control.Applicative ((<|>))
import Data.Default (def)
import Data.Version (Version)
import Foreign.Lua (Peekable, Lua, NumResults)
import Text.Pandoc.Class (runIO, setUserDataDir)
import Text.Pandoc.Definition ( Pandoc, Meta, MetaValue (..), Block, Inline
                              , Citation, Attr, ListAttributes)
import Text.Pandoc.Lua.Marshaling ()
import Text.Pandoc.Lua.Util (addFunction)

import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.Filter.JSON as JSONFilter
import qualified Text.Pandoc.Shared as Shared

-- | Push the "pandoc.utils" module to the lua stack.
pushModule :: Maybe FilePath -> Lua NumResults
pushModule mbDatadir = do
  Lua.newtable
  addFunction "blocks_to_inlines" blocksToInlines
  addFunction "equals" equals
  addFunction "make_sections" makeSections
  addFunction "normalize_date" normalizeDate
  addFunction "run_json_filter" (runJSONFilter mbDatadir)
  addFunction "sha1" sha1
  addFunction "stringify" stringify
  addFunction "to_roman_numeral" toRomanNumeral
  addFunction "Version" (return :: Version -> Lua Version)
  return 1

-- | Squashes a list of blocks into inlines.
blocksToInlines :: [Block] -> Lua.Optional [Inline] -> Lua [Inline]
blocksToInlines blks optSep = do
  let sep = case Lua.fromOptional optSep of
              Just x -> B.fromList x
              Nothing -> Shared.defaultBlocksSeparator
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
runJSONFilter :: Maybe FilePath
              -> Pandoc
              -> FilePath
              -> Lua.Optional [T.Text]
              -> Lua NumResults
runJSONFilter mbDatadir doc filterFile optArgs = do
  args <- case Lua.fromOptional optArgs of
            Just x -> return x
            Nothing -> do
              Lua.getglobal "FORMAT"
              (:[]) <$> Lua.popValue
  filterRes <- Lua.liftIO . runIO $ do
    setUserDataDir mbDatadir
    JSONFilter.apply def (T.unpack <$> args) filterFile doc -- TODO text: refactor
  case filterRes of
    Left err -> Lua.raiseError (show err)
    Right d -> (1 :: NumResults) <$ Lua.push d

-- | Calculate the hash of the given contents.
sha1 :: BSL.ByteString
     -> Lua T.Text
sha1 = return . T.pack . SHA.showDigest . SHA.sha1

-- | Convert pandoc structure to a string with formatting removed.
-- Footnotes are skipped (since we don't want their contents in link
-- labels).
stringify :: AstElement -> Lua T.Text
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

equals :: AstElement -> AstElement -> Lua Bool
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
    res <- Lua.try $  (PandocElement <$> Lua.peek idx)
                  <|> (InlineElement <$> Lua.peek idx)
                  <|> (BlockElement <$> Lua.peek idx)
                  <|> (AttrElement <$> Lua.peek idx)
                  <|> (ListAttributesElement <$> Lua.peek idx)
                  <|> (MetaElement <$> Lua.peek idx)
                  <|> (MetaValueElement <$> Lua.peek idx)
    case res of
      Right x -> return x
      Left _ -> Lua.throwException
        "Expected an AST element, but could not parse value as such."

-- | Convert a number < 4000 to uppercase roman numeral.
toRomanNumeral :: Lua.Integer -> Lua T.Text
toRomanNumeral = return . Shared.toRomanNumeral . fromIntegral
