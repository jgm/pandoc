{-# LANGUAGE OverloadedStrings   #-}
{- |
   Module      : Text.Pandoc.Readers.Metadata
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Parse YAML/JSON metadata to 'Pandoc' 'Meta'.
-}
module Text.Pandoc.Readers.Metadata ( yamlBsToMeta, yamlMap ) where

import Control.Monad
import Control.Monad.Except (throwError)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.YAML as YAML
import qualified Data.YAML.Event as YE
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.Parsing hiding (tableWith)
import Text.Pandoc.Shared

yamlBsToMeta :: PandocMonad m
             => ParserT Text ParserState m (F MetaValue)
             -> BL.ByteString
             -> ParserT Text ParserState m (F Meta)
yamlBsToMeta pMetaValue bstr = do
  pos <- getPosition
  case YAML.decodeNode' YAML.failsafeSchemaResolver False False bstr of
       Right (YAML.Doc (YAML.Mapping _ _ o):_)
                -> fmap Meta <$> yamlMap pMetaValue o
       Right [] -> return . return $ mempty
       Right [YAML.Doc (YAML.Scalar _ YAML.SNull)]
                -> return . return $ mempty
       Right _  -> do logMessage $ CouldNotParseYamlMetadata "not an object"
                                   pos
                      return . return $ mempty
       Left (_pos, err')
                -> do logMessage $ CouldNotParseYamlMetadata
                                   (T.pack err') pos
                      return . return $ mempty

nodeToKey :: PandocMonad m
          => YAML.Node YE.Pos
          -> m Text
nodeToKey (YAML.Scalar _ (YAML.SStr t))       = return t
nodeToKey (YAML.Scalar _ (YAML.SUnknown _ t)) = return t
nodeToKey _  = throwError $ PandocParseError
                              "Non-string key in YAML mapping"

normalizeMetaValue :: PandocMonad m
                   => ParserT Text ParserState m (F MetaValue)
                   -> Text
                   -> ParserT Text ParserState m (F MetaValue)
normalizeMetaValue pMetaValue x =
   -- Note: a standard quoted or unquoted YAML value will
   -- not end in a newline, but a "block" set off with
   -- `|` or `>` will.
   if "\n" `T.isSuffixOf` x
      then parseFromString' pMetaValue (x <> "\n")
      else parseFromString' asInlines x
  where asInlines = fmap b2i <$> pMetaValue
        b2i (MetaBlocks [Plain ils]) = MetaInlines ils
        b2i (MetaBlocks [Para ils]) = MetaInlines ils
        b2i bs = bs

checkBoolean :: Text -> Maybe Bool
checkBoolean t
  | t == T.pack "true" || t == T.pack "True" || t == T.pack "TRUE" = Just True
  | t == T.pack "false" || t == T.pack "False" || t == T.pack "FALSE" = Just False
  | otherwise = Nothing

yamlToMetaValue :: PandocMonad m
                => ParserT Text ParserState m (F MetaValue)
                -> YAML.Node YE.Pos
                -> ParserT Text ParserState m (F MetaValue)
yamlToMetaValue pMetaValue (YAML.Scalar _ x) =
  case x of
       YAML.SStr t       -> normalizeMetaValue pMetaValue t
       YAML.SBool b      -> return $ return $ MetaBool b
       YAML.SFloat d     -> return $ return $ MetaString $ tshow d
       YAML.SInt i       -> return $ return $ MetaString $ tshow i
       YAML.SUnknown _ t ->
         case checkBoolean t of
           Just b        -> return $ return $ MetaBool b
           Nothing       -> normalizeMetaValue pMetaValue t
       YAML.SNull        -> return $ return $ MetaString ""

yamlToMetaValue pMetaValue (YAML.Sequence _ _ xs) =
  fmap MetaList . sequence
  <$> mapM (yamlToMetaValue pMetaValue) xs
yamlToMetaValue pMetaValue (YAML.Mapping _ _ o) =
  fmap MetaMap <$> yamlMap pMetaValue o
yamlToMetaValue _ _ = return $ return $ MetaString ""

yamlMap :: PandocMonad m
        => ParserT Text ParserState m (F MetaValue)
        -> M.Map (YAML.Node YE.Pos) (YAML.Node YE.Pos)
        -> ParserT Text ParserState m (F (M.Map Text MetaValue))
yamlMap pMetaValue o = do
    kvs <- forM (M.toList o) $ \(key, v) -> do
             k <- nodeToKey key
             return (k, v)
    let kvs' = filter (not . ignorable . fst) kvs
    fmap M.fromList . sequence <$> mapM toMeta kvs'
  where
    ignorable t = "_" `T.isSuffixOf` t
    toMeta (k, v) = do
      fv <- yamlToMetaValue pMetaValue v
      return $ do
        v' <- fv
        return (k, v')
