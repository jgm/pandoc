{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RelaxedPolyRec      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}
{- |
   Module      : Text.Pandoc.Readers.Metadata
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Parse YAML/JSON metadata to 'Pandoc' 'Meta'.
-}
module Text.Pandoc.Readers.Metadata ( yamlBsToMeta ) where

import Prelude
import Control.Monad
import Control.Monad.Except (throwError)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.YAML as YAML
import qualified Data.YAML.Event as YE
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Builder (Blocks)
import Text.Pandoc.Class (PandocMonad (..))
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.Parsing hiding (tableWith)
import Text.Pandoc.Shared

yamlBsToMeta :: PandocMonad m
             => ParserT Text ParserState m (F Blocks)
             -> BL.ByteString
             -> ParserT Text ParserState m (F Meta)
yamlBsToMeta pBlocks bstr = do
  pos <- getPosition
  case YAML.decodeNode' YAML.failsafeSchemaResolver False False bstr of
       Right ((YAML.Doc (YAML.Mapping _ _ o)):_)
                -> (fmap Meta) <$> yamlMap pBlocks o
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

toMetaValue :: PandocMonad m
            => ParserT Text ParserState m (F Blocks)
            -> Text
            -> ParserT Text ParserState m (F MetaValue)
toMetaValue pBlocks x =
   -- Note: a standard quoted or unquoted YAML value will
   -- not end in a newline, but a "block" set off with
   -- `|` or `>` will.
   if "\n" `T.isSuffixOf` x
      then parseFromString' (asBlocks <$> pBlocks) (x <> "\n")
      else parseFromString' pInlines x
  where pInlines = do
          bs <- pBlocks
          return $ do
            bs' <- bs
            return $
              case B.toList bs' of
                [Plain ils] -> MetaInlines ils
                [Para ils]  -> MetaInlines ils
                xs          -> MetaBlocks xs
        asBlocks p = do
          p' <- p
          return $ MetaBlocks (B.toList p')

checkBoolean :: Text -> Maybe Bool
checkBoolean t =
  if t == T.pack "true" || t == T.pack "True" || t == T.pack "TRUE"
     then Just True
     else if t == T.pack "false" || t == T.pack "False" || t == T.pack "FALSE"
             then Just False
             else Nothing

yamlToMetaValue :: PandocMonad m
                => ParserT Text ParserState m (F Blocks)
                -> YAML.Node YE.Pos
                -> ParserT Text ParserState m (F MetaValue)
yamlToMetaValue pBlocks (YAML.Scalar _ x) =
  case x of
       YAML.SStr t       -> toMetaValue pBlocks t
       YAML.SBool b      -> return $ return $ MetaBool b
       YAML.SFloat d     -> return $ return $ MetaString $ tshow d
       YAML.SInt i       -> return $ return $ MetaString $ tshow i
       YAML.SUnknown _ t ->
         case checkBoolean t of
           Just b        -> return $ return $ MetaBool b
           Nothing       -> toMetaValue pBlocks t
       YAML.SNull        -> return $ return $ MetaString ""

yamlToMetaValue pBlocks (YAML.Sequence _ _ xs) = do
  xs' <- mapM (yamlToMetaValue pBlocks) xs
  return $ do
    xs'' <- sequence xs'
    return $ B.toMetaValue xs''
yamlToMetaValue pBlocks (YAML.Mapping _ _ o) =
  fmap B.toMetaValue <$> yamlMap pBlocks o
yamlToMetaValue _ _ = return $ return $ MetaString ""

yamlMap :: PandocMonad m
        => ParserT Text ParserState m (F Blocks)
        -> M.Map (YAML.Node YE.Pos) (YAML.Node YE.Pos)
        -> ParserT Text ParserState m (F (M.Map Text MetaValue))
yamlMap pBlocks o = do
    kvs <- forM (M.toList o) $ \(key, v) -> do
             k <- nodeToKey key
             return (k, v)
    let kvs' = filter (not . ignorable . fst) kvs
    (fmap M.fromList . sequence) <$> mapM toMeta kvs'
  where
    ignorable t = "_" `T.isSuffixOf` t
    toMeta (k, v) = do
      fv <- yamlToMetaValue pBlocks v
      return $ do
        v' <- fv
        return (k, v')

