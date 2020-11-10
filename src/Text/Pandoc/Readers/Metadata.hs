{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Readers.Metadata
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Parse YAML/JSON metadata to 'Pandoc' 'Meta'.
-}
module Text.Pandoc.Readers.Metadata (
  yamlBsToMeta,
  yamlBsToRefs,
  yamlMap ) where

import Control.Monad
import Control.Monad.Except (throwError)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.YAML as YAML
import qualified Data.YAML.Event as YE
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Parsing hiding (tableWith)
import Text.Pandoc.Shared

yamlBsToMeta :: PandocMonad m
             => ParserT Text ParserState m (F MetaValue)
             -> BL.ByteString
             -> ParserT Text ParserState m (F Meta)
yamlBsToMeta pMetaValue bstr = do
  case YAML.decodeNode' YAML.failsafeSchemaResolver False False bstr of
       Right (YAML.Doc (YAML.Mapping _ _ o):_)
                -> fmap Meta <$> yamlMap pMetaValue o
       Right [] -> return . return $ mempty
       Right [YAML.Doc (YAML.Scalar _ YAML.SNull)]
                -> return . return $ mempty
       Right _  -> Prelude.fail "expected YAML object"
       Left (yamlpos, err')
                -> do pos <- getPosition
                      setPosition $ incSourceLine
                            (setSourceColumn pos (YE.posColumn yamlpos))
                            (YE.posLine yamlpos - 1)
                      Prelude.fail err'

fakePos :: YAML.Pos
fakePos = YAML.Pos (-1) (-1) 1 0

lookupYAML :: Text
           -> YAML.Node YE.Pos
           -> Maybe (YAML.Node YE.Pos)
lookupYAML t (YAML.Mapping _ _ m) =
  M.lookup (YAML.Scalar fakePos (YAML.SUnknown YE.untagged t)) m
    `mplus`
    M.lookup (YAML.Scalar fakePos (YAML.SStr t)) m
lookupYAML _ _ = Nothing

-- Returns filtered list of references.
yamlBsToRefs :: PandocMonad m
             => ParserT Text ParserState m (F MetaValue)
             -> (Text -> Bool) -- ^ Filter for id
             -> BL.ByteString
             -> ParserT Text ParserState m (F [MetaValue])
yamlBsToRefs pMetaValue idpred bstr =
  case YAML.decodeNode' YAML.failsafeSchemaResolver False False bstr of
       Right (YAML.Doc o@YAML.Mapping{}:_)
                -> case lookupYAML "references" o of
                     Just (YAML.Sequence _ _ ns) -> do
                       let g n = case lookupYAML "id" n of
                                    Just n' ->
                                      case nodeToKey n' of
                                        Nothing -> False
                                        Just t -> idpred t ||
                                          case lookupYAML "other-ids" n of
                                            Just (YAML.Sequence _ _ ns') ->
                                              let ts' = mapMaybe nodeToKey ns'
                                               in any idpred ts'
                                            _ -> False
                                    Nothing   -> False
                       sequence <$>
                         mapM (yamlToMetaValue pMetaValue) (filter g ns)
                     Just _ ->
                       Prelude.fail "expecting sequence in 'references' field"
                     Nothing ->
                       Prelude.fail "expecting 'references' field"

       Right [] -> return . return $ mempty
       Right [YAML.Doc (YAML.Scalar _ YAML.SNull)]
                -> return . return $ mempty
       Right _  -> Prelude.fail "expecting YAML object"
       Left (_pos, err')
                -> Prelude.fail err'


nodeToKey :: YAML.Node YE.Pos -> Maybe Text
nodeToKey (YAML.Scalar _ (YAML.SStr t))       = Just t
nodeToKey (YAML.Scalar _ (YAML.SUnknown _ t)) = Just t
nodeToKey _                                   = Nothing

normalizeMetaValue :: PandocMonad m
                   => ParserT Text ParserState m (F MetaValue)
                   -> Text
                   -> ParserT Text ParserState m (F MetaValue)
normalizeMetaValue pMetaValue x =
   -- Note: a standard quoted or unquoted YAML value will
   -- not end in a newline, but a "block" set off with
   -- `|` or `>` will.
   if "\n" `T.isSuffixOf` T.dropWhileEnd isSpaceChar x -- see #6823
      then parseFromString' pMetaValue (x <> "\n")
      else parseFromString' asInlines x
  where asInlines = fmap b2i <$> pMetaValue
        b2i (MetaBlocks [Plain ils]) = MetaInlines ils
        b2i (MetaBlocks [Para ils]) = MetaInlines ils
        b2i bs = bs
        isSpaceChar ' '  = True
        isSpaceChar '\t' = True
        isSpaceChar _    = False

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
             k <- maybe (throwError $ PandocParseError
                            "Non-string key in YAML mapping")
                        return $ nodeToKey key
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
