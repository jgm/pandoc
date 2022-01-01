{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Readers.Metadata
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Parse YAML/JSON metadata to 'Pandoc' 'Meta'.
-}
module Text.Pandoc.Readers.Metadata (
  yamlBsToMeta,
  yamlBsToRefs,
  yamlMetaBlock,
  yamlMap ) where


import Control.Monad.Except (throwError)
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (Value(..), Object, Result(..), fromJSON, (.:?), withObject)
import Data.Aeson.Types (parse)
import Text.Pandoc.Shared (tshow)
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Definition hiding (Null)
import Text.Pandoc.Error
import Text.Pandoc.Parsing hiding (tableWith, parse)


import qualified Text.Pandoc.UTF8 as UTF8

yamlBsToMeta :: (PandocMonad m, HasLastStrPosition st)
             => ParserT Sources st m (Future st MetaValue)
             -> B.ByteString
             -> ParserT Sources st m (Future st Meta)
yamlBsToMeta pMetaValue bstr = do
  case Yaml.decodeAllEither' bstr of
       Right (Object o:_) -> fmap Meta <$> yamlMap pMetaValue o
       Right [] -> return . return $ mempty
       Right [Null] -> return . return $ mempty
       Right _  -> Prelude.fail "expected YAML object"
       Left err' -> do
         throwError $ PandocParseError
                    $ T.pack $ Yaml.prettyPrintParseException err'

-- Returns filtered list of references.
yamlBsToRefs :: (PandocMonad m, HasLastStrPosition st)
             => ParserT Sources st m (Future st MetaValue)
             -> (Text -> Bool) -- ^ Filter for id
             -> B.ByteString
             -> ParserT Sources st m (Future st [MetaValue])
yamlBsToRefs pMetaValue idpred bstr =
  case Yaml.decodeAllEither' bstr of
       Right (Object m : _) -> do
         let isSelected (String t) = idpred t
             isSelected _ = False
         let hasSelectedId (Object o) =
               case parse (withObject "ref" (.:? "id")) (Object o) of
                 Success (Just id') -> isSelected id'
                 _ -> False
             hasSelectedId _ = False
         case parse (withObject "metadata" (.:? "references")) (Object m) of
           Success (Just refs) -> sequence <$>
                 mapM (yamlToMetaValue pMetaValue) (filter hasSelectedId refs)
           _ -> return $ return []
       Right _ -> return . return $ []
       Left err' -> do
         throwError $ PandocParseError
                    $ T.pack $ Yaml.prettyPrintParseException err'

normalizeMetaValue :: (PandocMonad m, HasLastStrPosition st)
                   => ParserT Sources st m (Future st MetaValue)
                   -> Text
                   -> ParserT Sources st m (Future st MetaValue)
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

yamlToMetaValue :: (PandocMonad m, HasLastStrPosition st)
                => ParserT Sources st m (Future st MetaValue)
                -> Value
                -> ParserT Sources st m (Future st MetaValue)
yamlToMetaValue pMetaValue v =
  case v of
       String t -> normalizeMetaValue pMetaValue t
       Bool b -> return $ return $ MetaBool b
       Number d -> normalizeMetaValue pMetaValue $
         case fromJSON v of
           Success (x :: Int) -> tshow x
           _ -> tshow d
       Null -> return $ return $ MetaString ""
       Array{} -> do
         case fromJSON v of
           Error err' -> throwError $ PandocParseError $ T.pack err'
           Success xs -> fmap MetaList . sequence <$>
                          mapM (yamlToMetaValue pMetaValue) xs
       Object o -> fmap MetaMap <$> yamlMap pMetaValue o

yamlMap :: (PandocMonad m, HasLastStrPosition st)
        => ParserT Sources st m (Future st MetaValue)
        -> Object
        -> ParserT Sources st m (Future st (M.Map Text MetaValue))
yamlMap pMetaValue o = do
    case fromJSON (Object o) of
      Error err' -> throwError $ PandocParseError $ T.pack err'
      Success (m' :: M.Map Text Value) -> do
        let kvs = filter (not . ignorable . fst) $ M.toList m'
        fmap M.fromList . sequence <$> mapM toMeta kvs
  where
    ignorable t = "_" `T.isSuffixOf` t
    toMeta (k, v) = do
      fv <- yamlToMetaValue pMetaValue v
      return $ do
        v' <- fv
        return (k, v')

-- | Parse a YAML metadata block using the supplied 'MetaValue' parser.
yamlMetaBlock :: (HasLastStrPosition st, PandocMonad m)
              => ParserT Sources st m (Future st MetaValue)
              -> ParserT Sources st m (Future st Meta)
yamlMetaBlock parser = try $ do
  string "---"
  blankline
  notFollowedBy blankline  -- if --- is followed by a blank it's an HRULE
  rawYamlLines <- manyTill anyLine stopLine
  -- by including --- and ..., we allow yaml blocks with just comments:
  let rawYaml = T.unlines ("---" : (rawYamlLines ++ ["..."]))
  optional blanklines
  yamlBsToMeta parser $ UTF8.fromText rawYaml

stopLine :: Monad m => ParserT Sources st m ()
stopLine = try $ (string "---" <|> string "...") >> blankline >> return ()
