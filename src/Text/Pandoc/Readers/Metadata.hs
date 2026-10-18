{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{- |
   Module      : Text.Pandoc.Readers.Metadata
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
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
  yamlMap,
  OrderedYaml(..),
  orderedYamlMap ) where


import Control.Monad.Except (throwError)
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Map.Ordered as OM
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Yamlet as Y
import qualified Yamlet.Syntax as YS
import Yamlet.Aeson ()
import Data.Aeson (Value(..), Object, Result(..), fromJSON)
import Text.Pandoc.Shared (tshow, blocksToInlines)
import Text.Pandoc.Class (PandocMonad (..))
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Parsing hiding (tableWith, parse)
import qualified Text.Pandoc.UTF8 as UTF8

yamlBsToMeta :: (PandocMonad m, HasLastStrPosition st)
             => ParsecT Sources st m (Future st MetaValue)
             -> B.ByteString
             -> ParsecT Sources st m (Future st Meta)
yamlBsToMeta pMetaValue bstr = do
  pos <- getPosition
  case Y.decodeAll @OrderedYaml bstr of
       Right xs ->
         case xs of
           (OYMapping kvs : _) -> fmap Meta <$> orderedYamlMap pMetaValue kvs
           [OYScalar Null] -> return . return $ mempty
           [] -> return . return $ mempty
           _  -> Prelude.fail "expected YAML object"
       Left errs -> throwError $ PandocParseError $ T.pack $
         "Error parsing YAML metadata at " <> show pos <> ":\n" <>
         renderYamlErrors pos errs

-- | Render the errors with their lines in the source, given the position of
-- the first line of the YAML input.
renderYamlErrors :: SourcePos -> NE.NonEmpty Y.Error -> String
renderYamlErrors pos =
  L.intercalate "\n" . map (Y.prettyError (sourceName pos) . shift) . NE.toList
  where
    shift :: Y.Error -> Y.Error
    shift err@Y.Error{Y.location = loc@Y.Location{Y.line = l}}
      -- Line 0 means that the error has no position.
      | l == 0 = err
      | otherwise = err{Y.location = loc{Y.line = l + sourceLine pos - 1}}

-- Returns filtered list of references.
yamlBsToRefs :: (PandocMonad m, HasLastStrPosition st)
             => ParsecT Sources st m (Future st MetaValue)
             -> (Text -> Bool) -- ^ Filter for id
             -> B.ByteString
             -> ParsecT Sources st m (Future st [MetaValue])
yamlBsToRefs pMetaValue idpred bstr = do
  pos <- getPosition
  case Y.decodeAll @OrderedYaml bstr of
       Right (OYMapping kvs : _) ->
         case lookup "references" kvs of
           Just (OYSequence refs) -> sequence <$>
                 mapM (yamlToMetaValue pMetaValue) (filter hasSelectedId refs)
           _ -> return $ return []
       Right (OYSequence refs : _) ->
         sequence <$> mapM (yamlToMetaValue pMetaValue) (filter hasSelectedId refs)
       Right _ -> return . return $ []
       Left errs -> throwError $ PandocParseError
                               $ T.pack $ renderYamlErrors pos errs
 where
   hasSelectedId (OYMapping kvs) =
     case lookup "id" kvs of
       Just (OYScalar (String t)) -> idpred t
       _ -> False
   hasSelectedId _ = False

-- | A YAML value whose mappings keep their entries in the order of the
-- input.  Keys are the text of scalar keys, and scalars are converted
-- to aeson 'Value's, both as in the 'Y.FromYaml' instance of 'Value'.
data OrderedYaml = OYMapping [(Text, OrderedYaml)]
                 | OYSequence [OrderedYaml]
                 | OYScalar Value

instance Y.FromYaml OrderedYaml where
  parseYaml = Y.parseNode $ \n -> case n of
    YS.Node{YS.content = YS.MappingContent _ kvs} -> OYMapping <$> traverse entry kvs
    YS.Node{YS.content = YS.SequenceContent _ xs} ->
      OYSequence <$> Y.parseItems Y.parseYaml xs
    _ -> OYScalar <$> Y.parseYaml n
   where
    entry (k, v) = (,) <$> Y.parseNode keyText k <*> Y.parseNode Y.parseYaml v
    keyText k = case k of
      YS.Node{YS.content = YS.ScalarContent _ t} -> pure t
      _ -> Y.typeMismatch "a scalar key" k

-- | Convert an aeson 'Value'. The keys of an object come out sorted,
-- because an aeson 'Object' does not record their order.
aesonToOrderedYaml :: Value -> OrderedYaml
aesonToOrderedYaml v =
  case v of
    Object _ | Success (m :: M.Map Text Value) <- fromJSON v ->
      OYMapping [(k, aesonToOrderedYaml x) | (k, x) <- M.toList m]
    Array xs -> OYSequence (map aesonToOrderedYaml (V.toList xs))
    _ -> OYScalar v

normalizeMetaValue :: (PandocMonad m, HasLastStrPosition st)
                   => ParsecT Sources st m (Future st MetaValue)
                   -> Text
                   -> ParsecT Sources st m (Future st MetaValue)
normalizeMetaValue pMetaValue x =
   -- Note: a standard quoted or unquoted YAML value will
   -- not end in a newline, but a "block" set off with
   -- `|` or `>` will.
   if "\n" `T.isSuffixOf` (T.dropWhileEnd isSpaceChar x) -- see #6823
      then parseFromString' pMetaValue (x <> "\n\n")
      else try (parseFromString' asInlines x') -- see #8358
           <|> -- see #8465
           parseFromString' asInlines (x' <> "\n\n")
  where x' = T.dropWhile isSpaceOrNlChar x
        asInlines = fmap b2i <$> pMetaValue
        b2i (MetaBlocks bs) = MetaInlines (blocksToInlines bs)
        b2i y = y
        isSpaceChar ' '  = True
        isSpaceChar '\t' = True
        isSpaceChar _    = False
        isSpaceOrNlChar '\r' = True
        isSpaceOrNlChar '\n' = True
        isSpaceOrNlChar c = isSpaceChar c

yamlToMetaValue :: (PandocMonad m, HasLastStrPosition st)
                => ParsecT Sources st m (Future st MetaValue)
                -> OrderedYaml
                -> ParsecT Sources st m (Future st MetaValue)
yamlToMetaValue pMetaValue ov =
  case ov of
       OYMapping kvs -> fmap MetaMap <$> orderedYamlMap pMetaValue kvs
       OYSequence xs -> fmap MetaList . sequence <$>
                          mapM (yamlToMetaValue pMetaValue) xs
       OYScalar v ->
         case v of
           String t -> normalizeMetaValue pMetaValue t
           Bool b -> return $ return $ MetaBool b
           Number d -> normalizeMetaValue pMetaValue $
             case fromJSON v of
               Success (x :: Int) -> tshow x
               _ -> tshow d
           Null -> return $ return $ MetaString ""
           _ -> yamlToMetaValue pMetaValue (aesonToOrderedYaml v)

-- | Convert the entries of a YAML mapping, keeping their order.
orderedYamlMap :: (PandocMonad m, HasLastStrPosition st)
               => ParsecT Sources st m (Future st MetaValue)
               -> [(Text, OrderedYaml)]
               -> ParsecT Sources st m (Future st (OM.OMap Text MetaValue))
orderedYamlMap pMetaValue kvs =
  fmap OM.fromList . sequence <$>
    mapM toMeta (filter (not . ignorable . fst) kvs)
  where
    ignorable t = "_" `T.isSuffixOf` t
    toMeta (k, v) = do
      fv <- yamlToMetaValue pMetaValue v
      return $ do
        v' <- fv
        return (k, v')

-- | Convert an aeson 'Object' (e.g. from a defaults file). Its keys come
-- out sorted, because an aeson 'Object' does not record their order.
yamlMap :: (PandocMonad m, HasLastStrPosition st)
        => ParsecT Sources st m (Future st MetaValue)
        -> Object
        -> ParsecT Sources st m (Future st (OM.OMap Text MetaValue))
yamlMap pMetaValue o =
  case aesonToOrderedYaml (Object o) of
    OYMapping kvs -> orderedYamlMap pMetaValue kvs
    _ -> return $ return OM.empty

-- | Parse a YAML metadata block using the supplied 'MetaValue' parser.
yamlMetaBlock :: (HasLastStrPosition st, PandocMonad m)
              => ParsecT Sources st m (Future st MetaValue)
              -> ParsecT Sources st m (Future st Meta)
yamlMetaBlock parser = try $ do
  pos <- getPosition
  string "---"
  blankline
  notFollowedBy blankline  -- if --- is followed by a blank it's an HRULE
  rawYamlLines <- manyTill anyLine stopLine
  -- by including --- and ..., we allow yaml blocks with just comments:
  let rawYaml = T.unlines ("---" : (rawYamlLines ++ ["..."]))
  optional blanklines
  oldPos <- getPosition
  setPosition pos
  res <- yamlBsToMeta parser $ UTF8.fromText rawYaml
  setPosition oldPos
  pure res

stopLine :: Monad m => ParsecT Sources st m ()
stopLine = try $ (string "---" <|> string "...") >> blankline >> return ()
