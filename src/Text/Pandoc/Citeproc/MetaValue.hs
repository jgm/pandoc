{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Citeproc.MetaValue
  ( referenceToMetaValue
  , metaValueToReference
  , metaValueToText
  )
where

import Citeproc.Types
import Text.Pandoc.Definition
import Text.Pandoc.Builder as B
import Text.Pandoc.Walk (query)
import Text.Pandoc.Shared (stringify)
import Data.Maybe
import Safe
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Text.Printf (printf)
import Control.Applicative ((<|>))

metaValueToText :: MetaValue -> Maybe Text
metaValueToText (MetaString t) = Just t
metaValueToText (MetaInlines ils) = Just $ stringify ils
metaValueToText (MetaBlocks bls) = Just $ stringify bls
metaValueToText (MetaList xs) = T.unwords <$> mapM metaValueToText xs
metaValueToText _ = Nothing

metaValueToBool :: MetaValue -> Maybe Bool
metaValueToBool (MetaBool b) = Just b
metaValueToBool (MetaString "true") = Just True
metaValueToBool (MetaString "false") = Just False
metaValueToBool (MetaInlines ils) =
  metaValueToBool (MetaString (stringify ils))
metaValueToBool _ = Nothing

referenceToMetaValue :: Reference Inlines -> MetaValue
referenceToMetaValue ref =
  let ItemId id' = referenceId ref
      type' = referenceType ref
   in MetaMap $ M.insert "id" (MetaString id')
              $ M.insert "type" (MetaString type')
              $ M.map valToMetaValue
              $ M.mapKeys fromVariable
              $ referenceVariables ref


valToMetaValue :: Val Inlines -> MetaValue
valToMetaValue (TextVal t) = MetaString t
valToMetaValue (FancyVal ils) = MetaInlines (B.toList ils)
valToMetaValue (NumVal n) = MetaString (T.pack $ show n)
valToMetaValue (NamesVal ns) = MetaList $ map nameToMetaValue ns
valToMetaValue (DateVal d) = dateToMetaValue d
valToMetaValue _ = MetaString mempty

nameToMetaValue :: Name -> MetaValue
nameToMetaValue name =
  MetaMap $
    (maybe id (M.insert "family" . MetaString) (nameFamily name)) .
    (maybe id (M.insert "given" . MetaString) (nameGiven name)) .
    (maybe id (M.insert "dropping-particle" . MetaString)
       (nameDroppingParticle name)) .
    (maybe id (M.insert "non-dropping-particle" . MetaString)
       (nameNonDroppingParticle name)) .
    (maybe id (M.insert "suffix" . MetaString) (nameSuffix name)) .
    (maybe id (M.insert "literal" . MetaString) (nameLiteral name)) .
    (if nameCommaSuffix name
        then M.insert "comma-suffix" (MetaBool True)
        else id) .
    (if nameStaticOrdering name
        then M.insert "static-ordering" (MetaBool True)
        else id)
    $ mempty

dateToMetaValue :: Date -> MetaValue
dateToMetaValue date =
  MetaString $
    (case dateLiteral date of
       Just l  -> l
       Nothing -> T.intercalate "/" $ map datePartsToEDTF $ dateParts date)
    <> (if dateCirca date then "~" else "")
 where
  datePartsToEDTF (DateParts dps) =
    T.pack $
     (case dps of
        (y:_) | y > 9999 || y < -10000 -> ('y':)
        _ -> id) $
    case dps of
      (y:m:d:_)
       | y < -1     -> printf "%05d-%02d-%02d" (y+1) m d
       | otherwise  -> printf "%04d-%02d-%02d" y m d
      (y:m:[])
       | y < -1     -> printf "%05d-%02d" (y+1) m
       | otherwise  -> printf "%04d-%02d" y m
      (y:[])
       | y == 0     -> printf "" -- used for open range
       | y < -1     -> printf "%05d" (y+1)
       | otherwise  -> printf "%04d" y
      _             -> mempty

metaValueToReference :: MetaValue
                     -> Maybe (Reference Inlines)
metaValueToReference (MetaMap m) = do
  let m' = M.mapKeys normalizeKey m
  id' <- M.lookup "id" m' >>= metaValueToText
  type' <- (M.lookup "type" m' >>= metaValueToText) <|> pure ""
  let m'' = M.delete "id" $ M.delete "type" m'
  let vars = M.mapKeys toVariable $ M.mapWithKey metaValueToVal m''
  return $ Reference { referenceId = ItemId id'
                     , referenceType = type'
                     , referenceDisambiguation = Nothing
                     , referenceVariables = vars }
metaValueToReference _ = Nothing

metaValueToVal :: Text -> MetaValue -> Val Inlines
metaValueToVal k v
  | k `Set.member` dateVariables
       = DateVal $ metaValueToDate v
  | k `Set.member` nameVariables
       = NamesVal $ metaValueToNames v
  | k == "other-ids"
       = TextVal $ fromMaybe mempty $ metaValueToText v
         -- will create space-separated list
  | otherwise =
    case v of
      MetaString t -> TextVal t
      MetaInlines ils -> FancyVal (B.fromList ils)
      MetaBlocks bs   -> FancyVal (B.fromList $ query id bs)
      MetaBool b   -> TextVal (if b then "true" else "false")
      MetaList _   -> TextVal mempty
      MetaMap  _   -> TextVal mempty

metaValueToDate :: MetaValue -> Date
metaValueToDate (MetaMap m) = fromMaybe
  (Date
   { dateParts = dateparts
   , dateCirca = circa
   , dateSeason = season
   , dateLiteral = literal })
  rawdate
 where
  dateparts = case M.lookup "date-parts" m of
                Just (MetaList xs) ->
                  mapMaybe metaValueToDateParts xs
                Just _ -> []
                Nothing ->
                  maybeToList $ metaValueToDateParts (MetaMap m)
  circa = fromMaybe False $
            M.lookup "circa" m >>= metaValueToBool
  season = M.lookup "season" m >>= metaValueToInt
  literal = M.lookup "literal" m >>= metaValueToText
  rawdate = M.lookup "raw" m >>= metaValueToText >>= rawDateEDTF
metaValueToDate (MetaList xs) =
  Date{ dateParts = mapMaybe metaValueToDateParts xs
      , dateCirca = False
      , dateSeason = Nothing
      , dateLiteral = Nothing }
metaValueToDate x =
  fromMaybe emptyDate $ metaValueToText x >>= rawDateEDTF


metaValueToInt :: MetaValue -> Maybe Int
metaValueToInt x = metaValueToText x >>= readMay . T.unpack

metaValueToDateParts :: MetaValue -> Maybe DateParts
metaValueToDateParts (MetaList xs) =
  Just $ DateParts $ map (fromMaybe 0 . metaValueToInt) xs
metaValueToDateParts (MetaMap m) =
  case (M.lookup "year" m >>= metaValueToInt,
        ((M.lookup "month" m >>= metaValueToInt)
          <|>
        ((+ 20) <$> (M.lookup "season" m >>= metaValueToInt))),
        M.lookup "day" m >>= metaValueToInt) of
    (Just y, Just mo, Just d)  -> Just $ DateParts [y, mo, d]
    (Just y, Just mo, Nothing) -> Just $ DateParts [y, mo]
    (Just y, Nothing, _)       -> Just $ DateParts [y]
    _                          -> Nothing
metaValueToDateParts _ = Nothing

emptyDate :: Date
emptyDate = Date { dateParts = []
                 , dateCirca = False
                 , dateSeason = Nothing
                 , dateLiteral = Nothing }

metaValueToNames :: MetaValue -> [Name]
metaValueToNames (MetaList xs) = mapMaybe metaValueToName xs
metaValueToNames x = maybeToList $ metaValueToName x

metaValueToName :: MetaValue -> Maybe Name
metaValueToName (MetaMap m) = extractParticles <$>
  Just Name
    { nameFamily = family
    , nameGiven = given
    , nameDroppingParticle = dropping
    , nameNonDroppingParticle = nondropping
    , nameSuffix = suffix
    , nameCommaSuffix = commasuffix
    , nameStaticOrdering = staticordering
    , nameLiteral = literal
    }
 where
  family = M.lookup "family" m >>= metaValueToText
  given  = M.lookup "given" m >>= metaValueToText
  dropping = M.lookup "dropping-particle" m
                >>= metaValueToText
  nondropping = M.lookup "non-dropping-particle" m
                >>= metaValueToText
  suffix = M.lookup "suffix" m >>= metaValueToText
  commasuffix = fromMaybe False $
    M.lookup "comma-suffix" m >>= metaValueToBool
  staticordering = fromMaybe False $
    M.lookup "static-ordering" m >>= metaValueToBool
  literal = M.lookup "literal" m >>= metaValueToText
metaValueToName x = extractParticles <$>
  case metaValueToText x of
    Nothing -> Nothing
    Just lit -> Just Name
                     { nameFamily = Nothing
                     , nameGiven = Nothing
                     , nameDroppingParticle = Nothing
                     , nameNonDroppingParticle = Nothing
                     , nameSuffix = Nothing
                     , nameCommaSuffix = False
                     , nameStaticOrdering = False
                     , nameLiteral = Just lit }

dateVariables :: Set.Set Text
dateVariables = Set.fromList
  [ "accessed", "container", "event-date", "issued",
    "original-date", "submitted" ]

nameVariables :: Set.Set Text
nameVariables = Set.fromList
  [ "author", "collection-editor", "composer",
    "container-author", "director", "editor",
    "editorial-director", "illustrator",
    "interviewer", "original-author",
    "recipient", "reviewed-author",
    "translator" ]

normalizeKey :: Text -> Text
normalizeKey k =
  case T.toLower k of
    "doi"   -> "DOI"
    "isbn"  -> "ISBN"
    "issn"  -> "ISSN"
    "pmcid" -> "PMCID"
    "pmid"  -> "PMID"
    "url"   -> "URL"
    x       -> x
