{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.RIS
   Copyright   : Copyright (C) 2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Parses RIS bibliographies into a Pandoc document
with empty body and `references` and `nocite` fields
in the metadata.  A wildcard `nocite` is used so that
if the document is rendered in another format, the
entire bibliography will be printed.
-}
module Text.Pandoc.Readers.RIS
  ( readRIS
  )
where

import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Parsing
import Data.Char (isAsciiUpper, isDigit, isSpace, ord, chr)
import Data.List (foldl')
import Citeproc (Reference(..), ItemId(..), Val(..), Date(..), DateParts(..),
                 toVariable)
import Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Citeproc.MetaValue (referenceToMetaValue)
import Text.Pandoc.Citeproc.BibTeX (toName)
import Control.Monad.Except (throwError)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Safe (readMay)

-- | Read RIS from an input string and return a Pandoc document.
-- The document will have only metadata, with an empty body.
-- The metadata will contain a `references` field with the
-- bibliography entries, and a `nocite` field with the wildcard `[@*]`.
readRIS :: (PandocMonad m, ToSources a)
        => ReaderOptions -> a -> m Pandoc
readRIS _opts inp = do
  parsed <- readWithM risReferences () inp
  case parsed of
    Right refs -> do
      refs' <- mapM (traverse (return . text)) refs
      return $
        setMeta "nocite" (cite [Citation {citationId = "*"
                                         , citationPrefix = []
                                         , citationSuffix = []
                                         , citationMode = NormalCitation
                                         , citationNoteNum = 0
                                         , citationHash = 0}] (str "[@*]")) $
        setMeta "references" (map referenceToMetaValue refs') $
        B.doc mempty
    Left e       -> throwError e

type RISParser m = ParserT Sources () m

risLine :: PandocMonad m => RISParser m (Text, Text)
risLine = do
  key <- T.pack <$> count 2 (satisfy (\c -> isAsciiUpper c || isDigit c))
  _ <- many1 spaceChar
  char '-'
  val <- (many1 spaceChar *> anyLine) <|> mempty <$ newline
  return (key, T.strip val)

risSeparator :: PandocMonad m => RISParser m ()
risSeparator = do
  try $ string "ER"
  _ <- many1 spaceChar
  char '-'
  _ <- anyLine
  optional blanklines

risRecord :: PandocMonad m => RISParser m [(Text, Text)]
risRecord = manyTill risLine risSeparator

risRecordToReference :: [(Text, Text)] -> Reference Text
risRecordToReference keys = addId $ foldr go defref keys
 where
   go (key, val) =
     case key of
       "TY" -> \ref -> ref{ referenceType =
          fromMaybe "misc" (M.lookup val risTypes) }
       "ID" -> \ref -> ref{ referenceId = ItemId val }
       "VL" -> addVar "volume" val
       "KW" -> \ref ->
         ref{ referenceVariables =
               M.alter (\x -> case x of
                           Nothing -> Just $ TextVal val
                           Just (TextVal kws)
                                   -> Just (TextVal (kws <> ", " <> val))
                           _ -> x)
               "keyword"
               (referenceVariables ref) }
       "PB" -> addVar "publisher" val
       "PP" -> addVar "publisher-place" val
       "DO" -> addVar "DOI" val
       "SP" -> \ref ->
         case M.lookup "page" (referenceVariables ref) of
           Nothing -> addVar "page" val ref
           Just (FancyVal eg) -> addVar "page" (val <> eg) ref
           _ -> ref
       "EP" -> \ref ->
         case M.lookup "page" (referenceVariables ref) of
           Nothing -> addVar "page" ("-" <> val) ref
           Just (FancyVal eg) -> addVar "page" (val <> "-" <> eg) ref
           _ -> ref
       "AU" -> addName "author" val
       "A1" -> addName "author" val
       "ED" -> addName "editor" val
       "A2" -> addName "editor" val
       "TI" -> addVar "title" val
       "T1" -> addVar "title" val
       "CT" -> addVar "title" val
       "BT" -> \ref ->
         if referenceType ref == "book"
            then addVar "title" val ref
            else addVar "container-title" val ref
       "JO" -> addVar "container-title" val
       "JF" -> addVar "container-title" val
       "T2" -> addVar "container-title" val
       "ET" -> addVar "edition" val
       "NV" -> addVar "number-of-volumes" val
       "AB" -> addVar "abstract" val
       "PY" -> addYear "issued" val
       "Y1" -> addYear "issued" val
       "IS" -> addVar "issue" val
       "SN" -> addVar "ISSN" val
       "LA" -> addVar "language" val
       "UR" -> addVar "url" val
       "LK" -> addVar "url" val
       _ -> id -- TODO
   addVar k v r = r{ referenceVariables =
                       M.insert (toVariable k) (FancyVal v)
                       (referenceVariables r) }
   addName k v r =
     let new = toName [] . B.toList .  B.text $ v
         f Nothing   = Just (NamesVal new)
         f (Just (NamesVal ns)) = Just (NamesVal (new ++ ns))
         f (Just x) = Just x
      in r{ referenceVariables =
              M.alter f k (referenceVariables r) }
   addYear k v r =
     let d = DateVal $
              case readMay (T.unpack v) of
                Nothing ->
                  Date { dateParts = []
                       , dateCirca = False
                       , dateSeason = Nothing
                       , dateLiteral = Just v }
                Just y ->
                  Date { dateParts = [DateParts [y]]
                       , dateCirca = False
                       , dateSeason = Nothing
                       , dateLiteral = Nothing }
      in r{ referenceVariables = M.insert k d (referenceVariables r) }

   defref = Reference{
       referenceId = mempty
     , referenceType = mempty
     , referenceDisambiguation = Nothing
     , referenceVariables = mempty }
   addId rec =
     if referenceId rec == mempty
        then rec{ referenceId = ItemId (authors <> pubdate) }
        else rec
   authors = T.intercalate "_" $
               [T.takeWhile (\c -> c /= ',' && not (isSpace c)) n
                 | (k, n) <- keys, k == "AU" || k == "A1"]
   pubdate = mconcat ["_" <> d | (k, d) <- keys, k == "PY" || k == "Y1"]

risReferences :: PandocMonad m => RISParser m [Reference Text]
risReferences = do
  recs <- many risRecord
  spaces
  eof
  return $ fixDuplicateIds $ map risRecordToReference recs

fixDuplicateIds :: [Reference Text] -> [Reference Text]
fixDuplicateIds = reverse . snd . foldl' go (mempty, [])
 where
   go (ids_seen, refs) ref =
     case M.lookup (referenceId ref) ids_seen of
       Nothing -> (M.insert (referenceId ref) (ord 'a') ids_seen, ref:refs)
       Just n  -> (M.insert (referenceId ref) (n+1) ids_seen,
                     ref{ referenceId =
                          ItemId . (<> T.singleton (chr n)) . unItemId $
                           referenceId ref }
                    : refs)

risTypes :: M.Map Text Text
risTypes = M.fromList
    [ ("ABST", "article")
    , ("ADVS", "motion-picture")
    , ("AGGR", "dataset")
    , ("ANCIENT", "book")
    , ("ART", "graphic")
    , ("BILL", "bill")
    , ("BLOG", "post-weblog")
    , ("BOOK", "book")
    , ("CASE", "legal_case")
    , ("CHAP", "chapter")
    , ("CHART", "graphic")
    , ("CLSWK", "book")
    , ("COMP", "program")
    , ("CONF", "paper-conference")
    , ("CPAPER", "paper-conference")
    , ("CTLG", "catalog")
    , ("DATA", "dataset")
    , ("DBASE", "dataset")
    , ("DICT", "book")
    , ("EBOOK", "book")
    , ("ECHAP", "chapter")
    , ("EDBOOK", "book")
    , ("EJOUR", "article")
    , ("WEB", "webpage")
    , ("ENCYC", "entry-encyclopedia")
    , ("EQUA", "figure")
    , ("FIGURE", "figure")
    , ("GEN", "entry")
    , ("GOVDOC", "report")
    , ("GRANT", "report")
    , ("HEAR", "report")
    , ("ICOMM", "personal_communication")
    , ("INPR", "article-journal")
    , ("JFULL", "article-journal")
    , ("JOUR", "article-journal")
    , ("LEGAL", "legal_case")
    , ("MANSCPT", "manuscript")
    , ("MAP", "map")
    , ("MGZN", "article-magazine")
    , ("MPCT", "motion-picture")
    , ("MULTI", "webpage")
    , ("MUSIC", "musical_score")
    , ("NEWS", "article-newspaper")
    , ("PAMP", "pamphlet")
    , ("PAT", "patent")
    , ("PCOMM", "personal_communication")
    , ("RPRT", "report")
    , ("SER", "article")
    , ("SLIDE", "graphic")
    , ("SOUND", "musical_score")
    , ("STAND", "report")
    , ("STAT", "legislation")
    , ("THES", "thesis")
    , ("UNBILL", "bill")
    , ("UNPB", "unpublished")
    , ("VIDEO", "graphic") ]
