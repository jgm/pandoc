{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.JATS.References
   Copyright   : © 2021-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Creation of a bibliography list using @<element-citation>@ elements in
reference items.
-}
module Text.Pandoc.Writers.JATS.References
  ( referencesToJATS
  , referenceToJATS
  ) where

import Citeproc.Pandoc ()
import Citeproc.Types
  ( Date (..), DateParts (..), ItemId (..), Name (..), Reference (..)
  , Val (..) , lookupVariable, valToText
  )
import Data.Text (Text)
import Text.DocLayout (Doc, empty, isEmpty, literal, vcat)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Builder (Inlines)
import Text.Pandoc.Options (WriterOptions)
import Text.Pandoc.Shared (tshow)
import Text.Pandoc.Writers.JATS.Types
import Text.Pandoc.XML (escapeNCName, escapeStringForXML, inTags)
import qualified Data.Text as T

referencesToJATS :: PandocMonad m
                 => WriterOptions
                 -> [Reference Inlines]
                 -> JATS m (Doc Text)
referencesToJATS opts =
  fmap (inTags True "ref-list" [] . vcat) . mapM (referenceToJATS opts)

referenceToJATS :: PandocMonad m
                => WriterOptions
                -> Reference Inlines
                -> JATS m (Doc Text)
referenceToJATS _opts ref = do
  let refType = referenceType ref
  let pubType = [("publication-type", refType) | not (T.null refType)]
  let ident = escapeNCName $ "ref-" <> unItemId (referenceId ref)
  let wrap = inTags True "ref" [("id", ident)]
           . inTags True "element-citation" pubType
  return . wrap . vcat $
    [ authors
    , "title" `varInTag`
      if refType == "book"
      then "source"
      else "article-title"
    , if refType == "book"
      then empty
      else "container-title" `varInTag` "source"
    , editors
    , "publisher"       `varInTag` "publisher-name"
    , "publisher-place" `varInTag` "publisher-loc"
    , yearTag
    , accessed
    , "volume"          `varInTag` "volume"
    , "issue"           `varInTag` "issue"
    , "page-first"      `varInTag` "fpage"
    , "page-last"       `varInTag` "lpage"
    , "pages"           `varInTag` "page-range"
    , "ISBN"            `varInTag` "isbn"
    , "ISSN"            `varInTag` "issn"
    , "URL"             `varInTag` "uri"
    , varInTagWith "doi"  "pub-id" [("pub-id-type", "doi")]
    , varInTagWith "pmid" "pub-id" [("pub-id-type", "pmid")]
    ]
  where
    varInTag var tagName = varInTagWith var tagName []

    varInTagWith var tagName tagAttribs =
      case lookupVariable var ref >>= valToText of
        Nothing  -> mempty
        Just val -> inTags' tagName tagAttribs . literal $
                    escapeStringForXML val

    authors = case lookupVariable "author" ref of
      Just (NamesVal names) ->
        inTags True "person-group" [("person-group-type", "author")] . vcat $
        map toNameElements names
      _                     -> empty

    editors = case lookupVariable "editor" ref of
      Just (NamesVal names) ->
        inTags True "person-group" [("person-group-type", "editor")] . vcat $
        map toNameElements names
      _                     -> empty

    yearTag =
      case lookupVariable "issued" ref of
        Just (DateVal date) -> toDateElements date
        _ -> empty

    accessed =
      case lookupVariable "accessed" ref of
        Just (DateVal d) -> inTags' "date-in-citation"
                                    [("content-type", "access-date")]
                                    (toDateElements d)
        _ -> empty

toDateElements :: Date -> Doc Text
toDateElements date =
  case dateParts date of
    dp@(DateParts (y:m:d:_)):_ -> yearElement y dp <>
                                  monthElement m <>
                                  dayElement d
    dp@(DateParts (y:m:_)):_   -> yearElement y dp <> monthElement m
    dp@(DateParts (y:_)):_     -> yearElement y dp
    _                          -> empty

yearElement :: Int -> DateParts -> Doc Text
yearElement year dp =
  inTags' "year" [("iso-8601-date", iso8601 dp)] $ literal (fourDigits year)

monthElement :: Int -> Doc Text
monthElement month = inTags' "month" [] . literal $ twoDigits month

dayElement :: Int -> Doc Text
dayElement day = inTags' "day" [] . literal $ twoDigits day

iso8601 :: DateParts -> Text
iso8601 = T.intercalate "-" . \case
  DateParts (y:m:d:_) -> [fourDigits y, twoDigits m, twoDigits d]
  DateParts (y:m:_)   -> [fourDigits y, twoDigits m]
  DateParts (y:_)     -> [fourDigits y]
  _                   -> []

twoDigits :: Int -> Text
twoDigits n = T.takeEnd 2 $ '0' `T.cons` tshow n

fourDigits :: Int -> Text
fourDigits n = T.takeEnd 4 $ "000" <> tshow n

toNameElements :: Name -> Doc Text
toNameElements name =
  if not (isEmpty nameTags)
  then inTags' "name" [] nameTags
  else nameLiteral name `inNameTag` "string-name"
    where
      inNameTag mVal tag = case mVal of
        Nothing  -> empty
        Just val -> inTags' tag [] . literal $ escapeStringForXML val
      surnamePrefix = maybe mempty (`T.snoc` ' ') $
                      nameNonDroppingParticle name
      givenSuffix = maybe mempty (T.cons ' ') $
                    nameDroppingParticle name
      nameTags = mconcat
        [ ((surnamePrefix <>) <$> nameFamily name) `inNameTag` "surname"
        , ((<> givenSuffix) <$> nameGiven name) `inNameTag` "given-names"
        , nameSuffix name `inNameTag` "suffix"
        ]

-- | Put the supplied contents between start and end tags of tagType,
--   with specified attributes.
inTags' :: Text -> [(Text, Text)] -> Doc Text -> Doc Text
inTags' = inTags False
