{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.EndNote
   Copyright   : Copyright (C) 2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Parses EndNote XML bibliographies into a Pandoc document
with empty body and `references` and `nocite` fields
in the metadata.  A wildcard `nocite` is used so that
if the document is rendered in another format, the
entire bibliography will be printed.
-}
module Text.Pandoc.Readers.EndNote
  ( readEndNoteXML
  )
where

import Text.Pandoc.Options
import Text.Pandoc.Definition
import Citeproc (Reference(..), ItemId(..), Val(..), Date(..), DateParts(..))
import Text.Pandoc.Builder as B
import Text.Pandoc.Error (PandocError(..))
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Citeproc.MetaValue (referenceToMetaValue)
import Text.Pandoc.Sources (ToSources(..), sourcesToText)
import Text.Pandoc.Citeproc.BibTeX (toName)
import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad (mzero)
import Text.Pandoc.XML.Light
    ( filterElementName,
      strContent,
      QName(qName),
      Element(..),
      Content(..),
      CData(..),
      filterElementsName,
      filterChildrenName,
      findAttrBy,
      parseXMLElement )
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import Safe (readMay)

-- | Read EndNote XML from an input string and return a Pandoc document.
-- The document will have only metadata, with an empty body.
-- The metadata will contain a `references` field with the
-- bibliography entries, and a `nocite` field with the wildcard `[@*]`.
readEndNoteXML :: (PandocMonad m, ToSources a)
               => ReaderOptions -> a -> m Pandoc
readEndNoteXML _opts inp = do
  let sources = toSources inp
  tree <- either (throwError . PandocXMLError "") return $
              parseXMLElement (TL.fromStrict . sourcesToText $ sources)
  let records = filterElementsName ((== "record") . qName) tree
  let refs = map (referenceToMetaValue . recordToReference) records
  return $ setMeta "references" refs $ B.doc mempty

recordToReference :: Element -> Reference Inlines
recordToReference e =
  Reference{ referenceId = ItemId refid,
             referenceType = reftype,
             referenceDisambiguation = Nothing,
             referenceVariables = refvars }

 where
   -- get strContent, recursing inside style elements:
   getText el = getText' (Elem el)
   getText' (Elem el) =  mconcat $ map getText' $ elContent el
   getText' (Text cd) = cdData cd
   getText' (CRef _) = mempty
     -- mconcat . map cdData . onlyText . elContent
   name t = (== t) . qName
   refid = maybe mempty (T.strip . strContent)
           (filterElementName (name "key") e
            <|> filterElementName (name "rec-number") e)
   reftype = maybe "document" toCslReferenceType
              (filterElementName (name "ref-type") e >>=
                findAttrBy (name "name"))
   authors =
     filterChildrenName (name "contributors") e >>=
     filterChildrenName (name "authors") >>=
     filterChildrenName (name "author") >>=
     toName [] . B.toList . B.text . T.strip . getText
   titles = do
     x <- filterChildrenName (name "titles") e
     (key, name') <- [("title", "title"),
                      ("container-title", "secondary-title")]
     (key,) . FancyVal . B.text . T.strip . getText <$>
                    filterChildrenName (name name') x
   pages = ("pages",) . FancyVal . B.text. T.strip . getText <$>
                filterChildrenName (name "pages") e
   volume = ("volume",) . FancyVal . B.text. T.strip . getText <$>
                filterChildrenName (name "volume") e
   number = ("number",) . FancyVal . B.text. T.strip . getText <$>
                filterChildrenName (name "number") e
   isbn = ("isbn",) . FancyVal . B.text. T.strip . getText <$>
                filterChildrenName (name "isbn") e
   publisher = ("publisher",) . FancyVal . B.text. T.strip . getText <$>
                filterChildrenName (name "publisher") e
   originalPublisher =
     ("original-publisher",) . FancyVal . B.text. T.strip . getText <$>
                filterChildrenName (name "orig-pub") e
   publisherPlace =
     ("publisher-place",) . FancyVal . B.text. T.strip . getText <$>
                filterChildrenName (name "pub-location") e
   abstract = ("abstract",) . FancyVal . B.text. T.strip . getText <$>
                filterChildrenName (name "abstract") e
   dates = ("issued",) . toDate <$> filterChildrenName (name "dates") e
   toDate e' = DateVal $
    Date { dateParts = toDateParts e'
         , dateCirca = False
         , dateSeason = Nothing
         , dateLiteral = Nothing }
   toDateParts e' = do
    x <- filterChildrenName (name "year") e'
    case readMay . T.unpack . T.strip . getText $ x of
      Nothing -> mzero
      Just y  -> return $ DateParts [y]

   refvars = M.fromList $
     [ ("author", NamesVal authors) | not (null authors) ] ++
     titles ++
     pages ++
     volume ++
     number ++
     isbn ++
     dates ++
     publisher ++
     originalPublisher ++
     publisherPlace ++
     abstract

toCslReferenceType :: Text -> Text
toCslReferenceType t =
  case t of
    "Aggregated Database" -> "dataset"
    "Ancient Text" -> "classic"
    "Artwork" -> "graphic"
    "Audiovisual Material" -> "graphic"
    "Bill" -> "legislation"
    "Blog" -> "post-weblog"
    "Book" -> "book"
    "Book Section" -> "chapter"
    "Case" -> "legal_case"
    "Catalog" -> "document"
    "Chart or Table" -> "graphic"
    "Classical Work" -> "classic"
    "Computer program" -> "software"
    "Conference Paper" -> "article"
    "Conference Proceedings" -> "periodical"
    "Dataset" -> "dataset"
    "Dictionary" -> "book"
    "Edited Book" -> "book"
    "Electronic Article" -> "article"
    "Electronic Book" -> "book"
    "Electronic Book Section" -> "chapter"
    "Encyclopedia" -> "book"
    "Equation" -> "document"
    "Figure" -> "graphic"
    "Film or Broadcast" -> "motion_picture"
    "Government Document" -> "document"
    "Grant" -> "document"
    "Hearing" -> "hearing"
    "Interview" -> "interview"
    "Journal Article" -> "article-journal"
    "Legal Rule or Regulation" -> "regulation"
    "Magazine Article" -> "article-magazine"
    "Manuscript" -> "manuscript"
    "Map" -> "map"
    "Music" -> "musical_score"
    "Newspaper Article" -> "article-newspaper"
    "Online Database" -> "dataset"
    "Online Multimedia" -> "webpage"
    "Pamphlet" -> "pamphlet"
    "Patent" -> "patent"
    "Personal Communication" -> "personal_communication"
    "Podcast" -> "document"
    "Press Release" -> "report"
    "Report" -> "report"
    "Serial" -> "periodical"
    "Standard" -> "standard"
    "Statute" -> "legislation"
    "Thesis" -> "thesis"
    "Unpublished Work" -> "unpublished"
    "Web Page" -> "webpage"
    _ -> "document"
