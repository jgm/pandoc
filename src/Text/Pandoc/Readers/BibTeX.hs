{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.BibTeX
   Copyright   : Copyright (C) 2020-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Parses BibTeX or BibLaTeX bibliographies into a Pandoc document
with empty body and `references` and `nocite` fields
in the metadata.  A wildcard `nocite` is used so that
if the document is rendered in another format, the
entire bibliography will be printed.
-}
module Text.Pandoc.Readers.BibTeX
  ( readBibTeX
  , readBibLaTeX
  )
where

import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Builder (setMeta, cite, str)
import Data.Text (Text)
import Citeproc (Lang(..), parseLang)
import Citeproc.Locale (getLocale)
import Text.Pandoc.Error (PandocError(..))
import Text.Pandoc.Class (PandocMonad, lookupEnv)
import Text.Pandoc.Citeproc.BibTeX as BibTeX
import Text.Pandoc.Citeproc.MetaValue (referenceToMetaValue)
import Control.Monad.Except (throwError)

-- | Read BibTeX from an input string and return a Pandoc document.
-- The document will have only metadata, with an empty body.
-- The metadata will contain a `references` field with the
-- bibliography entries, and a `nocite` field with the wildcard `[@*]`.
readBibTeX :: PandocMonad m => ReaderOptions -> Text -> m Pandoc
readBibTeX = readBibTeX' BibTeX.Bibtex

-- | Read BibLaTeX from an input string and return a Pandoc document.
-- The document will have only metadata, with an empty body.
-- The metadata will contain a `references` field with the
-- bibliography entries, and a `nocite` field with the wildcard `[@*]`.
readBibLaTeX :: PandocMonad m => ReaderOptions -> Text -> m Pandoc
readBibLaTeX = readBibTeX' BibTeX.Biblatex

readBibTeX' :: PandocMonad m => Variant -> ReaderOptions -> Text -> m Pandoc
readBibTeX' variant _opts t = do
  lang <- maybe (Lang "en" (Just "US")) parseLang
             <$> lookupEnv "LANG"
  locale <- case getLocale lang of
               Left e  ->
                 case getLocale (Lang "en" (Just "US")) of
                   Right l -> return l
                   Left _  -> throwError $ PandocCiteprocError e
               Right l -> return l
  case BibTeX.readBibtexString variant locale (const True) t of
    Left e -> throwError $ PandocParsecError t e
    Right refs -> return $ setMeta "references"
                              (map referenceToMetaValue refs)
                         . setMeta "nocite"
                            (cite [Citation {citationId = "*"
                                            , citationPrefix = []
                                            , citationSuffix = []
                                            , citationMode = NormalCitation
                                            , citationNoteNum = 0
                                            , citationHash = 0}]
                                            (str "[@*]"))
                        $ Pandoc nullMeta []
