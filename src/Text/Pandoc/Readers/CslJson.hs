{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.CslJson
   Copyright   : Copyright (C) 2020-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Parses CSL JSON bibliographies into a Pandoc document
with empty body and `references` and `nocite` fields
in the metadata.  A wildcard `nocite` is used so that
if the document is rendered in another format, the
entire bibliography will be printed.

<https://citeproc-js.readthedocs.io/en/latest/csl-json/markup.html>.
-}
module Text.Pandoc.Readers.CslJson
  ( readCslJson )
where

import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Builder (setMeta, cite, str)
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.Text as T
import Text.Pandoc.Error (PandocError(..))
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Citeproc.CslJson (cslJsonToReferences)
import Text.Pandoc.Citeproc.MetaValue (referenceToMetaValue)
import Control.Monad.Except (throwError)
import Text.Pandoc.Sources (ToSources(..), sourcesToText)

-- | Read CSL JSON from an input string and return a Pandoc document.
-- The document will have only metadata, with an empty body.
-- The metadata will contain a `references` field with the
-- bibliography entries, and a `nocite` field with the wildcard `[@*]`.
readCslJson :: (PandocMonad m, ToSources a)
            => ReaderOptions -> a -> m Pandoc
readCslJson _opts x =
  case cslJsonToReferences (UTF8.fromText $ sourcesToText $ toSources x) of
    Left e -> throwError $ PandocParseError $ T.pack e
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
