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
import Citeproc (Reference(..), ItemId(..), Val(..), Date(..), DateParts(..))
import qualified Citeproc
import Text.Pandoc.Builder as B
import Text.Pandoc.Error (PandocError(..))
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Citeproc.MetaValue (referenceToMetaValue)
import Text.Pandoc.Sources (Sources(..), ToSources(..), sourcesToText)
import Text.Pandoc.Citeproc.BibTeX (toName)
import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad (mzero, unless)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import Safe (readMay)

-- | Read RIS from an input string and return a Pandoc document.
-- The document will have only metadata, with an empty body.
-- The metadata will contain a `references` field with the
-- bibliography entries, and a `nocite` field with the wildcard `[@*]`.
readRIS :: (PandocMonad m, ToSources a)
        => ReaderOptions -> a -> m Pandoc
readRIS _opts inp = undefined

