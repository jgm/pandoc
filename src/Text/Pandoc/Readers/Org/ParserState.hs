{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-
Copyright (C) 2014-2016 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Readers.Org.Options
   Copyright   : Copyright (C) 2014-2016 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Define the Org-mode parser state.
-}
module Text.Pandoc.Readers.Org.ParserState
  ( OrgParserState (..)
  , OrgParserLocal (..)
  , OrgNoteRecord
  , HasReaderOptions (..)
  , HasQuoteContext (..)
  , F(..)
  , askF
  , asksF
  , trimInlinesF
  , runF
  , returnF
  , ExportSettings (..)
  , ArchivedTreesOption (..)
  , optionsToParserState
  ) where

import           Control.Monad (liftM, liftM2)
import           Control.Monad.Reader (Reader, runReader, ask, asks, local)

import           Data.Default (Default(..))
import qualified Data.Map as M
import qualified Data.Set as Set

import           Text.Pandoc.Builder ( Inlines, Blocks, trimInlines )
import           Text.Pandoc.Definition ( Meta(..), nullMeta )
import           Text.Pandoc.Options ( ReaderOptions(..) )
import           Text.Pandoc.Parsing ( HasHeaderMap(..)
                                     , HasIdentifierList(..)
                                     , HasLastStrPosition(..)
                                     , HasQuoteContext(..)
                                     , HasReaderOptions(..)
                                     , ParserContext(..)
                                     , QuoteContext(..)
                                     , SourcePos )

-- | An inline note / footnote containing the note key and its (inline) value.
type OrgNoteRecord = (String, F Blocks)
-- | Table of footnotes
type OrgNoteTable = [OrgNoteRecord]
-- | Map of functions for link transformations.  The map key is refers to the
-- link-type, the corresponding function transforms the given link string.
type OrgLinkFormatters = M.Map String (String -> String)

-- | Org-mode parser state
data OrgParserState = OrgParserState
  { orgStateAnchorIds            :: [String]
  , orgStateEmphasisCharStack    :: [Char]
  , orgStateEmphasisNewlines     :: Maybe Int
  , orgStateExportSettings       :: ExportSettings
  , orgStateHeaderMap            :: M.Map Inlines String
  , orgStateIdentifiers          :: Set.Set String
  , orgStateLastForbiddenCharPos :: Maybe SourcePos
  , orgStateLastPreCharPos       :: Maybe SourcePos
  , orgStateLastStrPos           :: Maybe SourcePos
  , orgStateLinkFormatters       :: OrgLinkFormatters
  , orgStateMeta                 :: F Meta
  , orgStateNotes'               :: OrgNoteTable
  , orgStateOptions              :: ReaderOptions
  , orgStateParserContext        :: ParserContext
  }

data OrgParserLocal = OrgParserLocal { orgLocalQuoteContext :: QuoteContext }

instance Default OrgParserLocal where
  def = OrgParserLocal NoQuote

instance HasReaderOptions OrgParserState where
  extractReaderOptions = orgStateOptions

instance HasLastStrPosition OrgParserState where
  getLastStrPos = orgStateLastStrPos
  setLastStrPos pos st = st{ orgStateLastStrPos = Just pos }

instance HasQuoteContext st (Reader OrgParserLocal) where
  getQuoteContext = asks orgLocalQuoteContext
  withQuoteContext q = local (\s -> s{orgLocalQuoteContext = q})

instance HasIdentifierList OrgParserState where
  extractIdentifierList = orgStateIdentifiers
  updateIdentifierList f s = s{ orgStateIdentifiers = f (orgStateIdentifiers s) }

instance HasHeaderMap OrgParserState where
  extractHeaderMap = orgStateHeaderMap
  updateHeaderMap  f s = s{ orgStateHeaderMap = f (orgStateHeaderMap s) }

instance Default OrgParserState where
  def = defaultOrgParserState

defaultOrgParserState :: OrgParserState
defaultOrgParserState = OrgParserState
  { orgStateAnchorIds = []
  , orgStateEmphasisCharStack = []
  , orgStateEmphasisNewlines = Nothing
  , orgStateExportSettings = def
  , orgStateHeaderMap = M.empty
  , orgStateIdentifiers = Set.empty
  , orgStateLastForbiddenCharPos = Nothing
  , orgStateLastPreCharPos = Nothing
  , orgStateLastStrPos = Nothing
  , orgStateLinkFormatters = M.empty
  , orgStateMeta = return nullMeta
  , orgStateNotes' = []
  , orgStateOptions = def
  , orgStateParserContext = NullState
  }

optionsToParserState :: ReaderOptions -> OrgParserState
optionsToParserState opts =
  def { orgStateOptions = opts }

--
-- Export Settings
--

-- | Options for the way archived trees are handled.
data ArchivedTreesOption =
    ArchivedTreesExport       -- ^ Export the complete tree
  | ArchivedTreesNoExport     -- ^ Exclude archived trees from exporting
  | ArchivedTreesHeadlineOnly -- ^ Export only the headline, discard the contents

-- | Export settings <http://orgmode.org/manual/Export-settings.html>
-- These settings can be changed via OPTIONS statements.
data ExportSettings = ExportSettings
  { exportArchivedTrees   :: ArchivedTreesOption -- ^ How to treat archived trees
  , exportDrawers         :: Either [String] [String]
  -- ^ Specify drawer names which should be exported.  @Left@ names are
  -- explicitly excluded from the resulting output while @Right@ means that
  -- only the listed drawer names should be included.
  , exportEmphasizedText  :: Bool -- ^ Parse emphasized text
  , exportHeadlineLevels  :: Int
  -- ^ Maximum depth of headlines, deeper headlines are convert to list
  , exportSmartQuotes     :: Bool -- ^ Parse quotes smartly
  , exportSpecialStrings  :: Bool -- ^ Parse ellipses and dashes smartly
  , exportSubSuperscripts :: Bool -- ^ TeX-like syntax for sub- and superscripts
  , exportWithAuthor      :: Bool -- ^ Include author in final meta-data
  , exportWithCreator     :: Bool -- ^ Include creator in final meta-data
  , exportWithEmail       :: Bool -- ^ Include email in final meta-data
  }

instance Default ExportSettings where
  def = defaultExportSettings

defaultExportSettings :: ExportSettings
defaultExportSettings = ExportSettings
  { exportArchivedTrees = ArchivedTreesHeadlineOnly
  , exportDrawers = Left ["LOGBOOK"]
  , exportEmphasizedText = True
  , exportHeadlineLevels = 3
  , exportSmartQuotes = True
  , exportSpecialStrings = True
  , exportSubSuperscripts = True
  , exportWithAuthor = True
  , exportWithCreator = True
  , exportWithEmail = True
  }


--
-- Parser state reader
--

-- | Reader monad wrapping the parser state.  This is used to delay evaluation
-- until all relevant information has been parsed and made available in the
-- parser state.  See also the newtype of the same name in
-- Text.Pandoc.Parsing.
newtype F a = F { unF :: Reader OrgParserState a
                } deriving (Functor, Applicative, Monad)

instance Monoid a => Monoid (F a) where
  mempty = return mempty
  mappend = liftM2 mappend
  mconcat = fmap mconcat . sequence

runF :: F a -> OrgParserState -> a
runF = runReader . unF

askF :: F OrgParserState
askF = F ask

asksF :: (OrgParserState -> a) -> F a
asksF f = F $ asks f

trimInlinesF :: F Inlines -> F Inlines
trimInlinesF = liftM trimInlines

returnF :: Monad m => a -> m (F a)
returnF = return . return
