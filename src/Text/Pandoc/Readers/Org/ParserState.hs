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
  ( OrgParserState(..)
  , OrgParserLocal(..)
  , OrgNoteRecord
  , F(..)
  , askF
  , asksF
  , trimInlinesF
  , runF
  , returnF
  ) where

import           Control.Monad (liftM, liftM2)
import           Control.Monad.Reader (Reader, runReader, ask, asks, local)

import           Data.Default (Default(..))
import qualified Data.Map as M
import qualified Data.Set as Set

import           Text.Pandoc.Builder ( Inlines, Blocks, HasMeta(..),
                                       trimInlines )
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
-- | Map of org block attributes (e.g. LABEL, CAPTION, NAME, etc)
type OrgBlockAttributes = M.Map String String
-- | Map of functions for link transformations.  The map key is refers to the
-- link-type, the corresponding function transforms the given link string.
type OrgLinkFormatters = M.Map String (String -> String)

-- | Org-mode parser state
data OrgParserState = OrgParserState
  { orgStateOptions              :: ReaderOptions
  , orgStateAnchorIds            :: [String]
  , orgStateBlockAttributes      :: OrgBlockAttributes
  , orgStateEmphasisCharStack    :: [Char]
  , orgStateEmphasisNewlines     :: Maybe Int
  , orgStateLastForbiddenCharPos :: Maybe SourcePos
  , orgStateLastPreCharPos       :: Maybe SourcePos
  , orgStateLastStrPos           :: Maybe SourcePos
  , orgStateLinkFormatters       :: OrgLinkFormatters
  , orgStateMeta                 :: Meta
  , orgStateMeta'                :: F Meta
  , orgStateNotes'               :: OrgNoteTable
  , orgStateParserContext        :: ParserContext
  , orgStateIdentifiers          :: Set.Set String
  , orgStateHeaderMap            :: M.Map Inlines String
  }

data OrgParserLocal = OrgParserLocal { orgLocalQuoteContext :: QuoteContext }

instance Default OrgParserLocal where
  def = OrgParserLocal NoQuote

instance HasReaderOptions OrgParserState where
  extractReaderOptions = orgStateOptions

instance HasMeta OrgParserState where
  setMeta field val st =
    st{ orgStateMeta = setMeta field val $ orgStateMeta st }
  deleteMeta field st =
    st{ orgStateMeta = deleteMeta field $ orgStateMeta st }

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
  { orgStateOptions = def
  , orgStateAnchorIds = []
  , orgStateBlockAttributes = M.empty
  , orgStateEmphasisCharStack = []
  , orgStateEmphasisNewlines = Nothing
  , orgStateLastForbiddenCharPos = Nothing
  , orgStateLastPreCharPos = Nothing
  , orgStateLastStrPos = Nothing
  , orgStateLinkFormatters = M.empty
  , orgStateMeta = nullMeta
  , orgStateMeta' = return nullMeta
  , orgStateNotes' = []
  , orgStateParserContext = NullState
  , orgStateIdentifiers = Set.empty
  , orgStateHeaderMap = M.empty
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
