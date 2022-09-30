{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{- |
   Module      : Text.Pandoc.Parsing
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
   License     : GPL-2.0-or-later
   Maintainer  : John MacFarlane <jgm@berkeley.edu>

A default parser state with commonly used properties.
-}

module Text.Pandoc.Parsing.State
  ( ParserState (..)
  , ParserContext (..)
  , HeaderType (..)
  , NoteTable
  , NoteTable'
  , Key (..)
  , KeyTable
  , SubstTable
  , defaultParserState
  , toKey
  )
where

import Data.Default (Default (def))
import Data.Text (Text)
import Text.Parsec (SourcePos, getState, setState)
import Text.Pandoc.Builder (Blocks, HasMeta (..), Inlines)
import Text.Pandoc.Definition (Attr, Meta, Target, nullMeta)
import Text.Pandoc.Logging (LogMessage)
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Parsing.Capabilities
import Text.Pandoc.Parsing.Types
import Text.Pandoc.Readers.LaTeX.Types (Macro)

import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Parsing options.
data ParserState = ParserState
  { stateOptions         :: ReaderOptions -- ^ User options
  , stateParserContext   :: ParserContext -- ^ Inside list?
  , stateQuoteContext    :: QuoteContext  -- ^ Inside quoted environment?
  , stateAllowLinks      :: Bool          -- ^ Allow parsing of links
  , stateAllowLineBreaks :: Bool          -- ^ Allow parsing of line breaks
  , stateMaxNestingLevel :: Int           -- ^ Max # of nested Strong/Emph
  , stateLastStrPos      :: Maybe SourcePos -- ^ Position after last str parsed
  , stateKeys            :: KeyTable      -- ^ List of reference keys
  , stateHeaderKeys      :: KeyTable      -- ^ List of implicit header ref keys
  , stateSubstitutions   :: SubstTable    -- ^ List of substitution references
  , stateNotes           :: NoteTable     -- ^ List of notes (raw bodies)
  , stateNotes'          :: NoteTable'    -- ^ List of notes (parsed bodies)
  , stateNoteRefs        :: Set.Set Text  -- ^ List of note references used
  , stateInNote          :: Bool          -- ^ True if parsing note contents
  , stateNoteNumber      :: Int           -- ^ Last note number for citations
  , stateMeta            :: Meta          -- ^ Document metadata
  , stateMeta'           :: Future ParserState Meta -- ^ Document metadata
  , stateCitations       :: M.Map Text Text -- ^ RST-style citations
  , stateHeaderTable     :: [HeaderType]  -- ^ Ordered list of header types used
  , stateIdentifiers     :: Set.Set Text  -- ^ Header identifiers used
  , stateNextExample     :: Int           -- ^ Number of next example
  , stateExamples        :: M.Map Text Int -- ^ Map from example labels to numbers
  , stateMacros          :: M.Map Text Macro -- ^ Table of macros defined so far
  , stateRstDefaultRole  :: Text          -- ^ Current rST default
                                           -- interpreted text role
  , stateRstHighlight    :: Maybe Text    -- ^ Current rST literal block
                                           -- language
  , stateRstCustomRoles  :: M.Map Text (Text, Maybe Text, Attr)
    -- ^ Current rST cust text roles;
    -- Triple represents:) Base role 2) Optional format (only for :raw:
    -- roles) 3) Addition classes (rest of Attr is unused)).
  , stateCaption         :: Maybe Inlines -- ^ Caption in current environment
  , stateInHtmlBlock     :: Maybe Text    -- ^ Tag type of HTML block being parsed
  , stateFencedDivLevel  :: Int           -- ^ Depth of fenced div
  , stateContainers      :: [Text]        -- ^ parent include files
  , stateLogMessages     :: [LogMessage]  -- ^ log messages
  , stateMarkdownAttribute :: Bool        -- ^ True if in markdown=1 context
  }

instance Default ParserState where
  def = defaultParserState

instance HasMeta ParserState where
  setMeta field val st =
    st{ stateMeta = setMeta field val $ stateMeta st }
  deleteMeta field st =
    st{ stateMeta = deleteMeta field $ stateMeta st }

instance HasReaderOptions ParserState where
  extractReaderOptions = stateOptions

instance Monad m => HasQuoteContext ParserState m where
  getQuoteContext = stateQuoteContext <$> getState
  withQuoteContext context parser = do
    oldState <- getState
    let oldQuoteContext = stateQuoteContext oldState
    setState oldState { stateQuoteContext = context }
    result <- parser
    newState <- getState
    setState newState { stateQuoteContext = oldQuoteContext }
    return result

instance HasIdentifierList ParserState where
  extractIdentifierList     = stateIdentifiers
  updateIdentifierList f st = st{ stateIdentifiers = f $ stateIdentifiers st }

instance HasMacros ParserState where
  extractMacros        = stateMacros
  updateMacros f st    = st{ stateMacros = f $ stateMacros st }

instance HasLastStrPosition ParserState where
  setLastStrPos pos st = st{ stateLastStrPos = pos }
  getLastStrPos st     = stateLastStrPos st

instance HasLogMessages ParserState where
  addLogMessage msg st = st{ stateLogMessages = msg : stateLogMessages st }
  getLogMessages st = reverse $ stateLogMessages st

instance HasIncludeFiles ParserState where
  getIncludeFiles = stateContainers
  addIncludeFile f s = s{ stateContainers = f : stateContainers s }
  dropLatestIncludeFile s = s { stateContainers = drop 1 $ stateContainers s }

data ParserContext
    = ListItemState   -- ^ Used when running parser on list item contents
    | NullState       -- ^ Default state
    deriving (Eq, Show)

data HeaderType
    = SingleHeader Char  -- ^ Single line of characters underneath
    | DoubleHeader Char  -- ^ Lines of characters above and below
    deriving (Eq, Show)

defaultParserState :: ParserState
defaultParserState = ParserState
  { stateOptions         = def
  , stateParserContext   = NullState
  , stateQuoteContext    = NoQuote
  , stateAllowLinks      = True
  , stateAllowLineBreaks = True
  , stateMaxNestingLevel = 6
  , stateLastStrPos      = Nothing
  , stateKeys            = M.empty
  , stateHeaderKeys      = M.empty
  , stateSubstitutions   = M.empty
  , stateNotes           = []
  , stateNotes'          = M.empty
  , stateNoteRefs        = Set.empty
  , stateInNote          = False
  , stateNoteNumber      = 0
  , stateMeta            = nullMeta
  , stateMeta'           = return nullMeta
  , stateCitations       = M.empty
  , stateHeaderTable     = []
  , stateIdentifiers     = Set.empty
  , stateNextExample     = 1
  , stateExamples        = M.empty
  , stateMacros          = M.empty
  , stateRstDefaultRole  = "title-reference"
  , stateRstHighlight    = Nothing
  , stateRstCustomRoles  = M.empty
  , stateCaption         = Nothing
  , stateInHtmlBlock     = Nothing
  , stateFencedDivLevel  = 0
  , stateContainers      = []
  , stateLogMessages     = []
  , stateMarkdownAttribute = False
  }

type NoteTable = [(Text, Text)]

type NoteTable' = M.Map Text (SourcePos, Future ParserState Blocks)
-- used in markdown reader

newtype Key = Key Text deriving (Show, Read, Eq, Ord)

toKey :: Text -> Key
toKey = Key . T.toLower . T.unwords . T.words . unbracket
  where unbracket t
          | Just ('[', t') <- T.uncons t
          , Just (t'', ']') <- T.unsnoc t'
          = t''
          | otherwise
          = t

type KeyTable = M.Map Key (Target, Attr)

type SubstTable = M.Map Key Inlines
