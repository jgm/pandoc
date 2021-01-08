{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
   Module      : Text.Pandoc.Readers.HTML.Types
   Copyright   : Copyright (C) 2006-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Types for pandoc's HTML reader.
-}
module Text.Pandoc.Readers.HTML.Types
  ( TagParser
  , HTMLParser
  , HTMLState (..)
  , HTMLLocal (..)
  )
where

import Control.Monad.Reader (ReaderT, asks, local)
import Data.Default (Default (def))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Network.URI (URI)
import Text.HTML.TagSoup
import Text.Pandoc.Builder (Blocks, HasMeta (..))
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Logging (LogMessage)
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Parsing
  ( HasIdentifierList (..), HasLastStrPosition (..), HasLogMessages (..)
  , HasMacros (..), HasQuoteContext (..), HasReaderOptions (..)
  , ParserT, ParserState, QuoteContext (NoQuote)
  )
import Text.Pandoc.Readers.LaTeX.Types (Macro)

-- | HTML parser type
type HTMLParser m s = ParserT s HTMLState (ReaderT HTMLLocal m)

-- | HTML parser, expecting @Tag Text@ as tokens.
type TagParser m = HTMLParser m [Tag Text]

-- | Global HTML parser state
data HTMLState = HTMLState
  { parserState :: ParserState
  , noteTable   :: [(Text, Blocks)]
  , baseHref    :: Maybe URI
  , identifiers :: Set Text
  , logMessages :: [LogMessage]
  , macros      :: Map Text Macro
  , readerOpts  :: ReaderOptions
  }

-- | Local HTML parser state
data HTMLLocal = HTMLLocal
  { quoteContext :: QuoteContext
  , inChapter    :: Bool -- ^ Set if in chapter section
  , inPlain      :: Bool -- ^ Set if in pPlain
  }


-- Instances

instance HasMacros HTMLState where
  extractMacros        = macros
  updateMacros f st    = st{ macros = f $ macros st }

instance HasIdentifierList HTMLState where
  extractIdentifierList = identifiers
  updateIdentifierList f s = s{ identifiers = f (identifiers s) }

instance HasLogMessages HTMLState where
  addLogMessage m s = s{ logMessages = m : logMessages s }
  getLogMessages = reverse . logMessages

-- This signature should be more general
-- MonadReader HTMLLocal m => HasQuoteContext st m
instance PandocMonad m => HasQuoteContext HTMLState (ReaderT HTMLLocal m) where
  getQuoteContext = asks quoteContext
  withQuoteContext q = local (\s -> s{quoteContext = q})

instance HasReaderOptions HTMLState where
    extractReaderOptions = extractReaderOptions . parserState

instance HasMeta HTMLState where
  setMeta s b st = st {parserState = setMeta s b $ parserState st}
  deleteMeta s st = st {parserState = deleteMeta s $ parserState st}

instance Default HTMLLocal where
  def = HTMLLocal NoQuote False False

instance HasLastStrPosition HTMLState where
  setLastStrPos s st = st {parserState = setLastStrPos s (parserState st)}
  getLastStrPos = getLastStrPos . parserState
