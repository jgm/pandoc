{-# LANGUAGE MultiParamTypeClasses      #-}
{- |
   Module      : Text.Pandoc.Parsing
   Copyright   : © 2006-2022 John MacFarlane
   License     : GPL-2.0-or-later
   Maintainer  : John MacFarlane <jgm@berkeley.edu>

Parser state capabilities.
-}
module Text.Pandoc.Parsing.Capabilities
  ( -- * Capabilities

    -- ** Element identifiers
    HasIdentifierList (..)

    -- ** Include files
  , HasIncludeFiles (..)

    -- ** String/Word boundaries
  , HasLastStrPosition (..)
  , updateLastStrPos
  , notAfterString

    -- ** Logging
  , HasLogMessages (..)
  , logMessage
  , reportLogMessages

    -- ** Macros
  , HasMacros (..)

    -- ** Quote context
  , QuoteContext (..)
  , HasQuoteContext (..)
  , failIfInQuoteContext

    -- ** Reader options
  , HasReaderOptions (..)
  , guardEnabled
  , guardDisabled
  )
where

import Control.Monad (guard, when)
import Data.Text (Text)
import Text.Parsec (ParsecT, SourcePos, Stream, getPosition, getState, updateState)
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Logging (LogMessage)
import Text.Pandoc.Options
  ( Extension
  , ReaderOptions(readerExtensions)
  , extensionEnabled
  )
import Text.Pandoc.Parsing.Types
import Text.Pandoc.Readers.LaTeX.Types (Macro)

import qualified Data.Map as M
import qualified Data.Set as Set

class HasReaderOptions st where
  extractReaderOptions :: st -> ReaderOptions
  getOption            :: (Stream s m t) => (ReaderOptions -> b) -> ParserT s st m b
  -- default
  getOption  f         = f . extractReaderOptions <$> getState

class HasQuoteContext st m where
  getQuoteContext :: (Stream s m t) => ParsecT s st m QuoteContext
  withQuoteContext :: QuoteContext -> ParsecT s st m a -> ParsecT s st m a

failIfInQuoteContext :: (HasQuoteContext st m, Stream s m t)
                     => QuoteContext
                     -> ParserT s st m ()
failIfInQuoteContext context = do
  context' <- getQuoteContext
  when (context' == context) $ Prelude.fail "already inside quotes"

class HasIdentifierList st where
  extractIdentifierList  :: st -> Set.Set Text
  updateIdentifierList   :: (Set.Set Text -> Set.Set Text) -> st -> st

class HasMacros st where
  extractMacros          :: st -> M.Map Text Macro
  updateMacros           :: (M.Map Text Macro -> M.Map Text Macro) -> st -> st

class HasLastStrPosition st where
  setLastStrPos  :: Maybe SourcePos -> st -> st
  getLastStrPos  :: st -> Maybe SourcePos

class HasLogMessages st where
  addLogMessage :: LogMessage -> st -> st
  getLogMessages :: st -> [LogMessage]

class HasIncludeFiles st where
  getIncludeFiles :: st -> [Text]
  addIncludeFile :: Text -> st -> st
  dropLatestIncludeFile :: st -> st

-- | Add a log message.
logMessage :: (Stream s m a, HasLogMessages st)
           => LogMessage -> ParserT s st m ()
logMessage msg = updateState (addLogMessage msg)

-- | Report all the accumulated log messages, according to verbosity level.
reportLogMessages :: (PandocMonad m, HasLogMessages st) => ParserT s st m ()
reportLogMessages = do
  msgs <- getLogMessages <$> getState
  mapM_ report msgs

-- | Succeed only if the extension is enabled.
guardEnabled :: (Stream s m a,  HasReaderOptions st)
             => Extension -> ParserT s st m ()
guardEnabled ext =
  getOption readerExtensions >>= guard . extensionEnabled ext

-- | Succeed only if the extension is disabled.
guardDisabled :: (Stream s m a, HasReaderOptions st)
              => Extension -> ParserT s st m ()
guardDisabled ext =
  getOption readerExtensions >>= guard . not . extensionEnabled ext

-- | Update the position on which the last string ended.
updateLastStrPos :: (Stream s m a, HasLastStrPosition st)
                 => ParserT s st m ()
updateLastStrPos = getPosition >>= updateState . setLastStrPos . Just

-- | Whether we are right after the end of a string.
notAfterString :: (Stream s m a, HasLastStrPosition st) => ParserT s st m Bool
notAfterString = do
  pos <- getPosition
  st  <- getState
  return $ getLastStrPos st /= Just pos

data QuoteContext
    = InSingleQuote   -- ^ Used when parsing inside single quotes
    | InDoubleQuote   -- ^ Used when parsing inside double quotes
    | NoQuote         -- ^ Used when not parsing inside quotes
    deriving (Eq, Show)
