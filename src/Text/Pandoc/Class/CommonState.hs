{-# LANGUAGE CPP #-}
{- |
Module      : Text.Pandoc.Class.CommonState
Copyright   : Copyright (C) 2016-2020 Jesse Rosenthal, John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
Stability   : alpha
Portability : portable

Common state shared by all pandoc-specific operations, including
those in readers, writers, and Lua filters.
-}

module Text.Pandoc.Class.CommonState
  ( CommonState(..)
  , defaultCommonState
  )
where

import Data.Default (Default (def))
import Data.Text (Text)
import Text.Collate.Lang (Lang)
import Text.Pandoc.MediaBag (MediaBag)
import Text.Pandoc.Logging (LogMessage, Verbosity (WARNING))
import Text.Pandoc.Translations.Types (Translations)
#ifdef PANDOC_HTTP_SUPPORT
import Network.HTTP.Client (Manager)
#endif

-- | 'CommonState' represents state that is used by all
-- instances of 'PandocMonad'.  Normally users should not
-- need to interact with it directly; instead, auxiliary
-- functions like 'setVerbosity' and 'withMediaBag' should be used.
data CommonState = CommonState
  { stLog          :: [LogMessage]
    -- ^ A list of log messages in reverse order
  , stUserDataDir  :: Maybe FilePath
    -- ^ Directory to search for data files
  , stSourceURL    :: Maybe Text
    -- ^ Absolute URL + dir of 1st source file
  , stRequestHeaders :: [(Text, Text)]
    -- ^ Headers to add for HTTP requests
  , stNoCheckCertificate :: Bool
    -- ^ Controls whether certificate validation is disabled
  , stMediaBag     :: MediaBag
    -- ^ Media parsed from binary containers
  , stTranslations :: Maybe (Lang, Maybe Translations)
    -- ^ Translations for localization
  , stInputFiles   :: [FilePath]
    -- ^ List of input files from command line
  , stOutputFile   :: Maybe FilePath
    -- ^ Output file from command line
  , stResourcePath :: [FilePath]
    -- ^ Path to search for resources like
    -- included images
#ifdef PANDOC_HTTP_SUPPORT
  , stManager      :: Maybe Manager
    -- ^ Manager for HTTP client; this needs to persist across many requests
    -- for efficiency.
#endif
  , stVerbosity    :: Verbosity
    -- ^ Verbosity level
  , stTrace        :: Bool
    -- ^ Controls whether tracing messages are
    -- issued.
  }

-- | The default @'CommonState'@. All fields are initialized as the
-- monoid identity of their resprective type, except for:
--
--   * @'stResourcePath'@, which is set to @["."]@,
--   * @'stTrace'@, which is set to @'False'@, and
--   * @'stVerbosity'@, which is set to @WARNING@.
defaultCommonState :: CommonState
defaultCommonState = CommonState
  { stLog = []
  , stUserDataDir = Nothing
  , stSourceURL = Nothing
  , stRequestHeaders = []
  , stNoCheckCertificate = False
  , stMediaBag = mempty
  , stTranslations = Nothing
  , stInputFiles = []
  , stOutputFile = Nothing
  , stResourcePath = ["."]
#ifdef PANDOC_HTTP_SUPPORT
  , stManager = Nothing
#endif
  , stVerbosity = WARNING
  , stTrace = False
  }

instance Default CommonState where
  def = defaultCommonState
