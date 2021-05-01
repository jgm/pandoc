{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Org
   Copyright   : Copyright (C) 2014-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Conversion of org-mode formatted plain text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Org ( readOrg ) where

import Text.Pandoc.Readers.Org.Blocks (blockList, meta)
import Text.Pandoc.Readers.Org.ParserState (optionsToParserState)
import Text.Pandoc.Readers.Org.Parsing (OrgParser, readWithM)

import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Parsing (reportLogMessages)
import Text.Pandoc.Sources (ToSources(..), ensureFinalNewlines)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (runReaderT)

-- | Parse org-mode string and return a Pandoc document.
readOrg :: (PandocMonad m, ToSources a)
        => ReaderOptions -- ^ Reader options
        -> a
        -> m Pandoc
readOrg opts s = do
  parsed <- flip runReaderT def $
            readWithM parseOrg (optionsToParserState opts)
            (ensureFinalNewlines 2 (toSources s))
  case parsed of
    Right result -> return result
    Left  e      -> throwError e

--
-- Parser
--
parseOrg :: PandocMonad m => OrgParser m Pandoc
parseOrg = do
  blocks' <- blockList
  meta'   <- meta
  reportLogMessages
  return $ Pandoc meta' blocks'
