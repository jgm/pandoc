{-
Copyright (C) 2014-2017 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

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
   Module      : Text.Pandoc.Readers.Org
   Copyright   : Copyright (C) 2014-2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Conversion of org-mode formatted plain text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Org ( readOrg ) where

import Text.Pandoc.Readers.Org.Blocks (blockList, meta)
import Text.Pandoc.Readers.Org.ParserState (optionsToParserState)
import Text.Pandoc.Readers.Org.Parsing (OrgParser, readWithM)

import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Options
import Text.Pandoc.Parsing (reportLogMessages)
import Text.Pandoc.Shared (crFilter)

import Control.Monad.Except (throwError)
import Control.Monad.Reader (runReaderT)

import Data.Text (Text)
import qualified Data.Text as T

-- | Parse org-mode string and return a Pandoc document.
readOrg :: PandocMonad m
        => ReaderOptions -- ^ Reader options
        -> Text          -- ^ String to parse (assuming @'\n'@ line endings)
        -> m Pandoc
readOrg opts s = do
  parsed <- flip runReaderT def $
            readWithM parseOrg (optionsToParserState opts)
            (T.unpack (crFilter s) ++ "\n\n")
  case parsed of
    Right result -> return result
    Left  _      -> throwError $ PandocParseError "problem parsing org"

--
-- Parser
--
parseOrg :: PandocMonad m => OrgParser m Pandoc
parseOrg = do
  blocks' <- blockList
  meta'   <- meta
  reportLogMessages
  return $ Pandoc meta' blocks'
