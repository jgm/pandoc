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
   Module      : Text.Pandoc.Readers.Org
   Copyright   : Copyright (C) 2014-2016 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Conversion of org-mode formatted plain text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Org ( readOrg ) where

import Text.Pandoc.Readers.Org.Blocks ( blockList, meta )
import Text.Pandoc.Readers.Org.Parsing ( OrgParser, optional, readWithM, try
                                       , updateState )
import Text.Pandoc.Readers.Org.ParserState ( OrgParserState ( orgStateMeta )
                                           , optionsToParserState )

import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Options
import Text.Pandoc.YAML ( yamlMetaBlock )

import Data.Monoid ( (<>) )
import Control.Monad.Reader ( runReader )


-- | Parse org-mode string and return a Pandoc document.
readOrg :: ReaderOptions -- ^ Reader options
        -> String        -- ^ String to parse (assuming @'\n'@ line endings)
        -> Either PandocError Pandoc
readOrg opts s = flip runReader def $
  readWithM parseOrg
            (optionsToParserState opts)
            (s ++ "\n\n")

--
-- Parser
--
parseOrg :: OrgParser Pandoc
parseOrg = do
  optional orgYamlMetaBlock
  blocks' <- blockList
  meta'   <- meta
  return $ Pandoc meta' blocks'

--
-- YAML Block
--
orgYamlMetaBlock :: OrgParser ()
orgYamlMetaBlock = try $ do
  meta' <- yamlMetaBlock readOrg
  updateState $ \st -> st{ orgStateMeta = orgStateMeta st <> (return meta') }
