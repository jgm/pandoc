{-
Copyright (C) 2011-2017 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; Either version 2 of the License, or
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
   Module      : Text.Pandoc.Readers.Native
   Copyright   : Copyright (C) 2011-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of a string representation of a pandoc type (@Pandoc@,
@[Block]@, @Block@, @[Inline]@, or @Inline@) to a @Pandoc@ document.
-}
module Text.Pandoc.Readers.Native ( readNative ) where

import Text.Pandoc.Definition
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Shared (safeRead)

import Control.Monad.Except (throwError)
import Text.Pandoc.Class
import Text.Pandoc.Error
import Data.Text (Text, unpack)

-- | Read native formatted text and return a Pandoc document.
-- The input may be a full pandoc document, a block list, a block,
-- an inline list, or an inline.  Thus, for example,
--
-- > Str "hi"
--
-- will be treated as if it were
--
-- > Pandoc nullMeta [Plain [Str "hi"]]
--
readNative :: PandocMonad m
           => ReaderOptions
           -> Text       -- ^ String to parse (assuming @'\n'@ line endings)
           -> m Pandoc
readNative _ s =
  case maybe (Pandoc nullMeta <$> readBlocks s) Right (safeRead (unpack s)) of
    Right doc -> return doc
    Left _    -> throwError $ PandocParseError "couldn't read native"

readBlocks :: Text -> Either PandocError [Block]
readBlocks s = maybe ((:[]) <$> readBlock s) Right (safeRead (unpack s))

readBlock :: Text -> Either PandocError Block
readBlock s = maybe (Plain <$> readInlines s) Right (safeRead (unpack s))

readInlines :: Text -> Either PandocError [Inline]
readInlines s = maybe ((:[]) <$> readInline s) Right (safeRead (unpack s))

readInline :: Text -> Either PandocError Inline
readInline s = maybe (Left . PandocParseError $ "Could not read: " ++ unpack s) Right (safeRead (unpack s))

