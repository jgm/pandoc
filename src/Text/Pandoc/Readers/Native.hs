{-
Copyright (C) 2011-2014 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Readers.Native
   Copyright   : Copyright (C) 2011-2014 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of a string representation of a pandoc type (@Pandoc@,
@[Block]@, @Block@, @[Inline]@, or @Inline@) to a @Pandoc@ document.
-}
module Text.Pandoc.Readers.Native ( readNative ) where

import Text.Pandoc.Definition
import Text.Pandoc.Shared (safeRead)

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
readNative :: String      -- ^ String to parse (assuming @'\n'@ line endings)
           -> Pandoc
readNative s =
  case safeRead s of
       Just d    -> d
       Nothing   -> Pandoc nullMeta $ readBlocks s

readBlocks :: String -> [Block]
readBlocks s =
  case safeRead s of
       Just d    -> d
       Nothing   -> [readBlock s]

readBlock :: String -> Block
readBlock s =
  case safeRead s of
       Just d    -> d
       Nothing   -> Plain $ readInlines s

readInlines :: String -> [Inline]
readInlines s =
  case safeRead s of
       Just d     -> d
       Nothing    -> [readInline s]

readInline :: String -> Inline
readInline s =
  case safeRead s of
       Just d     -> d
       Nothing    -> error "Cannot parse document"

