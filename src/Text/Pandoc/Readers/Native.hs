{-
Copyright (C) 2011 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2011 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of a string representation of a pandoc type (@Pandoc@,
@[Block]@, @Block@, @[Inline]@, or @Inline@) to a @Pandoc@ document.
-}
module Text.Pandoc.Readers.Native ( readNative ) where

import Text.Pandoc.Definition

nullMeta :: Meta
nullMeta = Meta{ docTitle = []
               , docAuthors = []
               , docDate = []
               }

-- | Read native formatted text and return a Pandoc document.
-- The input may be a full pandoc document, a block list, a block,
-- an inline list, or an inline.  Thus, for example,
--
-- > Str "hi"
--
-- will be treated as if it were
--
-- > Pandoc (Meta [] [] []) [Plain [Str "hi"]]
--
readNative :: String      -- ^ String to parse (assuming @'\n'@ line endings)
           -> Pandoc
readNative s =
  case reads s of
       (d,_):_    -> d
       []         -> Pandoc nullMeta $ readBlocks s

readBlocks :: String -> [Block]
readBlocks s =
  case reads s of
       (d,_):_    -> d
       []         -> [readBlock s]

readBlock :: String -> Block
readBlock s =
  case reads s of
       (d,_):_    -> d
       []         -> Plain $ readInlines s

readInlines :: String -> [Inline]
readInlines s =
  case reads s of
       (d,_):_    -> d
       []         -> [readInline s]

readInline :: String -> Inline
readInline s =
  case reads s of
       (d,_):_    -> d
       []         -> error "Cannot parse document"

