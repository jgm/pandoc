{-
Copyright (C) 2012 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Options
   Copyright   : Copyright (C) 2012 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Data structures and functions for representing parser and writer
options.
-}
module Text.Pandoc.Options ( Extension(..)
                           ) where
import Data.Set (Set)

-- | Individually selectable markdown syntax extensions.
data Extension = Footnotes
               | TeX_math
               | Delimited_code_blocks
               | Markdown_in_HTML_blocks
               | Fancy_lists
               | Definition_lists
               | Header_identifiers
               | All_symbols_escapable
               | Intraword_underscores
               | Blank_before_blockquote
               | Blank_before_header
               | Significant_bullets
               deriving (Show, Read, Enum, Eq, Bounded)


