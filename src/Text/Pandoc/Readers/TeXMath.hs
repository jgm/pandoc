{-
Copyright (C) 2007-2014 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Readers.TeXMath
   Copyright   : Copyright (C) 2007-2014 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of TeX math to a list of 'Pandoc' inline elements.
-}
module Text.Pandoc.Readers.TeXMath ( readTeXMath, readTeXMath' ) where

import Text.Pandoc.Definition
import Text.TeXMath

-- | Converts a raw TeX math formula to a list of 'Pandoc' inlines.
-- Defaults to raw formula between @$@ or @$$@ characters if entire formula
-- can't be converted.
readTeXMath' :: MathType
             -> String    -- ^ String to parse (assumes @'\n'@ line endings)
             -> [Inline]
readTeXMath' mt inp = case texMathToPandoc dt inp of
                           Left _    -> [Str (delim ++ inp ++ delim)]
                           Right res -> res
    where (dt, delim) = case mt of
                             DisplayMath -> (DisplayBlock, "$$")
                             InlineMath  -> (DisplayInline, "$")

{-# DEPRECATED readTeXMath "Use readTeXMath' from Text.Pandoc.JSON instead" #-}
-- | Converts a raw TeX math formula to a list of 'Pandoc' inlines.
-- Defaults to raw formula between @$@ characters if entire formula
-- can't be converted.  (This is provided for backwards compatibility;
-- it is better to use @readTeXMath'@, which properly distinguishes
-- between display and inline math.)
readTeXMath :: String    -- ^ String to parse (assumes @'\n'@ line endings)
            -> [Inline]
readTeXMath = readTeXMath' InlineMath
