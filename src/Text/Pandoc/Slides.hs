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
   Module      : Text.Pandoc.Slides
   Copyright   : Copyright (C) 2012 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Utility functions for splitting documents into slides for slide
show formats (dzslides, s5, slidy, beamer).
-}
module Text.Pandoc.Slides ( getSlideLevel, prepSlides ) where
import Text.Pandoc.Definition

-- | Find level of header that starts slides (defined as the least header
-- level that occurs before a non-header/non-hrule in the blocks).
getSlideLevel :: [Block] -> Int
getSlideLevel = go 6
  where go least (Header n _ : x : xs)
                 | n < least && nonHOrHR x = go n xs
                 | otherwise               = go least (x:xs)
        go least (_ : xs) = go least xs
        go least [] = least
        nonHOrHR (Header _ _) = False
        nonHOrHR (HorizontalRule) = False
        nonHOrHR _ = True

-- | Prepare a block list to be passed to hierarchicalize.
prepSlides :: Int -> [Block] -> [Block]
prepSlides slideLevel = ensureStartWithH . splitHrule
  where splitHrule (HorizontalRule : Header n xs : ys)
                       | n == slideLevel = Header slideLevel xs : splitHrule ys
        splitHrule (HorizontalRule : xs) = Header slideLevel [] : splitHrule xs
        splitHrule (x : xs)              = x : splitHrule xs
        splitHrule []                    = []
        ensureStartWithH bs@(Header n _:_)
                       | n <= slideLevel = bs
        ensureStartWithH bs              = Header slideLevel [] : bs
