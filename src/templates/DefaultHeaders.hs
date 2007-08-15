{-
Copyright (C) 2006-7 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.DefaultHeaders
   Copyright   : Copyright (C) 2006-7 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Default headers for Pandoc writers.
-}
module Text.Pandoc.Writers.DefaultHeaders  (
                                      defaultLaTeXHeader,
                                      defaultConTeXtHeader,
                                      defaultDocbookHeader,
                                      defaultS5Header,
                                      defaultRTFHeader
                                      ) where
import Text.Pandoc.Writers.S5

defaultLaTeXHeader :: String
defaultLaTeXHeader = "@LaTeXHeader@"

defaultConTeXtHeader :: String
defaultConTeXtHeader = "@ConTeXtHeader@"

defaultDocbookHeader :: String
defaultDocbookHeader = "@DocbookHeader@"

defaultS5Header :: String
defaultS5Header = s5Meta ++ s5CSS ++ s5Javascript

defaultRTFHeader :: String
defaultRTFHeader = "@RTFHeader@"
