{-
Copyright (C) 2007 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Include
   Copyright   : Copyright (C) 2006-7 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Template Haskell functions for including contents of files at compile time.
Usage:  myConstant = $(includeStrFrom "myfile")
Note that "myfile" need be present only at compile time.
-}
module Text.Pandoc.Include ( includeStrFrom 
                           , s5Path
                           , asciiMathMLPath
                           , headerPath 
                           ) where

import Language.Haskell.TH
import Control.Monad (liftM)
import System.FilePath

-- path of data files
dataPath = "data"

-- | Include contents of file as a string. 
includeStrFrom :: FilePath -> ExpQ
includeStrFrom fpath = (runIO $ readFile fpath) >>= stringE

-- | Path of an S5 file.
s5Path :: FilePath -> FilePath
s5Path p = joinPath [dataPath,"ui","default",p]

-- | Path of ASCIIMathML.js.
asciiMathMLPath :: FilePath
asciiMathMLPath = joinPath [dataPath,"ASCIIMathML.js"]

-- | Path of headers.
headerPath :: FilePath -> FilePath
headerPath p = joinPath [dataPath,"headers",p]

