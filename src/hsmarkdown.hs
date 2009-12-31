{-
Copyright (C) 2006-8 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2009 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Wrapper around pandoc that emulates Markdown.pl as closely as possible.
-}
module Main where
import System.Process
import System.Environment ( getArgs )
-- Note: ghc >= 6.12 (base >=4.2) supports unicode through iconv
-- So we use System.IO.UTF8 only if we have an earlier version
#if MIN_VERSION_base(4,2,0)
#else
import Prelude hiding ( putStr, putStrLn, writeFile, readFile, getContents )
import System.IO.UTF8
#endif
import Control.Monad (forM_)

main :: IO ()
main = do
    files <- getArgs
    let runPandoc inp = readProcess "pandoc" ["--from", "markdown", "--to", "html", "--strict"] inp >>= putStrLn
    if null files
       then getContents >>= runPandoc
       else forM_ files $ \f -> readFile f >>= runPandoc 
