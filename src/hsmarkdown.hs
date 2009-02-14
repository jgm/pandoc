{-# LANGUAGE CPP #-}
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
import Prelude hiding ( putStr, putStrLn, writeFile, readFile, getContents )
import System.IO.UTF8
import Control.Monad (forM_)

main :: IO ()
main = do
    files <- getArgs
    let runPandoc inp = readProcess "pandoc" ["--from", "markdown", "--to", "html", "--strict"] inp >>= putStrLn
    if null files
       then getContents >>= runPandoc
       else forM_ files $ \f -> readFile f >>= runPandoc 
