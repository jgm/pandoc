{-
Copyright (C) 2010 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.UTF8
   Copyright   : Copyright (C) 2010 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

UTF-8 aware string IO functions that will work with GHC 6.10, 6.12, or 7.
-}
module Text.Pandoc.UTF8 ( readFile
                        , writeFile
                        , getContents
                        , putStr
                        , putStrLn
                        , hPutStr
                        , hPutStrLn
                        , hGetContents
                        , encodePath
                        , decodeArg
                        )

where

#if MIN_VERSION_base(4,4,0)
#else
import Codec.Binary.UTF8.String (encodeString, decodeString)
#endif

import System.IO hiding (readFile, writeFile, getContents,
                          putStr, putStrLn, hPutStr, hPutStrLn, hGetContents)
import Prelude hiding (readFile, writeFile, getContents, putStr, putStrLn )
import qualified System.IO as IO

readFile :: FilePath -> IO String
readFile f = do
  h <- openFile (encodePath f) ReadMode
  hGetContents h

writeFile :: FilePath -> String -> IO ()
writeFile f s = withFile (encodePath f) WriteMode $ \h -> hPutStr h s

getContents :: IO String
getContents = hGetContents stdin

putStr :: String -> IO ()
putStr s = hPutStr stdout s

putStrLn :: String -> IO ()
putStrLn s = hPutStrLn stdout s

hPutStr :: Handle -> String -> IO ()
hPutStr h s = hSetEncoding h utf8 >> IO.hPutStr h s

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = hSetEncoding h utf8 >> IO.hPutStrLn h s

hGetContents :: Handle -> IO String
hGetContents h = hSetEncoding h utf8_bom >> hSetNewlineMode h universalNewlineMode
                  >> IO.hGetContents h

encodePath :: FilePath -> FilePath
decodeArg :: String -> String
#if MIN_VERSION_base(4,4,0)
encodePath = id
decodeArg = id
#else
encodePath = encodeString
decodeArg = decodeString
#endif
