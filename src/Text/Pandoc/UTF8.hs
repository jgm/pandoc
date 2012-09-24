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

UTF-8 aware string IO functions that will work with GHC 6.12 or 7.
The reading functions first attempt to read UTF-8; if an encoding
error is encountered, the local encoding is used instead.  This
should work well in practice because text in other encodings
is usually not valid UTF-8.
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
import Control.Exception (catch, throwIO)
import GHC.IO.Exception (IOException(..), IOErrorType(..))
import System.IO hiding (readFile, writeFile, getContents,
                          putStr, putStrLn, hPutStr, hPutStrLn, hGetContents)
import Prelude hiding (readFile, writeFile, getContents, putStr, putStrLn, catch )
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
hGetContents h = do
  hSetEncoding h utf8_bom
  catch (IO.hGetContents h) $ \e ->
    case ioe_type e of
         InvalidArgument -> do
           hSetEncoding h localeEncoding
           IO.hGetContents h
         _ -> throwIO e

encodePath :: FilePath -> FilePath
decodeArg :: String -> String
#if MIN_VERSION_base(4,4,0)
encodePath = id
decodeArg = id
#else
encodePath = encodeString
decodeArg = decodeString
#endif
