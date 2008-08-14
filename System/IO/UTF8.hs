-----------------------------------------------------------------------------
-- |
-- Module      :  System.IO.UTF8
-- Copyright   :  (c) Eric Mertens 2007
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer:    emertens@galois.com
-- Stability   :  experimental
-- Portability :  portable
--
-- String IO preserving UTF8 encoding.
--

module System.IO.UTF8 (
      print
    , putStr
    , putStrLn
    , getLine
    , readLn
    , readFile
    , writeFile
    , appendFile
    , getContents
    , hGetLine
    , hGetContents
    , hPutStr
    , hPutStrLn
  ) where

import Control.Monad (liftM)
import Data.Char (ord, chr)
import Data.Word (Word8)
import Prelude (String, ($), (=<<), (>>=), (.), map, toEnum, fromEnum, Read,
                Show(..))
import System.IO (Handle, IO, FilePath)
import qualified System.IO as IO

import Codec.Binary.UTF8.String (encode, decode)


-- | Encode a string in UTF8 form.
encodeString :: String -> String
encodeString xs = bytesToString (encode xs)

-- | Decode a string from UTF8
decodeString :: String -> String
decodeString xs = decode (stringToBytes xs)

-- | Convert a list of bytes to a String
bytesToString :: [Word8] -> String
bytesToString xs = map (chr . fromEnum) xs

-- | String to list of bytes.
stringToBytes :: String -> [Word8]
stringToBytes xs = map (toEnum . ord) xs

-- | The 'print' function outputs a value of any printable type to the
-- standard output device. This function differs from the
-- System.IO.print in that it preserves any UTF8 encoding of the shown value.
--
print :: Show a => a -> IO ()
print x = putStrLn (show x)

-- | Write a UTF8 string to the standard output device
putStr :: String -> IO ()
putStr x = IO.putStr (encodeString x)

-- | The same as 'putStr', but adds a newline character.
putStrLn :: String -> IO ()
putStrLn x = IO.putStrLn (encodeString x)

-- | Read a UTF8 line from the standard input device
getLine :: IO String
getLine = liftM decodeString IO.getLine

-- | The 'readLn' function combines 'getLine' and 'readIO', preserving UTF8
readLn :: Read a => IO a
readLn = IO.readIO =<< getLine

-- | The 'readFile' function reads a file and
-- returns the contents of the file as a UTF8 string.
-- The file is read lazily, on demand, as with 'getContents'.
readFile :: FilePath -> IO String
readFile n = liftM decodeString (IO.openBinaryFile n IO.ReadMode >>=
                                 IO.hGetContents)

-- | The computation 'writeFile' @file str@ function writes the UTF8 string @str@,
-- to the file @file@.
writeFile :: FilePath -> String -> IO ()
writeFile n c = IO.withBinaryFile n IO.WriteMode $ \ h ->
                    IO.hPutStr h $ encodeString c

-- | The computation 'appendFile' @file str@ function appends the UTF8 string @str@,
-- to the file @file@.
appendFile :: FilePath -> String -> IO ()
appendFile n c = IO.withBinaryFile n IO.AppendMode $ \h ->
                    IO.hPutStr h $ encodeString c

-- | Read a UTF8 line from a Handle
hGetLine :: Handle -> IO String
hGetLine h = liftM decodeString $ IO.hGetLine h

-- | Lazily read a UTF8 string from a Handle
hGetContents :: Handle -> IO String
hGetContents h = liftM decodeString (IO.hGetContents h)

-- | Write a UTF8 string to a Handle.
hPutStr :: Handle -> String -> IO ()
hPutStr h s = IO.hPutStr h (encodeString s)

-- | Write a UTF8 string to a Handle, appending a newline.
hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = IO.hPutStrLn h (encodeString s)

-- | Lazily read stdin as a UTF8 string.
getContents :: IO String
getContents = liftM decodeString IO.getContents

