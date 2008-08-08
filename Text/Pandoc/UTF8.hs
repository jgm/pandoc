-- | Functions for IO using UTF-8 encoding.
-- 
-- The basic encoding and decoding functions are taken from
-- <http://www.cse.ogi.edu/~hallgren/Talks/LHiH/base/lib/UTF8.hs>.
-- (c) 2003, OGI School of Science & Engineering, Oregon Health and
-- Science University.  
--
-- From the Char module supplied with HBC.
-- Modified by Martin Norbaeck to pass illegal UTF-8 sequences unchanged.
-- Modified by John MacFarlane to use [Word8] and export IO functions.

module Text.Pandoc.UTF8 ( 
              putStrLn
            , putStr
            , hPutStrLn
            , hPutStr
            , getContents
            , readFile
            , writeFile
            ) where
import Data.Word
import System.IO ( Handle )
import qualified Data.ByteString.Lazy as BS
import Prelude hiding ( putStrLn, putStr, getContents, readFile, writeFile )

putStrLn :: String -> IO ()
putStrLn =  BS.putStrLn . BS.pack . toUTF8

putStr :: String -> IO ()
putStr = BS.putStr . BS.pack . toUTF8

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h =  BS.hPut h . BS.pack . toUTF8 . (++ "\n")

hPutStr :: Handle -> String -> IO ()
hPutStr h = BS.hPut h . BS.pack . toUTF8

readFile :: FilePath -> IO String 
readFile p = BS.readFile p >>= return . fromUTF8 . BS.unpack

writeFile :: FilePath -> String -> IO ()
writeFile p = BS.writeFile p . BS.pack . toUTF8

getContents :: IO String
getContents = BS.getContents >>= return . fromUTF8 . BS.unpack

-- | Take a list of bytes in UTF-8 encoding and decode it into a Unicode string.
fromUTF8 :: [Word8] -> String
fromUTF8 [] = ""
fromUTF8 (0xef : 0xbb : 0xbf :cs) = fromUTF8 cs -- skip BOM (byte order marker)
fromUTF8 (c:c':cs) | 0xc0 <= c  && c  <= 0xdf && 
                       0x80 <= c' && c' <= 0xbf =
	toEnum ((fromEnum c `mod` 0x20) * 0x40 + fromEnum c' `mod` 0x40) : fromUTF8 cs
fromUTF8 (c:c':c'':cs) | 0xe0 <= c   && c   <= 0xef && 
		                   0x80 <= c'  && c'  <= 0xbf &&
                           0x80 <= c'' && c'' <= 0xbf =
	toEnum ((fromEnum c `mod` 0x10 * 0x1000) + (fromEnum c' `mod` 0x40) * 0x40 + fromEnum c'' `mod` 0x40) : fromUTF8 cs
fromUTF8 (c:cs) = toEnum (fromEnum c) : fromUTF8 cs

-- | Take a Unicode string and encode it as a list of bytes in UTF-8 encoding.
toUTF8 :: String -> [Word8]
toUTF8 "" = []
toUTF8 (c:cs) =
	if c > '\x0000' && c < '\x0080' then
	    toEnum (fromEnum c) : toUTF8 cs
	else if c < toEnum 0x0800 then
	    let i = fromEnum c
	    in  toEnum (0xc0 + i `div` 0x40) : 
	        toEnum (0x80 + i `mod` 0x40) : 
		toUTF8 cs
	else
	    let i = fromEnum c
	    in  toEnum (0xe0 + i `div` 0x1000) : 
	        toEnum (0x80 + (i `mod` 0x1000) `div` 0x40) : 
		toEnum (0x80 + i `mod` 0x40) : 
		toUTF8 cs
