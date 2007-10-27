-- | Functions for converting Unicode strings to UTF-8 and vice versa.
-- 
-- Taken from <http://www.cse.ogi.edu/~hallgren/Talks/LHiH/base/lib/UTF8.hs>.
-- (c) 2003, OGI School of Science & Engineering, Oregon Health and
-- Science University.  
--
-- Modified by Martin Norbaeck
-- to pass illegal UTF-8 sequences through unchanged.
module Text.Pandoc.UTF8 ( 
             fromUTF8, 
             toUTF8 
            ) where

-- From the Char module supplied with HBC.

-- | Take a UTF-8 string and decode it into a Unicode string.
fromUTF8 :: String -> String
fromUTF8 "" = ""
fromUTF8 ('\xef':'\xbb':'\xbf':cs) = fromUTF8 cs -- skip BOM (byte order marker)
fromUTF8 (c:c':cs) | '\xc0' <= c  && c  <= '\xdf' && 
		             '\x80' <= c' && c' <= '\xbf' =
	toEnum ((fromEnum c `mod` 0x20) * 0x40 + fromEnum c' `mod` 0x40) : fromUTF8 cs
fromUTF8 (c:c':c'':cs) | '\xe0' <= c   && c   <= '\xef' && 
		          '\x80' <= c'  && c'  <= '\xbf' &&
		          '\x80' <= c'' && c'' <= '\xbf' =
	toEnum ((fromEnum c `mod` 0x10 * 0x1000) + (fromEnum c' `mod` 0x40) * 0x40 + fromEnum c'' `mod` 0x40) : fromUTF8 cs
fromUTF8 (c:cs) = c : fromUTF8 cs

-- | Take a Unicode string and encode it as a UTF-8 string.
toUTF8 :: String -> String
toUTF8 "" = ""
toUTF8 (c:cs) =
	if c > '\x0000' && c < '\x0080' then
	    c : toUTF8 cs
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
