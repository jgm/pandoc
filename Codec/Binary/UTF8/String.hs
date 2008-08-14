--
-- |
-- Module      :  Codec.Binary.UTF8.String
-- Copyright   :  (c) Eric Mertens 2007
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer:    emertens@galois.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Support for encoding UTF8 Strings to and from @[Word8]@
--

module Codec.Binary.UTF8.String (
      encode
    , decode
    , encodeString
    , decodeString
  ) where

import Data.Word        (Word8)
import Data.Bits        ((.|.),(.&.),shiftL,shiftR)
import Data.Char        (chr,ord)

default(Int)

-- | Encode a string using 'encode' and store the result in a 'String'.
encodeString :: String -> String
encodeString xs = map (toEnum . fromEnum) (encode xs)

-- | Decode a string using 'decode' using a 'String' as input.
-- | This is not safe but it is necessary if UTF-8 encoded text
-- | has been loaded into a 'String' prior to being decoded.
decodeString :: String -> String
decodeString xs = decode (map (toEnum . fromEnum) xs)

replacement_character :: Char
replacement_character = '\xfffd'

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
encode :: String -> [Word8]
encode = concatMap (map fromIntegral . go . ord)
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

--
-- | Decode a UTF8 string packed into a list of Word8 values, directly to String
--
decode :: [Word8] -> String
decode [    ] = ""
decode (c:cs)
  | c < 0x80  = chr (fromEnum c) : decode cs
  | c < 0xc0  = replacement_character : decode cs
  | c < 0xe0  = multi1
  | c < 0xf0  = multi_byte 2 0xf  0x800
  | c < 0xf8  = multi_byte 3 0x7  0x10000
  | c < 0xfc  = multi_byte 4 0x3  0x200000
  | c < 0xfe  = multi_byte 5 0x1  0x4000000
  | otherwise = replacement_character : decode cs
  where
    multi1 = case cs of
      c1 : ds | c1 .&. 0xc0 == 0x80 ->
        let d = ((fromEnum c .&. 0x1f) `shiftL` 6) .|.  fromEnum (c1 .&. 0x3f)
        in if d >= 0x000080 then toEnum d : decode ds
                            else replacement_character : decode ds
      _ -> replacement_character : decode cs

    multi_byte :: Int -> Word8 -> Int -> [Char]
    multi_byte i mask overlong = aux i cs (fromEnum (c .&. mask))
      where
        aux 0 rs acc
          | overlong <= acc && acc <= 0x10ffff &&
            (acc < 0xd800 || 0xdfff < acc)     &&
            (acc < 0xfffe || 0xffff < acc)      = chr acc : decode rs
          | otherwise = replacement_character : decode rs

        aux n (r:rs) acc
          | r .&. 0xc0 == 0x80 = aux (n-1) rs
                               $ shiftL acc 6 .|. fromEnum (r .&. 0x3f)

        aux _ rs     _ = replacement_character : decode rs

