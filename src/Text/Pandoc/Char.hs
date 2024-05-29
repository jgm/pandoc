{- |
   Module      : Text.Pandoc.Char
   Copyright   : Copyright (C) 2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Character functions not defined in Data.Char.
-}
module Text.Pandoc.Char ( isCJK )
where

-- | Returns True if character is CJK. Matches anything in:
--
-- * CJK Unified Ideographs Basic Block: U+4E00 - U+9FFF
-- * CJK Unified Ideographs Extension A: U+3400 - U+4DBF
-- * CJK Unified Ideographs Extension B: U+20000 - U+2A6DF
-- * CJK Unified Ideographs Extension C: U+2A700 - U+2B73F
-- * CJK Unified Ideographs Extension D: U+2B740 - U+2B81F
-- * CJK Compatibility Ideographs: U+F900 - U+FAFF
-- * CJK Compatibility Ideographs Supplement: U+2F800 - U+2FA1F
isCJK :: Char -> Bool
isCJK c =
  c >= '\x4e00' &&
  (    (c >= '\x2e80' && c <= '\x2eff') -- CJK Radicals Supplement
    || (c >= '\x2f00' && c <= '\x2fdf') -- Kangxi Radicals
    || (c >= '\x2ff0' && c <= '\x2fff') -- Ideographic Description Characters
    || (c >= '\x3000' && c <= '\x303f') -- JK Symbols and Punctuation
    || (c >= '\x3040' && c <= '\x309f') -- Hiragana
    || (c >= '\x30a0' && c <= '\x30ff') -- Katakana
    || (c >= '\x3100' && c <= '\x312f') -- Bopomofo
    || (c >= '\x3130' && c <= '\x318f') -- Kanbun
    || (c >= '\x3190' && c <= '\x319f') -- Kanbun
    || (c >= '\x31c0' && c <= '\x31ef') -- CJK Strokes
    || (c >= '\x31f0' && c <= '\x31ff') -- Katakana Phonetic Extensions
    || (c >= '\x3200' && c <= '\x32ff') -- Enclosed CJK Letters & Months
    || (c >= '\x3300' && c <= '\x33ff') -- CJK Compatibility
    || (c >= '\x3400' && c <= '\x4dbf') -- CJK Unified Ideographs Extension A
    || (c >= '\x4e00' && c <= '\x9fff') -- CJK Unified Ideographs
    || (c >= '\xa000' && c <= '\xa48f') -- Yi Syllables
    || (c >= '\xa490' && c <= '\xa4cf') -- Yi Radicals
    || (c >= '\xf900' && c <= '\xfaff') -- CJK Compatibility Ideographs
    || (c >= '\xfe10' && c <= '\xfe1f') -- Vertical forms
    || (c >= '\xfe30' && c <= '\xfe4f') -- CJK Compatibility Forms
    || (c >= '\xFE50' && c <= '\xFE6F') -- Small Form Variants
    || (c >= '\xFF00' && c <= '\xFFEE') -- Halfwidth and Fullwidth Forms
    || (c >= '\x1B000' && c <= '\x1B0FF') -- Kana Supplement
    || (c >= '\x1B100' && c <= '\x1B12F') -- Kana Extended-A
    || (c >= '\x1B130' && c <= '\x1B16F') -- Small Kana Extension
    || (c >= '\x20000' && c <= '\x2A6DF') -- CJK Unified Ideographs Extension B
    || (c >= '\x2A700' && c <= '\x2B73F') -- CJK Unified Ideographs Extension C
    || (c >= '\x2B740' && c <= '\x2B81F') -- CJK Unified Ideographs Extension D
    || (c >= '\x2B820' && c <= '\x2CEAF') -- CJK Unified Ideographs Extension E
    || (c >= '\x2CEB0' && c <= '\x2EBEF') -- CJK Unified Ideographs Extension F
    || (c >= '\x2F800' && c <= '\x2FA1F') -- CJK Compatibility Ideographs Supp
    || (c >= '\x30000' && c <= '\x3134F') -- CJK Unified Ideographs Exten
  )
