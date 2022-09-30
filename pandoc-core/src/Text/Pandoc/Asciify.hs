{- |
   Module      : Text.Pandoc.Asciify
   Copyright   : Copyright (C) 2013-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Function to convert accented latin letters to their unaccented
ascii equivalents (used in constructing HTML identifiers).
-}
module Text.Pandoc.Asciify (toAsciiChar, toAsciiText)
where
import Data.Char (isAscii, isMark)
import qualified Data.Text.Normalize as TN
import Data.Text (Text)
import qualified Data.Text as T

toAsciiText :: Text -> Text
toAsciiText = T.filter isAscii . T.map specialCase . TN.normalize (TN.NFD)
 where
  specialCase '\x131' = 'i' -- Turkish undotted i
  specialCase c = c

toAsciiChar :: Char -> Maybe Char
toAsciiChar c = case T.unpack (TN.normalize TN.NFD (T.singleton c)) of
                  (x:xs) | isAscii x
                         , all isMark xs
                         -> Just x
                  ['\x131'] -> Just 'i'  -- Turkish undotted i
                  _      -> Nothing
