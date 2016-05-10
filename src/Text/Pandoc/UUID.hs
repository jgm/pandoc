{-
Copyright (C) 2010-2016 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.UUID
   Copyright   : Copyright (C) 2010-2016 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

UUID generation using Version 4 (random method) described
in RFC4122. See http://tools.ietf.org/html/rfc4122
-}

module Text.Pandoc.UUID ( UUID, getRandomUUID ) where

import Text.Printf ( printf )
import System.Random ( randomIO )
import Data.Word
import Data.Bits ( setBit, clearBit )
import Control.Monad ( liftM )

data UUID = UUID Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8
                 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8

instance Show UUID where
  show (UUID a b c d e f g h i j k l m n o p) =
   "urn:uuid:" ++
   printf "%02x" a ++
   printf "%02x" b ++
   printf "%02x" c ++
   printf "%02x" d ++
   "-" ++
   printf "%02x" e ++
   printf "%02x" f ++
   "-" ++
   printf "%02x" g ++
   printf "%02x" h ++
   "-" ++
   printf "%02x" i ++
   printf "%02x" j ++
   "-" ++
   printf "%02x" k ++
   printf "%02x" l ++
   printf "%02x" m ++
   printf "%02x" n ++
   printf "%02x" o ++
   printf "%02x" p

getRandomUUID :: IO UUID
getRandomUUID = do
  let getRN :: a -> IO Word8
      getRN _ = liftM fromIntegral (randomIO :: IO Int)
  [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] <- mapM getRN ([1..16] :: [Int])
  -- set variant
  let i' = i `setBit` 7 `clearBit` 6
  -- set version (0100 for random)
  let g' = g `clearBit` 7 `setBit` 6 `clearBit` 5 `clearBit` 4
  return $ UUID a b c d e f g' h i' j k l m n o p

