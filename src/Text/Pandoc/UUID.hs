{- |
   Module      : Text.Pandoc.UUID
   Copyright   : Copyright (C) 2010-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

UUID generation using Version 4 (random method) described
in RFC4122. See http://tools.ietf.org/html/rfc4122
-}

module Text.Pandoc.UUID ( UUID(..), getRandomUUID ) where

import Data.Bits (clearBit, setBit)
import Data.Word
import System.Random (RandomGen, randoms)
import Text.Printf (printf)
import Text.Pandoc.Class.PandocMonad

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

getUUID :: RandomGen g => g -> UUID
getUUID gen =
  case take 16 (randoms gen :: [Word8]) of
       [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] ->
         -- set variant
         let i' = i `setBit` 7 `clearBit` 6
         -- set version (0100 for random)
             g' = g `clearBit` 7 `setBit` 6 `clearBit` 5 `clearBit` 4
         in  UUID a b c d e f g' h i' j k l m n o p
       _ -> error "not enough random numbers for UUID" -- should not happen

getRandomUUID :: PandocMonad m => m UUID
getRandomUUID = getUUID <$> newStdGen
