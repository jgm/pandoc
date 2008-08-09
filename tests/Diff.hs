-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Algorithm.Diff
-- Copyright   :  (c) Sterling Clover 2008
-- License     :  BSD 3 Clause
-- Maintainer  :  s.clover@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This is an implementation of the O(ND) diff algorithm as described in
-- \"An O(ND) Difference Algorithm and Its Variations (1986)\"
-- <http://citeseer.ist.psu.edu/myers86ond.html>. It is O(mn) in space.
-- The algorithm is the same one used by standared Unix diff.
-- The assumption is that users of this library will want to diff over
-- interesting things or peform interesting tasks with the results
-- (given that, otherwise, they would simply use the standard Unix diff
-- utility). Thus no attempt is made to present a fancier API to aid
-- in doing standard and uninteresting things with the results.
-----------------------------------------------------------------------------

module Diff (DI(..), getDiff, getGroupedDiff) where
import Data.Array
import Data.List

-- | Difference Indicator. A value is either from the First list, the Second
-- or from Both.
data DI = F | S | B deriving (Show, Eq)

data DL = DL {poi::Int, poj::Int, path::[DI]} deriving (Show, Eq)

instance Ord DL where x <= y = poi x <= poi y

canDiag :: (Eq a)  => [a] -> [a] -> Int -> Int -> (Int, Int) -> Bool
canDiag as bs lena lenb = \(i,j) ->
   if i < lena && j < lenb then arAs ! i == arBs ! j else False
    where arAs = listArray (0,lena - 1) as
          arBs = listArray (0,lenb - 1) bs

chunk :: Int -> [a] -> [[a]]
chunk x = unfoldr (\a -> case splitAt x a of ([],[]) -> Nothing; a' -> Just a')

dstep :: ((Int,Int)->Bool) -> [DL] -> [DL]
dstep cd dls = map maximum $ [hd]:(chunk 2 rst)
    where (hd:rst)  = concatMap extend dls
          extend dl = let pdl = path dl
                      in [addsnake cd $ dl {poi=poi dl + 1, path=(F : pdl)},
                          addsnake cd $ dl {poj=poj dl + 1, path=(S : pdl)}]

addsnake :: ((Int,Int)->Bool) -> DL -> DL
addsnake cd dl
    | cd (pi, pj) = addsnake cd $
                   dl {poi = pi + 1, poj = pj + 1, path=(B : path dl)}
    | otherwise   = dl
    where pi = poi dl; pj = poj dl

lcs :: (Eq a) => [a] -> [a] -> [DI]
lcs as bs = path . head . dropWhile (\dl -> poi dl /= lena || poj dl /= lenb) .
            concat . iterate (dstep cd) . (:[]) . addsnake cd $
            DL {poi=0,poj=0,path=[]}
            where cd = canDiag as bs lena lenb
                  lena = length as; lenb = length bs

-- | Takes two lists and returns a list indicating the differences
-- between them.
getDiff :: (Eq t) => [t] -> [t] -> [(DI, t)]
getDiff a b = markup a b . reverse $ lcs a b
    where markup (x:xs)   ys   (F:ds) = (F, x) : markup xs ys ds
          markup   xs   (y:ys) (S:ds) = (S, y) : markup xs ys ds
          markup (x:xs) (_:ys) (B:ds) = (B, x) : markup xs ys ds
          markup _ _ _ = []

-- | Takes two lists and returns a list indicating the differences
-- between them, grouped into chunks.
getGroupedDiff :: (Eq t) => [t] -> [t] -> [(DI, [t])]
getGroupedDiff a b = map go . groupBy (\x y -> fst x == fst y) $ getDiff a b
    where go ((d,x) : xs) = (d, x : map snd xs)
