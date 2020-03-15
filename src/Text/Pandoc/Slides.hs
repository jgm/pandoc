{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Slides
   Copyright   : Copyright (C) 2012-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Utility functions for splitting documents into slides for slide
show formats (dzslides, revealjs, s5, slidy, slideous, beamer).
-}
module Text.Pandoc.Slides ( getSlideLevel, prepSlides ) where
import Text.Pandoc.Definition

-- | Find level of header that starts slides (defined as the least header
-- level that occurs before a non-header/non-hrule in the blocks).
getSlideLevel :: [Block] -> Int
getSlideLevel = go 6
  where go least (Header n _ _ : x : xs)
                 | n < least && nonHOrHR x = go n xs
                 | otherwise               = go least (x:xs)
        go least (Div _ bs : xs) = min (go least bs) (go least xs)
        go least (_ : xs) = go least xs
        go least [] = least
        nonHOrHR Header{}       = False
        nonHOrHR HorizontalRule = False
        nonHOrHR _              = True

-- | Prepare a block list to be passed to makeSections.
prepSlides :: Int -> [Block] -> [Block]
prepSlides slideLevel = ensureStartWithH . splitHrule . extractRefsHeader
  where splitHrule (HorizontalRule : Header n attr xs : ys)
                       | n == slideLevel = Header slideLevel attr xs : splitHrule ys
        splitHrule (HorizontalRule : xs) = Header slideLevel nullAttr [Str "\0"] :
                                           splitHrule xs
        splitHrule (x : xs)              = x : splitHrule xs
        splitHrule []                    = []
        extractRefsHeader bs             =
          case reverse bs of
               (Div ("refs",classes,kvs) (Header n attrs xs : ys) : zs)
                 -> reverse zs ++ [Header n attrs xs,
                                   Div ("refs",classes,kvs) ys]
               _ -> bs
        ensureStartWithH bs@(Header n _ _:_)
                       | n <= slideLevel = bs
        ensureStartWithH bs@(Div _ (Header n _ _:_) : _)
                       | n <= slideLevel = bs
        ensureStartWithH bs              = Header slideLevel nullAttr [Str "\0"] : bs
