{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Tests.Walk (tests) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Test.Framework
import Tests.Helpers
import Data.Char (toUpper)
import Tests.Arbitrary()
import Data.Generics
import Data.Monoid

tests :: [Test]
tests = [ testGroup "Walk"
          [ property "p_walk inlineTrans" (p_walk inlineTrans)
          , property "p_walk blockTrans" (p_walk blockTrans)
          , property "p_query inlineQuery" (p_query inlineQuery)
          , property "p_query blockQuery" (p_query blockQuery)
          ]
        ]

p_walk :: (Typeable a, Walkable a Pandoc)
       => (a -> a) -> Pandoc -> Bool
p_walk f d = everywhere (mkT f) d == walk f d

p_query :: (Eq a, Typeable a1, Monoid a, Walkable a1 Pandoc)
        => (a1 -> a) -> Pandoc -> Bool
p_query f d = everything mappend (mempty `mkQ` f) d == query f d

inlineTrans :: Inline -> Inline
inlineTrans (Str xs) = Str $ map toUpper xs
inlineTrans (Emph xs) = Strong xs
inlineTrans x = x

blockTrans :: Block -> Block
blockTrans (Plain xs) = Para xs
blockTrans (BlockQuote xs) = Div ("",["special"],[]) xs
blockTrans x = x

inlineQuery :: Inline -> String
inlineQuery (Str xs) = xs
inlineQuery _ = ""

blockQuery :: Block -> [Int]
blockQuery (Header lev _ _) = [lev]
blockQuery _ = []

