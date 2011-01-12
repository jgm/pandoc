{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.Framework

import qualified Old

tests :: [Test]
tests = [ testGroup "Old" Old.tests
        ]

main :: IO ()
main = defaultMain tests
