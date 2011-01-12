{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.Framework

import qualified Old
import qualified Latex.Reader

tests :: [Test]
tests = [ testGroup "Old" Old.tests
        , testGroup "Latex" [ testGroup "Reader" Latex.Reader.tests
                            ]
        ]

main :: IO ()
main = defaultMain tests
