{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.Framework

import qualified Tests.Old
import qualified Tests.Readers.LaTeX

tests :: [Test]
tests = [ testGroup "Old" Tests.Old.tests
        , testGroup "Readers"
          [ testGroup "LaTeX" Tests.Readers.LaTeX.tests
          ]
        ]

main :: IO ()
main = defaultMain tests
