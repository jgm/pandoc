{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.Framework

import qualified Tests.Old
import qualified Tests.Readers.LaTeX
import qualified Tests.Shared

tests :: [Test]
tests = [ testGroup "Old" Tests.Old.tests
        , testGroup "Readers"
          [ testGroup "LaTeX" Tests.Readers.LaTeX.tests
          ]
        , testGroup "Shared" Tests.Shared.tests
        ]

main :: IO ()
main = defaultMain tests
