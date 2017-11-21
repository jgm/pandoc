{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.JATS (tests) where

import Data.Text (Text)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

jats :: Text -> Pandoc
jats = purely $ readJATS def

tests :: [TestTree]
tests = []
