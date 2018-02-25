{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.Man (tests) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

creole :: Text -> Pandoc
creole = purely $ readCreole def{ readerStandalone = True }

tests :: [TestTree]
tests = []