{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Org.Shared
   Copyright   : © 2014-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Helper functions used by other org tests.
-}
module Tests.Readers.Org.Shared
  ( (=:)
  , org
  , spcSep
  , tagSpan
  ) where

import Prelude
import Data.List (intersperse)
import Data.Text (Text)
import Tests.Helpers (ToString, purely, test)
import Test.Tasty (TestTree)
import Text.Pandoc (Pandoc, ReaderOptions (readerExtensions),
                    def, getDefaultExtensions, readOrg)
import Text.Pandoc.Builder (Inlines, smallcaps, space, spanWith, str)

org :: Text -> Pandoc
org = purely $ readOrg def{ readerExtensions = getDefaultExtensions "org" }

infix 4 =:
(=:) :: ToString c
     => String -> (Text, c) -> TestTree
(=:) = test org

spcSep :: [Inlines] -> Inlines
spcSep = mconcat . intersperse space

-- | Create a span for the given tag.
tagSpan :: Text -> Inlines
tagSpan t = spanWith ("", ["tag"], [("tag-name", t)]) . smallcaps $ str t
