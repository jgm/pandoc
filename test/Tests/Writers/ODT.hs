{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Writers.ODT
   Copyright   : © 2026 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Tests for the ODT writers.
-}
module Tests.Writers.ODT (tests) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit
import Text.Pandoc
import Text.Pandoc.Builder
import qualified Text.Pandoc.XML.Light as XML

-- | The @xlink:href@ of every image in a flat OpenDocument file.  The
-- local name is enough to match on, since the prefix is an artifact of
-- the namespace declarations we happen to write.
imageHrefs :: Text -> IO [Text]
imageHrefs txt =
  case XML.parseXMLElement (TL.fromStrict txt) of
    Left msg    -> assertFailure $
                     "flat OpenDocument output is not valid XML: "
                     ++ T.unpack msg
    Right root  -> return
      [ href
      | el        <- XML.filterElementsName ((== "image") . XML.qName) root
      , Just href <- [XML.findAttrBy ((== "href") . XML.qName) el]
      ]

tests :: [TestTree]
tests =
  -- This cannot be a command test, because extracting the reference
  -- from the output needs a pipe into grep, and the command tests run
  -- under cmd.exe on Windows.
  [ testCase "fodt --link-images leaves a relative reference alone" $ do
      -- An ODT resolves relative references against its zip structure,
      -- so the writer prefixes them with "../" (see fixInternalLinks).
      -- A flat file is a single file, and must not have one.
      fodt  <- runIOorExplode $ setVerbosity ERROR >>
                 writeFODT def{ writerLinkImages = True } doc'
      hrefs <- imageHrefs fodt
      ["lalune.jpg"] @=? hrefs
  ]
 where
  doc' = doc $ para $ image "lalune.jpg" "" mempty
