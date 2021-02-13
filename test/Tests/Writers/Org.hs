{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Org (tests) where

import Data.Text as T
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, Text) -> TestTree
(=:) = test org

defopts :: WriterOptions
defopts = def
  { writerExtensions = getDefaultExtensions "org"
  }

org :: (ToPandoc a) => a -> Text
org = orgWithOpts defopts

orgWithOpts :: (ToPandoc a) => WriterOptions -> a -> Text
orgWithOpts opts x = purely (writeOrg opts) $ toPandoc x


tests :: [TestTree]
tests =
  [ testGroup "links"
    -- See http://orgmode.org/manual/Internal-links.html#Internal-links
    [ "simple link"
      =: link "/url" "" "foo"
      =?> "[[/url][foo]]"
    , "internal link to anchor"
      =: link "#my-custom-id" "" "#my-custom-id"
      =?> "[[#my-custom-id]]"
    ]

  , testGroup "lists"
    [ "bullet task list"
      =: bulletList [plain "☐ a", plain "☒ b"]
      =?> T.unlines
          [ "- [ ] a"
          , "- [X] b"
          ]
    , "ordered task list"
      =: orderedList [plain ("☐" <> space <> "a"), plain "☒ b"]
      =?> T.unlines
          [ "1. [ ] a"
          , "2. [X] b"
          ]
    , test (orgWithOpts def) "bullet without task_lists" $
      bulletList [plain "☐ a", plain "☒ b"]
      =?> T.unlines
          [ "- ☐ a"
          , "- ☒ b"
          ]
    ]
  ]
