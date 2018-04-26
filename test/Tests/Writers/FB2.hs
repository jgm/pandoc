{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.FB2 (tests) where

import Prelude
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

fb2 :: String -> String
fb2 x = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
        "<FictionBook xmlns=\"http://www.gribuser.ru/xml/fictionbook/2.0\" xmlns:l=\"http://www.w3.org/1999/xlink\"><description><title-info><genre>unrecognised</genre></title-info><document-info><program-used>pandoc</program-used></document-info></description><body><title><p /></title><section>" ++ x ++ "</section></body></FictionBook>"

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test (purely (writeFB2 def) . toPandoc)

tests :: [TestTree]
tests = [ testGroup "block elements"
          ["para"          =: para "Lorem ipsum cetera."
                           =?> fb2 "<p>Lorem ipsum cetera.</p>"
          ]
        , testGroup "inlines"
          [
            "Emphasis"      =:  para (emph "emphasized")
                            =?> fb2 "<p><emphasis>emphasized</emphasis></p>"
          ]
        , "bullet list" =: bulletList [ plain $ text "first"
                                      , plain $ text "second"
                                      , plain $ text "third"
                                      ]
                        =?> fb2 "<p>\x2022 first</p><p>\x2022 second</p><p>\x2022 third</p>"
         ]
