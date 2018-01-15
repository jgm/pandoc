{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.ConTeXt (tests) where

import Data.Text (unpack)
import Test.Tasty
import Test.Tasty.QuickCheck
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

context :: (ToPandoc a) => a -> String
context = unpack . purely (writeConTeXt def) . toPandoc

context' :: (ToPandoc a) => a -> String
context' = unpack . purely (writeConTeXt def{ writerWrapText = WrapNone }) . toPandoc

contextNtb :: (ToPandoc a) => a -> String
contextNtb = unpack . purely (writeConTeXt def{ writerExtensions = enableExtension Ext_ntb pandocExtensions }) . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test context "my test" $ X =?> Y

which is in turn shorthand for

  test context "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test context

tests :: [TestTree]
tests = [ testGroup "inline code"
          [ "with '}'" =: code "}" =?> "\\mono{\\}}"
          , "without '}'" =: code "]" =?> "\\type{]}"
          , testProperty "code property" $ \s -> null s ||
                if '{' `elem` s || '}' `elem` s
                   then (context' $ code s) == "\\mono{" ++
                             (context' $ str s) ++ "}"
                   else (context' $ code s) == "\\type{" ++ s ++ "}"
          ]
        , testGroup "headers"
          [ "level 1" =:
            headerWith ("my-header",[],[]) 1 "My header" =?> "\\section[my-header]{My header}"
          ]
        , testGroup "bullet lists"
          [ "nested" =:
            bulletList [
               plain (text "top")
                 <> bulletList [
                   plain (text "next")
                    <> bulletList [plain (text "bot")]
                 ]
            ] =?> unlines
                [ "\\startitemize[packed]"
                , "\\item"
                , "  top"
                , "  \\startitemize[packed]"
                , "  \\item"
                , "    next"
                , "    \\startitemize[packed]"
                , "    \\item"
                , "      bot"
                , "    \\stopitemize"
                , "  \\stopitemize"
                , "\\stopitemize" ]
          ]
        , testGroup "natural tables"
            [ test contextNtb "table with header and caption" $
              let caption = text "Table 1"
                  aligns = [(AlignRight, 0.0), (AlignLeft, 0.0), (AlignCenter, 0.0), (AlignDefault, 0.0)]
                  headers = [plain $ text "Right",
                             plain $ text "Left",
                             plain $ text "Center",
                             plain $ text "Default"]
                  rows = [[plain $ text "1.1",
                           plain $ text "1.2",
                           plain $ text "1.3",
                           plain $ text "1.4"]
                         ,[plain $ text "2.1",
                           plain $ text "2.2",
                           plain $ text "2.3",
                           plain $ text "2.4"]
                         ,[plain $ text "3.1",
                           plain $ text "3.2",
                           plain $ text "3.3",
                           plain $ text "3.4"]]
              in table caption aligns headers rows
              =?> unlines [ "\\startplacetable[caption={Table 1}]"
                          , "\\startTABLE"
                          , "\\startTABLEhead"
                          , "\\NC[align=left] Right"
                          , "\\NC[align=right] Left"
                          , "\\NC[align=middle] Center"
                          , "\\NC Default"
                          , "\\NC\\NR"
                          , "\\stopTABLEhead"
                          , "\\startTABLEbody"
                          , "\\NC[align=left] 1.1"
                          , "\\NC[align=right] 1.2"
                          , "\\NC[align=middle] 1.3"
                          , "\\NC 1.4"
                          , "\\NC\\NR"
                          , "\\NC[align=left] 2.1"
                          , "\\NC[align=right] 2.2"
                          , "\\NC[align=middle] 2.3"
                          , "\\NC 2.4"
                          , "\\NC\\NR"
                          , "\\stopTABLEbody"
                          , "\\startTABLEfoot"
                          , "\\NC[align=left] 3.1"
                          , "\\NC[align=right] 3.2"
                          , "\\NC[align=middle] 3.3"
                          , "\\NC 3.4"
                          , "\\NC\\NR"
                          , "\\stopTABLEfoot"
                          , "\\stopTABLE"
                          , "\\stopplacetable" ]
            ]
        ]

