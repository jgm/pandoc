{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.ConTeXt (tests) where

import Prelude
import Data.Text (unpack, pack)
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

contextDiv :: (ToPandoc a) => a -> String
contextDiv = unpack . purely (writeConTeXt def{ writerSectionDivs = True }) . toPandoc

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
          , testProperty "code property" $ \s -> null s || '\n' `elem` s ||
                if '{' `elem` s || '}' `elem` s
                   then context' (code $ pack s) == "\\mono{" ++
                             context' (str $ pack s) ++ "}"
                   else context' (code $ pack s) == "\\type{" ++ s ++ "}"
          ]
        , testGroup "headers"
          [ "level 1" =:
            headerWith ("my-header",[],[]) 1 "My header" =?> "\\section[title={My header},reference={my-header}]"
          , test contextDiv "section-divs" $
                   (   headerWith ("header1", [], []) 1 (text "Header1")
                    <> headerWith ("header2", [], []) 2 (text "Header2")
                    <> headerWith ("header3", [], []) 3 (text "Header3")
                    <> headerWith ("header4", [], []) 4 (text "Header4")
                    <> headerWith ("header5", [], []) 5 (text "Header5")
                    <> headerWith ("header6", [], []) 6 (text "Header6"))
                   =?>
              unlines [ "\\startsection[title={Header1},reference={header1}]\n"
                      , "\\startsubsection[title={Header2},reference={header2}]\n"
                      , "\\startsubsubsection[title={Header3},reference={header3}]\n"
                      , "\\startsubsubsubsection[title={Header4},reference={header4}]\n"
                      , "\\startsubsubsubsubsection[title={Header5},reference={header5}]\n"
                      , "\\startsubsubsubsubsubsection[title={Header6},reference={header6}]\n"
                      , "\\stopsubsubsubsubsubsection\n"
                      , "\\stopsubsubsubsubsection\n"
                      , "\\stopsubsubsubsection\n"
                      , "\\stopsubsubsection\n"
                      , "\\stopsubsection\n"
                      , "\\stopsection" ]
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
              let capt = text "Table 1"
                  aligns = [(AlignRight, ColWidthDefault), (AlignLeft, ColWidthDefault), (AlignCenter, ColWidthDefault), (AlignDefault, ColWidthDefault)]
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
                  toRow = Row nullAttr . map simpleCell
              in table (simpleCaption $ plain capt)
                       aligns
                       (TableHead nullAttr [toRow headers])
                       [TableBody nullAttr 0 [] $ map toRow rows]
                       (TableFoot nullAttr [])
              =?> unlines [ "\\startplacetable[title={Table 1}]"
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
