{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.AsciiDoc (tests) where

import Data.Text (unpack)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

asciidoc :: (ToPandoc a) => a -> String
asciidoc = unpack . purely (writeAsciiDoc def) . toPandoc

asciidoctor :: (ToPandoc a) => a -> String
asciidoctor = unpack . purely (writeAsciiDoctor def) . toPandoc

testAsciidoc :: (ToString a, ToPandoc a)
             => String
             -> (a, String)
             -> TestTree
testAsciidoc = test asciidoc

testAsciidoctor :: (ToString a, ToPandoc a)
             => String
             -> (a, String)
             -> TestTree
testAsciidoctor = test asciidoctor

tests :: [TestTree]
tests = [ testGroup "emphasis"
          [ testAsciidoc "emph word before" $
               para (text "foo" <> emph (text "bar")) =?>
                 "foo__bar__"
          , testAsciidoc "emph word after" $
               para (emph (text "foo") <> text "bar") =?>
                 "__foo__bar"
          , testAsciidoc "emph quoted" $
               para (doubleQuoted (emph (text "foo"))) =?>
                 "``__foo__''"
          , testAsciidoc "strong word before" $
               para (text "foo" <> strong (text "bar")) =?>
                 "foo**bar**"
          , testAsciidoc "strong word after" $
               para (strong (text "foo") <> text "bar") =?>
                 "**foo**bar"
          , testAsciidoc "strong quoted" $
               para (singleQuoted (strong (text "foo"))) =?>
                 "`**foo**'"
          ]
        , testGroup "blocks"
          [ testAsciidoc "code block without line numbers" $
               codeBlockWith ("", [ "haskell" ], []) "foo" =?> unlines
                                           [ "[source,haskell]"
                                           , "----"
                                           , "foo"
                                           , "----"
                                           ]
          , testAsciidoc "code block with line numbers" $
               codeBlockWith ("", [ "haskell", "numberLines" ], []) "foo" =?> unlines
                                           [ "[source%linesnum,haskell]"
                                           , "----"
                                           , "foo"
                                           , "----"
                                           ]
          ]
        , testGroup "tables"
          [ testAsciidoc "empty cells" $
               simpleTable [] [[mempty],[mempty]] =?> unlines
                                           [ "[cols=\"\",]"
                                           , "|==="
                                           , "|"
                                           , "|"
                                           , "|==="
                                           ]
          , test asciidoc "multiblock cells" $
               simpleTable [] [[para (text "Para 1") <> para (text "Para 2")]]
                                           =?> unlines
                                           [ "[cols=\"\",]"
                                           , "|==="
                                           , "a|"
                                           , "Para 1"
                                           , ""
                                           , "Para 2"
                                           , ""
                                           , "|==="
                                           ]
          ]
        , testGroup "lists"
          [ testAsciidoctor "bullet task list" $
               bulletList [plain "☐ a", plain "☒ b"] =?> unlines
                                           [ "* [ ] a"
                                           , "* [x] b"
                                           ]
          ]
        ]
