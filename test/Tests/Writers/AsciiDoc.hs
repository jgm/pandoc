module Tests.Writers.AsciiDoc (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Text.Pandoc.Arbitrary()

asciidoc :: (ToPandoc a) => a -> String
asciidoc = purely (writeAsciiDoc def{ writerWrapText = WrapNone }) . toPandoc

tests :: [Test]
tests = [ testGroup "emphasis"
          [ test asciidoc "emph word before" $
               para (text "foo" <> emph (text "bar")) =?>
                 "foo__bar__"
          , test asciidoc "emph word after" $
               para (emph (text "foo") <> text "bar") =?>
                 "__foo__bar"
          , test asciidoc "emph quoted" $
               para (doubleQuoted (emph (text "foo"))) =?>
                 "``__foo__''"
          , test asciidoc "strong word before" $
               para (text "foo" <> strong (text "bar")) =?>
                 "foo**bar**"
          , test asciidoc "strong word after" $
               para (strong (text "foo") <> text "bar") =?>
                 "**foo**bar"
          , test asciidoc "strong quoted" $
               para (singleQuoted (strong (text "foo"))) =?>
                 "`**foo**'"
          ]
        , testGroup "tables"
          [ test asciidoc "empty cells" $
               simpleTable [] [[mempty],[mempty]] =?> unlines
                                           [ "[cols=\"\",]"
                                           , "|===="
                                           , "|"
                                           , "|"
                                           , "|===="
                                           ]
          , test asciidoc "multiblock cells" $
               simpleTable [] [[para (text "Para 1") <> para (text "Para 2")]]
                                           =?> unlines
                                           [ "[cols=\"\",]"
                                           , "|====="
                                           , "a|"
                                           , "Para 1"
                                           , ""
                                           , "Para 2"
                                           , ""
                                           , "|====="
                                           ]
          ]
        ]
