{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.JATS (tests) where

import Prelude
import Data.Text (unpack)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

jats :: (ToPandoc a) => a -> String
jats = unpack
  . purely (writeJATS def{ writerWrapText = WrapNone })
  . toPandoc

jatsArticleAuthoring :: (ToPandoc a) => a -> String
jatsArticleAuthoring = unpack
  . purely (writeJatsArticleAuthoring def{ writerWrapText = WrapNone })
  . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test jats "my test" $ X =?> Y

which is in turn shorthand for

  test jats "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test jats

tests :: [TestTree]
tests = [ testGroup "inline code"
          [ "basic" =: code "@&" =?> "<p><monospace>@&amp;</monospace></p>"
          , "lang" =: codeWith ("", ["c"], []) "@&" =?> "<p><code language=\"c\">@&amp;</code></p>"
          ]
        , testGroup "block code"
          [ "basic" =: codeBlock "@&" =?> "<preformat>@&amp;</preformat>"
          , "lang" =: codeBlockWith ("", ["c"], []) "@&" =?> "<code language=\"c\">@&amp;</code>"
          ]
        , testGroup "images"
          [ "basic" =:
            image "/url" "title" mempty
            =?> "<graphic mimetype=\"image\" mime-subtype=\"\" xlink:href=\"/url\" xlink:title=\"title\" />"
          ]
        , testGroup "inlines"
          [ "Emphasis" =: emph "emphasized"
            =?> "<p><italic>emphasized</italic></p>"

          , test jatsArticleAuthoring "footnote in articleauthoring tag set"
            ("test" <> note (para "footnote") =?>
             unlines [ "<p>test<fn>"
                     , "  <p>footnote</p>"
                     , "</fn></p>"
                     ])
          ]
        , "bullet list" =: bulletList [ plain $ text "first"
                                      , plain $ text "second"
                                      , plain $ text "third"
                                      ]
            =?> "<list list-type=\"bullet\">\n\
                \  <list-item>\n\
                \    <p>first</p>\n\
                \  </list-item>\n\
                \  <list-item>\n\
                \    <p>second</p>\n\
                \  </list-item>\n\
                \  <list-item>\n\
                \    <p>third</p>\n\
                \  </list-item>\n\
                \</list>"
        , testGroup "definition lists"
          [ "with internal link" =: definitionList [(link "#go" "" (str "testing"),
             [plain (text "hi there")])] =?>
            "<def-list>\n\
            \  <def-item>\n\
            \    <term><xref alt=\"testing\" rid=\"go\">testing</xref></term>\n\
            \    <def>\n\
            \      <p>hi there</p>\n\
            \    </def>\n\
            \  </def-item>\n\
            \</def-list>"
          ]
        , testGroup "math"
          [ "escape |" =: para (math "\\sigma|_{\\{x\\}}") =?>
            "<p><inline-formula><alternatives>\n\
            \<tex-math><![CDATA[\\sigma|_{\\{x\\}}]]></tex-math>\n\
            \<mml:math display=\"inline\" xmlns:mml=\"http://www.w3.org/1998/Math/MathML\"><mml:mrow><mml:mi>σ</mml:mi><mml:msub><mml:mo stretchy=\"false\" form=\"prefix\">|</mml:mo><mml:mrow><mml:mo stretchy=\"false\" form=\"prefix\">{</mml:mo><mml:mi>x</mml:mi><mml:mo stretchy=\"false\" form=\"postfix\">}</mml:mo></mml:mrow></mml:msub></mml:mrow></mml:math></alternatives></inline-formula></p>"
          ]
        , testGroup "headers"
          [ "unnumbered header" =:
            headerWith ("foo",["unnumbered"],[]) 1
              (text "Header 1" <> note (plain $ text "note")) =?>
            "<sec id=\"foo\">\n\
            \  <title>Header 1<xref ref-type=\"fn\" rid=\"fn1\">1</xref></title>\n\
            \</sec>"
          , "unnumbered sub header" =:
            headerWith ("foo",["unnumbered"],[]) 1
              (text "Header")
            <> headerWith ("foo",["unnumbered"],[]) 2
              (text "Sub-Header") =?>
            "<sec id=\"foo\">\n\
            \  <title>Header</title>\n\
            \  <sec id=\"foo\">\n\
            \    <title>Sub-Header</title>\n\
            \  </sec>\n\
            \</sec>"
          , "containing image" =:
            header 1 (image "imgs/foo.jpg" "" (text "Alt text")) =?>
            "<sec>\n\
            \  <title><inline-graphic mimetype=\"image\" mime-subtype=\"jpeg\" xlink:href=\"imgs/foo.jpg\" /></title>\n\
            \</sec>"
          ]
        ]
