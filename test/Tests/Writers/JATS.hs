{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.JATS (tests) where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit (HasCallStack)
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder
import qualified Data.Text as T

jats :: (ToPandoc a) => a -> Text
jats = purely (writeJATS def{ writerWrapText = WrapNone })
     . toPandoc

jatsArticleAuthoring :: (ToPandoc a) => a -> Text
jatsArticleAuthoring =
    purely (writeJatsArticleAuthoring def{ writerWrapText = WrapNone })
  . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test jats "my test" $ X =?> Y

which is in turn shorthand for

  test jats "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a, HasCallStack)
     => String -> (a, Text) -> TestTree
(=:) = test jats

tests :: [TestTree]
tests =
  [ testGroup "inline code"
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
  , testGroup "bullet list"
    [ "plain items" =: bulletList [ plain $ text "first"
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

    , "item with implicit figure" =:
      bulletList [ simpleFigure (text "caption") "a.png" "" ] =?>
      T.unlines
        [ "<list list-type=\"bullet\">"
        , "  <list-item>"
        , "    <p specific-use=\"wrapper\">"
        , "      <fig>"
        , "        <caption><p>caption</p></caption>"
        , "        <graphic mimetype=\"image\" mime-subtype=\"png\"" <>
          " xlink:href=\"a.png\" xlink:title=\"\" />"
        , "      </fig>"
        , "    </p>"
        , "  </list-item>"
        , "</list>"
        ]
    ]
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

  , testGroup "ids"
    [ "non-ASCII in header ID" =:
      headerWith ("smørbrød",[],[]) 1 (text "smørbrød") =?>
      T.unlines [ "<sec id=\"smørbrød\">"
                , "  <title>smørbrød</title>"
                , "</sec>"
                ]

    , "disallowed symbol in header id" =:
      headerWith ("i/o",[],[]) 1 (text "I/O") =?>
      T.unlines [ "<sec id=\"iU002Fo\">"
                , "  <title>I/O</title>"
                , "</sec>"
                ]

    , "disallowed symbols in internal link target" =:
      link "#foo:bar" "" "baz" =?>
      "<p><xref alt=\"baz\" rid=\"fooU003Abar\">baz</xref></p>"

    , "code id starting with a number" =:
      codeWith ("7y",[],[]) "print 5" =?>
      "<p><monospace id=\"U0037y\">print 5</monospace></p>"
    ]

  , testGroup "spans"
    [ "unwrapped if no attributes given" =:
      spanWith nullAttr "text in span" =?>
      "<p>text in span</p>"

    , "converted to named-content element if class given" =:
      spanWith ("a", ["genus-species"], [("alt", "aa")]) "C. elegans" =?>
      ("<p><named-content id=\"a\" alt=\"aa\" content-type=\"genus-species\">"
       <> "C. elegans</named-content></p>")

    , "unwrapped if styled-content element would have no attributes" =:
      spanWith ("", [], [("hidden", "true")]) "text in span" =?>
      "<p>text in span</p>"

    , "use content-type attribute if present" =:
      spanWith ("", [], [("content-type", "species")]) "E. coli" =?>
      "<p><named-content content-type=\"species\">E. coli</named-content></p>"
    ]
  ]
