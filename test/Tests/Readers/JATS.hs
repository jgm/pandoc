{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.JATS
   Copyright   : © 2017 Hamish Mackenzie
   License     : GNU GPL, version 2 or above

   Maintainer  : Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
   Stability   : alpha
   Portability : portable

Tests for the JATS reader.
-}
module Tests.Readers.JATS (tests) where

import Prelude
import Data.Text (Text)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

import qualified Data.Text as T

jats :: Text -> Pandoc
jats = purely $ readJATS def

tests :: [TestTree]
tests = [ testGroup "inline code"
          [ test jats "basic" $ "<p>\n  <monospace>@&amp;</monospace>\n</p>" =?> para (code "@&")
          , test jats "lang" $ "<p>\n  <code language=\"c\">@&amp;</code>\n</p>" =?> para (codeWith ("", ["c"], []) "@&")
          ]
        , testGroup "block code"
          [ test jats "basic" $ "<preformat>@&amp;</preformat>" =?> codeBlock "@&"
          , test jats "lang" $ "<code language=\"c\">@&amp;</code>" =?> codeBlockWith ("", ["c"], []) "@&"
          ]
        , testGroup "images"
          [ test jats "basic" $ "<graphic mimetype=\"image\" mime-subtype=\"\" xlink:href=\"/url\" xlink:title=\"title\" />"
            =?> para (image "/url" "title" mempty)
          ]
        , test jats "bullet list" $
                            "<list list-type=\"bullet\">\n\
                            \  <list-item>\n\
                            \    <p>\n\
                            \      first\n\
                            \    </p>\n\
                            \  </list-item>\n\
                            \  <list-item>\n\
                            \    <p>\n\
                            \      second\n\
                            \    </p>\n\
                            \  </list-item>\n\
                            \  <list-item>\n\
                            \    <p>\n\
                            \      third\n\
                            \    </p>\n\
                            \  </list-item>\n\
                            \</list>"
                =?> bulletList [ para $ text "first"
                               , para $ text "second"
                               , para $ text "third"
                               ]
        , testGroup "definition lists"
          [ test jats "with internal link" $
            "<def-list>\n\
            \  <def-item>\n\
            \    <term>\n\
            \      <xref alt=\"testing\" rid=\"go\">testing</xref>\n\
            \    </term>\n\
            \    <def>\n\
            \      <p>\n\
            \        hi there\n\
            \      </p>\n\
            \    </def>\n\
            \  </def-item>\n\
            \</def-list>"
            =?> definitionList [(link "#go" "" (str "testing"),
                [para (text "hi there")])]
          ]
        , testGroup "math"
          [ test jats "escape |" $
            "<p>\n\
            \  <inline-formula><alternatives>\n\
            \  <tex-math><![CDATA[\\sigma|_{\\{x\\}}]]></tex-math>\n\
            \  <mml:math display=\"inline\" xmlns:mml=\"http://www.w3.org/1998/Math/MathML\"><mml:mrow><mml:mi>σ</mml:mi><mml:msub><mml:mo stretchy=\"false\" form=\"prefix\">|</mml:mo><mml:mrow><mml:mo stretchy=\"false\" form=\"prefix\">{</mml:mo><mml:mi>x</mml:mi><mml:mo stretchy=\"false\" form=\"postfix\">}</mml:mo></mml:mrow></mml:msub></mml:mrow></mml:math></alternatives></inline-formula>\n\
            \</p>"
            =?> para (math "\\sigma|_{\\{x\\}}")
          , test jats "tex-math only" $
            "<p>\n\
            \  <inline-formula><alternatives>\n\
            \  <tex-math><![CDATA[\\sigma|_{\\{x\\}}]]></tex-math>\n\
            \</p>"
            =?> para (math "\\sigma|_{\\{x\\}}")
          , test jats "math ml only" $
            "<p>\n\
            \  <inline-formula><alternatives>\n\
            \  <mml:math display=\"inline\" xmlns:mml=\"http://www.w3.org/1998/Math/MathML\"><mml:mrow><mml:mi>σ</mml:mi><mml:msub><mml:mo stretchy=\"false\" form=\"prefix\">|</mml:mo><mml:mrow><mml:mo stretchy=\"false\" form=\"prefix\">{</mml:mo><mml:mi>x</mml:mi><mml:mo stretchy=\"false\" form=\"postfix\">}</mml:mo></mml:mrow></mml:msub></mml:mrow></mml:math></alternatives></inline-formula>\n\
            \</p>"
            =?> para (math "\\sigma|_{\\{ x\\}}")
          ]
        , testGroup "headers"
-- TODO fix footnotes in headers
--          [ test jats "unnumbered header" $
--            "<sec>\n\
--            \  <title>Header 1<fn>\n\
--            \    <p>\n\
--            \      note\n\
--            \    </p>\n\
--            \  </fn></title>\n\
--            \</sec>"
--            =?> header 1
--                (text "Header 1" <> note (plain $ text "note"))
          [ test jats "unnumbered sub header" $
            "<sec id=\"foo\">\n\
            \  <title>Header</title>\n\
            \  <sec id=\"foo2\">\n\
            \    <title>Sub-Header</title>\n\
            \  </sec>\n\
            \</sec>"
            =?> headerWith ("foo", [], []) 1
                  (text "Header")
                <> headerWith  ("foo2", [], []) 2
                  (text "Sub-Header")
          , test jats "containing image" $
            "<sec>\n\
            \  <title><inline-graphic mimetype=\"image\" mime-subtype=\"jpeg\" xlink:href=\"imgs/foo.jpg\" /></title>\n\
            \</sec>"
            =?> header 1 (image "imgs/foo.jpg" "" mempty)
          ]

        , testGroup "metadata"
          [ test jats "abstract" $
            T.unlines [ "<front>"
                      , "<article-meta>"
                      , "<abstract>"
                      , "<p>Paragraph 1</p>"
                      , "<p>Paragraph 2</p>"
                      , "</abstract>"
                      , "</article-meta>"
                      , "</front>"
                      ] =?>
            let abstract = para "Paragraph 1" <> para "Paragraph 2"
            in setMeta "abstract" abstract $ doc mempty
          ]
        ]
