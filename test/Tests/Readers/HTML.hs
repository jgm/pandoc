{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{- |
   Module      : Tests.Readers.HTML
   Copyright   : © 2006-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Tests for the HTML reader.
-}
module Tests.Readers.HTML (tests) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.QuickCheck hiding (orderedList)
import Test.Tasty.Options (IsOption(defaultValue))
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Shared (isHeaderBlock)
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder
import Text.Pandoc.Walk (walk)

html :: Text -> Pandoc
html = purely $ readHtml def

htmlNativeDivs :: Text -> Pandoc
htmlNativeDivs = purely $ readHtml def { readerExtensions = enableExtension Ext_native_divs $ readerExtensions def }

makeRoundTrip :: Block -> Block
makeRoundTrip CodeBlock{} = Para [Str "code block was here"]
makeRoundTrip LineBlock{} = Para [Str "line block was here"]
makeRoundTrip RawBlock{} = Para [Str "raw block was here"]
makeRoundTrip (Div attr bs) = Div attr $ filter (not . isHeaderBlock) bs
-- avoids round-trip failures related to makeSections
-- e.g. with [Div ("loc",[],[("a","11"),("b_2","a b c")]) [Header 3 ("",[],[]) []]]
makeRoundTrip Table{} = Para [Str "table block was here"]
makeRoundTrip x           = x

removeRawInlines :: Inline -> Inline
removeRawInlines RawInline{} = Str "raw inline was here"
removeRawInlines x           = x

roundTrip :: Blocks -> Bool
roundTrip b = d'' == d'''
  where d = walk removeRawInlines $
            walk makeRoundTrip $ Pandoc nullMeta $ toList b
        d' = rewrite d
        d'' = rewrite d'
        d''' = rewrite d''
        rewrite = html . (`T.snoc` '\n') .
                  purely (writeHtml5String def
                            { writerWrapText = WrapPreserve })

tests :: [TestTree]
tests = [ testGroup "footnotes"
          [ test html "bullet list and backlink" $
              "<p><a href=\"#fn1\" role=\"doc-noteref\">1</a></p>\
              \<section role=\"doc-endnotes\"><hr><ol><li id=\"fn1\">\
              \<ul><li><p>one</p></li><li><p>two</p></li></ul>\
              \<a href=\"#ref1\" role=\"doc-backlink\">back</a>\
              \</li></ol></section>" =?>
              para (note (bulletList [para "one", para "two"]))
          , test html "ordered list (#11852)" $
              "<p><a href=\"#fn1\" role=\"doc-noteref\">1</a></p>\
              \<section role=\"doc-endnotes\"><hr><ol><li id=\"fn1\">\
              \<ol><li><p>one</p></li><li><p>two</p></li></ol>\
              \<a href=\"#ref1\" role=\"doc-backlink\">back</a>\
              \</li></ol></section>" =?>
              para (note (orderedList [para "one", para "two"]))
          , test html "nested ordered and bullet lists (#11852)" $
              "<p><a href=\"#fn1\" role=\"doc-noteref\">1</a></p>\
              \<section role=\"doc-endnotes\"><ol><li id=\"fn1\">\
              \<ol start=\"3\" type=\"I\"><li><p>one</p>\
              \<ul><li><p>two</p><ol start=\"2\" type=\"a\">\
              \<li><p>three</p></li></ol></li></ul></li></ol>\
              \<a href=\"#ref1\" role=\"doc-backlink\">back</a>\
              \</li></ol></section>" =?>
              para (note (orderedListWith (3, UpperRoman, DefaultDelim)
                [para "one" <> bulletList
                  [para "two" <>
                   orderedListWith (2, LowerAlpha, DefaultDelim)
                     [para "three"]]]))
          , test html "multiple notes and following list (#11852)" $
              "<p><a href=\"#fn1\" role=\"doc-noteref\">1</a>\
              \<a href=\"#fn2\" role=\"doc-noteref\">2</a>\
              \<a href=\"#fn3\" role=\"doc-noteref\">3</a></p>\
              \<section role=\"doc-endnotes\"><hr><ol><li id=\"fn1\">\
              \<ol><li><p>one</p></li></ol>\
              \<a href=\"#ref1\" role=\"doc-backlink\">back</a></li>\
              \<li id=\"fn2\"><ul><li><p>two</p><ol>\
              \<li><p>three</p></li></ol></li></ul>\
              \<a href=\"#ref2\" role=\"doc-backlink\">back</a></li>\
              \<li id=\"fn3\"><p>four\
              \<a href=\"#ref3\" role=\"doc-backlink\">back</a></p>\
              \</li></ol></section><ol><li><p>five</p></li></ol>" =?>
              para (note (orderedList [para "one"]) <>
                    note (bulletList
                      [para "two" <> orderedList [para "three"]]) <>
                    note (para "four")) <>
              orderedList [para "five"]
          , test (purely (readHtml def
                    { readerExtensions = enableExtension Ext_epub_html_exts
                        (readerExtensions def) }) :: Text -> Pandoc)
              "EPUB note contents (#11852)" $
              "<p><a href=\"#fn1\" epub:type=\"noteref\">1</a></p>\
              \<section epub:type=\"footnotes\">\
              \<aside id=\"fn1\" epub:type=\"footnote\">\
              \<ol><li><p>one</p></li></ol>\
              \<a href=\"#ref1\" role=\"doc-backlink\">back</a>\
              \</aside></section>" =?>
              para (note (orderedList [para "one"]))
          ]
        , testGroup "math groups and comments"
          [ testGroup name
            [ test reader "nested math in a color box" $
                wrap "\\colorbox{aqua}{$F=ma$}" =?>
                  para (makeMath "\\colorbox{aqua}{$F=ma$}")
            , test reader "unpaired escaped opening brace" $
                wrap "{\\{x}" =?> para (makeMath "{\\{x}")
            , test reader "unpaired escaped closing brace" $
                wrap escapedClosing =?> para (makeMath escapedClosing)
            , test reader "commented delimiters and braces" $
                wrap commented =?> para (makeMath commented)
            , test reader "commented closing brace inside a group" $
                wrap ("{x% } " <> close <> "\ny}") =?>
                  para (makeMath $ "{x% } " <> close <> "\ny}")
            , test reader "commented opening brace inside a group" $
                wrap "{x% {\ny}" =?> para (makeMath "{x% {\ny}")
            , test reader "escaped percent is literal" $
                wrap "x\\%" =?> para (makeMath "x\\%")
            , test reader "percent after an escaped backslash is a comment" $
                wrap ("x\\\\% " <> close <> "\ny") =?>
                  para (makeMath $ "x\\\\% " <> close <> "\ny")
            ]
          | (name, ext, open, close, makeMath) <-
              [ ("dollars", Ext_tex_math_dollars, "$", "$", math)
              , ("double dollars", Ext_tex_math_dollars,
                 "$$", "$$", displayMath)
              , ("parentheses", Ext_tex_math_single_backslash,
                 "\\(", "\\)", math)
              , ("brackets", Ext_tex_math_single_backslash,
                 "\\[", "\\]", displayMath)
              , ("double-backslash parentheses", Ext_tex_math_double_backslash,
                 "\\\\(", "\\\\)", math)
              , ("double-backslash brackets", Ext_tex_math_double_backslash,
                 "\\\\[", "\\\\]", displayMath)
              ]
          , let reader = purely $ readHtml def
                  { readerExtensions = extensionsFromList [ext] }
                wrap s = "<p>" <> open <> s <> close <> "</p>"
                escapedClosing = "{\\} hi " <> open <> "x" <> close <> " bye}"
                commented = "x% " <> close <> " { } \\%\ny"
          ]
        , testGroup "base tag"
          [ test html "simple" $
            "<head><base href=\"http://www.w3schools.com/images/foo\" ></head><body><img src=\"stickman.gif\" alt=\"Stickman\"></head>" =?>
            plain (image "http://www.w3schools.com/images/stickman.gif" "" (text "Stickman"))
          , test html "slash at end of base" $
            "<head><base href=\"http://www.w3schools.com/images/\" ></head><body><img src=\"stickman.gif\" alt=\"Stickman\"></head>" =?>
            plain (image "http://www.w3schools.com/images/stickman.gif" "" (text "Stickman"))
          , test html "slash at beginning of href" $
            "<head><base href=\"http://www.w3schools.com/images/\" ></head><body><img src=\"/stickman.gif\" alt=\"Stickman\"></head>" =?>
            plain (image "http://www.w3schools.com/stickman.gif" "" (text "Stickman"))
          , test html "absolute URL" $
            "<head><base href=\"http://www.w3schools.com/images/\" ></head><body><img src=\"http://example.com/stickman.gif\" alt=\"Stickman\"></head>" =?>
            plain (image "http://example.com/stickman.gif" "" (text "Stickman"))
          ]
        , testGroup "anchors"
          [ test html "anchor without href" $ "<a name=\"anchor\"/>" =?>
            plain (spanWith ("anchor",[],[]) mempty)
          ]
        , testGroup "img"
          [ test html "data-external attribute" $ "<img data-external=\"1\" src=\"http://example.com/stickman.gif\">" =?>
            plain (imageWith ("", [], [("external", "1")]) "http://example.com/stickman.gif" "" "")
          , test html "title" $ "<img title=\"The title\" src=\"http://example.com/stickman.gif\">" =?>
            plain (imageWith ("", [], []) "http://example.com/stickman.gif" "The title" "")
          ]
        , testGroup "lang"
          [ test html "lang on <html>" $ "<html lang=\"es\">hola" =?>
            setMeta "lang" (text "es") (doc (plain (text "hola")))
          , test html "xml:lang on <html>" $ "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"es\"><head></head><body>hola</body></html>" =?>
            setMeta "lang" (text "es") (doc (plain (text "hola")))
          ]
        , testGroup "main"
          [ test htmlNativeDivs "<main> contents are parsed" $ "<header>ignore me</header><nav><p>ignore me</p><main>hello</main><footer>ignore me</footer>" =?>
            doc (plain (text "hello"))
          , test htmlNativeDivs "<main role=X> becomes <div role=X>" $ "<main role=foobar>hello</main>" =?>
            doc (divWith ("", [], [("role", "foobar")]) (plain (text "hello")))
          , test htmlNativeDivs "<main> has attributes preserved" $ "<main id=foo class=bar data-baz=qux>hello</main>" =?>
            doc (divWith ("foo", ["bar"], [("role", "main"), ("baz", "qux")]) (plain (text "hello")))
          , test htmlNativeDivs "<main> closes <p>" $ "<p>hello<main>main content</main>" =?>
            doc (plain (text "main content"))
          , test htmlNativeDivs "<main> followed by text" $ "<main>main content</main>non-main content" =?>
            doc (plain (text "main content"))
          ]
        , testGroup "code"
          [
            test html "inline code block" $
            "<code>Answer is 42</code>" =?>
            plain (codeWith ("",[],[]) "Answer is 42")
          ]
        , testGroup "tt"
          [
            test html "inline tt block" $
            "<tt>Answer is 42</tt>" =?>
            plain (codeWith ("",[],[]) "Answer is 42")
          ]
        , testGroup "samp"
          [
            test html "inline samp block" $
            "<samp>Answer is 42</samp>" =?>
            plain (codeWith ("",["sample"],[]) "Answer is 42")
          ]
        , testGroup "var"
          [ test html "inline var block" $
            "<var>result</var>" =?>
            plain (codeWith ("",["variable"],[]) "result")
          ]
        , testGroup "header"
          [ test htmlNativeDivs "<header> is parsed as a div" $
            "<header id=\"title\">Title</header>" =?>
            divWith ("title", ["header"], mempty) (plain "Title")
          ]
        , testGroup "code block"
          [ test html "attributes in pre > code element" $
            "<pre><code id=\"a\" class=\"python\">\nprint('hi')\n</code></pre>"
            =?>
            codeBlockWith ("a", ["python"], []) "\nprint('hi')"

          , test html "attributes in pre take precedence" $
            "<pre id=\"c\"><code id=\"d\">print('hi mom!')\n</code></pre>"
            =?>
            codeBlockWith ("c", [], []) "print('hi mom!')"
          ]
        , askOption $ \(QuickCheckTests numtests) ->
            testProperty "Round trip" $
              withMaxSuccess (if QuickCheckTests numtests == defaultValue
                                 then 25
                                 else numtests) roundTrip
        ]
