{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.HTML (tests) where

import Data.Text (unpack)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit (HasCallStack)
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

htmlWithOpts :: (ToPandoc a) => WriterOptions -> a -> String
htmlWithOpts opts = unpack . purely (writeHtml4String opts{ writerWrapText = WrapNone }) . toPandoc

html :: (ToPandoc a) => a -> String
html = htmlWithOpts def

htmlQTags :: (ToPandoc a) => a -> String
htmlQTags = unpack
  . purely (writeHtml4String def{ writerWrapText = WrapNone, writerHtmlQTags = True })
  . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test html "my test" $ X =?> Y

which is in turn shorthand for

  test html "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a, HasCallStack)
     => String -> (a, String) -> TestTree
(=:) = test html

noteTestDoc :: Blocks
noteTestDoc =
  header 1 "Page title" <>
  header 2 "First section" <>
  para ("This is a footnote." <>
        note (para "Down here.") <>
        " And this is a " <>
        link "https://www.google.com" "" "link" <>
        ".") <>
  blockQuote (para ("A note inside a block quote." <>
                    note (para "The second note.")) <>
              para "A second paragraph.") <>
  header 2 "Second section" <>
  para "Some more text."

tests :: [TestTree]
tests =
  [ testGroup "inline code"
    [ "basic" =: code "@&" =?> "<code>@&amp;</code>"
    , "haskell" =: codeWith ("",["haskell"],[]) ">>="
      =?> "<code class=\"sourceCode haskell\"><span class=\"op\">&gt;&gt;=</span></code>"
    , "nolanguage" =: codeWith ("",["nolanguage"],[]) ">>="
      =?> "<code class=\"nolanguage\">&gt;&gt;=</code>"
    ]
  , testGroup "images"
    [ "alt with formatting" =:
      image "/url" "title" ("my " <> emph "image")
      =?> "<img src=\"/url\" title=\"title\" alt=\"my image\" />"
    ]
  , testGroup "blocks"
    [ "definition list with empty <dt>" =:
      definitionList [(mempty, [para $ text "foo bar"])]
      =?> "<dl>\n<dt></dt>\n<dd>\n<p>foo bar</p>\n</dd>\n</dl>"
    , "heading with disallowed attributes" =:
      headerWith ("", [], [("invalid","1"), ("lang", "en")]) 1 "test"
      =?>
      "<h1 lang=\"en\">test</h1>"
    ]
  , testGroup "quotes"
    [ "quote with cite attribute (without q-tags)" =:
      doubleQuoted (spanWith ("", [], [("cite", "http://example.org")]) (str "examples"))
      =?> "“<span cite=\"http://example.org\">examples</span>”"
    , tQ "quote with cite attribute (with q-tags)" $
      doubleQuoted (spanWith ("", [], [("cite", "http://example.org")]) (str "examples"))
      =?> "<q cite=\"http://example.org\">examples</q>"
    ]
  , testGroup "sample"
    [ "sample should be rendered correctly" =:
      plain (codeWith ("",["sample"],[]) "Answer is 42") =?>
      "<samp>Answer is 42</samp>"
    ]
  , testGroup "variable"
    [ "variable should be rendered correctly" =:
      plain (codeWith ("",["variable"],[]) "result") =?>
      "<var>result</var>"
    ]
  , testGroup "sample with style"
    [ "samp should wrap highlighted code" =:
      codeWith ("",["sample","haskell"],[]) ">>="
      =?> ("<samp><code class=\"sourceCode haskell\">" ++
          "<span class=\"op\">&gt;&gt;=</span></code></samp>")
    ]
  , testGroup "variable with style"
    [ "var should wrap highlighted code" =:
      codeWith ("",["haskell","variable"],[]) ">>="
      =?> ("<var><code class=\"sourceCode haskell\">" ++
          "<span class=\"op\">&gt;&gt;=</span></code></var>")
    ]
  , testGroup "footnotes"
      [ test (htmlWithOpts def{writerReferenceLocation=EndOfDocument})
        "at the end of a document" $
        noteTestDoc =?>
        T.unlines
          [ "<h1>Page title</h1>"
          , "<h2>First section</h2>"
          , "<p>This is a footnote.<a href=\"#fn1\" class=\"footnote-ref\" id=\"fnref1\"><sup>1</sup></a> And this is a <a href=\"https://www.google.com\">link</a>.</p>"
          , "<blockquote>"
          , "<p>A note inside a block quote.<a href=\"#fn2\" class=\"footnote-ref\" id=\"fnref2\"><sup>2</sup></a></p>"
          , "<p>A second paragraph.</p>"
          , "</blockquote>"
          , "<h2>Second section</h2>"
          , "<p>Some more text.</p>"
          , "<div class=\"footnotes footnotes-end-of-document\">"
          , "<hr />"
          , "<ol>"
          , "<li id=\"fn1\"><p>Down here.<a href=\"#fnref1\" class=\"footnote-back\">↩︎</a></p></li>"
          , "<li id=\"fn2\"><p>The second note.<a href=\"#fnref2\" class=\"footnote-back\">↩︎</a></p></li>"
          , "</ol>"
          , "</div>"
          ]
      , test (htmlWithOpts def{writerReferenceLocation=EndOfBlock})
        "at the end of a block" $
        noteTestDoc =?>
        T.unlines
          [ "<h1>Page title</h1>"
          , "<h2>First section</h2>"
          , "<p>This is a footnote.<a href=\"#fn1\" class=\"footnote-ref\" id=\"fnref1\"><sup>1</sup></a> And this is a <a href=\"https://www.google.com\">link</a>.</p>"
          , "<div class=\"footnotes footnotes-end-of-block\">"
          , "<ol>"
          , "<li id=\"fn1\"><p>Down here.<a href=\"#fnref1\" class=\"footnote-back\">↩︎</a></p></li>"
          , "</ol>"
          , "</div>"
          , "<blockquote>"
          , "<p>A note inside a block quote.<a href=\"#fn2\" class=\"footnote-ref\" id=\"fnref2\"><sup>2</sup></a></p>"
          , "<p>A second paragraph.</p>"
          , "</blockquote>"
          , "<div class=\"footnotes footnotes-end-of-block\">"
          , "<ol start=\"2\">"
          , "<li id=\"fn2\"><p>The second note.<a href=\"#fnref2\" class=\"footnote-back\">↩︎</a></p></li>"
          , "</ol>"
          , "</div>"
          , "<h2>Second section</h2>"
          , "<p>Some more text.</p>"
          ]
      , test (htmlWithOpts def{writerReferenceLocation=EndOfSection})
        "at the end of a section" $
        noteTestDoc =?>
        T.unlines
          [ "<h1>Page title</h1>"
          , "<h2>First section</h2>"
          , "<p>This is a footnote.<a href=\"#fn1\" class=\"footnote-ref\" id=\"fnref1\"><sup>1</sup></a> And this is a <a href=\"https://www.google.com\">link</a>.</p>"
          , "<blockquote>"
          , "<p>A note inside a block quote.<a href=\"#fn2\" class=\"footnote-ref\" id=\"fnref2\"><sup>2</sup></a></p>"
          , "<p>A second paragraph.</p>"
          , "</blockquote>"
          , "<div class=\"footnotes footnotes-end-of-section\">"
          , "<hr />"
          , "<ol>"
          , "<li id=\"fn1\"><p>Down here.<a href=\"#fnref1\" class=\"footnote-back\">↩︎</a></p></li>"
          , "<li id=\"fn2\"><p>The second note.<a href=\"#fnref2\" class=\"footnote-back\">↩︎</a></p></li>"
          , "</ol>"
          , "</div>"
          , "<h2>Second section</h2>"
          , "<p>Some more text.</p>"
          ]
      , test (htmlWithOpts def{writerReferenceLocation=EndOfSection, writerSectionDivs=True})
        "at the end of a section, with section divs" $
        noteTestDoc =?>
        -- Footnotes are rendered _after_ their section (in this case after the level2 section
        -- that contains it).
        T.unlines
          [ "<div class=\"section level1\">"
          , "<h1>Page title</h1>"
          , "<div class=\"section level2\">"
          , "<h2>First section</h2>"
          , "<p>This is a footnote.<a href=\"#fn1\" class=\"footnote-ref\" id=\"fnref1\"><sup>1</sup></a> And this is a <a href=\"https://www.google.com\">link</a>.</p>"
          , "<blockquote>"
          , "<p>A note inside a block quote.<a href=\"#fn2\" class=\"footnote-ref\" id=\"fnref2\"><sup>2</sup></a></p>"
          , "<p>A second paragraph.</p>"
          , "</blockquote>"
          , "</div>"
          , "<div class=\"footnotes footnotes-end-of-section\">"
          , "<hr />"
          , "<ol>"
          , "<li id=\"fn1\"><p>Down here.<a href=\"#fnref1\" class=\"footnote-back\">↩︎</a></p></li>"
          , "<li id=\"fn2\"><p>The second note.<a href=\"#fnref2\" class=\"footnote-back\">↩︎</a></p></li>"
          , "</ol>"
          , "</div>"
          , "<div class=\"section level2\">"
          , "<h2>Second section</h2>"
          , "<p>Some more text.</p>"
          , "</div>"
          , "</div>"
          ]
      ]
  ]
  where
    tQ :: (ToString a, ToPandoc a)
         => String -> (a, String) -> TestTree
    tQ = test htmlQTags
