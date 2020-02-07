{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Tests.Writers.Markdown (tests) where

import Prelude
import Data.Text (unpack)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

defopts :: WriterOptions
defopts = def{ writerExtensions = pandocExtensions }

markdown :: (ToPandoc a) => a -> String
markdown = unpack . purely (writeMarkdown defopts) . toPandoc

markdownWithOpts :: (ToPandoc a) => WriterOptions -> a -> String
markdownWithOpts opts x = unpack . purely (writeMarkdown opts) $ toPandoc x

{-
  "my test" =: X =?> Y

is shorthand for

  test markdown "my test" $ X =?> Y

which is in turn shorthand for

  test markdown "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test markdown

tests :: [TestTree]
tests = [ "indented code after list"
             =: (orderedList [ para "one" <> para "two" ] <> codeBlock "test")
             =?> "1.  one\n\n    two\n\n```{=html}\n<!-- -->\n```\n    test"
        , "list with tight sublist"
             =: bulletList [ plain "foo" <> bulletList [ plain "bar" ],
                             plain "baz" ]
             =?> "-   foo\n    -   bar\n-   baz\n"
        ] ++ [noteTests] ++ [shortcutLinkRefsTests]

{-

Testing with the following text:

First Header
============

This is a footnote.[^1] And this is a [link](https://www.google.com).

> A note inside a block quote.[^2]
>
> A second paragraph.

Second Header
=============

Some more text.


[^1]: Down here.

[^2]: The second note.

-}

noteTestDoc :: Blocks
noteTestDoc =
  header 1 "First Header" <>
  para ("This is a footnote." <>
        note (para "Down here.") <>
        " And this is a " <>
        link "https://www.google.com" "" "link" <>
        ".") <>
  blockQuote (para ("A note inside a block quote." <>
                    note (para "The second note.")) <>
              para "A second paragraph.") <>
  header 1 "Second Header" <>
  para "Some more text."



noteTests :: TestTree
noteTests = testGroup "note and reference location"
  [ test (markdownWithOpts defopts)
    "footnotes at the end of a document" $
    noteTestDoc =?>
    unlines [ "First Header"
               , "============"
               , ""
               , "This is a footnote.[^1] And this is a [link](https://www.google.com)."
               , ""
               , "> A note inside a block quote.[^2]"
               , ">"
               , "> A second paragraph."
               , ""
               , "Second Header"
               , "============="
               , ""
               , "Some more text."
               , ""
               , "[^1]: Down here."
               , ""
               , "[^2]: The second note."
               ]
  , test (markdownWithOpts defopts{writerReferenceLocation=EndOfBlock})
    "footnotes at the end of blocks" $
    noteTestDoc =?>
    unlines [ "First Header"
               , "============"
               , ""
               , "This is a footnote.[^1] And this is a [link](https://www.google.com)."
               , ""
               , "[^1]: Down here."
               , ""
               , "> A note inside a block quote.[^2]"
               , ">"
               , "> A second paragraph."
               , ""
               , "[^2]: The second note."
               , ""
               , "Second Header"
               , "============="
               , ""
               , "Some more text."
               ]
  , test (markdownWithOpts defopts{writerReferenceLocation=EndOfBlock, writerReferenceLinks=True})
    "footnotes and reference links at the end of blocks" $
    noteTestDoc =?>
    unlines [ "First Header"
               , "============"
               , ""
               , "This is a footnote.[^1] And this is a [link]."
               , ""
               , "[^1]: Down here."
               , ""
               , "  [link]: https://www.google.com"
               , ""
               , "> A note inside a block quote.[^2]"
               , ">"
               , "> A second paragraph."
               , ""
               , "[^2]: The second note."
               , ""
               , "Second Header"
               , "============="
               , ""
               , "Some more text."
               ]
  , test (markdownWithOpts defopts{writerReferenceLocation=EndOfSection})
    "footnotes at the end of section" $
    noteTestDoc =?>
    unlines [ "First Header"
               , "============"
               , ""
               , "This is a footnote.[^1] And this is a [link](https://www.google.com)."
               , ""
               , "> A note inside a block quote.[^2]"
               , ">"
               , "> A second paragraph."
               , ""
               , "[^1]: Down here."
               , ""
               , "[^2]: The second note."
               , ""
               , "Second Header"
               , "============="
               , ""
               , "Some more text."
               ]

  ]

shortcutLinkRefsTests :: TestTree
shortcutLinkRefsTests =
  let infix 4 =:
      (=:) :: (ToString a, ToPandoc a)

        => String -> (a, String) -> TestTree
      (=:) = test (purely (writeMarkdown defopts{writerReferenceLinks = True}) . toPandoc)
  in testGroup "Shortcut reference links"
     [ "Simple link (shortcutable)"
           =: para (link "/url" "title" "foo")
           =?> "[foo]\n\n  [foo]: /url \"title\""
     , "Followed by another link (unshortcutable)"
           =: para (link "/url1" "title1" "first"
                  <> link "/url2" "title2" "second")
           =?> unlines [ "[first][][second]"
                       , ""
                       , "  [first]: /url1 \"title1\""
                       , "  [second]: /url2 \"title2\""
                       ]
     , "Followed by space and another link (unshortcutable)"
           =: para (link "/url1" "title1" "first" <> " "
                  <> link "/url2" "title2" "second")
           =?> unlines [ "[first][] [second]"
                       , ""
                       , "  [first]: /url1 \"title1\""
                       , "  [second]: /url2 \"title2\""
                       ]
     , "Reference link is used multiple times (unshortcutable)"
           =: para (link "/url1" "" "foo" <> link "/url2" "" "foo"
                                             <> link "/url3" "" "foo")
           =?> unlines [ "[foo][][foo][1][foo][2]"
                       , ""
                       , "  [foo]: /url1"
                       , "  [1]: /url2"
                       , "  [2]: /url3"
                       ]
     , "Reference link is used multiple times (unshortcutable)"
           =: para (link "/url1" "" "foo" <> " " <> link "/url2" "" "foo"
                                             <> " " <> link "/url3" "" "foo")
           =?> unlines [ "[foo][] [foo][1] [foo][2]"
                       , ""
                       , "  [foo]: /url1"
                       , "  [1]: /url2"
                       , "  [2]: /url3"
                       ]
     , "Reference link is followed by text in brackets"
          =:  para (link "/url" "" "link" <> "[text in brackets]")
          =?> unlines [ "[link][]\\[text in brackets\\]"
                      , ""
                      , "  [link]: /url"
                      ]
     , "Reference link is followed by space and text in brackets"
          =:  para (link "/url" "" "link" <> " [text in brackets]")
          =?> unlines [ "[link][] \\[text in brackets\\]"
                      , ""
                      , "  [link]: /url"
                      ]
     , "Reference link is followed by RawInline"
          =: para (link "/url" "" "link" <> rawInline "markdown" "[rawText]")
          =?> unlines [ "[link][][rawText]"
                      , ""
                      , "  [link]: /url"
                      ]
     , "Reference link is followed by space and RawInline"
          =: para (link "/url" "" "link" <> space <> rawInline "markdown" "[rawText]")
          =?> unlines [ "[link][] [rawText]"
                      , ""
                      , "  [link]: /url"
                      ]
     , "Reference link is followed by RawInline with space"
          =: para (link "/url" "" "link" <> rawInline "markdown" " [rawText]")
          =?> unlines [ "[link][] [rawText]"
                      , ""
                      , "  [link]: /url"
                      ]
     , "Reference link is followed by citation"
          =: para (link "/url" "" "link" <> cite [Citation "author" [] [] NormalCitation 0 0] (str "[@author]"))
          =?> unlines [ "[link][][@author]"
                      , ""
                      , "  [link]: /url"
                      ]
     , "Reference link is followed by space and citation"
          =: para (link "/url" "" "link" <> space <> cite [Citation "author" [] [] NormalCitation 0 0] (str "[@author]"))
          =?> unlines [ "[link][] [@author]"
                      , ""
                      , "  [link]: /url"
                      ]
     ]
