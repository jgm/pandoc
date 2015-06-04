{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Tests.Writers.Markdown (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Tests.Arbitrary()

markdown :: (ToString a, ToPandoc a) => a -> String
markdown = writeMarkdown def . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test markdown "my test" $ X =?> Y

which is in turn shorthand for

  test markdown "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> Test
(=:) = test markdown

tests :: [Test]
tests = [ "indented code after list"
             =: (orderedList [ para "one" <> para "two" ] <> codeBlock "test")
             =?> "1.  one\n\n    two\n\n<!-- -->\n\n    test"
        , "list with tight sublist"
             =: bulletList [ plain "foo" <> bulletList [ plain "bar" ],
                             plain "baz" ]
             =?> "-   foo\n    -   bar\n-   baz\n"
        ] ++ [shortcutLinkRefsTests]

shortcutLinkRefsTests :: Test
shortcutLinkRefsTests =
  let infix 4 =:
      (=:) :: (ToString a, ToPandoc a)
           => String -> (a, String) -> Test
      (=:) = test (writeMarkdown (def {writerReferenceLinks = True}) . toPandoc)
  in testGroup "Shortcut reference links"
     [ "Simple link (shortcutable)"
           =: (para (link "/url" "title" "foo"))
           =?> "[foo]\n\n  [foo]: /url \"title\""
     , "Followed by another link (unshortcutable)"
           =: (para ((link "/url1" "title1" "first")
                  <> (link "/url2" "title2" "second")))
           =?> unlines [ "[first][][second]"
                       , ""
                       , "  [first]: /url1 \"title1\""
                       , "  [second]: /url2 \"title2\""
                       ]
     , "Followed by space and another link (unshortcutable)"
           =: (para ((link "/url1" "title1" "first") <> " "
                  <> (link "/url2" "title2" "second")))
           =?> unlines [ "[first][] [second]"
                       , ""
                       , "  [first]: /url1 \"title1\""
                       , "  [second]: /url2 \"title2\""
                       ]
     , "Reference link is used multiple times (unshortcutable)"
           =: (para ((link "/url1" "" "foo") <> (link "/url2" "" "foo")
                                             <> (link "/url3" "" "foo")))
           =?> unlines [ "[foo][][foo][1][foo][2]"
                       , ""
                       , "  [foo]: /url1"
                       , "  [1]: /url2"
                       , "  [2]: /url3"
                       ]
     , "Reference link is used multiple times (unshortcutable)"
           =: (para ((link "/url1" "" "foo") <> " " <> (link "/url2" "" "foo")
                                             <> " " <> (link "/url3" "" "foo")))
           =?> unlines [ "[foo][] [foo][1] [foo][2]"
                       , ""
                       , "  [foo]: /url1"
                       , "  [1]: /url2"
                       , "  [2]: /url3"
                       ]
     , "Reference link is followed by text in brackets"
          =:  (para ((link "/url" "" "link") <> "[text in brackets]"))
          =?> unlines [ "[link][]\\[text in brackets\\]"
                      , ""
                      , "  [link]: /url"
                      ]
     , "Reference link is followed by space and text in brackets"
          =:  (para ((link "/url" "" "link") <> " [text in brackets]"))
          =?> unlines [ "[link][] \\[text in brackets\\]"
                      , ""
                      , "  [link]: /url"
                      ]
     , "Reference link is followed by RawInline"
          =: (para ((link "/url" "" "link") <> rawInline "markdown" "[rawText]"))
          =?> unlines [ "[link][][rawText]"
                      , ""
                      , "  [link]: /url"
                      ]
     , "Reference link is followed by space and RawInline"
          =: (para ((link "/url" "" "link") <> space <> rawInline "markdown" "[rawText]"))
          =?> unlines [ "[link][] [rawText]"
                      , ""
                      , "  [link]: /url"
                      ]
     , "Reference link is followed by RawInline with space"
          =: (para ((link "/url" "" "link") <> rawInline "markdown" " [rawText]"))
          =?> unlines [ "[link][] [rawText]"
                      , ""
                      , "  [link]: /url"
                      ]
     , "Reference link is followed by citation"
          =: (para ((link "/url" "" "link") <> cite [Citation "author" [] [] NormalCitation 0 0] (str "[@author]")))
          =?> unlines [ "[link][][@author]"
                      , ""
                      , "  [link]: /url"
                      ]
     , "Reference link is followed by space and citation"
          =: (para ((link "/url" "" "link") <> space <> cite [Citation "author" [] [] NormalCitation 0 0] (str "[@author]")))
          =?> unlines [ "[link][] [@author]"
                      , ""
                      , "  [link]: /url"
                      ]
     ]
