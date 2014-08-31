{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.Markdown (tests) where

import Text.Pandoc.Definition
import Test.Framework
import Tests.Helpers
import Tests.Arbitrary()
import Text.Pandoc.Builder
import qualified Data.Set as Set
-- import Text.Pandoc.Shared ( normalize )
import Text.Pandoc

markdown :: String -> Pandoc
markdown = readMarkdown def

markdownSmart :: String -> Pandoc
markdownSmart = readMarkdown def { readerSmart = True }

markdownCDL :: String -> Pandoc
markdownCDL = readMarkdown def { readerExtensions = Set.insert
                 Ext_compact_definition_lists $ readerExtensions def }

infix 4 =:
(=:) :: ToString c
     => String -> (String, c) -> Test
(=:) = test markdown

testBareLink :: (String, Inlines) -> Test
testBareLink (inp, ils) =
  test (readMarkdown def{ readerExtensions =
             Set.fromList [Ext_autolink_bare_uris, Ext_raw_html] })
       inp (inp, doc $ para ils)

autolink :: String -> Inlines
autolink s = link s "" (str s)

bareLinkTests :: [(String, Inlines)]
bareLinkTests =
  [ ("http://google.com is a search engine.",
     autolink "http://google.com" <> " is a search engine.")
  , ("<a href=\"http://foo.bar.baz\">http://foo.bar.baz</a>",
     rawInline "html" "<a href=\"http://foo.bar.baz\">" <>
     "http://foo.bar.baz" <> rawInline "html" "</a>")
  , ("Try this query: http://google.com?search=fish&time=hour.",
     "Try this query: " <> autolink "http://google.com?search=fish&time=hour" <> ".")
  , ("HTTPS://GOOGLE.COM,",
      autolink "HTTPS://GOOGLE.COM" <> ",")
  , ("http://el.wikipedia.org/wiki/Τεχνολογία,",
      autolink "http://el.wikipedia.org/wiki/Τεχνολογία" <> ",")
  , ("doi:10.1000/182,",
      autolink "doi:10.1000/182" <> ",")
  , ("git://github.com/foo/bar.git,",
      autolink "git://github.com/foo/bar.git" <> ",")
  , ("file:///Users/joe/joe.txt, and",
      autolink "file:///Users/joe/joe.txt" <> ", and")
  , ("mailto:someone@somedomain.com.",
      autolink "mailto:someone@somedomain.com" <> ".")
  , ("Use http: this is not a link!",
      "Use http: this is not a link!")
  , ("(http://google.com).",
      "(" <> autolink "http://google.com" <> ").")
  , ("http://en.wikipedia.org/wiki/Sprite_(computer_graphics)",
      autolink "http://en.wikipedia.org/wiki/Sprite_(computer_graphics)")
  , ("http://en.wikipedia.org/wiki/Sprite_[computer_graphics]",
      autolink "http://en.wikipedia.org/wiki/Sprite_[computer_graphics]")
  , ("http://en.wikipedia.org/wiki/Sprite_{computer_graphics}",
      autolink "http://en.wikipedia.org/wiki/Sprite_{computer_graphics}")
  , ("http://example.com/Notification_Center-GitHub-20101108-140050.jpg",
      autolink "http://example.com/Notification_Center-GitHub-20101108-140050.jpg")
  , ("https://github.com/github/hubot/blob/master/scripts/cream.js#L20-20",
      autolink "https://github.com/github/hubot/blob/master/scripts/cream.js#L20-20")
  , ("http://www.rubyonrails.com",
      autolink "http://www.rubyonrails.com")
  , ("http://www.rubyonrails.com:80",
      autolink "http://www.rubyonrails.com:80")
  , ("http://www.rubyonrails.com/~minam",
      autolink "http://www.rubyonrails.com/~minam")
  , ("https://www.rubyonrails.com/~minam",
      autolink "https://www.rubyonrails.com/~minam")
  , ("http://www.rubyonrails.com/~minam/url%20with%20spaces",
      autolink "http://www.rubyonrails.com/~minam/url%20with%20spaces")
  , ("http://www.rubyonrails.com/foo.cgi?something=here",
      autolink "http://www.rubyonrails.com/foo.cgi?something=here")
  , ("http://www.rubyonrails.com/foo.cgi?something=here&and=here",
      autolink "http://www.rubyonrails.com/foo.cgi?something=here&and=here")
  , ("http://www.rubyonrails.com/contact;new",
      autolink "http://www.rubyonrails.com/contact;new")
  , ("http://www.rubyonrails.com/contact;new%20with%20spaces",
      autolink "http://www.rubyonrails.com/contact;new%20with%20spaces")
  , ("http://www.rubyonrails.com/contact;new?with=query&string=params",
      autolink "http://www.rubyonrails.com/contact;new?with=query&string=params")
  , ("http://www.rubyonrails.com/~minam/contact;new?with=query&string=params",
      autolink "http://www.rubyonrails.com/~minam/contact;new?with=query&string=params")
  , ("http://en.wikipedia.org/wiki/Wikipedia:Today%27s_featured_picture_%28animation%29/January_20%2C_2007",
      autolink "http://en.wikipedia.org/wiki/Wikipedia:Today%27s_featured_picture_%28animation%29/January_20%2C_2007")
  , ("http://www.mail-archive.com/rails@lists.rubyonrails.org/",
      autolink "http://www.mail-archive.com/rails@lists.rubyonrails.org/")
  , ("http://www.amazon.com/Testing-Equal-Sign-In-Path/ref=pd_bbs_sr_1?ie=UTF8&s=books&qid=1198861734&sr=8-1",
      autolink "http://www.amazon.com/Testing-Equal-Sign-In-Path/ref=pd_bbs_sr_1?ie=UTF8&s=books&qid=1198861734&sr=8-1")
  , ("http://en.wikipedia.org/wiki/Texas_hold%27em",
      autolink "http://en.wikipedia.org/wiki/Texas_hold%27em")
  , ("https://www.google.com/doku.php?id=gps:resource:scs:start",
      autolink "https://www.google.com/doku.php?id=gps:resource:scs:start")
  , ("http://www.rubyonrails.com",
      autolink "http://www.rubyonrails.com")
  , ("http://manuals.ruby-on-rails.com/read/chapter.need_a-period/103#page281",
      autolink "http://manuals.ruby-on-rails.com/read/chapter.need_a-period/103#page281")
  , ("http://foo.example.com/controller/action?parm=value&p2=v2#anchor123",
      autolink "http://foo.example.com/controller/action?parm=value&p2=v2#anchor123")
  , ("http://foo.example.com:3000/controller/action",
      autolink "http://foo.example.com:3000/controller/action")
  , ("http://foo.example.com:3000/controller/action+pack",
      autolink "http://foo.example.com:3000/controller/action+pack")
  , ("http://business.timesonline.co.uk/article/0,,9065-2473189,00.html",
      autolink "http://business.timesonline.co.uk/article/0,,9065-2473189,00.html")
  , ("http://www.mail-archive.com/ruby-talk@ruby-lang.org/",
      autolink "http://www.mail-archive.com/ruby-talk@ruby-lang.org/")
  ]

{-
p_markdown_round_trip :: Block -> Bool
p_markdown_round_trip b = matches d' d''
  where d'  = normalize $ Pandoc (Meta [] [] []) [b]
        d'' = normalize
              $ readMarkdown def { readerSmart = True }
              $ writeMarkdown def d'
        matches (Pandoc _ [Plain []]) (Pandoc _ []) = True
        matches (Pandoc _ [Para []]) (Pandoc _ []) = True
        matches (Pandoc _ [Plain xs]) (Pandoc _ [Para xs']) = xs == xs'
        matches x y = x == y
-}

tests :: [Test]
tests = [ testGroup "inline code"
          [ "with attribute" =:
            "`document.write(\"Hello\");`{.javascript}"
            =?> para
                (codeWith ("",["javascript"],[]) "document.write(\"Hello\");")
          , "with attribute space" =:
            "`*` {.haskell .special x=\"7\"}"
            =?> para (codeWith ("",["haskell","special"],[("x","7")]) "*")
          ]
        , testGroup "emph and strong"
          [ "two strongs in emph" =:
             "***a**b **c**d*" =?> para (emph (strong (str "a") <> str "b" <> space
                                         <> strong (str "c") <> str "d"))
          , "emph and strong emph alternating" =:
            "*xxx* ***xxx*** xxx\n*xxx* ***xxx*** xxx"
            =?> para (emph "xxx" <> space <> strong (emph "xxx") <>
                      space <> "xxx" <> space <>
                      emph "xxx" <> space <> strong (emph "xxx") <>
                      space <> "xxx")
          , "emph with spaced strong" =:
            "*x **xx** x*"
            =?> para (emph ("x" <> space <> strong "xx" <> space <> "x"))
          , "intraword underscore with opening underscore (#1121)" =:
            "_foot_ball_" =?> para (emph (text "foot_ball"))
          ]
        , testGroup "raw LaTeX"
          [ "in URL" =:
            "\\begin\n" =?> para (text "\\begin")
          ]
        , testGroup "raw HTML"
          [ "nesting (issue #1330)" =:
            "<del>test</del>" =?>
            rawBlock "html" "<del>" <> plain (str "test") <>
            rawBlock "html" "</del>"
          ]
        , "unbalanced brackets" =:
            "[[[[[[[[[[[[[[[hi" =?> para (text "[[[[[[[[[[[[[[[hi")
        , testGroup "backslash escapes"
          [ "in URL" =:
            "[hi](/there\\))"
            =?> para (link "/there)" "" "hi")
          , "in title" =:
            "[hi](/there \"a\\\"a\")"
            =?> para (link "/there" "a\"a" "hi")
          , "in reference link title" =:
            "[hi]\n\n[hi]: /there (a\\)a)"
            =?> para (link "/there" "a)a" "hi")
          , "in reference link URL" =:
            "[hi]\n\n[hi]: /there\\.0"
            =?> para (link "/there.0" "" "hi")
          ]
        , testGroup "bare URIs"
          (map testBareLink bareLinkTests)
        , testGroup "Headers"
          [ "blank line before header" =:
            "\n# Header\n"
            =?> headerWith ("header",[],[]) 1 "Header"
          ]
        , testGroup "smart punctuation"
          [ test markdownSmart "quote before ellipses"
            ("'...hi'"
            =?> para (singleQuoted "…hi"))
          , test markdownSmart "apostrophe before emph"
            ("D'oh! A l'*aide*!"
            =?> para ("D’oh! A l’" <> emph "aide" <> "!"))
          , test markdownSmart "apostrophe in French"
            ("À l'arrivée de la guerre, le thème de l'«impossibilité du socialisme»"
            =?> para "À l’arrivée de la guerre, le thème de l’«impossibilité du socialisme»")
          ]
        , testGroup "footnotes"
          [ "indent followed by newline and flush-left text" =:
            "[^1]\n\n[^1]: my note\n\n     \nnot in note\n"
            =?> para (note (para "my note")) <> para "not in note"
          , "indent followed by newline and indented text" =:
            "[^1]\n\n[^1]: my note\n     \n    in note\n"
            =?> para (note (para "my note" <> para "in note"))
          , "recursive note" =:
            "[^1]\n\n[^1]: See [^1]\n"
            =?> para (note (para "See [^1]"))
          ]
        , testGroup "lhs"
          [ test (readMarkdown def{ readerExtensions = Set.insert
                       Ext_literate_haskell $ readerExtensions def })
              "inverse bird tracks and html" $
              "> a\n\n< b\n\n<div>\n"
              =?> codeBlockWith ("",["sourceCode","literate","haskell"],[]) "a"
                  <>
                  codeBlockWith ("",["sourceCode","haskell"],[]) "b"
                  <>
                  rawBlock "html" "<div>\n\n"
          ]
-- the round-trip properties frequently fail
--        , testGroup "round trip"
--          [ property "p_markdown_round_trip" p_markdown_round_trip
--          ]
        , testGroup "definition lists"
          [ "no blank space" =:
            "foo1\n  :  bar\n\nfoo2\n  : bar2\n  : bar3\n" =?>
            definitionList [ (text "foo1", [plain (text "bar")])
                           , (text "foo2", [plain (text "bar2"),
                                            plain (text "bar3")])
                           ]
          , "blank space before first def" =:
            "foo1\n\n  :  bar\n\nfoo2\n\n  : bar2\n  : bar3\n" =?>
            definitionList [ (text "foo1", [para (text "bar")])
                           , (text "foo2", [para (text "bar2"),
                                            plain (text "bar3")])
                           ]
          , "blank space before second def" =:
            "foo1\n  :  bar\n\nfoo2\n  : bar2\n\n  : bar3\n" =?>
            definitionList [ (text "foo1", [plain (text "bar")])
                           , (text "foo2", [plain (text "bar2"),
                                            para (text "bar3")])
                           ]
          , "laziness" =:
            "foo1\n  :  bar\nbaz\n  : bar2\n" =?>
            definitionList [ (text "foo1", [plain (text "bar baz"),
                                            plain (text "bar2")])
                           ]
          , "no blank space before first of two paragraphs" =:
            "foo1\n  : bar\n\n    baz\n" =?>
            definitionList [ (text "foo1", [para (text "bar") <>
                                            para (text "baz")])
                           ]
          ]
        , testGroup "+compact_definition_lists"
          [ test markdownCDL "basic compact list" $
            "foo1\n:   bar\n    baz\nfoo2\n:   bar2\n" =?>
            definitionList [ (text "foo1", [plain (text "bar baz")])
                           , (text "foo2", [plain (text "bar2")])
                           ]
          ]
        , testGroup "lists"
          [ "issue #1154" =:
              " -  <div>\n    first div breaks\n    </div>\n\n    <button>if this button exists</button>\n\n    <div>\n    with this div too.\n    </div>\n"
              =?> bulletList [divWith nullAttr (para $ text "first div breaks") <>
                              rawBlock "html" "<button>" <>
                              plain (text "if this button exists") <>
                              rawBlock "html" "</button>" <>
                              divWith nullAttr (para $ text "with this div too.")]
          ]
        ]
