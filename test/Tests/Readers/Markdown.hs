{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Markdown
   Copyright   : © 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Tests for the Markdown reader.
-}
module Tests.Readers.Markdown (tests) where

import Prelude
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

markdown :: Text -> Pandoc
markdown = purely $ readMarkdown def { readerExtensions =
                            disableExtension Ext_smart pandocExtensions }

markdownSmart :: Text -> Pandoc
markdownSmart = purely $  readMarkdown def { readerExtensions =
                             enableExtension Ext_smart pandocExtensions }

markdownCDL :: Text -> Pandoc
markdownCDL = purely $ readMarkdown def { readerExtensions = enableExtension
                 Ext_compact_definition_lists pandocExtensions }

markdownGH :: Text -> Pandoc
markdownGH = purely $ readMarkdown def {
                readerExtensions = githubMarkdownExtensions }

infix 4 =:
(=:) :: ToString c
     => String -> (Text, c) -> TestTree
(=:) = test markdown

testBareLink :: (Text, Inlines) -> TestTree
testBareLink (inp, ils) =
  test (purely $ readMarkdown def{ readerExtensions =
             extensionsFromList [Ext_autolink_bare_uris, Ext_raw_html] })
       (unpack inp) (inp, doc $ para ils)

autolink :: String -> Inlines
autolink = autolinkWith ("",["uri"],[])

autolinkWith :: Attr -> String -> Inlines
autolinkWith attr s = linkWith attr s' "" (str s')
  where s' = T.pack s

bareLinkTests :: [(Text, Inlines)]
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
      linkWith ("",["uri"],[])
        "http://en.wikipedia.org/wiki/Sprite_%5Bcomputer_graphics%5D" ""
        (str "http://en.wikipedia.org/wiki/Sprite_[computer_graphics]"))
  , ("http://en.wikipedia.org/wiki/Sprite_{computer_graphics}",
      linkWith ("",["uri"],[])
        "http://en.wikipedia.org/wiki/Sprite_%7Bcomputer_graphics%7D" ""
        (str "http://en.wikipedia.org/wiki/Sprite_{computer_graphics}"))
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
  , ("https://example.org/?anchor=lala-",
      autolink "https://example.org/?anchor=lala-")
  , ("https://example.org/?anchor=-lala",
      autolink "https://example.org/?anchor=-lala")
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

tests :: [TestTree]
tests = [ testGroup "inline code"
          [ "with attribute" =:
            "`document.write(\"Hello\");`{.javascript}"
            =?> para
                (codeWith ("",["javascript"],[]) "document.write(\"Hello\");")
          , "with attribute space" =:
            "`*` {.haskell .special x=\"7\"}"
            =?> para (code "*" <> space <> str "{.haskell" <> space <>
                      str ".special" <> space <> str "x=\"7\"}")
          ]
        , testGroup "inline code in lists (regression tests for #6284)" $
          let lists = [("ordered", "1. ", ol), ("bullet", "- ", ul)]
              ol = orderedListWith (1, Decimal, Period)
              ul = bulletList
              items =
                [ ("in text"                , ["If `(1) x`, then `2`"], [text "If " <> code "(1) x" <> text ", then " <> code "2"])
                , ("at start"               , ["`#. x`"              ], [code "#. x"                                             ])
                , ("at start"               , ["`- x`"               ], [code "- x"                                              ])
                , ("after literal backticks", ["`x``#. x`"           ], [code "x``#. x"                                          ])
                , ("after literal backticks", ["`x``- x`"            ], [code "x``- x"                                           ])
                ]
              lis = ["`text","y","x`"]
              lis' = ["text","y","x"]
              bldLsts w lsts txts
                = let (res, res', f) =
                         foldr (\((_, _, lt), lc) (acc, tacc, t) ->
                             if lt [] == t []
                             then (acc, lc : tacc, lt)
                             else (join t tacc acc, [lc], lt))
                           (mempty, [], mconcat)
                           (zip lsts (map text txts))
                      join t tacc acc = case tacc of
                          [] -> acc
                          [x] -> t [plain x] <> acc
                          xs -> t (map w xs) <> acc
                  in join f res' res
          in ["code with list marker "<>mp<>" in " <> ln <> " list" =:
              T.intercalate "\n" (map (lstr <>) istrs) =?> lbld (map plain iblds)
              | (ln, lstr, lbld) <- lists, (mp, istrs, iblds) <- items]
          <> [ "lists with newlines in backticks" =:
               T.intercalate "\n" (zipWith (\i (_, lt, _) -> lt <> i) lis lsts)
               =?> bldLsts plain lsts lis
             | lsts <- [ [i, j, k] | i <- lists, j <- lists, k <- lists]
             ]
          <> [ "lists with newlines and indent in backticks" =:
               T.intercalate ("\n" <> T.replicate 4 " ") (zipWith (\i (_, lt, _) -> lt <> i) lis lsts)
               =?> let (_, _, f) = head lsts
                   in f [plain $ code $ T.intercalate (T.replicate 5 " ") $ head lis' : zipWith (\i (_, lt, _) -> lt <> i) (tail lis') (tail lsts)]
             | lsts <- [ [i, j, k] | i <- lists, j <- lists, k <- lists]
             ]
          <> [ "lists with blank lines and indent in backticks" =:
               T.intercalate ("\n\n" <> T.replicate 4 " ") (zipWith (\i (_, lt, _) -> lt <> i) lis lsts)
               <> "\n"
               =?> let (_, _, f) = head lsts
                   in f . pure $ (para . text $ head lis) <> bldLsts para (tail lsts) (tail lis)
             | lsts <- [ [i, j, k] | i <- lists, j <- lists, k <- lists]
             ]
        , testGroup "emph and strong"
          [ "two strongs in emph" =:
             "***a**b **c**d*" =?> para (emph (strong (str "a") <> str "b" <> space
                                         <> strong (str "c") <> str "d"))
          , "emph and strong emph alternating" =:
            "*xxx* ***xxx*** xxx\n*xxx* ***xxx*** xxx"
            =?> para (emph "xxx" <> space <> strong (emph "xxx") <>
                      space <> "xxx" <> softbreak <>
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
          , "invalid tag (issue #1820" =:
            "</ div></.div>" =?>
            para (text "</ div></.div>")
          , "technically invalid comment" =:
            "<!-- pandoc --help -->" =?>
            rawBlock "html" "<!-- pandoc --help -->"
          , test markdownGH "issue 2469" $
            "<\n\na>" =?>
            para (text "<") <> para (text "a>")
          ]
        , testGroup "raw email addresses"
          [ test markdownGH "issue 2940" $
            "**@user**" =?>
            para (strong (text "@user"))
          ]
        , testGroup "emoji"
          [ test markdownGH "emoji symbols" $
            ":smile: and :+1:" =?> para (spanWith ("", ["emoji"], [("data-emoji", "smile")]) "😄" <>
                                         space <> str "and" <> space <>
                                         spanWith ("", ["emoji"], [("data-emoji", "+1")]) "👍")
          ]
        , "unbalanced brackets" =:
            "[[[[[[[[[[[[hi" =?> para (text "[[[[[[[[[[[[hi")
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
        , testGroup "autolinks"
          [ "with unicode dash following" =:
            "<http://foo.bar>\8212" =?> para (autolink "http://foo.bar" <>
                                         str "\8212")
          , "a partial URL (#2277)" =:
            "<www.boe.es/buscar/act.php?id=BOE-A-1996-8930#a66>" =?>
            para (text "<www.boe.es/buscar/act.php?id=BOE-A-1996-8930#a66>")
          , "with some attributes" =:
            "<http://foo.bar>{#i .j .z k=v}" =?>
            para (autolinkWith ("i", ["j", "z"], [("k", "v")]) "http://foo.bar")
          , "with some attributes and spaces" =:
            "<http://foo.bar> {#i .j .z k=v}" =?>
            para (autolink "http://foo.bar" <> space <> text "{#i .j .z k=v}")
          ]
        , testGroup "links"
          [ "no autolink inside link" =:
            "[<https://example.org>](url)" =?>
            para (link "url" "" (text "<https://example.org>"))
          , "no inline link inside link" =:
            "[[a](url2)](url)" =?>
            para (link "url" "" (text "[a](url2)"))
          , "no bare URI inside link" =:
            "[https://example.org(](url)" =?>
            para (link "url" "" (text "https://example.org("))
          ]
        , testGroup "Headers"
          [ "blank line before header" =:
            "\n# Header\n"
            =?> headerWith ("header",[],[]) 1 "Header"
          , "bracketed text (#2062)" =:
            "# [hi]\n"
            =?> headerWith ("hi",[],[]) 1 "[hi]"
          , "ATX header without trailing #s" =:
            "# Foo bar\n\n" =?>
            headerWith ("foo-bar",[],[]) 1 "Foo bar"
          , "ATX header without trailing #s" =:
            "# Foo bar with # #" =?>
            headerWith ("foo-bar-with",[],[]) 1 "Foo bar with #"
          , "setext header" =:
            "Foo bar\n=\n\n Foo bar 2 \n=" =?>
            headerWith ("foo-bar",[],[]) 1 "Foo bar"
            <> headerWith ("foo-bar-2",[],[]) 1 "Foo bar 2"
          ]
        , testGroup "Implicit header references"
          [ "ATX header without trailing #s" =:
            "# Header\n[header]\n\n[header ]\n\n[ header]" =?>
            headerWith ("header",[],[]) 1 "Header"
            <> para (link "#header" "" (text "header"))
            <> para (link "#header" "" (text "header"))
            <> para (link "#header" "" (text "header"))
          , "ATX header with trailing #s" =:
            "# Foo bar #\n[foo bar]\n\n[foo bar ]\n\n[ foo bar]" =?>
            headerWith ("foo-bar",[],[]) 1 "Foo bar"
            <> para (link "#foo-bar" "" (text "foo bar"))
            <> para (link "#foo-bar" "" (text "foo bar"))
            <> para (link "#foo-bar" "" (text "foo bar"))
          , "setext header" =:
            " Header \n=\n\n[header]\n\n[header ]\n\n[ header]" =?>
            headerWith ("header",[],[]) 1 "Header"
            <> para (link "#header" "" (text "header"))
            <> para (link "#header" "" (text "header"))
            <> para (link "#header" "" (text "header"))
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
          , test markdownSmart "apostrophe after math" $ -- issue #1909
              "The value of the $x$'s and the systems' condition." =?>
              para (text "The value of the " <> math "x" <> text "\8217s and the systems\8217 condition.")
          , test markdownSmart "unclosed double quote"
            ("**this should \"be bold**"
            =?> para (strong "this should \"be bold"))
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
          [ test (purely $ readMarkdown def{ readerExtensions = enableExtension
                       Ext_literate_haskell pandocExtensions })
              "inverse bird tracks and html" $
              "> a\n\n< b\n\n<div>\n"
              =?> codeBlockWith ("",["haskell","literate"],[]) "a"
                  <>
                  codeBlockWith ("",["haskell"],[]) "b"
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
            definitionList [ (text "foo1", [plain (text "bar" <>
                                                 softbreak <> text "baz"),
                                            plain (text "bar2")])
                           ]
          , "no blank space before first of two paragraphs" =:
            "foo1\n  : bar\n\n    baz\n" =?>
            definitionList [ (text "foo1", [para (text "bar") <>
                                            para (text "baz")])
                           ]
          , "first line not indented" =:
            "foo\n: bar\n" =?>
            definitionList [ (text "foo", [plain (text "bar")]) ]
          , "list in definition" =:
            "foo\n:   - bar\n" =?>
            definitionList [ (text "foo", [bulletList [plain (text "bar")]]) ]
          , "in div" =:
            "<div>foo\n:   - bar\n</div>" =?>
            divWith nullAttr (definitionList
              [ (text "foo", [bulletList [plain (text "bar")]]) ])
          ]
        , testGroup "+compact_definition_lists"
          [ test markdownCDL "basic compact list" $
            "foo1\n:   bar\n    baz\nfoo2\n:   bar2\n" =?>
            definitionList [ (text "foo1", [plain (text "bar" <> softbreak <>
                                                     text "baz")])
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
          , test markdownGH "issue #1636" $
              T.unlines [ "* a"
                        , "* b"
                        , "* c"
                        , "    * d" ]
              =?>
              bulletList [ plain "a"
                         , plain "b"
                         , plain "c" <> bulletList [plain "d"] ]
          ]
        , testGroup "entities"
          [ "character references" =:
            "&lang; &ouml;" =?> para (text "\10216 ö")
          , "numeric" =:
            "&#44;&#x44;&#X44;" =?> para (text ",DD")
          , "in link title" =:
            "[link](/url \"title &lang; &ouml; &#44;\")" =?>
               para (link "/url" "title \10216 ö ," (text "link"))
          ]
        , testGroup "citations"
          [ "simple" =:
            "@item1" =?> para (cite [
                Citation{ citationId      = "item1"
                        , citationPrefix  = []
                        , citationSuffix  = []
                        , citationMode    = AuthorInText
                        , citationNoteNum = 0
                        , citationHash    = 0
                        }
                ] "@item1")
          , "key starts with digit" =:
            "@1657:huyghens" =?> para (cite [
                Citation{ citationId      = "1657:huyghens"
                        , citationPrefix  = []
                        , citationSuffix  = []
                        , citationMode    = AuthorInText
                        , citationNoteNum = 0
                        , citationHash    = 0
                        }
                ] "@1657:huyghens")
          ]
        , let citation = cite [Citation "cita" [] [] AuthorInText 0 0] (str "@cita")
          in testGroup "footnote/link following citation" -- issue #2083
          [ "footnote" =:
              T.unlines [ "@cita[^note]"
                        , ""
                        , "[^note]: note" ] =?>
              para (
                citation <> note (para $ str "note")
              )
          , "normal link" =:
              "@cita [link](http://www.com)" =?>
              para (
                citation <> space <> link "http://www.com" "" (str "link")
              )
          , "reference link" =:
              T.unlines [ "@cita [link][link]"
                        , ""
                        , "[link]: http://www.com" ] =?>
              para (
                citation <> space <> link "http://www.com" "" (str "link")
              )
          , "short reference link" =:
              T.unlines [ "@cita [link]"
                        , ""
                        , "[link]: http://www.com" ] =?>
              para (
                citation <> space <> link "http://www.com" "" (str "link")
              )
          , "implicit header link" =:
              T.unlines [ "# Header"
                        , "@cita [Header]" ] =?>
              headerWith ("header",[],[]) 1 (str "Header") <> para (
                citation <> space <> link "#header" "" (str "Header")
              )
          , "regular citation" =:
              "@cita [foo]" =?>
              para (
                cite [Citation "cita" [] [Str "foo"] AuthorInText 0 0]
                  (str "@cita" <> space <> str "[foo]")
              )
          ]
        ]
