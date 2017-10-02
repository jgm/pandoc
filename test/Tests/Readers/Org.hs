{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.Org (tests) where

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Builder

org :: Text -> Pandoc
org = purely $ readOrg def{ readerExtensions = getDefaultExtensions "org" }

orgSmart :: Text -> Pandoc
orgSmart = purely $ readOrg def { readerExtensions =
                     enableExtension Ext_smart $ getDefaultExtensions "org" }

infix 4 =:
(=:) :: ToString c
     => String -> (Text, c) -> TestTree
(=:) = test org

spcSep :: [Inlines] -> Inlines
spcSep = mconcat . intersperse space

simpleTable' :: Int
             -> [Blocks]
             -> [[Blocks]]
             -> Blocks
simpleTable' n = table "" (replicate n (AlignDefault, 0.0))

-- | Create a span for the given tag.
tagSpan :: String -> Inlines
tagSpan t = spanWith ("", ["tag"], [("tag-name", t)]) . smallcaps $ str t

tests :: [TestTree]
tests =
  [ testGroup "Inlines" $
      [ "Plain String" =:
          "Hello, World" =?>
          para (spcSep [ "Hello,", "World" ])

      , "Emphasis" =:
          "/Planet Punk/" =?>
          para (emph . spcSep $ ["Planet", "Punk"])

      , "Strong" =:
          "*Cider*" =?>
          para (strong "Cider")

      , "Strong Emphasis" =:
          "/*strength*/" =?>
          para (emph . strong $ "strength")

      , "Emphasized Strong preceded by space" =:
          " */super/*" =?>
          para (strong . emph $ "super")

      , "Strikeout" =:
          "+Kill Bill+" =?>
          para (strikeout . spcSep $ [ "Kill", "Bill" ])

      , "Verbatim" =:
          "=Robot.rock()=" =?>
          para (code "Robot.rock()")

      , "Code" =:
          "~word for word~" =?>
          para (code "word for word")

      , "Math $..$" =:
          "$E=mc^2$" =?>
           para (math "E=mc^2")

      , "Math $$..$$" =:
          "$$E=mc^2$$" =?>
          para (displayMath "E=mc^2")

      , "Math \\[..\\]" =:
          "\\[E=ℎν\\]" =?>
          para (displayMath "E=ℎν")

      , "Math \\(..\\)" =:
          "\\(σ_x σ_p ≥ \\frac{ℏ}{2}\\)" =?>
          para (math "σ_x σ_p ≥ \\frac{ℏ}{2}")

      , "Symbol" =:
          "A * symbol" =?>
          para (str "A" <> space <> str "*" <> space <> "symbol")

      , "Superscript simple expression" =:
          "2^-λ" =?>
          para (str "2" <> superscript "-λ")

      , "Superscript multi char" =:
          "2^{n-1}" =?>
          para (str "2" <> superscript "n-1")

      , "Subscript simple expression" =:
          "a_n" =?>
          para (str "a" <> subscript "n")

      , "Subscript multi char" =:
          "a_{n+1}" =?>
          para (str "a" <> subscript "n+1")

      , "Linebreak" =:
          "line \\\\ \nbreak" =?>
          para ("line" <> linebreak <> "break")

      , "Inline note" =:
          "[fn::Schreib mir eine E-Mail]" =?>
          para (note $ para "Schreib mir eine E-Mail")

      , "Markup-chars not occuring on word break are symbols" =:
          T.unlines [ "this+that+ +so+on"
                    , "seven*eight* nine*"
                    , "+not+funny+"
                    ] =?>
          para ("this+that+ +so+on" <> softbreak <>
                "seven*eight* nine*" <> softbreak <>
                strikeout "not+funny")

      , "No empty markup" =:
          "// ** __ <> == ~~ $$" =?>
          para (spcSep [ "//", "**", "__", "<>", "==", "~~", "$$" ])

      , "Adherence to Org's rules for markup borders" =:
          "/t/& a/ / ./r/ (*l*) /e/! /b/." =?>
          para (spcSep [ emph $ "t/&" <> space <> "a"
                       , "/"
                       , "./r/"
                       , "(" <> strong "l" <> ")"
                       , emph "e" <> "!"
                       , emph "b" <> "."
                       ])

      , "Quotes are allowed border chars" =:
          "/'yep/ *sure\"*" =?>
          para (emph "'yep" <> space <> strong "sure\"")

      , "Spaces are forbidden border chars" =:
          "/nada /" =?>
          para "/nada /"

      , "Markup should work properly after a blank line" =:
        T.unlines ["foo", "", "/bar/"] =?>
        (para $ text "foo") <> (para $ emph $ text "bar")

      , "Inline math must stay within three lines" =:
          T.unlines [ "$a", "b", "c$", "$d", "e", "f", "g$" ] =?>
          para ((math "a\nb\nc") <> softbreak <>
                "$d" <> softbreak <> "e" <> softbreak <>
                "f" <> softbreak <> "g$")

      , "Single-character math" =:
          "$a$ $b$! $c$?" =?>
          para (spcSep [ math "a"
                       , "$b$!"
                       , (math "c") <> "?"
                       ])

      , "Markup may not span more than two lines" =:
          "/this *is +totally\nnice+ not*\nemph/" =?>
          para ("/this" <> space <>
                  strong ("is" <> space <>
                          strikeout ("totally" <>
                            softbreak <> "nice") <>
                          space <> "not") <>
                  softbreak <> "emph/")

      , "Sub- and superscript expressions" =:
         T.unlines [ "a_(a(b)(c)d)"
                   , "e^(f(g)h)"
                   , "i_(jk)l)"
                   , "m^()n"
                   , "o_{p{q{}r}}"
                   , "s^{t{u}v}"
                   , "w_{xy}z}"
                   , "1^{}2"
                   , "3_{{}}"
                   , "4^(a(*b(c*)d))"
                   ] =?>
         para (mconcat $ intersperse softbreak
                      [ "a" <> subscript "(a(b)(c)d)"
                      , "e" <> superscript "(f(g)h)"
                      , "i" <> (subscript "(jk)") <> "l)"
                      , "m" <> (superscript "()") <> "n"
                      , "o" <> subscript "p{q{}r}"
                      , "s" <> superscript "t{u}v"
                      , "w" <> (subscript "xy") <> "z}"
                      , "1" <> (superscript "") <> "2"
                      , "3" <> subscript "{}"
                      , "4" <> superscript ("(a(" <> strong "b(c" <> ")d))")
                      ])
      , "Verbatim text can contain equal signes (=)" =:
          "=is_subst = True=" =?>
          para (code "is_subst = True")

      , testGroup "Images"
        [ "Image" =:
            "[[./sunset.jpg]]" =?>
            (para $ image "./sunset.jpg" "" "")

        , "Image with explicit file: prefix" =:
            "[[file:sunrise.jpg]]" =?>
            (para $ image "sunrise.jpg" "" "")

        , "Multiple images within a paragraph" =:
            T.unlines [ "[[file:sunrise.jpg]]"
                      , "[[file:sunset.jpg]]"
                      ] =?>
            (para $ (image "sunrise.jpg" "" "")
                 <> softbreak
                 <> (image "sunset.jpg" "" ""))

        , "Image with html attributes" =:
            T.unlines [ "#+ATTR_HTML: :width 50%"
                      , "[[file:guinea-pig.gif]]"
                      ] =?>
            (para $ imageWith ("", [], [("width", "50%")]) "guinea-pig.gif" "" "")
        ]

      , "Explicit link" =:
          "[[http://zeitlens.com/][pseudo-random /nonsense/]]" =?>
          (para $ link "http://zeitlens.com/" ""
                       ("pseudo-random" <> space <> emph "nonsense"))

      , "Self-link" =:
          "[[http://zeitlens.com/]]" =?>
          (para $ link "http://zeitlens.com/" "" "http://zeitlens.com/")

      , "Absolute file link" =:
          "[[/url][hi]]" =?>
          (para $ link "file:///url" "" "hi")

      , "Link to file in parent directory" =:
          "[[../file.txt][moin]]" =?>
          (para $ link "../file.txt" "" "moin")

      , "Empty link (for gitit interop)" =:
          "[[][New Link]]" =?>
          (para $ link "" "" "New Link")

      , "Image link" =:
          "[[sunset.png][file:dusk.svg]]" =?>
          (para $ link "sunset.png" "" (image "dusk.svg" "" ""))

      , "Image link with non-image target" =:
          "[[http://example.com][./logo.png]]" =?>
          (para $ link "http://example.com" "" (image "./logo.png" "" ""))

      , "Plain link" =:
          "Posts on http://zeitlens.com/ can be funny at times." =?>
          (para $ spcSep [ "Posts", "on"
                         , link "http://zeitlens.com/" "" "http://zeitlens.com/"
                         , "can", "be", "funny", "at", "times."
                         ])

      , "Angle link" =:
          "Look at <http://moltkeplatz.de> for fnords." =?>
          (para $ spcSep [ "Look", "at"
                         , link "http://moltkeplatz.de" "" "http://moltkeplatz.de"
                         , "for", "fnords."
                         ])

      , "Absolute file link" =:
          "[[file:///etc/passwd][passwd]]" =?>
          (para $ link "file:///etc/passwd" "" "passwd")

      , "File link" =:
          "[[file:target][title]]" =?>
          (para $ link "target" "" "title")

      , "Anchor" =:
          "<<anchor>> Link here later." =?>
          (para $ spanWith ("anchor", [], []) mempty <>
                  "Link" <> space <> "here" <> space <> "later.")

      , "Inline code block" =:
          "src_emacs-lisp{(message \"Hello\")}" =?>
          (para $ codeWith ( ""
                           , [ "commonlisp" ]
                           , [ ("org-language", "emacs-lisp") ])
                           "(message \"Hello\")")

      , "Inline code block with arguments" =:
          "src_sh[:export both :results output]{echo 'Hello, World'}" =?>
          (para $ codeWith ( ""
                           , [ "bash" ]
                           , [ ("org-language", "sh")
                             , ("export", "both")
                             , ("results", "output")
                             ]
                           )
                           "echo 'Hello, World'")

      , "Inline code block with toggle" =:
          "src_sh[:toggle]{echo $HOME}" =?>
          (para $ codeWith ( ""
                           , [ "bash" ]
                           , [ ("org-language", "sh")
                             , ("toggle", "yes")
                             ]
                           )
                           "echo $HOME")

      , "Citation" =:
          "[@nonexistent]" =?>
          let citation = Citation
                         { citationId = "nonexistent"
                         , citationPrefix = []
                         , citationSuffix = []
                         , citationMode = NormalCitation
                         , citationNoteNum = 0
                         , citationHash = 0}
          in (para $ cite [citation] "[@nonexistent]")

      , "Citation containing text" =:
          "[see @item1 p. 34-35]" =?>
          let citation = Citation
                         { citationId = "item1"
                         , citationPrefix = [Str "see"]
                         , citationSuffix = [Space ,Str "p.",Space,Str "34-35"]
                         , citationMode = NormalCitation
                         , citationNoteNum = 0
                         , citationHash = 0}
          in (para $ cite [citation] "[see @item1 p. 34-35]")

      , "Org-ref simple citation" =:
        "cite:pandoc" =?>
        let citation = Citation
                       { citationId = "pandoc"
                       , citationPrefix = mempty
                       , citationSuffix = mempty
                       , citationMode = AuthorInText
                       , citationNoteNum = 0
                       , citationHash = 0
                       }
        in (para $ cite [citation] "cite:pandoc")

      , "Org-ref simple citation with underscores" =:
        "cite:pandoc_org_ref" =?>
        let citation = Citation
                       { citationId = "pandoc_org_ref"
                       , citationPrefix = mempty
                       , citationSuffix = mempty
                       , citationMode = AuthorInText
                       , citationNoteNum = 0
                       , citationHash = 0
                       }
        in (para $ cite [citation] "cite:pandoc_org_ref")

      , "Org-ref simple citation succeeded by comma" =:
        "cite:pandoc," =?>
        let citation = Citation
                       { citationId = "pandoc"
                       , citationPrefix = mempty
                       , citationSuffix = mempty
                       , citationMode = AuthorInText
                       , citationNoteNum = 0
                       , citationHash = 0
                       }
        in (para $ cite [citation] "cite:pandoc" <> str ",")

      , "Org-ref simple citation succeeded by dot" =:
        "cite:pandoc." =?>
        let citation = Citation
                       { citationId = "pandoc"
                       , citationPrefix = mempty
                       , citationSuffix = mempty
                       , citationMode = AuthorInText
                       , citationNoteNum = 0
                       , citationHash = 0
                       }
        in (para $ cite [citation] "cite:pandoc" <> str ".")

      , "Org-ref simple citation succeeded by colon" =:
        "cite:pandoc:" =?>
        let citation = Citation
                       { citationId = "pandoc"
                       , citationPrefix = mempty
                       , citationSuffix = mempty
                       , citationMode = AuthorInText
                       , citationNoteNum = 0
                       , citationHash = 0
                       }
        in (para $ cite [citation] "cite:pandoc" <> str ":")

      , "Org-ref simple citep citation" =:
        "citep:pandoc" =?>
        let citation = Citation
                       { citationId = "pandoc"
                       , citationPrefix = mempty
                       , citationSuffix = mempty
                       , citationMode = NormalCitation
                       , citationNoteNum = 0
                       , citationHash = 0
                       }
        in (para $ cite [citation] "citep:pandoc")

      , "Org-ref extended citation" =:
        "[[citep:Dominik201408][See page 20::, for example]]" =?>
        let citation = Citation
                       { citationId = "Dominik201408"
                       , citationPrefix = toList "See page 20"
                       , citationSuffix = toList ", for example"
                       , citationMode = NormalCitation
                       , citationNoteNum = 0
                       , citationHash = 0
                       }
        in (para $ cite [citation] "[[citep:Dominik201408][See page 20::, for example]]")

      , testGroup "Berkeley-style citations" $
        let pandocCite = Citation
              { citationId = "Pandoc"
              , citationPrefix = mempty
              , citationSuffix = mempty
              , citationMode = NormalCitation
              , citationNoteNum = 0
              , citationHash = 0
              }
            pandocInText = pandocCite { citationMode = AuthorInText }
            dominikCite = Citation
              { citationId = "Dominik201408"
              , citationPrefix = mempty
              , citationSuffix = mempty
              , citationMode = NormalCitation
              , citationNoteNum = 0
              , citationHash = 0
              }
            dominikInText = dominikCite { citationMode = AuthorInText }
        in [
            "Berkeley-style in-text citation" =:
              "See @Dominik201408." =?>
                (para $ "See "
                      <> cite [dominikInText] "@Dominik201408"
                      <> ".")

          , "Berkeley-style parenthetical citation list" =:
              "[(cite): see; @Dominik201408;also @Pandoc; and others]" =?>
              let pandocCite'  = pandocCite {
                                   citationPrefix = toList "also"
                                 , citationSuffix = toList "and others"
                                 }
                  dominikCite' = dominikCite {
                                   citationPrefix = toList "see"
                                 }
              in (para $ cite [dominikCite', pandocCite'] "")

          , "Berkeley-style plain citation list" =:
              "[cite: See; @Dominik201408; and @Pandoc; and others]" =?>
              let pandocCite' = pandocInText {
                                  citationPrefix = toList "and"
                                }
              in (para $ "See "
                      <> cite [dominikInText] ""
                      <> "," <> space
                      <> cite [pandocCite'] ""
                      <> "," <> space <> "and others")
        ]

      , "Inline LaTeX symbol" =:
          "\\dots" =?>
          para "…"

      , "Inline LaTeX command" =:
          "\\textit{Emphasised}" =?>
          para (emph "Emphasised")

      , "Inline LaTeX command with spaces" =:
          "\\emph{Emphasis mine}" =?>
          para (emph "Emphasis mine")

      , "Inline LaTeX math symbol" =:
          "\\tau" =?>
          para (emph "τ")

      , "Unknown inline LaTeX command" =:
          "\\notacommand{foo}" =?>
          para (rawInline "latex" "\\notacommand{foo}")

      , "Export snippet" =:
          "@@html:<kbd>M-x org-agenda</kbd>@@" =?>
          para (rawInline "html" "<kbd>M-x org-agenda</kbd>")

      , "MathML symbol in LaTeX-style" =:
          "There is a hackerspace in Lübeck, Germany, called nbsp (unicode symbol: '\\nbsp')." =?>
          para ("There is a hackerspace in Lübeck, Germany, called nbsp (unicode symbol: ' ').")

      , "MathML symbol in LaTeX-style, including braces" =:
          "\\Aacute{}stor" =?>
          para "Ástor"

      , "MathML copy sign" =:
          "\\copy" =?>
          para "©"

      , "MathML symbols, space separated" =:
          "\\ForAll \\Auml" =?>
          para "∀ Ä"

      , "LaTeX citation" =:
          "\\cite{Coffee}" =?>
          let citation = Citation
                         { citationId = "Coffee"
                         , citationPrefix = []
                         , citationSuffix = []
                         , citationMode = NormalCitation
                         , citationNoteNum = 0
                         , citationHash = 0}
          in (para . cite [citation] $ rawInline "latex" "\\cite{Coffee}")

      , "Macro" =:
          T.unlines [ "#+MACRO: HELLO /Hello, $1/"
                    , "{{{HELLO(World)}}}"
                    ] =?>
          para (emph "Hello, World")

      , "Macro repeting its argument" =:
          T.unlines [ "#+MACRO: HELLO $1$1"
                    , "{{{HELLO(moin)}}}"
                    ] =?>
          para "moinmoin"

      , "Macro called with too few arguments" =:
          T.unlines [ "#+MACRO: HELLO Foo $1 $2 Bar"
                    , "{{{HELLO()}}}"
                    ] =?>
          para "Foo Bar"
      ]

  , testGroup "Meta Information" $
      [ "Comment" =:
          "# Nothing to see here" =?>
          (mempty::Blocks)

      , "Not a comment" =:
          "#-tag" =?>
          para "#-tag"

      , "Comment surrounded by Text" =:
          T.unlines [ "Before"
                    , "# Comment"
                    , "After"
                    ] =?>
          mconcat [ para "Before"
                  , para "After"
                  ]

      , "Title" =:
        "#+TITLE: Hello, World" =?>
        let titleInline = toList $ "Hello," <> space <> "World"
            meta = setMeta "title" (MetaInlines titleInline) $ nullMeta
        in Pandoc meta mempty

      , "Author" =:
        "#+author: Albert /Emacs-Fanboy/ Krewinkel" =?>
        let author = toList . spcSep $ [ "Albert", emph "Emacs-Fanboy", "Krewinkel" ]
            meta = setMeta "author" (MetaList [MetaInlines author]) $ nullMeta
        in Pandoc meta mempty

      , "Multiple authors" =:
        "#+author: James Dewey Watson, Francis Harry Compton Crick " =?>
        let watson = MetaInlines $ toList "James Dewey Watson"
            crick = MetaInlines $ toList "Francis Harry Compton Crick"
            meta = setMeta "author" (MetaList [watson, crick]) $ nullMeta
        in Pandoc meta mempty

      , "Date" =:
        "#+Date: Feb. *28*, 2014" =?>
        let date = toList . spcSep $ [ "Feb.", (strong "28") <> ",", "2014" ]
            meta = setMeta "date" (MetaInlines date) $ nullMeta
        in Pandoc meta mempty

      , "Description" =:
        "#+DESCRIPTION: Explanatory text" =?>
        let description = "Explanatory text"
            meta = setMeta "description" (MetaString description) $ nullMeta
        in Pandoc meta mempty

      , "Properties drawer" =:
          T.unlines [ "  :PROPERTIES:"
                    , "  :setting: foo"
                    , "  :END:"
                    ] =?>
          (mempty::Blocks)

      , "LaTeX_headers options are translated to header-includes" =:
          "#+LaTeX_header: \\usepackage{tikz}" =?>
          let latexInlines = rawInline "latex" "\\usepackage{tikz}"
              inclList = MetaList [MetaInlines (toList latexInlines)]
              meta = setMeta "header-includes" inclList nullMeta
          in Pandoc meta mempty

      , "LaTeX_class option is translated to documentclass" =:
          "#+LATEX_CLASS: article" =?>
          let meta = setMeta "documentclass" (MetaString "article") nullMeta
          in Pandoc meta mempty

      , "LaTeX_class_options is translated to classoption" =:
          "#+LATEX_CLASS_OPTIONS: [a4paper]" =?>
          let meta = setMeta "classoption" (MetaString "a4paper") nullMeta
          in Pandoc meta mempty

      , "LaTeX_class_options is translated to classoption" =:
          "#+html_head: <meta/>" =?>
          let html = rawInline "html" "<meta/>"
              inclList = MetaList [MetaInlines (toList html)]
              meta = setMeta "header-includes" inclList nullMeta
          in Pandoc meta mempty

      , "later meta definitions take precedence" =:
          T.unlines [ "#+AUTHOR: this will not be used"
                    , "#+author: Max"
                    ] =?>
          let author = MetaInlines [Str "Max"]
              meta = setMeta "author" (MetaList [author]) $ nullMeta
          in Pandoc meta mempty

      , "Logbook drawer" =:
          T.unlines [ "  :LogBook:"
                    , "  - State \"DONE\"       from \"TODO\"       [2014-03-03 Mon 11:00]"
                    , "  :END:"
                    ] =?>
          (mempty::Blocks)

      , "Drawer surrounded by text" =:
          T.unlines [ "Before"
                    , ":PROPERTIES:"
                    , ":END:"
                    , "After"
                    ] =?>
          para "Before" <> para "After"

      , "Drawer markers must be the only text in the line" =:
          T.unlines [ "  :LOGBOOK: foo"
                    , "  :END: bar"
                    ] =?>
          para (":LOGBOOK: foo" <> softbreak <> ":END: bar")

      , "Drawers can be arbitrary" =:
          T.unlines [ ":FOO:"
                    , "/bar/"
                    , ":END:"
                    ] =?>
          divWith (mempty, ["FOO", "drawer"], mempty) (para $ emph "bar")

      , "Anchor reference" =:
          T.unlines [ "<<link-here>> Target."
                    , ""
                    , "[[link-here][See here!]]"
                    ] =?>
          (para (spanWith ("link-here", [], []) mempty <> "Target.") <>
           para (link "#link-here" "" ("See" <> space <> "here!")))

      , "Search links are read as emph" =:
          "[[Wally][Where's Wally?]]" =?>
          (para (emph $ "Where's" <> space <> "Wally?"))

      , "Link to nonexistent anchor" =:
          T.unlines [ "<<link-here>> Target."
                    , ""
                    , "[[link$here][See here!]]"
                    ] =?>
          (para (spanWith ("link-here", [], []) mempty <> "Target.") <>
           para (emph ("See" <> space <> "here!")))

      , "Link abbreviation" =:
          T.unlines [ "#+LINK: wp https://en.wikipedia.org/wiki/%s"
                    , "[[wp:Org_mode][Wikipedia on Org-mode]]"
                    ] =?>
          (para (link "https://en.wikipedia.org/wiki/Org_mode" ""
                      ("Wikipedia" <> space <> "on" <> space <> "Org-mode")))

      , "Link abbreviation, defined after first use" =:
          T.unlines [ "[[zl:non-sense][Non-sense articles]]"
                    , "#+LINK: zl http://zeitlens.com/tags/%s.html"
                    ] =?>
          (para (link "http://zeitlens.com/tags/non-sense.html" ""
                      ("Non-sense" <> space <> "articles")))

      , "Link abbreviation, URL encoded arguments" =:
          T.unlines [ "#+link: expl http://example.com/%h/foo"
                    , "[[expl:Hello, World!][Moin!]]"
                    ] =?>
          (para (link "http://example.com/Hello%2C%20World%21/foo" "" "Moin!"))

      , "Link abbreviation, append arguments" =:
          T.unlines [ "#+link: expl http://example.com/"
                    , "[[expl:foo][bar]]"
                    ] =?>
          (para (link "http://example.com/foo" "" "bar"))


      , testGroup "export options"

          [ "disable simple sub/superscript syntax" =:
              T.unlines [ "#+OPTIONS: ^:nil"
                        , "a^b"
                        ] =?>
              para "a^b"

          , "directly select drawers to be exported" =:
              T.unlines [ "#+OPTIONS: d:(\"IMPORTANT\")"
                        , ":IMPORTANT:"
                        , "23"
                        , ":END:"
                        , ":BORING:"
                        , "very boring"
                        , ":END:"
                        ] =?>
              divWith (mempty, ["IMPORTANT", "drawer"], mempty) (para "23")

          , "exclude drawers from being exported" =:
              T.unlines [ "#+OPTIONS: d:(not \"BORING\")"
                        , ":IMPORTANT:"
                        , "5"
                        , ":END:"
                        , ":BORING:"
                        , "very boring"
                        , ":END:"
                        ] =?>
              divWith (mempty, ["IMPORTANT", "drawer"], mempty) (para "5")

          , "don't include archive trees" =:
              T.unlines [ "#+OPTIONS: arch:nil"
                        , "* old  :ARCHIVE:"
                        ] =?>
              (mempty ::Blocks)

          , "include complete archive trees" =:
              T.unlines [ "#+OPTIONS: arch:t"
                        , "* old  :ARCHIVE:"
                        , "  boring"
                        ] =?>
              mconcat [ headerWith ("old", [], mempty) 1
                                   ("old" <> space <> tagSpan "ARCHIVE")
                      , para "boring"
                      ]

          , "include archive tree header only" =:
              T.unlines [ "#+OPTIONS: arch:headline"
                        , "* old  :ARCHIVE:"
                        , "  boring"
                        ] =?>
              headerWith ("old", [], mempty) 1 ("old" <> space <> tagSpan "ARCHIVE")

          , "limit headline depth" =:
              T.unlines [ "#+OPTIONS: H:2"
                        , "* top-level section"
                        , "** subsection"
                        , "*** list item 1"
                        , "*** list item 2"
                        ] =?>
              mconcat [ headerWith ("top-level-section", [], [])    1 "top-level section"
                      , headerWith ("subsection", [], []) 2 "subsection"
                      , orderedList [ para "list item 1", para "list item 2" ]
                      ]

          , "turn all headlines into lists" =:
              T.unlines [ "#+OPTIONS: H:0"
                        , "first block"
                        , "* top-level section 1"
                        , "** subsection"
                        , "* top-level section 2"
                        ] =?>
              mconcat [ para "first block"
                      , orderedList
                        [ (para "top-level section 1" <>
                           orderedList [ para "subsection" ])
                        , para "top-level section 2" ]
                      ]

          , "preserve linebreaks as hard breaks" =:
              T.unlines [ "#+OPTIONS: \\n:t"
                        , "first"
                        , "second"
                        ] =?>
              para ("first" <> linebreak <> "second")

          , "disable author export" =:
              T.unlines [ "#+OPTIONS: author:nil"
                        , "#+AUTHOR: ShyGuy"
                        ] =?>
              Pandoc nullMeta mempty

          , "disable creator export" =:
              T.unlines [ "#+OPTIONS: creator:nil"
                        , "#+creator: The Architect"
                        ] =?>
              Pandoc nullMeta mempty

          , "disable email export" =:
              T.unlines [ "#+OPTIONS: email:nil"
                        , "#+email: no-mail-please@example.com"
                        ] =?>
              Pandoc nullMeta mempty

          , "disable inclusion of todo keywords" =:
              T.unlines [ "#+OPTIONS: todo:nil"
                        , "** DONE todo export"
                        ] =?>
              headerWith ("todo-export", [], []) 2 "todo export"

          , "remove tags from headlines" =:
              T.unlines [ "#+OPTIONS: tags:nil"
                        , "* Headline :hello:world:"
                        ] =?>
              headerWith ("headline", [], mempty) 1 "Headline"
          ]
      ]

  , testGroup "Basic Blocks" $
      [ "Paragraph" =:
          "Paragraph\n" =?>
          para "Paragraph"

      , testGroup "headers" $
        [ "First Level Header" =:
            "* Headline\n" =?>
            headerWith ("headline", [], []) 1 "Headline"

        , "Third Level Header" =:
            "*** Third Level Headline\n" =?>
            headerWith ("third-level-headline", [], [])
                       3
                       ("Third" <> space <> "Level" <> space <> "Headline")

        , "Compact Headers with Paragraph" =:
            T.unlines [ "* First Level"
                      , "** Second Level"
                      , "   Text"
                      ] =?>
            mconcat [ headerWith ("first-level", [], [])
                                 1
                                 ("First" <> space <> "Level")
                    , headerWith ("second-level", [], [])
                                 2
                                 ("Second" <> space <> "Level")
                    , para "Text"
                    ]

        , "Separated Headers with Paragraph" =:
            T.unlines [ "* First Level"
                      , ""
                      , "** Second Level"
                      , ""
                      , "   Text"
                      ] =?>
            mconcat [ headerWith ("first-level", [], [])
                                 1
                                 ("First" <> space <> "Level")
                    , headerWith ("second-level", [], [])
                                 2
                                 ("Second" <> space <> "Level")
                    , para "Text"
                    ]

        , "Headers not preceded by a blank line" =:
            T.unlines [ "** eat dinner"
                      , "Spaghetti and meatballs tonight."
                      , "** walk dog"
                      ] =?>
            mconcat [ headerWith ("eat-dinner", [], [])
                                 2
                                 ("eat" <> space <> "dinner")
                    , para $ spcSep [ "Spaghetti", "and", "meatballs", "tonight." ]
                    , headerWith ("walk-dog", [], [])
                                 2
                                 ("walk" <> space <> "dog")
                    ]

        , testGroup "Todo keywords"
          [ "Header with known todo keyword" =:
              "* TODO header" =?>
              let todoSpan = spanWith ("", ["todo", "TODO"], []) "TODO"
              in headerWith ("header", [], []) 1 (todoSpan <> space <> "header")

          , "Header marked as done" =:
              "* DONE header" =?>
              let todoSpan = spanWith ("", ["done", "DONE"], []) "DONE"
              in headerWith ("header", [], []) 1 (todoSpan <> space <> "header")

          , "Header with unknown todo keyword" =:
              "* WAITING header" =?>
              headerWith ("waiting-header", [], []) 1 "WAITING header"

          , "Custom todo keywords" =:
              T.unlines [ "#+TODO: WAITING CANCELLED"
                        , "* WAITING compile"
                        , "* CANCELLED lunch"
                        ] =?>
              let todoSpan = spanWith ("", ["todo", "WAITING"], []) "WAITING"
                  doneSpan = spanWith ("", ["done", "CANCELLED"], []) "CANCELLED"
              in headerWith ("compile", [], []) 1 (todoSpan <> space <> "compile")
              <> headerWith ("lunch", [], []) 1 (doneSpan <> space <> "lunch")

          , "Custom todo keywords with multiple done-states" =:
              T.unlines [ "#+TODO: WAITING | DONE CANCELLED "
                        , "* WAITING compile"
                        , "* CANCELLED lunch"
                        , "* DONE todo-feature"
                        ] =?>
              let waiting = spanWith ("", ["todo", "WAITING"], []) "WAITING"
                  cancelled = spanWith ("", ["done", "CANCELLED"], []) "CANCELLED"
                  done = spanWith ("", ["done", "DONE"], []) "DONE"
              in headerWith ("compile", [], []) 1 (waiting <> space <> "compile")
              <> headerWith ("lunch", [], []) 1 (cancelled <> space <> "lunch")
              <> headerWith ("todo-feature", [], []) 1 (done <> space <> "todo-feature")
          ]

        , "Tagged headers" =:
            T.unlines [ "* Personal       :PERSONAL:"
                      , "** Call Mom      :@PHONE:"
                      , "** Call John     :@PHONE:JOHN: "
                      ] =?>
            mconcat [ headerWith ("personal", [], [])
                                 1
                                 ("Personal " <> tagSpan "PERSONAL")
                    , headerWith ("call-mom", [], [])
                                 2
                                 ("Call Mom " <> tagSpan "@PHONE")
                    , headerWith ("call-john", [], [])
                                 2
                                 ("Call John " <> tagSpan "@PHONE" <> "\160" <> tagSpan "JOHN")
                    ]

        , "Untagged header containing colons" =:
            "* This: is not: tagged" =?>
            headerWith ("this-is-not-tagged", [], []) 1 "This: is not: tagged"

        , "Header starting with strokeout text" =:
            T.unlines [ "foo"
                      , ""
                      , "* +thing+ other thing"
                      ] =?>
            mconcat [ para "foo"
                    , headerWith ("thing-other-thing", [], [])
                                 1
                                 ((strikeout "thing") <> " other thing")
                    ]

        , "Comment Trees" =:
            T.unlines [ "* COMMENT A comment tree"
                      , "  Not much going on here"
                      , "** This will be dropped"
                      , "* Comment tree above"
                      ] =?>
            headerWith ("comment-tree-above", [], []) 1 "Comment tree above"

        , "Nothing but a COMMENT header" =:
            "* COMMENT Test" =?>
            (mempty::Blocks)

        , "Tree with :noexport:" =:
            T.unlines [ "* Should be ignored :archive:noexport:old:"
                      , "** Old stuff"
                      , "   This is not going to be exported"
                      ] =?>
            (mempty::Blocks)

        , "Subtree with :noexport:" =:
            T.unlines [ "* Exported"
                      , "** This isn't exported :noexport:"
                      , "*** This neither"
                      , "** But this is"
                      ] =?>
            mconcat [ headerWith ("exported", [], []) 1 "Exported"
                    , headerWith ("but-this-is", [], []) 2 "But this is"
                    ]

        , "Preferences are treated as header attributes" =:
            T.unlines [ "* foo"
                      , "  :PROPERTIES:"
                      , "  :custom_id: fubar"
                      , "  :bar: baz"
                      , "  :END:"
                      ] =?>
            headerWith ("fubar", [], [("bar", "baz")]) 1 "foo"


        , "Headers marked with a unnumbered property get a class of the same name" =:
            T.unlines [ "* Not numbered"
                      , "  :PROPERTIES:"
                      , "  :UNNUMBERED: t"
                      , "  :END:"
                      ] =?>
            headerWith ("not-numbered", ["unnumbered"], []) 1 "Not numbered"
      ]
      , "Paragraph starting with an asterisk" =:
          "*five" =?>
          para "*five"

      , "Paragraph containing asterisk at beginning of line" =:
          T.unlines [ "lucky"
                    , "*star"
                    ] =?>
          para ("lucky" <> softbreak <> "*star")

      , "Example block" =:
          T.unlines [ ": echo hello"
                    , ": echo dear tester"
                    ] =?>
          codeBlockWith ("", ["example"], []) "echo hello\necho dear tester\n"

      , "Example block surrounded by text" =:
          T.unlines [ "Greetings"
                    , ": echo hello"
                    , ": echo dear tester"
                    , "Bye"
                    ] =?>
          mconcat [ para "Greetings"
                  , codeBlockWith ("", ["example"], [])
                                  "echo hello\necho dear tester\n"
                  , para "Bye"
                  ]

      , "Horizontal Rule" =:
          T.unlines [ "before"
                    , "-----"
                    , "after"
                    ] =?>
          mconcat [ para "before"
                  , horizontalRule
                  , para "after"
                  ]

      , "Not a Horizontal Rule" =:
          "----- em and en dash" =?>
          para "\8212\8211 em and en dash"

      , "Comment Block" =:
          T.unlines [ "#+BEGIN_COMMENT"
                    , "stuff"
                    , "bla"
                    , "#+END_COMMENT"] =?>
          (mempty::Blocks)

      , testGroup "Figures" $
        [ "Figure" =:
            T.unlines [ "#+caption: A very courageous man."
                      , "#+name: goodguy"
                      , "[[file:edward.jpg]]"
                      ] =?>
            para (image "edward.jpg" "fig:goodguy" "A very courageous man.")

        , "Figure with no name" =:
            T.unlines [ "#+caption: I've been through the desert on this"
                      , "[[file:horse.png]]"
                      ] =?>
            para (image "horse.png" "fig:" "I've been through the desert on this")

        , "Figure with `fig:` prefix in name" =:
            T.unlines [ "#+caption: Used as a metapher in evolutionary biology."
                      , "#+name: fig:redqueen"
                      , "[[./the-red-queen.jpg]]"
                      ] =?>
            para (image "./the-red-queen.jpg" "fig:redqueen"
                        "Used as a metapher in evolutionary biology.")

        , "Figure with HTML attributes" =:
            T.unlines [ "#+CAPTION: mah brain just explodid"
                      , "#+NAME: lambdacat"
                      , "#+ATTR_HTML: :style color: blue :role button"
                      , "[[file:lambdacat.jpg]]"
                      ] =?>
            let kv = [("style", "color: blue"), ("role", "button")]
                name = "fig:lambdacat"
                caption = "mah brain just explodid"
            in para (imageWith (mempty, mempty, kv) "lambdacat.jpg" name caption)

        , "Labelled figure" =:
            T.unlines [ "#+CAPTION: My figure"
                      , "#+LABEL: fig:myfig"
                      , "[[file:blub.png]]"
                      ] =?>
            let attr = ("fig:myfig", mempty, mempty)
            in para (imageWith attr "blub.png" "fig:" "My figure")

        , "Figure with empty caption" =:
            T.unlines [ "#+CAPTION:"
                      , "[[file:guess.jpg]]"
                      ] =?>
            para (image "guess.jpg" "fig:" "")
        ]

      , "Footnote" =:
          T.unlines [ "A footnote[1]"
                    , ""
                    , "[1] First paragraph"
                    , ""
                    , "second paragraph"
                    ] =?>
          para (mconcat
                [ "A", space, "footnote"
                , note $ mconcat [ para ("First" <> space <> "paragraph")
                                 , para ("second" <> space <> "paragraph")
                                 ]
                ])

      , "Two footnotes" =:
          T.unlines [ "Footnotes[fn:1][fn:2]"
                    , ""
                    , "[fn:1] First note."
                    , ""
                    , "[fn:2] Second note."
                    ] =?>
          para (mconcat
                [ "Footnotes"
                , note $ para ("First" <> space <> "note.")
                , note $ para ("Second" <> space <> "note.")
                ])

      , "Emphasized text before footnote" =:
          T.unlines [ "/text/[fn:1]"
                    , ""
                    , "[fn:1] unicorn"
                    ] =?>
          para (mconcat
               [ emph "text"
               , note . para $ "unicorn"
               ])

      , "Footnote that starts with emphasized text" =:
          T.unlines [ "text[fn:1]"
                    , ""
                    , "[fn:1] /emphasized/"
                    ] =?>
          para (mconcat
               [ "text"
               , note . para $ emph "emphasized"
               ])

      , "Footnote followed by header" =:
          T.unlines [ "Another note[fn:yay]"
                    , ""
                    , "[fn:yay] This is great!"
                    , ""
                    , "** Headline"
                    ] =?>
          mconcat
          [ para (mconcat
                  [ "Another", space, "note"
                  , note $ para ("This" <> space <> "is" <> space <> "great!")
                  ])
          , headerWith ("headline", [], []) 2 "Headline"
          ]
      ]

  , testGroup "Lists" $
      [ "Simple Bullet Lists" =:
          ("- Item1\n" <>
           "- Item2\n") =?>
          bulletList [ plain "Item1"
                     , plain "Item2"
                     ]

      , "Indented Bullet Lists" =:
          ("   - Item1\n" <>
           "   - Item2\n") =?>
          bulletList [ plain "Item1"
                     , plain "Item2"
                     ]

      , "Unindented *" =:
          ("- Item1\n" <>
           "* Item2\n") =?>
          bulletList [ plain "Item1"
                     ] <>
          headerWith ("item2", [], []) 1 "Item2"

      , "Multi-line Bullet Lists" =:
          ("- *Fat\n" <>
           "  Tony*\n" <>
           "- /Sideshow\n" <>
           " Bob/") =?>
          bulletList [ plain $ strong ("Fat" <> softbreak <> "Tony")
                     , plain $ emph ("Sideshow" <> softbreak <> "Bob")
                     ]

      , "Nested Bullet Lists" =:
          ("- Discovery\n" <>
           "  + One More Time\n" <>
           "  + Harder, Better, Faster, Stronger\n" <>
           "- Homework\n" <>
           "  + Around the World\n"<>
           "- Human After All\n" <>
           "  + Technologic\n" <>
           "  + Robot Rock\n") =?>
          bulletList [ mconcat
                       [ plain "Discovery"
                       , bulletList [ plain ("One" <> space <>
                                             "More" <> space <>
                                             "Time")
                                    , plain ("Harder," <> space <>
                                             "Better," <> space <>
                                             "Faster," <> space <>
                                             "Stronger")
                                    ]
                       ]
                     , mconcat
                       [ plain "Homework"
                       , bulletList [ plain ("Around" <> space <>
                                             "the" <> space <>
                                             "World")
                                    ]
                       ]
                     , mconcat
                       [ plain ("Human" <> space <> "After" <> space <> "All")
                       , bulletList [ plain "Technologic"
                                    , plain ("Robot" <> space <> "Rock")
                                    ]
                       ]
                     ]

      , "Bullet List with Decreasing Indent" =:
           ("  - Discovery\n\
            \ - Human After All\n") =?>
           mconcat [ bulletList [ plain "Discovery" ]
                   , bulletList [ plain ("Human" <> space <> "After" <> space <> "All")]
                   ]

      , "Header follows Bullet List" =:
          ("  - Discovery\n\
           \  - Human After All\n\
           \* Homework") =?>
          mconcat [ bulletList [ plain "Discovery"
                               , plain ("Human" <> space <> "After" <> space <> "All")
                               ]
                  , headerWith ("homework", [], []) 1 "Homework"
                  ]

      , "Bullet List Unindented with trailing Header" =:
          ("- Discovery\n\
           \- Homework\n\
           \* NotValidListItem") =?>
          mconcat [ bulletList [ plain "Discovery"
                               , plain "Homework"
                               ]
                  , headerWith ("notvalidlistitem", [], []) 1 "NotValidListItem"
                  ]

      , "Simple Ordered List" =:
          ("1. Item1\n" <>
           "2. Item2\n") =?>
          let listStyle = (1, DefaultStyle, DefaultDelim)
              listStructure = [ plain "Item1"
                              , plain "Item2"
                              ]
          in orderedListWith listStyle listStructure

      , "Simple Ordered List with Parens" =:
          ("1) Item1\n" <>
           "2) Item2\n") =?>
          let listStyle = (1, DefaultStyle, DefaultDelim)
              listStructure = [ plain "Item1"
                              , plain "Item2"
                              ]
          in orderedListWith listStyle listStructure

      , "Indented Ordered List" =:
          (" 1. Item1\n" <>
           " 2. Item2\n") =?>
          let listStyle = (1, DefaultStyle, DefaultDelim)
              listStructure = [ plain "Item1"
                              , plain "Item2"
                              ]
          in orderedListWith listStyle listStructure

      , "Nested Ordered Lists" =:
          ("1. One\n" <>
           "   1. One-One\n" <>
           "   2. One-Two\n" <>
           "2. Two\n" <>
           "   1. Two-One\n"<>
           "   2. Two-Two\n") =?>
          let listStyle = (1, DefaultStyle, DefaultDelim)
              listStructure = [ mconcat
                                [ plain "One"
                                , orderedList [ plain "One-One"
                                              , plain "One-Two"
                                              ]
                                ]
                              , mconcat
                                [ plain "Two"
                                , orderedList [ plain "Two-One"
                                              , plain "Two-Two"
                                              ]
                                ]
                              ]
          in orderedListWith listStyle listStructure

      , "Ordered List in Bullet List" =:
          ("- Emacs\n" <>
           "  1. Org\n") =?>
          bulletList [ (plain "Emacs") <>
                       (orderedList [ plain "Org"])
                     ]

      , "Bullet List in Ordered List" =:
          ("1. GNU\n" <>
           "   - Freedom\n") =?>
          orderedList [ (plain "GNU") <> bulletList [ (plain "Freedom") ] ]

      , "Definition List" =:
          T.unlines [ "- PLL :: phase-locked loop"
                    , "- TTL ::"
                    , "  transistor-transistor logic"
                    , "- PSK :: phase-shift keying"
                    , ""
                    , "  a digital modulation scheme"
                    ] =?>
          definitionList [ ("PLL", [ plain $ "phase-locked" <> space <> "loop" ])
                         , ("TTL", [ plain $ "transistor-transistor" <> space <>
                                               "logic" ])
                         , ("PSK", [ mconcat
                                     [ para $ "phase-shift" <> space <> "keying"
                                     , para $ spcSep [ "a", "digital"
                                                     , "modulation", "scheme" ]
                                     ]
                                   ])
                         ]
      , "Definition list with multi-word term" =:
        " - Elijah Wood :: He plays Frodo" =?>
         definitionList [ ("Elijah" <> space <> "Wood", [plain $ "He" <> space <> "plays" <> space <> "Frodo"])]
      , "Compact definition list" =:
           T.unlines [ "- ATP :: adenosine 5' triphosphate"
                     , "- DNA :: deoxyribonucleic acid"
                     , "- PCR :: polymerase chain reaction"
                     , ""
                     ] =?>
          definitionList
          [ ("ATP", [ plain $ spcSep [ "adenosine", "5'", "triphosphate" ] ])
          , ("DNA", [ plain $ spcSep [ "deoxyribonucleic", "acid" ] ])
          , ("PCR", [ plain $ spcSep [ "polymerase", "chain", "reaction" ] ])
          ]

      , "Definition List With Trailing Header" =:
          "- definition :: list\n\
          \- cool :: defs\n\
          \* header" =?>
          mconcat [ definitionList [ ("definition", [plain "list"])
                                   , ("cool", [plain "defs"])
                                   ]
                  , headerWith ("header", [], []) 1 "header"
                  ]

      , "Definition lists double-colon markers must be surrounded by whitespace" =:
          "- std::cout" =?>
          bulletList [ plain "std::cout" ]

      , "Loose bullet list" =:
         T.unlines [ "- apple"
                   , ""
                   , "- orange"
                   , ""
                   , "- peach"
                   ] =?>
          bulletList [ para "apple"
                     , para "orange"
                     , para "peach"
                     ]

      , "Recognize preceding paragraphs in non-list contexts" =:
          T.unlines [ "CLOSED: [2015-10-19 Mon 15:03]"
                    , "- Note taken on [2015-10-19 Mon 13:24]"
                    ] =?>
          mconcat [ para "CLOSED: [2015-10-19 Mon 15:03]"
                  , bulletList [ plain "Note taken on [2015-10-19 Mon 13:24]" ]
                  ]
      ]

  , testGroup "Tables"
      [ "Single cell table" =:
          "|Test|" =?>
          simpleTable' 1 mempty [[plain "Test"]]

      , "Multi cell table" =:
          "| One | Two |" =?>
           simpleTable' 2 mempty [ [ plain "One", plain "Two" ] ]

      , "Multi line table" =:
          T.unlines [ "| One   |"
                    , "| Two   |"
                    , "| Three |"
                    ] =?>
           simpleTable' 1 mempty
                        [ [ plain "One" ]
                        , [ plain "Two" ]
                        , [ plain "Three" ]
                        ]

      , "Empty table" =:
          "||" =?>
          simpleTable' 1 mempty [[mempty]]

      , "Glider Table" =:
          T.unlines [ "| 1 | 0 | 0 |"
                    , "| 0 | 1 | 1 |"
                    , "| 1 | 1 | 0 |"
                    ] =?>
          simpleTable' 3 mempty
                       [ [ plain "1", plain "0", plain "0" ]
                       , [ plain "0", plain "1", plain "1" ]
                       , [ plain "1", plain "1", plain "0" ]
                       ]

      , "Table between Paragraphs" =:
          T.unlines [ "Before"
                    , "| One | Two |"
                    , "After"
                    ] =?>
          mconcat [ para "Before"
                  , simpleTable' 2 mempty [ [ plain "One", plain "Two" ] ]
                  , para "After"
                  ]

      , "Table with Header" =:
          T.unlines [ "| Species      | Status       |"
                    , "|--------------+--------------|"
                    , "| cervisiae    | domesticated |"
                    , "| paradoxus    | wild         |"
                    ] =?>
          simpleTable [ plain "Species", plain "Status" ]
                      [ [ plain "cervisiae", plain "domesticated" ]
                      , [ plain "paradoxus", plain "wild" ]
                      ]

      , "Table with final hline" =:
          T.unlines [ "| cervisiae    | domesticated |"
                    , "| paradoxus    | wild         |"
                    , "|--------------+--------------|"
                    ] =?>
          simpleTable' 2 mempty
                [ [ plain "cervisiae", plain "domesticated" ]
                 , [ plain "paradoxus", plain "wild" ]
                ]

      , "Table in a box" =:
          T.unlines [ "|---------|---------|"
                    , "| static  | Haskell |"
                    , "| dynamic | Lisp    |"
                    , "|---------+---------|"
                    ] =?>
          simpleTable' 2 mempty
                [ [ plain "static", plain "Haskell" ]
                , [ plain "dynamic", plain "Lisp" ]
                ]

      , "Table with empty cells" =:
          "|||c|" =?>
          simpleTable' 3 mempty [[mempty, mempty, plain "c"]]

      , "Table with empty rows" =:
          T.unlines [ "| first  |"
                    , "|        |"
                    , "| third  |"
                    ] =?>
          simpleTable' 1 mempty [[plain "first"], [mempty], [plain "third"]]

      , "Table with alignment row" =:
          T.unlines [ "| Numbers | Text | More |"
                    , "| <c>     | <r>  |      |"
                    , "| 1       | One  | foo  |"
                    , "| 2       | Two  | bar  |"
                    ] =?>
          table "" (zip [AlignCenter, AlignRight, AlignDefault] [0, 0, 0])
                []
                [ [ plain "Numbers", plain "Text", plain "More" ]
                , [ plain "1"      , plain "One" , plain "foo"  ]
                , [ plain "2"      , plain "Two" , plain "bar"  ]
                ]

      , "Pipe within text doesn't start a table" =:
          "Ceci n'est pas une | pipe " =?>
          para (spcSep [ "Ceci", "n'est", "pas", "une", "|", "pipe" ])

      , "Missing pipe at end of row" =:
          "|incomplete-but-valid" =?>
          simpleTable' 1 mempty [ [ plain "incomplete-but-valid" ] ]

      , "Table with differing row lengths" =:
          T.unlines [ "| Numbers | Text "
                    , "|-"
                    , "| <c>     | <r>  |"
                    , "| 1       | One  | foo  |"
                    , "| 2"
                    ] =?>
          table "" (zip [AlignCenter, AlignRight] [0, 0])
                [ plain "Numbers", plain "Text" ]
                [ [ plain "1" , plain "One" , plain "foo" ]
                , [ plain "2" ]
                ]

      , "Table with caption" =:
          T.unlines [ "#+CAPTION: Hitchhiker's Multiplication Table"
                    , "| x |  6 |"
                    , "| 9 | 42 |"
                    ] =?>
          table "Hitchhiker's Multiplication Table"
                [(AlignDefault, 0), (AlignDefault, 0)]
                []
                [ [ plain "x", plain "6" ]
                , [ plain "9", plain "42" ]
                ]
      ]

    , testGroup "Blocks and fragments"
      [ "Source block" =:
           T.unlines [ "  #+BEGIN_SRC haskell"
                     , "  main = putStrLn greeting"
                     , "    where greeting = \"moin\""
                     , "  #+END_SRC" ] =?>
           let attr' = ("", ["haskell"], [])
               code' = "main = putStrLn greeting\n" <>
                       "  where greeting = \"moin\"\n"
           in codeBlockWith attr' code'

      , "Source block with indented code" =:
           T.unlines [ "  #+BEGIN_SRC haskell"
                     , "    main = putStrLn greeting"
                     , "      where greeting = \"moin\""
                     , "  #+END_SRC" ] =?>
           let attr' = ("", ["haskell"], [])
               code' = "main = putStrLn greeting\n" <>
                       "  where greeting = \"moin\"\n"
           in codeBlockWith attr' code'

      , "Source block with tab-indented code" =:
           T.unlines [ "\t#+BEGIN_SRC haskell"
                     , "\tmain = putStrLn greeting"
                     , "\t  where greeting = \"moin\""
                     , "\t#+END_SRC" ] =?>
           let attr' = ("", ["haskell"], [])
               code' = "main = putStrLn greeting\n" <>
                       "  where greeting = \"moin\"\n"
           in codeBlockWith attr' code'

      , "Empty source block" =:
           T.unlines [ "  #+BEGIN_SRC haskell"
                     , "  #+END_SRC" ] =?>
           let attr' = ("", ["haskell"], [])
               code' = ""
           in codeBlockWith attr' code'

      , "Source block between paragraphs" =:
           T.unlines [ "Low German greeting"
                     , "  #+BEGIN_SRC haskell"
                     , "  main = putStrLn greeting"
                     , "    where greeting = \"Moin!\""
                     , "  #+END_SRC" ] =?>
           let attr' = ("", ["haskell"], [])
               code' = "main = putStrLn greeting\n" <>
                        "  where greeting = \"Moin!\"\n"
           in mconcat [ para $ spcSep [ "Low", "German", "greeting"  ]
                      , codeBlockWith attr' code'
                      ]
      , "Source block with babel arguments" =:
           T.unlines [ "#+BEGIN_SRC emacs-lisp :exports both"
                     , "(progn (message \"Hello, World!\")"
                     , "       (+ 23 42))"
                     , "#+END_SRC" ] =?>
           let classes = [ "commonlisp" ] -- as kate doesn't know emacs-lisp syntax
               params = [ ("org-language", "emacs-lisp")
                        , ("exports", "both")
                        ]
               code' = unlines [ "(progn (message \"Hello, World!\")"
                               , "       (+ 23 42))" ]
           in codeBlockWith ("", classes, params) code'

      , "Source block with results and :exports both" =:
           T.unlines [ "#+BEGIN_SRC emacs-lisp :exports both"
                     , "(progn (message \"Hello, World!\")"
                     , "       (+ 23 42))"
                     , "#+END_SRC"
                     , ""
                     , "#+RESULTS:"
                     , ": 65"] =?>
           let classes = [ "commonlisp" ]
               params = [ ("org-language", "emacs-lisp")
                        , ("exports", "both")
                        ]
               code' = unlines [ "(progn (message \"Hello, World!\")"
                               , "       (+ 23 42))" ]
               results' = "65\n"
           in codeBlockWith ("", classes, params) code'
              <>
              codeBlockWith ("", ["example"], []) results'

      , "Source block with results and :exports code" =:
           T.unlines [ "#+BEGIN_SRC emacs-lisp :exports code"
                     , "(progn (message \"Hello, World!\")"
                     , "       (+ 23 42))"
                     , "#+END_SRC"
                     , ""
                     , "#+RESULTS:"
                     , ": 65" ] =?>
           let classes = [ "commonlisp" ]
               params = [ ("org-language", "emacs-lisp")
                        , ("exports", "code")
                        ]
               code' = unlines [ "(progn (message \"Hello, World!\")"
                               , "       (+ 23 42))" ]
           in codeBlockWith ("", classes, params) code'

      , "Source block with results and :exports results" =:
           T.unlines [ "#+BEGIN_SRC emacs-lisp :exports results"
                     , "(progn (message \"Hello, World!\")"
                     , "       (+ 23 42))"
                     , "#+END_SRC"
                     , ""
                     , "#+RESULTS:"
                     , ": 65" ] =?>
           let results' = "65\n"
           in codeBlockWith ("", ["example"], []) results'

      , "Source block with results and :exports none" =:
           T.unlines [ "#+BEGIN_SRC emacs-lisp :exports none"
                     , "(progn (message \"Hello, World!\")"
                     , "       (+ 23 42))"
                     , "#+END_SRC"
                     , ""
                     , "#+RESULTS:"
                     , ": 65" ] =?>
           (mempty :: Blocks)

      , "Source block with toggling header arguments" =:
        T.unlines [ "#+BEGIN_SRC sh :noeval"
                  , "echo $HOME"
                  , "#+END_SRC"
                  ] =?>
        let classes = [ "bash" ]
            params = [ ("org-language", "sh"), ("noeval", "yes") ]
        in codeBlockWith ("", classes, params) "echo $HOME\n"

      , "Source block with line number switch" =:
        T.unlines [ "#+BEGIN_SRC sh -n 10"
                  , ":() { :|:& };:"
                  , "#+END_SRC"
                  ] =?>
        let classes = [ "bash", "numberLines" ]
            params = [ ("org-language", "sh"), ("startFrom", "10") ]
        in codeBlockWith ("", classes, params) ":() { :|:& };:\n"

      , "Source block with multi-word parameter values" =:
        T.unlines [ "#+BEGIN_SRC dot :cmdline -Kdot -Tpng "
                  , "digraph { id [label=\"ID\"] }"
                  , "#+END_SRC"
                  ] =?>
        let classes = [ "dot" ]
            params = [ ("cmdline", "-Kdot -Tpng") ]
        in codeBlockWith ("", classes, params) "digraph { id [label=\"ID\"] }\n"

      , "Example block" =:
           T.unlines [ "#+begin_example"
                     , "A chosen representation of"
                     , "a rule."
                     , "#+eND_exAMPle"
                     ] =?>
           codeBlockWith ("", ["example"], [])
                         "A chosen representation of\na rule.\n"

      , "HTML block" =:
           T.unlines [ "#+BEGIN_HTML"
                     , "<aside>HTML5 is pretty nice.</aside>"
                     , "#+END_HTML"
                     ] =?>
           rawBlock "html" "<aside>HTML5 is pretty nice.</aside>\n"

      , "Quote block" =:
           T.unlines [ "#+BEGIN_QUOTE"
                     , "/Niemand/ hat die Absicht, eine Mauer zu errichten!"
                     , "#+END_QUOTE"
                     ] =?>
           blockQuote (para (spcSep [ emph "Niemand", "hat", "die", "Absicht,"
                                    , "eine", "Mauer", "zu", "errichten!"
                                    ]))

      , "Verse block" =:
          T.unlines [ "The first lines of Goethe's /Faust/:"
                    , "#+begin_verse"
                    , "Habe nun, ach! Philosophie,"
                    , "Juristerei und Medizin,"
                    , "Und leider auch Theologie!"
                    , "Durchaus studiert, mit heißem Bemühn."
                    , "#+end_verse"
                    ] =?>
          mconcat
          [ para $ spcSep [ "The", "first", "lines", "of"
                          , "Goethe's", emph "Faust" <> ":"]
          , lineBlock
              [ "Habe nun, ach! Philosophie,"
              , "Juristerei und Medizin,"
              , "Und leider auch Theologie!"
              , "Durchaus studiert, mit heißem Bemühn."
              ]
          ]

      , "Verse block with blank lines" =:
          T.unlines [ "#+BEGIN_VERSE"
                    , "foo"
                    , ""
                    , "bar"
                    , "#+END_VERSE"
                    ] =?>
          lineBlock [ "foo", mempty, "bar" ]

      , "Verse block with varying indentation" =:
          T.unlines [ "#+BEGIN_VERSE"
                    , "  hello darkness"
                    , "my old friend"
                    , "#+END_VERSE"
                    ] =?>
          lineBlock [ "\160\160hello darkness", "my old friend" ]

      , "Raw block LaTeX" =:
          T.unlines [ "#+BEGIN_LaTeX"
                    , "The category $\\cat{Set}$ is adhesive."
                    , "#+END_LaTeX"
                    ] =?>
          rawBlock "latex" "The category $\\cat{Set}$ is adhesive.\n"

      , "Raw LaTeX line" =:
          "#+LATEX: \\let\\foo\\bar" =?>
          rawBlock "latex" "\\let\\foo\\bar"

      , "Raw Beamer line" =:
          "#+beamer: \\pause" =?>
          rawBlock "beamer" "\\pause"

      , "Raw HTML line" =:
          "#+HTML: <aside>not important</aside>" =?>
          rawBlock "html" "<aside>not important</aside>"

      , "Export block HTML" =:
          T.unlines [ "#+BEGIN_export html"
                    , "<samp>Hello, World!</samp>"
                    , "#+END_export"
                    ] =?>
          rawBlock "html" "<samp>Hello, World!</samp>\n"

      , "LaTeX fragment" =:
          T.unlines [ "\\begin{equation}"
                    , "X_i = \\begin{cases}"
                    , "      G_{\\alpha(i)} & \\text{if }\\alpha(i-1) = \\alpha(i)\\\\"
                    , "      C_{\\alpha(i)} & \\text{otherwise}"
                    , "      \\end{cases}"
                    , "\\end{equation}"
                    ] =?>
          rawBlock "latex"
                   (unlines [ "\\begin{equation}"
                            , "X_i = \\begin{cases}"
                            , "      G_{\\alpha(i)} & \\text{if }\\alpha(i-1) =" <>
                              " \\alpha(i)\\\\"
                            , "      C_{\\alpha(i)} & \\text{otherwise}"
                            , "      \\end{cases}"
                            , "\\end{equation}"
                            ])

      , "Code block with caption" =:
          T.unlines [ "#+CAPTION: Functor laws in Haskell"
                    , "#+NAME: functor-laws"
                    , "#+BEGIN_SRC haskell"
                    , "fmap id = id"
                    , "fmap (p . q) = (fmap p) . (fmap q)"
                    , "#+END_SRC"
                    ] =?>
          divWith
             nullAttr
             (mappend
              (plain $ spanWith ("", ["label"], [])
                                (spcSep [ "Functor", "laws", "in", "Haskell" ]))
              (codeBlockWith ("functor-laws", ["haskell"], [])
                             (unlines [ "fmap id = id"
                                      , "fmap (p . q) = (fmap p) . (fmap q)"
                                      ])))

      , "Convert blank lines in blocks to single newlines" =:
          T.unlines [ "#+begin_html"
                    , ""
                    , "<span>boring</span>"
                    , ""
                    , "#+end_html"
                    ] =?>
          rawBlock "html" "\n<span>boring</span>\n\n"

      , "Accept `ATTR_HTML` attributes for generic block" =:
          T.unlines [ "#+ATTR_HTML: :title hello, world :id test :class fun code"
                    , "#+BEGIN_TEST"
                    , "nonsense"
                    , "#+END_TEST"
                    ] =?>
          let attr = ("test", ["fun", "code", "TEST"], [("title", "hello, world")])
          in divWith attr (para "nonsense")

      , "Non-letter chars in source block parameters" =:
          T.unlines [ "#+BEGIN_SRC C :tangle xxxx.c :city Zürich"
                    , "code body"
                    , "#+END_SRC"
                    ] =?>
          let params  = [ ("org-language", "C")
                        , ("tangle", "xxxx.c")
                        , ("city", "Zürich")
                        ]
          in codeBlockWith ( "", ["c"], params) "code body\n"
      ]

    , testGroup "Smart punctuation"
      [ test orgSmart "quote before ellipses"
        ("'...hi'"
         =?> para (singleQuoted "…hi"))

      , test orgSmart "apostrophe before emph"
        ("D'oh! A l'/aide/!"
         =?> para ("D’oh! A l’" <> emph "aide" <> "!"))

      , test orgSmart "apostrophe in French"
        ("À l'arrivée de la guerre, le thème de l'«impossibilité du socialisme»"
         =?> para "À l’arrivée de la guerre, le thème de l’«impossibilité du socialisme»")

      , test orgSmart "Quotes cannot occur at the end of emphasized text"
        ("/say \"yes\"/" =?>
         para ("/say" <> space <> doubleQuoted "yes" <> "/"))

      , test orgSmart "Dashes are allowed at the borders of emphasis'"
        ("/foo---/" =?>
         para (emph "foo—"))

      , test orgSmart "Single quotes can be followed by emphasized text"
        ("Singles on the '/meat market/'" =?>
         para ("Singles on the " <> (singleQuoted $ emph "meat market")))

      , test orgSmart "Double quotes can be followed by emphasized text"
        ("Double income, no kids: \"/DINK/\"" =?>
         para ("Double income, no kids: " <> (doubleQuoted $ emph "DINK")))
      ]
  ]
