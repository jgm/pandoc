{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.Org (tests) where

import Text.Pandoc.Definition
import Test.Framework
import Tests.Helpers
import Tests.Arbitrary()
import Text.Pandoc.Builder
import Text.Pandoc
import Data.List (intersperse)
import Data.Monoid (mempty, mappend, mconcat)

org :: String -> Pandoc
org = readOrg def

infix 4 =:
(=:) :: ToString c
     => String -> (String, c) -> Test
(=:) = test org

spcSep :: [Inlines] -> Inlines
spcSep = mconcat . intersperse space

simpleTable' :: Int
             -> [Blocks]
             -> [[Blocks]]
             -> Blocks
simpleTable' n = table "" (take n $ repeat (AlignDefault, 0.0))

tests :: [Test]
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
          unlines [ "this+that+ +so+on"
                  , "seven*eight* nine*"
                  , "+not+funny+"
                  ] =?>
          para (spcSep [ "this+that+", "+so+on"
                       , "seven*eight*", "nine*"
                       , strikeout "not+funny"
                       ])

      , "No empty markup" =:
          "// ** __ ++ == ~~ $$" =?>
          para (spcSep [ "//", "**", "__", "++", "==", "~~", "$$" ])

      , "Adherence to Org's rules for markup borders" =:
          "/t/& a/ / ./r/ (*l*) /e/! /b/." =?>
          para (spcSep [ emph $ "t/&" <> space <> "a"
                       , "/"
                       , "./r/"
                       , "(" <> (strong "l") <> ")"
                       , (emph "e") <> "!"
                       , (emph "b") <> "."
                       ])

      , "Quotes are forbidden border chars" =:
          "/'nope/ *nope\"*" =?>
          para ("/'nope/" <> space <> "*nope\"*")

      , "Commata are forbidden border chars" =:
          "/nada,/" =?>
          para "/nada,/"

      , "Markup should work properly after a blank line" =:
        unlines ["foo", "", "/bar/"] =?>
        (para $ text "foo") <> (para $ emph $ text "bar")

      , "Inline math must stay within three lines" =:
          unlines [ "$a", "b", "c$", "$d", "e", "f", "g$" ] =?>
          para ((math "a\nb\nc") <> space <>
                spcSep [ "$d", "e", "f", "g$" ])

      , "Single-character math" =:
          "$a$ $b$! $c$?" =?>
          para (spcSep [ math "a"
                       , "$b$!"
                       , (math "c") <> "?"
                       ])

      , "Markup may not span more than two lines" =:
          unlines [ "/this *is +totally", "nice+ not*", "emph/" ] =?>
          para (spcSep [ "/this"
                       , (strong (spcSep
                                  [ "is"
                                  , (strikeout ("totally" <> space <> "nice"))
                                  , "not"
                                  ]))
                       , "emph/" ])

      , "Sub- and superscript expressions" =:
         unlines [ "a_(a(b)(c)d)"
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
         para (spcSep [ "a" <> subscript "(a(b)(c)d)"
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

      , "Image" =:
          "[[./sunset.jpg]]" =?>
          (para $ image "./sunset.jpg" "" "")

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
          "[[sunset.png][dusk.svg]]" =?>
          (para $ link "sunset.png" "" (image "dusk.svg" "" ""))

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

      , "Anchor" =:
          "<<anchor>> Link here later." =?>
          (para $ spanWith ("anchor", [], []) mempty <>
                  "Link" <> space <> "here" <> space <> "later.")

      , "Inline code block" =:
          "src_emacs-lisp{(message \"Hello\")}" =?>
          (para $ codeWith ( ""
                           , [ "commonlisp", "rundoc-block" ]
                           , [ ("rundoc-language", "emacs-lisp") ])
                           "(message \"Hello\")")

      , "Inline code block with arguments" =:
          "src_sh[:export both :results output]{echo 'Hello, World'}" =?>
          (para $ codeWith ( ""
                           , [ "bash", "rundoc-block" ]
                           , [ ("rundoc-language", "sh")
                             , ("rundoc-export", "both")
                             , ("rundoc-results", "output")
                             ]
                           )
                           "echo 'Hello, World'")

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

      , "Inline LaTeX symbol" =:
          "\\dots" =?>
          para "…"

      , "Inline LaTeX command" =:
          "\\textit{Emphasised}" =?>
          para (emph "Emphasised")

      , "Inline LaTeX math symbol" =:
          "\\tau" =?>
          para (emph "τ")

      , "Unknown inline LaTeX command" =:
          "\\notacommand{foo}" =?>
          para (rawInline "latex" "\\notacommand{foo}")

      , "MathML symbol in LaTeX-style" =:
          "There is a hackerspace in Lübeck, Germany, called nbsp (unicode symbol: '\\nbsp')." =?>
          para ("There is a hackerspace in Lübeck, Germany, called nbsp (unicode symbol: ' ').")

      , "MathML symbol in LaTeX-style, including braces" =:
          "\\Aacute{}stor" =?>
          para "Ástor"

      , "MathML copy sign" =:
          "\\copy" =?>
          para "©"

      , "LaTeX citation" =:
          "\\cite{Coffee}" =?>
          let citation = Citation
                         { citationId = "Coffee"
                         , citationPrefix = []
                         , citationSuffix = []
                         , citationMode = AuthorInText
                         , citationNoteNum = 0
                         , citationHash = 0}
          in (para . cite [citation] $ rawInline "latex" "\\cite{Coffee}")
      ]

  , testGroup "Meta Information" $
      [ "Comment" =:
          "# Nothing to see here" =?>
          (mempty::Blocks)

      , "Not a comment" =:
          "#-tag" =?>
          para "#-tag"

      , "Comment surrounded by Text" =:
          unlines [ "Before"
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
            meta = setMeta "author" (MetaInlines author) $ nullMeta
        in Pandoc meta mempty

      , "Date" =:
        "#+Date: Feb. *28*, 2014" =?>
        let date = toList . spcSep $ [ "Feb.", (strong "28") <> ",", "2014" ]
            meta = setMeta "date" (MetaInlines date) $ nullMeta
        in Pandoc meta mempty

      , "Description" =:
        "#+DESCRIPTION: Explanatory text" =?>
        let description = toList . spcSep $ [ "Explanatory", "text" ]
            meta = setMeta "description" (MetaInlines description) $ nullMeta
        in Pandoc meta mempty

      , "Properties drawer" =:
          unlines [ "  :PROPERTIES:"
                  , "  :setting: foo"
                  , "  :END:"
                  ] =?>
          (mempty::Blocks)

      , "Logbook drawer" =:
          unlines [ "  :LogBook:"
                  , "  - State \"DONE\"       from \"TODO\"       [2014-03-03 Mon 11:00]"
                  , "  :END:"
                  ] =?>
          (mempty::Blocks)

      , "Drawer surrounded by text" =:
          unlines [ "Before"
                  , ":PROPERTIES:"
                  , ":END:"
                  , "After"
                  ] =?>
          para "Before" <> para "After"

      , "Drawer start is the only text in first line of a drawer" =:
          unlines [ "  :LOGBOOK: foo"
                  , "  :END:"
                  ] =?>
          para (spcSep [ ":LOGBOOK:", "foo", ":END:" ])

      , "Drawers with unknown names are just text" =:
          unlines [ ":FOO:"
                  , ":END:"
                  ] =?>
          para (":FOO:" <> space <> ":END:")

      , "Anchor reference" =:
          unlines [ "<<link-here>> Target."
                  , ""
                  , "[[link-here][See here!]]"
                  ] =?>
          (para (spanWith ("link-here", [], []) mempty <> "Target.") <>
           para (link "#link-here" "" ("See" <> space <> "here!")))

      , "Search links are read as emph" =:
          "[[Wally][Where's Wally?]]" =?>
          (para (emph $ "Where's" <> space <> "Wally?"))

      , "Link to nonexistent anchor" =:
          unlines [ "<<link-here>> Target."
                  , ""
                  , "[[link$here][See here!]]"
                  ] =?>
          (para (spanWith ("link-here", [], []) mempty <> "Target.") <>
           para (emph ("See" <> space <> "here!")))

      , "Link abbreviation" =:
          unlines [ "#+LINK: wp https://en.wikipedia.org/wiki/%s"
                  , "[[wp:Org_mode][Wikipedia on Org-mode]]"
                  ] =?>
          (para (link "https://en.wikipedia.org/wiki/Org_mode" ""
                      ("Wikipedia" <> space <> "on" <> space <> "Org-mode")))

      , "Link abbreviation, defined after first use" =:
          unlines [ "[[zl:non-sense][Non-sense articles]]"
                  , "#+LINK: zl http://zeitlens.com/tags/%s.html"
                  ] =?>
          (para (link "http://zeitlens.com/tags/non-sense.html" ""
                      ("Non-sense" <> space <> "articles")))

      , "Link abbreviation, URL encoded arguments" =:
          unlines [ "#+link: expl http://example.com/%h/foo"
                  , "[[expl:Hello, World!][Moin!]]"
                  ] =?>
          (para (link "http://example.com/Hello%2C%20World%21/foo" "" "Moin!"))

      , "Link abbreviation, append arguments" =:
          unlines [ "#+link: expl http://example.com/"
                  , "[[expl:foo][bar]]"
                  ] =?>
          (para (link "http://example.com/foo" "" "bar"))
      ]

  , testGroup "Basic Blocks" $
      [ "Paragraph" =:
          "Paragraph\n" =?>
          para "Paragraph"

      , "First Level Header" =:
          "* Headline\n" =?>
          header 1 "Headline"

      , "Third Level Header" =:
          "*** Third Level Headline\n" =?>
          header 3 ("Third" <> space <>
                    "Level" <> space <>
                    "Headline")

      , "Compact Headers with Paragraph" =:
          unlines [ "* First Level"
                  , "** Second Level"
                  , "   Text"
                  ] =?>
          mconcat [ header 1 ("First" <> space <> "Level")
                  , header 2 ("Second" <> space <> "Level")
                  , para "Text"
                  ]

      , "Separated Headers with Paragraph" =:
          unlines [ "* First Level"
                  , ""
                  , "** Second Level"
                  , ""
                  , "   Text"
                  ] =?>
          mconcat [ header 1 ("First" <> space <> "Level")
                  , header 2 ("Second" <> space <> "Level")
                  , para "Text"
                  ]

      , "Headers not preceded by a blank line" =:
          unlines [ "** eat dinner"
                  , "Spaghetti and meatballs tonight."
                  , "** walk dog"
                  ] =?>
          mconcat [ header 2 ("eat" <> space <> "dinner")
                  , para $ spcSep [ "Spaghetti", "and", "meatballs", "tonight." ]
                  , header 2 ("walk" <> space <> "dog")
                  ]

      , "Comment Trees" =:
          unlines [ "* COMMENT A comment tree"
                  , "  Not much going on here"
                  , "** This will be dropped"
                  , "* Comment tree above"
                  ] =?>
          header 1 "Comment tree above"

      , "Nothing but a COMMENT header" =:
          "* COMMENT Test" =?>
          (mempty::Blocks)

      , "Paragraph starting with an asterisk" =:
          "*five" =?>
          para "*five"

      , "Paragraph containing asterisk at beginning of line" =:
          unlines [ "lucky"
                  , "*star"
                  ] =?>
          para ("lucky" <> space <> "*star")

      , "Example block" =:
          unlines [ ": echo hello"
                  , ": echo dear tester"
                  ] =?>
          codeBlockWith ("", ["example"], []) "echo hello\necho dear tester\n"

      , "Example block surrounded by text" =:
          unlines [ "Greetings"
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
          unlines [ "before"
                  , "-----"
                  , "after"
                  ] =?>
          mconcat [ para "before"
                  , horizontalRule
                  , para "after"
                  ]

      , "Not a Horizontal Rule" =:
          "----- five dashes" =?>
          (para $ spcSep [ "-----", "five", "dashes" ])

      , "Comment Block" =:
          unlines [ "#+BEGIN_COMMENT"
                  , "stuff"
                  , "bla"
                  , "#+END_COMMENT"] =?>
          (mempty::Blocks)

      , "Figure" =:
          unlines [ "#+caption: A very courageous man."
                  , "#+name: goodguy"
                  , "[[edward.jpg]]"
                  ] =?>
          para (image "edward.jpg" "fig:goodguy" "A very courageous man.")

      , "Unnamed figure" =:
          unlines [ "#+caption: A great whistleblower."
                  , "[[snowden.png]]"
                  ] =?>
          para (image "snowden.png" "" "A great whistleblower.")

      , "Figure with `fig:` prefix in name" =:
          unlines [ "#+caption: Used as a metapher in evolutionary biology."
                  , "#+name: fig:redqueen"
                  , "[[the-red-queen.jpg]]"
                  ] =?>
          para (image "the-red-queen.jpg" "fig:redqueen"
                      "Used as a metapher in evolutionary biology.")

      , "Footnote" =:
          unlines [ "A footnote[1]"
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
          unlines [ "Footnotes[fn:1][fn:2]"
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

      , "Footnote followed by header" =:
          unlines [ "Another note[fn:yay]"
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
          , header 2 "Headline"
          ]
      ]

  , testGroup "Lists" $
      [ "Simple Bullet Lists" =:
          ("- Item1\n" ++
           "- Item2\n") =?>
          bulletList [ plain "Item1"
                     , plain "Item2"
                     ]

      , "Indented Bullet Lists" =:
          ("   - Item1\n" ++
           "   - Item2\n") =?>
          bulletList [ plain "Item1"
                     , plain "Item2"
                     ]

      , "Multi-line Bullet Lists" =:
          ("- *Fat\n" ++
           "  Tony*\n" ++
           "- /Sideshow\n" ++
           " Bob/") =?>
          bulletList [ plain $ strong ("Fat" <> space <> "Tony")
                     , plain $ emph ("Sideshow" <> space <> "Bob")
                     ]

      , "Nested Bullet Lists" =:
          ("- Discovery\n" ++
           "  + One More Time\n" ++
           "  + Harder, Better, Faster, Stronger\n" ++
           "- Homework\n" ++
           "  + Around the World\n"++
           "- Human After All\n" ++
           "  + Technologic\n" ++
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
                  , header 1 "Homework"
                  ]

      , "Bullet List Unindented with trailing Header" =:
          ("- Discovery\n\
           \- Homework\n\
           \* NotValidListItem") =?>
          mconcat [ bulletList [ plain "Discovery"
                               , plain "Homework"
                               ]
                  , header 1 "NotValidListItem"
                  ]

      , "Simple Ordered List" =:
          ("1. Item1\n" ++
           "2. Item2\n") =?>
          let listStyle = (1, DefaultStyle, DefaultDelim)
              listStructure = [ plain "Item1"
                              , plain "Item2"
                              ]
          in orderedListWith listStyle listStructure

      , "Simple Ordered List with Parens" =:
          ("1) Item1\n" ++
           "2) Item2\n") =?>
          let listStyle = (1, DefaultStyle, DefaultDelim)
              listStructure = [ plain "Item1"
                              , plain "Item2"
                              ]
          in orderedListWith listStyle listStructure

      , "Indented Ordered List" =:
          (" 1. Item1\n" ++
           " 2. Item2\n") =?>
          let listStyle = (1, DefaultStyle, DefaultDelim)
              listStructure = [ plain "Item1"
                              , plain "Item2"
                              ]
          in orderedListWith listStyle listStructure

      , "Nested Ordered Lists" =:
          ("1. One\n" ++
           "   1. One-One\n" ++
           "   2. One-Two\n" ++
           "2. Two\n" ++
           "   1. Two-One\n"++
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
          ("- Emacs\n" ++
           "  1. Org\n") =?>
          bulletList [ (plain "Emacs") <>
                       (orderedList [ plain "Org"])
                     ]

      , "Bullet List in Ordered List" =:
          ("1. GNU\n" ++
           "   - Freedom\n") =?>
          orderedList [ (plain "GNU") <> bulletList [ (plain "Freedom") ] ]

      , "Definition List" =:
          unlines [ "- PLL :: phase-locked loop"
                  , "- TTL ::"
                  , "  transistor-transistor logic"
                  , "- PSK::phase-shift keying"
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
          unlines [ "- ATP :: adenosine 5' triphosphate"
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
                  , header 1 "header"
                  ]

      , "Loose bullet list" =:
          unlines [ "- apple"
                  , ""
                  , "- orange"
                  , ""
                  , "- peach"
                  ] =?>
          bulletList [ para "apple"
                     , para "orange"
                     , para "peach"
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
          unlines [ "| One   |"
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
          simpleTable' 1 mempty mempty

      , "Glider Table" =:
          unlines [ "| 1 | 0 | 0 |"
                  , "| 0 | 1 | 1 |"
                  , "| 1 | 1 | 0 |"
                  ] =?>
          simpleTable' 3 mempty
                       [ [ plain "1", plain "0", plain "0" ]
                       , [ plain "0", plain "1", plain "1" ]
                       , [ plain "1", plain "1", plain "0" ]
                       ]

      , "Table between Paragraphs" =:
          unlines [ "Before"
                  , "| One | Two |"
                  , "After"
                  ] =?>
          mconcat [ para "Before"
                  , simpleTable' 2 mempty [ [ plain "One", plain "Two" ] ]
                  , para "After"
                  ]

      , "Table with Header" =:
          unlines [ "| Species      | Status       |"
                  , "|--------------+--------------|"
                  , "| cervisiae    | domesticated |"
                  , "| paradoxus    | wild         |"
                  ] =?>
          simpleTable [ plain "Species", plain "Status" ]
                      [ [ plain "cervisiae", plain "domesticated" ]
                      , [ plain "paradoxus", plain "wild" ]
                      ]

      , "Table with final hline" =:
          unlines [ "| cervisiae    | domesticated |"
                  , "| paradoxus    | wild         |"
                  , "|--------------+--------------|"
                  ] =?>
          simpleTable' 2 mempty
                [ [ plain "cervisiae", plain "domesticated" ]
                 , [ plain "paradoxus", plain "wild" ]
                ]

      , "Table in a box" =:
          unlines [ "|---------|---------|"
                  , "| static  | Haskell |"
                  , "| dynamic | Lisp    |"
                  , "|---------+---------|"
                  ] =?>
          simpleTable' 2 mempty
                [ [ plain "static", plain "Haskell" ]
                , [ plain "dynamic", plain "Lisp" ]
                ]

      , "Table with alignment row" =:
          unlines [ "| Numbers | Text | More |"
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
          unlines [ "| Numbers | Text "
                  , "|-"
                  , "| <c>     | <r>  |"
                  , "| 1       | One  | foo  |"
                  , "| 2"
                  ] =?>
          table "" (zip [AlignCenter, AlignRight, AlignDefault] [0, 0, 0])
                [ plain "Numbers", plain "Text" , plain mempty ]
                [ [ plain "1"      , plain "One"  , plain "foo"  ]
                , [ plain "2"      , plain mempty , plain mempty  ]
                ]

      , "Table with caption" =:
          unlines [ "#+CAPTION: Hitchhiker's Multiplication Table"
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
           unlines [ "  #+BEGIN_SRC haskell"
                   , "  main = putStrLn greeting"
                   , "    where greeting = \"moin\""
                   , "  #+END_SRC" ] =?>
           let attr' = ("", ["haskell"], [])
               code' = "main = putStrLn greeting\n" ++
                       "  where greeting = \"moin\"\n"
           in codeBlockWith attr' code'

      , "Source block between paragraphs" =:
           unlines [ "Low German greeting"
                   , "  #+BEGIN_SRC haskell"
                   , "  main = putStrLn greeting"
                   , "    where greeting = \"Moin!\""
                   , "  #+END_SRC" ] =?>
           let attr' = ("", ["haskell"], [])
               code' = "main = putStrLn greeting\n" ++
                        "  where greeting = \"Moin!\"\n"
           in mconcat [ para $ spcSep [ "Low", "German", "greeting"  ]
                      , codeBlockWith attr' code'
                      ]
      , "Source block with rundoc/babel arguments" =:
           unlines [ "#+BEGIN_SRC emacs-lisp :exports both"
                   , "(progn (message \"Hello, World!\")"
                   , "       (+ 23 42))"
                   , "#+END_SRC" ] =?>
           let classes = [ "commonlisp"  -- as kate doesn't know emacs-lisp syntax
                         , "rundoc-block"
                         ]
               params = [ ("rundoc-language", "emacs-lisp")
                        , ("rundoc-exports", "both")
                        ]
               code' = unlines [ "(progn (message \"Hello, World!\")"
                               , "       (+ 23 42))" ]
           in codeBlockWith ("", classes, params) code'

      , "Source block with results and :exports both" =:
           unlines [ "#+BEGIN_SRC emacs-lisp :exports both"
                   , "(progn (message \"Hello, World!\")"
                   , "       (+ 23 42))"
                   , "#+END_SRC"
                   , ""
                   , "#+RESULTS:"
                   , ": 65"] =?>
           let classes = [ "commonlisp"  -- as kate doesn't know emacs-lisp syntax
                         , "rundoc-block"
                         ]
               params = [ ("rundoc-language", "emacs-lisp")
                        , ("rundoc-exports", "both")
                        ]
               code' = unlines [ "(progn (message \"Hello, World!\")"
                               , "       (+ 23 42))" ]
               results' = "65\n"
           in codeBlockWith ("", classes, params) code'
              <>
              codeBlockWith ("", ["example"], []) results'

      , "Source block with results and :exports code" =:
           unlines [ "#+BEGIN_SRC emacs-lisp :exports code"
                   , "(progn (message \"Hello, World!\")"
                   , "       (+ 23 42))"
                   , "#+END_SRC"
                   , ""
                   , "#+RESULTS:"
                   , ": 65" ] =?>
           let classes = [ "commonlisp"  -- as kate doesn't know emacs-lisp syntax
                         , "rundoc-block"
                         ]
               params = [ ("rundoc-language", "emacs-lisp")
                        , ("rundoc-exports", "code")
                        ]
               code' = unlines [ "(progn (message \"Hello, World!\")"
                               , "       (+ 23 42))" ]
           in codeBlockWith ("", classes, params) code'

      , "Source block with results and :exports results" =:
           unlines [ "#+BEGIN_SRC emacs-lisp :exports results"
                   , "(progn (message \"Hello, World!\")"
                   , "       (+ 23 42))"
                   , "#+END_SRC"
                   , ""
                   , "#+RESULTS:"
                   , ": 65" ] =?>
           let results' = "65\n"
           in codeBlockWith ("", ["example"], []) results'

      , "Source block with results and :exports none" =:
           unlines [ "#+BEGIN_SRC emacs-lisp :exports none"
                   , "(progn (message \"Hello, World!\")"
                   , "       (+ 23 42))"
                   , "#+END_SRC"
                   , ""
                   , "#+RESULTS:"
                   , ": 65" ] =?>
           rawBlock "html" ""

      , "Example block" =:
           unlines [ "#+begin_example"
                   , "A chosen representation of"
                   , "a rule."
                   , "#+eND_exAMPle"
                   ] =?>
           codeBlockWith ("", ["example"], [])
                         "A chosen representation of\na rule.\n"

      , "HTML block" =:
           unlines [ "#+BEGIN_HTML"
                   , "<aside>HTML5 is pretty nice.</aside>"
                   , "#+END_HTML"
                   ] =?>
           rawBlock "html" "<aside>HTML5 is pretty nice.</aside>\n"

      , "Quote block" =:
           unlines [ "#+BEGIN_QUOTE"
                   , "/Niemand/ hat die Absicht, eine Mauer zu errichten!"
                   , "#+END_QUOTE"
                   ] =?>
           blockQuote (para (spcSep [ emph "Niemand", "hat", "die", "Absicht,"
                                    , "eine", "Mauer", "zu", "errichten!"
                                    ]))

      , "Verse block" =:
          unlines [ "The first lines of Goethe's /Faust/:"
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
          , para $ mconcat
              [ spcSep [ "Habe", "nun,", "ach!", "Philosophie," ]
              , linebreak
              , spcSep [ "Juristerei", "und", "Medizin," ]
              , linebreak
              , spcSep [ "Und", "leider", "auch", "Theologie!" ]
              , linebreak
              , spcSep [ "Durchaus", "studiert,", "mit", "heißem", "Bemühn." ]
              ]
          ]

      , "LaTeX fragment" =:
          unlines [ "\\begin{equation}"
                  , "X_i = \\begin{cases}"
                  , "      G_{\\alpha(i)} & \\text{if }\\alpha(i-1) = \\alpha(i)\\\\"
                  , "      C_{\\alpha(i)} & \\text{otherwise}"
                  , "      \\end{cases}"
                  , "\\end{equation}"
                  ] =?>
          rawBlock "latex"
                   (unlines [ "\\begin{equation}"
                            , "X_i = \\begin{cases}"
                            , "      G_{\\alpha(i)} & \\text{if }\\alpha(i-1) =" ++
                              " \\alpha(i)\\\\"
                            , "      C_{\\alpha(i)} & \\text{otherwise}"
                            , "      \\end{cases}"
                            , "\\end{equation}"
                            ])

      , "Code block with caption" =:
          unlines [ "#+CAPTION: Functor laws in Haskell"
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
          unlines [ "#+begin_html"
                  , ""
                  , "<span>boring</span>"
                  , ""
                  , "#+end_html"
                  ] =?>
          rawBlock "html" "\n<span>boring</span>\n\n"

      , "Non-letter chars in source block parameters" =:
          unlines [ "#+BEGIN_SRC C :tangle xxxx.c :city Zürich"
                  , "code body"
                  , "#+END_SRC"
                  ] =?>
          let classes = [ "c", "rundoc-block" ]
              params  = [ ("rundoc-language", "C")
                        , ("rundoc-tangle", "xxxx.c")
                        , ("rundoc-city", "Zürich")
                        ]
          in codeBlockWith ( "", classes, params) "code body\n"
      ]
  ]
