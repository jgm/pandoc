{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.Org (tests) where

import Text.Pandoc.Definition
import Test.Framework
import Tests.Helpers
import Tests.Arbitrary()
import Text.Pandoc.Builder
import Text.Pandoc
import Data.List (intersperse)
import Data.Monoid (mempty, mconcat)

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

      , "Code" =:
          "=Robot.rock()=" =?>
          para (code "Robot.rock()")

      , "Math" =:
          "$E=mc^2$" =?>
           para (math "E=mc^2")

      , "Verbatim" =:
          "~word for word~" =?>
          para (rawInline "" "word for word")

      , "Symbol" =:
          "A * symbol" =?>
          para (str "A" <> space <> str "*" <> space <> "symbol")

      , "Superscript single char" =:
          "2^n" =?>
          para (str "2" <> superscript "n")

      , "Superscript multi char" =:
          "2^{n-1}" =?>
          para (str "2" <> superscript "n-1")

      , "Subscript single char" =:
          "a_n" =?>
          para (str "a" <> subscript "n")

      , "Subscript multi char" =:
          "a_{n+1}" =?>
          para (str "a" <> subscript "n+1")

      , "Markup-chars not occuring on word break are symbols" =:
          unlines [ "this+that+ +so+on"
                  , "seven*eight* nine*"
                  , "+not+funny+"
                  , "this == self"
                  ] =?>
          para (spcSep [ "this+that+", "+so+on"
                       , "seven*eight*", "nine*"
                       , strikeout "not+funny"
                       , "this" <> space <> "==" <> space <> "self"
                       ])

      , "Adherence to Org's rules for markup borders" =:
          "/t/& a/ / ./r/ (*l*) /e/! /b/." =?>
          para (spcSep [ emph $ "t/&" <> space <> "a"
                       , "/"
                       , "./r/"
                       , "(" <> (strong "l") <> ")"
                       , (emph "e") <> "!"
                       , (emph "b") <> "."
                       ])

      , "Inline math must stay within three lines" =:
          unlines [ "$a", "b", "c$", "$d", "e", "f", "g$" ] =?>
          para ((math "a\nb\nc") <> space <>
                spcSep [ "$d", "e", "f", "g$" ])

      , "Markup may not span more than two lines" =:
          unlines [ "/this *is +totally", "nice+ not*", "emph/" ] =?>
          para (spcSep [ "/this"
                       , (strong (spcSep
                                  [ "is"
                                  , (strikeout ("totally" <> space <> "nice"))
                                  , "not"
                                  ]))
                       , "emph/" ])

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

      , "Image link" =:
          "[[sunset.png][dusk.svg]]" =?>
          (para $ link "sunset.png" "" (image "dusk.svg" "" ""))
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

      , "Source Block in Text" =:
          unlines [ "Low German greeting"
                  , "  #+BEGIN_SRC haskell"
                  , "  main = putStrLn greeting"
                  , "    where greeting = \"moin\""
                  , "  #+END_SRC" ] =?>
          let attr' = ("", ["haskell"], [])
              code' = "main = putStrLn greeting\n" ++
                       "  where greeting = \"moin\"\n"
          in mconcat [ para $ spcSep [ "Low", "German", "greeting"  ]
                     , codeBlockWith attr' code'
                     ]

      , "Source Block" =:
          unlines [ "  #+BEGIN_SRC haskell"
                  , "  main = putStrLn greeting"
                  , "    where greeting = \"moin\""
                  , "  #+END_SRC" ] =?>
          let attr' = ("", ["haskell"], [])
              code' = "main = putStrLn greeting\n" ++
                       "  where greeting = \"moin\"\n"
          in codeBlockWith attr' code'
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
                       [ para "Discovery"
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
                       [ para "Homework"
                       , bulletList [ plain ("Around" <> space <>
                                             "the" <> space <>
                                             "World")
                                    ]
                       ]
                     , mconcat
                       [ para ("Human" <> space <> "After" <> space <> "All")
                       , bulletList [ plain "Technologic"
                                    , plain ("Robot" <> space <> "Rock")
                                    ]
                       ]
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
                                [ para "One"
                                , orderedList [ plain "One-One"
                                              , plain "One-Two"
                                              ]
                                ]
                              , mconcat
                                [ para "Two"
                                , orderedList [ plain "Two-One"
                                              , plain "Two-Two"
                                              ]
                                ]
                              ]
          in orderedListWith listStyle listStructure

      , "Ordered List in Bullet List" =:
          ("- Emacs\n" ++
           "  1. Org\n") =?>
          bulletList [ (para "Emacs") <>
                       (orderedList [ plain "Org"])
                     ]

      , "Bullet List in Ordered List" =:
          ("1. GNU\n" ++
           "   - Freedom\n") =?>
          orderedList [ (para "GNU") <> bulletList [ (plain "Freedom") ] ]

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
                                     [ para  $ "phase-shift" <> space <> "keying"
                                     , plain $ spcSep [ "a", "digital"
                                                      , "modulation", "scheme" ]
                                     ]
                                   ]
                                   )
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
      ]
  ]
