{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Txt2Tags
   Copyright   : © 2014-2020 John MacFarlane,
                 © 2014 Matthew Pickering
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Tests for the Txt2Tags reader.
-}
module Tests.Readers.Txt2Tags (tests) where

import Prelude
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

t2t :: Text -> Pandoc
-- t2t = handleError . readTxt2Tags (T2TMeta "date" "mtime" "in" "out") def
t2t = purely $ \s -> do
  setInputFiles ["in"]
  setOutputFile (Just "out")
  readTxt2Tags def s

infix 4 =:
(=:) :: ToString c
     => String -> (Text, c) -> TestTree
(=:) = test t2t

spcSep :: [Inlines] -> Inlines
spcSep = mconcat . intersperse space

simpleTable' :: Int
             -> [Blocks]
             -> [[Blocks]]
             -> Blocks
simpleTable' n = simpleTable'' $ replicate n (AlignCenter, ColWidthDefault)

simpleTable'' :: [ColSpec] -> [Blocks] -> [[Blocks]] -> Blocks
simpleTable'' spec headers rows
  = table emptyCaption
          spec
          (TableHead nullAttr $ toHeaderRow headers)
          [TableBody nullAttr 0 [] $ map toRow rows]
          (TableFoot nullAttr [])
  where
    toRow = Row nullAttr . map simpleCell
    toHeaderRow l = if null l then [] else [toRow l]

tests :: [TestTree]
tests =
  [ testGroup "Inlines"
      [ "Plain String" =:
          "Hello, World" =?>
          para (spcSep [ "Hello,", "World" ])

      , "Emphasis" =:
          "//Planet Punk//" =?>
          para (emph . spcSep $ ["Planet", "Punk"])

      , "Strong" =:
          "**Cider**" =?>
          para (strong "Cider")

      , "Strong Emphasis" =:
          "//**strength**//" =?>
          para (emph . strong $ "strength")

      , "Strikeout" =:
          "--Kill Bill--" =?>
          para (strikeout . spcSep $ [ "Kill", "Bill" ])

      , "Verbatim" =:
          "``Robot.rock()``" =?>
          para (code "Robot.rock()")

      , "Symbol" =:
          "A * symbol" =?>
          para (str "A" <> space <> str "*" <> space <> "symbol")

      , "No empty markup" =:
          "//// **** ____ ---- ```` \"\"\"\" ''''" =?>
          para (spcSep [ "////", "****", "____", "----", "````", "\"\"\"\"", "''''" ])

      , "Inline markup is greedy" =:
          "***** ///// _____ ----- ````` \"\"\"\"\" '''''" =?>
          para (spcSep [strong "*", emph "/", underline "_"
                       , strikeout "-", code "`", text "\""
                       , rawInline "html" "'"])
      , "Markup must be greedy" =:
          "**********    //////////    __________    ----------    ``````````   \"\"\"\"\"\"\"\"\"\"   ''''''''''" =?>
                      para (spcSep [strong "******", emph "//////", underline "______"
                       , strikeout "------", code "``````", text "\"\"\"\"\"\""
                       , rawInline "html" "''''''"])
      , "Inlines must be glued" =:
          "** a** **a ** ** a **" =?>
          para (text "** a** **a ** ** a **")

      , "Macros: Date" =:
          "%%date" =?>
            para "1970-01-01"
      , "Macros: Mod Time" =:
          "%%mtime" =?>
            para (str "")
      , "Macros: Infile" =:
          "%%infile" =?>
            para "in"
      , "Macros: Outfile" =:
          "%%outfile" =?>
            para "out"
      , "Autolink" =:
          "http://www.google.com" =?>
            para (link "http://www.google.com" "" (str "http://www.google.com"))
      , "JPEG Image" =:
          "[image.jpg]" =?>
            para (image "image.jpg" "" mempty)
      , "PNG Image" =:
          "[image.png]" =?>
            para (image "image.png" "" mempty)

      , "Link" =:
          "[title http://google.com]" =?>
            para (link "http://google.com" "" (str "title"))

      , "Image link" =:
          "[[image.jpg] abc]" =?>
            para (link "abc" "" (image "image.jpg" "" mempty))
      , "Invalid link: No trailing space" =:
          "[title invalid ]" =?>
            para (text "[title invalid ]")


      ]

  , testGroup "Basic Blocks"
      ["Paragraph, lines grouped together" =:
          "A paragraph\n A blank line ends the \n current paragraph\n"
            =?> para "A paragraph\n A blank line ends the\n current paragraph"
      , "Paragraph, ignore leading and trailing spaces" =:
          "   Leading and trailing spaces are ignored.   \n" =?>
            para "Leading and trailing spaces are ignored."
      , "Comment line in paragraph" =:
          "A comment line can be placed inside a paragraph.\n% this comment will be ignored \nIt will not affect it.\n"
          =?> para "A comment line can be placed inside a paragraph.\nIt will not affect it."
      , "Paragraph" =:
          "Paragraph\n" =?>
          para "Paragraph"

      , "First Level Header" =:
          "+ Headline +\n" =?>
          header 1 "Headline"

      , "Third Level Header" =:
          "=== Third Level Headline ===\n" =?>
          header 3 ("Third" <> space <>
                    "Level" <> space <>
                    "Headline")

      , "Header with label" =:
          "= header =[label]" =?>
            headerWith ("label", [], []) 1 "header"

      , "Invalid header, mismatched delimiters" =:
          "== header =" =?>
            para (text "== header =")

      , "Invalid header, spaces in label" =:
          "== header ==[ haha ]" =?>
            para (text "== header ==[ haha ]")

      , "Invalid header, invalid label character" =:
          "== header ==[lab/el]" =?>
            para (text "== header ==[lab/el]")
      , "Headers not preceded by a blank line" =:
          T.unlines [ "++ eat dinner ++"
                  , "Spaghetti and meatballs tonight."
                  , "== walk dog =="
                  ] =?>
          mconcat [ header 2 ("eat" <> space <> "dinner")
                  , para $ spcSep [ "Spaghetti", "and", "meatballs", "tonight." ]
                  , header 2 ("walk" <> space <> "dog")
                  ]

      , "Paragraph starting with an equals" =:
          "=five" =?>
          para "=five"

      , "Paragraph containing asterisk at beginning of line" =:
          T.unlines [ "lucky"
                  , "*star"
                  ] =?>
          para ("lucky" <> softbreak <> "*star")

      , "Horizontal Rule" =:
          T.unlines [ "before"
                  , T.replicate 20 "-"
                  , T.replicate 20 "="
                  , T.replicate 20 "_"
                  , "after"
                  ] =?>
          mconcat [ para "before"
                  , horizontalRule
                  , horizontalRule
                  , horizontalRule
                  , para "after"
                  ]

      , "Comment Block" =:
          T.unlines [ "%%%"
                  , "stuff"
                  , "bla"
                  , "%%%"] =?>
          (mempty::Blocks)


    ]

  , testGroup "Lists"
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
                       , orderedList [ plain ("One" <> space <>
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
                       , orderedList [ plain ("Around" <> space <>
                                             "the" <> space <>
                                             "World")
                                    ]
                       ]
                     , mconcat
                       [ plain ("Human" <> space <> "After" <> space <> "All")
                       , orderedList [ plain "Technologic"
                                    , plain ("Robot" <> space <> "Rock")
                                    ]
                       ]
                     ]

      , "Simple Ordered List" =:
          ("+ Item1\n" <>
           "+ Item2\n") =?>
          let listStyle = (1, DefaultStyle, DefaultDelim)
              listStructure = [ plain "Item1"
                              , plain "Item2"
                              ]
          in orderedListWith listStyle listStructure


      , "Indented Ordered List" =:
          (" + Item1\n" <>
           " + Item2\n") =?>
          let listStyle = (1, DefaultStyle, DefaultDelim)
              listStructure = [ plain "Item1"
                              , plain "Item2"
                              ]
          in orderedListWith listStyle listStructure

      , "Nested Ordered Lists" =:
          ("+ One\n" <>
           "   + One-One\n" <>
           "   + One-Two\n" <>
           "+ Two\n" <>
           "   + Two-One\n"<>
           "   + Two-Two\n") =?>
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
           "  + Org\n") =?>
          bulletList [ plain "Emacs" <>
                       orderedList [ plain "Org"]
                     ]

      , "Bullet List in Ordered List" =:
          ("+ GNU\n" <>
           "   - Freedom\n") =?>
          orderedList [ plain "GNU" <> bulletList [ plain "Freedom" ] ]

      , "Definition List" =:
          T.unlines [ ": PLL"
                  , "  phase-locked loop"
                  , ": TTL"
                  , "  transistor-transistor logic"
                  , ": PSK"
                  , "  a digital"
                  ] =?>
          definitionList [ ("PLL", [ plain $ "phase-locked" <> space <> "loop" ])
                         , ("TTL", [ plain $ "transistor-transistor" <> space <> "logic" ])
                         , ("PSK", [ plain $ "a" <> space <> "digital" ])
                         ]


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
      ]

  , testGroup "Tables"
      [ "Single cell table" =:
          "| Test " =?>
          simpleTable' 1 mempty [[plain "Test"]]

      , "Multi cell table" =:
          "| One | Two |" =?>
           simpleTable' 2 mempty [ [ plain "One", plain "Two" ] ]

      , "Multi line table" =:
          T.unlines [ "| One |"
                  , "| Two |"
                  , "| Three |"
                  ] =?>
           simpleTable' 1 mempty
                        [ [ plain "One" ]
                        , [ plain "Two" ]
                        , [ plain "Three" ]
                        ]

      , "Empty table" =:
          "| |" =?>
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


      , "Table with Header" =:
          T.unlines [ "|| Species     | Status       |"
                  , "| cervisiae    | domesticated |"
                  , "| paradoxus | wild         |"
                  ] =?>
          simpleTable [ plain "Species", plain "Status" ]
                      [ [ plain "cervisiae", plain "domesticated" ]
                      , [ plain "paradoxus", plain "wild" ]
                      ]

      , "Table alignment determined by spacing" =:
          T.unlines [ "| Numbers |     Text | More    |"
                  , "| 1 |    One  |    foo  |"
                  , "| 2 |    Two  | bar  |"
                  ] =?>
          simpleTable''
            (zip
              [AlignCenter, AlignRight, AlignDefault]
              [ColWidthDefault, ColWidthDefault, ColWidthDefault])
            []
            [ [ plain "Numbers", plain "Text", plain "More" ]
            , [ plain "1"      , plain "One" , plain "foo"  ]
            , [ plain "2"      , plain "Two" , plain "bar"  ]
            ]

      , "Pipe within text doesn't start a table" =:
          "Ceci n'est pas une | pipe " =?>
          para (spcSep [ "Ceci", "n'est", "pas", "une", "|", "pipe" ])


      , "Table with differing row lengths" =:
          T.unlines [ "|| Numbers | Text "
                  , "| 1 | One  | foo  |"
                  , "| 2 "
                  ] =?>
          simpleTable''
            (zip
              [AlignCenter, AlignLeft, AlignLeft]
              [ColWidthDefault, ColWidthDefault, ColWidthDefault])
            [ plain "Numbers", plain "Text" , plain mempty ]
            [ [ plain "1"      , plain "One"  , plain "foo"  ]
            , [ plain "2"      , plain mempty , plain mempty  ]
            ]

      ]

    , testGroup "Blocks and fragments"
      [ "Source block" =:
           T.unlines [ "```"
                   , "main = putStrLn greeting"
                   , "  where greeting = \"moin\""
                   , "```" ] =?>
           let code' = "main = putStrLn greeting\n" <>
                       "  where greeting = \"moin\"\n"
           in codeBlock code'

      , "tagged block" =:
           T.unlines [ "'''"
                   , "<aside>HTML5 is pretty nice.</aside>"
                   , "'''"
                   ] =?>
           rawBlock "html" "<aside>HTML5 is pretty nice.</aside>\n"

      , "Quote block" =:
           T.unlines ["\t//Niemand// hat die Absicht, eine Mauer zu errichten!"
                   ] =?>
           blockQuote (para (spcSep [ emph "Niemand", "hat", "die", "Absicht,"
                                    , "eine", "Mauer", "zu", "errichten!"
                                    ]))

      ]
  ]
