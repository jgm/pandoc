{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Org.Block.Header
   Copyright   : © 2014-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Test parsing of org lists.
-}
module Tests.Readers.Org.Block.List (tests) where

import Data.Text (Text)
import Test.Tasty (TestTree)
import Tests.Helpers ((=?>), purely, test)
import Tests.Readers.Org.Shared ((=:), spcSep)
import Text.Pandoc (ReaderOptions (readerExtensions),
                    Extension (Ext_fancy_lists), def, enableExtension,
                    getDefaultExtensions, readOrg)
import Text.Pandoc.Builder
import qualified Data.Text as T

orgFancyLists :: Text -> Pandoc
orgFancyLists = purely $
  let extensionsFancy = enableExtension Ext_fancy_lists (getDefaultExtensions "org")
  in readOrg def{ readerExtensions = extensionsFancy }

tests :: [TestTree]
tests =
  [ "Simple Bullet Lists" =:
      ("- Item1\n" <>
       "- Item2\n") =?>
      bulletList [ plain "Item1"
                 , plain "Item2"
                 ]

  , "Simple Bullet List with Ignored Counter Cookie" =:
      ("- [@4] Item1\n" <>
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
       "  - Discovery\n\
        \ - Human After All\n" =?>
       mconcat [ bulletList [ plain "Discovery" ]
               , bulletList [ plain ("Human" <> space <> "After" <> space <> "All")]
               ]

  , "Header follows Bullet List" =:
      "  - Discovery\n\
       \  - Human After All\n\
       \* Homework" =?>
      mconcat [ bulletList [ plain "Discovery"
                           , plain ("Human" <> space <> "After" <> space <> "All")
                           ]
              , headerWith ("homework", [], []) 1 "Homework"
              ]

  , "Bullet List Unindented with trailing Header" =:
      "- Discovery\n\
       \- Homework\n\
       \* NotValidListItem" =?>
      mconcat [ bulletList [ plain "Discovery"
                           , plain "Homework"
                           ]
              , headerWith ("notvalidlistitem", [], []) 1 "NotValidListItem"
              ]

  , "Empty bullet points" =:
      T.unlines [ "-"
                , "- "
                ] =?>
      bulletList [ plain "", plain "" ]

  , "Task list" =:
    T.unlines [ "- [ ] nope"
              , "- [X] yup"
              , "- [-] started"
              , "  1. [X] sure"
              , "  2. [ ] nuh-uh"
              ] =?>
    bulletList [ plain "☐ nope", plain "☒ yup"
               , mconcat [ plain "☐ started"
                         , orderedList [plain "☒ sure", plain "☐ nuh-uh"]
                         ]
               ]

  , "Task List with Counter Cookies" =:
    T.unlines [ "- [ ] nope"
              , "- [@9] [X] yup"
              , "- [@a][-] started"
              , "  1. [@3][X] sure"
              , "  2. [@b] [ ] nuh-uh"
              ] =?>
    bulletList [ plain "☐ nope", plain "☒ yup"
               , mconcat [ plain "☐ started"
                         , orderedListWith
                           (3, DefaultStyle, DefaultDelim)
                           [plain "☒ sure", plain "☐ nuh-uh"]
                         ]
               ]

  , test orgFancyLists "Task with alphabetical markers and counter cookie" $
    T.unlines [ "- [ ] nope"
              , "- [@9] [X] yup"
              , "- [@a][-] started"
              , "  a) [@D][X] sure"
              , "  b) [@8] [ ] nuh-uh"
              ] =?>
    bulletList [ plain "☐ nope", plain "☒ yup"
               , mconcat [ plain "☐ started"
                         , orderedListWith
                           (4, LowerAlpha, OneParen)
                           [plain "☒ sure", plain "☐ nuh-uh"]
                         ]
               ]

  , "Simple Ordered List" =:
      ("1. Item1\n" <>
       "2. Item2\n") =?>
      let listStyle = (1, DefaultStyle, DefaultDelim)
          listStructure = [ plain "Item1"
                          , plain "Item2"
                          ]
      in orderedListWith listStyle listStructure

  , test orgFancyLists "Simple Ordered List with fancy lists extension" $
      ("1. Item1\n" <>
       "2. Item2\n") =?>
      let listStyle = (1, Decimal, Period)
          listStructure = [ plain "Item1"
                          , plain "Item2"
                          ]
      in orderedListWith listStyle listStructure

  , test orgFancyLists "Simple Ordered List with lower alpha marker" $
      ("a) Item1\n" <>
       "b) Item2\n") =?>
      let listStyle = (1, LowerAlpha, OneParen)
          listStructure = [ plain "Item1"
                          , plain "Item2"
                          ]
      in orderedListWith listStyle listStructure

  , test orgFancyLists "Simple Ordered List with upper and lower alpha markers" $
      ("A. Item1\n" <>
       "b) Item2\n") =?>
      let listStyle = (1, UpperAlpha, Period)
          listStructure = [ plain "Item1"
                          , plain "Item2"
                          ]
      in orderedListWith listStyle listStructure

  , "Simple Ordered List with Counter Cookie" =:
      ("1. [@1234] Item1\n" <>
       "2. Item2\n") =?>
      let listStyle = (1234, DefaultStyle, DefaultDelim)
          listStructure = [ plain "Item1"
                          , plain "Item2"
                          ]
      in orderedListWith listStyle listStructure

  , "Simple Ordered List with Alphabetical Counter Cookie" =:
      ("1. [@c] Item1\n" <>
       "2. Item2\n") =?>
      let listStyle = (3, DefaultStyle, DefaultDelim)
          listStructure = [ plain "Item1"
                          , plain "Item2"
                          ]
      in orderedListWith listStyle listStructure

  , "Simple Ordered List with Ignored Counter Cookie" =:
      ("1. Item1\n" <>
       "2. [@4] Item2\n") =?>
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

  , "Empty ordered list item" =:
      T.unlines [ "1."
                , "3. "
                ] =?>
      orderedList [ plain "", plain "" ]

  , test orgFancyLists "Empty ordered list item with fancy lists extension" $
      T.unlines [ "a."
                , "2. "
                ] =?>
      orderedListWith (1, LowerAlpha, Period) [ plain "", plain "" ]

  , "Empty ordered list item with counter cookie" =:
      T.unlines [ "1. [@5]"
                , "3. [@e] "
                ] =?>
      let listStyle = (5, DefaultStyle, DefaultDelim)
      in orderedListWith listStyle [ plain "", plain "" ]

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
      bulletList [ plain "Emacs" <>
                   orderedList [ plain "Org"]
                 ]

  , "Bullet List in Ordered List" =:
      ("1. GNU\n" <>
       "   - Freedom\n") =?>
      orderedList [ plain "GNU" <> bulletList [ plain "Freedom" ] ]

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

  , "Markup after header and list" =:
      T.unlines [ "* headline"
                , "- list"
                , ""
                , "~variable name~"
                ] =?>
      mconcat [ headerWith ("headline", [], []) 1 "headline"
              , bulletList [ plain "list" ]
              , para (code "variable name")
              ]
  ]
