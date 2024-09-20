{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Mdoc
   Copyright   : © 2024 Evan Silberman
   License     : GNU GPL, version 2 or above

   Maintainer  : 
   Stability   : alpha
   Portability : portable

Tests for the Mdoc reader.
-}

module Tests.Readers.Mdoc (tests) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit (HasCallStack)
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

mdoc :: Text -> Pandoc
mdoc = purely $ readMdoc def

infix 4 =:
(=:) :: (ToString c, HasCallStack)
     => String -> (Text, c) -> TestTree
(=:) = test mdoc

cls :: Text -> Attr
cls x = (mempty, [x], mempty)

tests :: [TestTree]
tests = [
  testGroup "one-line enclosures"
    [ "Dq" =:
        ".Dq hello world" =?>
        para (doubleQuoted "hello world")
    , "Sq" =:
        ".Sq hello world" =?>
        para (singleQuoted "hello world")
    , "empty" =:
        ".Dq" =?>
        para (doubleQuoted mempty)
    , "nested" =:
        ".Dq Pq hello world" =?>
        para (doubleQuoted "(hello world)")
    , "with inlines" =:
        ".Dq hello Sy world ." =?>
        para (doubleQuoted ("hello" <> space <> strong "world" <> "."))
    ]
  , testGroup "multiline enclosures"
    [ "nested multiline" =:
        T.unlines [".Bo", ".Po", "hi", ".Pc", ".Bc"] =?>
        para ("[(hi)]")
    , "nested on one line" =:
        ".Bo Po hi Pc Bc" =?>
        para ("[(hi)]")
    ]
  , testGroup "simple inlines"
    [ "Sy" =:
        ".Sy hello world" =?>
        para (strong "hello world")
    , "Em" =:
        ".Em hello world" =?>
        para (emph "hello world")
    , "Ev" =:
        ".Ev HELLO_WORLD ," =?>
        para (codeWith (cls "Ev") "HELLO_WORLD" <> ",")
    , "Mt" =:
        ".Mt a@example.org , b@example.org" =?>
        para ((link "mailto:a@example.org" "" "a@example.org") <>
               "," <> space <> (link "mailto:b@example.org" "" "b@example.org"))
    , "No" =:
        ".No ( hello , world ! )" =?>
        para "(hello, world!)"
    , "empty Pa with closing punctuation" =:
        ".Pa ) z" =?>
        para (spanWith (cls "Pa") "~" <> ")" <> space <> spanWith (mempty, ["Pa"], mempty) "z")
    , "delimiters" =:
        ".Sy ( hello world )" =?>
        para (mconcat ["(", strong "hello world", ")"])
    , "multiple" =:
        ".Sy hello Em world" =?>
        para (strong "hello" <> space <> emph "world")
    ]
  , testGroup "Fl"
    [ "simple" =:
        ".Fl w" =?>
        para (codeWith (cls "Fl") "-w")
    , "multiple" =:
        ".Fl W all" =?>
        para (codeWith (cls "Fl") "-W" <> space <> codeWith (cls "Fl") "-all")
    , "empty with following macro" =:
        ".Fl Cm x" =?>
        para (codeWith (cls "Fl") "-" <> codeWith (cls "Cm") "x")
    , "following Ns" =:
        ".Fl W Ns Cm all" =?>
        para (codeWith (cls "Fl") "-W" <> codeWith (cls "Cm") "all")
    , "GNU" =:
        ".Fl -help" =?>
        para (codeWith (cls "Fl") "--help")
    , "GNU escaped" =:
        ".Fl \\-help" =?>
        para (codeWith (cls "Fl") "--help")
    , "punctuation" =:
        ".Op Fl a | b" =?>
        para ("[" <> codeWith (cls "Fl") "-a" <> " | " <> codeWith (cls "Fl") "-b" <> "]")
    , "middle close paren" =:
        ".Fl a ) z" =?>
        para (codeWith (cls "Fl") "-a" <> ") " <> codeWith (cls "Fl") "-z")
    , "empty with close paren" =:
        ".Fl ) z" =?>
        para (codeWith (cls "Fl") "-" <> ") " <> codeWith (cls "Fl") "-z")
    , "empty with pipe" =:
        ".Fl | z" =?>
        para (codeWith (cls "Fl") "-" <> " | " <> codeWith (cls "Fl") "-z")
    , "empty with parens" =:
        ".Fl ( )" =?>
        para ("(" <> codeWith (cls "Fl") "-" <> ")")
    ]
  , testGroup "links"
    [ "basic" =:
        ".Lk href name" =?>
        para (link "href" "" "name")
    , "complicated" =:
        ".Lk , ( href name )" =?>
        para ("," <> space <> "(" <> link "href" "" "name" <> ")")
    , "unnamed" =:
        ".Lk href" =?>
        para (link "href" "" "href")
    ]
  , testGroup "Ns macro"
    [ "at the beginning of a macro line (mandoc delta)" =:
        T.unlines [".Op before", ".Ns Op after"] =?>
        para "[before][after]"  -- mandoc: warning + "[before] [after]"
    , "after a block closing macro" =:
        T.unlines [".Oo before", ".Oc Ns Op after"] =?>
        para "[before][after]"
    , "in the middle of a macro line" =:
        ".Oo before Oc Ns Op after" =?>
        para "[before][after]"
    , "before closing punctuation" =:
        ".Oo before Oc Ns : Op after" =?>
        para "[before]: [after]"  -- mandoc: warning
    , "after closing punctuation" =:
        ".Oo before Oc : Ns Op after" =?>
        para "[before]:[after]"
    , "at the end of a macro line" =:
        T.unlines [".Oo before Oc Ns", ".Op after"] =?>
        para "[before][after]"
    , "at the end of a partial-implicit line" =:
        T.unlines [".Op before Ns", ".Op after"] =?>
        para "[before][after]"
    , "normal words" =:
        ".No no Ns ns No no" =?>
        para ("nons" <> space <> "no")
    , "opening punctuation" =:
        ".No no Ns \"(\" ns No no" =?>
        para ("no(ns" <> space <> "no")
    , "closing punctuation" =:
        ".No no \"Ns\" ns \")\" No no" =?>
        para ("nons)" <> space <> "no")
    ]
  , testGroup "inline punctuation"
    [ testGroup "leading punctuation"
      [ "open paren"           =: ".Em ( b"     =?> para ("(" <> emph "b")
      , "open square bracket"  =: ".Em \"[\" b" =?> para ("[" <> emph "b")
      , "pipe"                 =: ".Em | b"     =?> para ("|" <> space <> emph "b")
      , "period"               =: ".Em . b"     =?> para ("." <> space <> emph "b")
      , "comma"                =: ".Em , b"     =?> para ("," <> space <> emph "b")
      , "semicolon"            =: ".Em ; b"     =?> para (";" <> space <> emph "b")
      , "colon"                =: ".Em : b"     =?> para (":" <> space <> emph "b")
      , "question mark"        =: ".Em ? b"     =?> para ("?" <> space <> emph "b")
      , "exclamation"          =: ".Em ! b"     =?> para ("!" <> space <> emph "b")
      , "close paren"          =: ".Em ) b"     =?> para (")" <> space <> emph "b")
      , "close square bracket" =: ".Em \"]\" b" =?> para ("]" <> space <> emph "b")
      ]
    , testGroup "trailing punctuation"
      [ "open paren"           =: ".Em a ("     =?> para (emph "a" <> space <> "(")
      , "open square bracket"  =: ".Em a ["     =?> para (emph "a" <> space <> "[")
      , "pipe"                 =: ".Em a |"     =?> para (emph "a" <> space <> "|")
      , "period"               =: ".Em a ."     =?> para (emph "a" <> ".")
      , "comma"                =: ".Em a ,"     =?> para (emph "a" <> ",")
      , "semicolon"            =: ".Em a ;"     =?> para (emph "a" <> ";")
      , "colon"                =: ".Em a :"     =?> para (emph "a" <> ":")
      , "question mark"        =: ".Em a ?"     =?> para (emph "a" <> "?")
      , "exclamation"          =: ".Em a !"     =?> para (emph "a" <> "!")
      , "close parens"         =: ".Em a \")\"" =?> para (emph "a" <> ")")
      , "close square bracket" =: ".Em a ]"     =?> para (emph "a" <> "]")
      ]
    , testGroup "middle punctuation"
      [ "open paren"           =: ".Em a ( b"     =?> para (mconcat [emph "a", space, "(", emph "b"])
      , "open square bracket"  =: ".Em a [ b"     =?> para (mconcat [emph "a", space, "[", emph "b"])
      , "pipe"                 =: ".Em a \"|\" b" =?> para (mconcat [emph "a", space, "|", space, emph "b"])
      , "period"               =: ".Em a . b"     =?> para (mconcat [emph "a", ".", space, emph "b"])
      , "comma"                =: ".Em a , b"     =?> para (mconcat [emph "a", ",", space, emph "b"])
      , "semicolon"            =: ".Em a ; b"     =?> para (mconcat [emph "a", ";", space, emph "b"])
      , "colon"                =: ".Em a \":\" b" =?> para (mconcat [emph "a", ":", space, emph "b"])
      , "question mark"        =: ".Em a ? b"     =?> para (mconcat [emph "a", "?", space, emph "b"])
      , "exclamation"          =: ".Em a ! b"     =?> para (mconcat [emph "a", "!", space, emph "b"])
      , "close paren"          =: ".Em a ) b"     =?> para (mconcat [emph "a", ")", space, emph "b"])
      , "close square bracket" =: ".Em a ] b"     =?> para (mconcat [emph "a", "]", space, emph "b"])
      ]
    ]
  ]
