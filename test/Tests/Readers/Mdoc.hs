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
    , "nested with closing delimiters" =:
        ".Dq Pq hi mom !" =?>
        para (doubleQuoted "(hi mom)" <> "!")
    , "nested multiline enclosure" =:
        ".Dq Po a \\&; b \\&; c Pc ." =?>
        para (doubleQuoted "(a ; b ; c)" <> ".")
    , "with inlines" =:
        ".Dq hello Sy world ." =?>
        para (doubleQuoted ("hello" <> space <> strong "world" <> "."))
    , "with text production" =:
        ".Dq I love the St -iso8601 standard!" =?>
        para (doubleQuoted "I love the ISO 8601 standard!")
    , "with Ns" =:
        ".Op , Ns Ar value ..." =?>
        para ("[," <> (codeWith (cls "variable") "value ...") <> "]")
    , "ending with open delimiter" =:
        ".Dq hi (" =?>
        para (doubleQuoted "hi (")
    ]
  , testGroup "multiline enclosures"
    [ "nested multiline" =:
        T.unlines [".Bo", ".Po", "hi", ".Pc", ".Bc"] =?>
        para ("[(hi)]")
    , "nested on one line" =:
        ".Bo Po hi Pc Bc" =?>
        para ("[(hi)]")
    , "with wacky delimiters" =:
        ".Bo ( | hi ! Bc ?" =?>
        para ("([| hi!]?")
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
    , "Ar" =:
        ".Ar ) z" =?>
        para (codeWith (cls "variable") "file ..." <> ") " <> codeWith (cls "variable") "z")
    , "In" =:
        ".In ( math.h ) b c" =?>
        para ("(" <> codeWith (cls "In") "<math.h>" <> ") b c")
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
    , "enclosed with following macro" =:
        ".Pq Fl Cm x" =?>
        para ("(" <> codeWith (cls "Fl") "-" <> codeWith (cls "Cm") "x" <> ")")
    -- XXX this is a mandoc delta, the period is meant to land outside
    -- the enclosure. parseInline has learned how to defer eols but not
    -- closing delimiters
    , "enclosed with following delimiters" =:
        ".Pq Fl x ." =?>
        para ("(" <> codeWith (cls "Fl") "-x" <> ".)")
    , "following Ns" =:
        ".Fl W Ns Cm all" =?>
        para (codeWith (cls "Fl") "-W" <> codeWith (cls "Cm") "all")
    , "GNU" =:
        ".Fl -help" =?>
        para (codeWith (cls "Fl") "--help")
    , "GNU escaped" =:
        ".Fl \\-help" =?>
        para (codeWith (cls "Fl") "--help")
    , "GNU Fl Fl" =:
        ".Fl Fl help" =?>
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
        para "[before] [after]"  -- mandoc: warning
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
  , testGroup "spacing mode"
    [ "all text" =:
        T.unlines ["a", ".Sm off", "b c", "d", ".Sm on", "e"] =?>
        para ("a b c d e")
    , "text around macro" =:
        T.unlines ["a", ".Sm off", ".Sy b c", ".Sm on", "d"] =?>
        para ("a" <> space <> strong "bc" <> space <> "d")
    , "mulitple macros" =:
        T.unlines ["a", ".Sm off", ".Sy b Em c", ".Sm on", "d"] =?>
        para ("a" <> space <> strong "b" <> emph "c" <> space <> "d")
    , "mulitple control lines" =:
        T.unlines ["a", ".Sm off", ".Sy b", ".Em c", ".Sm on", "d"] =?>
        para ("a" <> space <> strong "b" <> emph "c" <> space <> "d")
    , "mixed control and text lines" =:
        T.unlines ["a", ".Sm off", ".Sy b", "c", ".Em d", ".Sm on", "d"] =?>
        para ("a" <> space <> strong "b" <> "c" <> emph "d" <> space <> "d")
    , "delimiters" =:
        T.unlines [".Sm off", ".Em a ", ".Em [ b | c ]", ".Sm on"] =?>
        para (emph "a" <> "[" <> emph "b" <> "|" <> emph "c" <> "]")
    ]
  , testGroup "Ap macro"
    [ "in the middle of a macro line" =:
        ".Xr mandoc 1 Ap s" =?>
        para (spanWith (cls "Xr") "mandoc(1)" <> "'s")
    -- mandoc difference: the edge case of "Ap (" tested in this mandoc regress
    -- isn't present in any actual OpenBSD base system manuals, where Ap is
    -- only ever followed by a letter. Furthermore, "Ap" is generally uncommon
    -- compared to "Ns '" (e.g. ".Xr mandoc 1 Ns 's"). I'm accepting a
    -- difference from mandoc here because correctly suppressing space after
    -- the "(" here would require more refactoring than I feel like doing at
    -- time of writing.
    -- per mandoc, should be: para (strong "bold" <> "'(" <> strong "bold")
    , "with punctuation and called macro" =:
        ".Sy bold Ap ( \"Sy\" bold" =?>
        para (strong "bold" <> "'( " <> strong "bold")
    ]
  , testGroup "Pf macro"
    [ "closing punctuation" =:
        ".Pf . right ." =?>
        para ".right."
    , "double punctuation" =:
        ".Pf . . double" =?>
        para ".. double"
    , "opening punctuation" =:
        ".Pf ( left ." =?>
        para "(left."
    , "unparsed argument" =:
        ".Pf Ar Sy gument " =?>
        para ("Ar" <> strong "gument")
    ]
  , testGroup "text production"
    [ "early NetBSD versions" =:
        T.unlines [".Nx 0.9a", "and", ".Nx 9.4b"] =?>
        para "NetBSD 0.9A and NetBSD 9.4b"
    , "with Ns" =:
        ".Ox Ns -specific" =?>
        para "OpenBSD-specific"
    , "with punctuation" =:
        ".Fx ," =?>
        para "FreeBSD,"
    , "with argument and punctuation" =:
        ".Fx 12.0 ." =?>
        para "FreeBSD 12.0."
    , "BSD alone" =:
        ".Bx ." =?>
        para "BSD."
    , "BSD with macro" =:
        ".Bx No rocks" =?>
        para "BSD rocks"
    , "BSD with version" =:
        ".Bx 4.4 ," =?>
        para "4.4BSD,"
    , "BSD with variant" =:
        ".Bx 4.3 tahoe !" =?>
        para "4.3BSD-Tahoe!"
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
