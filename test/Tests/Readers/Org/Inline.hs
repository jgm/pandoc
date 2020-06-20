{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Org.Inline
   Copyright   : © 2014-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Tests parsing of org inlines.
-}
module Tests.Readers.Org.Inline (tests) where

import Prelude
import Data.List (intersperse)
import Test.Tasty (TestTree, testGroup)
import Tests.Helpers ((=?>))
import Tests.Readers.Org.Shared ((=:), spcSep)
import Text.Pandoc.Builder
import qualified Data.Text as T
import qualified Tests.Readers.Org.Inline.Citation as Citation
import qualified Tests.Readers.Org.Inline.Note as Note
import qualified Tests.Readers.Org.Inline.Smart as Smart

tests :: [TestTree]
tests =
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

  , "Underline" =:
      "_underline_" =?>
      para (underline "underline")

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

  , "Markup-chars not occurring on word break are symbols" =:
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
    para (text "foo") <>
    para (emph $ text "bar")

  , "Inline math must stay within three lines" =:
      T.unlines [ "$a", "b", "c$", "$d", "e", "f", "g$" ] =?>
      para (math "a\nb\nc" <> softbreak <>
            "$d" <> softbreak <> "e" <> softbreak <>
            "f" <> softbreak <> "g$")

  , "Single-character math" =:
      "$a$ $b$! $c$?" =?>
      para (spcSep [ math "a"
                   , "$b$!"
                   , math "c" <> "?"
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
                  , "i" <> subscript "(jk)" <> "l)"
                  , "m" <> superscript "()" <> "n"
                  , "o" <> subscript "p{q{}r}"
                  , "s" <> superscript "t{u}v"
                  , "w" <> subscript "xy" <> "z}"
                  , "1" <> superscript "" <> "2"
                  , "3" <> subscript "{}"
                  , "4" <> superscript ("(a(" <> strong "b(c" <> ")d))")
                  ])
  , "Verbatim text can contain equal signes (=)" =:
      "=is_subst = True=" =?>
      para (code "is_subst = True")

  , testGroup "Images"
    [ "Image" =:
      "[[./sunset.jpg]]" =?>
      para (image "./sunset.jpg" "" "")

    , "Image with explicit file: prefix" =:
      "[[file:sunrise.jpg]]" =?>
      para (image "sunrise.jpg" "" "")

    , "Multiple images within a paragraph" =:
      T.unlines [ "[[file:sunrise.jpg]]"
                , "[[file:sunset.jpg]]"
                ] =?>
      para (image "sunrise.jpg" "" ""
             <> softbreak
             <> image "sunset.jpg" "" "")

    , "Image with html attributes" =:
      T.unlines [ "#+ATTR_HTML: :width 50%"
                , "[[file:guinea-pig.gif]]"
                ] =?>
      para (imageWith ("", [], [("width", "50%")]) "guinea-pig.gif" "" "")

    , "Uppercase extension" =:
      "[[file:test.PNG]]" =?>
      para (image "test.PNG" "" "")
    ]

  , "Explicit link" =:
      "[[http://zeitlens.com/][pseudo-random /nonsense/]]" =?>
    para (link "http://zeitlens.com/" ""
                   ("pseudo-random" <> space <> emph "nonsense"))

  , "Self-link" =:
      "[[http://zeitlens.com/]]" =?>
    para (link "http://zeitlens.com/" "" "http://zeitlens.com/")

  , "Internal self-link (reference)" =:
    "[[#rabbit]]" =?>
    para (link "#rabbit" "" "#rabbit")

  , "Absolute file link" =:
      "[[/url][hi]]" =?>
    para (link "file:///url" "" "hi")

  , "Link to file in parent directory" =:
      "[[../file.txt][moin]]" =?>
    para (link "../file.txt" "" "moin")

  , "Empty link (for gitit interop)" =:
      "[[][New Link]]" =?>
    para (link "" "" "New Link")

  , "Image link" =:
      "[[sunset.png][file:dusk.svg]]" =?>
    para (link "sunset.png" "" (image "dusk.svg" "" ""))

  , "Image link with non-image target" =:
      "[[http://example.com][./logo.png]]" =?>
    para (link "http://example.com" "" (image "./logo.png" "" ""))

  , "Link to image" =:
    "[[https://example.com/image.jpg][Look!]]" =?>
    para (link "https://example.com/image.jpg" "" (str "Look!"))

  , "Plain link" =:
      "Posts on http://zeitlens.com/ can be funny at times." =?>
    para (spcSep [ "Posts", "on"
                     , link "http://zeitlens.com/" "" "http://zeitlens.com/"
                     , "can", "be", "funny", "at", "times."
                     ])

  , "Angle link" =:
      "Look at <http://moltkeplatz.de> for fnords." =?>
    para (spcSep [ "Look", "at"
                     , link "http://moltkeplatz.de" "" "http://moltkeplatz.de"
                     , "for", "fnords."
                     ])

  , "Absolute file link" =:
      "[[file:///etc/passwd][passwd]]" =?>
    para (link "file:///etc/passwd" "" "passwd")

  , "File link" =:
      "[[file:target][title]]" =?>
    para (link "target" "" "title")

  , "Anchor" =:
      "<<anchor>> Link here later." =?>
    para (spanWith ("anchor", [], []) mempty <>
              "Link" <> space <> "here" <> space <> "later.")

  , "Inline code block" =:
      "src_emacs-lisp{(message \"Hello\")}" =?>
    para (codeWith ( ""
                       , [ "commonlisp" ]
                       , [ ("org-language", "emacs-lisp") ])
                       "(message \"Hello\")")

  , "Inline code block with arguments" =:
      "src_sh[:export both :results output]{echo 'Hello, World'}" =?>
    para (codeWith ( ""
                       , [ "bash" ]
                       , [ ("org-language", "sh")
                         , ("export", "both")
                         , ("results", "output")
                         ]
                       )
                       "echo 'Hello, World'")

  , "Inline code block with a blank argument array" =:
      "src_sh[]{echo 'Hello, World'}" =?>
    para (codeWith ( ""
                       , [ "bash" ]
                       , [ ("org-language", "sh") ])
                       "echo 'Hello, World'")

  , "Inline code block with toggle" =:
      "src_sh[:toggle]{echo $HOME}" =?>
    para (codeWith ( ""
                       , [ "bash" ]
                       , [ ("org-language", "sh")
                         , ("toggle", "yes")
                         ]
                       )
                       "echo $HOME")

  , "Inline LaTeX symbol" =:
      "\\dots" =?>
      para "…"

  , "Inline LaTeX command" =:
      "\\textit{Emphasised}" =?>
      para (emph "Emphasised")

  , "Inline LaTeX command with spaces" =:
      "\\emph{Emphasis mine}" =?>
      para (emph "Emphasis mine")

  , "Inline math symbols" =:
      "\\tau \\oplus \\alpha" =?>
      para "τ ⊕ α"

  , "Inline LaTeX math command" =:
      "\\crarr" =?>
      para "↵"

  , "Unknown inline LaTeX command" =:
      "\\notacommand{foo}" =?>
      para (rawInline "latex" "\\notacommand{foo}")

  , "Export snippet" =:
      "@@html:<kbd>M-x org-agenda</kbd>@@" =?>
      para (rawInline "html" "<kbd>M-x org-agenda</kbd>")

  , "MathML symbol in LaTeX-style" =:
      "There is a hackerspace in Lübeck, Germany, called nbsp (unicode symbol: '\\nbsp')." =?>
      para "There is a hackerspace in Lübeck, Germany, called nbsp (unicode symbol: ' ')."

  , "MathML symbol in LaTeX-style, including braces" =:
      "\\Aacute{}stor" =?>
      para "Ástor"

  , "MathML copy sign" =:
      "\\copy" =?>
      para "©"

  , "MathML symbols, space separated" =:
      "\\ForAll \\Auml" =?>
      para "∀ Ä"

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

  , testGroup "Citations" Citation.tests
  , testGroup "Footnotes" Note.tests
  , testGroup "Smart punctuation" Smart.tests
  ]
