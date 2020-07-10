{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Org.Meta
   Copyright   : © 2014-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Tests parsing of org meta data (mostly lines starting with @#+@).
-}
module Tests.Readers.Org.Meta (tests) where

import Prelude
import Test.Tasty (TestTree, testGroup)
import Tests.Helpers ((=?>))
import Tests.Readers.Org.Shared ((=:), spcSep)
import Text.Pandoc
import Text.Pandoc.Builder
import qualified Data.Text as T

tests :: [TestTree]
tests =
  [ testGroup "Comments"
    [ "Comment" =:
      "# Nothing to see here" =?>
      (mempty::Blocks)

    , "Hash not followed by space is text" =:
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
    ]

  , testGroup "Export settings"
    [ "Title" =:
      "#+TITLE: Hello, World" =?>
      let titleInline = toList $ "Hello," <> space <> "World"
          meta = setMeta "title" (MetaInlines titleInline) nullMeta
      in Pandoc meta mempty

    , testGroup "Author"
      [ "sets 'author' field" =:
        "#+author: John /Emacs-Fanboy/ Doe" =?>
        let author = toList . spcSep $ [ "John", emph "Emacs-Fanboy", "Doe" ]
            meta = setMeta "author" (MetaInlines author) nullMeta
        in Pandoc meta mempty

      , "Multiple author lines" =:
        T.unlines [ "#+author: James Dewey Watson,"
                  , "#+author: Francis Harry Compton Crick"
                  ] =?>
        let watson = toList "James Dewey Watson,"
            crick = toList "Francis Harry Compton Crick"
            meta = setMeta "author"
                           (MetaInlines (watson ++ SoftBreak : crick))
                           nullMeta
        in Pandoc meta mempty
      ]

    , "Date" =:
      "#+Date: Feb. *28*, 2014" =?>
      let date = toList . spcSep $ [ "Feb.", strong "28" <> ",", "2014" ]
          meta = setMeta "date" (MetaInlines date) nullMeta
      in Pandoc meta mempty

    , testGroup "Description"
      [ "Single line" =:
        "#+DESCRIPTION: Explanatory text" =?>
        let description = [Str "Explanatory", Space, Str "text"]
            meta = setMeta "description" (MetaInlines description) nullMeta
        in Pandoc meta mempty

      , "Multiline" =:
        T.unlines [ "#+DESCRIPTION: /Short/ introduction"
                  , "#+DESCRIPTION: to Org-mode"
                  ] =?>
        let description = [ Emph [Str "Short"], Space, Str "introduction"
                          , SoftBreak
                          , Str "to", Space, Str "Org-mode"
                          ]
            meta = setMeta "description" (MetaInlines description) nullMeta
        in Pandoc meta mempty
      ]

    , "Subtitle" =:
      T.unlines [ "#+SUBTITLE: Your Life in"
                , "#+SUBTITLE: /Plain/ Text"
                ] =?>
      let subtitle = "Your Life in" <> softbreak <> emph "Plain" <> " Text"
      in Pandoc (setMeta "subtitle" (toMetaValue subtitle) nullMeta) mempty

    , "Keywords" =:
      T.unlines [ "#+KEYWORDS: pandoc, testing,"
                , "#+KEYWORDS: Org"
                ] =?>
      let keywords = toList $ "pandoc, testing," <> softbreak <> "Org"
          meta = setMeta "keywords" (MetaInlines keywords) nullMeta
      in Pandoc meta mempty

    , "Institute" =:
      "#+INSTITUTE: ACME Inc." =?>
      Pandoc (setMeta "institute" ("ACME Inc." :: Inlines) nullMeta) mempty

    , testGroup "LaTeX"
      [ "LATEX_HEADER" =:
        "#+LaTeX_header: \\usepackage{tikz}" =?>
        let latexInlines = rawInline "latex" "\\usepackage{tikz}"
            inclList = MetaList [MetaInlines (toList latexInlines)]
            meta = setMeta "header-includes" inclList nullMeta
        in Pandoc meta mempty

      , "LATEX_HEADER_EXTRA" =:
        "#+LATEX_HEADER_EXTRA: \\usepackage{calc}" =?>
        let latexInlines = rawInline "latex" "\\usepackage{calc}"
            inclList = toMetaValue [latexInlines]
        in Pandoc (setMeta "header-includes" inclList nullMeta) mempty

      , testGroup "LaTeX_CLASS"
        [ "stored as documentclass" =:
          "#+LATEX_CLASS: article" =?>
          let meta = setMeta "documentclass" (MetaString "article") nullMeta
          in Pandoc meta mempty

        , "last definition takes precedence" =:
          T.unlines [ "#+LATEX_CLASS: this will not be used"
                    , "#+LATEX_CLASS: report"
                    ] =?>
          let meta = setMeta "documentclass" (MetaString "report") nullMeta
          in Pandoc meta mempty
        ]

      , "LATEX_CLASS_OPTIONS as classoption" =:
        "#+LATEX_CLASS_OPTIONS: [a4paper]" =?>
        let meta = setMeta "classoption" (MetaString "a4paper") nullMeta
        in Pandoc meta mempty
      ]

    , testGroup "HTML"
      [ "HTML_HEAD values are added to header-includes" =:
        "#+html_head: <meta/>" =?>
        let html = rawInline "html" "<meta/>"
            inclList = MetaList [MetaInlines (toList html)]
            meta = setMeta "header-includes" inclList nullMeta
        in Pandoc meta mempty

      , "HTML_HEAD_EXTRA behaves like HTML_HEAD" =:
        T.unlines [ "#+HTML_HEAD: <meta name=\"generator\" content=\"pandoc\">"
                  , "#+HTML_HEAD_EXTRA: <meta charset=\"utf-8\">"
                  ] =?>
        let generator = rawInline "html"
                                  "<meta name=\"generator\" content=\"pandoc\">"
            charset = rawInline "html" "<meta charset=\"utf-8\">"
            inclList = toMetaValue [generator, charset]
        in Pandoc (setMeta "header-includes" inclList nullMeta) mempty
      ]
    ]

  , testGroup "Non-export keywords"
    [ testGroup "#+LINK"
      [ "Link abbreviation" =:
        T.unlines [ "#+LINK: wp https://en.wikipedia.org/wiki/%s"
                  , "[[wp:Org_mode][Wikipedia on Org-mode]]"
                  ] =?>
        para (link "https://en.wikipedia.org/wiki/Org_mode" ""
               ("Wikipedia" <> space <> "on" <> space <> "Org-mode"))

      , "Link abbreviation, defined after first use" =:
        T.unlines [ "[[zl:non-sense][Non-sense articles]]"
                  , "#+LINK: zl http://zeitlens.com/tags/%s.html"
                  ] =?>
        para (link "http://zeitlens.com/tags/non-sense.html" ""
               ("Non-sense" <> space <> "articles"))

      , "Link abbreviation, URL encoded arguments" =:
        T.unlines [ "#+link: expl http://example.com/%h/foo"
                  , "[[expl:Hello, World!][Moin!]]"
                  ] =?>
        para (link "http://example.com/Hello%2C%20World%21/foo" "" "Moin!")

      , "Link abbreviation, append arguments" =:
        T.unlines [ "#+link: expl http://example.com/"
                  , "[[expl:foo][bar]]"
                  ] =?>
        para (link "http://example.com/foo" "" "bar")
      ]

    , testGroup "emphasis config"
      [ "Changing pre and post chars for emphasis" =:
        T.unlines [ "#+pandoc-emphasis-pre: \"[)\""
                  , "#+pandoc-emphasis-post: \"]\\n\""
                  , "([/emph/])*foo*"
                  ] =?>
        para ("([" <> emph "emph" <> "])" <> strong "foo")

      , "setting an invalid value restores the default" =:
        T.unlines [ "#+pandoc-emphasis-pre: \"[\""
                  , "#+pandoc-emphasis-post: \"]\""
                  , "#+pandoc-emphasis-pre:"
                  , "#+pandoc-emphasis-post:"
                  , "[/noemph/]"
                  ] =?>
        para "[/noemph/]"
      ]

    , "Unknown keyword" =:
      T.unlines [ "#+UNKNOWN_KEYWORD: Chumbawamba"
                , "#+ANOTHER_UNKNOWN: Blur"
                ] =?>
      rawBlock "org" "#+UNKNOWN_KEYWORD: Chumbawamba" <>
      rawBlock "org" "#+ANOTHER_UNKNOWN: Blur"
    ]

  , "Properties drawer" =:
      T.unlines [ "  :PROPERTIES:"
                , "  :setting: foo"
                , "  :END:"
                ] =?>
      (mempty::Blocks)

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
      para (emph $ "Where's" <> space <> "Wally?")

  , "Link to nonexistent anchor" =:
      T.unlines [ "<<link-here>> Target."
                , ""
                , "[[link$here][See here!]]"
                ] =?>
      (para (spanWith ("link-here", [], []) mempty <> "Target.") <>
       para (emph ("See" <> space <> "here!")))
  ]
