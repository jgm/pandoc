{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
        meta = setMeta "title" (MetaInlines titleInline) nullMeta
    in Pandoc meta mempty

  , "Author" =:
    "#+author: John /Emacs-Fanboy/ Doe" =?>
    let author = toList . spcSep $ [ "John", emph "Emacs-Fanboy", "Doe" ]
        meta = setMeta "author" (MetaList [MetaInlines author]) nullMeta
    in Pandoc meta mempty

  , "Multiple authors" =:
    "#+author: James Dewey Watson, Francis Harry Compton Crick " =?>
    let watson = MetaInlines $ toList "James Dewey Watson"
        crick = MetaInlines $ toList "Francis Harry Compton Crick"
        meta = setMeta "author" (MetaList [watson, crick]) nullMeta
    in Pandoc meta mempty

  , "Date" =:
    "#+Date: Feb. *28*, 2014" =?>
    let date = toList . spcSep $ [ "Feb.", strong "28" <> ",", "2014" ]
        meta = setMeta "date" (MetaInlines date) nullMeta
    in Pandoc meta mempty

  , "Description" =:
    "#+DESCRIPTION: Explanatory text" =?>
    let description = "Explanatory text"
        meta = setMeta "description" (MetaString description) nullMeta
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
          meta = setMeta "author" (MetaList [author]) nullMeta
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
      para (emph $ "Where's" <> space <> "Wally?")

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
        para ("[/noemph/]")
    ]
  ]
