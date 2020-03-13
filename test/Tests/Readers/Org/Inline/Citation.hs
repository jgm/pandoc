{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Org.Inline.Citation
   Copyright   : © 2014-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Test parsing of citations in org input.
-}
module Tests.Readers.Org.Inline.Citation (tests) where

import Prelude
import Test.Tasty (TestTree, testGroup)
import Tests.Helpers ((=?>))
import Tests.Readers.Org.Shared ((=:))
import Text.Pandoc.Builder

tests :: [TestTree]
tests =
  [ testGroup "Markdown-style citations"
    [ "Citation" =:
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
    ]

  , testGroup "org-ref citations"
    [ "simple citation" =:
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

    , "simple citation with underscores" =:
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

    , "simple citation succeeded by comma" =:
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

    , "simple citation succeeded by dot" =:
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

    , "simple citation succeeded by colon" =:
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

    , "simple citep citation" =:
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

    , "extended citation" =:
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
    ]

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
    in
      [ "Berkeley-style in-text citation" =:
        "See @Dominik201408." =?>
        para ("See "
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
        let pandocCite' = pandocInText { citationPrefix = toList "and" }
        in (para $ "See "
             <> cite [dominikInText] ""
             <> "," <> space
             <> cite [pandocCite'] ""
             <> "," <> space <> "and others")
    ]

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

  ]
