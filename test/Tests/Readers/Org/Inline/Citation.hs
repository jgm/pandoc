{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Org.Inline.Citation
   Copyright   : © 2014-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Test parsing of citations in org input.
-}
module Tests.Readers.Org.Inline.Citation (tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Helpers ((=?>))
import Tests.Readers.Org.Shared ((=:))
import Text.Pandoc.Builder

tests :: [TestTree]
tests =
  [ testGroup "Org-cite citations"
    [ "Citation" =:
      "[cite:@nonexistent]" =?>
      let citation = Citation
                     { citationId = "nonexistent"
                     , citationPrefix = []
                     , citationSuffix = []
                     , citationMode = NormalCitation
                     , citationNoteNum = 0
                     , citationHash = 0}
      in (para $ cite [citation] "[cite:@nonexistent]")

    , "Citation containing text" =:
      "[cite:see @item1 p. 34-35]" =?>
      let citation = Citation
                     { citationId = "item1"
                     , citationPrefix = [Str "see"]
                     , citationSuffix = [Space ,Str "p.",Space,Str "34-35"]
                     , citationMode = NormalCitation
                     , citationNoteNum = 0
                     , citationHash = 0}
      in (para $ cite [citation] "[cite:see @item1 p. 34-35]")

    , "Author-in-text citation with locator and suffix" =:
      "[cite/t:see @item1 p. 34-35 and *passim*; @item2]" =?>
      let citations =
            [ Citation
                { citationId = "item1"
                , citationPrefix = [ Str "see" ]
                , citationSuffix =
                    [ Str "p."
                    , Space
                    , Str "34-35"
                    , Space
                    , Str "and"
                    , Space
                    , Strong [ Str "passim" ]
                    ]
                , citationMode = AuthorInText
                , citationNoteNum = 0
                , citationHash = 0
                }
            , Citation
                { citationId = "item2"
                , citationPrefix = []
                , citationSuffix = []
                , citationMode = NormalCitation
                , citationNoteNum = 0
                , citationHash = 0
                }
            ]
      in (para $ cite citations "[cite/t:see @item1 p. 34-35 and *passim*; @item2]")
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

    , "multiple simple citations" =:
      "citep:picard,riker" =?>
      let picard = Citation
                 { citationId = "picard"
                 , citationPrefix = mempty
                 , citationSuffix = mempty
                 , citationMode = NormalCitation
                 , citationNoteNum = 0
                 , citationHash = 0
                 }
          riker  = Citation
                 { citationId = "riker"
                 , citationPrefix = mempty
                 , citationSuffix = mempty
                 , citationMode = NormalCitation
                 , citationNoteNum = 0
                 , citationHash = 0
                 }
      in (para $ cite [picard,riker] "citep:picard,riker")

    , "multiple simple citations succeeded by comma" =:
      "citep:picard,riker," =?>
      let picard = Citation
                 { citationId = "picard"
                 , citationPrefix = mempty
                 , citationSuffix = mempty
                 , citationMode = NormalCitation
                 , citationNoteNum = 0
                 , citationHash = 0
                 }
          riker  = Citation
                 { citationId = "riker"
                 , citationPrefix = mempty
                 , citationSuffix = mempty
                 , citationMode = NormalCitation
                 , citationNoteNum = 0
                 , citationHash = 0
                 }
      in (para $ cite [picard,riker] "citep:picard,riker" <> str ",")

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
