{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Pod
   Copyright   : © 2024 Evan Silberman
   License     : GNU GPL, version 2 or above

   Maintainer  : 
   Stability   : alpha
   Portability : portable

Tests for the Pod reader.
-}

module Tests.Readers.Pod (tests) where

import Data.Text (Text, pack)
import Test.Tasty
import Test.Tasty.HUnit (HasCallStack)
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

pod :: Text -> Pandoc
pod t = (purely $ readPod def) ("=pod\n\n" <> t <> "\n\n=cut\n")

manLink :: Text -> Maybe Text -> Inlines -> Inlines
manLink nm Nothing = linkWith (mempty, mempty, [("manual", nm)]) "" ""
manLink nm (Just sc) = linkWith (mempty, mempty, [("manual", nm), ("section", sc)]) "" ""

bogusEntity :: String -> TestTree
bogusEntity t = t =: "E<" <> pack t <> ">" =?> para ("E<" <> str (pack t) <> ">")

infix 4 =:
(=:) :: (ToString c, HasCallStack)
     => String -> (Text, c) -> TestTree
(=:) = test pod

tests :: [TestTree]
tests = [
  testGroup "inlines"
    [ "code with nested inlines" =:
        "C</I<A> (*PRUNE) I<B>/>" =?>
        para (code "/A (*PRUNE) B/")
    , "compact in compact" =:
        "I<B<strong> emphasis>" =?>
        para (emph $ (strong "strong") <> " emphasis")
    , "expanded in compact" =:
        "I<B<< strong >> emphasis>" =?>
        para (emph $ (strong "strong") <> " emphasis")
    , "compact in expanded" =:
        "I<<< B<strong> emphasis >>>" =?>
        para (emph $ (strong "strong") <> " emphasis")
    , "expanded in expanded" =:
        "I<<< B<<< strong >>> emphasis >>>" =?>
        para (emph $ (strong "strong") <> " emphasis")
    ]
  , testGroup "links"
    [ testGroup "compact"
      [ "URL" =:
          "L<https://example.org>" =?>
          para (link "https://example.org" "" "https://example.org")
      , "URL with link text" =:
          "L<link|https://example.org/index.html>" =?>
          para (link "https://example.org/index.html" "" "link")
      , "perl manual" =:
          "L<Foo::Bar>" =?>
          para (manLink "Foo::Bar" Nothing "Foo::Bar")
      , "manual with quoted section" =:
          "L<crontab(5)/\"DESCRIPTION\">" =?>
          para (manLink "crontab(5)" (Just "DESCRIPTION") (doubleQuoted "DESCRIPTION" <> " in crontab(5)"))
      , "manual with section and formatted link text" =:
          "L<B<< extravagant >> link|HTTP::Simple/is_info>" =?>
          para (manLink "HTTP::Simple" (Just "is_info") (strong "extravagant" <> " link"))
      , "internal link" =:
          "L</section name>" =?>
          para (link "#section-name" "" (doubleQuoted "section name"))
      , "internal link with formatting" =:
          "L</The C<pod2html> command>" =?>
          para (link "#the-pod2html-command" "" (doubleQuoted ("The " <> code "pod2html" <> " command")))
      , "link with angle bracket" =:
          "L<m<>" =?>
          para (manLink "m<" Nothing "m<")
      , "empty name" =:
          "L<|https://example.org>" =?>
          para (link "https://example.org" "" mempty)
      ]
    , testGroup "expanded"
      [ "URL" =:
          "L<< https://example.org >>" =?>
          para (link "https://example.org" "" "https://example.org")
      , "URL with link text" =:
          "L<< link|https://example.org/index.html >>" =?>
          para (link "https://example.org/index.html" "" "link")
      , "perl manual" =:
          "L<<< Foo::Bar >>>" =?>
          para (manLink "Foo::Bar" Nothing "Foo::Bar")
      , "manual with quoted section" =:
          "L<< crontab(5)/\"DESCRIPTION\"     >>" =?>
          para (manLink "crontab(5)" (Just "DESCRIPTION") (doubleQuoted "DESCRIPTION" <> " in crontab(5)"))
      , "manual with section and formatted link text" =:
          "L<< B<< extravagant >> link|HTTP::Simple/is_info >>" =?>
          para (manLink "HTTP::Simple" (Just "is_info") (strong "extravagant" <> " link"))
      , "internal link" =:
          "L<<   /section name  >>" =?>
          para (link "#section-name" "" (doubleQuoted "section name"))
      , "internal link with formatting" =:
          "L<<<<<          /The C<pod2html> command    >>>>>" =?>
          para (link "#the-pod2html-command" "" (doubleQuoted ("The " <> code "pod2html" <> " command")))
      , "link with angle bracket" =:
          "L<< m< >>" =?>
          para (manLink "m<" Nothing "m<")
      , "empty name" =:
          "L<< |https://example.org >>" =?>
          para (link "https://example.org" "" mempty)
      ]
    ]
  , testGroup "entities"
      [ testGroup "required"
        [ "quot" =:
            "E<quot>" =?>
          para "\""
        , "amp" =:
            "E<amp>" =?>
            para "&"
        , "apos" =:
            "E<apos>" =?>
            para "'"
        , "lt" =:
            "E<lt>" =?>
            para "<"
        , "gt" =:
            "E<gt>" =?>
            para ">"
        , "sol" =:
            "E<sol>" =?>
            para "/"
        , "verbar" =:
            "E<verbar>" =?>
            para "|"
        , "lchevron" =:
            "E<lchevron>" =?>
            para "«"
        , "rchevron" =:
            "E<rchevron>" =?>
            para "»"
        ]
      , testGroup "numeric"
        [ "decimal" =:
            "E<162>" =?>
            para "¢"
        , "octal" =:
            "E<0242>" =?>
            para "¢"
        , "hexadecimal" =:
            "E<0xA2>" =?>
            para "¢"
        , "hexadecimal variant" =:
            "E<0x00A2>" =?>
            para "¢"
        , "actually decimal" =:
            "E<099>" =?>
            para "c"
        ]
      , testGroup "bogus"
        [ bogusEntity "0XA2"
        , bogusEntity "not a real entity"
        , bogusEntity "162 1"
        , bogusEntity "99 bottles of beer"
        , bogusEntity "0xhh"
        , bogusEntity "077x"
        , bogusEntity "0x63 skidoo"
        ]
      ]
    ]
