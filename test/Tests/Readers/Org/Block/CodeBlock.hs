{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Org.Block.CodeBlock
   Copyright   : © 2014-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Test parsing of org code blocks.
-}
module Tests.Readers.Org.Block.CodeBlock (tests) where

import Prelude
import Test.Tasty (TestTree)
import Tests.Helpers ((=?>))
import Tests.Readers.Org.Shared ((=:), spcSep)
import Text.Pandoc.Builder
import qualified Data.Text as T

tests :: [TestTree]
tests =
  [ "Source block" =:
       T.unlines [ "  #+BEGIN_SRC haskell"
                 , "  main = putStrLn greeting"
                 , "    where greeting = \"moin\""
                 , "  #+END_SRC" ] =?>
       let attr' = ("", ["haskell"], [])
           code' = "main = putStrLn greeting\n" <>
                   "  where greeting = \"moin\"\n"
       in codeBlockWith attr' code'

  , "Source block with indented code" =:
       T.unlines [ "  #+BEGIN_SRC haskell"
                 , "    main = putStrLn greeting"
                 , "      where greeting = \"moin\""
                 , "  #+END_SRC" ] =?>
       let attr' = ("", ["haskell"], [])
           code' = "main = putStrLn greeting\n" <>
                   "  where greeting = \"moin\"\n"
       in codeBlockWith attr' code'

  , "Source block with tab-indented code" =:
       T.unlines [ "\t#+BEGIN_SRC haskell"
                 , "\tmain = putStrLn greeting"
                 , "\t  where greeting = \"moin\""
                 , "\t#+END_SRC" ] =?>
       let attr' = ("", ["haskell"], [])
           code' = "main = putStrLn greeting\n" <>
                   "  where greeting = \"moin\"\n"
       in codeBlockWith attr' code'

  , "Empty source block" =:
       T.unlines [ "  #+BEGIN_SRC haskell"
                 , "  #+END_SRC" ] =?>
       let attr' = ("", ["haskell"], [])
           code' = ""
       in codeBlockWith attr' code'

  , "Source block between paragraphs" =:
       T.unlines [ "Low German greeting"
                 , "  #+BEGIN_SRC haskell"
                 , "  main = putStrLn greeting"
                 , "    where greeting = \"Moin!\""
                 , "  #+END_SRC" ] =?>
       let attr' = ("", ["haskell"], [])
           code' = "main = putStrLn greeting\n" <>
                    "  where greeting = \"Moin!\"\n"
       in mconcat [ para $ spcSep [ "Low", "German", "greeting"  ]
                  , codeBlockWith attr' code'
                  ]
  , "Source block with babel arguments" =:
       T.unlines [ "#+BEGIN_SRC emacs-lisp :exports both"
                 , "(progn (message \"Hello, World!\")"
                 , "       (+ 23 42))"
                 , "#+END_SRC" ] =?>
       let classes = [ "commonlisp" ] -- as kate doesn't know emacs-lisp syntax
           params = [ ("org-language", "emacs-lisp")
                    , ("exports", "both")
                    ]
           code' = T.unlines [ "(progn (message \"Hello, World!\")"
                           , "       (+ 23 42))" ]
       in codeBlockWith ("", classes, params) code'

  , "Source block with results and :exports both" =:
       T.unlines [ "#+BEGIN_SRC emacs-lisp :exports both"
                 , "(progn (message \"Hello, World!\")"
                 , "       (+ 23 42))"
                 , "#+END_SRC"
                 , ""
                 , "#+RESULTS:"
                 , ": 65"] =?>
       let classes = [ "commonlisp" ]
           params = [ ("org-language", "emacs-lisp")
                    , ("exports", "both")
                    ]
           code' = T.unlines [ "(progn (message \"Hello, World!\")"
                             , "       (+ 23 42))" ]
           results' = "65\n"
       in codeBlockWith ("", classes, params) code'
          <>
          codeBlockWith ("", ["example"], []) results'

  , "Source block with results and :exports code" =:
       T.unlines [ "#+BEGIN_SRC emacs-lisp :exports code"
                 , "(progn (message \"Hello, World!\")"
                 , "       (+ 23 42))"
                 , "#+END_SRC"
                 , ""
                 , "#+RESULTS:"
                 , ": 65" ] =?>
       let classes = [ "commonlisp" ]
           params = [ ("org-language", "emacs-lisp")
                    , ("exports", "code")
                    ]
           code' = T.unlines [ "(progn (message \"Hello, World!\")"
                             , "       (+ 23 42))" ]
       in codeBlockWith ("", classes, params) code'

  , "Source block with results and :exports results" =:
       T.unlines [ "#+BEGIN_SRC emacs-lisp :exports results"
                 , "(progn (message \"Hello, World!\")"
                 , "       (+ 23 42))"
                 , "#+END_SRC"
                 , ""
                 , "#+RESULTS:"
                 , ": 65" ] =?>
       let results' = "65\n"
       in codeBlockWith ("", ["example"], []) results'

  , "Source block with results and :exports none" =:
       T.unlines [ "#+BEGIN_SRC emacs-lisp :exports none"
                 , "(progn (message \"Hello, World!\")"
                 , "       (+ 23 42))"
                 , "#+END_SRC"
                 , ""
                 , "#+RESULTS:"
                 , ": 65" ] =?>
       (mempty :: Blocks)

  , "Source block with toggling header arguments" =:
    T.unlines [ "#+BEGIN_SRC sh :noeval"
              , "echo $HOME"
              , "#+END_SRC"
              ] =?>
    let classes = [ "bash" ]
        params = [ ("org-language", "sh"), ("noeval", "yes") ]
    in codeBlockWith ("", classes, params) "echo $HOME\n"

  , "Source block with line number switch" =:
    T.unlines [ "#+BEGIN_SRC sh -n 10"
              , ":() { :|:& };:"
              , "#+END_SRC"
              ] =?>
    let classes = [ "bash", "numberLines" ]
        params = [ ("org-language", "sh"), ("startFrom", "10") ]
    in codeBlockWith ("", classes, params) ":() { :|:& };:\n"

  , "Source block with multi-word parameter values" =:
    T.unlines [ "#+BEGIN_SRC dot :cmdline -Kdot -Tpng "
              , "digraph { id [label=\"ID\"] }"
              , "#+END_SRC"
              ] =?>
    let classes = [ "dot" ]
        params = [ ("cmdline", "-Kdot -Tpng") ]
    in codeBlockWith ("", classes, params) "digraph { id [label=\"ID\"] }\n"

  , "Example block" =:
       T.unlines [ "#+begin_example"
                 , "A chosen representation of"
                 , "a rule."
                 , "#+eND_exAMPle"
                 ] =?>
       codeBlockWith ("", ["example"], [])
                     "A chosen representation of\na rule.\n"

  , "Code block with caption" =:
      T.unlines [ "#+CAPTION: Functor laws in Haskell"
                , "#+NAME: functor-laws"
                , "#+BEGIN_SRC haskell"
                , "fmap id = id"
                , "fmap (p . q) = (fmap p) . (fmap q)"
                , "#+END_SRC"
                ] =?>
      divWith
         nullAttr
         (mappend
          (plain $ spanWith ("", ["label"], [])
                            (spcSep [ "Functor", "laws", "in", "Haskell" ]))
          (codeBlockWith ("functor-laws", ["haskell"], [])
                         (T.unlines [ "fmap id = id"
                                    , "fmap (p . q) = (fmap p) . (fmap q)"
                                    ])))

  , "Non-letter chars in source block parameters" =:
      T.unlines [ "#+BEGIN_SRC C :tangle xxxx.c :city Zürich"
                , "code body"
                , "#+END_SRC"
                ] =?>
      let params  = [ ("org-language", "C")
                    , ("tangle", "xxxx.c")
                    , ("city", "Zürich")
                    ]
      in codeBlockWith ( "", ["c"], params) "code body\n"
  ]
