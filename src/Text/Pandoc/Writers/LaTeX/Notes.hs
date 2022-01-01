{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.LaTeX.Notes
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Output tables as LaTeX.
-}
module Text.Pandoc.Writers.LaTeX.Notes
  ( notesToLaTeX
  ) where

import Data.List (intersperse)
import Text.DocLayout ( Doc, braces, empty, text, vcat, ($$))
import Data.Text (Text)

notesToLaTeX :: [Doc Text] -> Doc Text
notesToLaTeX = \case
  [] -> empty
  ns -> (case length ns of
            n | n > 1 -> "\\addtocounter" <>
                         braces "footnote" <>
                         braces (text $ show $ 1 - n)
              | otherwise -> empty)
        $$
        vcat (intersperse
               ("\\addtocounter" <> braces "footnote" <> braces "1")
               $ map (\x -> "\\footnotetext" <> braces x)
               $ reverse ns)
