{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.LaTeX.Notes
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Output deferred footnote texts as LaTeX.
-}
module Text.Pandoc.Writers.LaTeX.Notes
  ( notesToLaTeX
  ) where

import Text.DocLayout ( Doc, braces, text, vcat )
import Data.Text (Text)

-- | Render the texts of notes whose marks were produced with
-- @\\pandocfootnotemark{n}@. @\\pandocfootnotetext{n}@ restores
-- the footnote mark and hyperref anchor saved by the corresponding
-- mark (see the template). Notes are stored in reverse order.
notesToLaTeX :: [(Int, Doc Text)] -> Doc Text
notesToLaTeX = vcat . map toNoteText . reverse
  where
    toNoteText (num, x) =
      "\\pandocfootnotetext" <> braces (text (show num)) <> braces x
