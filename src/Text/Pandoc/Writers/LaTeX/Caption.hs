{- |
   Module      : Text.Pandoc.Writers.LaTeX.Caption
   Copyright   : Copyright (C) 2006-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Write figure or table captions as LaTeX.
-}
module Text.Pandoc.Writers.LaTeX.Caption
  ( getCaption
  ) where

import Control.Monad.State.Strict
import Data.Monoid (Any(..))
import Data.Text (Text)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.DocLayout (Doc, brackets, empty)
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Pandoc.Writers.LaTeX.Notes (notesToLaTeX)
import Text.Pandoc.Writers.LaTeX.Types
  ( LW, WriterState (stExternalNotes, stNotes) )

getCaption :: PandocMonad m
           => ([Inline] -> LW m (Doc Text))
           -> Bool -> [Inline]
           -> LW m (Doc Text, Doc Text, Doc Text)
getCaption inlineListToLaTeX externalNotes txt = do
  oldExternalNotes <- gets stExternalNotes
  modify $ \st -> st{ stExternalNotes = externalNotes, stNotes = [] }
  capt <- inlineListToLaTeX txt
  footnotes <- if externalNotes
                  then notesToLaTeX <$> gets stNotes
                  else return empty
  modify $ \st -> st{ stExternalNotes = oldExternalNotes, stNotes = [] }
  -- We can't have footnotes in the list of figures/tables, so remove them:
  let getNote (Note _) = Any True
      getNote _        = Any False
  let hasNotes = getAny . query getNote
  captForLof <- if hasNotes txt
                   then brackets <$> inlineListToLaTeX (walk deNote txt)
                   else return empty
  return (capt, captForLof, footnotes)
