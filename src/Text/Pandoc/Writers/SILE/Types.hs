module Text.Pandoc.Writers.SILE.Types
  ( LW
  , WriterState (..)
  , startingState
  ) where

import Control.Monad.State.Strict (StateT)
import Text.Pandoc.Options
  ( WriterOptions (writerTopLevelDivision)
  , TopLevelDivision (..)
  )

-- | LaTeX writer type. The type constructor @m@ will typically be an
-- instance of PandocMonad.
type LW m = StateT WriterState m

data WriterState =
  WriterState {
                stOLLevel       :: Int           -- level of ordered list nesting
              , stOptions       :: WriterOptions -- writer options, so they don't have to be parameter
              , stHasChapters   :: Bool          -- true if document has chapters
              , stEmptyLine     :: Bool          -- true if no content on line
              }

startingState :: WriterOptions -> WriterState
startingState options = WriterState {
                  stOLLevel = 1
                , stOptions = options
                , stHasChapters = case writerTopLevelDivision options of
                                TopLevelPart    -> True
                                TopLevelChapter -> True
                                _               -> False
                , stEmptyLine = True }
