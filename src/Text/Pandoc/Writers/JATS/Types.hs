{- |
   Module      : Text.Pandoc.Writers.JATS.Types
   Copyright   : Copyright (C) 2017-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Types for pandoc's JATS writer.
-}
module Text.Pandoc.Writers.JATS.Types
  ( JATS
  , JATSEnv (..)
  , JATSState (..)
  , JATSTagSet (..)
  )
where

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.Text (Text)
import Text.DocLayout (Doc)
import Text.Pandoc.Definition (Block, Inline)
import Text.Pandoc.Options (WriterOptions)

-- | JATS tag set variant
data JATSTagSet
  = TagSetArchiving         -- ^ Archiving and Interchange Tag Set
  | TagSetPublishing        -- ^ Journal Publishing Tag Set
  | TagSetArticleAuthoring  -- ^ Article Authoring Tag Set
  deriving (Eq)

-- | Internal state used by the writer.
newtype JATSState = JATSState
  { jatsNotes :: [(Int, Doc Text)]
  }

data JATSEnv m = JATSEnv
  { jatsTagSet :: JATSTagSet
  , jatsInlinesWriter :: WriterOptions -> [Inline] -> JATS m (Doc Text)
  , jatsBlockWriter   :: WriterOptions -> Block    -> JATS m (Doc Text)
  }

-- | JATS writer type
type JATS m = StateT JATSState (ReaderT (JATSEnv m) m)
