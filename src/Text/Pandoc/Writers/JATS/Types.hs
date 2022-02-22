{- |
   Module      : Text.Pandoc.Writers.JATS.Types
   Copyright   : Copyright (C) 2017-2022 John MacFarlane
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

import Citeproc.Types (Reference)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.Text (Text)
import Text.DocLayout (Doc)
import Text.Pandoc.Builder (Block, Inline, Inlines)
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

-- | Environment containing all information relevant for rendering.
data JATSEnv m = JATSEnv
  { jatsTagSet :: JATSTagSet  -- ^ The tag set that's being output

  , jatsBlockWriter   :: (Block -> Bool)
                      -> WriterOptions -> [Block]  -> JATS m (Doc Text)
    -- ^ Converts a block list to JATS, wrapping top-level blocks into a
    -- @<p>@ element if the property evaluates to @True@.
    -- See #7227.

  , jatsInlinesWriter :: WriterOptions -> [Inline] -> JATS m (Doc Text)
    -- ^ Converts an inline list to JATS.

  , jatsReferences    :: [Reference Inlines] -- ^ List of references
  }

-- | JATS writer type
type JATS m = StateT JATSState (ReaderT (JATSEnv m) m)
