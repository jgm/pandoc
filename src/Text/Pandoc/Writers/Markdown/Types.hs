{-# LANGUAGE OverloadedStrings   #-}
{- |
   Module      : Text.Pandoc.Writers.Markdown.Types
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable
-}
module Text.Pandoc.Writers.Markdown.Types (
  MarkdownVariant(..),
  WriterState(..),
  WriterEnv(..),
  Notes,
  Ref,
  Refs,
  MD,
  evalMD
  ) where
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Default
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Text (Text)
import Text.Pandoc.Parsing (Key)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition

type Notes = [[Block]]
type Ref   = (Text, Target, Attr)
type Refs  = [Ref]

type MD m = ReaderT WriterEnv (StateT WriterState m)

evalMD :: PandocMonad m => MD m a -> WriterEnv -> WriterState -> m a
evalMD md env st = evalStateT (runReaderT md env) st

data WriterEnv = WriterEnv { envInList          :: Bool
                           , envVariant         :: MarkdownVariant
                           , envRefShortcutable :: Bool
                           , envBlockLevel      :: Int
                           , envEscapeSpaces    :: Bool
                           }

data MarkdownVariant =
      Markua
    | PlainText
    | Commonmark
    | Markdown
    deriving (Show, Eq)

instance Default WriterEnv
  where def = WriterEnv { envInList          = False
                        , envVariant         = Markdown
                        , envRefShortcutable = True
                        , envBlockLevel      = 0
                        , envEscapeSpaces    = False
                        }

data WriterState = WriterState { stNotes   :: Notes
                               , stPrevRefs :: Refs
                               , stRefs    :: Refs
                               , stKeys    :: M.Map Key
                                                (M.Map (Target, Attr) Int)
                               , stLastIdx  :: Int
                               , stIds     :: Set.Set Text
                               , stNoteNum :: Int
                               }

instance Default WriterState
  where def = WriterState{ stNotes = []
                         , stPrevRefs = []
                         , stRefs = []
                         , stKeys = M.empty
                         , stLastIdx = 0
                         , stIds = Set.empty
                         , stNoteNum = 1
                         }
