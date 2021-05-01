{-# LANGUAGE FlexibleInstances #-}
{- |
   Module      : Text.Pandoc.Readers.LaTeX.Types
   Copyright   : Copyright (C) 2017-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Types for LaTeX tokens and macros.
-}
module Text.Pandoc.Readers.LaTeX.Types ( Tok(..)
                                       , TokType(..)
                                       , Macro(..)
                                       , ArgSpec(..)
                                       , ExpansionPoint(..)
                                       , SourcePos
                                       )
where
import Data.Text (Text)
import Text.Parsec.Pos (SourcePos, sourceName)
import Text.Pandoc.Sources
import Data.List (groupBy)

data TokType = CtrlSeq Text | Spaces | Newline | Symbol | Word | Comment |
               Esc1    | Esc2   | Arg Int
     deriving (Eq, Ord, Show)

data Tok = Tok SourcePos TokType Text
     deriving (Eq, Ord, Show)

instance ToSources [Tok] where
  toSources = Sources
    . map (\ts -> case ts of
                    Tok p _ _ : _ -> (p, mconcat $ map tokToText ts)
                    _ -> error "toSources [Tok] encountered empty group")
    . groupBy (\(Tok p1 _ _) (Tok p2 _ _) -> sourceName p1 == sourceName p2)

tokToText :: Tok -> Text
tokToText (Tok _ _ t) = t

data ExpansionPoint = ExpandWhenDefined | ExpandWhenUsed
     deriving (Eq, Ord, Show)

data Macro = Macro ExpansionPoint [ArgSpec] (Maybe [Tok]) [Tok]
     deriving Show

data ArgSpec = ArgNum Int | Pattern [Tok]
     deriving Show
