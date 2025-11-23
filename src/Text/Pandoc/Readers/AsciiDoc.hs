{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
   Module      : Text.Pandoc.Readers.AsciiDoc
   Copyright   : Copyright (C) 2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Reads and evaluates a AsciiDoc document as a Pandoc AST.
-}
module Text.Pandoc.Readers.AsciiDoc
  ( readAsciiDoc
  )
where

import Text.Pandoc.Class
import Text.Pandoc.Sources
import Text.Parsec.Pos (newPos)
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Shared (addPandocAttributes, tshow)
import qualified Text.Pandoc.UTF8 as UTF8
import AsciiDoc (ParseOptions(..), SourcePosOption(..), parseDoc, Pos(..))
import qualified AsciiDoc as A
import Text.Pandoc.Error (PandocError(..))
import Control.Monad.Except (throwError)
import qualified Data.Text as T
import Text.Pandoc.Builder
import Text.Pandoc.Logging
import Text.Pandoc.Emoji (emojiToInline)
import Control.Monad.Reader
import qualified Data.Foldable as F
import Data.List (foldl')
import Data.ByteString (ByteString)
-- import Debug.Trace

-- | Read AsciiDoc from an input string and return a Pandoc document.
readAsciiDoc :: (PandocMonad m, ToSources a) => ReaderOptions -> a -> m Pandoc
readAsciiDoc opts inp = do
  let sources = toSources inp
  case A.parseDocument (UTF8.fromText $ sourcesToText sources) of
    Left e -> throwError $ PandocParseError $ T.pack $ show e
    Right d -> todo

-- TODO write a MonadError A.ParseError instance for PandocMonad
-- TODO add the function to read an include file (perhaps use our fancy one with loop detection)
