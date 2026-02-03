{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.MoinMoin
   Copyright   : Copyright (C) 2026 Jonathan Dowland
   License     : GNU GPL, version 2 or above

   Maintainer  : Jonathan Dowland <jmtd@debian.org>
   Stability   : alpha
   Portability : portable

Conversion of MoinMoin text to 'Pandoc' document.
-}

module Text.Pandoc.Readers.MoinMoin( readMoinMoin ) where

import qualified Data.Map.Strict as Map
import Text.Pandoc.Definition
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Parsing (ToSources, toSources)

-- | Read MoinMoin from an input string and return a Pandoc document.
readMoinMoin :: (PandocMonad m, ToSources a)
             =>  ReaderOptions
             ->  a
             ->  m Pandoc
readMoinMoin opts s = let
  sources = toSources s
  meta = Meta $ Map.fromList
    [ ("Title", MetaString "Hello world")
    , ("Author", MetaString "Jon")
    ]
  in return $ Pandoc meta
    [ Header 1 nullAttr [Str "Hello World"]
    , Plain [Str "Hello world"]
    ]
