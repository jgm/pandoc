{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{- |
Module      : Text.Pandoc.Parsing.Base64
Copyright   : © 2024 Evan Silberman
License     : GPL-2.0-or-later
Maintainer  : John MacFarlane <jgm@berkeley.edu>

Parse large base64 strings efficiently within Pandoc's
normal parsing environment
-}

module Text.Pandoc.Parsing.Base64
  ( parseBase64String )

where

import Data.Text as T
import Data.Attoparsec.Text as A
import Text.Parsec (ParsecT, getInput, setInput, incSourceColumn)
import Text.Pandoc.Sources
import Control.Monad (mzero)

parseBase64String :: Monad m => ParsecT Sources u m Text
parseBase64String = do
  Sources ((pos, txt):rest) <- getInput
  let r = A.parse pBase64 txt
  case r of
    Done remaining consumed -> do
      let pos' = incSourceColumn pos (T.length consumed)
      setInput $ Sources ((pos', remaining):rest)
      return consumed
    _ -> mzero

pBase64 :: A.Parser Text
pBase64 = do
  most <- A.takeWhile1 (A.inClass "A-Za-z0-9+/")
  rest <- A.takeWhile (== '=')
  return $ most <> rest
