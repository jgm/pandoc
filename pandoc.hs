{-# LANGUAGE NoImplicitPrelude #-}
{- |
   Module      : Main
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Parses command-line options and calls the appropriate readers and
writers.
-}
module Main where
import Prelude
import qualified Control.Exception as E
import Text.Pandoc.App (convertWithOpts, defaultOpts, options, parseOptions)
import Text.Pandoc.Error (handleError)

main :: IO ()
main = E.catch (parseOptions options defaultOpts >>= convertWithOpts)
          (handleError . Left)
