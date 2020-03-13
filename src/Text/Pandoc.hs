{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

This helper module exports the main writers, readers, and data
structure definitions from the Pandoc libraries.

A typical application will chain together a reader and a writer
to convert strings from one format to another.  For example, the
following simple program will act as a filter converting markdown
fragments to reStructuredText, using reference-style links instead of
inline links:

> module Main where
> import Text.Pandoc
> import Data.Text (Text)
> import qualified Data.Text.IO as T
>
> mdToRST :: Text -> IO Text
> mdToRST txt = runIOorExplode $
>   readMarkdown def txt
>   >>= writeRST def{ writerReferenceLinks = True }
>
> main :: IO ()
> main = do
>   T.getContents >>= mdToRST >>= T.putStrLn

-}

module Text.Pandoc
               (
               -- * Definitions
               module Text.Pandoc.Definition
               -- * Generics
               , module Text.Pandoc.Generic
               -- * Options
               , module Text.Pandoc.Options
               -- * Logging
               , module Text.Pandoc.Logging
               -- * Typeclass
               , module Text.Pandoc.Class
               -- * Error handling
               , module Text.Pandoc.Error
               -- * Readers: converting /to/ Pandoc format
               , module Text.Pandoc.Readers
               -- * Writers: converting /from/ Pandoc format
               , module Text.Pandoc.Writers
               -- * Rendering templates and default templates
               , module Text.Pandoc.Templates
               -- * Miscellaneous
               , pandocVersion
             ) where

import Text.Pandoc.Class
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Generic
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Readers
import Text.Pandoc.Shared (pandocVersion)
import Text.Pandoc.Templates
import Text.Pandoc.Writers
