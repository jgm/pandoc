{-# LANGUAGE NoImplicitPrelude #-}
{-
Copyright (C) 2006-2018 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Main
   Copyright   : Copyright (C) 2006-2018 John MacFarlane
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
