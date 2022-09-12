{- |
   Module      : Text.Pandoc.Readers.Custom
   Copyright   : Copyright (C) 2021-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Supports custom parsers written in Lua which produce a Pandoc AST.
-}
module Text.Pandoc.Readers.Custom ( readCustom ) where
import Text.Pandoc.Lua.Reader ( readCustom )
