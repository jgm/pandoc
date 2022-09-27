{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{- |
   Module      : Text.Pandoc.Writers.Custom
   Copyright   : 2012-2022 John MacFarlane,
   License     : GNU GPL, version 2 or above
   Maintainer  : John MacFarlane <jgm@berkeley.edu>

Conversion of 'Pandoc' documents to custom markup using
a Lua writer.
-}
module Text.Pandoc.Writers.Custom ( writeCustom ) where

import Text.Pandoc.Lua.Writer ( writeCustom )
