{- |
   Module      : Text.Pandoc.Class
   Copyright   : Copyright (C) 2016-2020 Jesse Rosenthal, John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

This module defines a type class, 'PandocMonad', for pandoc readers
and writers. A pure instance 'PandocPure' and an impure instance
'PandocIO' are provided.  This allows users of the library to choose
whether they want conversions to perform IO operations (such as
reading include files or images).
-}

module Text.Pandoc.Class
  ( module Text.Pandoc.Class.CommonState
  , module Text.Pandoc.Class.PandocIO
  , module Text.Pandoc.Class.PandocMonad
  , module Text.Pandoc.Class.PandocPure
  , Translations
  ) where

import Text.Pandoc.Class.CommonState (CommonState (..))
import Text.Pandoc.Class.PandocMonad
import Text.Pandoc.Class.PandocIO
import Text.Pandoc.Class.PandocPure
import Text.Pandoc.Translations (Translations)
