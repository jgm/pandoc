{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
#ifdef DERIVE_JSON_VIA_TH
{-# LANGUAGE TemplateHaskell    #-}
#endif
{-
Copyright (C) 2012-2018 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.IO.Options
   Copyright   : Copyright (C) 2012-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Data structures for IO options.
-}
module Text.Pandoc.IO.Options
  ( LineEnding (..)
  , defaultLineEnding
  ) where
import Prelude
import GHC.Generics (Generic)
#ifdef DERIVE_JSON_VIA_TH
import Data.Aeson.TH (deriveJSON, defaultOptions)
#else
import Data.Aeson (FromJSON, ToJSON (..), defaultOptions, genericToEncoding)
#endif

-- | The type of line-endings to be used when writing plain-text.
data LineEnding = LF | CRLF | Native deriving (Show, Generic)

-- | Default line ending character(s) used by pandoc.
defaultLineEnding :: LineEnding
defaultLineEnding = Native

#ifdef DERIVE_JSON_VIA_TH
$(deriveJSON defaultOptions ''LineEnding)
#else
instance ToJSON LineEnding where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON LineEnding
#endif
