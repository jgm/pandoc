{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
Copyright (C) 2008 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.TH
   Copyright   : Copyright (C) 2006-8 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Template haskell functions used by Pandoc modules.
-}
module Text.Pandoc.TH (
                        contentsOf,
                        binaryContentsOf
                      ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift (..))
import qualified Data.ByteString as B
import Data.ByteString.Internal ( w2c )
import Prelude hiding ( readFile )
#ifdef _UTF8STRING
import System.IO.UTF8
#else
import Text.Pandoc.UTF8
#endif

-- | Insert contents of text file into a template.
contentsOf :: FilePath -> ExpQ
contentsOf p = lift =<< (runIO $ readFile p)

-- | Insert contents of binary file into a template.
-- Note that @Data.ByteString.readFile@ uses binary mode on windows.
binaryContentsOf :: FilePath -> ExpQ
binaryContentsOf p = lift =<< (runIO $ B.readFile p)

instance Lift B.ByteString where
  lift x = return (LitE (StringL $ map w2c $ B.unpack x))
