{-# LANGUAGE NoImplicitPrelude #-}


{-
Copyright (C) 2017-2018 Jesse Rosenthal <jrosenthal@jhu.edu>

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
   Module      : Text.Pandoc.Writers.Powerpoint
   Copyright   : Copyright (C) 2017-2018 Jesse Rosenthal
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to powerpoint (pptx). -}

{-
This is a wrapper around two modules:

  - Text.Pandoc.Writers.Powerpoint.Presentation (which converts a
    pandoc document into a Presentation datatype), and

  - Text.Pandoc.Writers.Powerpoint.Output (which converts a
    Presentation into a zip archive, which can be output).
-}

module Text.Pandoc.Writers.Powerpoint (writePowerpoint) where

import Prelude
import Codec.Archive.Zip
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Options (WriterOptions)
import Text.Pandoc.Writers.Shared (fixDisplayMath)
import Text.Pandoc.Writers.Powerpoint.Presentation (documentToPresentation)
import Text.Pandoc.Writers.Powerpoint.Output (presentationToArchive)
import qualified Data.ByteString.Lazy as BL

writePowerpoint :: (PandocMonad m)
                => WriterOptions  -- ^ Writer options
                -> Pandoc         -- ^ Document to convert
                -> m BL.ByteString
writePowerpoint opts (Pandoc meta blks) = do
  let blks' = walk fixDisplayMath blks
  let (pres, logMsgs) = documentToPresentation opts (Pandoc meta blks')
  mapM_ report logMsgs
  archv <- presentationToArchive opts pres
  return $ fromArchive archv
