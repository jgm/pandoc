{- |
   Module      : Text.Pandoc.Writers.Powerpoint
   Copyright   : Copyright (C) 2017-2020 Jesse Rosenthal
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

import Codec.Archive.Zip
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
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
