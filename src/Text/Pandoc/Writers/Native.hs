{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.Native
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of a 'Pandoc' document to a string representation.
-}
module Text.Pandoc.Writers.Native ( writeNative )
where
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options (WriterOptions (..))
import Text.Show.Pretty (ppDoc)
import Text.PrettyPrint (renderStyle, Style(..), style, char)

-- | Prettyprint Pandoc document.
writeNative :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeNative opts (Pandoc meta blocks) = do
  let style' = style{ lineLength = writerColumns opts,
                      ribbonsPerLine = 1.2 }
  return $ T.pack $ renderStyle style' $
    case writerTemplate opts of
      Just _  -> ppDoc (Pandoc meta blocks) <> char '\n'
      Nothing -> ppDoc blocks
