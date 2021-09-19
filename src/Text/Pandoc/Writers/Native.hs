{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.Native
   Copyright   : Copyright (C) 2006-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of a 'Pandoc' document to a string representation.
-}
module Text.Pandoc.Writers.Native ( writeNative )
where
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options (WriterOptions (..))
import Text.Pretty.Simple (pShowOpt, defaultOutputOptionsNoColor,
                           OutputOptions(..), StringOutputStyle(..))

-- | Prettyprint Pandoc document.
writeNative :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeNative opts (Pandoc meta blocks) = do
  let popts = defaultOutputOptionsNoColor{
               outputOptionsIndentAmount = 2,
               outputOptionsPageWidth = writerColumns opts,
               outputOptionsCompact = True,
               outputOptionsCompactParens = False,
               outputOptionsStringStyle = Literal }
  return $
    case writerTemplate opts of
      Just _  -> TL.toStrict $ pShowOpt popts (Pandoc meta blocks) <> "\n"
      Nothing -> TL.toStrict $ pShowOpt popts blocks
