{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Native
   Copyright   : Copyright (C) 2011-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of a string representation of a pandoc type (@Pandoc@,
@[Block]@, @Block@, @[Inline]@, or @Inline@) to a @Pandoc@ document.
-}
module Text.Pandoc.Readers.Native ( readNative ) where

import Text.Pandoc.Definition
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Shared (safeRead)

import Control.Monad.Except (throwError)
import Data.Text (Text)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Error
import Text.Pandoc.Sources (ToSources(..), sourcesToText)

-- | Read native formatted text and return a Pandoc document.
-- The input may be a full pandoc document, a block list, a block,
-- an inline list, or an inline.  Thus, for example,
--
-- > Str "hi"
--
-- will be treated as if it were
--
-- > Pandoc nullMeta [Plain [Str "hi"]]
--
readNative :: (PandocMonad m, ToSources a)
           => ReaderOptions
           -> a
           -> m Pandoc
readNative _ s =
  let t = sourcesToText . toSources $ s
  in  case maybe (Pandoc nullMeta <$> readBlocks t) Right (safeRead t) of
        Right doc -> return doc
        Left _    -> throwError $ PandocParseError "couldn't read native"

readBlocks :: Text -> Either PandocError [Block]
readBlocks s = maybe ((:[]) <$> readBlock s) Right (safeRead s)

readBlock :: Text -> Either PandocError Block
readBlock s = maybe (Plain <$> readInlines s) Right (safeRead s)

readInlines :: Text -> Either PandocError [Inline]
readInlines s = maybe ((:[]) <$> readInline s) Right (safeRead s)

readInline :: Text -> Either PandocError Inline
readInline s = maybe (Left . PandocParseError $ "Could not read: " <> s) Right (safeRead s)
