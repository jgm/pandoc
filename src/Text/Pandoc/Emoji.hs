{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Emoji
   Copyright   : Copyright (C) 2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Emoji symbol lookup from canonical string identifier.
-}
module Text.Pandoc.Emoji ( emojis, emojiToInline ) where
import qualified Text.Emoji as E
import Text.Pandoc.Definition (Inline (Span, Str))
import Data.Text (Text)
import qualified Data.Map as M

emojis :: M.Map Text Text
emojis = M.fromList E.emojis

emojiToInline :: Text -> Maybe Inline
emojiToInline emojikey = makeSpan <$> E.emojiFromAlias emojikey
  where makeSpan = Span ("", ["emoji"], [("data-emoji", emojikey)]) . (:[]) . Str
