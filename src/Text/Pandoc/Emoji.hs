{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
import Prelude
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc.Definition (Inline (Span, Str))
import Text.Pandoc.Emoji.TH (genEmojis)

emojis :: M.Map T.Text T.Text
emojis = M.fromList $(genEmojis "emoji.json")

emojiToInline :: T.Text -> Maybe Inline
emojiToInline emojikey = makeSpan <$> M.lookup emojikey emojis
  where makeSpan = Span ("", ["emoji"], [("data-emoji", emojikey)]) . (:[]) . Str
