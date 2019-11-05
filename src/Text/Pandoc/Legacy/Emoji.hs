module Text.Pandoc.Legacy.Emoji ( emojis, emojiToInline ) where

import qualified Data.Text as T
import qualified Data.Map as M

import qualified Text.Pandoc.Emoji as TP
import qualified Text.Pandoc.Definition as TP

emojis :: M.Map String String
emojis = M.map T.unpack . M.mapKeys T.unpack $ TP.emojis

emojiToInline :: String -> Maybe TP.Inline
emojiToInline = TP.emojiToInline . T.pack
