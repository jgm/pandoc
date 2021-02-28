{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Readers.LaTeX.Accent
  ( accentCommands )
where

import Text.Pandoc.Class
import Text.Pandoc.Readers.LaTeX.Parsing
import Text.Pandoc.Builder as B
import qualified Data.Map as M
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Text.Pandoc.Parsing
import qualified Data.Text as T
import qualified Data.Text.Normalize as Normalize

accentCommands :: PandocMonad m => LP m Inlines -> M.Map Text (LP m Inlines)
accentCommands tok =
  let accent = accentWith tok
      lit = pure . str
  in M.fromList
  [ ("aa", lit "å")
  , ("AA", lit "Å")
  , ("ss", lit "ß")
  , ("o", lit "ø")
  , ("O", lit "Ø")
  , ("L", lit "Ł")
  , ("l", lit "ł")
  , ("ae", lit "æ")
  , ("AE", lit "Æ")
  , ("oe", lit "œ")
  , ("OE", lit "Œ")
  , ("pounds", lit "£")
  , ("euro", lit "€")
  , ("copyright", lit "©")
  , ("textasciicircum", lit "^")
  , ("textasciitilde", lit "~")
  , ("H", accent '\779' Nothing) -- hungarumlaut
  , ("`", accent '\768' (Just '`')) -- grave
  , ("'", accent '\769' (Just '\'')) -- acute
  , ("^", accent '\770' (Just '^')) -- circ
  , ("~", accent '\771' (Just '~')) -- tilde
  , ("\"", accent '\776' Nothing) -- umlaut
  , (".", accent '\775' Nothing) -- dot
  , ("=", accent '\772' Nothing) -- macron
  , ("|", accent '\781' Nothing) -- vertical line above
  , ("b", accent '\817' Nothing) -- macron below
  , ("c", accent '\807' Nothing) -- cedilla
  , ("G", accent '\783' Nothing) -- doublegrave
  , ("h", accent '\777' Nothing) -- hookabove
  , ("d", accent '\803' Nothing) -- dotbelow
  , ("f", accent '\785' Nothing)  -- inverted breve
  , ("r", accent '\778' Nothing)  -- ringabove
  , ("t", accent '\865' Nothing)  -- double inverted breve
  , ("U", accent '\782' Nothing)  -- double vertical line above
  , ("v", accent '\780' Nothing) -- hacek
  , ("u", accent '\774' Nothing) -- breve
  , ("k", accent '\808' Nothing) -- ogonek
  , ("textogonekcentered", accent '\808' Nothing) -- ogonek
  , ("i", lit "ı")  -- dotless i
  , ("j", lit "ȷ")  -- dotless j
  , ("newtie", accent '\785' Nothing) -- inverted breve
  , ("textcircled", accent '\8413' Nothing) -- combining circle
  ]

accentWith :: PandocMonad m
           => LP m Inlines -> Char -> Maybe Char -> LP m Inlines
accentWith tok combiningAccent fallBack = try $ do
  ils <- tok
  case toList ils of
       (Str (T.uncons -> Just (x, xs)) : ys) -> return $ fromList $
         -- try to normalize to the combined character:
         Str (Normalize.normalize Normalize.NFC
               (T.pack [x, combiningAccent]) <> xs) : ys
       [Space]           -> return $ str $ T.singleton $ fromMaybe combiningAccent fallBack
       []                -> return $ str $ T.singleton $ fromMaybe combiningAccent fallBack
       _                 -> return ils

