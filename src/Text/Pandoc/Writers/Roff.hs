{-# LANGUAGE NoImplicitPrelude #-}
{-
Copyright (C) 2007-2018 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.Roff
   Copyright   : Copyright (C) 2007-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Common functions for roff writers (man, ms).
-}

module Text.Pandoc.Writers.Roff (
      WriterState(..)
    , defaultWriterState
    , MS
    , Note
    , EscapeMode(..)
    , escapeString
    , withFontFeature
    ) where
import Prelude
import Data.Char (ord, isAscii)
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Pretty
import Text.Printf (printf)
import Text.Pandoc.RoffChar (standardEscapes,
                              characterCodes, combiningAccents)

data WriterState = WriterState { stHasInlineMath :: Bool
                               , stFirstPara     :: Bool
                               , stNotes         :: [Note]
                               , stSmallCaps     :: Bool
                               , stHighlighting  :: Bool
                               , stInHeader      :: Bool
                               , stFontFeatures  :: Map.Map Char Bool
                               , stHasTables     :: Bool
                               }

defaultWriterState :: WriterState
defaultWriterState = WriterState{ stHasInlineMath = False
                                , stFirstPara     = True
                                , stNotes         = []
                                , stSmallCaps     = False
                                , stHighlighting  = False
                                , stInHeader      = False
                                , stFontFeatures  = Map.fromList [
                                                       ('I',False)
                                                     , ('B',False)
                                                     , ('C',False)
                                                     ]
                                , stHasTables     = False
                                }

type Note = [Block]

type MS = StateT WriterState

data EscapeMode = AllowUTF8        -- ^ use preferred man escapes
                | AsciiOnly        -- ^ escape everything
                deriving Show

combiningAccentsMap :: Map.Map Char String
combiningAccentsMap = Map.fromList combiningAccents

essentialEscapes :: Map.Map Char String
essentialEscapes = Map.fromList standardEscapes

-- | Escape special characters for roff.
escapeString :: EscapeMode -> String -> String
escapeString _ [] = []
escapeString escapeMode ('\n':'.':xs) =
  '\n':'\\':'&':'.':escapeString escapeMode xs
escapeString escapeMode (x:xs) =
  case Map.lookup x essentialEscapes of
    Just s  -> s ++ escapeString escapeMode xs
    Nothing
     | isAscii x -> x : escapeString escapeMode xs
     | otherwise ->
        case escapeMode of
          AllowUTF8 -> x : escapeString escapeMode xs
          AsciiOnly ->
            let accents = catMaybes $ takeWhile isJust
                  (map (\c -> Map.lookup c combiningAccentsMap) xs)
                rest = drop (length accents) xs
                s = case Map.lookup x characterCodeMap of
                      Just t  -> "\\[" <> unwords (t:accents) <> "]"
                      Nothing -> "\\[" <> unwords
                       (printf "u%04X" (ord x) : accents) <> "]"
            in  s ++ escapeString escapeMode rest

characterCodeMap :: Map.Map Char String
characterCodeMap = Map.fromList characterCodes

fontChange :: PandocMonad m => MS m Doc
fontChange = do
  features <- gets stFontFeatures
  inHeader <- gets stInHeader
  let filling = ['C' | fromMaybe False $ Map.lookup 'C' features] ++
                ['B' | inHeader ||
                       fromMaybe False (Map.lookup 'B' features)] ++
                ['I' | fromMaybe False $ Map.lookup 'I' features]
  return $
    if null filling
       then text "\\f[R]"
       else text $ "\\f[" ++ filling ++ "]"

withFontFeature :: PandocMonad m => Char -> MS m Doc -> MS m Doc
withFontFeature c action = do
  modify $ \st -> st{ stFontFeatures = Map.adjust not c $ stFontFeatures st }
  begin <- fontChange
  d <- action
  modify $ \st -> st{ stFontFeatures = Map.adjust not c $ stFontFeatures st }
  end <- fontChange
  return $ begin <> d <> end
