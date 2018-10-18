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
   Module      : Text.Pandoc.Writers.Groff
   Copyright   : Copyright (C) 2007-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Common functions for groff writers (man, ms).
-}

module Text.Pandoc.Writers.Groff (
      WriterState(..)
    , defaultWriterState
    , MS
    , Note
    , escapeChar
    , escapeString
    , escapeCode
    , withFontFeature
    ) where
import Prelude
import Data.Char (ord, isAscii)
import Control.Monad.State.Strict
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Pretty
import Text.Printf (printf)
import Text.Pandoc.GroffChar (essentialEscapes, characterCodes)

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

escapeChar :: Bool -> Char -> String
escapeChar useAscii c =
  case Map.lookup c essentialEscapes of
       Just s  -> s
       Nothing
         | useAscii
         , not (isAscii c) ->
             case Map.lookup c characterCodeMap of
                  Just t  -> "\\[" <> t <> "]"
                  Nothing -> printf "\\[u%04X]" (ord c)
         | otherwise -> [c]

-- | Escape special characters for groff.
escapeString :: Bool -> String -> String
escapeString useAscii = concatMap (escapeChar useAscii)

-- | Escape a literal (code) section for groff.
escapeCode :: Bool -> String -> String
escapeCode useAScii = intercalate "\n" . map escapeLine . lines
  where escapeCodeChar ' '  = "\\ "
        escapeCodeChar '\t' = "\\\t"
        escapeCodeChar c    = escapeChar useAScii c
        escapeLine codeline =
          case concatMap escapeCodeChar codeline of
            a@('.':_) -> "\\&" ++ a
            b         -> b

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
