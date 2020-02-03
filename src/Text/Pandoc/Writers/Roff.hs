{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.Roff
   Copyright   : Copyright (C) 2007-2020 John MacFarlane
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
import Data.Char (ord, isAscii)
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.String
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.DocLayout
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

combiningAccentsMap :: Map.Map Char Text
combiningAccentsMap = Map.fromList combiningAccents

essentialEscapes :: Map.Map Char Text
essentialEscapes = Map.fromList standardEscapes

-- | Escape special characters for roff.
escapeString :: EscapeMode -> Text -> Text
escapeString e = Text.concat . escapeString' e . Text.unpack
  where
    escapeString' _ [] = []
    escapeString' escapeMode ('\n':'.':xs) =
      "\n\\&." : escapeString' escapeMode xs
    escapeString' escapeMode (x:xs) =
      case Map.lookup x essentialEscapes of
        Just s  -> s : escapeString' escapeMode xs
        Nothing
          | isAscii x -> Text.singleton x : escapeString' escapeMode xs
          | otherwise ->
              case escapeMode of
                AllowUTF8 -> Text.singleton x : escapeString' escapeMode xs
                AsciiOnly ->
                  let accents = catMaybes $ takeWhile isJust
                        (map (\c -> Map.lookup c combiningAccentsMap) xs)
                      rest = drop (length accents) xs
                      s = case Map.lookup x characterCodeMap of
                            Just t  -> "\\[" <> Text.unwords (t:accents) <> "]"
                            Nothing -> "\\[" <> Text.unwords
                              (Text.pack (printf "u%04X" (ord x)) : accents) <> "]"
                  in  s : escapeString' escapeMode rest

characterCodeMap :: Map.Map Char Text
characterCodeMap = Map.fromList characterCodes

fontChange :: (HasChars a, IsString a, PandocMonad m) => MS m (Doc a)
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

withFontFeature :: (HasChars a, IsString a, PandocMonad m)
                => Char -> MS m (Doc a) -> MS m (Doc a)
withFontFeature c action = do
  modify $ \st -> st{ stFontFeatures = Map.adjust not c $ stFontFeatures st }
  begin <- fontChange
  d <- action
  modify $ \st -> st{ stFontFeatures = Map.adjust not c $ stFontFeatures st }
  end <- fontChange
  return $ begin <> d <> end
