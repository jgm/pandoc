{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Input.Name
-- Copyright   :  (c) John MacFarlane
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  John MacFarlane <fiddlosopher@gmail.com>
-- Stability   :  unstable
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Text.Pandoc.Citeproc.Name
    ( toName
    , NameOpts(..)
    , emptyName
    )
    where

import Text.Pandoc.Definition
import Text.Pandoc.Shared (stringify)
import Citeproc.Types
import Citeproc.Pandoc ()
import Text.Pandoc.Citeproc.Util (splitStrWhen)
import qualified Data.Text              as T
import           Data.List.Split        (splitWhen, wordsBy)
import           Control.Monad.RWS      hiding ((<>))
import Data.Char (isUpper, isDigit)
import Data.List (foldl')

emptyName :: Name
emptyName =
    Name {  nameFamily              = Nothing
          , nameGiven               = Nothing
          , nameDroppingParticle    = Nothing
          , nameNonDroppingParticle = Nothing
          , nameSuffix              = Nothing
          , nameLiteral             = Nothing
          , nameCommaSuffix         = False
          , nameStaticOrdering      = False
          }

-- | Options for 'toName'.
data NameOpts =
  NameOpts
    { nameOptsPrefixIsNonDroppingParticle :: Bool
        -- ^ Treat a prefix on the last name as a non-dropping particle
        -- (default is to treat it as a dropping particle). This corresponds
        -- to the biblatex option @useprefix@.
    , nameOptsUseJuniorComma              :: Bool
        -- ^ Put a comma before a suffix like "Jr." This corresponds to the
        -- biblatex option @juniorcomma@.
    } deriving (Show)

-- | Parse a list of 'Inline's into a citeproc 'Name', identifying
-- first and last name, particles, suffixes.
toName :: MonadPlus m => NameOpts -> [Inline] -> m Name
toName _ [Str "others"] =
  return emptyName{ nameLiteral = Just "others" }
toName _ [Span ("",[],[]) ils] = -- corporate author
  return emptyName{ nameLiteral = Just $ stringify ils }
-- extended BibLaTeX name format - see #266
toName _ ils@(Str ys:_) | T.any (== '=') ys = do
  let commaParts = splitWhen (== Str ",")
                   . splitStrWhen (\c -> c == ',' || c == '=' || c == '\160')
                   $ ils
  let addPart ag (Str "given" : Str "=" : xs) =
        ag{ nameGiven = case nameGiven ag of
                          Nothing -> Just $ stringify xs
                          Just t  -> Just $ t <> " " <> stringify xs }
      addPart ag (Str "family" : Str "=" : xs) =
        ag{ nameFamily = Just $ stringify xs }
      addPart ag (Str "prefix" : Str "=" : xs) =
        ag{ nameDroppingParticle =  Just $ stringify xs }
      addPart ag (Str "useprefix" : Str "=" : Str "true" : _) =
        ag{ nameNonDroppingParticle = nameDroppingParticle ag
          , nameDroppingParticle    = Nothing }
      addPart ag (Str "suffix" : Str "=" : xs) =
        ag{ nameSuffix = Just $ stringify xs }
      addPart ag (Space : xs) = addPart ag xs
      addPart ag _ = ag
  return $ foldl' addPart emptyName commaParts
-- First von Last
-- von Last, First
-- von Last, Jr ,First
-- NOTE: biblatex and bibtex differ on:
-- Drummond de Andrade, Carlos
-- bibtex takes "Drummond de" as the von;
-- biblatex takes the whole as a last name.
-- See https://github.com/plk/biblatex/issues/236
-- Here we implement the more sensible biblatex behavior.
toName opts ils = do
  let words' = wordsBy (\x -> x == Space || x == Str "\160")
  let commaParts = map words' $ splitWhen (== Str ",")
                              $ splitStrWhen
                                   (\c -> c == ',' || c == '\160') ils
  let (first, vonlast, jr) =
          case commaParts of
               --- First is the longest sequence of white-space separated
               -- words starting with an uppercase and that is not the
               -- whole string. von is the longest sequence of whitespace
               -- separated words whose last word starts with lower case
               -- and that is not the whole string.
               [fvl]      -> let (caps', rest') = span isCapitalized fvl
                             in  if null rest' && not (null caps')
                                 then (init caps', [last caps'], [])
                                 else (caps', rest', [])
               [vl,f]     -> (f, vl, [])
               (vl:j:f:_) -> (f, vl, j )
               []         -> ([], [], [])

  let (von, lastname) =
                 case break isCapitalized vonlast of
                        (vs@(_:_), []) -> (init vs, [last vs])
                        (vs, ws)       -> (vs, ws)
  let prefix = T.unwords $ map stringify von
  let family = T.unwords $ map stringify lastname
  let suffix = T.unwords $ map stringify jr
  let given = T.unwords $ map stringify first
  return
    Name {  nameFamily              = if T.null family
                                         then Nothing
                                         else Just family
          , nameGiven               = if T.null given
                                         then Nothing
                                         else Just given
          , nameDroppingParticle    = if nameOptsPrefixIsNonDroppingParticle opts ||
                                          T.null prefix
                                         then Nothing
                                         else Just prefix
          , nameNonDroppingParticle = if nameOptsPrefixIsNonDroppingParticle opts &&
                                          not (T.null prefix)
                                         then Just prefix
                                         else Nothing
          , nameSuffix              = if T.null suffix
                                         then Nothing
                                         else Just suffix
          , nameLiteral             = Nothing
          , nameCommaSuffix         = nameOptsUseJuniorComma opts
          , nameStaticOrdering      = False
          }

isCapitalized :: [Inline] -> Bool
isCapitalized (Str (T.uncons -> Just (c,cs)) : rest)
  | isUpper c = True
  | isDigit c = isCapitalized (Str cs : rest)
  | otherwise = False
isCapitalized (_:rest) = isCapitalized rest
isCapitalized [] = True
