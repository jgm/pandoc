{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-
Copyright (C) 2007-2010 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Readers.TeXMath
   Copyright   : Copyright (C) 2007-2010 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of TeX math to a list of 'Pandoc' inline elements.
-}
module Text.Pandoc.Readers.TeXMath ( readTeXMath ) where

import Text.Pandoc.Definition
import Text.TeXMath.Types
import Text.TeXMath.Parser

-- | Converts a raw TeX math formula to a list of 'Pandoc' inlines.
-- Defaults to raw formula between @$@ characters if entire formula
-- can't be converted.
readTeXMath :: String    -- ^ String to parse (assumes @'\n'@ line endings)
            -> [Inline]
readTeXMath inp = case texMathToPandoc inp of
                        Left _    -> [Str ("$" ++ inp ++ "$")]
                        Right res -> res

texMathToPandoc :: String -> Either String [Inline]
texMathToPandoc inp = inp `seq`
  case parseFormula inp of
         Left err    -> Left err
         Right exps  -> case expsToInlines exps of
                             Nothing  -> Left "Formula too complex for [Inline]"
                             Just r   -> Right r

expsToInlines :: [Exp] -> Maybe [Inline]
expsToInlines xs = do
  res <- mapM expToInlines xs
  return (concat res)

expToInlines :: Exp -> Maybe [Inline]
expToInlines (ENumber s) = Just [Str s]
expToInlines (EIdentifier s) = Just [Emph [Str s]]
expToInlines (EMathOperator s) = Just [Str s]
expToInlines (ESymbol t s) = Just $ addSpace t (Str s)
  where addSpace Op x = [x, thinspace]
        addSpace Bin x = [medspace, x, medspace]
        addSpace Rel x = [widespace, x, widespace]
        addSpace Pun x = [x, thinspace]
        addSpace _ x = [x]
        thinspace = Str "\x2006"
        medspace  = Str "\x2005"
        widespace = Str "\x2004"
expToInlines (EStretchy x) = expToInlines x
expToInlines (EDelimited start end xs) = do
  xs' <- mapM expToInlines xs
  return $ [Str start] ++ concat xs' ++ [Str end]
expToInlines (EGrouped xs) = expsToInlines xs
expToInlines (ESpace "0.167em") = Just [Str "\x2009"]
expToInlines (ESpace "0.222em") = Just [Str "\x2005"]
expToInlines (ESpace "0.278em") = Just [Str "\x2004"]
expToInlines (ESpace "0.333em") = Just [Str "\x2004"]
expToInlines (ESpace "1em")     = Just [Str "\x2001"]
expToInlines (ESpace "2em")     = Just [Str "\x2001\x2001"]
expToInlines (ESpace _)         = Just [Str " "]
expToInlines (EBinary _ _ _) = Nothing
expToInlines (ESub x y) = do
  x' <- expToInlines x
  y' <- expToInlines y
  return $ x' ++ [Subscript y']
expToInlines (ESuper x y) = do
  x' <- expToInlines x
  y' <- expToInlines y
  return $ x' ++ [Superscript y']
expToInlines (ESubsup x y z) = do
  x' <- expToInlines x
  y' <- expToInlines y
  z' <- expToInlines z
  return $ x' ++ [Subscript y'] ++ [Superscript z']
expToInlines (EDown x y) = expToInlines (ESub x y)
expToInlines (EUp x y) = expToInlines (ESuper x y)
expToInlines (EDownup x y z) = expToInlines (ESubsup x y z)
expToInlines (EText TextNormal x) = Just [Str x]
expToInlines (EText TextBold x) = Just [Strong [Str x]]
expToInlines (EText TextMonospace x) = Just [Code nullAttr x]
expToInlines (EText TextItalic x) = Just [Emph [Str x]]
expToInlines (EText _ x) = Just [Str x]
expToInlines (EOver (EGrouped [EIdentifier [c]]) (ESymbol Accent [accent])) =
    case accent of
         '\x203E' -> Just [Emph [Str [c,'\x0304']]]  -- bar
         '\x00B4' -> Just [Emph [Str [c,'\x0301']]]  -- acute
         '\x0060' -> Just [Emph [Str [c,'\x0300']]]  -- grave
         '\x02D8' -> Just [Emph [Str [c,'\x0306']]]  -- breve
         '\x02C7' -> Just [Emph [Str [c,'\x030C']]]  -- check
         '.'      -> Just [Emph [Str [c,'\x0307']]]  -- dot
         '\x00B0' -> Just [Emph [Str [c,'\x030A']]]  -- ring
         '\x20D7' -> Just [Emph [Str [c,'\x20D7']]]  -- arrow right
         '\x20D6' -> Just [Emph [Str [c,'\x20D6']]]  -- arrow left
         '\x005E' -> Just [Emph [Str [c,'\x0302']]]  -- hat
         '\x0302' -> Just [Emph [Str [c,'\x0302']]]  -- hat
         '~'      -> Just [Emph [Str [c,'\x0303']]]  -- tilde
         _        -> Nothing
expToInlines _ = Nothing


