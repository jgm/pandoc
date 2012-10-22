{-# LANGUAGE PatternGuards #-}
{-
Copyright (C) 2008 Andrea Rossato <andrea.rossato@ing.unitn.it>

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
   Module      : Text.Pandoc.Biblio
   Copyright   : Copyright (C) 2008-2010 Andrea Rossato
   License     : GNU GPL, version 2 or above

   Maintainer  : Andrea Rossato <andrea.rossato@unitn.it>
   Stability   : alpha
   Portability : portable
-}

module Text.Pandoc.Biblio ( processBiblio ) where

import Data.List
import Data.Char ( isDigit, isPunctuation )
import qualified Data.Map as M
import Text.CSL hiding ( Cite(..), Citation(..) )
import qualified Text.CSL as CSL ( Cite(..) )
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.Shared (stringify)
import Text.Parsec hiding (State)
import Control.Monad
import Control.Monad.State

-- | Process a 'Pandoc' document by adding citations formatted
-- according to a CSL style, using 'citeproc' from citeproc-hs.
processBiblio :: Maybe Style -> [Reference] -> Pandoc -> Pandoc
processBiblio Nothing _ p = p
processBiblio _      [] p = p
processBiblio (Just style) r p =
  let p'         = evalState (bottomUpM setHash p) 1
      grps       = queryWith getCitation p'
      result     = citeproc procOpts style r (setNearNote style $
                      map (map toCslCite) grps)
      cits_map   = M.fromList $ zip grps (citations result)
      biblioList = map (renderPandoc' style) (bibliography result)
      Pandoc m b = bottomUp (processCite style cits_map) p'
      b' = bottomUp mvPunct $ deNote b
  in  Pandoc m $ b' ++ biblioList

-- | Substitute 'Cite' elements with formatted citations.
processCite :: Style -> M.Map [Citation] [FormattedOutput] -> Inline -> Inline
processCite s cs (Cite t _) =
   case M.lookup t cs of
        Just (x:xs)
          | isTextualCitation t && not (null xs) ->
             let xs' = renderPandoc s xs
             in  if styleClass s == "note"
                    then Cite t (renderPandoc s [x] ++ [Note [Para xs']])
                    else Cite t (renderPandoc s [x] ++ [Space | not (startWithPunct xs')] ++ xs')
          | otherwise -> if styleClass s == "note"
                            then Cite t [Note [Para $ renderPandoc s (x:xs)]]
                            else Cite t (renderPandoc s (x:xs))
        _             -> Strong [Str "???"]  -- TODO raise error instead?
processCite _ _ x = x

isNote :: Inline -> Bool
isNote (Note _) = True
isNote (Cite _ [Note _]) = True
isNote _ = False

mvPunct :: [Inline] -> [Inline]
mvPunct (Space : Space : xs) = Space : xs
mvPunct (Space : x : ys) | isNote x, startWithPunct ys =
   Str (headInline ys) : x : tailFirstInlineStr ys
mvPunct (Space : x : ys) | isNote x = x : ys
mvPunct xs = xs

sanitize :: [Inline] -> [Inline]
sanitize xs | endWithPunct xs = toCapital xs
            | otherwise       = toCapital (xs ++ [Str "."])

deNote :: [Block] -> [Block]
deNote = topDown go
  where go (Note [Para xs]) = Note $ bottomUp go' [Para $ sanitize xs]
        go (Note xs) = Note $ bottomUp go' xs
        go x = x
        go' (Note [Para xs]:ys) =
             if startWithPunct ys && endWithPunct xs
                then initInline xs ++ ys
                else xs ++ ys
        go' xs = xs

isTextualCitation :: [Citation] -> Bool
isTextualCitation (c:_) = citationMode c == AuthorInText
isTextualCitation _     = False

-- | Retrieve all citations from a 'Pandoc' docuument. To be used with
-- 'queryWith'.
getCitation :: Inline -> [[Citation]]
getCitation i | Cite t _ <- i = [t]
              | otherwise     = []

setHash :: Citation -> State Int Citation
setHash c = do
  ident <- get
  put $ ident + 1
  return c{ citationHash = ident }

toCslCite :: Citation -> CSL.Cite
toCslCite c
    = let (l, s)  = locatorWords $ citationSuffix c
          (la,lo) = parseLocator l
          s'      = case (l,s,citationMode c) of
                         -- treat a bare locator as if it begins with comma
                         -- so @item1 [blah] is like [@item1, blah]
                         ("",(x:_),AuthorInText) | not (isPunct x)
                                                 -> [Str ",",Space] ++ s
                         _                       -> s
          isPunct (Str (x:_)) = isPunctuation x
          isPunct _           = False
          citMode = case citationMode c of
                      AuthorInText   -> (True, False)
                      SuppressAuthor -> (False,True )
                      NormalCitation -> (False,False)
      in   emptyCite { CSL.citeId         = citationId c
                     , CSL.citePrefix     = PandocText $ citationPrefix c
                     , CSL.citeSuffix     = PandocText s'
                     , CSL.citeLabel      = la
                     , CSL.citeLocator    = lo
                     , CSL.citeNoteNumber = show $ citationNoteNum c
                     , CSL.authorInText   = fst citMode
                     , CSL.suppressAuthor = snd citMode
                     , CSL.citeHash       = citationHash c
                     }

locatorWords :: [Inline] -> (String, [Inline])
locatorWords inp =
  case parse pLocatorWords "suffix" $ breakup inp of
       Right r   -> r
       Left _    -> ("",inp)
   where breakup [] = []
         breakup (Str x : xs) = map Str (splitup x) ++ breakup xs
         breakup (x : xs) = x : breakup xs
         splitup = groupBy (\x y -> x /= '\160' && y /= '\160')

pLocatorWords :: Parsec [Inline] st (String, [Inline])
pLocatorWords = do
  l <- pLocator
  s <- getInput -- rest is suffix
  if length l > 0 && last l == ','
     then return (init l, Str "," : s)
     else return (l, s)

pMatch :: (Inline -> Bool) -> Parsec [Inline] st Inline
pMatch condition = try $ do
  t <- anyToken
  guard $ condition t
  return t

pSpace :: Parsec [Inline] st Inline
pSpace = pMatch (\t -> t == Space || t == Str "\160")

pLocator :: Parsec [Inline] st String
pLocator = try $ do
  optional $ pMatch (== Str ",")
  optional pSpace
  f  <- many1 (notFollowedBy pSpace >> anyToken)
  gs <- many1 pWordWithDigits
  return $ stringify f ++ (' ' : unwords gs)

pWordWithDigits :: Parsec [Inline] st String
pWordWithDigits = try $ do
  pSpace
  r <- many1 (notFollowedBy pSpace >> anyToken)
  let s = stringify r
  guard $ any isDigit s
  return s

