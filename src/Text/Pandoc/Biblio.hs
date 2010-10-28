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
   Copyright   : Copyright (C) 2008 Andrea Rossato
   License     : GNU GPL, version 2 or above

   Maintainer  : Andrea Rossato <andrea.rossato@unitn.it>
   Stability   : alpha
   Portability : portable
-}

module Text.Pandoc.Biblio ( processBiblio ) where

import Control.Monad ( when )
import Data.List
import Data.Unique
import Text.CSL hiding ( Cite(..), Citation(..) )
import qualified Text.CSL as CSL ( Cite(..) )
import Text.Pandoc.Definition

-- | Process a 'Pandoc' document by adding citations formatted
-- according to a CSL style, using 'citeproc' from citeproc-hs.
processBiblio :: String -> [Reference] -> Pandoc -> IO Pandoc
processBiblio cf r p
    = if null r then return p
      else do
        when (null cf) $ error "Missing the needed citation style file"
        csl  <- readCSLFile cf
        p'   <- if styleClass csl == "note"
                then processNote p
                else processWithM setHash p
        let groups     = if styleClass csl /= "note"
                         then queryWith getCitation p'
                         else getNoteCitations p'
            result     = citeproc' csl r (setNearNote csl $ map (map toCslCite) groups)
            cits_map   = zip groups (citations result)
            biblioList = map (read . renderPandoc' csl) (bibliography result)
            Pandoc m b = processWith (processCite csl cits_map) p'
        return $ Pandoc m $ b ++ biblioList

-- | Substitute 'Cite' elements with formatted citations.
processCite :: Style -> [([Citation],[FormattedOutput])] -> Inline -> Inline
processCite s cs il
    | Cite t _ <- il = Cite t (process t)
    | otherwise      = il
    where
      process t = case lookup t cs of
                    Just  i -> read $ renderPandoc s i
                    Nothing -> [Str ("Error processing " ++ show t)]

-- | Retrieve all citations from a 'Pandoc' docuument. To be used with
-- 'queryWith'.
getCitation :: Inline -> [[Citation]]
getCitation i | Cite t _ <- i = [t]
          | otherwise         = []

getNote :: Inline -> [Inline]
getNote i | Note _ <- i = [i]
          | otherwise   = []

getCite :: Inline -> [Inline]
getCite i | Cite _ _ <- i = [i]
          | otherwise     = []

getNoteCitations :: Pandoc -> [[Citation]]
getNoteCitations
    = let cits = concat . flip (zipWith $ setCiteNoteNum) [1..] .
                 map (queryWith getCite) . queryWith getNote
      in  queryWith getCitation . cits

setHash :: Citation -> IO Citation
setHash (Citation i p l nn ao na _)
    = hashUnique `fmap` newUnique >>= return . Citation i p l nn ao na

processNote :: Pandoc  -> IO Pandoc
processNote p = do
  p' <- processWithM setHash p
  let cits     = queryWith getCite p'
      ncits    = map (queryWith getCite) $ queryWith getNote p'
      needNote = cits \\ concat ncits
  return $ processWith (mvCiteInNote needNote) p'

mvCiteInNote :: [Inline] -> Inline -> Inline
mvCiteInNote is i = if i `elem` is then Note [Para [i]] else i

setCiteNoteNum :: [Inline] -> Int -> [Inline]
setCiteNoteNum ((Cite cs o):xs) n = Cite (setCitationNoteNum n cs) o : setCiteNoteNum xs n
setCiteNoteNum               _  _ = []

setCitationNoteNum :: Int -> [Citation] -> [Citation]
setCitationNoteNum i = map $ \c -> c { citationNoteNum = i}

toCslCite :: Citation -> CSL.Cite
toCslCite (Citation i p l nn ao na _)
    = let (la,lo) = parseLocator l
      in   emptyCite { CSL.citeId         = i
                     , CSL.citePrefix     = p
                     , CSL.citeLabel      = la
                     , CSL.citeLocator    = lo
                     , CSL.citeNoteNumber = show nn
                     , CSL.authorOnly     = ao
                     , CSL.suppressAuthor = na
                     }
