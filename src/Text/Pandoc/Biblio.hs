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
        p'   <- processWithM setHash p
        let (nts,grps) = if styleClass csl /= "note"
                         then (,) [] $ queryWith getCitation p'
                         else let cits   = queryWith getCite p'
                                  ncits  = map (queryWith getCite) $ queryWith getNote p'
                                  needNt = cits \\ concat ncits
                              in (,) needNt $ getNoteCitations needNt p'
            result     = citeproc csl r (setNearNote csl $ map (map toCslCite) grps)
            cits_map   = zip grps (citations result)
            biblioList = map (renderPandoc' csl) (bibliography result)
            Pandoc m b = processWith (processCite csl cits_map) p'
        return . generateNotes nts . Pandoc m $ b ++ biblioList

-- | Substitute 'Cite' elements with formatted citations.
processCite :: Style -> [([Citation],[FormattedOutput])] -> Inline -> Inline
processCite s cs il
    | Cite t _ <- il = Cite t (process t)
    | otherwise      = il
    where
      process t = case lookup t cs of
                    Just  i -> renderPandoc s i
                    Nothing -> [Str ("Error processing " ++ show t)]

-- | Retrieve all citations from a 'Pandoc' docuument. To be used with
-- 'queryWith'.
getCitation :: Inline -> [[Citation]]
getCitation i | Cite t _ <- i = [t]
              | otherwise     = []

getNote :: Inline -> [Inline]
getNote i | Note _ <- i = [i]
          | otherwise   = []

getCite :: Inline -> [Inline]
getCite i | Cite _ _ <- i = [i]
          | otherwise     = []

getNoteCitations :: [Inline] -> Pandoc -> [[Citation]]
getNoteCitations needNote
    = let mvCite i = if i `elem` needNote then Note [Para [i]] else i
          setNote  = processWith mvCite
          getCits  = concat . flip (zipWith $ setCiteNoteNum) [1..] .
                     map (queryWith getCite) . queryWith getNote . setNote
      in  queryWith getCitation . getCits

setHash :: Citation -> IO Citation
setHash (Citation i p l cm nn _)
    = hashUnique `fmap` newUnique >>= return . Citation i p l cm nn

generateNotes :: [Inline] -> Pandoc -> Pandoc
generateNotes needNote = processWith (mvCiteInNote needNote)

procInlines :: ([Inline] -> [Inline]) -> Block -> Block
procInlines f b
    | Plain    inls <- b = Plain    $ f inls
    | Para     inls <- b = Para     $ f inls
    | Header i inls <- b = Header i $ f inls
    | otherwise          = b

mvCiteInNote :: [Inline] -> Block -> Block
mvCiteInNote is = procInlines mvCite
    where
      mvCite :: [Inline] -> [Inline]
      mvCite inls
          | x:i:xs <- inls, startWithPunct xs
          , x == Space,   i `elem_` is = split i xs ++ mvCite (tailFirstInlineStr xs)
          | x:i:xs <- inls
          , x == Space,   i `elem_` is = mvInNote i :  mvCite xs
          | i:xs <- inls, i `elem_` is
          , startWithPunct xs          = split i xs ++ mvCite (tailFirstInlineStr xs)
          | i:xs <- inls, Note _ <- i  = checkNt  i :  mvCite xs
          | i:xs <- inls               = i          :  mvCite xs
          | otherwise                  = []
      elem_ x xs = case x of Cite cs _ -> (Cite cs []) `elem` xs; _ -> False
      split i xs = Str (headInline xs) : mvInNote i : []
      mvInNote i
          | Cite t o <- i = Note [Para [Cite t $ sanitize o]]
          | otherwise     = Note [Para [i                  ]]
      sanitize i
          | endWithPunct i = toCapital i
          | otherwise      = toCapital (i ++ [Str "."])

      checkPt i
          | Cite c o : xs <- i
          , endWithPunct o, startWithPunct xs
          , endWithPunct o = Cite c (initInline o) : checkPt xs
          | x:xs <- i      = x : checkPt xs
          | otherwise      = []
      checkNt  = processWith $ procInlines checkPt

setCiteNoteNum :: [Inline] -> Int -> [Inline]
setCiteNoteNum ((Cite cs o):xs) n = Cite (setCitationNoteNum n cs) o : setCiteNoteNum xs n
setCiteNoteNum               _  _ = []

setCitationNoteNum :: Int -> [Citation] -> [Citation]
setCitationNoteNum i = map $ \c -> c { citationNoteNum = i}

toCslCite :: Citation -> CSL.Cite
toCslCite (Citation i p l cm nn _)
    = let (la,lo) = parseLocator l
          citMode = case cm of
                      AuthorOnly     -> (True, False)
                      SuppressAuthor -> (False,True )
                      NormalCitation -> (False,False)
      in   emptyCite { CSL.citeId         = i
                     , CSL.citePrefix     = p
                     , CSL.citeLabel      = la
                     , CSL.citeLocator    = lo
                     , CSL.citeNoteNumber = show nn
                     , CSL.authorOnly     = fst citMode
                     , CSL.suppressAuthor = snd citMode
                     }
