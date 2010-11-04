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
import Data.Char ( toUpper )
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
                         then (,) [] $ queryWith getCite p'
                         else let cits   = queryWith getCite p'
                                  ncits  = map (queryWith getCite) $ queryWith getNote p'
                                  needNt = cits \\ concat ncits
                              in (,) needNt $ getNoteCitations needNt p'
            result     = citeproc' csl r (setNearNote csl $ map toCslCites grps)
            cits_map   = zip grps (citations result)
            biblioList = map (read . renderPandoc' csl) (bibliography result)
            Pandoc m b = processWith (processCite csl cits_map) p'
        return . generateNotes nts . Pandoc m $ b ++ biblioList

-- | Substitute 'Cite' elements with formatted citations.
processCite :: Style -> [(Inline,[FormattedOutput])] -> Inline -> Inline
processCite s cs il
    | Cite o t _ <- il = Cite o t process 
    | otherwise        = il
    where
      process = case lookup il cs of
                    Just  i -> read $ renderPandoc s i
                    Nothing -> [Str ("Error processing " ++ show il ++ " not found in " ++ show cs)]

getNote :: Inline -> [Inline]
getNote i | Note _ <- i = [i]
          | otherwise   = []

getCite :: Inline -> [Inline]
getCite i | Cite _ _ _ <- i = [i]
          | otherwise     = []

getNoteCitations :: [Inline] -> Pandoc -> [Inline]
getNoteCitations needNote
    = let mvCite i = if i `elem` needNote then Note [Para [i]] else i
          setNote  = processWith mvCite
          getCits  = concat . flip (zipWith $ setCiteNoteNum) [1..] .
                     map (queryWith getCite) . queryWith getNote . setNote
      in  queryWith getCite . getCits

setHash :: Citation -> IO Citation
setHash (Citation i p l _)
    = hashUnique `fmap` newUnique >>= return . Citation i p l 

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
          | x:i:xs <- inls, startWPt xs
          , x == Space,   i `elem_` is = split i xs ++ mvCite (tailInline xs)
          | x:i:xs <- inls
          , x == Space,   i `elem_` is = mvInNote i :  mvCite xs
          | i:xs <- inls, i `elem_` is
          , startWPt xs                = split i xs ++ mvCite (tailInline xs)
          | i:xs <- inls, Note _ <- i  = checkNt  i :  mvCite xs
          | i:xs <- inls               = i          :  mvCite xs
          | otherwise                  = []
      elem_ x xs = case x of Cite op cs _ -> (Cite op cs []) `elem` xs; _ -> False
      split i xs = Str (headInline xs) : mvInNote i : []
      mvInNote i
          | Cite op t o <- i = Note [Para [Cite op t $ sanitize o]]
          | otherwise     = Note [Para [i                  ]]
      sanitize i
          | endWPt  i = toCapital i
          | otherwise = toCapital (i ++ [Str "."])

      checkPt i
          | Cite op c o : xs <- i
          , endWPt o, startWPt xs
          , endWPt  o = Cite op c (initInline o) : checkPt xs
          | x:xs <- i = x : checkPt xs
          | otherwise = []
      endWPt   = and . map (`elem` ".,;:!?") . lastInline
      startWPt = and . map (`elem` ".,;:!?") . headInline
      checkNt  = processWith $ procInlines checkPt

headInline :: [Inline] -> String
headInline [] = []
headInline (i:_)
    | Str s <- i = head' s
    | Space <- i = " "
    | otherwise  = headInline $ getInline i
    where
      head' s = if s /= [] then [head s] else []

lastInline :: [Inline] -> String
lastInline [] = []
lastInline (i:[])
    | Str s <- i = last' s
    | Space <- i = " "
    | otherwise  = lastInline $ getInline i
    where
      last' s = if s /= [] then [last s] else []
lastInline (_:xs) = lastInline xs

initInline :: [Inline] -> [Inline]
initInline [] = []
initInline (i:[])
    | Str          s <- i = return $ Str         (init'       s)
    | Emph        is <- i = return $ Emph        (initInline is)
    | Strong      is <- i = return $ Strong      (initInline is)
    | Strikeout   is <- i = return $ Strikeout   (initInline is)
    | Superscript is <- i = return $ Superscript (initInline is)
    | Subscript   is <- i = return $ Subscript   (initInline is)
    | Quoted q    is <- i = return $ Quoted q    (initInline is)
    | SmallCaps   is <- i = return $ SmallCaps   (initInline is)
    | Link      is t <- i = return $ Link        (initInline is) t
    | otherwise           = []
    where
      init' s = if s /= [] then init s else []
initInline (i:xs) = i : initInline xs

tailInline :: [Inline] -> [Inline]
tailInline = mapHeadInline tail'
    where
      tail' s = if s /= [] then tail s else []

toCapital :: [Inline] -> [Inline]
toCapital = mapHeadInline toCap
    where
      toCap s = if s /= [] then toUpper (head s) : tail s else []

mapHeadInline :: (String -> String) -> [Inline] -> [Inline]
mapHeadInline _ [] = []
mapHeadInline f (i:xs)
    | Str          s <- i = Str         (f                s)   : xs
    | Emph        is <- i = Emph        (mapHeadInline f is)   : xs
    | Strong      is <- i = Strong      (mapHeadInline f is)   : xs
    | Strikeout   is <- i = Strikeout   (mapHeadInline f is)   : xs
    | Superscript is <- i = Superscript (mapHeadInline f is)   : xs
    | Subscript   is <- i = Subscript   (mapHeadInline f is)   : xs
    | Quoted q    is <- i = Quoted q    (mapHeadInline f is)   : xs
    | SmallCaps   is <- i = SmallCaps   (mapHeadInline f is)   : xs
    | Link      is t <- i = Link        (mapHeadInline f is) t : xs
    | otherwise           = []

getInline :: Inline -> [Inline]
getInline i
    | Emph        is <- i = is
    | Strong      is <- i = is
    | Strikeout   is <- i = is
    | Superscript is <- i = is
    | Subscript   is <- i = is
    | Quoted _    is <- i = is
    | SmallCaps   is <- i = is
    | Link      is _ <- i = is
    | otherwise           = []

setCiteNoteNum :: [Inline] -> Int -> [Inline]
setCiteNoteNum ((Cite op cs o):xs) n = Cite op { citationNoteNum = n } cs o : setCiteNoteNum xs n
setCiteNoteNum               _  _ = []

toCslCites :: Inline -> [CSL.Cite]
toCslCites (Cite op t _) = map (toCslCite op) t
toCslCites _ = []

toCslCite :: CiteOptions -> Citation -> CSL.Cite
toCslCite op (Citation i p l _)
    = let (la,lo) = parseLocator l
      in   emptyCite { CSL.citeId         = i
                     , CSL.citePrefix     = p
                     , CSL.citeLabel      = la
                     , CSL.citeLocator    = lo
                     , CSL.citeNoteNumber = show $ citationNoteNum op
                     , CSL.authorOnly     = citationVariant op == AuthorOnlyCitation
                     , CSL.suppressAuthor = citationVariant op == NoAuthorCitation 
                     }
