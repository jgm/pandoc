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
import Data.Unique
import Data.Char ( isDigit )
import qualified Data.Map as M
import Text.CSL hiding ( Cite(..), Citation(..) )
import qualified Text.CSL as CSL ( Cite(..) )
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.Shared (stringify)
import Text.ParserCombinators.Parsec
import Control.Monad

-- | Process a 'Pandoc' document by adding citations formatted
-- according to a CSL style, using 'citeproc' from citeproc-hs.
processBiblio :: FilePath -> Maybe FilePath -> [Reference] -> Pandoc
              -> IO Pandoc
processBiblio cslfile abrfile r p
    = if null r then return p
      else do
        csl <- readCSLFile cslfile
        abbrevs <- case abrfile of
                      Just f  -> readJsonAbbrevFile f
                      Nothing -> return []
        p'   <- bottomUpM setHash p
        let (nts,grps) = if styleClass csl == "note"
                         then let cits   = queryWith getCite p'
                                  ncits  = map (queryWith getCite) $ queryWith getNote p'
                                  needNt = cits \\ concat ncits
                              in (,) needNt $ getNoteCitations needNt p'
                         else (,) [] $ queryWith getCitation p'
            style      = csl { styleAbbrevs = abbrevs }
            result     = citeproc procOpts style r (setNearNote style $
                            map (map toCslCite) grps)
            cits_map   = M.fromList $ zip grps (citations result)
            biblioList = map (renderPandoc' style) (bibliography result)
            Pandoc m b = bottomUp (procInlines $ processCite style cits_map) p'
        return . generateNotes nts . Pandoc m $ b ++ biblioList

-- | Substitute 'Cite' elements with formatted citations.
processCite :: Style -> M.Map [Citation] [FormattedOutput] -> [Inline] -> [Inline]
processCite s cs (Cite t _ : rest) =
   case M.lookup t cs of
        Just (x:xs) ->
                   if isTextualCitation t
                   then renderPandoc s [x] ++
                         if null xs
                         then processCite s cs rest
                         else [Space,  Cite t (renderPandoc s xs)]
                            ++ processCite s cs rest
                   else Cite t (renderPandoc s (x:xs)) : processCite s cs rest
        _ -> Str ("Error processing " ++ show t) : processCite s cs rest
processCite s cs (x:xs) = bottomUp (processCite s cs) x : processCite s cs xs
processCite _ _ [] = []

procInlines :: ([Inline] -> [Inline]) -> Block -> Block
procInlines f b
    | Plain    inls <- b = Plain    $ f inls
    | Para     inls <- b = Para     $ f inls
    | Header i inls <- b = Header i $ f inls
    | otherwise          = b

isTextualCitation :: [Citation] -> Bool
isTextualCitation (c:_) = citationMode c == AuthorInText
isTextualCitation _     = False

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
          setNote  = bottomUp mvCite
          getCits  = concat . flip (zipWith $ setCiteNoteNum) [1..] .
                     map (queryWith getCite) . queryWith getNote . setNote
      in  queryWith getCitation . getCits

setHash :: Citation -> IO Citation
setHash (Citation i p s cm nn _)
    = hashUnique `fmap` newUnique >>= return . Citation i p s cm nn

generateNotes :: [Inline] -> Pandoc -> Pandoc
generateNotes needNote = bottomUp (mvCiteInNote needNote)

mvCiteInNote :: [Inline] -> Block -> Block
mvCiteInNote is = procInlines mvCite
    where
      mvCite :: [Inline] -> [Inline]
      mvCite inls
          | x:i:xs <- inls, startWithPunct xs
          , x == Space,   i `elem_` is = switch i xs ++ mvCite (tailFirstInlineStr xs)
          | x:i:xs <- inls
          , x == Space,   i `elem_` is = mvInNote i :   mvCite xs
          | i:xs <- inls, i `elem_` is
          , startWithPunct xs          = switch i xs ++ mvCite (tailFirstInlineStr xs)
          | i:xs <- inls, Note _ <- i  = checkNt  i :   mvCite xs
          | i:xs <- inls               = i          :   mvCite xs
          | otherwise                  = []
      elem_  x xs = case x of Cite cs _ -> (Cite cs []) `elem` xs; _ -> False
      switch i xs = Str (headInline xs) : mvInNote i : []
      mvInNote i
          | Cite t o <- i = Note [Para [Cite t $ sanitize o]]
          | otherwise     = Note [Para [i                  ]]
      sanitize i
          | endWithPunct   i = toCapital i
          | otherwise        = toCapital (i ++ [Str "."])

      checkPt i
          | Cite c o : xs <- i , endWithPunct o, startWithPunct xs
                           = Cite c (initInline o) : checkPt xs
          | x:xs <- i      = x : checkPt xs
          | otherwise      = []
      checkNt  = bottomUp $ procInlines checkPt

setCiteNoteNum :: [Inline] -> Int -> [Inline]
setCiteNoteNum ((Cite cs o):xs) n = Cite (setCitationNoteNum n cs) o : setCiteNoteNum xs n
setCiteNoteNum               _  _ = []

setCitationNoteNum :: Int -> [Citation] -> [Citation]
setCitationNoteNum i = map $ \c -> c { citationNoteNum = i}

toCslCite :: Citation -> CSL.Cite
toCslCite c
    = let (l, s)  = locatorWords $ citationSuffix c
          (la,lo) = parseLocator l
          citMode = case citationMode c of
                      AuthorInText   -> (True, False)
                      SuppressAuthor -> (False,True )
                      NormalCitation -> (False,False)
      in   emptyCite { CSL.citeId         = citationId c
                     , CSL.citePrefix     = PandocText $ citationPrefix c
                     , CSL.citeSuffix     = PandocText $ s
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

pLocatorWords :: GenParser Inline st (String, [Inline])
pLocatorWords = do
  l <- pLocator
  s <- getInput -- rest is suffix
  if length l > 0 && last l == ','
     then return (init l, Str "," : s)
     else return (l, s)

pMatch :: (Inline -> Bool) -> GenParser Inline st Inline
pMatch condition = try $ do
  t <- anyToken
  guard $ condition t
  return t

pSpace :: GenParser Inline st Inline
pSpace = pMatch (\t -> t == Space || t == Str "\160")

pLocator :: GenParser Inline st String
pLocator = try $ do
  optional $ pMatch (== Str ",")
  optional pSpace
  f  <- many1 (notFollowedBy pSpace >> anyToken)
  gs <- many1 pWordWithDigits
  return $ stringify f ++ (' ' : unwords gs)

pWordWithDigits :: GenParser Inline st String
pWordWithDigits = try $ do
  pSpace
  r <- many1 (notFollowedBy pSpace >> anyToken)
  let s = stringify r
  guard $ any isDigit s
  return s

