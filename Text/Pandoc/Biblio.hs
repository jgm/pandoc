{-# LANGUAGE PatternGuards, CPP #-}
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

   Maintainer  : Andrea Rossato <andrea.rossato@ing.unitn.it>
   Stability   : alpha
   Portability : portable
-}

#ifdef _CITEPROC
module Text.Pandoc.Biblio ( processBiblio ) where

import Control.Monad ( when )
import Data.List
import Text.CSL
import Text.Pandoc.Definition

#else
module Text.Pandoc.Biblio () where
#endif

#ifdef _CITEPROC
processBiblio :: String -> [Reference] -> Pandoc -> IO Pandoc
processBiblio cf r p
    = if null r then return p
      else do
        when (null cf) $ error "Missing the needed citation style file"
        csl  <- readCSLFile cf
        let groups     = queryPandoc getCite p
            citations  = zip groups . processCitations csl r $ groups
            Pandoc m b = processPandoc (processCite citations) p
        return $ Pandoc m $ b ++ renderBiblio csl r p

renderBiblio :: Style -> [Reference] -> Pandoc -> [Block]
renderBiblio s r p
    = map (read . renderPandoc') $ processBibliography s refs
      where cits = nub . map fst . concat . queryPandoc getCite $ p
            refs = getRefs r $ zip cits (repeat "")

processCite :: [([Target],[FormattedOutput])] -> Inline -> Inline
processCite cs il
    | Cite t _ <- il = Cite t (process t)
    | otherwise      = il
    where
      process t = case elemIndex t (map fst cs) of
                    Just i -> read . renderPandoc $ snd (cs !! i)
                    Nothing -> [Str ("Error processing " ++ show t)]

getCite :: Inline -> [[(String,String)]]
getCite i | Cite t _ <- i = [t]
          | otherwise     = []

getRefs :: [Reference] -> [Target] -> [Reference]
getRefs = map . getReference

#endif
