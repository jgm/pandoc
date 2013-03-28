{- |
   Module      : Text.Pandoc.Readers.Haddock
   Copyright   : Copyright (C) 2013 David Lazar
   License     : GNU GPL, version 2 or above

   Maintainer  : David Lazar <lazar6@illinois.edu>
   Stability   : alpha

Conversion of Haddock markup to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Haddock
    ( readHaddock
    ) where

import Text.Pandoc.Builder
import Text.Pandoc.Options
import Text.Pandoc.Readers.Haddock.Lex
import Text.Pandoc.Readers.Haddock.Parse

-- | Parse Haddock markup and return a 'Pandoc' document.
readHaddock :: ReaderOptions -- ^ Reader options
            -> String        -- ^ String to parse
            -> Pandoc
readHaddock _ s = Pandoc (Meta [] [] []) blocks
  where
    blocks = case parseParas (tokenise s (0,0)) of
        Nothing -> []
        Just x -> mergeLists (toList x)

-- similar to 'docAppend' in Haddock.Doc
mergeLists :: [Block] -> [Block]
mergeLists (BulletList xs : BulletList ys : blocks)
    = mergeLists (BulletList (xs ++ ys) : blocks)
mergeLists (OrderedList _ xs : OrderedList a ys : blocks) 
    = mergeLists (OrderedList a (xs ++ ys) : blocks)
mergeLists (DefinitionList xs : DefinitionList ys : blocks)
    = mergeLists (DefinitionList (xs ++ ys) : blocks)
mergeLists (x : blocks) = x : mergeLists blocks
mergeLists [] = []
