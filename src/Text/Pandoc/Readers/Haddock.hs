{- |
   Module      : Text.Pandoc.Readers.Haddock
   Copyright   : Copyright (C) 2013 David Lazar
   License     : GNU GPL, version 2 or above

   Maintainer  : David Lazar <lazar6@illinois.edu>,
                 John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha

Conversion of Haddock markup to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Haddock
    ( readHaddock
    ) where

import Text.Pandoc.Builder (Blocks, Inlines)
import qualified Text.Pandoc.Builder as B
import Data.Monoid
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Documentation.Haddock.Parser (parseParas, Identifier)
import Documentation.Haddock.Types

-- | Parse Haddock markup and return a 'Pandoc' document.
readHaddock :: ReaderOptions -- ^ Reader options
            -> String        -- ^ String to parse
            -> Pandoc
readHaddock _ = B.doc . docHToBlocks . parseParas

docHToBlocks :: DocH mod Identifier -> Blocks
docHToBlocks d =
  case d of
    DocAppend d1 d2 -> mappend (docHToBlocks d1) (docHToBlocks d2)
    DocParagraph ils -> B.para $ docHToInlines ils

docHToInlines :: DocH mod Identifier -> Inlines
docHToInlines d =
  case d of
    DocAppend d1 d2 -> mappend (docHToInlines d1) (docHToInlines d2)
    DocString s -> B.text s

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
