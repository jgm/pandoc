{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.Native
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Utility functions and definitions used by the various Pandoc modules.
-}
module Text.Pandoc.Writers.Native ( writeNative )
where
import Text.Pandoc.Shared ( WriterOptions(..) )
import Data.List ( intercalate )
import Text.Pandoc.Definition

-- | Indent string as a block.
indentBy :: Int    -- ^ Number of spaces to indent the block 
         -> Int    -- ^ Number of spaces (rel to block) to indent first line
         -> String -- ^ Contents of block to indent
         -> String
indentBy _ _ [] = ""
indentBy num first str = 
  let (firstLine:restLines) = lines str 
      firstLineIndent = num + first
  in  (replicate firstLineIndent ' ') ++ firstLine ++ "\n" ++ 
      (intercalate "\n" $ map ((replicate num ' ') ++ ) restLines)

-- | Prettyprint list of Pandoc blocks elements.
prettyBlockList :: Int       -- ^ Number of spaces to indent list of blocks
                -> [Block]   -- ^ List of blocks
                -> String
prettyBlockList indent [] = indentBy indent 0 "[]"
prettyBlockList indent blocks = indentBy indent (-2) $ "[ " ++ 
  (intercalate "\n, " (map prettyBlock blocks)) ++ " ]"

-- | Prettyprint Pandoc block element.
prettyBlock :: Block -> String
prettyBlock (BlockQuote blocks) = "BlockQuote\n  " ++ 
                                  (prettyBlockList 2 blocks) 
prettyBlock (OrderedList attribs blockLists) = 
  "OrderedList " ++ show attribs ++ "\n" ++ indentBy 2 0 ("[ " ++ 
  (intercalate ", " $ map (\blocks -> prettyBlockList 2 blocks)
  blockLists)) ++ " ]"
prettyBlock (BulletList blockLists) = "BulletList\n" ++ 
  indentBy 2 0 ("[ " ++ (intercalate ", "
  (map (\blocks -> prettyBlockList 2 blocks) blockLists))) ++ " ]" 
prettyBlock (DefinitionList items) = "DefinitionList\n" ++ 
  indentBy 2 0 ("[ " ++ (intercalate "\n, "
  (map (\(term, defs) -> "(" ++ show term ++ ",\n" ++ 
  indentBy 3 0 ("[ " ++ (intercalate ", "
  (map (\blocks -> prettyBlockList 2 blocks) defs)) ++ "]") ++
   ")") items))) ++ " ]" 
prettyBlock (Table caption aligns widths header rows) = 
  "Table " ++ show caption ++ " " ++ show aligns ++ " " ++ 
  show widths ++ "\n" ++ prettyRow header ++ " [\n" ++  
  (intercalate ",\n" (map prettyRow rows)) ++ " ]"
  where prettyRow cols = indentBy 2 0 ("[ " ++ (intercalate ", "
                         (map (\blocks -> prettyBlockList 2 blocks) 
                         cols))) ++ " ]"
prettyBlock block = show block

-- | Prettyprint Pandoc document.
writeNative :: WriterOptions -> Pandoc -> String
writeNative opts (Pandoc meta blocks) | writerStandalone opts =
  "Pandoc " ++ "(" ++ show meta ++ ")\n  " ++ prettyBlockList 2 blocks
-- -- writeNative _ (Pandoc _ [Plain [x]]) = show x
-- writeNative _ (Pandoc _ [Plain xs])  = show xs
-- writeNative _ (Pandoc _ [x])         = prettyBlock x
writeNative _ (Pandoc _ xs)          = prettyBlockList 0 xs
