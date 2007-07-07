{-
Copyright (C) 2007 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Blocks
   Copyright   : Copyright (C) 2007 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions for the manipulation of fixed-width blocks of text.
These are used in the construction of plain-text tables.
-}

module Text.Pandoc.Blocks
               ( 
                TextBlock (..),
                docToBlock,
                blockToDoc,
                widthOfBlock,
                heightOfBlock,
                hcatBlocks,
                hsepBlocks,
                centerAlignBlock,
                leftAlignBlock,
                rightAlignBlock
               )
where

import Text.PrettyPrint
import Data.List (transpose, intersperse)

-- | A fixed-width block of text.  Parameters are width of block,
-- height of block, and list of lines.
data TextBlock = TextBlock Int Int [String]
instance Show TextBlock where
  show x = show $ blockToDoc x

-- | Convert a @Doc@ element into a @TextBlock@ with a specified width. 
docToBlock :: Int  -- ^ Width of text block.
           -> Doc  -- ^ @Doc@ to convert.
           -> TextBlock
docToBlock width doc =
  let rendered    = renderStyle (style {lineLength = width, 
                                        ribbonsPerLine = 1}) doc
      lns         = lines rendered
      chop []     = []
      chop (l:ls) = if length l > width
                       then (take width l):(chop ((drop width l):ls))
                       else l:(chop ls)
      lns'        = chop lns
  in  TextBlock width (length lns') lns' 

-- | Convert a @TextBlock@ to a @Doc@ element.
blockToDoc :: TextBlock -> Doc
blockToDoc (TextBlock _ _ lns) = 
  if null lns
     then empty
     else vcat $ map text lns

-- | Returns width of a @TextBlock@ (number of columns).
widthOfBlock :: TextBlock -> Int
widthOfBlock (TextBlock width _ _) = width

-- | Returns height of a @TextBlock@ (number of rows).
heightOfBlock :: TextBlock -> Int
heightOfBlock (TextBlock _ height _) = height

-- | Pads a string out to a given width using spaces.
hPad :: Int     -- ^ Desired width.
     -> String  -- ^ String to pad.
     -> String
hPad width line = 
  let lineLength = length line
  in  if lineLength <= width 
         then line ++ replicate (width - lineLength) ' '
         else take width line

-- | Concatenates a list of @TextBlock@s into a new @TextBlock@ in
-- which they appear side by side.
hcatBlocks :: [TextBlock] -> TextBlock
hcatBlocks [] = TextBlock 0 0 []
hcatBlocks ((TextBlock width1 height1 lns1):xs) = 
  let (TextBlock width2 height2 lns2) = hcatBlocks xs
      height = max height1 height2
      width  = width1 + width2
      lns1'  = map (hPad width1) $ lns1 ++ replicate (height - height1) ""
      lns2'  = lns2 ++ replicate (height - height2) ""
      lns    = zipWith (++) lns1' lns2'
   in TextBlock width height lns 

-- | Like @hcatBlocks@, but inserts space between the @TextBlock@s.
hsepBlocks = hcatBlocks . (intersperse (TextBlock 1 1 [" "]))

isWhitespace x = x `elem` " \t"

-- | Left-aligns the contents of a @TextBlock@ within the block.
leftAlignBlock :: TextBlock -> TextBlock
leftAlignBlock (TextBlock width height lns) =
  TextBlock width height $ 
            map (dropWhile isWhitespace) lns

-- | Right-aligns the contents of a @TextBlock@ within the block.
rightAlignBlock :: TextBlock -> TextBlock
rightAlignBlock (TextBlock width height lns) =
  let rightAlignLine ln = 
        let (spaces, rest) = span isWhitespace $ reverse $ hPad width ln
        in  reverse (rest ++ spaces)
  in  TextBlock width height $ map rightAlignLine lns

-- | Centers the contents of a @TextBlock@ within the block.
centerAlignBlock :: TextBlock -> TextBlock
centerAlignBlock (TextBlock width height lns) = 
  let centerAlignLine ln =
        let ln' = hPad width ln
            (startSpaces, rest) = span isWhitespace ln'
            endSpaces = takeWhile isWhitespace (reverse ln')
            numSpaces = length (startSpaces ++ endSpaces)
            startSpaces' = replicate (quot numSpaces 2) ' '
        in  startSpaces' ++ rest 
  in  TextBlock width height $ map centerAlignLine lns

