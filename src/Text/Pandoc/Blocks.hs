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

data TextBlock = TextBlock Int Int [String] -- width height lines
instance Show TextBlock where
  show x = show $ blockToDoc x

docToBlock :: Int -> Doc -> TextBlock
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

blockToDoc :: TextBlock -> Doc
blockToDoc (TextBlock _ _ lns) = 
  if null lns
     then empty
     else vcat $ map text lns

widthOfBlock :: TextBlock -> Int
widthOfBlock (TextBlock width _ _) = width

heightOfBlock :: TextBlock -> Int
heightOfBlock (TextBlock _ height _) = height

-- pad line out to width using spaces
hPad :: Int -> String -> String
hPad width line = 
  let lineLength = length line
  in  if lineLength <= width 
         then line ++ replicate (width - lineLength) ' '
         else take width line

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

hsepBlocks = hcatBlocks . (intersperse (TextBlock 1 1 [" "]))

isWhitespace x = x `elem` " \t"

leftAlignBlock :: TextBlock -> TextBlock
leftAlignBlock (TextBlock width height lns) =
  TextBlock width height $ 
            map (dropWhile isWhitespace) lns

rightAlignBlock :: TextBlock -> TextBlock
rightAlignBlock (TextBlock width height lns) =
  let rightAlignLine ln = 
        let (spaces, rest) = span isWhitespace $ reverse $ hPad width ln
        in  reverse (rest ++ spaces)
  in  TextBlock width height $ map rightAlignLine lns

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

