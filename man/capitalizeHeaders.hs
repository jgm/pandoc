import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Data.Char (toUpper)

main :: IO ()
main = toJSONFilter capitalizeHeaders

capitalizeHeaders :: Block -> Block
capitalizeHeaders (Header 1 attr xs) = Header 1 attr $ walk capitalize xs
capitalizeHeaders x = x

capitalize :: Inline -> Inline
capitalize (Str xs) = Str $ map toUpper xs
capitalize x = x

{-
capitalizeHeaderLinks :: Inline -> Inline
capitalizeHeaderLinks (Link xs t@('#':_,_)) = Link (walk capitalize xs) t
capitalizeHeaderLinks x = x
-}
