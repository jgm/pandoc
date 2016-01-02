import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter removeLinks

removeLinks :: Inline -> [Inline]
removeLinks (Link _ l _) = l
removeLinks x = [x]

