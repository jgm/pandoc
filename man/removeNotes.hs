import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter removeNotes

removeNotes :: Inline -> Inline
removeNotes (Note _) = Str ""
removeNotes x = x

