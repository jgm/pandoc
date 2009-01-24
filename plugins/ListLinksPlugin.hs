module ListLinksPlugin (transform) where
import Text.Pandoc

-- This plugin returns an empty document and prints a list
-- of the URLs linked to in the source document.

transform :: Pandoc -> IO Pandoc
transform p = do
  let urls = queryIn findURLs p
  putStrLn $ unlines urls
  return $ Pandoc (Meta [] [] []) []

findURLs :: Inline -> [String]
findURLs (Link label (url, title)) = [url]
findURLs x                         = []
