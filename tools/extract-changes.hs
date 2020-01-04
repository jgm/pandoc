#!/usr/bin/env stack
-- stack --stack-yaml=stack.yaml runghc --package pandoc-types

-- Extract changes from latest version in changelog.
import Text.Pandoc.JSON

main = toJSONFilter extractFirst

extractFirst :: Pandoc -> Pandoc
extractFirst (Pandoc meta bs) =
  let bs' = dropWhile (not . isSubhead) bs
   in Pandoc meta (takeWhile (not . isSubhead) (drop 1 bs'))

isSubhead (Header 2 _ _) = True
isSubhead _ = False
