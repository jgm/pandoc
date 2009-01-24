module DotPlugin (transform) where
import Text.Pandoc
import Text.Pandoc.Shared
import System.Process (readProcess)
import Data.Char (ord)
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA

-- This plugin allows you to include a graphviz "dot" diagram
-- in a document like this:
--
-- ~~~ {.dot name="diagram1"}
-- digraph G {Hello->World}
-- ~~~

transform :: Block -> IO Block
transform (CodeBlock (id, classes, namevals) contents) | "dot" `elem` classes = do
  let (name, outfile) =  case lookup "name" namevals of
                                Just fn   -> ([Str fn], fn ++ ".png")
                                Nothing   -> ([], uniqueName contents ++ ".png")
  result <- readProcess "dot" ["-Tpng"] contents
  writeFile outfile result
  return $ Para [Image name (outfile, "")]
transform x = return x

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString
