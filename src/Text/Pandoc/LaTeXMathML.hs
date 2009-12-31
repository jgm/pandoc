-- | Definitions for use of LaTeXMathML in HTML.  
-- (See <http://math.etsu.edu/LaTeXMathML/>)
module Text.Pandoc.LaTeXMathML ( latexMathMLScript ) where
import System.FilePath ( (</>) )
import Text.Pandoc.Shared (readDataFile)

-- | String containing LaTeXMathML javascript.
latexMathMLScript :: IO String
latexMathMLScript = do
 jsCom <- readDataFile $ "data" </> "LaTeXMathML.js.comment"
 jsPacked <- readDataFile $ "data" </> "LaTeXMathML.js.packed"
 return $ "<script type=\"text/javascript\">\n" ++ jsCom ++ jsPacked ++
          "</script>\n"
