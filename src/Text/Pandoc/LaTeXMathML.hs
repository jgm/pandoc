-- | Definitions for use of LaTeXMathML in HTML.  
-- (See <http://math.etsu.edu/LaTeXMathML/>)
module Text.Pandoc.LaTeXMathML ( latexMathMLScript ) where
import System.FilePath ( (</>) )
import Text.Pandoc.Shared (readDataFile)

-- | String containing LaTeXMathML javascript.
latexMathMLScript :: Maybe FilePath -> IO String
latexMathMLScript datadir = do
 jsCom <- readDataFile datadir $ "data" </> "LaTeXMathML.js.comment"
 jsPacked <- readDataFile datadir $ "data" </> "LaTeXMathML.js.packed"
 return $ "<script type=\"text/javascript\">\n" ++ jsCom ++ jsPacked ++
          "</script>\n"
