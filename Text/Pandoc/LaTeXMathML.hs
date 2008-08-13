{-# LANGUAGE CPP, TemplateHaskell #-}
-- | Definitions for use of LaTeXMathML in HTML.  
-- (See http://math.etsu.edu/LaTeXMathML/)
module Text.Pandoc.LaTeXMathML ( latexMathMLScript ) where
import Text.Pandoc.TH ( contentsOf )
import System.FilePath ( (</>) )

-- | String containing LaTeXMathML javascript.
latexMathMLScript :: String
#ifndef __HADDOCK__
latexMathMLScript = "<script type=\"text/javascript\">\n" ++
                    $(contentsOf $ "data" </> "LaTeXMathML.js.comment") ++
                    $(contentsOf $ "data" </> "LaTeXMathML.js.packed") ++ "</script>\n"
#endif
