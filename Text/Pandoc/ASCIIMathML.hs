{-# LANGUAGE CPP, TemplateHaskell #-}
-- | Definitions for use of ASCIIMathML in HTML.  
-- (See <http://www1.chapman.edu/~jipsen/mathml/asciimath.html>.)
module Text.Pandoc.ASCIIMathML ( asciiMathMLScript ) where
import Text.Pandoc.Shared ( contentsOf )
import System.FilePath ( (</>) )

-- | String containing ASCIIMathML javascript.
asciiMathMLScript :: String
#ifndef __HADDOCK__
asciiMathMLScript = "<script type=\"text/javascript\">\n" ++
                    $(contentsOf $ "data" </> "ASCIIMathML.js.comment") ++
                    $(contentsOf $ "data" </> "ASCIIMathML.js.packed") ++ "</script>\n"
#endif
