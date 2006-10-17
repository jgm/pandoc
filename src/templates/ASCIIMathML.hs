-- | Definitions for use of Pandoc.ASCIIMathML in HTML.  
-- (See <http://www1.chapman.edu/~jipsen/mathml/asciimath.html>.)
module Text.Pandoc.ASCIIMathML ( asciiMathMLScript ) where

-- | String containing Pandoc.ASCIIMathML javascript.
asciiMathMLScript :: String
asciiMathMLScript = "<script type=\"text/javascript\">\n<ASCIIMathML.js></script>\n"
