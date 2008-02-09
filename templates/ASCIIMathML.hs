-- | Definitions for use of ASCIIMathML in HTML.  
-- (See <http://www1.chapman.edu/~jipsen/mathml/asciimath.html>.)
module Text.Pandoc.ASCIIMathML ( asciiMathMLScript ) where

-- | String containing ASCIIMathML javascript.
asciiMathMLScript :: String
asciiMathMLScript = "<script type=\"text/javascript\">\n" ++ @ASCIIMathML.js.comment@ ++ @ASCIIMathML.js.packed@ ++ "</script>\n"
