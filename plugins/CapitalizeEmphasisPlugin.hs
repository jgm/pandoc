module CapitalizeEmphasisPlugin (transform) where
import Text.Pandoc
import Data.Char (toUpper)

-- This plugin changes emphasized text into CAPITALIZED TEXT.

transform :: [Inline] -> [Inline]
transform (Emph x : ys) = processWith capStr x ++ transform ys
transform (x : ys)      = x : transform ys
transform []            = []

capStr :: Inline -> Inline
capStr (Str x) = Str (map toUpper x)
capStr x       = x
