module IncludeFilePlugin (transform) where
import Text.Pandoc
import Text.Pandoc.Shared
import Control.Monad

-- This plugin allows you to include the contents of an
-- external file in a delimited code block like this:
--
-- ~~~ {include="filename"}
-- ~~~
--
-- Trailing newlines are trimmed.

transform :: Block -> IO Block
transform cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
       Just f     -> return . (CodeBlock (id, classes, namevals) . stripTrailingNewlines) =<< readFile f
       Nothing    -> return cb
transform x = return x
