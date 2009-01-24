module WordPressPlugin (transform) where
import Text.Pandoc

-- This plugin (when used with -m) prints LaTeX math in the
-- format required by WordPress blogs.  $e=mc^2$ becomes
-- $LaTeX e=mc^2$.

transform :: Inline -> Inline
transform (Math x y) = Math x $ "LaTeX " ++ y
transform x          = x
