module Text.Pandoc.Writers.Math
  ( texMathToInlines
  , convertMath
  )
where

import Text.Pandoc.Class
import Text.Pandoc.Definition
import Text.TeXMath (Exp, writePandoc, DisplayType(..), readTeX)

-- | Converts a raw TeX math formula to a list of 'Pandoc' inlines.
-- Defaults to raw formula between @$@ or @$$@ characters if entire formula
-- can't be converted.
texMathToInlines :: PandocMonad m
                 => MathType
                 -> String    -- ^ String to parse (assumes @'\n'@ line endings)
                 -> m [Inline]
texMathToInlines mt inp = do
  res <- convertMath writePandoc mt inp
  case res of
       Right (Just ils)  -> return ils
       Right (Nothing)   -> do
         warning $ "Could not render TeX math as unicode, rendering as raw TeX:\n" ++ inp
         return [mkFallback mt inp]
       Left il           -> return [il]

mkFallback :: MathType -> String -> Inline
mkFallback mt str = Str (delim ++ str ++ delim)
   where delim = case mt of
                      DisplayMath -> "$$"
                      InlineMath  -> "$"

-- | Converts a raw TeX math formula using a writer function,
-- issuing a warning and producing a fallback (a raw string)
-- on failure.
convertMath :: PandocMonad m
            => (DisplayType -> [Exp] -> a) -> MathType -> String
            -> m (Either Inline a)
convertMath writer mt str = do
  case writer dt <$> readTeX str of
       Right r  -> return (Right r)
       Left e   -> do
         warning $ "Could not convert TeX math, rendering as raw TeX:\n" ++
                 str ++ "\n" ++ e
         return (Left $ mkFallback mt str)
   where dt = case mt of
                   DisplayMath -> DisplayBlock
                   InlineMath  -> DisplayInline

