{-# LANGUAGE NoImplicitPrelude #-}
module Text.Pandoc.Writers.Math
  ( texMathToInlines
  , convertMath
  , defaultMathJaxURL
  , defaultKaTeXURL
  )
where

import Prelude
import qualified Data.Text as T
import Text.Pandoc.Class
import Text.Pandoc.Legacy.Definition -- TODO text: remove Legacy
import Text.Pandoc.Logging
-- import Text.TeXMath (DisplayType (..), Exp, readTeX, writePandoc) TODO text: restore
import Text.Pandoc.Options (defaultMathJaxURL, defaultKaTeXURL)

-- TODO text: remove
import qualified Text.TeXMath as TM
import Text.TeXMath (Exp, DisplayType(..), writePandoc)

readTeX :: String -> Either String [Exp]
readTeX = either (Left . T.unpack) Right . TM.readTeX . T.pack
--

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
       Right Nothing   -> do
         report $ CouldNotConvertTeXMath inp ""
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
convertMath writer mt str =
  case writer dt <$> readTeX str of
       Right r  -> return (Right r)
       Left e   -> do
         report $ CouldNotConvertTeXMath str e
         return (Left $ mkFallback mt str)
   where dt = case mt of
                   DisplayMath -> DisplayBlock
                   InlineMath  -> DisplayInline

