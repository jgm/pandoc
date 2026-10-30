{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Xlsx.NumberFormat
   Copyright   : © 2026 Nacho García
   License     : GNU GPL, version 2 or above

   Maintainer  : Nacho García <hello@nachogarcia.dev>
   Stability   : alpha
   Portability : portable

Number formats of XLSX cells, other than dates and times: decimals,
thousands separators, percentages and literal text such as currency
symbols.
-}
module Text.Pandoc.Readers.Xlsx.NumberFormat
  ( NumberFormat
  , builtinNumberFormat
  , parseNumberFormat
  , applyNumberFormat
  ) where

import Data.Char (isAlpha, toLower)
import Data.List (intercalate, isInfixOf)
import qualified Data.Text as T
import Data.Text (Text)
import Numeric (floatToDigits)

-- | A number format: its sections for positive numbers, negative
-- numbers and zero
data NumberFormat = NumberFormat
  { nfPositive :: Section
  , nfNegative :: Maybe Section
  , nfZero     :: Maybe Section
  } deriving (Show)

-- | One section of a number format
data Section = Section
  { secPrefix      :: Text
  , secSuffix      :: Text
  , secDigits      :: Bool  -- ^ Whether it shows the number at all
  , secIntZeros    :: Int   -- ^ Minimum number of integer digits
  , secGrouping    :: Bool  -- ^ Thousands separators
  , secMinDecimals :: Int
  , secMaxDecimals :: Int
  , secScale       :: Int   -- ^ Power of ten the number is scaled by
  } deriving (Show)

data Token
  = TLit Text
  | TDigit Char      -- ^ @0@, @#@ or @?@
  | TPoint
  | TComma
  | TPercent
  deriving (Show, Eq)

-- | The built-in number formats other than General, dates and times
-- that can be applied
builtinNumberFormat :: Int -> Maybe NumberFormat
builtinNumberFormat fmtId = lookup fmtId codes >>= parseNumberFormat
  where
    codes = [ (1, "0"), (2, "0.00"), (3, "#,##0"), (4, "#,##0.00")
            , (9, "0%"), (10, "0.00%")
            , (37, "#,##0 ;(#,##0)"), (38, "#,##0 ;(#,##0)")
            , (39, "#,##0.00;(#,##0.00)"), (40, "#,##0.00;(#,##0.00)")
            ]

-- | Parse a custom format code. Nothing for General, and for what
-- isn't supported, so that the number is shown as under General:
-- conditions, scientific notation, fractions, text sections, and text
-- between the digits.
parseNumberFormat :: Text -> Maybe NumberFormat
parseNumberFormat code =
  case splitSections (T.unpack code) of
    [] -> Nothing
    (p:rest) -> do
      positive <- parseSection p
      negative <- traverse parseSection (take 1 rest)
      zero <- traverse parseSection (take 1 (drop 1 rest))
      return $ NumberFormat positive (headMay negative) (headMay zero)
  where
    headMay xs = case xs of
      x:_ -> Just x
      [] -> Nothing

-- | Split a format code at the semicolons outside quotes and escapes
splitSections :: String -> [String]
splitSections = go ""
  where
    go acc [] = [reverse acc]
    go acc ('"':cs) = let (q, rest) = break (== '"') cs
                       in go (reverse ('"' : q ++ "\"") ++ acc) (drop 1 rest)
    go acc ('\\':c:cs) = go (c : '\\' : acc) cs
    go acc (';':cs) = reverse acc : go "" cs
    go acc (c:cs) = go (c : acc) cs

parseSection :: String -> Maybe Section
parseSection s = do
  tokens <- tokenize s
  let isDigitTok t = case t of
        TDigit _ -> True
        _ -> False
      isText t = case t of
        TLit _ -> True
        TPercent -> True
        _ -> False
      (before, rest) = break isDigitTok tokens
      -- The number runs from the first digit placeholder to the last
      lastDigit = length (takeWhile (not . isDigitTok) (reverse rest))
      (numberToks, afterAll) = splitAt (length rest - lastDigit) rest
      -- Commas right after the last digit scale the number by 1000 each
      (scaleCommas, after) = span (== TComma) afterAll
      (intPart, fracPart) = break (== TPoint) numberToks
      decimals = [c | TDigit c <- drop 1 fracPart]
      percents = length (filter (== TPercent) tokens)
  -- Text between the digits isn't supported
  if any isText numberToks || length (filter (== TPoint) numberToks) > 1
     then Nothing
     else Just Section
       { secPrefix = literal before
       , secSuffix = literal after
       , secDigits = any isDigitTok tokens
       , secIntZeros = length [() | TDigit '0' <- intPart]
       , secGrouping = TComma `elem` intPart
       , secMinDecimals = length (takeWhile (== '0') decimals)
       , secMaxDecimals = length decimals
       , secScale = 2 * percents - 3 * length scaleCommas
       }
  where
    literal ts = T.concat [ case t of
                              TLit l -> l
                              TPercent -> "%"
                              _ -> ""
                          | t <- ts ]

-- | The tokens of a section; Nothing if it has anything unsupported
tokenize :: String -> Maybe [Token]
tokenize s
  | "general" `isInfixOf` map toLower s = Nothing
  | otherwise = go s
  where
    go [] = Just []
    go ('"':cs) = let (q, rest) = break (== '"') cs
                   in (TLit (T.pack q) :) <$> go (drop 1 rest)
    go ('\\':c:cs) = (TLit (T.singleton c) :) <$> go cs
    go ('_':_:cs) = go cs   -- space as wide as a character
    go ('*':_:cs) = go cs   -- fill with a character
    go ('[':cs) =
      let (inside, rest) = break (== ']') cs
       in case inside of
            '$':cur -> (TLit (T.pack (takeWhile (/= '-') cur)) :) <$>
                         go (drop 1 rest)
            c:_ | c `elem` ("<>=" :: String) -> Nothing  -- condition
            _ -> go (drop 1 rest)                         -- colour
    go (c:cs)
      | c `elem` ("0#?" :: String) = (TDigit c :) <$> go cs
      | c == '.' = (TPoint :) <$> go cs
      | c == ',' = (TComma :) <$> go cs
      | c == '%' = (TPercent :) <$> go cs
      | c `elem` ("eE/@" :: String) = Nothing
      | isAlpha c = Nothing
      | otherwise = (TLit (T.singleton c) :) <$> go cs

-- | Render a number with a number format. Nothing if it can't be shown
-- with it (a number too large to round), so that General is used.
applyNumberFormat :: NumberFormat -> Double -> Maybe Text
applyNumberFormat nf x
  | isNaN x || isInfinite x = Nothing
  | x < 0, Just sec <- nfNegative nf = render sec False (negate x)
  | x == 0, Just sec <- nfZero nf = render sec False 0
  | otherwise = render (nfPositive nf) (x < 0) (abs x)

render :: Section -> Bool -> Double -> Maybe Text
render sec minus x
  | not (secDigits sec) = Just $ secPrefix sec <> secSuffix sec
  | otherwise = do
      digits <- roundTo (secMaxDecimals sec) (secScale sec) x
      let decimals = secMaxDecimals sec
          (intDigits, fracDigits) =
            splitAt (length digits - decimals) digits
          intShown = case dropWhile (== '0') intDigits of
            "" -> replicate (secIntZeros sec) '0'
            ds -> replicate (secIntZeros sec - length ds) '0' ++ ds
          fracShown = dropTrailingZeros (secMinDecimals sec) fracDigits
          number = (if secGrouping sec then group3 intShown else intShown) ++
                   (if null fracShown then "" else '.' : fracShown)
          isZero = all (== '0') digits
      return $ T.pack (if minus && not isZero then "-" else "") <>
               secPrefix sec <> T.pack number <> secSuffix sec
  where
    dropTrailingZeros keep ds =
      let (kept, rest) = splitAt keep ds
       in kept ++ reverse (dropWhile (== '0') (reverse rest))
    group3 = reverse . intercalate "," . chunks3 . reverse
    chunks3 [] = []
    chunks3 cs = take 3 cs : chunks3 (drop 3 cs)

-- | The digits of a non-negative number scaled by @10^scale@ and rounded
-- half away from zero to @decimals@ places, as Excel does: starting from
-- its decimal form with at most 15 significant digits. The last
-- @decimals@ digits are the decimals. Nothing for a number too large to
-- write out.
roundTo :: Int -> Int -> Double -> Maybe String
roundTo decimals scale x
  | x == 0 = Just (replicate (decimals + 1) '0')
  | e + scale + decimals > 40 = Nothing
  | otherwise = Just (replicate (decimals + 1 - length s) '0' ++ s)
  where
    -- x == mantissa * 10^expo, with mantissa of at most 15 digits
    (ds, e) = floatToDigits 10 x
    extra = max 0 (length ds - 15)
    mantissa = roundDiv (fromDigits ds) (10 ^ extra)
    expo = e - length ds + extra
    shift = expo + scale + decimals
    n | shift >= 0 = mantissa * 10 ^ shift
      | otherwise = roundDiv mantissa (10 ^ negate shift)
    s = show n
    fromDigits = foldl (\acc d -> acc * 10 + toInteger d) 0
    roundDiv :: Integer -> Integer -> Integer
    roundDiv a b = let (q, r) = a `quotRem` b
                    in q + (if 2 * r >= b then 1 else 0)
