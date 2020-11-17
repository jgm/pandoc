{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Readers.LaTeX.SIunitx
  ( dosi
  , doSI
  , doSIrange
  , doSInum
  , doSInumlist
  , doSIang
  )
where
import Text.Pandoc.Builder
import Text.Pandoc.Readers.LaTeX.Parsing
import Text.Pandoc.Readers.LaTeX.Types
import Text.Pandoc.Class
import Text.Pandoc.Parsing hiding (blankline, mathDisplay, mathInline,
                            optional, space, spaces, withRaw, (<|>))
import Control.Applicative ((<|>))
import qualified Data.Map as M
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intersperse)

dosi :: PandocMonad m => LP m Inlines -> LP m Inlines
dosi tok = grouped (siUnit tok) <|> siUnit tok

-- converts e.g. \SI{1}[\$]{} to "$ 1" or \SI{1}{\euro} to "1 €"
doSI :: PandocMonad m => LP m Inlines -> LP m Inlines
doSI tok = do
  skipopts
  value <- doSInum
  valueprefix <- option "" $ bracketed tok
  unit <- dosi tok
  return . mconcat $ [valueprefix,
                      emptyOr160 valueprefix,
                      value,
                      emptyOr160 unit,
                      unit]

doSInum :: PandocMonad m => LP m Inlines
doSInum = skipopts *> (tonum . untokenize <$> braced)

tonum :: Text -> Inlines
tonum value =
  case runParser parseNum () "" value of
    Left _    -> text value
    Right num -> num

doSInumlist :: PandocMonad m => LP m Inlines
doSInumlist = do
  skipopts
  xs <- map tonum . T.splitOn ";" . untokenize <$> braced
  case xs of
    []  -> return mempty
    [x] -> return x
    _   -> return $
             mconcat (intersperse (str "," <> space) (init xs)) <>
             text ", & " <> last xs

parseNum :: Parser Text () Inlines
parseNum = (mconcat <$> many parseNumPart) <* eof

parseNumPart :: Parser Text () Inlines
parseNumPart =
  parseDecimalNum <|>
  parseComma <|>
  parsePlusMinus <|>
  parseI <|>
  parseExp <|>
  parseX <|>
  parseSpace
 where
  parseDecimalNum = do
    pref <- option mempty $ (mempty <$ char '+') <|> ("\x2212" <$ char '-')
    basenum <- (pref <>) . T.pack
                <$> many1 (satisfy (\c -> isDigit c || c == '.'))
    uncertainty <- option mempty $ T.pack <$> parseParens
    if T.null uncertainty
       then return $ str basenum
       else return $ str $ basenum <> "\xa0\xb1\xa0" <>
             let (_,ys) = T.break (=='.') basenum
              in case (T.length ys - 1, T.length uncertainty) of
                   (0,_) -> uncertainty
                   (x,y)
                     | x > y  -> "0." <> T.replicate (x - y) "0" <>
                                      T.dropWhileEnd (=='0') uncertainty
                     | otherwise -> T.take (y - x) uncertainty <>
                                      case T.dropWhileEnd (=='0')
                                             (T.drop (y - x) uncertainty) of
                                             t | T.null t -> mempty
                                               | otherwise -> "." <> t
  parseComma = str "." <$ char ','
  parsePlusMinus = str "\xa0\xb1\xa0" <$ try (string "+-")
  parseParens =
    char '(' *> many1 (satisfy (\c -> isDigit c || c == '.')) <* char ')'
  parseI = str "i" <$ char 'i'
  parseX = str "\xa0\xd7\xa0" <$ char 'x'
  parseExp = (\n -> str ("\xa0\xd7\xa0" <> "10") <> superscript n)
               <$> (char 'e' *> parseDecimalNum)
  parseSpace = mempty <$ skipMany1 (char ' ')

doSIang :: PandocMonad m => LP m Inlines
doSIang = do
  skipopts
  ps <- T.splitOn ";" . untokenize <$> braced
  case ps ++ repeat "" of
    (d:m:s:_) -> return $
      (if T.null d then mempty else str d <> str "\xb0") <>
      (if T.null m then mempty else str m <> str "\x2032") <>
      (if T.null s then mempty else str s <> str "\x2033")
    _ -> return mempty

-- converts e.g. \SIrange{100}{200}{\ms} to "100 ms--200 ms"
doSIrange :: PandocMonad m => Bool -> LP m Inlines -> LP m Inlines
doSIrange includeUnits tok = do
  skipopts
  startvalue <- doSInum
  startvalueprefix <- option "" $ bracketed tok
  stopvalue <- doSInum
  stopvalueprefix <- option "" $ bracketed tok
  unit <- if includeUnits
             then dosi tok
             else return mempty
  return . mconcat $ [startvalueprefix,
                      emptyOr160 startvalueprefix,
                      startvalue,
                      emptyOr160 unit,
                      unit,
                      "\8211", -- An en-dash
                      stopvalueprefix,
                      emptyOr160 stopvalueprefix,
                      stopvalue,
                      emptyOr160 unit,
                      unit]

emptyOr160 :: Inlines -> Inlines
emptyOr160 x = if x == mempty then x else str "\160"

siUnit :: PandocMonad m => LP m Inlines -> LP m Inlines
siUnit tok = try (do
  Tok _ (CtrlSeq name) _ <- anyControlSeq
  case name of
    "square" -> do
       unit <- siUnit tok
       return $ unit <> superscript "2"
    "cubic" -> do
       unit <- siUnit tok
       return $ unit <> superscript "3"
    "raisetothe" -> do
       n <- tok
       unit <- siUnit tok
       return $ unit <> superscript n
    _ ->
       case M.lookup name siUnitMap of
            Just il ->
              option il $
                choice
                 [ (il <> superscript "2") <$ controlSeq "squared"
                 , (il <> superscript "3") <$ controlSeq "cubed"
                 , (\n -> il <> superscript n) <$> (controlSeq "tothe" *> tok)
                 ]
            Nothing -> fail "not an siunit unit command")
 <|> (lookAhead anyControlSeq >> tok)
 <|> (do Tok _ Word t <- satisfyTok isWordTok
         return $ str t)
 <|> (symbol '^' *> (superscript <$> tok))
 <|> (symbol '_' *> (subscript <$> tok))
 <|> ("\xa0" <$ symbol '.')
 <|> ("\xa0" <$ symbol '~')
 <|> tok
 <|> (do Tok _ _ t <- anyTok
         return (str t))

siUnitMap :: M.Map Text Inlines
siUnitMap = M.fromList
  [ ("fg", str "fg")
  , ("pg", str "pg")
  , ("ng", str "ng")
  , ("ug", str "μg")
  , ("mg", str "mg")
  , ("g", str "g")
  , ("kg", str "kg")
  , ("amu", str "u")
  , ("pm", str "pm")
  , ("nm", str "nm")
  , ("um", str "μm")
  , ("mm", str "mm")
  , ("cm", str "cm")
  , ("dm", str "dm")
  , ("m", str "m")
  , ("km", str "km")
  , ("as", str "as")
  , ("fs", str "fs")
  , ("ps", str "ps")
  , ("ns", str "ns")
  , ("us", str "μs")
  , ("ms", str "ms")
  , ("s", str "s")
  , ("fmol", str "fmol")
  , ("pmol", str "pmol")
  , ("nmol", str "nmol")
  , ("umol", str "μmol")
  , ("mmol", str "mmol")
  , ("mol", str "mol")
  , ("kmol", str "kmol")
  , ("pA", str "pA")
  , ("nA", str "nA")
  , ("uA", str "μA")
  , ("mA", str "mA")
  , ("A", str "A")
  , ("kA", str "kA")
  , ("ul", str "μl")
  , ("ml", str "ml")
  , ("l", str "l")
  , ("hl", str "hl")
  , ("uL", str "μL")
  , ("mL", str "mL")
  , ("L", str "L")
  , ("hL", str "hL")
  , ("mHz", str "mHz")
  , ("Hz", str "Hz")
  , ("kHz", str "kHz")
  , ("MHz", str "MHz")
  , ("GHz", str "GHz")
  , ("THz", str "THz")
  , ("mN", str "mN")
  , ("N", str "N")
  , ("kN", str "kN")
  , ("MN", str "MN")
  , ("Pa", str "Pa")
  , ("kPa", str "kPa")
  , ("MPa", str "MPa")
  , ("GPa", str "GPa")
  , ("mohm", str "mΩ")
  , ("kohm", str "kΩ")
  , ("Mohm", str "MΩ")
  , ("pV", str "pV")
  , ("nV", str "nV")
  , ("uV", str "μV")
  , ("mV", str "mV")
  , ("V", str "V")
  , ("kV", str "kV")
  , ("W", str "W")
  , ("uW", str "μW")
  , ("mW", str "mW")
  , ("kW", str "kW")
  , ("MW", str "MW")
  , ("GW", str "GW")
  , ("J", str "J")
  , ("uJ", str "μJ")
  , ("mJ", str "mJ")
  , ("kJ", str "kJ")
  , ("eV", str "eV")
  , ("meV", str "meV")
  , ("keV", str "keV")
  , ("MeV", str "MeV")
  , ("GeV", str "GeV")
  , ("TeV", str "TeV")
  , ("kWh", str "kWh")
  , ("F", str "F")
  , ("fF", str "fF")
  , ("pF", str "pF")
  , ("K", str "K")
  , ("dB", str "dB")
  , ("ampere", str "A")
  , ("angstrom", str "Å")
  , ("arcmin", str "′")
  , ("arcminute", str "′")
  , ("arcsecond", str "″")
  , ("astronomicalunit", str "ua")
  , ("atomicmassunit", str "u")
  , ("atto", str "a")
  , ("bar", str "bar")
  , ("barn", str "b")
  , ("becquerel", str "Bq")
  , ("bel", str "B")
  , ("bohr", emph (str "a") <> subscript (str "0"))
  , ("candela", str "cd")
  , ("celsius", str "°C")
  , ("centi", str "c")
  , ("clight", emph (str "c") <> subscript (str "0"))
  , ("coulomb", str "C")
  , ("dalton", str "Da")
  , ("day", str "d")
  , ("deca", str "d")
  , ("deci", str "d")
  , ("decibel", str "db")
  , ("degreeCelsius",str "°C")
  , ("degree", str "°")
  , ("deka", str "d")
  , ("electronmass", emph (str "m") <> subscript (str "e"))
  , ("electronvolt", str "eV")
  , ("elementarycharge", emph (str "e"))
  , ("exa", str "E")
  , ("farad", str "F")
  , ("femto", str "f")
  , ("giga", str "G")
  , ("gram", str "g")
  , ("gray", str "Gy")
  , ("hartree", emph (str "E") <> subscript (str "h"))
  , ("hectare", str "ha")
  , ("hecto", str "h")
  , ("henry", str "H")
  , ("hertz", str "Hz")
  , ("hour", str "h")
  , ("joule", str "J")
  , ("katal", str "kat")
  , ("kelvin", str "K")
  , ("kilo", str "k")
  , ("kilogram", str "kg")
  , ("knot", str "kn")
  , ("liter", str "L")
  , ("litre", str "l")
  , ("lumen", str "lm")
  , ("lux", str "lx")
  , ("mega", str "M")
  , ("meter", str "m")
  , ("metre", str "m")
  , ("micro", str "μ")
  , ("milli", str "m")
  , ("minute", str "min")
  , ("mmHg", str "mmHg")
  , ("mole", str "mol")
  , ("nano", str "n")
  , ("nauticalmile", str "M")
  , ("neper", str "Np")
  , ("newton", str "N")
  , ("ohm", str "Ω")
  , ("Pa", str "Pa")
  , ("pascal", str "Pa")
  , ("percent", str "%")
  , ("per", str "/")
  , ("peta", str "P")
  , ("pico", str "p")
  , ("planckbar", emph (str "\x210f"))
  , ("radian", str "rad")
  , ("second", str "s")
  , ("siemens", str "S")
  , ("sievert", str "Sv")
  , ("steradian", str "sr")
  , ("tera", str "T")
  , ("tesla", str "T")
  , ("tonne", str "t")
  , ("volt", str "V")
  , ("watt", str "W")
  , ("weber", str "Wb")
  , ("yocto", str "y")
  , ("yotta", str "Y")
  , ("zepto", str "z")
  , ("zetta", str "Z")
  ]


