{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
{- |
   Module      : Text.Pandoc.Readers.LaTeX
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of LaTeX to 'Pandoc' document.

-}
module Text.Pandoc.Readers.LaTeX ( readLaTeX,
                                   applyMacros,
                                   rawLaTeXInline,
                                   rawLaTeXBlock,
                                   inlineCommand,
                                   tokenize,
                                   untokenize
                                 ) where

import Control.Applicative (many, optional, (<|>))
import Control.Monad
import Control.Monad.Except (throwError)
import Data.Char (isDigit, isLetter, toUpper, chr)
import Data.Default
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (addExtension, replaceExtension, takeExtension)
import Text.Pandoc.BCP47 (Lang (..), renderLang)
import Text.Pandoc.Builder
import Text.Pandoc.Class.PandocPure (PandocPure)
import Text.Pandoc.Class.PandocMonad (PandocMonad (..), getResourcePath,
                                      readFileFromDirs, report, setResourcePath,
                                      setTranslations, translateTerm)
import Text.Pandoc.Error (PandocError (PandocParseError, PandocParsecError))
import Text.Pandoc.Highlighting (fromListingsLanguage, languagesByExtension)
import Text.Pandoc.ImageSize (numUnit, showFl)
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (blankline, many, mathDisplay, mathInline,
                            optional, space, spaces, withRaw, (<|>))
import Text.Pandoc.Readers.LaTeX.Types (ExpansionPoint (..), Macro (..),
                                        ArgSpec (..), Tok (..), TokType (..))
import Text.Pandoc.Readers.LaTeX.Parsing
import Text.Pandoc.Readers.LaTeX.Lang (polyglossiaLangToBCP47,
                                       babelLangToBCP47)
import Text.Pandoc.Shared
import qualified Text.Pandoc.Translations as Translations
import Text.Pandoc.Walk
import qualified Text.Pandoc.Builder as B
import qualified Data.Text.Normalize as Normalize
import Safe

-- for debugging:
-- import Text.Pandoc.Extensions (getDefaultExtensions)
-- import Text.Pandoc.Class.PandocIO (runIOorExplode, PandocIO)
-- import Debug.Trace (traceShowId)

-- | Parse LaTeX from string and return 'Pandoc' document.
readLaTeX :: PandocMonad m
          => ReaderOptions -- ^ Reader options
          -> Text        -- ^ String to parse (assumes @'\n'@ line endings)
          -> m Pandoc
readLaTeX opts ltx = do
  parsed <- runParserT parseLaTeX def{ sOptions = opts } "source"
               (tokenize "source" (crFilter ltx))
  case parsed of
    Right result -> return result
    Left e       -> throwError $ PandocParsecError ltx e

parseLaTeX :: PandocMonad m => LP m Pandoc
parseLaTeX = do
  bs <- blocks
  eof
  st <- getState
  let meta = sMeta st
  let doc' = doc bs
  let headerLevel (Header n _ _) = [n]
      headerLevel _              = []
#if MIN_VERSION_safe(0,3,18)
  let bottomLevel = minimumBound 1 $ query headerLevel doc'
#else
  let bottomLevel = minimumDef 1 $ query headerLevel doc'
#endif
  let adjustHeaders m (Header n attr ils) = Header (n+m) attr ils
      adjustHeaders _ x                   = x
  let (Pandoc _ bs') =
       -- handle the case where you have \part or \chapter
       (if bottomLevel < 1
           then walk (adjustHeaders (1 - bottomLevel))
           else id) $
       walk (resolveRefs (sLabels st)) doc'
  return $ Pandoc meta bs'

resolveRefs :: M.Map Text [Inline] -> Inline -> Inline
resolveRefs labels x@(Link (ident,classes,kvs) _ _) =
  case (lookup "reference-type" kvs,
        lookup "reference" kvs) of
        (Just "ref", Just lab) ->
          case M.lookup lab labels of
               Just txt -> Link (ident,classes,kvs) txt ("#" <> lab, "")
               Nothing  -> x
        _ -> x
resolveRefs _ x = x


-- testParser :: LP PandocIO a -> Text -> IO a
-- testParser p t = do
--   res <- runIOorExplode (runParserT p defaultLaTeXState{
--             sOptions = def{ readerExtensions =
--               enableExtension Ext_raw_tex $
--                 getDefaultExtensions "latex" }} "source" (tokenize "source" t))
--   case res of
--        Left e  -> error (show e)
--        Right r -> return r


rawLaTeXBlock :: (PandocMonad m, HasMacros s, HasReaderOptions s)
              => ParserT Text s m Text
rawLaTeXBlock = do
  lookAhead (try (char '\\' >> letter))
  inp <- getInput
  let toks = tokenize "source" inp
  snd <$> (rawLaTeXParser toks False (macroDef (const mempty)) blocks
      <|> (rawLaTeXParser toks True
             (do choice (map controlSeq
                   ["include", "input", "subfile", "usepackage"])
                 skipMany opt
                 braced
                 return mempty) blocks)
      <|> rawLaTeXParser toks True
           (environment <|> blockCommand)
           (mconcat <$> (many (block <|> beginOrEndCommand))))

-- See #4667 for motivation; sometimes people write macros
-- that just evaluate to a begin or end command, which blockCommand
-- won't accept.
beginOrEndCommand :: PandocMonad m => LP m Blocks
beginOrEndCommand = try $ do
  Tok _ (CtrlSeq name) txt <- anyControlSeq
  guard $ name == "begin" || name == "end"
  (envname, rawargs) <- withRaw braced
  if M.member (untokenize envname)
      (inlineEnvironments :: M.Map Text (LP PandocPure Inlines))
     then mzero
     else return $ rawBlock "latex"
                    (txt <> untokenize rawargs)

rawLaTeXInline :: (PandocMonad m, HasMacros s, HasReaderOptions s)
               => ParserT Text s m Text
rawLaTeXInline = do
  lookAhead (try (char '\\' >> letter))
  inp <- getInput
  let toks = tokenize "source" inp
  raw <- snd <$>
          (   rawLaTeXParser toks True
              (mempty <$ (controlSeq "input" >> skipMany opt >> braced))
              inlines
          <|> rawLaTeXParser toks True (inlineEnvironment <|> inlineCommand')
              inlines
          )
  finalbraces <- mconcat <$> many (try (string "{}")) -- see #5439
  return $ raw <> T.pack finalbraces

inlineCommand :: PandocMonad m => ParserT Text ParserState m Inlines
inlineCommand = do
  lookAhead (try (char '\\' >> letter))
  inp <- getInput
  let toks = tokenize "source" inp
  fst <$> rawLaTeXParser toks True (inlineEnvironment <|> inlineCommand')
          inlines

-- inline elements:

word :: PandocMonad m => LP m Inlines
word = (str . untoken) <$> satisfyTok isWordTok

regularSymbol :: PandocMonad m => LP m Inlines
regularSymbol = (str . untoken) <$> satisfyTok isRegularSymbol
  where isRegularSymbol (Tok _ Symbol t) = not $ T.any isSpecial t
        isRegularSymbol _                = False
        isSpecial c = c `Set.member` specialChars

inlineGroup :: PandocMonad m => LP m Inlines
inlineGroup = do
  ils <- grouped inline
  if isNull ils
     then return mempty
     else return $ spanWith nullAttr ils
          -- we need the span so we can detitlecase bibtex entries;
          -- we need to know when something is {C}apitalized

doLHSverb :: PandocMonad m => LP m Inlines
doLHSverb =
  (codeWith ("",["haskell"],[]) . untokenize)
    <$> manyTill (satisfyTok (not . isNewlineTok)) (symbol '|')

mkImage :: PandocMonad m => [(Text, Text)] -> Text -> LP m Inlines
mkImage options (T.unpack -> src) = do
   let replaceTextwidth (k,v) =
         case numUnit v of
              Just (num, "\\textwidth") -> (k, showFl (num * 100) <> "%")
              _                         -> (k, v)
   let kvs = map replaceTextwidth
             $ filter (\(k,_) -> k `elem` ["width", "height"]) options
   let attr = ("",[], kvs)
   let alt = str "image"
   defaultExt <- getOption readerDefaultImageExtension
   let exts' = [".pdf", ".png", ".jpg", ".mps", ".jpeg", ".jbig2", ".jb2"]
   let exts  = exts' ++ map (map toUpper) exts'
   let findFile s [] = return s
       findFile s (e:es) = do
         let s' = addExtension s e
         exists <- fileExists s'
         if exists
            then return s'
            else findFile s es
   src' <- case takeExtension src of
               "" | not (T.null defaultExt) -> return $ addExtension src $ T.unpack defaultExt
                  | otherwise -> findFile src exts
               _  -> return src
   return $ imageWith attr (T.pack src') "" alt

doxspace :: PandocMonad m => LP m Inlines
doxspace =
  (space <$ lookAhead (satisfyTok startsWithLetter)) <|> return mempty
  where startsWithLetter (Tok _ Word t) =
          case T.uncons t of
               Just (c, _) | isLetter c -> True
               _           -> False
        startsWithLetter _ = False


-- converts e.g. \SI{1}[\$]{} to "$ 1" or \SI{1}{\euro} to "1 €"
dosiunitx :: PandocMonad m => LP m Inlines
dosiunitx = do
  skipopts
  value <- tok
  valueprefix <- option "" $ bracketed tok
  unit <- grouped (mconcat <$> many1 siUnit) <|> siUnit <|> tok
  let emptyOr160 "" = ""
      emptyOr160 _  = "\160"
  return . mconcat $ [valueprefix,
                      emptyOr160 valueprefix,
                      value,
                      emptyOr160 unit,
                      unit]

-- converts e.g. \SIRange{100}{200}{\ms} to "100 ms--200 ms"
doSIRange :: PandocMonad m => LP m Inlines
doSIRange = do
  skipopts
  startvalue <- tok
  startvalueprefix <- option "" $ bracketed tok
  stopvalue <- tok
  stopvalueprefix <- option "" $ bracketed tok
  unit <- grouped (mconcat <$> many1 siUnit) <|> siUnit <|> tok
  let emptyOr160 "" = ""
      emptyOr160 _  = "\160"
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

siUnit :: PandocMonad m => LP m Inlines
siUnit = do
  Tok _ (CtrlSeq name) _ <- anyControlSeq
  if name == "square"
     then do
       unit <- grouped (mconcat <$> many1 siUnit) <|> siUnit <|> tok
       return . mconcat $ [unit, "\178"]
     else
       case M.lookup name siUnitMap of
            Just il -> return il
            Nothing -> mzero

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
  , ("candela", str "cd")
  , ("celsius", str "°C")
  , ("centi", str "c")
  , ("coulomb", str "C")
  , ("dalton", str "Da")
  , ("day", str "d")
  , ("deca", str "d")
  , ("deci", str "d")
  , ("decibel", str "db")
  , ("degreeCelsius",str "°C")
  , ("degree", str "°")
  , ("deka", str "d")
  , ("electronvolt", str "eV")
  , ("exa", str "E")
  , ("farad", str "F")
  , ("femto", str "f")
  , ("giga", str "G")
  , ("gram", str "g")
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

lit :: Text -> LP m Inlines
lit = pure . str

removeDoubleQuotes :: Text -> Text
removeDoubleQuotes t =
  Data.Maybe.fromMaybe t $ T.stripPrefix "\"" t >>= T.stripSuffix "\""

doubleQuote :: PandocMonad m => LP m Inlines
doubleQuote =
       quoted' doubleQuoted (try $ count 2 $ symbol '`')
                     (void $ try $ count 2 $ symbol '\'')
   <|> quoted' doubleQuoted ((:[]) <$> symbol '“') (void $ symbol '”')
   -- the following is used by babel for localized quotes:
   <|> quoted' doubleQuoted (try $ sequence [symbol '"', symbol '`'])
                            (void $ try $ sequence [symbol '"', symbol '\''])

singleQuote :: PandocMonad m => LP m Inlines
singleQuote =
       quoted' singleQuoted ((:[]) <$> symbol '`')
                     (try $ symbol '\'' >>
                           notFollowedBy (satisfyTok startsWithLetter))
   <|> quoted' singleQuoted ((:[]) <$> symbol '‘')
                            (try $ symbol '’' >>
                                  notFollowedBy (satisfyTok startsWithLetter))
  where startsWithLetter (Tok _ Word t) =
          case T.uncons t of
               Just (c, _) | isLetter c -> True
               _           -> False
        startsWithLetter _ = False

quoted' :: PandocMonad m
        => (Inlines -> Inlines)
        -> LP m [Tok]
        -> LP m ()
        -> LP m Inlines
quoted' f starter ender = do
  startchs <- untokenize <$> starter
  smart <- extensionEnabled Ext_smart <$> getOption readerExtensions
  if smart
     then do
       ils <- many (notFollowedBy ender >> inline)
       (ender >> return (f (mconcat ils))) <|>
            (<> mconcat ils) <$>
                    lit (case startchs of
                              "``" -> "“"
                              "`"  -> "‘"
                              cs   -> cs)
     else lit startchs

enquote :: PandocMonad m => Bool -> Maybe Text -> LP m Inlines
enquote starred mblang = do
  skipopts
  let lang = mblang >>= babelLangToBCP47
  let langspan = case lang of
                      Nothing -> id
                      Just l  -> spanWith ("",[],[("lang", renderLang l)])
  quoteContext <- sQuoteContext <$> getState
  if starred || quoteContext == InDoubleQuote
     then singleQuoted . langspan <$> withQuoteContext InSingleQuote tok
     else doubleQuoted . langspan <$> withQuoteContext InDoubleQuote tok

blockquote :: PandocMonad m => Bool -> Maybe Text -> LP m Blocks
blockquote citations mblang = do
  citePar <- if citations
                then do
                  cs <- cites NormalCitation False
                  return $ para (cite cs mempty)
                else return mempty
  let lang = mblang >>= babelLangToBCP47
  let langdiv = case lang of
                      Nothing -> id
                      Just l  -> divWith ("",[],[("lang", renderLang l)])
  bs <- grouped block
  return $ blockQuote . langdiv $ (bs <> citePar)

doAcronym :: PandocMonad m => Text -> LP m Inlines
doAcronym form = do
  acro <- braced
  return . mconcat $ [spanWith ("",[],[("acronym-label", untokenize acro),
    ("acronym-form", "singular+" <> form)])
    $ str $ untokenize acro]

doAcronymPlural :: PandocMonad m => Text -> LP m Inlines
doAcronymPlural form = do
  acro <- braced
  plural <- lit "s"
  return . mconcat $ [spanWith ("",[],[("acronym-label", untokenize acro),
    ("acronym-form", "plural+" <> form)]) $
   mconcat [str $ untokenize acro, plural]]

doverb :: PandocMonad m => LP m Inlines
doverb = do
  Tok _ Symbol t <- anySymbol
  marker <- case T.uncons t of
              Just (c, ts) | T.null ts -> return c
              _            -> mzero
  withVerbatimMode $
    (code . untokenize) <$>
      manyTill (notFollowedBy newlineTok >> verbTok marker) (symbol marker)

verbTok :: PandocMonad m => Char -> LP m Tok
verbTok stopchar = do
  t@(Tok pos toktype txt) <- anyTok
  case T.findIndex (== stopchar) txt of
       Nothing -> return t
       Just i  -> do
         let (t1, t2) = T.splitAt i txt
         inp <- getInput
         setInput $ Tok (incSourceColumn pos i) Symbol (T.singleton stopchar)
                  : totoks (incSourceColumn pos (i + 1)) (T.drop 1 t2) ++ inp
         return $ Tok pos toktype t1

listingsLanguage :: [(Text, Text)] -> Maybe Text
listingsLanguage opts =
  case lookup "language" opts of
    Nothing  -> Nothing
    Just l   -> fromListingsLanguage l `mplus` Just l

dolstinline :: PandocMonad m => LP m Inlines
dolstinline = do
  options <- option [] keyvals
  let classes = maybeToList $ listingsLanguage options
  doinlinecode classes

domintinline :: PandocMonad m => LP m Inlines
domintinline = do
  skipopts
  cls <- untokenize <$> braced
  doinlinecode [cls]

doinlinecode :: PandocMonad m => [Text] -> LP m Inlines
doinlinecode classes = do
  Tok _ Symbol t <- anySymbol
  marker <- case T.uncons t of
              Just (c, ts) | T.null ts -> return c
              _            -> mzero
  let stopchar = if marker == '{' then '}' else marker
  withVerbatimMode $
    (codeWith ("",classes,[]) . T.map nlToSpace . untokenize) <$>
      manyTill (verbTok stopchar) (symbol stopchar)

nlToSpace :: Char -> Char
nlToSpace '\n' = ' '
nlToSpace x    = x

mathDisplay :: Text -> Inlines
mathDisplay = displayMath . trimMath

mathInline :: Text -> Inlines
mathInline = math . trimMath

dollarsMath :: PandocMonad m => LP m Inlines
dollarsMath = do
  symbol '$'
  display <- option False (True <$ symbol '$')
  (do contents <- try $ untokenize <$> pDollarsMath 0
      if display
         then (mathDisplay contents <$ symbol '$')
         else return $ mathInline contents)
   <|> (guard display >> return (mathInline ""))

-- Int is number of embedded groupings
pDollarsMath :: PandocMonad m => Int -> LP m [Tok]
pDollarsMath n = do
  tk@(Tok _ toktype t) <- anyTok
  case toktype of
       Symbol | t == "$"
              , n == 0 -> return []
              | t == "\\" -> do
                  tk' <- anyTok
                  ((tk :) . (tk' :)) <$> pDollarsMath n
              | t == "{" -> (tk :) <$> pDollarsMath (n+1)
              | t == "}" ->
                if n > 0
                then (tk :) <$> pDollarsMath (n-1)
                else mzero
       _ -> (tk :) <$> pDollarsMath n

-- citations

addPrefix :: [Inline] -> [Citation] -> [Citation]
addPrefix p (k:ks) = k {citationPrefix = p ++ citationPrefix k} : ks
addPrefix _ _      = []

addSuffix :: [Inline] -> [Citation] -> [Citation]
addSuffix s ks@(_:_) =
  let k = last ks
  in  init ks ++ [k {citationSuffix = citationSuffix k ++ s}]
addSuffix _ _ = []

simpleCiteArgs :: PandocMonad m => LP m [Citation]
simpleCiteArgs = try $ do
  first  <- optionMaybe $ toList <$> opt
  second <- optionMaybe $ toList <$> opt
  keys <- try $ bgroup *> manyTill citationLabel egroup
  let (pre, suf) = case (first  , second ) of
        (Just s , Nothing) -> (mempty, s )
        (Just s , Just t ) -> (s , t )
        _                  -> (mempty, mempty)
      conv k = Citation { citationId      = k
                        , citationPrefix  = []
                        , citationSuffix  = []
                        , citationMode    = NormalCitation
                        , citationHash    = 0
                        , citationNoteNum = 0
                        }
  return $ addPrefix pre $ addSuffix suf $ map conv keys

citationLabel :: PandocMonad m => LP m Text
citationLabel  = do
  sp
  untokenize <$>
    (many1 (satisfyTok isWordTok <|> symbolIn bibtexKeyChar)
          <* sp
          <* optional (symbol ',')
          <* sp)
  where bibtexKeyChar = ".:;?!`'()/*@_+=-&[]" :: [Char]

cites :: PandocMonad m => CitationMode -> Bool -> LP m [Citation]
cites mode multi = try $ do
  cits <- if multi
             then do
               multiprenote <- optionMaybe $ toList <$> paropt
               multipostnote <- optionMaybe $ toList <$> paropt
               let (pre, suf) = case (multiprenote, multipostnote) of
                     (Just s , Nothing) -> (mempty, s)
                     (Nothing , Just t) -> (mempty, t)
                     (Just s , Just t ) -> (s, t)
                     _                  -> (mempty, mempty)
               tempCits <- many1 simpleCiteArgs
               case tempCits of
                 (k:ks) -> case ks of
                             (_:_) -> return $ ((addMprenote pre k):init ks) ++
                                                 [addMpostnote suf (last ks)]
                             _ -> return [addMprenote pre (addMpostnote suf k)]
                 _ -> return [[]]
             else count 1 simpleCiteArgs
  let cs = concat cits
  return $ case mode of
        AuthorInText -> case cs of
                             (c:rest) -> c {citationMode = mode} : rest
                             []       -> []
        _            -> map (\a -> a {citationMode = mode}) cs
  where mprenote (k:ks) = (k:ks) ++ [Space]
        mprenote _ = mempty
        mpostnote (k:ks) = [Str ",", Space] ++ (k:ks)
        mpostnote _ = mempty
        addMprenote mpn (k:ks) =
          let mpnfinal = case citationPrefix k of
                           (_:_) -> mprenote mpn
                           _ -> mpn
          in addPrefix mpnfinal (k:ks)
        addMprenote _ _ = []
        addMpostnote = addSuffix . mpostnote

citation :: PandocMonad m => Text -> CitationMode -> Bool -> LP m Inlines
citation name mode multi = do
  (c,raw) <- withRaw $ cites mode multi
  return $ cite c (rawInline "latex" $ "\\" <> name <> untokenize raw)

handleCitationPart :: Inlines -> [Citation]
handleCitationPart ils =
  let isCite Cite{} = True
      isCite _      = False
      (pref, rest) = break isCite (toList ils)
  in case rest of
          (Cite cs _:suff) -> addPrefix pref $ addSuffix suff cs
          _                -> []

complexNatbibCitation :: PandocMonad m => CitationMode -> LP m Inlines
complexNatbibCitation mode = try $ do
  (cs, raw) <-
    withRaw $ concat <$> do
      bgroup
      items <- mconcat <$>
                many1 (notFollowedBy (symbol ';') >> inline)
                  `sepBy1` (symbol ';')
      egroup
      return $ map handleCitationPart items
  case cs of
       []       -> mzero
       (c:cits) -> return $ cite (c{ citationMode = mode }:cits)
                      (rawInline "latex" $ "\\citetext" <> untokenize raw)

inNote :: Inlines -> Inlines
inNote ils =
  note $ para $ ils <> str "."

inlineCommand' :: PandocMonad m => LP m Inlines
inlineCommand' = try $ do
  Tok _ (CtrlSeq name) cmd <- anyControlSeq
  guard $ name /= "begin" && name /= "end"
  star <- option "" ("*" <$ symbol '*' <* sp)
  overlay <- option "" overlaySpecification
  let name' = name <> star <> overlay
  let names = ordNub [name', name] -- check non-starred as fallback
  let raw = do
       guard $ isInlineCommand name || not (isBlockCommand name)
       rawcommand <- getRawCommand name (cmd <> star)
       (guardEnabled Ext_raw_tex >> return (rawInline "latex" rawcommand))
         <|> ignore rawcommand
  lookupListDefault raw names inlineCommands


tok :: PandocMonad m => LP m Inlines
tok = try $ spaces >> grouped inline <|> inlineCommand' <|> singleChar'
  where singleChar' = do
          Tok _ _ t <- singleChar
          return $ str t

opt :: PandocMonad m => LP m Inlines
opt = bracketed inline <|> (str <$> rawopt)

paropt :: PandocMonad m => LP m Inlines
paropt = parenWrapped inline

inBrackets :: Inlines -> Inlines
inBrackets x = str "[" <> x <> str "]"

unescapeURL :: Text -> Text
unescapeURL = T.concat . go . T.splitOn "\\"
  where
    isEscapable c = c `elemText` "#$%&~_^\\{}"
    go (x:xs) = x : map unescapeInterior xs
    go []     = []
    unescapeInterior t
      | Just (c, _) <- T.uncons t
      , isEscapable c = t
      | otherwise = "\\" <> t

mathEnvWith :: PandocMonad m
            => (Inlines -> a) -> Maybe Text -> Text -> LP m a
mathEnvWith f innerEnv name = f . mathDisplay . inner <$> mathEnv name
   where inner x = case innerEnv of
                        Nothing -> x
                        Just y  -> "\\begin{" <> y <> "}\n" <> x <>
                                   "\\end{" <> y <> "}"

mathEnv :: PandocMonad m => Text -> LP m Text
mathEnv name = do
  skipopts
  optional blankline
  res <- manyTill anyTok (end_ name)
  return $ stripTrailingNewlines $ untokenize res

inlineEnvironment :: PandocMonad m => LP m Inlines
inlineEnvironment = try $ do
  controlSeq "begin"
  name <- untokenize <$> braced
  M.findWithDefault mzero name inlineEnvironments

inlineEnvironments :: PandocMonad m => M.Map Text (LP m Inlines)
inlineEnvironments = M.fromList [
    ("displaymath", mathEnvWith id Nothing "displaymath")
  , ("math", math <$> mathEnv "math")
  , ("equation", mathEnvWith id Nothing "equation")
  , ("equation*", mathEnvWith id Nothing "equation*")
  , ("gather", mathEnvWith id (Just "gathered") "gather")
  , ("gather*", mathEnvWith id (Just "gathered") "gather*")
  , ("multline", mathEnvWith id (Just "gathered") "multline")
  , ("multline*", mathEnvWith id (Just "gathered") "multline*")
  , ("eqnarray", mathEnvWith id (Just "aligned") "eqnarray")
  , ("eqnarray*", mathEnvWith id (Just "aligned") "eqnarray*")
  , ("align", mathEnvWith id (Just "aligned") "align")
  , ("align*", mathEnvWith id (Just "aligned") "align*")
  , ("alignat", mathEnvWith id (Just "aligned") "alignat")
  , ("alignat*", mathEnvWith id (Just "aligned") "alignat*")
  , ("dmath", mathEnvWith id Nothing "dmath")
  , ("dmath*", mathEnvWith id Nothing "dmath*")
  , ("dgroup", mathEnvWith id (Just "aligned") "dgroup")
  , ("dgroup*", mathEnvWith id (Just "aligned") "dgroup*")
  , ("darray", mathEnvWith id (Just "aligned") "darray")
  , ("darray*", mathEnvWith id (Just "aligned") "darray*")
  ]

inlineCommands :: PandocMonad m => M.Map Text (LP m Inlines)
inlineCommands = M.union inlineLanguageCommands $ M.fromList
  [ ("emph", extractSpaces emph <$> tok)
  , ("textit", extractSpaces emph <$> tok)
  , ("textsl", extractSpaces emph <$> tok)
  , ("textsc", extractSpaces smallcaps <$> tok)
  , ("textsf", extractSpaces (spanWith ("",["sans-serif"],[])) <$> tok)
  , ("textmd", extractSpaces (spanWith ("",["medium"],[])) <$> tok)
  , ("textrm", extractSpaces (spanWith ("",["roman"],[])) <$> tok)
  , ("textup", extractSpaces (spanWith ("",["upright"],[])) <$> tok)
  , ("texttt", ttfamily)
  , ("sout", extractSpaces strikeout <$> tok)
  , ("alert", skipopts >> spanWith ("",["alert"],[]) <$> tok) -- beamer
  , ("lq", return (str "‘"))
  , ("rq", return (str "’"))
  , ("textquoteleft", return (str "‘"))
  , ("textquoteright", return (str "’"))
  , ("textquotedblleft", return (str "“"))
  , ("textquotedblright", return (str "”"))
  , ("textsuperscript", extractSpaces superscript <$> tok)
  , ("textsubscript", extractSpaces subscript <$> tok)
  , ("textbackslash", lit "\\")
  , ("backslash", lit "\\")
  , ("slash", lit "/")
  , ("textbf", extractSpaces strong <$> tok)
  , ("textnormal", extractSpaces (spanWith ("",["nodecor"],[])) <$> tok)
  , ("underline", underline <$> tok)
  , ("ldots", lit "…")
  , ("vdots", lit "\8942")
  , ("dots", lit "…")
  , ("mdots", lit "…")
  , ("sim", lit "~")
  , ("sep", lit ",")
  , ("label", rawInlineOr "label" dolabel)
  , ("ref", rawInlineOr "ref" $ doref "ref")
  , ("cref", rawInlineOr "cref" $ doref "ref")       -- from cleveref.sty
  , ("vref", rawInlineOr "vref" $ doref "ref+page")  -- from varioref.sty
  , ("eqref", rawInlineOr "eqref" $ doref "eqref")   -- from amsmath.sty
  , ("mbox", rawInlineOr "mbox" $ processHBox <$> tok)
  , ("hbox", rawInlineOr "hbox" $ processHBox <$> tok)
  , ("lettrine", rawInlineOr "lettrine" $ lettrine)
  , ("(", mathInline . untokenize <$> manyTill anyTok (controlSeq ")"))
  , ("[", mathDisplay . untokenize <$> manyTill anyTok (controlSeq "]"))
  , ("ensuremath", mathInline . untokenize <$> braced)
  , ("texorpdfstring", const <$> tok <*> tok)
  , ("P", lit "¶")
  , ("S", lit "§")
  , ("$", lit "$")
  , ("%", lit "%")
  , ("&", lit "&")
  , ("#", lit "#")
  , ("_", lit "_")
  , ("{", lit "{")
  , ("}", lit "}")
  , ("qed", lit "\a0\x25FB")
  -- old TeX commands
  , ("em", extractSpaces emph <$> inlines)
  , ("it", extractSpaces emph <$> inlines)
  , ("sl", extractSpaces emph <$> inlines)
  , ("bf", extractSpaces strong <$> inlines)
  , ("tt", code . stringify . toList <$> inlines)
  , ("rm", inlines)
  , ("itshape", extractSpaces emph <$> inlines)
  , ("slshape", extractSpaces emph <$> inlines)
  , ("scshape", extractSpaces smallcaps <$> inlines)
  , ("bfseries", extractSpaces strong <$> inlines)
  , ("MakeUppercase", makeUppercase <$> tok)
  , ("MakeTextUppercase", makeUppercase <$> tok) -- textcase
  , ("uppercase", makeUppercase <$> tok)
  , ("MakeLowercase", makeLowercase <$> tok)
  , ("MakeTextLowercase", makeLowercase <$> tok)
  , ("lowercase", makeLowercase <$> tok)
  , ("/", pure mempty) -- italic correction
  , ("aa", lit "å")
  , ("AA", lit "Å")
  , ("ss", lit "ß")
  , ("o", lit "ø")
  , ("O", lit "Ø")
  , ("L", lit "Ł")
  , ("l", lit "ł")
  , ("ae", lit "æ")
  , ("AE", lit "Æ")
  , ("oe", lit "œ")
  , ("OE", lit "Œ")
  , ("pounds", lit "£")
  , ("euro", lit "€")
  , ("copyright", lit "©")
  , ("textasciicircum", lit "^")
  , ("textasciitilde", lit "~")
  , ("H", accent '\779' Nothing) -- hungarumlaut
  , ("`", accent '\768' (Just '`')) -- grave
  , ("'", accent '\769' (Just '\'')) -- acute
  , ("^", accent '\770' (Just '^')) -- circ
  , ("~", accent '\771' (Just '~')) -- tilde
  , ("\"", accent '\776' Nothing) -- umlaut
  , (".", accent '\775' Nothing) -- dot
  , ("=", accent '\772' Nothing) -- macron
  , ("|", accent '\781' Nothing) -- vertical line above
  , ("b", accent '\817' Nothing) -- macron below
  , ("c", accent '\807' Nothing) -- cedilla
  , ("G", accent '\783' Nothing) -- doublegrave
  , ("h", accent '\777' Nothing) -- hookabove
  , ("d", accent '\803' Nothing) -- dotbelow
  , ("f", accent '\785' Nothing)  -- inverted breve
  , ("r", accent '\778' Nothing)  -- ringabove
  , ("t", accent '\865' Nothing)  -- double inverted breve
  , ("U", accent '\782' Nothing)  -- double vertical line above
  , ("v", accent '\780' Nothing) -- hacek
  , ("u", accent '\774' Nothing) -- breve
  , ("k", accent '\808' Nothing) -- ogonek
  , ("textogonekcentered", accent '\808' Nothing) -- ogonek
  , ("i", lit "ı")  -- dotless i
  , ("j", lit "ȷ")  -- dotless j
  , ("newtie", accent '\785' Nothing) -- inverted breve
  , ("textcircled", accent '\8413' Nothing) -- combining circle
  , ("\\", linebreak <$ (do inTableCell <- sInTableCell <$> getState
                            guard $ not inTableCell
                            optional opt
                            spaces))
  , (",", lit "\8198")
  , ("@", pure mempty)
  , (" ", lit "\160")
  , ("ps", pure $ str "PS." <> space)
  , ("TeX", lit "TeX")
  , ("LaTeX", lit "LaTeX")
  , ("bar", lit "|")
  , ("textless", lit "<")
  , ("textgreater", lit ">")
  , ("thanks", skipopts >> note <$> grouped block)
  , ("footnote", skipopts >> note <$> grouped block)
  , ("passthrough", tok) -- \passthrough macro used by latex writer
                         -- for listings
  , ("verb", doverb)
  , ("lstinline", dolstinline)
  , ("mintinline", domintinline)
  , ("Verb", doverb)
  , ("url", (\url -> link url "" (str url)) . unescapeURL . untokenize <$>
                  bracedUrl)
  , ("nolinkurl", code . unescapeURL . untokenize <$> bracedUrl)
  , ("href", do url <- bracedUrl
                sp
                link (unescapeURL $ untokenize url) "" <$> tok)
  , ("includegraphics", do options <- option [] keyvals
                           src <- braced
                           mkImage options . unescapeURL . removeDoubleQuotes $
                               untokenize src)
  , ("enquote*", enquote True Nothing)
  , ("enquote", enquote False Nothing)
  -- foreignquote is supposed to use native quote marks
  , ("foreignquote*", braced >>= enquote True . Just . untokenize)
  , ("foreignquote", braced >>= enquote False . Just . untokenize)
  -- hypehnquote uses regular quotes
  , ("hyphenquote*", braced >>= enquote True . Just . untokenize)
  , ("hyphenquote", braced >>= enquote False . Just . untokenize)
  , ("figurename", doTerm Translations.Figure)
  , ("prefacename", doTerm Translations.Preface)
  , ("refname", doTerm Translations.References)
  , ("bibname", doTerm Translations.Bibliography)
  , ("chaptername", doTerm Translations.Chapter)
  , ("partname", doTerm Translations.Part)
  , ("contentsname", doTerm Translations.Contents)
  , ("listfigurename", doTerm Translations.ListOfFigures)
  , ("listtablename", doTerm Translations.ListOfTables)
  , ("indexname", doTerm Translations.Index)
  , ("abstractname", doTerm Translations.Abstract)
  , ("tablename", doTerm Translations.Table)
  , ("enclname", doTerm Translations.Encl)
  , ("ccname", doTerm Translations.Cc)
  , ("headtoname", doTerm Translations.To)
  , ("pagename", doTerm Translations.Page)
  , ("seename", doTerm Translations.See)
  , ("seealsoname", doTerm Translations.SeeAlso)
  , ("proofname", doTerm Translations.Proof)
  , ("glossaryname", doTerm Translations.Glossary)
  , ("lstlistingname", doTerm Translations.Listing)
  , ("cite", citation "cite" NormalCitation False)
  , ("Cite", citation "Cite" NormalCitation False)
  , ("citep", citation "citep" NormalCitation False)
  , ("citep*", citation "citep*" NormalCitation False)
  , ("citeal", citation "citeal" NormalCitation False)
  , ("citealp", citation "citealp" NormalCitation False)
  , ("citealp*", citation "citealp*" NormalCitation False)
  , ("autocite", citation "autocite" NormalCitation False)
  , ("smartcite", citation "smartcite" NormalCitation False)
  , ("footcite", inNote <$> citation "footcite" NormalCitation False)
  , ("parencite", citation "parencite" NormalCitation False)
  , ("supercite", citation "supercite" NormalCitation False)
  , ("footcitetext", inNote <$> citation "footcitetext" NormalCitation False)
  , ("citeyearpar", citation "citeyearpar" SuppressAuthor False)
  , ("citeyear", citation "citeyear" SuppressAuthor False)
  , ("autocite*", citation "autocite*" SuppressAuthor False)
  , ("cite*", citation "cite*" SuppressAuthor False)
  , ("parencite*", citation "parencite*" SuppressAuthor False)
  , ("textcite", citation "textcite" AuthorInText False)
  , ("citet", citation "citet" AuthorInText False)
  , ("citet*", citation "citet*" AuthorInText False)
  , ("citealt", citation "citealt" AuthorInText False)
  , ("citealt*", citation "citealt*" AuthorInText False)
  , ("textcites", citation "textcites" AuthorInText True)
  , ("cites", citation "cites" NormalCitation True)
  , ("autocites", citation "autocites" NormalCitation True)
  , ("footcites", inNote <$> citation "footcites" NormalCitation True)
  , ("parencites", citation "parencites" NormalCitation True)
  , ("supercites", citation "supercites" NormalCitation True)
  , ("footcitetexts", inNote <$> citation "footcitetexts" NormalCitation True)
  , ("Autocite", citation "Autocite" NormalCitation False)
  , ("Smartcite", citation "Smartcite" NormalCitation False)
  , ("Footcite", inNote <$> citation "Footcite" NormalCitation False)
  , ("Parencite", citation "Parencite" NormalCitation False)
  , ("Supercite", citation "Supercite" NormalCitation False)
  , ("Footcitetext", inNote <$> citation "Footcitetext" NormalCitation False)
  , ("Citeyearpar", citation "Citeyearpar" SuppressAuthor False)
  , ("Citeyear", citation "Citeyear" SuppressAuthor False)
  , ("Autocite*", citation "Autocite*" SuppressAuthor False)
  , ("Cite*", citation "Cite*" SuppressAuthor False)
  , ("Parencite*", citation "Parencite*" SuppressAuthor False)
  , ("Textcite", citation "Textcite" AuthorInText False)
  , ("Textcites", citation "Textcites" AuthorInText True)
  , ("Cites", citation "Cites" NormalCitation True)
  , ("Autocites", citation "Autocites" NormalCitation True)
  , ("Footcites", inNote <$> citation "Footcites" NormalCitation True)
  , ("Parencites", citation "Parencites" NormalCitation True)
  , ("Supercites", citation "Supercites" NormalCitation True)
  , ("Footcitetexts", inNote <$> citation "Footcitetexts" NormalCitation True)
  , ("citetext", complexNatbibCitation NormalCitation)
  , ("citeauthor", (try (tok *> sp *> controlSeq "citetext") *>
                        complexNatbibCitation AuthorInText)
                   <|> citation "citeauthor" AuthorInText False)
  , ("nocite", mempty <$ (citation "nocite" NormalCitation False >>=
                          addMeta "nocite"))
  , ("hyperlink", hyperlink)
  , ("hypertarget", hypertargetInline)
  -- glossaries package
  , ("gls", doAcronym "short")
  , ("Gls", doAcronym "short")
  , ("glsdesc", doAcronym "long")
  , ("Glsdesc", doAcronym "long")
  , ("GLSdesc", doAcronym "long")
  , ("acrlong", doAcronym "long")
  , ("Acrlong", doAcronym "long")
  , ("acrfull", doAcronym "full")
  , ("Acrfull", doAcronym "full")
  , ("acrshort", doAcronym "abbrv")
  , ("Acrshort", doAcronym "abbrv")
  , ("glspl", doAcronymPlural "short")
  , ("Glspl", doAcronymPlural "short")
  , ("glsdescplural", doAcronymPlural "long")
  , ("Glsdescplural", doAcronymPlural "long")
  , ("GLSdescplural", doAcronymPlural "long")
  -- acronyms package
  , ("ac", doAcronym "short")
  , ("acf", doAcronym "full")
  , ("acs", doAcronym "abbrv")
  , ("acp", doAcronymPlural "short")
  , ("acfp", doAcronymPlural "full")
  , ("acsp", doAcronymPlural "abbrv")
  -- siuntix
  , ("SI", dosiunitx)
  , ("SIRange", doSIRange)
  -- hyphenat
  , ("bshyp", lit "\\\173")
  , ("fshyp", lit "/\173")
  , ("dothyp", lit ".\173")
  , ("colonhyp", lit ":\173")
  , ("hyp", lit "-")
  , ("nohyphens", tok)
  , ("textnhtt", ttfamily)
  , ("nhttfamily", ttfamily)
  -- LaTeX colors
  , ("textcolor", coloredInline "color")
  , ("colorbox", coloredInline "background-color")
  -- fontawesome
  , ("faCheck", lit "\10003")
  , ("faClose", lit "\10007")
  -- xspace
  , ("xspace", doxspace)
  -- etoolbox
  , ("ifstrequal", ifstrequal)
  , ("newtoggle", braced >>= newToggle)
  , ("toggletrue", braced >>= setToggle True)
  , ("togglefalse", braced >>= setToggle False)
  , ("iftoggle", try $ ifToggle >> inline)
  -- biblatex misc
  , ("RN", romanNumeralUpper)
  , ("Rn", romanNumeralLower)
  -- babel
  , ("foreignlanguage", foreignlanguage)
  -- include
  , ("input", rawInlineOr "input" $ include "input")
  -- soul package
  , ("ul", underline <$> tok)
  -- ulem package
  , ("uline", underline <$> tok)
  -- plain tex stuff that should just be passed through as raw tex
  , ("ifdim", ifdim)
  ]

accent :: PandocMonad m => Char -> Maybe Char -> LP m Inlines
accent combiningAccent fallBack = try $ do
  ils <- tok
  case toList ils of
       (Str (T.uncons -> Just (x, xs)) : ys) -> return $ fromList $
         -- try to normalize to the combined character:
         Str (Normalize.normalize Normalize.NFC
               (T.pack [x, combiningAccent]) <> xs) : ys
       [Space]           -> return $ str $ T.singleton $ fromMaybe combiningAccent fallBack
       []                -> return $ str $ T.singleton $ fromMaybe combiningAccent fallBack
       _                 -> return ils


lettrine :: PandocMonad m => LP m Inlines
lettrine = do
  optional opt
  x <- tok
  y <- tok
  return $ extractSpaces (spanWith ("",["lettrine"],[])) x <> smallcaps y

ifdim :: PandocMonad m => LP m Inlines
ifdim = do
  contents <- manyTill anyTok (controlSeq "fi")
  return $ rawInline "latex" $ "\\ifdim" <> untokenize contents <> "\\fi"

makeUppercase :: Inlines -> Inlines
makeUppercase = fromList . walk (alterStr T.toUpper) . toList

makeLowercase :: Inlines -> Inlines
makeLowercase = fromList . walk (alterStr T.toLower) . toList

alterStr :: (Text -> Text) -> Inline -> Inline
alterStr f (Str xs) = Str (f xs)
alterStr _ x = x

foreignlanguage :: PandocMonad m => LP m Inlines
foreignlanguage = do
  babelLang <- untokenize <$> braced
  case babelLangToBCP47 babelLang of
       Just lang -> spanWith ("", [], [("lang",  renderLang lang)]) <$> tok
       _ -> tok

inlineLanguageCommands :: PandocMonad m => M.Map Text (LP m Inlines)
inlineLanguageCommands = M.fromList $ mk <$> M.toList polyglossiaLangToBCP47
  where
    mk (polyglossia, bcp47Func) =
      ("text" <> polyglossia, inlineLanguage bcp47Func)

inlineLanguage :: PandocMonad m => (Text -> Lang) -> LP m Inlines
inlineLanguage bcp47Func = do
  o <- option "" $ T.filter (\c -> c /= '[' && c /= ']')
                <$> rawopt
  let lang = renderLang $ bcp47Func o
  extractSpaces (spanWith ("", [], [("lang", lang)])) <$> tok

hyperlink :: PandocMonad m => LP m Inlines
hyperlink = try $ do
  src <- untokenize <$> braced
  lab <- tok
  return $ link ("#" <> src) "" lab

hypertargetBlock :: PandocMonad m => LP m Blocks
hypertargetBlock = try $ do
  ref <- untokenize <$> braced
  bs <- grouped block
  case toList bs of
       [Header 1 (ident,_,_) _] | ident == ref -> return bs
       _                        -> return $ divWith (ref, [], []) bs

hypertargetInline :: PandocMonad m => LP m Inlines
hypertargetInline = try $ do
  ref <- untokenize <$> braced
  ils <- grouped inline
  return $ spanWith (ref, [], []) ils

romanNumeralUpper :: (PandocMonad m) => LP m Inlines
romanNumeralUpper =
  str . toRomanNumeral <$> romanNumeralArg

romanNumeralLower :: (PandocMonad m) => LP m Inlines
romanNumeralLower =
  str . T.toLower . toRomanNumeral <$> romanNumeralArg

romanNumeralArg :: (PandocMonad m) => LP m Int
romanNumeralArg = spaces *> (parser <|> inBraces)
  where
    inBraces = do
      symbol '{'
      spaces
      res <- parser
      spaces
      symbol '}'
      return res
    parser = do
      Tok _ Word s <- satisfyTok isWordTok
      let (digits, rest) = T.span isDigit s
      unless (T.null rest) $
        Prelude.fail "Non-digits in argument to \\Rn or \\RN"
      safeRead digits

newToggle :: (Monoid a, PandocMonad m) => [Tok] -> LP m a
newToggle name = do
  updateState $ \st ->
    st{ sToggles = M.insert (untokenize name) False (sToggles st) }
  return mempty

setToggle :: (Monoid a, PandocMonad m) => Bool -> [Tok] -> LP m a
setToggle on name = do
  updateState $ \st ->
    st{ sToggles = M.adjust (const on) (untokenize name) (sToggles st) }
  return mempty

ifToggle :: PandocMonad m => LP m ()
ifToggle = do
  name <- braced
  spaces
  yes <- braced
  spaces
  no <- braced
  toggles <- sToggles <$> getState
  inp <- getInput
  let name' = untokenize name
  case M.lookup name' toggles of
                Just True  -> setInput (yes ++ inp)
                Just False -> setInput (no  ++ inp)
                Nothing    -> do
                  pos <- getPosition
                  report $ UndefinedToggle name' pos
  return ()

doTerm :: PandocMonad m => Translations.Term -> LP m Inlines
doTerm term = str <$> translateTerm term

ifstrequal :: (PandocMonad m, Monoid a) => LP m a
ifstrequal = do
  str1 <- tok
  str2 <- tok
  ifequal <- braced
  ifnotequal <- braced
  if str1 == str2
     then getInput >>= setInput . (ifequal ++)
     else getInput >>= setInput . (ifnotequal ++)
  return mempty

coloredInline :: PandocMonad m => Text -> LP m Inlines
coloredInline stylename = do
  skipopts
  color <- braced
  spanWith ("",[],[("style",stylename <> ": " <> untokenize color)]) <$> tok

ttfamily :: PandocMonad m => LP m Inlines
ttfamily = (code . stringify . toList) <$> tok

rawInlineOr :: PandocMonad m => Text -> LP m Inlines -> LP m Inlines
rawInlineOr name' fallback = do
  parseRaw <- extensionEnabled Ext_raw_tex <$> getOption readerExtensions
  if parseRaw
     then rawInline "latex" <$> getRawCommand name' ("\\" <> name')
     else fallback

processHBox :: Inlines -> Inlines
processHBox = walk convert
  where
    convert Space     = Str $ T.singleton $ chr 160 -- non-breakable space
    convert SoftBreak = Str $ T.singleton $ chr 160 -- non-breakable space
    convert LineBreak = Str ""
    convert x         = x

isBlockCommand :: Text -> Bool
isBlockCommand s =
  s `M.member` (blockCommands :: M.Map Text (LP PandocPure Blocks))
  || s `Set.member` treatAsBlock

treatAsBlock :: Set.Set Text
treatAsBlock = Set.fromList
   [ "special", "pdfannot", "pdfstringdef"
   , "bibliographystyle"
   , "maketitle", "makeindex", "makeglossary"
   , "addcontentsline", "addtocontents", "addtocounter"
      -- \ignore{} is used conventionally in literate haskell for definitions
      -- that are to be processed by the compiler but not printed.
   , "ignore"
   , "hyperdef"
   , "markboth", "markright", "markleft"
   , "hspace", "vspace"
   , "newpage"
   , "clearpage"
   , "pagebreak"
   , "titleformat"
   , "listoffigures"
   , "listoftables"
   , "write"
   ]

isInlineCommand :: Text -> Bool
isInlineCommand s =
  s `M.member` (inlineCommands :: M.Map Text (LP PandocPure Inlines))
  || s `Set.member` treatAsInline

treatAsInline :: Set.Set Text
treatAsInline = Set.fromList
  [ "index"
  , "hspace"
  , "vspace"
  , "noindent"
  , "newpage"
  , "clearpage"
  , "pagebreak"
  ]

label :: PandocMonad m => LP m ()
label = do
  controlSeq "label"
  t <- braced
  updateState $ \st -> st{ sLastLabel = Just $ untokenize t }

dolabel :: PandocMonad m => LP m Inlines
dolabel = do
  v <- braced
  let refstr = untokenize v
  updateState $ \st ->
    st{ sLastLabel = Just refstr }
  return $ spanWith (refstr,[],[("label", refstr)])
    $ inBrackets $ str $ untokenize v

doref :: PandocMonad m => Text -> LP m Inlines
doref cls = do
  v <- braced
  let refstr = untokenize v
  return $ linkWith ("",[],[ ("reference-type", cls)
                           , ("reference", refstr)])
                    ("#" <> refstr)
                    ""
                    (inBrackets $ str refstr)

lookupListDefault :: (Ord k) => v -> [k] -> M.Map k v -> v
lookupListDefault d = (fromMaybe d .) . lookupList
  where lookupList l m = msum $ map (`M.lookup` m) l

inline :: PandocMonad m => LP m Inlines
inline = (mempty <$ comment)
     <|> (space  <$ whitespace)
     <|> (softbreak <$ endline)
     <|> word
     <|> macroDef (rawInline "latex")
     <|> inlineCommand'
     <|> inlineEnvironment
     <|> inlineGroup
     <|> (symbol '-' *>
           option (str "-") (symbol '-' *>
             option (str "–") (str "—" <$ symbol '-')))
     <|> doubleQuote
     <|> singleQuote
     <|> (str "”" <$ try (symbol '\'' >> symbol '\''))
     <|> (str "”" <$ symbol '”')
     <|> (str "’" <$ symbol '\'')
     <|> (str "’" <$ symbol '’')
     <|> (str "\160" <$ symbol '~')
     <|> dollarsMath
     <|> (guardEnabled Ext_literate_haskell *> symbol '|' *> doLHSverb)
     <|> (str . T.singleton <$> primEscape)
     <|> regularSymbol
     <|> (do res <- symbolIn "#^'`\"[]&"
             pos <- getPosition
             let s = untoken res
             report $ ParsingUnescaped s pos
             return $ str s)

inlines :: PandocMonad m => LP m Inlines
inlines = mconcat <$> many inline

-- block elements:

preamble :: PandocMonad m => LP m Blocks
preamble = mconcat <$> many preambleBlock
  where preambleBlock =  (mempty <$ spaces1)
                     <|> macroDef (rawBlock "latex")
                     <|> (mempty <$ blockCommand)
                     <|> (mempty <$ braced)
                     <|> (do notFollowedBy (begin_ "document")
                             anyTok
                             return mempty)

paragraph :: PandocMonad m => LP m Blocks
paragraph = do
  x <- trimInlines . mconcat <$> many1 inline
  if x == mempty
     then return mempty
     else return $ para x

rawBlockOr :: PandocMonad m => Text -> LP m Blocks -> LP m Blocks
rawBlockOr name fallback = do
  -- if raw_tex allowed, don't process
  parseRaw <- extensionEnabled Ext_raw_tex <$> getOption readerExtensions
  if parseRaw
     then rawBlock "latex" <$> getRawCommand name ("\\" <> name)
     else fallback

doSubfile :: PandocMonad m => LP m Blocks
doSubfile = do
  skipMany opt
  f <- T.unpack . removeDoubleQuotes . T.strip . untokenize <$> braced
  oldToks <- getInput
  setInput []
  insertIncluded ".tex" f
  bs <- blocks
  eof
  setInput oldToks
  return bs

include :: (PandocMonad m, Monoid a) => Text -> LP m a
include name = do
  skipMany opt
  fs <- (map (T.unpack . removeDoubleQuotes . T.strip) . T.splitOn "," .
         untokenize) <$> braced
  let defaultExt | name == "usepackage" = ".sty"
                 | otherwise            = ".tex"
  mapM_ (insertIncluded defaultExt) fs
  return mempty

insertIncluded :: PandocMonad m
               => FilePath
               -> FilePath
               -> LP m ()
insertIncluded defaultExtension f' = do
  let f = case takeExtension f' of
                ".tex" -> f'
                ".sty" -> f'
                _      -> addExtension f' defaultExtension
  dirs <- (map T.unpack . splitTextBy (==':') . fromMaybe ".") <$> lookupEnv "TEXINPUTS"
  pos <- getPosition
  containers <- getIncludeFiles <$> getState
  when (T.pack f `elem` containers) $
    throwError $ PandocParseError $ T.pack $ "Include file loop at " ++ show pos
  updateState $ addIncludeFile $ T.pack f
  mbcontents <- readFileFromDirs dirs f
  contents <- case mbcontents of
                   Just s -> return s
                   Nothing -> do
                     report $ CouldNotLoadIncludeFile (T.pack f) pos
                     return ""
  getInput >>= setInput . (tokenize f contents ++)
  updateState dropLatestIncludeFile

addMeta :: PandocMonad m => ToMetaValue a => Text -> a -> LP m ()
addMeta field val = updateState $ \st ->
   st{ sMeta = addMetaField field val $ sMeta st }

authors :: PandocMonad m => LP m ()
authors = try $ do
  bgroup
  let oneAuthor = mconcat <$>
       many1 (notFollowedBy' (controlSeq "and") >>
               (inline <|> mempty <$ blockCommand))
               -- skip e.g. \vspace{10pt}
  auths <- sepBy oneAuthor (controlSeq "and")
  egroup
  addMeta "author" (map trimInlines auths)

macroDef :: (PandocMonad m, Monoid a) => (Text -> a) -> LP m a
macroDef constructor = do
    (_, s) <- withRaw (commandDef <|> environmentDef)
    (constructor (untokenize s) <$
      guardDisabled Ext_latex_macros)
     <|> return mempty
  where commandDef = do
          (name, macro') <- newcommand <|> letmacro <|> defmacro
          guardDisabled Ext_latex_macros <|>
           updateState (\s -> s{ sMacros = M.insert name macro' (sMacros s) })
        environmentDef = do
          mbenv <- newenvironment
          case mbenv of
            Nothing -> return ()
            Just (name, macro1, macro2) ->
              guardDisabled Ext_latex_macros <|>
                do updateState $ \s -> s{ sMacros =
                    M.insert name macro1 (sMacros s) }
                   updateState $ \s -> s{ sMacros =
                    M.insert ("end" <> name) macro2 (sMacros s) }
        -- @\newenvironment{envname}[n-args][default]{begin}{end}@
        -- is equivalent to
        -- @\newcommand{\envname}[n-args][default]{begin}@
        -- @\newcommand{\endenvname}@

letmacro :: PandocMonad m => LP m (Text, Macro)
letmacro = do
  controlSeq "let"
  (name, contents) <- withVerbatimMode $ do
    Tok _ (CtrlSeq name) _ <- anyControlSeq
    optional $ symbol '='
    spaces
    -- we first parse in verbatim mode, and then expand macros,
    -- because we don't want \let\foo\bar to turn into
    -- \let\foo hello if we have previously \def\bar{hello}
    contents <- bracedOrToken
    return (name, contents)
  contents' <- doMacros' 0 contents
  return (name, Macro ExpandWhenDefined [] Nothing contents')

defmacro :: PandocMonad m => LP m (Text, Macro)
defmacro = try $
  -- we use withVerbatimMode, because macros are to be expanded
  -- at point of use, not point of definition
  withVerbatimMode $ do
    controlSeq "def"
    Tok _ (CtrlSeq name) _ <- anyControlSeq
    argspecs <- many (argspecArg <|> argspecPattern)
    contents <- bracedOrToken
    return (name, Macro ExpandWhenUsed argspecs Nothing contents)

argspecArg :: PandocMonad m => LP m ArgSpec
argspecArg = do
  Tok _ (Arg i) _ <- satisfyTok isArgTok
  return $ ArgNum i

argspecPattern :: PandocMonad m => LP m ArgSpec
argspecPattern =
  Pattern <$> many1 (satisfyTok (\(Tok _ toktype' txt) ->
                              (toktype' == Symbol || toktype' == Word) &&
                              (txt /= "{" && txt /= "\\" && txt /= "}")))

newcommand :: PandocMonad m => LP m (Text, Macro)
newcommand = do
  pos <- getPosition
  Tok _ (CtrlSeq mtype) _ <- controlSeq "newcommand" <|>
                             controlSeq "renewcommand" <|>
                             controlSeq "providecommand" <|>
                             controlSeq "DeclareMathOperator" <|>
                             controlSeq "DeclareRobustCommand"
  withVerbatimMode $ do
    Tok _ (CtrlSeq name) txt <- do
      optional (symbol '*')
      anyControlSeq <|>
        (symbol '{' *> spaces *> anyControlSeq <* spaces <* symbol '}')
    spaces
    numargs <- option 0 $ try bracketedNum
    let argspecs = map ArgNum [1..numargs]
    spaces
    optarg <- option Nothing $ Just <$> try bracketedToks
    spaces
    contents' <- bracedOrToken
    let contents =
         case mtype of
              "DeclareMathOperator" ->
                 Tok pos (CtrlSeq "mathop") "\\mathop"
                 : Tok pos Symbol "{"
                 : Tok pos (CtrlSeq "mathrm") "\\mathrm"
                 : Tok pos Symbol "{"
                 : (contents' ++
                   [ Tok pos Symbol "}", Tok pos Symbol "}" ])
              _                     -> contents'
    macros <- sMacros <$> getState
    case M.lookup name macros of
        Just macro
          | mtype == "newcommand" -> do
              report $ MacroAlreadyDefined txt pos
              return (name, macro)
          | mtype == "providecommand" -> return (name, macro)
        _ -> return (name, Macro ExpandWhenUsed argspecs optarg contents)

newenvironment :: PandocMonad m => LP m (Maybe (Text, Macro, Macro))
newenvironment = do
  pos <- getPosition
  Tok _ (CtrlSeq mtype) _ <- controlSeq "newenvironment" <|>
                             controlSeq "renewenvironment" <|>
                             controlSeq "provideenvironment"
  withVerbatimMode $ do
    optional $ symbol '*'
    spaces
    name <- untokenize <$> braced
    spaces
    numargs <- option 0 $ try bracketedNum
    spaces
    optarg <- option Nothing $ Just <$> try bracketedToks
    let argspecs = map (\i -> ArgNum i) [1..numargs]
    startcontents <- spaces >> bracedOrToken
    endcontents <- spaces >> bracedOrToken
    macros <- sMacros <$> getState
    case M.lookup name macros of
         Just _
           | mtype == "newenvironment" -> do
               report $ MacroAlreadyDefined name pos
               return Nothing
           | mtype == "provideenvironment" ->
               return Nothing
         _ -> return $ Just (name,
                      Macro ExpandWhenUsed argspecs optarg startcontents,
                      Macro ExpandWhenUsed [] Nothing endcontents)

bracketedNum :: PandocMonad m => LP m Int
bracketedNum = do
  ds <- untokenize <$> bracketedToks
  case safeRead ds of
       Just i -> return i
       _      -> return 0

setCaption :: PandocMonad m => LP m ()
setCaption = try $ do
  skipopts
  ils <- tok
  optional $ try $ spaces *> label
  updateState $ \st -> st{ sCaption = Just ils }

looseItem :: PandocMonad m => LP m Blocks
looseItem = do
  inListItem <- sInListItem <$> getState
  guard $ not inListItem
  skipopts
  return mempty

epigraph :: PandocMonad m => LP m Blocks
epigraph = do
  p1 <- grouped block
  p2 <- grouped block
  return $ divWith ("", ["epigraph"], []) (p1 <> p2)

resetCaption :: PandocMonad m => LP m ()
resetCaption = updateState $ \st -> st{ sCaption   = Nothing
                                      , sLastLabel = Nothing }

section :: PandocMonad m => Attr -> Int -> LP m Blocks
section (ident, classes, kvs) lvl = do
  skipopts
  contents <- grouped inline
  lab <- option ident $
          try (spaces >> controlSeq "label"
               >> spaces >> untokenize <$> braced)
  when (lvl == 0) $
    updateState $ \st -> st{ sHasChapters = True }
  unless ("unnumbered" `elem` classes) $ do
    hn <- sLastHeaderNum <$> getState
    hasChapters <- sHasChapters <$> getState
    let lvl' = lvl + if hasChapters then 1 else 0
    let num = incrementDottedNum lvl' hn
    updateState $ \st -> st{ sLastHeaderNum = num
                           , sLabels = M.insert lab
                              [Str (renderDottedNum num)]
                              (sLabels st) }
  attr' <- registerHeader (lab, classes, kvs) contents
  return $ headerWith attr' lvl contents

blockCommand :: PandocMonad m => LP m Blocks
blockCommand = try $ do
  Tok _ (CtrlSeq name) txt <- anyControlSeq
  guard $ name /= "begin" && name /= "end"
  star <- option "" ("*" <$ symbol '*' <* sp)
  let name' = name <> star
  let names = ordNub [name', name]
  let rawDefiniteBlock = do
        guard $ isBlockCommand name
        rawcontents <- getRawCommand name (txt <> star)
        (guardEnabled Ext_raw_tex >> return (rawBlock "latex" rawcontents))
          <|> ignore rawcontents
  -- heuristic:  if it could be either block or inline, we
  -- treat it if block if we have a sequence of block
  -- commands followed by a newline.  But we stop if we
  -- hit a \startXXX, since this might start a raw ConTeXt
  -- environment (this is important because this parser is
  -- used by the Markdown reader).
  let startCommand = try $ do
        Tok _ (CtrlSeq n) _ <- anyControlSeq
        guard $ "start" `T.isPrefixOf` n
  let rawMaybeBlock = try $ do
        guard $ not $ isInlineCommand name
        rawcontents <- getRawCommand name (txt <> star)
        curr <- (guardEnabled Ext_raw_tex >>
                    return (rawBlock "latex" rawcontents))
                   <|> ignore rawcontents
        rest <- many $ notFollowedBy startCommand *> blockCommand
        lookAhead $ blankline <|> startCommand
        return $ curr <> mconcat rest
  let raw = rawDefiniteBlock <|> rawMaybeBlock
  lookupListDefault raw names blockCommands

closing :: PandocMonad m => LP m Blocks
closing = do
  contents <- tok
  st <- getState
  let extractInlines (MetaBlocks [Plain ys]) = ys
      extractInlines (MetaBlocks [Para ys ]) = ys
      extractInlines _                       = []
  let sigs = case lookupMeta "author" (sMeta st) of
                  Just (MetaList xs) ->
                    para $ trimInlines $ fromList $
                      intercalate [LineBreak] $ map extractInlines xs
                  _ -> mempty
  return $ para (trimInlines contents) <> sigs

parbox :: PandocMonad m => LP m Blocks
parbox = try $ do
  skipopts
  braced -- size
  oldInTableCell <- sInTableCell <$> getState
  -- see #5711
  updateState $ \st -> st{ sInTableCell = False }
  res <- grouped block
  updateState $ \st -> st{ sInTableCell = oldInTableCell }
  return res

blockCommands :: PandocMonad m => M.Map Text (LP m Blocks)
blockCommands = M.fromList
   [ ("par", mempty <$ skipopts)
   , ("parbox",  parbox)
   , ("title", mempty <$ (skipopts *>
                             (grouped inline >>= addMeta "title")
                         <|> (grouped block >>= addMeta "title")))
   , ("subtitle", mempty <$ (skipopts *> tok >>= addMeta "subtitle"))
   , ("author", mempty <$ (skipopts *> authors))
   -- -- in letter class, temp. store address & sig as title, author
   , ("address", mempty <$ (skipopts *> tok >>= addMeta "address"))
   , ("signature", mempty <$ (skipopts *> authors))
   , ("date", mempty <$ (skipopts *> tok >>= addMeta "date"))
   , ("newtheorem", newtheorem)
   , ("theoremstyle", theoremstyle)
   -- KOMA-Script metadata commands
   , ("extratitle", mempty <$ (skipopts *> tok >>= addMeta "extratitle"))
   , ("frontispiece", mempty <$ (skipopts *> tok >>= addMeta "frontispiece"))
   , ("titlehead", mempty <$ (skipopts *> tok >>= addMeta "titlehead"))
   , ("subject", mempty <$ (skipopts *> tok >>= addMeta "subject"))
   , ("publishers", mempty <$ (skipopts *> tok >>= addMeta "publishers"))
   , ("uppertitleback", mempty <$ (skipopts *> tok >>= addMeta "uppertitleback"))
   , ("lowertitleback", mempty <$ (skipopts *> tok >>= addMeta "lowertitleback"))
   , ("dedication", mempty <$ (skipopts *> tok >>= addMeta "dedication"))
   -- sectioning
   , ("part", section nullAttr (-1))
   , ("part*", section nullAttr (-1))
   , ("chapter", section nullAttr 0)
   , ("chapter*", section ("",["unnumbered"],[]) 0)
   , ("section", section nullAttr 1)
   , ("section*", section ("",["unnumbered"],[]) 1)
   , ("subsection", section nullAttr 2)
   , ("subsection*", section ("",["unnumbered"],[]) 2)
   , ("subsubsection", section nullAttr 3)
   , ("subsubsection*", section ("",["unnumbered"],[]) 3)
   , ("paragraph", section nullAttr 4)
   , ("paragraph*", section ("",["unnumbered"],[]) 4)
   , ("subparagraph", section nullAttr 5)
   , ("subparagraph*", section ("",["unnumbered"],[]) 5)
   -- beamer slides
   , ("frametitle", section nullAttr 3)
   , ("framesubtitle", section nullAttr 4)
   -- letters
   , ("opening", (para . trimInlines) <$> (skipopts *> tok))
   , ("closing", skipopts *> closing)
   -- memoir
   , ("plainbreak", braced >> pure horizontalRule)
   , ("plainbreak*", braced >> pure horizontalRule)
   , ("fancybreak", braced >> pure horizontalRule)
   , ("fancybreak*", braced >> pure horizontalRule)
   , ("plainfancybreak", braced >> braced >> braced >> pure horizontalRule)
   , ("plainfancybreak*", braced >> braced >> braced >> pure horizontalRule)
   , ("pfbreak", pure horizontalRule)
   , ("pfbreak*", pure horizontalRule)
   --
   , ("hrule", pure horizontalRule)
   , ("strut", pure mempty)
   , ("rule", skipopts *> tok *> tok *> pure horizontalRule)
   , ("item", looseItem)
   , ("documentclass", skipopts *> braced *> preamble)
   , ("centerline", (para . trimInlines) <$> (skipopts *> tok))
   , ("caption", mempty <$ setCaption)
   , ("bibliography", mempty <$ (skipopts *> braced >>=
         addMeta "bibliography" . splitBibs . untokenize))
   , ("addbibresource", mempty <$ (skipopts *> braced >>=
         addMeta "bibliography" . splitBibs . untokenize))
   , ("endinput", mempty <$ skipMany anyTok)
   -- includes
   , ("lstinputlisting", inputListing)
   , ("inputminted", inputMinted)
   , ("graphicspath", graphicsPath)
   -- polyglossia
   , ("setdefaultlanguage", setDefaultLanguage)
   , ("setmainlanguage", setDefaultLanguage)
   -- hyperlink
   , ("hypertarget", hypertargetBlock)
   -- LaTeX colors
   , ("textcolor", coloredBlock "color")
   , ("colorbox", coloredBlock "background-color")
   -- csquotes
   , ("blockquote", blockquote False Nothing)
   , ("blockcquote", blockquote True Nothing)
   , ("foreignblockquote", braced >>= blockquote False . Just . untokenize)
   , ("foreignblockcquote", braced >>= blockquote True . Just . untokenize)
   , ("hyphenblockquote", braced >>= blockquote False . Just . untokenize)
   , ("hyphenblockcquote", braced >>= blockquote True . Just . untokenize)
   -- include
   , ("include", rawBlockOr "include" $ include "include")
   , ("input", rawBlockOr "input" $ include "input")
   , ("subfile", rawBlockOr "subfile" doSubfile)
   , ("usepackage", rawBlockOr "usepackage" $ include "usepackage")
   -- preamble
   , ("PackageError", mempty <$ (braced >> braced >> braced))
   -- epigraph package
   , ("epigraph", epigraph)
   ]


environments :: PandocMonad m => M.Map Text (LP m Blocks)
environments = M.fromList
   [ ("document", env "document" blocks <* skipMany anyTok)
   , ("abstract", mempty <$ (env "abstract" blocks >>= addMeta "abstract"))
   , ("sloppypar", env "sloppypar" $ blocks)
   , ("letter", env "letter" letterContents)
   , ("minipage", env "minipage" $
          skipopts *> spaces *> optional braced *> spaces *> blocks)
   , ("figure", env "figure" $ skipopts *> figure)
   , ("subfigure", env "subfigure" $ skipopts *> tok *> figure)
   , ("center", env "center" blocks)
   , ("longtable",  env "longtable" $
          resetCaption *> simpTable "longtable" False >>= addTableCaption)
   , ("table",  env "table" $
          skipopts *> resetCaption *> blocks >>= addTableCaption)
   , ("tabular*", env "tabular*" $ simpTable "tabular*" True)
   , ("tabularx", env "tabularx" $ simpTable "tabularx" True)
   , ("tabular", env "tabular"  $ simpTable "tabular" False)
   , ("quote", blockQuote <$> env "quote" blocks)
   , ("quotation", blockQuote <$> env "quotation" blocks)
   , ("verse", blockQuote <$> env "verse" blocks)
   , ("itemize", bulletList <$> listenv "itemize" (many item))
   , ("description", definitionList <$> listenv "description" (many descItem))
   , ("enumerate", orderedList')
   , ("alltt", alltt <$> env "alltt" blocks)
   , ("code", guardEnabled Ext_literate_haskell *>
       (codeBlockWith ("",["haskell","literate"],[]) <$> verbEnv "code"))
   , ("comment", mempty <$ verbEnv "comment")
   , ("verbatim", codeBlock <$> verbEnv "verbatim")
   , ("Verbatim", fancyverbEnv "Verbatim")
   , ("BVerbatim", fancyverbEnv "BVerbatim")
   , ("lstlisting", do attr <- parseListingsOptions <$> option [] keyvals
                       codeBlockWith attr <$> verbEnv "lstlisting")
   , ("minted", minted)
   , ("obeylines", obeylines)
   , ("tikzpicture", rawVerbEnv "tikzpicture")
   , ("tikzcd", rawVerbEnv "tikzcd")
   , ("lilypond", rawVerbEnv "lilypond")
   , ("ly", rawVerbEnv "ly")
   -- amsthm
   , ("proof", proof)
   -- etoolbox
   , ("ifstrequal", ifstrequal)
   , ("newtoggle", braced >>= newToggle)
   , ("toggletrue", braced >>= setToggle True)
   , ("togglefalse", braced >>= setToggle False)
   , ("iftoggle", try $ ifToggle >> block)
   ]

theoremstyle :: PandocMonad m => LP m Blocks
theoremstyle = do
  stylename <- untokenize <$> braced
  let mbstyle = case stylename of
                  "plain"      -> Just PlainStyle
                  "definition" -> Just DefinitionStyle
                  "remark"     -> Just RemarkStyle
                  _            -> Nothing
  case mbstyle of
    Nothing  -> return ()
    Just sty -> updateState $ \s -> s{ sLastTheoremStyle = sty }
  return mempty

newtheorem :: PandocMonad m => LP m Blocks
newtheorem = do
  number <- option True (False <$ symbol '*' <* sp)
  name <- untokenize <$> braced
  sp
  series <- option Nothing $ Just . untokenize <$> bracketedToks
  sp
  showName <- untokenize <$> braced
  sp
  syncTo <- option Nothing $ Just . untokenize <$> bracketedToks
  sty <- sLastTheoremStyle <$> getState
  let spec = TheoremSpec { theoremName = showName
                         , theoremStyle = sty
                         , theoremSeries = series
                         , theoremSyncTo = syncTo
                         , theoremNumber = number
                         , theoremLastNum = DottedNum [0] }
  tmap <- sTheoremMap <$> getState
  updateState $ \s -> s{ sTheoremMap =
                            M.insert name spec tmap }
  return mempty

proof :: PandocMonad m => LP m Blocks
proof = do
  title <- option (B.text "Proof") opt
  bs <- env "proof" blocks
  return $
    B.divWith ("", ["proof"], []) $
      addQed $ addTitle (B.emph (title <> ".")) $ bs

addTitle :: Inlines -> Blocks -> Blocks
addTitle ils bs =
  case B.toList bs of
    (Para xs : rest)
      -> B.fromList (Para (B.toList ils ++ (Space : xs)) : rest)
    _ -> B.para ils <> bs

addQed :: Blocks -> Blocks
addQed bs =
  case Seq.viewr (B.unMany bs) of
    s Seq.:> Para ils
      -> B.Many (s Seq.|> Para (ils ++ B.toList qedSign))
    _ -> bs <> B.para qedSign
 where
  qedSign = B.str "\xa0\x25FB"

environment :: PandocMonad m => LP m Blocks
environment = try $ do
  controlSeq "begin"
  name <- untokenize <$> braced
  M.findWithDefault mzero name environments <|>
    theoremEnvironment name <|>
    if M.member name (inlineEnvironments
                       :: M.Map Text (LP PandocPure Inlines))
       then mzero
       else try (rawEnv name) <|> rawVerbEnv name

theoremEnvironment :: PandocMonad m => Text -> LP m Blocks
theoremEnvironment name = do
  tmap <- sTheoremMap <$> getState
  case M.lookup name tmap of
    Nothing -> mzero
    Just tspec -> do
       optTitle <- option mempty $ (\x -> space <> "(" <> x <> ")") <$> opt
       mblabel <- option Nothing $ Just . untokenize <$>
                   try (spaces >> controlSeq "label" >> spaces >> braced)
       bs <- env name blocks
       number <-
         if theoremNumber tspec
            then do
               let name' = fromMaybe name $ theoremSeries tspec
               num <- getNextNumber
                   (fromMaybe (DottedNum [0]) .
                    fmap theoremLastNum .
                    M.lookup name' . sTheoremMap)
               updateState $ \s ->
                 s{ sTheoremMap =
                       M.adjust
                       (\spec -> spec{ theoremLastNum = num })
                       name'
                       (sTheoremMap s)
                  }

               case mblabel of
                 Just ident ->
                   updateState $ \s ->
                     s{ sLabels = M.insert ident
                         [Str (theoremName tspec), Str "\160",
                          Str (renderDottedNum num)] (sLabels s) }
                 Nothing -> return ()
               return $ space <> B.text (renderDottedNum num)
            else return mempty
       let titleEmph = case theoremStyle tspec of
                         PlainStyle      -> B.strong
                         DefinitionStyle -> B.strong
                         RemarkStyle     -> B.emph
       let title = titleEmph (B.text (theoremName tspec) <> number)
                                      <> optTitle <> "." <> space
       return $ divWith (fromMaybe "" mblabel, [name], []) $ addTitle title
              $ case theoremStyle tspec of
                  PlainStyle -> walk italicize bs
                  _          -> bs

italicize :: Block -> Block
italicize (Para ils) = Para [Emph ils]
italicize (Plain ils) = Plain [Emph ils]
italicize x = x

env :: PandocMonad m => Text -> LP m a -> LP m a
env name p = p <* end_ name

rawEnv :: PandocMonad m => Text -> LP m Blocks
rawEnv name = do
  exts <- getOption readerExtensions
  let parseRaw = extensionEnabled Ext_raw_tex exts
  rawOptions <- mconcat <$> many rawopt
  let beginCommand = "\\begin{" <> name <> "}" <> rawOptions
  pos1 <- getPosition
  (bs, raw) <- withRaw $ env name blocks
  if parseRaw
     then return $ rawBlock "latex"
                 $ beginCommand <> untokenize raw
     else do
       report $ SkippedContent beginCommand pos1
       pos2 <- getPosition
       report $ SkippedContent ("\\end{" <> name <> "}") pos2
       return bs

rawVerbEnv :: PandocMonad m => Text -> LP m Blocks
rawVerbEnv name = do
  pos <- getPosition
  (_, raw) <- withRaw $ verbEnv name
  let raw' = "\\begin{" <> name <> "}" <> untokenize raw
  exts <- getOption readerExtensions
  let parseRaw = extensionEnabled Ext_raw_tex exts
  if parseRaw
     then return $ rawBlock "latex" raw'
     else do
       report $ SkippedContent raw' pos
       return mempty

fancyverbEnv :: PandocMonad m => Text -> LP m Blocks
fancyverbEnv name = do
  options <- option [] keyvals
  let kvs = [ (if k == "firstnumber"
                  then "startFrom"
                  else k, v) | (k,v) <- options ]
  let classes = [ "numberLines" |
                  lookup "numbers" options == Just "left" ]
  let attr = ("",classes,kvs)
  codeBlockWith attr <$> verbEnv name

obeylines :: PandocMonad m => LP m Blocks
obeylines =
  para . fromList . removeLeadingTrailingBreaks .
   walk softBreakToHard . toList <$> env "obeylines" inlines
  where softBreakToHard SoftBreak = LineBreak
        softBreakToHard x         = x
        removeLeadingTrailingBreaks = reverse . dropWhile isLineBreak .
                                      reverse . dropWhile isLineBreak
        isLineBreak LineBreak = True
        isLineBreak _         = False

minted :: PandocMonad m => LP m Blocks
minted = do
  attr <- mintedAttr
  codeBlockWith attr <$> verbEnv "minted"

mintedAttr :: PandocMonad m => LP m Attr
mintedAttr = do
  options <- option [] keyvals
  lang <- untokenize <$> braced
  let kvs = [ (if k == "firstnumber"
                  then "startFrom"
                  else k, v) | (k,v) <- options ]
  let classes = [ lang | not (T.null lang) ] ++
                [ "numberLines" |
                  lookup "linenos" options == Just "true" ]
  return ("",classes,kvs)

inputMinted :: PandocMonad m => LP m Blocks
inputMinted = do
  pos <- getPosition
  attr <- mintedAttr
  f <- T.filter (/='"') . untokenize <$> braced
  dirs <- (map T.unpack . splitTextBy (==':') . fromMaybe ".") <$> lookupEnv "TEXINPUTS"
  mbCode <- readFileFromDirs dirs (T.unpack f)
  rawcode <- case mbCode of
                  Just s -> return s
                  Nothing -> do
                    report $ CouldNotLoadIncludeFile f pos
                    return ""
  return $ B.codeBlockWith attr rawcode

letterContents :: PandocMonad m => LP m Blocks
letterContents = do
  bs <- blocks
  st <- getState
  -- add signature (author) and address (title)
  let addr = case lookupMeta "address" (sMeta st) of
                  Just (MetaBlocks [Plain xs]) ->
                     para $ trimInlines $ fromList xs
                  _ -> mempty
  return $ addr <> bs -- sig added by \closing

figure :: PandocMonad m => LP m Blocks
figure = try $ do
  resetCaption
  blocks >>= addImageCaption

addImageCaption :: PandocMonad m => Blocks -> LP m Blocks
addImageCaption = walkM go
  where go (Image attr@(_, cls, kvs) alt (src,tit))
            | not ("fig:" `T.isPrefixOf` tit) = do
          st <- getState
          let (alt', tit') = case sCaption st of
                               Just ils -> (toList ils, "fig:" <> tit)
                               Nothing  -> (alt, tit)
              attr' = case sLastLabel st of
                        Just lab -> (lab, cls, kvs)
                        Nothing  -> attr
          case attr' of
               ("", _, _)    -> return ()
               (ident, _, _) -> do
                  num <- getNextNumber sLastFigureNum
                  setState
                    st{ sLastFigureNum = num
                      , sLabels = M.insert ident
                                 [Str (renderDottedNum num)] (sLabels st) }
          return $ Image attr' alt' (src, tit')
        go x = return x

getNextNumber :: Monad m
              => (LaTeXState -> DottedNum) -> LP m DottedNum
getNextNumber getCurrentNum = do
  st <- getState
  let chapnum =
        case sLastHeaderNum st of
             DottedNum (n:_) | sHasChapters st -> Just n
             _                                 -> Nothing
  return . DottedNum $
    case getCurrentNum st of
       DottedNum [m,n]  ->
         case chapnum of
              Just m' | m' == m   -> [m, n+1]
                      | otherwise -> [m', 1]
              Nothing             -> [1]
                                      -- shouldn't happen
       DottedNum [n]   ->
         case chapnum of
              Just m  -> [m, 1]
              Nothing -> [n + 1]
       _               ->
         case chapnum of
               Just n  -> [n, 1]
               Nothing -> [1]


coloredBlock :: PandocMonad m => Text -> LP m Blocks
coloredBlock stylename = try $ do
  skipopts
  color <- braced
  notFollowedBy (grouped inline)
  let constructor = divWith ("",[],[("style",stylename <> ": " <> untokenize color)])
  constructor <$> grouped block

graphicsPath :: PandocMonad m => LP m Blocks
graphicsPath = do
  ps <- map (T.unpack . untokenize) <$>
          (bgroup *> spaces *> manyTill (braced <* spaces) egroup)
  getResourcePath >>= setResourcePath . (<> ps)
  return mempty

splitBibs :: Text -> [Inlines]
splitBibs = map (str . T.pack . flip replaceExtension "bib" . T.unpack . trim) . splitTextBy (==',')

alltt :: Blocks -> Blocks
alltt = walk strToCode
  where strToCode (Str s)   = Code nullAttr s
        strToCode Space     = RawInline (Format "latex") "\\ "
        strToCode SoftBreak = LineBreak
        strToCode x         = x

parseListingsOptions :: [(Text, Text)] -> Attr
parseListingsOptions options =
  let kvs = [ (if k == "firstnumber"
                  then "startFrom"
                  else k, v) | (k,v) <- options ]
      classes = [ "numberLines" |
                  lookup "numbers" options == Just "left" ]
             ++ maybeToList (listingsLanguage options)
  in  (fromMaybe "" (lookup "label" options), classes, kvs)

inputListing :: PandocMonad m => LP m Blocks
inputListing = do
  pos <- getPosition
  options <- option [] keyvals
  f <- T.filter (/='"') . untokenize <$> braced
  dirs <- (map T.unpack . splitTextBy (==':') . fromMaybe ".") <$> lookupEnv "TEXINPUTS"
  mbCode <- readFileFromDirs dirs (T.unpack f)
  codeLines <- case mbCode of
                      Just s -> return $ T.lines s
                      Nothing -> do
                        report $ CouldNotLoadIncludeFile f pos
                        return []
  let (ident,classes,kvs) = parseListingsOptions options
  let classes' =
        (case listingsLanguage options of
           Nothing -> (take 1 (languagesByExtension (T.pack $ takeExtension $ T.unpack f)) <>)
           Just _  -> id) classes
  let firstline = fromMaybe 1 $ lookup "firstline" options >>= safeRead
  let lastline = fromMaybe (length codeLines) $
                       lookup "lastline" options >>= safeRead
  let codeContents = T.intercalate "\n" $ take (1 + lastline - firstline) $
                       drop (firstline - 1) codeLines
  return $ codeBlockWith (ident,classes',kvs) codeContents

-- lists

item :: PandocMonad m => LP m Blocks
item = void blocks *> controlSeq "item" *> skipopts *> blocks

descItem :: PandocMonad m => LP m (Inlines, [Blocks])
descItem = do
  blocks -- skip blocks before item
  controlSeq "item"
  sp
  ils <- opt
  bs <- blocks
  return (ils, [bs])

listenv :: PandocMonad m => Text -> LP m a -> LP m a
listenv name p = try $ do
  oldInListItem <- sInListItem `fmap` getState
  updateState $ \st -> st{ sInListItem = True }
  res <- env name p
  updateState $ \st -> st{ sInListItem = oldInListItem }
  return res

orderedList' :: PandocMonad m => LP m Blocks
orderedList' = try $ do
  spaces
  let markerSpec = do
        symbol '['
        ts <- untokenize <$> manyTill anyTok (symbol ']')
        case runParser anyOrderedListMarker def "option" ts of
             Right r -> return r
             Left _  -> do
               pos <- getPosition
               report $ SkippedContent ("[" <> ts <> "]") pos
               return (1, DefaultStyle, DefaultDelim)
  (_, style, delim) <- option (1, DefaultStyle, DefaultDelim) markerSpec
  spaces
  optional $ try $ controlSeq "setlength"
                   *> grouped (count 1 $ controlSeq "itemindent")
                   *> braced
  spaces
  start <- option 1 $ try $ do pos <- getPosition
                               controlSeq "setcounter"
                               ctr <- untokenize <$> braced
                               guard $ "enum" `T.isPrefixOf` ctr
                               guard $ T.all (`elem` ['i','v']) (T.drop 4 ctr)
                               sp
                               num <- untokenize <$> braced
                               case safeRead num of
                                    Just i -> return (i + 1 :: Int)
                                    Nothing -> do
                                      report $ SkippedContent
                                        ("\\setcounter{" <> ctr <>
                                         "}{" <> num <> "}") pos
                                      return 1
  bs <- listenv "enumerate" (many item)
  return $ orderedListWith (start, style, delim) bs

-- tables

hline :: PandocMonad m => LP m ()
hline = try $ do
  spaces
  controlSeq "hline" <|>
    -- booktabs rules:
    controlSeq "toprule" <|>
    controlSeq "bottomrule" <|>
    controlSeq "midrule" <|>
    controlSeq "endhead" <|>
    controlSeq "endfirsthead"
  spaces
  optional opt
  return ()

lbreak :: PandocMonad m => LP m Tok
lbreak = (controlSeq "\\" <|> controlSeq "tabularnewline")
         <* skipopts <* spaces

amp :: PandocMonad m => LP m Tok
amp = symbol '&'

-- Split a Word into individual Symbols (for parseAligns)
splitWordTok :: PandocMonad m => LP m ()
splitWordTok = do
  inp <- getInput
  case inp of
       (Tok spos Word t : rest) ->
         setInput $ map (Tok spos Symbol . T.singleton) (T.unpack t) <> rest
       _ -> return ()

parseAligns :: PandocMonad m => LP m [(Alignment, ColWidth, ([Tok], [Tok]))]
parseAligns = try $ do
  let maybeBar = skipMany
        (try $ sp *> (() <$ symbol '|' <|> () <$ (symbol '@' >> braced)))
  let cAlign = AlignCenter <$ symbol 'c'
  let lAlign = AlignLeft <$ symbol 'l'
  let rAlign = AlignRight <$ symbol 'r'
  let parAlign = AlignLeft <$ symbol 'p'
  -- aligns from tabularx
  let xAlign = AlignLeft <$ symbol 'X'
  let mAlign = AlignLeft <$ symbol 'm'
  let bAlign = AlignLeft <$ symbol 'b'
  let alignChar = splitWordTok *> (  cAlign <|> lAlign <|> rAlign <|> parAlign
                                 <|> xAlign <|> mAlign <|> bAlign )
  let alignPrefix = symbol '>' >> braced
  let alignSuffix = symbol '<' >> braced
  let colWidth = try $ do
        symbol '{'
        ds <- trim . untokenize <$> manyTill anyTok (controlSeq "linewidth")
        spaces
        symbol '}'
        return $ safeRead ds
  let alignSpec = do
        pref <- option [] alignPrefix
        spaces
        al <- alignChar
        width <- colWidth <|> option Nothing (do s <- untokenize <$> braced
                                                 pos <- getPosition
                                                 report $ SkippedContent s pos
                                                 return Nothing)
        spaces
        suff <- option [] alignSuffix
        return (al, width, (pref, suff))
  let starAlign = do -- '*{2}{r}' == 'rr', we just expand like a macro
        symbol '*'
        spaces
        ds <- trim . untokenize <$> braced
        spaces
        spec <- braced
        case safeRead ds of
             Just n  ->
               getInput >>= setInput . (mconcat (replicate n spec) ++)
             Nothing -> Prelude.fail $ "Could not parse " <> T.unpack ds <> " as number"
  bgroup
  spaces
  maybeBar
  aligns' <- many $ try $ spaces >> optional starAlign >>
                            (alignSpec <* maybeBar)
  spaces
  egroup
  spaces
  return $ map toSpec aligns'
  where
    toColWidth (Just w) | w > 0 = ColWidth w
    toColWidth _                = ColWidthDefault
    toSpec (x, y, z) = (x, toColWidth y, z)

parseTableRow :: PandocMonad m
              => Text   -- ^ table environment name
              -> [([Tok], [Tok])] -- ^ pref/suffixes
              -> LP m Row
parseTableRow envname prefsufs = do
  notFollowedBy (spaces *> end_ envname)
  -- add prefixes and suffixes in token stream:
  let celltoks (pref, suff) = do
        prefpos <- getPosition
        contents <- mconcat <$>
            many ( snd <$> withRaw (controlSeq "parbox" >> parbox) -- #5711
                  <|>
                   (do notFollowedBy
                         (() <$ amp <|> () <$ lbreak <|> end_ envname)
                       count 1 anyTok) )

        suffpos <- getPosition
        option [] (count 1 amp)
        return $ map (setpos prefpos) pref ++ contents ++ map (setpos suffpos) suff
  rawcells <- mapM celltoks prefsufs
  oldInput <- getInput
  cells <- mapM (\ts -> setInput ts >> parseTableCell) rawcells
  setInput oldInput
  spaces
  return $ Row nullAttr cells 

parseTableCell :: PandocMonad m => LP m Cell
parseTableCell = do
  updateState $ \st -> st{ sInTableCell = True }
  cell' <- parseMultiCell <|> parseSimpleCell
  updateState $ \st -> st{ sInTableCell = False }
  return cell'

cellAlignment :: PandocMonad m => LP m Alignment
cellAlignment = skipMany (symbol '|') *> alignment <* skipMany (symbol '|')
  where
    alignment = do
      c <- untoken <$> singleChar
      return $ case c of
        "l" -> AlignLeft
        "r" -> AlignRight
        "c" -> AlignCenter
        "*" -> AlignDefault
        _   -> AlignDefault

plainify :: Blocks -> Blocks
plainify bs = case toList bs of
                [Para ils] -> plain (fromList ils)
                _          -> bs

parseMultiCell :: PandocMonad m => LP m Cell
parseMultiCell =   (controlSeq "multirow"    >> parseMultirowCell) 
               <|> (controlSeq "multicolumn" >> parseMulticolCell)
  where
    parseMultirowCell = parseMultiXCell RowSpan (const $ ColSpan 1)
    parseMulticolCell = parseMultiXCell (const $ RowSpan 1) ColSpan

    parseMultiXCell rowspanf colspanf = do
      span' <- fmap (fromMaybe 1 . safeRead . untokenize) braced
      alignment <- symbol '{' *> cellAlignment <* symbol '}'

      -- Two possible contents: either a nested \multirow/\multicol, or content.
      -- E.g. \multirow{1}{c}{\multicol{1}{c}{content}}
      let singleCell = do
            content <- plainify <$> blocks
            return $ cell alignment (rowspanf span') (colspanf span') content

      let nestedCell = do
            (Cell _ _ (RowSpan rs) (ColSpan cs) bs) <- parseMultiCell
            return $ cell
                      alignment
                      (RowSpan $ max span' rs)
                      (ColSpan $ max span' cs)
                      (fromList bs)

      symbol '{' *> (nestedCell <|> singleCell) <* symbol '}'

-- Parse a simple cell, i.e. not multirow/multicol
parseSimpleCell :: PandocMonad m => LP m Cell
parseSimpleCell = simpleCell <$> (plainify <$> blocks)

simpTable :: PandocMonad m => Text -> Bool -> LP m Blocks
simpTable envname hasWidthParameter = try $ do
  when hasWidthParameter $ () <$ (spaces >> tok)
  skipopts
  colspecs <- parseAligns
  let (aligns, widths, prefsufs) = unzip3 colspecs
  optional $ controlSeq "caption" *> setCaption
  spaces
  optional label
  spaces
  optional lbreak
  spaces
  skipMany hline
  spaces
  header' <- option [] . try . fmap (:[]) $
             parseTableRow envname prefsufs <* lbreak <* many1 hline
  spaces
  rows <- sepEndBy (parseTableRow envname prefsufs)
                    (lbreak <* optional (skipMany hline))
  spaces
  optional $ controlSeq "caption" *> setCaption
  spaces
  optional label
  spaces
  optional lbreak
  spaces
  lookAhead $ controlSeq "end" -- make sure we're at end
  return $ table emptyCaption
                 (zip aligns widths)
                 (TableHead nullAttr $ header')
                 [TableBody nullAttr 0 [] rows]
                 (TableFoot nullAttr [])

addTableCaption :: PandocMonad m => Blocks -> LP m Blocks
addTableCaption = walkM go
  where go (Table attr c spec th tb tf) = do
          st <- getState
          let mblabel = sLastLabel st
          capt <- case (sCaption st, mblabel) of
                   (Just ils, Nothing)  -> return $ caption Nothing (plain ils)
                   (Just ils, Just lab) -> do
                     num <- getNextNumber sLastTableNum
                     setState
                       st{ sLastTableNum = num
                         , sLabels = M.insert lab
                                    [Str (renderDottedNum num)]
                                    (sLabels st) }
                     return $ caption Nothing (plain ils) -- add number??
                   (Nothing, _)  -> return c
          let attr' = case (attr, mblabel) of
                        ((_,classes,kvs), Just ident) ->
                           (ident,classes,kvs)
                        _ -> attr
          return $ addAttrDiv attr' $ Table nullAttr capt spec th tb tf
        go x = return x

-- TODO: For now we add a Div to contain table attributes, since
-- most writers don't do anything yet with attributes on Table.
-- This can be removed when that changes.
addAttrDiv :: Attr -> Block -> Block
addAttrDiv ("",[],[]) b = b
addAttrDiv attr b       = Div attr [b]

block :: PandocMonad m => LP m Blocks
block = do
  res <- (mempty <$ spaces1)
    <|> environment
    <|> macroDef (rawBlock "latex")
    <|> blockCommand
    <|> paragraph
    <|> grouped block
  trace (T.take 60 $ tshow $ B.toList res)
  return res

blocks :: PandocMonad m => LP m Blocks
blocks = mconcat <$> many block

setDefaultLanguage :: PandocMonad m => LP m Blocks
setDefaultLanguage = do
  o <- option "" $ T.filter (\c -> c /= '[' && c /= ']')
                <$> rawopt
  polylang <- untokenize <$> braced
  case M.lookup polylang polyglossiaLangToBCP47 of
       Nothing -> return mempty -- TODO mzero? warning?
       Just langFunc -> do
         let l = langFunc o
         setTranslations l
         updateState $ setMeta "lang" $ str (renderLang l)
         return mempty
