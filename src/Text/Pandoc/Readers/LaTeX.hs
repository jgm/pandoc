{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{- |
   Module      : Text.Pandoc.Readers.LaTeX
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
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

import Prelude
import Control.Applicative (many, optional, (<|>))
import Control.Monad
import Control.Monad.Except (throwError)
import Data.Char (isDigit, isLetter, toLower, toUpper)
import Data.Default
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Safe (minimumDef)
import System.FilePath (addExtension, replaceExtension, takeExtension)
import Text.Pandoc.BCP47 (Lang (..), renderLang)
import Text.Pandoc.Builder
import Text.Pandoc.Class (PandocMonad, PandocPure, getResourcePath, lookupEnv,
                          readFileFromDirs, report, setResourcePath,
                          setTranslations, translateTerm, trace)
import Text.Pandoc.Error (PandocError ( PandocParseError, PandocParsecError))
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

-- for debugging:
-- import Text.Pandoc.Extensions (getDefaultExtensions)
-- import Text.Pandoc.Class (runIOorExplode, PandocIO)
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
    Left e       -> throwError $ PandocParsecError (T.unpack ltx) e

parseLaTeX :: PandocMonad m => LP m Pandoc
parseLaTeX = do
  bs <- blocks
  eof
  st <- getState
  let meta = sMeta st
  let doc' = doc bs
  let headerLevel (Header n _ _) = [n]
      headerLevel _              = []
  let bottomLevel = minimumDef 1 $ query headerLevel doc'
  let adjustHeaders m (Header n attr ils) = Header (n+m) attr ils
      adjustHeaders _ x                   = x
  let (Pandoc _ bs') =
       -- handle the case where you have \part or \chapter
       (if bottomLevel < 1
           then walk (adjustHeaders (1 - bottomLevel))
           else id) $
       walk (resolveRefs (sLabels st)) doc'
  return $ Pandoc meta bs'

resolveRefs :: M.Map String [Inline] -> Inline -> Inline
resolveRefs labels x@(Link (ident,classes,kvs) _ _) =
  case (lookup "reference-type" kvs,
        lookup "reference" kvs) of
        (Just "ref", Just lab) ->
          case M.lookup lab labels of
               Just txt -> Link (ident,classes,kvs) txt ('#':lab, "")
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
              => ParserT String s m String
rawLaTeXBlock = do
  lookAhead (try (char '\\' >> letter))
  snd <$> (rawLaTeXParser False macroDef blocks
      <|> (rawLaTeXParser True
             (do choice (map controlSeq
                   ["include", "input", "subfile", "usepackage"])
                 skipMany opt
                 braced
                 return mempty) blocks)
      <|> rawLaTeXParser True
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
                    (T.unpack (txt <> untokenize rawargs))

rawLaTeXInline :: (PandocMonad m, HasMacros s, HasReaderOptions s)
               => ParserT String s m String
rawLaTeXInline = do
  lookAhead (try (char '\\' >> letter))
  snd <$> (  rawLaTeXParser True
              (mempty <$ (controlSeq "input" >> skipMany opt >> braced))
              inlines
        <|> rawLaTeXParser True (inlineEnvironment <|> inlineCommand') inlines)

inlineCommand :: PandocMonad m => ParserT String ParserState m Inlines
inlineCommand = do
  lookAhead (try (char '\\' >> letter))
  fst <$> rawLaTeXParser True (inlineEnvironment <|> inlineCommand') inlines

-- inline elements:

word :: PandocMonad m => LP m Inlines
word = (str . T.unpack . untoken) <$> satisfyTok isWordTok

regularSymbol :: PandocMonad m => LP m Inlines
regularSymbol = (str . T.unpack . untoken) <$> satisfyTok isRegularSymbol
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
  (codeWith ("",["haskell"],[]) . T.unpack . untokenize)
    <$> manyTill (satisfyTok (not . isNewlineTok)) (symbol '|')

mkImage :: PandocMonad m => [(String, String)] -> String -> LP m Inlines
mkImage options src = do
   let replaceTextwidth (k,v) =
         case numUnit v of
              Just (num, "\\textwidth") -> (k, showFl (num * 100) ++ "%")
              _                         -> (k, v)
   let kvs = map replaceTextwidth
             $ filter (\(k,_) -> k `elem` ["width", "height"]) options
   let attr = ("",[], kvs)
   let alt = str "image"
   case takeExtension src of
        "" -> do
              defaultExt <- getOption readerDefaultImageExtension
              return $ imageWith attr (addExtension src defaultExt) "" alt
        _  -> return $ imageWith attr src "" alt

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

lit :: String -> LP m Inlines
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
  startchs <- (T.unpack . untokenize) <$> starter
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
  let lang = (T.unpack <$> mblang) >>= babelLangToBCP47
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
  let lang = (T.unpack <$> mblang) >>= babelLangToBCP47
  let langdiv = case lang of
                      Nothing -> id
                      Just l  -> divWith ("",[],[("lang", renderLang l)])
  bs <- grouped block
  return $ blockQuote . langdiv $ (bs <> citePar)

doAcronym :: PandocMonad m => String -> LP m Inlines
doAcronym form = do
  acro <- braced
  return . mconcat $ [spanWith ("",[],[("acronym-label", toksToString acro),
    ("acronym-form", "singular+" ++ form)])
    $ str $ toksToString acro]

doAcronymPlural :: PandocMonad m => String -> LP m Inlines
doAcronymPlural form = do
  acro <- braced
  plural <- lit "s"
  return . mconcat $ [spanWith ("",[],[("acronym-label", toksToString acro),
    ("acronym-form", "plural+" ++ form)]) $
   mconcat [str $ toksToString acro, plural]]

doverb :: PandocMonad m => LP m Inlines
doverb = do
  Tok _ Symbol t <- anySymbol
  marker <- case T.uncons t of
              Just (c, ts) | T.null ts -> return c
              _            -> mzero
  withVerbatimMode $
    (code . T.unpack . untokenize) <$>
      manyTill (verbTok marker) (symbol marker)

verbTok :: PandocMonad m => Char -> LP m Tok
verbTok stopchar = do
  t@(Tok pos toktype txt) <- satisfyTok (not . isNewlineTok)
  case T.findIndex (== stopchar) txt of
       Nothing -> return t
       Just i  -> do
         let (t1, t2) = T.splitAt i txt
         inp <- getInput
         setInput $ Tok (incSourceColumn pos i) Symbol (T.singleton stopchar)
                  : totoks (incSourceColumn pos (i + 1)) (T.drop 1 t2) ++ inp
         return $ Tok pos toktype t1

dolstinline :: PandocMonad m => LP m Inlines
dolstinline = do
  options <- option [] keyvals
  let classes = maybeToList $ lookup "language" options >>= fromListingsLanguage
  doinlinecode classes

domintinline :: PandocMonad m => LP m Inlines
domintinline = do
  skipopts
  cls <- toksToString <$> braced
  doinlinecode [cls]

doinlinecode :: PandocMonad m => [String] -> LP m Inlines
doinlinecode classes = do
  Tok _ Symbol t <- anySymbol
  marker <- case T.uncons t of
              Just (c, ts) | T.null ts -> return c
              _            -> mzero
  let stopchar = if marker == '{' then '}' else marker
  withVerbatimMode $
    (codeWith ("",classes,[]) . T.unpack . untokenize) <$>
      manyTill (verbTok stopchar) (symbol stopchar)

keyval :: PandocMonad m => LP m (String, String)
keyval = try $ do
  Tok _ Word key <- satisfyTok isWordTok
  optional sp
  val <- option mempty $ do
           symbol '='
           optional sp
           (untokenize <$> braced) <|>
             (mconcat <$> many1 (
                 (untokenize . snd <$> withRaw braced)
                 <|>
                 (untokenize <$> (many1
                      (satisfyTok
                         (\t -> case t of
                                Tok _ Symbol "]" -> False
                                Tok _ Symbol "," -> False
                                Tok _ Symbol "{" -> False
                                Tok _ Symbol "}" -> False
                                _                -> True))))))
  optional (symbol ',')
  optional sp
  return (T.unpack key, T.unpack $ T.strip val)

keyvals :: PandocMonad m => LP m [(String, String)]
keyvals = try $ symbol '[' >> manyTill keyval (symbol ']')

accent :: PandocMonad m => Char -> Maybe Char -> LP m Inlines
accent combiningAccent fallBack = try $ do
  ils <- tok
  case toList ils of
       (Str (x:xs) : ys) -> return $ fromList $
         -- try to normalize to the combined character:
         Str (T.unpack
           (Normalize.normalize Normalize.NFC
             (T.pack [x, combiningAccent])) ++ xs) : ys
       [Space]           -> return $ str [fromMaybe combiningAccent fallBack]
       []                -> return $ str [fromMaybe combiningAccent fallBack]
       _                 -> return ils
mathDisplay :: String -> Inlines
mathDisplay = displayMath . trimMath

mathInline :: String -> Inlines
mathInline = math . trimMath

dollarsMath :: PandocMonad m => LP m Inlines
dollarsMath = do
  symbol '$'
  display <- option False (True <$ symbol '$')
  (do contents <- try $ T.unpack <$> pDollarsMath 0
      if display
         then (mathDisplay contents <$ symbol '$')
         else return $ mathInline contents)
   <|> (guard display >> return (mathInline ""))

-- Int is number of embedded groupings
pDollarsMath :: PandocMonad m => Int -> LP m Text
pDollarsMath n = do
  Tok _ toktype t <- anyTok
  case toktype of
       Symbol | t == "$"
              , n == 0 -> return mempty
              | t == "\\" -> do
                  Tok _ _ t' <- anyTok
                  return (t <> t')
              | t == "{" -> (t <>) <$> pDollarsMath (n+1)
              | t == "}" ->
                if n > 0
                then (t <>) <$> pDollarsMath (n-1)
                else mzero
       _ -> (t <>) <$> pDollarsMath n

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

citationLabel :: PandocMonad m => LP m String
citationLabel  = do
  optional spaces
  toksToString <$>
    (many1 (satisfyTok isWordTok <|> symbolIn bibtexKeyChar)
          <* optional spaces
          <* optional (symbol ',')
          <* optional spaces)
  where bibtexKeyChar = ".:;?!`'()/*@_+=-[]" :: [Char]

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

citation :: PandocMonad m => String -> CitationMode -> Bool -> LP m Inlines
citation name mode multi = do
  (c,raw) <- withRaw $ cites mode multi
  return $ cite c (rawInline "latex" $ "\\" ++ name ++ toksToString raw)

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
                      (rawInline "latex" $ "\\citetext" ++ toksToString raw)

inNote :: Inlines -> Inlines
inNote ils =
  note $ para $ ils <> str "."

inlineCommand' :: PandocMonad m => LP m Inlines
inlineCommand' = try $ do
  Tok _ (CtrlSeq name) cmd <- anyControlSeq
  guard $ name /= "begin" && name /= "end"
  star <- option "" ("*" <$ symbol '*' <* optional sp)
  let name' = name <> star
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
          return (str (T.unpack t))

opt :: PandocMonad m => LP m Inlines
opt = bracketed inline <|> (str . T.unpack <$> rawopt)

paropt :: PandocMonad m => LP m Inlines
paropt = parenWrapped inline

rawopt :: PandocMonad m => LP m Text
rawopt = try $ do
  optional sp
  inner <- untokenize <$> bracketedToks
  optional sp
  return $ "[" <> inner <> "]"

skipopts :: PandocMonad m => LP m ()
skipopts = skipMany (overlaySpecification <|> void rawopt)

-- opts in angle brackets are used in beamer
overlaySpecification :: PandocMonad m => LP m ()
overlaySpecification = try $ do
  symbol '<'
  ts <- manyTill overlayTok (symbol '>')
  guard $ case ts of
               -- see issue #3368
               [Tok _ Word s] | T.all isLetter s -> s `elem`
                                ["beamer","presentation", "trans",
                                 "handout","article", "second"]
               _ -> True

overlayTok :: PandocMonad m => LP m Tok
overlayTok =
  satisfyTok (\t ->
                  case t of
                    Tok _ Word _       -> True
                    Tok _ Spaces _     -> True
                    Tok _ Symbol c     -> c `elem` ["-","+","@","|",":",","]
                    _                  -> False)

inBrackets :: Inlines -> Inlines
inBrackets x = str "[" <> x <> str "]"

unescapeURL :: String -> String
unescapeURL ('\\':x:xs) | isEscapable x = x:unescapeURL xs
  where isEscapable c = c `elem` ("#$%&~_^\\{}" :: String)
unescapeURL (x:xs) = x:unescapeURL xs
unescapeURL [] = ""

mathEnvWith :: PandocMonad m
            => (Inlines -> a) -> Maybe Text -> Text -> LP m a
mathEnvWith f innerEnv name = f . mathDisplay . inner <$> mathEnv name
   where inner x = case innerEnv of
                        Nothing -> x
                        Just y  -> "\\begin{" ++ T.unpack y ++ "}\n" ++ x ++
                                   "\\end{" ++ T.unpack y ++ "}"

mathEnv :: PandocMonad m => Text -> LP m String
mathEnv name = do
  skipopts
  optional blankline
  res <- manyTill anyTok (end_ name)
  return $ stripTrailingNewlines $ T.unpack $ untokenize res

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
  , ("underline", underlineSpan <$> tok)
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
  , ("lettrine", optional opt >> extractSpaces (spanWith ("",["lettrine"],[])) <$> tok)
  , ("(", mathInline . toksToString <$> manyTill anyTok (controlSeq ")"))
  , ("[", mathDisplay . toksToString <$> manyTill anyTok (controlSeq "]"))
  , ("ensuremath", mathInline . toksToString <$> braced)
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
  -- old TeX commands
  , ("em", extractSpaces emph <$> inlines)
  , ("it", extractSpaces emph <$> inlines)
  , ("sl", extractSpaces emph <$> inlines)
  , ("bf", extractSpaces strong <$> inlines)
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
  , ("verb", doverb)
  , ("lstinline", dolstinline)
  , ("mintinline", domintinline)
  , ("Verb", doverb)
  , ("url", ((unescapeURL . T.unpack . untokenize) <$> bracedUrl) >>= \url ->
                  pure (link url "" (str url)))
  , ("nolinkurl", ((unescapeURL . T.unpack . untokenize) <$> bracedUrl) >>= \url ->
                  pure (code url))
  , ("href", (unescapeURL . toksToString <$>
                 bracedUrl <* optional sp) >>= \url ->
                   tok >>= \lab -> pure (link url "" lab))
  , ("includegraphics", do options <- option [] keyvals
                           src <- unescapeURL . T.unpack .
                                    removeDoubleQuotes . untokenize <$> braced
                           mkImage options src)
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
  , ("citeauthor", (try (tok *> optional sp *> controlSeq "citetext") *>
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
  , ("input", include "input")
  -- soul package
  , ("ul", underlineSpan <$> tok)
  -- ulem package
  , ("uline", underlineSpan <$> tok)
  -- plain tex stuff that should just be passed through as raw tex
  , ("ifdim", ifdim)
  ]

ifdim :: PandocMonad m => LP m Inlines
ifdim = do
  contents <- manyTill anyTok (controlSeq "fi")
  return $ rawInline "latex" $ T.unpack $
           "\\ifdim" <> untokenize contents <> "\\fi"

makeUppercase :: Inlines -> Inlines
makeUppercase = fromList . walk (alterStr (map toUpper)) . toList

makeLowercase :: Inlines -> Inlines
makeLowercase = fromList . walk (alterStr (map toLower)) . toList

alterStr :: (String -> String) -> Inline -> Inline
alterStr f (Str xs) = Str (f xs)
alterStr _ x = x

foreignlanguage :: PandocMonad m => LP m Inlines
foreignlanguage = do
  babelLang <- T.unpack . untokenize <$> braced
  case babelLangToBCP47 babelLang of
       Just lang -> spanWith ("", [], [("lang",  renderLang lang)]) <$> tok
       _ -> tok

inlineLanguageCommands :: PandocMonad m => M.Map Text (LP m Inlines)
inlineLanguageCommands = M.fromList $ mk <$> M.toList polyglossiaLangToBCP47
  where
    mk (polyglossia, bcp47Func) =
      ("text" <> T.pack polyglossia, inlineLanguage bcp47Func)

inlineLanguage :: PandocMonad m => (String -> Lang) -> LP m Inlines
inlineLanguage bcp47Func = do
  o <- option "" $ (T.unpack . T.filter (\c -> c /= '[' && c /= ']'))
                <$> rawopt
  let lang = renderLang $ bcp47Func o
  extractSpaces (spanWith ("", [], [("lang", lang)])) <$> tok

hyperlink :: PandocMonad m => LP m Inlines
hyperlink = try $ do
  src <- toksToString <$> braced
  lab <- tok
  return $ link ('#':src) "" lab

hypertargetBlock :: PandocMonad m => LP m Blocks
hypertargetBlock = try $ do
  ref <- toksToString <$> braced
  bs <- grouped block
  case toList bs of
       [Header 1 (ident,_,_) _] | ident == ref -> return bs
       _                        -> return $ divWith (ref, [], []) bs

hypertargetInline :: PandocMonad m => LP m Inlines
hypertargetInline = try $ do
  ref <- toksToString <$> braced
  ils <- grouped inline
  return $ spanWith (ref, [], []) ils

romanNumeralUpper :: (PandocMonad m) => LP m Inlines
romanNumeralUpper =
  str . toRomanNumeral <$> romanNumeralArg

romanNumeralLower :: (PandocMonad m) => LP m Inlines
romanNumeralLower =
  str . map toLower . toRomanNumeral <$> romanNumeralArg

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
        fail "Non-digits in argument to \\Rn or \\RN"
      safeRead $ T.unpack digits

newToggle :: (Monoid a, PandocMonad m) => [Tok] -> LP m a
newToggle name = do
  updateState $ \st ->
    st{ sToggles = M.insert (toksToString name) False (sToggles st) }
  return mempty

setToggle :: (Monoid a, PandocMonad m) => Bool -> [Tok] -> LP m a
setToggle on name = do
  updateState $ \st ->
    st{ sToggles = M.adjust (const on) (toksToString name) (sToggles st) }
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
  let name' = toksToString name
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

coloredInline :: PandocMonad m => String -> LP m Inlines
coloredInline stylename = do
  skipopts
  color <- braced
  spanWith ("",[],[("style",stylename ++ ": " ++ toksToString color)]) <$> tok

ttfamily :: PandocMonad m => LP m Inlines
ttfamily = (code . stringify . toList) <$> tok

rawInlineOr :: PandocMonad m => Text -> LP m Inlines -> LP m Inlines
rawInlineOr name' fallback = do
  parseRaw <- extensionEnabled Ext_raw_tex <$> getOption readerExtensions
  if parseRaw
     then rawInline "latex" <$> getRawCommand name' ("\\" <> name')
     else fallback

getRawCommand :: PandocMonad m => Text -> Text -> LP m String
getRawCommand name txt = do
  (_, rawargs) <- withRaw $
      case name of
           "write" -> do
             void $ satisfyTok isWordTok -- digits
             void braced
           "titleformat" -> do
             void braced
             skipopts
             void $ count 4 braced
           "def" ->
             void $ manyTill anyTok braced
           _ | isFontSizeCommand name -> return ()
             | otherwise -> do
               skipopts
               option "" (try dimenarg)
               void $ many braced
  return $ T.unpack (txt <> untokenize rawargs)

isFontSizeCommand :: Text -> Bool
isFontSizeCommand "tiny" = True
isFontSizeCommand "scriptsize" = True
isFontSizeCommand "footnotesize" = True
isFontSizeCommand "small" = True
isFontSizeCommand "normalsize" = True
isFontSizeCommand "large" = True
isFontSizeCommand "Large" = True
isFontSizeCommand "LARGE" = True
isFontSizeCommand "huge" = True
isFontSizeCommand "Huge" = True
isFontSizeCommand _ = False

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

dolabel :: PandocMonad m => LP m Inlines
dolabel = do
  v <- braced
  let refstr = toksToString v
  return $ spanWith (refstr,[],[("label", refstr)])
    $ inBrackets $ str $ toksToString v

doref :: PandocMonad m => String -> LP m Inlines
doref cls = do
  v <- braced
  let refstr = toksToString v
  return $ linkWith ("",[],[ ("reference-type", cls)
                           , ("reference", refstr)])
                    ('#':refstr)
                    ""
                    (inBrackets $ str refstr)

lookupListDefault :: (Show k, Ord k) => v -> [k] -> M.Map k v -> v
lookupListDefault d = (fromMaybe d .) . lookupList
  where lookupList l m = msum $ map (`M.lookup` m) l

inline :: PandocMonad m => LP m Inlines
inline = (mempty <$ comment)
     <|> (space  <$ whitespace)
     <|> (softbreak <$ endline)
     <|> word
     <|> macroDef
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
     <|> (str . (:[]) <$> primEscape)
     <|> regularSymbol
     <|> (do res <- symbolIn "#^'`\"[]&"
             pos <- getPosition
             let s = T.unpack (untoken res)
             report $ ParsingUnescaped s pos
             return $ str s)

inlines :: PandocMonad m => LP m Inlines
inlines = mconcat <$> many inline

-- block elements:

begin_ :: PandocMonad m => Text -> LP m ()
begin_ t = try (do
  controlSeq "begin"
  spaces
  txt <- untokenize <$> braced
  guard (t == txt)) <?> ("\\begin{" ++ T.unpack t ++ "}")

end_ :: PandocMonad m => Text -> LP m ()
end_ t = try (do
  controlSeq "end"
  spaces
  txt <- untokenize <$> braced
  guard $ t == txt) <?> ("\\end{" ++ T.unpack t ++ "}")

preamble :: PandocMonad m => LP m Blocks
preamble = mempty <$ many preambleBlock
  where preambleBlock =  spaces1
                     <|> void (macroDef <|> blockCommand)
                     <|> void braced
                     <|> (notFollowedBy (begin_ "document") >> void anyTok)

paragraph :: PandocMonad m => LP m Blocks
paragraph = do
  x <- trimInlines . mconcat <$> many1 inline
  if x == mempty
     then return mempty
     else return $ para x

include :: (PandocMonad m, Monoid a) => Text -> LP m a
include name = do
  skipMany opt
  fs <- (map (T.unpack . removeDoubleQuotes . T.strip) . T.splitOn "," .
         untokenize) <$> braced
  let addExt f = case takeExtension f of
                      ".tex" -> f
                      ".sty" -> f
                      -- note, we can have cc_by_4.0 for example...
                      _ | name == "usepackage" -> addExtension f ".sty"
                        | otherwise -> addExtension f ".tex"
  dirs <- (splitBy (==':') . fromMaybe ".") <$> lookupEnv "TEXINPUTS"
  mapM_ (insertIncluded dirs) (map addExt fs)
  return mempty

insertIncluded :: PandocMonad m
               => [FilePath]
               -> FilePath
               -> LP m ()
insertIncluded dirs f = do
  pos <- getPosition
  containers <- getIncludeFiles <$> getState
  when (f `elem` containers) $
    throwError $ PandocParseError $ "Include file loop at " ++ show pos
  updateState $ addIncludeFile f
  mbcontents <- readFileFromDirs dirs f
  contents <- case mbcontents of
                   Just s -> return s
                   Nothing -> do
                     report $ CouldNotLoadIncludeFile f pos
                     return ""
  getInput >>= setInput . (tokenize f (T.pack contents) ++)
  updateState dropLatestIncludeFile

addMeta :: PandocMonad m => ToMetaValue a => String -> a -> LP m ()
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

macroDef :: (Monoid a, PandocMonad m) => LP m a
macroDef =
  mempty <$ (commandDef <|> environmentDef)
  where commandDef = do
          (name, macro') <- newcommand <|> letmacro <|> defmacro
          guardDisabled Ext_latex_macros <|>
           updateState (\s -> s{ sMacros = M.insert name macro' (sMacros s) })
        environmentDef = do
          (name, macro1, macro2) <- newenvironment
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
    let argspecs = map (\i -> ArgNum i) [1..numargs]
    spaces
    optarg <- option Nothing $ Just <$> try bracketedToks
    spaces
    contents' <- bracedOrToken
    let contents =
         case mtype of
              "DeclareMathOperator" ->
                 Tok pos (CtrlSeq "mathop") "\\mathop"
                 : Tok pos (CtrlSeq "mathrm") "\\mathrm"
                 : Tok pos Symbol "{"
                 : (contents' ++
                   [ Tok pos Symbol "}" ])
              _                     -> contents'
    when (mtype == "newcommand") $ do
      macros <- sMacros <$> getState
      case M.lookup name macros of
           Just _  -> report $ MacroAlreadyDefined (T.unpack txt) pos
           Nothing -> return ()
    return (name, Macro ExpandWhenUsed argspecs optarg contents)

newenvironment :: PandocMonad m => LP m (Text, Macro, Macro)
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
    when (mtype == "newenvironment") $ do
      macros <- sMacros <$> getState
      case M.lookup name macros of
           Just _  -> report $ MacroAlreadyDefined (T.unpack name) pos
           Nothing -> return ()
    return (name, Macro ExpandWhenUsed argspecs optarg startcontents,
             Macro ExpandWhenUsed [] Nothing endcontents)

bracketedNum :: PandocMonad m => LP m Int
bracketedNum = do
  ds <- untokenize <$> bracketedToks
  case safeRead (T.unpack ds) of
       Just i -> return i
       _      -> return 0

setCaption :: PandocMonad m => LP m Blocks
setCaption = do
  ils <- tok
  mblabel <- option Nothing $
               try $ spaces >> controlSeq "label" >> (Just <$> tok)
  let capt = case mblabel of
                  Just lab -> let slab = stringify lab
                                  ils' = ils <> spanWith
                                    ("",[],[("label", slab)]) mempty
                              in  (Just ils', Just slab)
                  Nothing  -> (Just ils, Nothing)
  updateState $ \st -> st{ sCaption = capt }
  return mempty

looseItem :: PandocMonad m => LP m Blocks
looseItem = do
  inListItem <- sInListItem <$> getState
  guard $ not inListItem
  skipopts
  return mempty

resetCaption :: PandocMonad m => LP m ()
resetCaption = updateState $ \st -> st{ sCaption = (Nothing, Nothing) }

section :: PandocMonad m => Attr -> Int -> LP m Blocks
section (ident, classes, kvs) lvl = do
  skipopts
  contents <- grouped inline
  lab <- option ident $
          try (spaces >> controlSeq "label"
               >> spaces >> toksToString <$> braced)
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
  star <- option "" ("*" <$ symbol '*' <* optional sp)
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

blockCommands :: PandocMonad m => M.Map Text (LP m Blocks)
blockCommands = M.fromList
   [ ("par", mempty <$ skipopts)
   , ("parbox",  skipopts >> braced >> grouped blocks)
   , ("title", mempty <$ (skipopts *>
                             (grouped inline >>= addMeta "title")
                         <|> (grouped block >>= addMeta "title")))
   , ("subtitle", mempty <$ (skipopts *> tok >>= addMeta "subtitle"))
   , ("author", mempty <$ (skipopts *> authors))
   -- -- in letter class, temp. store address & sig as title, author
   , ("address", mempty <$ (skipopts *> tok >>= addMeta "address"))
   , ("signature", mempty <$ (skipopts *> authors))
   , ("date", mempty <$ (skipopts *> tok >>= addMeta "date"))
   -- Koma-script metadata commands
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
   , ("caption", skipopts *> setCaption)
   , ("bibliography", mempty <$ (skipopts *> braced >>=
         addMeta "bibliography" . splitBibs . toksToString))
   , ("addbibresource", mempty <$ (skipopts *> braced >>=
         addMeta "bibliography" . splitBibs . toksToString))
   , ("endinput", mempty <$ skipMany tok)
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
   , ("include", include "include")
   , ("input", include "input")
   , ("subfile", include "subfile")
   , ("usepackage", include "usepackage")
   -- preamble
   , ("PackageError", mempty <$ (braced >> braced >> braced))
   ]


environments :: PandocMonad m => M.Map Text (LP m Blocks)
environments = M.fromList
   [ ("document", env "document" blocks)
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
          resetCaption *> skipopts *> blocks >>= addTableCaption)
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
   , ("lilypond", rawVerbEnv "lilypond")
   -- etoolbox
   , ("ifstrequal", ifstrequal)
   , ("newtoggle", braced >>= newToggle)
   , ("toggletrue", braced >>= setToggle True)
   , ("togglefalse", braced >>= setToggle False)
   , ("iftoggle", try $ ifToggle >> block)
   ]

environment :: PandocMonad m => LP m Blocks
environment = try $ do
  controlSeq "begin"
  name <- untokenize <$> braced
  M.findWithDefault mzero name environments <|>
    if M.member name (inlineEnvironments
                       :: M.Map Text (LP PandocPure Inlines))
       then mzero
       else rawEnv name

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
                 $ T.unpack $ beginCommand <> untokenize raw
     else do
       report $ SkippedContent (T.unpack beginCommand) pos1
       pos2 <- getPosition
       report $ SkippedContent ("\\end{" ++ T.unpack name ++ "}") pos2
       return bs

rawVerbEnv :: PandocMonad m => Text -> LP m Blocks
rawVerbEnv name = do
  pos <- getPosition
  (_, raw) <- withRaw $ verbEnv name
  let raw' = "\\begin{" ++ T.unpack name ++ "}" ++ toksToString raw
  exts <- getOption readerExtensions
  let parseRaw = extensionEnabled Ext_raw_tex exts
  if parseRaw
     then return $ rawBlock "latex" raw'
     else do
       report $ SkippedContent raw' pos
       return mempty

verbEnv :: PandocMonad m => Text -> LP m String
verbEnv name = withVerbatimMode $ do
  optional blankline
  res <- manyTill anyTok (end_ name)
  return $ T.unpack
         $ stripTrailingNewline
         $ untokenize
         $ res

-- Strip single final newline and any spaces following it.
-- Input is unchanged if it doesn't end with newline +
-- optional spaces.
stripTrailingNewline :: Text -> Text
stripTrailingNewline t =
  let (b, e) = T.breakOnEnd "\n" t
  in  if T.all (== ' ') e
         then T.dropEnd 1 b
         else t

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
  lang <- toksToString <$> braced
  let kvs = [ (if k == "firstnumber"
                  then "startFrom"
                  else k, v) | (k,v) <- options ]
  let classes = [ lang | not (null lang) ] ++
                [ "numberLines" |
                  lookup "linenos" options == Just "true" ]
  return ("",classes,kvs)

inputMinted :: PandocMonad m => LP m Blocks
inputMinted = do
  pos <- getPosition
  attr <- mintedAttr
  f <- filter (/='"') . toksToString <$> braced
  dirs <- (splitBy (==':') . fromMaybe ".") <$> lookupEnv "TEXINPUTS"
  mbCode <- readFileFromDirs dirs f
  rawcode <- case mbCode of
                  Just s -> return s
                  Nothing -> do
                    report $ CouldNotLoadIncludeFile f pos
                    return []
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
            | not ("fig:" `isPrefixOf` tit) = do
          (mbcapt, mblab) <- sCaption <$> getState
          let (alt', tit') = case mbcapt of
                               Just ils -> (toList ils, "fig:" ++ tit)
                               Nothing  -> (alt, tit)
              attr' = case mblab of
                        Just lab -> (lab, cls, kvs)
                        Nothing  -> attr
          case attr' of
               ("", _, _)    -> return ()
               (ident, _, _) -> do
                  st <- getState
                  let chapnum =
                        case (sHasChapters st, sLastHeaderNum st) of
                             (True, DottedNum (n:_)) -> Just n
                             _                       -> Nothing
                  let num = case sLastFigureNum st of
                       DottedNum [m,n]  ->
                         case chapnum of
                              Just m' | m' == m   -> DottedNum [m, n+1]
                                      | otherwise -> DottedNum [m', 1]
                              Nothing             -> DottedNum [1]
                                                      -- shouldn't happen
                       DottedNum [n]   ->
                         case chapnum of
                              Just m  -> DottedNum [m, 1]
                              Nothing -> DottedNum [n + 1]
                       _               ->
                         case chapnum of
                               Just n  -> DottedNum [n, 1]
                               Nothing -> DottedNum [1]
                  setState $
                    st{ sLastFigureNum = num
                      , sLabels = M.insert ident
                                 [Str (renderDottedNum num)] (sLabels st) }
          return $ Image attr' alt' (src, tit')
        go x = return x

coloredBlock :: PandocMonad m => String -> LP m Blocks
coloredBlock stylename = try $ do
  skipopts
  color <- braced
  notFollowedBy (grouped inline)
  let constructor = divWith ("",[],[("style",stylename ++ ": " ++ toksToString color)])
  constructor <$> grouped block

graphicsPath :: PandocMonad m => LP m Blocks
graphicsPath = do
  ps <- map toksToString <$>
          (bgroup *> spaces *> manyTill (braced <* spaces) egroup)
  getResourcePath >>= setResourcePath . (++ ps)
  return mempty

splitBibs :: String -> [Inlines]
splitBibs = map (str . flip replaceExtension "bib" . trim) . splitBy (==',')

alltt :: Blocks -> Blocks
alltt = walk strToCode
  where strToCode (Str s)   = Code nullAttr s
        strToCode Space     = RawInline (Format "latex") "\\ "
        strToCode SoftBreak = LineBreak
        strToCode x         = x

parseListingsOptions :: [(String, String)] -> Attr
parseListingsOptions options =
  let kvs = [ (if k == "firstnumber"
                  then "startFrom"
                  else k, v) | (k,v) <- options ]
      classes = [ "numberLines" |
                  lookup "numbers" options == Just "left" ]
             ++ maybeToList (lookup "language" options
                     >>= fromListingsLanguage)
  in  (fromMaybe "" (lookup "label" options), classes, kvs)

inputListing :: PandocMonad m => LP m Blocks
inputListing = do
  pos <- getPosition
  options <- option [] keyvals
  f <- filter (/='"') . toksToString <$> braced
  dirs <- (splitBy (==':') . fromMaybe ".") <$> lookupEnv "TEXINPUTS"
  mbCode <- readFileFromDirs dirs f
  codeLines <- case mbCode of
                      Just s -> return $ lines s
                      Nothing -> do
                        report $ CouldNotLoadIncludeFile f pos
                        return []
  let (ident,classes,kvs) = parseListingsOptions options
  let language = case lookup "language" options >>= fromListingsLanguage of
                      Just l -> [l]
                      Nothing -> take 1 $ languagesByExtension (takeExtension f)
  let firstline = fromMaybe 1 $ lookup "firstline" options >>= safeRead
  let lastline = fromMaybe (length codeLines) $
                       lookup "lastline" options >>= safeRead
  let codeContents = intercalate "\n" $ take (1 + lastline - firstline) $
                       drop (firstline - 1) codeLines
  return $ codeBlockWith (ident,ordNub (classes ++ language),kvs) codeContents

-- lists

item :: PandocMonad m => LP m Blocks
item = void blocks *> controlSeq "item" *> skipopts *> blocks

descItem :: PandocMonad m => LP m (Inlines, [Blocks])
descItem = do
  blocks -- skip blocks before item
  controlSeq "item"
  optional sp
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
        ts <- toksToString <$> manyTill anyTok (symbol ']')
        case runParser anyOrderedListMarker def "option" ts of
             Right r -> return r
             Left _  -> do
               pos <- getPosition
               report $ SkippedContent ("[" ++ ts ++ "]") pos
               return (1, DefaultStyle, DefaultDelim)
  (_, style, delim) <- option (1, DefaultStyle, DefaultDelim) markerSpec
  spaces
  optional $ try $ controlSeq "setlength"
                   *> grouped (count 1 $ controlSeq "itemindent")
                   *> braced
  spaces
  start <- option 1 $ try $ do pos <- getPosition
                               controlSeq "setcounter"
                               ctr <- toksToString <$> braced
                               guard $ "enum" `isPrefixOf` ctr
                               guard $ all (`elem` ['i','v']) (drop 4 ctr)
                               optional sp
                               num <- toksToString <$> braced
                               case safeRead num of
                                    Just i -> return (i + 1 :: Int)
                                    Nothing -> do
                                      report $ SkippedContent
                                        ("\\setcounter{" ++ ctr ++
                                         "}{" ++ num ++ "}") pos
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
         setInput $ map (Tok spos Symbol . T.singleton) (T.unpack t) ++ rest
       _ -> return ()

parseAligns :: PandocMonad m => LP m [(Alignment, Double, ([Tok], [Tok]))]
parseAligns = try $ do
  let maybeBar = skipMany $
        sp <|> () <$ symbol '|' <|> () <$ (symbol '@' >> braced)
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
        ds <- trim . toksToString <$> manyTill anyTok (controlSeq "linewidth")
        spaces
        symbol '}'
        case safeRead ds of
              Just w  -> return w
              Nothing -> return 0.0
  let alignSpec = do
        pref <- option [] alignPrefix
        spaces
        al <- alignChar
        width <- colWidth <|> option 0.0 (do s <- toksToString <$> braced
                                             pos <- getPosition
                                             report $ SkippedContent s pos
                                             return 0.0)
        spaces
        suff <- option [] alignSuffix
        return (al, width, (pref, suff))
  let starAlign = do -- '*{2}{r}' == 'rr', we just expand like a macro
        symbol '*'
        spaces
        ds <- trim . toksToString <$> braced
        spaces
        spec <- braced
        case safeRead ds of
             Just n  ->
               getInput >>= setInput . (mconcat (replicate n spec) ++)
             Nothing -> fail $ "Could not parse " ++ ds ++ " as number"
  bgroup
  spaces
  maybeBar
  aligns' <- many $ try $ spaces >> optional starAlign >>
                            (alignSpec <* maybeBar)
  spaces
  egroup
  spaces
  return aligns'

parseTableRow :: PandocMonad m
              => Text   -- ^ table environment name
              -> [([Tok], [Tok])] -- ^ pref/suffixes
              -> LP m [Blocks]
parseTableRow envname prefsufs = do
  notFollowedBy (spaces *> end_ envname)
  let cols = length prefsufs
  -- add prefixes and suffixes in token stream:
  let celltoks (pref, suff) = do
        prefpos <- getPosition
        contents <- many (notFollowedBy
                         (() <$ amp <|> () <$ lbreak <|> end_ envname)
                         >> anyTok)
        suffpos <- getPosition
        option [] (count 1 amp)
        return $ map (setpos prefpos) pref ++ contents ++ map (setpos suffpos) suff
  rawcells <- mapM celltoks prefsufs
  oldInput <- getInput
  cells <- mapM (\ts -> setInput ts >> parseTableCell) rawcells
  setInput oldInput
  spaces
  let numcells = length cells
  guard $ numcells <= cols && numcells >= 1
  guard $ cells /= [mempty]
  -- note:  a & b in a three-column table leaves an empty 3rd cell:
  return $ cells ++ replicate (cols - numcells) mempty

parseTableCell :: PandocMonad m => LP m Blocks
parseTableCell = do
  let plainify bs = case toList bs of
                         [Para ils] -> plain (fromList ils)
                         _          -> bs
  updateState $ \st -> st{ sInTableCell = True }
  cells <- plainify <$> blocks
  updateState $ \st -> st{ sInTableCell = False }
  return cells

simpTable :: PandocMonad m => Text -> Bool -> LP m Blocks
simpTable envname hasWidthParameter = try $ do
  when hasWidthParameter $ () <$ (spaces >> tok)
  skipopts
  colspecs <- parseAligns
  let (aligns, widths, prefsufs) = unzip3 colspecs
  let cols = length colspecs
  optional $ controlSeq "caption" *> skipopts *> setCaption
  optional lbreak
  spaces
  skipMany hline
  spaces
  header' <- option [] $ try (parseTableRow envname prefsufs <*
                                   lbreak <* many1 hline)
  spaces
  rows <- sepEndBy (parseTableRow envname prefsufs)
                    (lbreak <* optional (skipMany hline))
  spaces
  optional $ controlSeq "caption" *> skipopts *> setCaption
  optional lbreak
  spaces
  let header'' = if null header'
                    then replicate cols mempty
                    else header'
  lookAhead $ controlSeq "end" -- make sure we're at end
  return $ table mempty (zip aligns widths) header'' rows

addTableCaption :: PandocMonad m => Blocks -> LP m Blocks
addTableCaption = walkM go
  where go (Table c als ws hs rs) = do
          (mbcapt, _) <- sCaption <$> getState
          return $ case mbcapt of
               Just ils -> Table (toList ils) als ws hs rs
               Nothing  -> Table c als ws hs rs
        go x = return x


block :: PandocMonad m => LP m Blocks
block = do
  res <- (mempty <$ spaces1)
    <|> environment
    <|> macroDef
    <|> blockCommand
    <|> paragraph
    <|> grouped block
  trace (take 60 $ show $ B.toList res)
  return res

blocks :: PandocMonad m => LP m Blocks
blocks = mconcat <$> many block

setDefaultLanguage :: PandocMonad m => LP m Blocks
setDefaultLanguage = do
  o <- option "" $ (T.unpack . T.filter (\c -> c /= '[' && c /= ']'))
                <$> rawopt
  polylang <- toksToString <$> braced
  case M.lookup polylang polyglossiaLangToBCP47 of
       Nothing -> return mempty -- TODO mzero? warning?
       Just langFunc -> do
         let l = langFunc o
         setTranslations l
         updateState $ setMeta "lang" $ str (renderLang l)
         return mempty
