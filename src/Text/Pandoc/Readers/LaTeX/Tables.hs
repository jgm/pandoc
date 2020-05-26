{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
{- |
   Module      : Text.Pandoc.Readers.LaTeX.Tables
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of LaTeX to 'Pandoc' document.

-}
module Text.Pandoc.Readers.LaTeX.Tables where

import Control.Applicative (many, optional, (<|>))
import Control.Monad
import Control.Monad.Except (throwError)
import Data.Char (isDigit, isLetter, toUpper, chr)
import Data.Default
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as Set
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
   -- etoolbox
   , ("ifstrequal", ifstrequal)
   , ("newtoggle", braced >>= newToggle)
   , ("toggletrue", braced >>= setToggle True)
   , ("togglefalse", braced >>= setToggle False)
   , ("iftoggle", try $ ifToggle >> block)
   ]
