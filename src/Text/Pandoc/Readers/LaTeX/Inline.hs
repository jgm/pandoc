{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
{- |
   Module      : Text.Pandoc.Readers.LaTeX.Inline
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable
-}
module Text.Pandoc.Readers.LaTeX.Inline
  ( acronymCommands
  , verbCommands
  , charCommands
  , accentCommands
  , miscCommands
  , nameCommands
  , biblatexInlineCommands
  , refCommands
  , rawInlineOr
  , listingsLanguage
  )
where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Builder
import Text.Pandoc.Shared (toRomanNumeral, safeRead)
import Text.Pandoc.TeX (Tok (..), TokType (..))
import Control.Applicative (optional, (<|>))
import Control.Monad (guard, mzero, mplus, unless)
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Translations (translateTerm)
import Text.Pandoc.Readers.LaTeX.Parsing
import Text.Pandoc.Extensions (extensionEnabled, Extension(..))
import Text.Pandoc.Parsing (getOption, updateState, getState, notFollowedBy,
                            manyTill, getInput, setInput, incSourceColumn,
                            option, many1)
import Data.Char (isDigit)
import Text.Pandoc.Highlighting (fromListingsLanguage,)
import Data.Maybe (maybeToList, fromMaybe)
import Text.Pandoc.Options (ReaderOptions(..))
import qualified Data.Text.Normalize as Normalize
import qualified Text.Pandoc.Translations as Translations

rawInlineOr :: PandocMonad m => Text -> LP m Inlines -> LP m Inlines
rawInlineOr name' fallback = do
  parseRaw <- extensionEnabled Ext_raw_tex <$> getOption readerExtensions
  if parseRaw
     then rawInline "latex" <$> getRawCommand name' ("\\" <> name')
     else fallback

dolabel :: PandocMonad m => LP m Inlines
dolabel = do
  v <- braced
  let refstr = untokenize v
  updateState $ \st ->
    st{ sLastLabel = Just refstr }
  return $ spanWith (refstr,[],[("label", refstr)]) mempty

doref :: PandocMonad m => Text -> LP m Inlines
doref cls = do
  v <- braced
  let refstr = untokenize v
  return $ linkWith ("",[],[ ("reference-type", cls)
                           , ("reference", refstr)])
                    ("#" <> refstr)
                    ""
                    (inBrackets $ str refstr)

inBrackets :: Inlines -> Inlines
inBrackets x = str "[" <> x <> str "]"

doTerm :: PandocMonad m => Translations.Term -> LP m Inlines
doTerm term = str <$> translateTerm term

lit :: Text -> LP m Inlines
lit = pure . str

doverb :: PandocMonad m => LP m Inlines
doverb = do
  Tok _ Symbol t <- anySymbol
  marker <- case T.uncons t of
              Just (c, ts) | T.null ts -> return c
              _            -> mzero
  withVerbatimMode $
    code . untokenize <$>
      manyTill (notFollowedBy newlineTok >> verbTok marker) (symbol marker)

verbTok :: PandocMonad m => Char -> LP m Tok
verbTok stopchar = do
  t@(Tok pos toktype txt) <- anyTok
  case T.findIndex (== stopchar) txt of
       Nothing -> return t
       Just i  -> do
         let (t1, t2) = T.splitAt i txt
         TokStream macrosExpanded inp <- getInput
         setInput $ TokStream macrosExpanded
                  $ Tok (incSourceColumn pos i) Symbol (T.singleton stopchar)
                  : tokenize (incSourceColumn pos (i + 1)) (T.drop 1 t2) ++ inp
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
    codeWith ("",classes,[]) . T.map nlToSpace . untokenize <$>
      manyTill (verbTok stopchar) (symbol stopchar)

nlToSpace :: Char -> Char
nlToSpace '\n' = ' '
nlToSpace x    = x

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
      s <- untokenize <$> many1 (satisfyTok isWordTok)
      let (digits, rest) = T.span isDigit s
      unless (T.null rest) $
        Prelude.fail "Non-digits in argument to \\Rn or \\RN"
      safeRead digits

accentWith :: PandocMonad m
           => LP m Inlines -> Char -> Maybe Char -> LP m Inlines
accentWith tok combiningAccent fallBack = do
  ils <- option mempty tok
  case toList ils of
       (Str (T.uncons -> Just (x, xs)) : ys) -> return $ fromList $
         -- try to normalize to the combined character:
         Str (Normalize.normalize Normalize.NFC
               (T.pack [x, combiningAccent]) <> xs) : ys
       [Space] -> return $ str $ T.singleton
                         $ fromMaybe combiningAccent fallBack
       []      -> return $ str $ T.singleton
                         $ fromMaybe combiningAccent fallBack
       _       -> return ils


verbCommands :: PandocMonad m => M.Map Text (LP m Inlines)
verbCommands = M.fromList
  [ ("verb", doverb)
  , ("lstinline", dolstinline)
  , ("mintinline", domintinline)
  , ("Verb", doverb)
  ]

miscCommands :: PandocMonad m => M.Map Text (LP m Inlines)
miscCommands =
  M.fromList
  [ ("pounds", lit "£")
  , ("euro", lit "€")
  , ("copyright", lit "©")
  , ("textasciicircum", lit "^")
  , ("textasciitilde", lit "~")
  , ("textbaht", lit "฿")
  , ("textblank", lit "␢")
  , ("textbigcircle", lit "○")
  , ("textbrokenbar", lit "¦")
  , ("textbullet", lit "•")
  , ("textcentoldstyle", lit "¢")
  , ("textcopyright", lit "©")
  , ("textdagger", lit "†")
  , ("textdegree", lit "°")
  , ("textdollar", lit "$")
  , ("textdong", lit "₫")
  , ("textlira", lit "₤")
  , ("textmu", lit "μ")
  , ("textmusicalnote", lit "♪")
  , ("textonehalf", lit "½")
  , ("textonequarter", lit "¼")
  , ("textparagraph", lit "¶")
  , ("textpertenthousand", lit "‱")
  , ("textpeso", lit "₱")
  , ("textquotesingle", lit "'")
  , ("textregistered", lit "®")
  , ("textsection", lit "§")
  , ("textsterling", lit "£")
  , ("textthreequarters", lit "¾")
  , ("textthreesuperior", lit "³")
  , ("texttwosuperior", lit "²")
  , ("textyen", lit "¥")
  ]

accentCommands :: PandocMonad m => LP m Inlines -> M.Map Text (LP m Inlines)
accentCommands tok =
  let accent = accentWith tok
  in  M.fromList
  [ ("aa", lit "å")
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
  ]

charCommands :: PandocMonad m => M.Map Text (LP m Inlines)
charCommands = M.fromList
  [ ("ldots", lit "…")
  , ("vdots", lit "\8942")
  , ("dots", lit "…")
  , ("mdots", lit "…")
  , ("sim", lit "~")
  , ("sep", lit ",")
  , ("P", lit "¶")
  , ("S", lit "§")
  , ("$", lit "$")
  , ("%", lit "%")
  , ("&", lit "&")
  , ("#", lit "#")
  , ("_", lit "_")
  , ("{", lit "{")
  , ("}", lit "}")
  , ("-", lit "\x00ad") -- soft hyphen
  , ("qed", lit "\a0\x25FB")
  , ("lq", return (str "‘"))
  , ("rq", return (str "’"))
  , ("textquoteleft", return (str "‘"))
  , ("textquoteright", return (str "’"))
  , ("textquotedblleft", return (str "“"))
  , ("textquotedblright", return (str "”"))
  , ("/", pure mempty) -- italic correction
  , ("\\", linebreak <$ (do inTableCell <- sInTableCell <$> getState
                            guard $ not inTableCell
                            optional rawopt
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
  , ("textbackslash", lit "\\")
  , ("backslash", lit "\\")
  , ("slash", lit "/")
  -- fontawesome
  , ("faCheck", lit "\10003")
  , ("faClose", lit "\10007")
  -- hyphenat
  , ("bshyp", lit "\\\173")
  , ("fshyp", lit "/\173")
  , ("dothyp", lit ".\173")
  , ("colonhyp", lit ":\173")
  , ("hyp", lit "-")
  -- ngerman (babel)
  , ("glq", lit "‚")
  , ("grq", lit "‘")
  , ("glqq", lit "„")
  , ("grqq", lit "“")
  , ("flq", lit "‹")
  , ("frq", lit "›")
  , ("flqq", lit "«")
  , ("frqq", lit "»")
  , ("dq", lit "\"")
  -- fontspec
  , ("guillemetleft", lit "«")
  , ("guillemotleft", lit "«")
  , ("guillemetright", lit "»")
  , ("guillemotright", lit "»")
  , ("guilsinglleft", lit "‹")
  , ("guilsinglright", lit "›")
  , ("quotedblbase", lit "„")
  , ("quotesinglbase", lit ",")
  , ("textquotedbl", lit "\"")
  ]

biblatexInlineCommands :: PandocMonad m
                       => LP m Inlines -> M.Map Text (LP m Inlines)
biblatexInlineCommands tok = M.fromList
  -- biblatex misc
  [ ("RN", romanNumeralUpper)
  , ("Rn", romanNumeralLower)
  , ("mkbibquote", spanWith nullAttr . doubleQuoted <$> tok)
  , ("mkbibemph", spanWith nullAttr . emph <$> tok)
  , ("mkbibitalic", spanWith nullAttr . emph <$> tok)
  , ("mkbibbold", spanWith nullAttr . strong <$> tok)
  , ("mkbibparens",
       spanWith nullAttr . (\x -> str "(" <> x <> str ")") <$> tok)
  , ("mkbibbrackets",
       spanWith nullAttr . (\x -> str "[" <> x <> str "]") <$> tok)
  , ("autocap", spanWith nullAttr <$> tok)
  , ("textnormal", spanWith ("",["nodecor"],[]) <$> tok)
  , ("bibstring",
       (\x -> spanWith ("",[],[("bibstring",x)]) (str x)) . untokenize
         <$> braced)
  , ("adddot", pure (str "."))
  , ("adddotspace", pure (spanWith nullAttr (str "." <> space)))
  , ("addabbrvspace", pure space)
  , ("hyphen", pure (str "-"))
  ]

nameCommands :: PandocMonad m => M.Map Text (LP m Inlines)
nameCommands = M.fromList
  [ ("figurename", doTerm Translations.Figure)
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
  ]

refCommands :: PandocMonad m => M.Map Text (LP m Inlines)
refCommands = M.fromList
  [ ("label", rawInlineOr "label" dolabel)
  , ("ref", rawInlineOr "ref" $ doref "ref")
  , ("cref", rawInlineOr "cref" $ doref "ref+label")       -- from cleveref.sty
  , ("Cref", rawInlineOr "Cref" $ doref "ref+Label")       -- from cleveref.sty
  , ("vref", rawInlineOr "vref" $ doref "ref")  -- from varioref.sty
  , ("eqref", rawInlineOr "eqref" $ doref "eqref")   -- from amsmath.sty
  , ("autoref", rawInlineOr "autoref" $ doref "ref+label") -- from hyperref.sty
  ]

acronymCommands :: PandocMonad m => M.Map Text (LP m Inlines)
acronymCommands = M.fromList
  -- glossaries package
  [ ("gls", doAcronym "short")
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
  , ("acl", doAcronym "long")
  , ("acp", doAcronymPlural "short")
  , ("acfp", doAcronymPlural "full")
  , ("acsp", doAcronymPlural "abbrv")
  , ("aclp", doAcronymPlural "long")
  , ("Ac", doAcronym "short")
  , ("Acf", doAcronym "full")
  , ("Acs", doAcronym "abbrv")
  , ("Acl", doAcronym "long")
  , ("Acp", doAcronymPlural "short")
  , ("Acfp", doAcronymPlural "full")
  , ("Acsp", doAcronymPlural "abbrv")
  , ("Aclp", doAcronymPlural "long")
  ]

doAcronym :: PandocMonad m => Text -> LP m Inlines
doAcronym form = do
  acro <- braced
  return . mconcat $ [spanWith ("",[],[("acronym-label", untokenize acro),
    ("acronym-form", "singular+" <> form)])
    $ str $ untokenize acro]

doAcronymPlural :: PandocMonad m => Text -> LP m Inlines
doAcronymPlural form = do
  acro <- braced
  let plural = str "s"
  return . mconcat $ [spanWith ("",[],[("acronym-label", untokenize acro),
    ("acronym-form", "plural+" <> form)]) $
   mconcat [str $ untokenize acro, plural]]
