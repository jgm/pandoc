{-
Copyright (C) 2006-2012 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Readers.LaTeX
   Copyright   : Copyright (C) 2006-2012 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of LaTeX to 'Pandoc' document.
-}
module Text.Pandoc.Readers.LaTeX ( readLaTeX,
                                   rawLaTeXInline,
                                   rawLaTeXBlock,
                                   handleIncludes
                                 ) where

import Text.ParserCombinators.Parsec hiding ((<|>), space, many, optional)
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Parsing
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Char ( chr, ord )
import Control.Monad
import Text.Pandoc.Builder
import Data.Char (isLetter, isPunctuation, isSpace)
import Control.Applicative
import Data.Monoid
import System.FilePath (replaceExtension)
import Data.List (intercalate)
import qualified Data.Map as M

-- | Parse LaTeX from string and return 'Pandoc' document.
readLaTeX :: ParserState   -- ^ Parser state, including options for parser
          -> String        -- ^ String to parse (assumes @'\n'@ line endings)
          -> Pandoc
readLaTeX = readWith parseLaTeX

parseLaTeX :: LP Pandoc
parseLaTeX = do
  bs <- blocks
  eof
  st <- getState
  let title' = stateTitle st
  let authors' = stateAuthors st
  let date' = stateDate st
  return $ Pandoc (Meta title' authors' date') $ toList bs

type LP = GenParser Char ParserState

anyControlSeq :: LP String
anyControlSeq = do
  char '\\'
  next <- option '\n' anyChar
  name <- case next of
               '\n'           -> return ""
               c | isLetter c -> (c:) <$> (many letter <* optional sp)
                 | otherwise  -> return [c]
  return name

controlSeq :: String -> LP String
controlSeq name = try $ do
  char '\\'
  case name of
        ""   -> mzero
        [c] | not (isLetter c) -> string [c]
        cs   -> string cs <* notFollowedBy letter <* optional sp
  return name

dimenarg :: LP String
dimenarg = try $ do
  ch  <- option "" $ string "="
  num <- many1 digit
  dim <- oneOfStrings ["pt","pc","in","bp","cm","mm","dd","cc","sp"]
  return $ ch ++ num ++ dim

sp :: LP ()
sp = skipMany1 $ satisfy (\c -> c == ' ' || c == '\t')
        <|> (try $ newline >>~ lookAhead anyChar >>~ notFollowedBy blankline)

isLowerHex :: Char -> Bool
isLowerHex x = x >= '0' && x <= '9' || x >= 'a' && x <= 'f'

tildeEscape :: LP Char
tildeEscape = try $ do
  string "^^"
  c <- satisfy (\x -> x >= '\0' && x <= '\128')
  d <- if isLowerHex c
          then option "" $ count 1 (satisfy isLowerHex)
          else return ""
  if null d
     then case ord c of
           x | x >= 64 && x <= 127 -> return $ chr (x - 64)
             | otherwise           -> return $ chr (x + 64)
     else return $ chr $ read ('0':'x':c:d)

comment :: LP ()
comment = do
  char '%'
  skipMany (satisfy (/='\n'))
  newline
  return ()

bgroup :: LP ()
bgroup = () <$ char '{'
     <|> () <$ controlSeq "bgroup"
     <|> () <$ controlSeq "begingroup"

egroup :: LP ()
egroup = () <$ char '}'
     <|> () <$ controlSeq "egroup"
     <|> () <$ controlSeq "endgroup"

grouped :: Monoid a => LP a -> LP a
grouped parser = try $ bgroup *> (mconcat <$> manyTill parser egroup)

braced :: LP String
braced = bgroup *> (concat <$> manyTill
         (  many1 (satisfy (\c -> c /= '\\' && c /= '}' && c /= '{'))
        <|> try (string "\\}")
        <|> try (string "\\{")
        <|> try (string "\\\\")
        <|> ((\x -> "{" ++ x ++ "}") <$> braced)
        <|> count 1 anyChar
         ) egroup)

bracketed :: Monoid a => LP a -> LP a
bracketed parser = try $ char '[' *> (mconcat <$> manyTill parser (char ']'))

trim :: String -> String
trim = removeLeadingTrailingSpace

mathDisplay :: LP String -> LP Inlines
mathDisplay p = displayMath <$> (try p >>= applyMacros' . trim)

mathInline :: LP String -> LP Inlines
mathInline p = math <$> (try p >>= applyMacros')

mathChars :: LP String
mathChars = concat <$>
  many (   many1 (satisfy (\c -> c /= '$' && c /='\\'))
      <|> (\c -> ['\\',c]) <$> (try $ char '\\' *> anyChar)
       )

double_quote :: LP Inlines
double_quote = (doubleQuoted . mconcat) <$>
  (try $ string "``" *> manyTill inline (try $ string "''"))

single_quote :: LP Inlines
single_quote = char '`' *>
  ( try ((singleQuoted . mconcat) <$>
         manyTill inline (try $ char '\'' >> notFollowedBy letter))
  <|> lit "`")

inline :: LP Inlines
inline = (mempty <$ comment)
     <|> (space  <$ sp)
     <|> inlineText
     <|> inlineCommand
     <|> grouped inline
     <|> (char '-' *> option (str "-")
           ((char '-') *> option (str "–") (str "—" <$ char '-')))
     <|> double_quote
     <|> single_quote
     <|> (str "’" <$ char '\'')
     <|> (str "\160" <$ char '~')
     <|> (mathDisplay $ string "$$" *> mathChars <* string "$$")
     <|> (mathInline  $ char '$' *> mathChars <* char '$')
     <|> (superscript <$> (char '^' *> tok))
     <|> (subscript <$> (char '_' *> tok))
     <|> (failUnlessLHS *> char '|' *> doLHSverb)
     <|> (str <$> count 1 tildeEscape)
     <|> (str <$> string "]")
     <|> (str <$> string "#") -- TODO print warning?
     <|> (str <$> string "&") -- TODO print warning?
     -- <|> (str <$> count 1 (satisfy (\c -> c /= '\\' && c /='\n' && c /='}' && c /='{'))) -- eat random leftover characters

inlines :: LP Inlines
inlines = mconcat <$> many (notFollowedBy (char '}') *> inline)

block :: LP Blocks
block = (mempty <$ comment)
    <|> (mempty <$ ((spaceChar <|> newline) *> spaces))
    <|> environment
    <|> mempty <$ macro -- TODO improve macros, make them work everywhere
    <|> blockCommand
    <|> grouped block
    <|> paragraph
    <|> (mempty <$ char '&')  -- loose & in table environment


blocks :: LP Blocks
blocks = mconcat <$> many block

blockCommand :: LP Blocks
blockCommand = try $ do
  name <- anyControlSeq
  star <- option "" (string "*" <* optional sp)
  let name' = name ++ star
  case M.lookup name' blockCommands of
       Just p      -> p
       Nothing     -> case M.lookup name blockCommands of
                           Just p    -> p
                           Nothing   -> mzero

inBrackets :: Inlines -> Inlines
inBrackets x = (str "[") <> x <> (str "]")

-- eat an optional argument and one or more arguments in braces
ignoreInlines :: String -> (String, LP Inlines)
ignoreInlines name = (name, doraw <|> (mempty <$ optargs))
  where optargs = skipopts *> skipMany (try $ optional sp *> braced)
        contseq = '\\':name
        doraw = (rawInline "latex" . (contseq ++) . snd) <$>
                 (getState >>= guard . stateParseRaw >> (withRaw optargs))

ignoreBlocks :: String -> (String, LP Blocks)
ignoreBlocks name = (name, doraw <|> (mempty <$ optargs))
  where optargs = skipopts *> skipMany (try $ optional sp *> braced)
        contseq = '\\':name
        doraw = (rawBlock "latex" . (contseq ++) . snd) <$>
                 (getState >>= guard . stateParseRaw >> (withRaw optargs))

blockCommands :: M.Map String (LP Blocks)
blockCommands = M.fromList $
  [ ("par", mempty <$ skipopts)
  , ("title", mempty <$ (skipopts *> tok >>= addTitle))
  , ("subtitle", mempty <$ (skipopts *> tok >>= addSubtitle))
  , ("author", mempty <$ (skipopts *> authors))
  -- -- in letter class, temp. store address & sig as title, author
  , ("address", mempty <$ (skipopts *> tok >>= addTitle))
  , ("signature", mempty <$ (skipopts *> authors))
  , ("date", mempty <$ (skipopts *> tok >>= addDate))
  -- sectioning
  , ("chapter", updateState (\s -> s{ stateHasChapters = True }) *> section 0)
  , ("section", section 1)
  , ("subsection", section 2)
  , ("subsubsection", section 3)
  , ("paragraph", section 4)
  , ("subparagraph", section 5)
  -- beamer slides
  , ("frametitle", section 3)
  , ("framesubtitle", section 4)
  -- letters
  , ("opening", (para . trimInlines) <$> (skipopts *> tok))
  , ("closing", skipopts *> closing)
  --
  , ("rule", skipopts *> tok *> tok *> pure horizontalRule)
  , ("begin", mzero)   -- these are here so they won't be interpreted as inline
  , ("end", mzero)
  , ("item", skipopts *> loose_item)
  , ("documentclass", skipopts *> braced *> preamble)
  ] ++ map ignoreBlocks
  -- these commands will be ignored unless --parse-raw is specified,
  -- in which case they will appear as raw latex blocks
  [ "newcommand", "renewcommand", "newenvironment", "renewenvironment"
    -- newcommand, etc. should be parsed by macro, but we need this
    -- here so these aren't parsed as inline commands to ignore
  , "special", "pdfannot", "pdfstringdef"
  , "bibliography", "bibliographystyle"
  , "maketitle", "makeindex", "makeglossary"
  , "addcontentsline", "addtocontents", "addtocounter"
     -- \ignore{} is used conventionally in literate haskell for definitions
     -- that are to be processed by the compiler but not printed.
  , "ignore"
  , "hyperdef"
  , "noindent"
  , "markboth", "markright", "markleft"
  , "hspace", "vspace"
  ]

addTitle :: Inlines -> LP ()
addTitle tit = updateState (\s -> s{ stateTitle = toList tit })

addSubtitle :: Inlines -> LP ()
addSubtitle tit = updateState (\s -> s{ stateTitle = stateTitle s ++
                        toList (str ":" <> linebreak <> tit) })

authors :: LP ()
authors = try $ do
  char '{'
  let oneAuthor = mconcat <$>
       many1 (notFollowedBy' (controlSeq "and") >> inline)
  auths <- sepBy oneAuthor (controlSeq "and")
  char '}'
  updateState (\s -> s { stateAuthors = map (normalizeSpaces . toList) auths })

addDate :: Inlines -> LP ()
addDate dat = updateState (\s -> s{ stateDate = toList dat })

section :: Int -> LP Blocks
section lvl = do
  hasChapters <- stateHasChapters `fmap` getState
  let lvl' = if hasChapters then lvl + 1 else lvl
  skipopts
  contents <- grouped inline
  return $ header lvl' contents

inlineCommand :: LP Inlines
inlineCommand = try $ do
  name <- anyControlSeq
  guard $ not $ isBlockCommand name
  parseRaw <- stateParseRaw `fmap` getState
  star <- option "" (string "*")
  let name' = name ++ star
  case M.lookup name' inlineCommands of
       Just p      -> p
       Nothing     -> case M.lookup name inlineCommands of
                           Just p    -> p
                           Nothing   ->
                             if parseRaw
                                then (rawInline "latex" . (('\\':name') ++)) <$> rawargs
                                else mempty <$> rawargs
                              where rawargs = withRaw (skipopts *> option "" dimenarg
                                                 *> many braced) >>= applyMacros' . snd

isBlockCommand :: String -> Bool
isBlockCommand s = maybe False (const True) $ M.lookup s blockCommands

inlineCommands :: M.Map String (LP Inlines)
inlineCommands = M.fromList $
  [ ("emph", emph <$> tok)
  , ("textit", emph <$> tok)
  , ("textsc", smallcaps <$> tok)
  , ("sout", strikeout <$> tok)
  , ("textsuperscript", superscript <$> tok)
  , ("textsubscript", subscript <$> tok)
  , ("textbackslash", lit "\\")
  , ("backslash", lit "\\")
  , ("textbf", strong <$> tok)
  , ("ldots", lit "…")
  , ("dots", lit "…")
  , ("mdots", lit "…")
  , ("sim", lit "~")
  , ("label", inBrackets <$> tok)
  , ("ref", inBrackets <$> tok)
  , ("(", mathInline $ manyTill anyChar (try $ string "\\)"))
  , ("[", mathDisplay $ manyTill anyChar (try $ string "\\]"))
  , ("ensuremath", mathInline $ braced)
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
  , ("em", emph <$> inlines)
  , ("it", emph <$> inlines)
  , ("sl", emph <$> inlines)
  , ("bf", strong <$> inlines)
  , ("rm", inlines)
  , ("itshape", emph <$> inlines)
  , ("slshape", emph <$> inlines)
  , ("scshape", smallcaps <$> inlines)
  , ("bfseries", strong <$> inlines)
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
  , ("pounds", lit "£")
  , ("euro", lit "€")
  , ("copyright", lit "©")
  , ("`", option (str "`") $ try $ tok >>= accent grave)
  , ("'", option (str "'") $ try $ tok >>= accent acute)
  , ("^", option (str "^") $ try $ tok >>= accent circ)
  , ("~", option (str "~") $ try $ tok >>= accent tilde)
  , ("\"", option (str "\"") $ try $ tok >>= accent umlaut)
  , (".", option (str ".") $ try $ tok >>= accent dot)
  , ("=", option (str "=") $ try $ tok >>= accent macron)
  , ("c", option (str "c") $ try $ tok >>= accent cedilla)
  , ("i", lit "i")
  , ("\\", linebreak <$ (optional (bracketed inline) *> optional sp))
  , (",", pure mempty)
  , ("@", pure mempty)
  , (" ", lit "\160")
  , ("ps", pure $ str "PS." <> space)
  , ("TeX", lit "TeX")
  , ("LaTeX", lit "LaTeX")
  , ("bar", lit "|")
  , ("textless", lit "<")
  , ("textgreater", lit ">")
  , ("thanks", (note . mconcat) <$> (char '{' *> manyTill block (char '}')))
  , ("footnote", (note . mconcat) <$> (char '{' *> manyTill block (char '}')))
  , ("verb", doverb)
  , ("lstinline", doverb)
  , ("texttt", (code . stringify . toList) <$> tok)
  , ("url", (unescapeURL <$> braced) >>= \url ->
       pure (link url "" (codeWith ("",["url"],[]) url)))
  , ("href", (unescapeURL <$> braced <* optional sp) >>= \url ->
       tok >>= \lab ->
         pure (link url "" lab))
  , ("includegraphics", skipopts *> (unescapeURL <$> braced) >>=
       (\src -> pure (image src "" (str "image"))))
  , ("cite", citation "cite" NormalCitation False)
  , ("citep", citation "citep" NormalCitation False)
  , ("citep*", citation "citep*" NormalCitation False)
  , ("citeal", citation "citeal" NormalCitation False)
  , ("citealp", citation "citealp" NormalCitation False)
  , ("citealp*", citation "citealp*" NormalCitation False)
  , ("autocite", citation "autocite" NormalCitation False)
  , ("footcite", citation "footcite" NormalCitation False)
  , ("parencite", citation "parencite" NormalCitation False)
  , ("supercite", citation "supercite" NormalCitation False)
  , ("footcitetext", citation "footcitetext" NormalCitation False)
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
  , ("footcites", citation "footcites" NormalCitation True)
  , ("parencites", citation "parencites" NormalCitation True)
  , ("supercites", citation "supercites" NormalCitation True)
  , ("footcitetexts", citation "footcitetexts" NormalCitation True)
  , ("Autocite", citation "Autocite" NormalCitation False)
  , ("Footcite", citation "Footcite" NormalCitation False)
  , ("Parencite", citation "Parencite" NormalCitation False)
  , ("Supercite", citation "Supercite" NormalCitation False)
  , ("Footcitetext", citation "Footcitetext" NormalCitation False)
  , ("Citeyearpar", citation "Citeyearpar" SuppressAuthor False)
  , ("Citeyear", citation "Citeyear" SuppressAuthor False)
  , ("Autocite*", citation "Autocite*" SuppressAuthor False)
  , ("Cite*", citation "Cite*" SuppressAuthor False)
  , ("Parencite*", citation "Parencite*" SuppressAuthor False)
  , ("Textcite", citation "Textcite" AuthorInText False)
  , ("Textcites", citation "Textcites" AuthorInText True)
  , ("Cites", citation "Cites" NormalCitation True)
  , ("Autocites", citation "Autocites" NormalCitation True)
  , ("Footcites", citation "Footcites" NormalCitation True)
  , ("Parencites", citation "Parencites" NormalCitation True)
  , ("Supercites", citation "Supercites" NormalCitation True)
  , ("Footcitetexts", citation "Footcitetexts" NormalCitation True)
  , ("citetext", complexNatbibCitation NormalCitation)
  , ("citeauthor", (try (tok *> optional sp *> controlSeq "citetext") *>
                        complexNatbibCitation AuthorInText)
                   <|> citation "citeauthor" AuthorInText False)
  ] ++ map ignoreInlines
  -- these commands will be ignored unless --parse-raw is specified,
  -- in which case they will appear as raw latex blocks:
  [ "index", "nocite" ]

unescapeURL :: String -> String
unescapeURL ('\\':x:xs) | isEscapable x = x:unescapeURL xs
  where isEscapable '%' = True
        isEscapable '#' = True
        isEscapable _   = False
unescapeURL (x:xs) = x:unescapeURL xs
unescapeURL [] = ""

doverb :: LP Inlines
doverb = do
  marker <- anyChar
  code <$> manyTill (satisfy (/='\n')) (char marker)

doLHSverb :: LP Inlines
doLHSverb = codeWith ("",["haskell"],[]) <$> manyTill (satisfy (/='\n')) (char '|')

lit :: String -> LP Inlines
lit = pure . str

accent :: (Char -> Char) -> Inlines -> LP Inlines
accent f ils =
  case toList ils of
       (Str (x:xs) : ys) -> return $ fromList $ (Str (f x : xs) : ys)
       []                -> mzero
       _                 -> return ils

grave :: Char -> Char
grave 'A' = 'À'
grave 'E' = 'È'
grave 'I' = 'Ì'
grave 'O' = 'Ò'
grave 'U' = 'Ù'
grave 'a' = 'à'
grave 'e' = 'è'
grave 'i' = 'ì'
grave 'o' = 'ò'
grave 'u' = 'ù'
grave c   = c

acute :: Char -> Char
acute 'A' = 'Á'
acute 'E' = 'É'
acute 'I' = 'Í'
acute 'O' = 'Ó'
acute 'U' = 'Ú'
acute 'Y' = 'Ý'
acute 'a' = 'á'
acute 'e' = 'é'
acute 'i' = 'í'
acute 'o' = 'ó'
acute 'u' = 'ú'
acute 'y' = 'ý'
acute 'C' = 'Ć'
acute 'c' = 'ć'
acute 'L' = 'Ĺ'
acute 'l' = 'ĺ'
acute 'N' = 'Ń'
acute 'n' = 'ń'
acute 'R' = 'Ŕ'
acute 'r' = 'ŕ'
acute 'S' = 'Ś'
acute 's' = 'ś'
acute 'Z' = 'Ź'
acute 'z' = 'ź'
acute c = c

circ :: Char -> Char
circ 'A' = 'Â'
circ 'E' = 'Ê'
circ 'I' = 'Î'
circ 'O' = 'Ô'
circ 'U' = 'Û'
circ 'a' = 'â'
circ 'e' = 'ê'
circ 'i' = 'î'
circ 'o' = 'ô'
circ 'u' = 'û'
circ 'C' = 'Ĉ'
circ 'c' = 'ĉ'
circ 'G' = 'Ĝ'
circ 'g' = 'ĝ'
circ 'H' = 'Ĥ'
circ 'h' = 'ĥ'
circ 'J' = 'Ĵ'
circ 'j' = 'ĵ'
circ 'S' = 'Ŝ'
circ 's' = 'ŝ'
circ 'W' = 'Ŵ'
circ 'w' = 'ŵ'
circ 'Y' = 'Ŷ'
circ 'y' = 'ŷ'
circ c = c

tilde :: Char -> Char
tilde 'A' = 'Ã'
tilde 'a' = 'ã'
tilde 'O' = 'Õ'
tilde 'o' = 'õ'
tilde 'I' = 'Ĩ'
tilde 'i' = 'ĩ'
tilde 'U' = 'Ũ'
tilde 'u' = 'ũ'
tilde 'N' = 'Ñ'
tilde 'n' = 'ñ'
tilde c   = c

umlaut :: Char -> Char
umlaut 'A' = 'Ä'
umlaut 'E' = 'Ë'
umlaut 'I' = 'Ï'
umlaut 'O' = 'Ö'
umlaut 'U' = 'Ü'
umlaut 'a' = 'ä'
umlaut 'e' = 'ë'
umlaut 'i' = 'ï'
umlaut 'o' = 'ö'
umlaut 'u' = 'ü'
umlaut c = c

dot :: Char -> Char
dot 'C' = 'Ċ'
dot 'c' = 'ċ'
dot 'E' = 'Ė'
dot 'e' = 'ė'
dot 'G' = 'Ġ'
dot 'g' = 'ġ'
dot 'I' = 'İ'
dot 'Z' = 'Ż'
dot 'z' = 'ż'
dot c = c

macron :: Char -> Char
macron 'A' = 'Ā'
macron 'E' = 'Ē'
macron 'I' = 'Ī'
macron 'O' = 'Ō'
macron 'U' = 'Ū'
macron 'a' = 'ā'
macron 'e' = 'ē'
macron 'i' = 'ī'
macron 'o' = 'ō'
macron 'u' = 'ū'
macron c = c

cedilla :: Char -> Char
cedilla 'c' = 'ç'
cedilla 'C' = 'Ç'
cedilla 's' = 'ş'
cedilla 'S' = 'Ş'
cedilla c = c

tok :: LP Inlines
tok = try $ grouped inline <|> inlineCommand <|> str <$> (count 1 $ inlineChar)

opt :: LP Inlines
opt = bracketed inline <* optional sp

skipopts :: LP ()
skipopts = skipMany opt

inlineText :: LP Inlines
inlineText = str <$> many1 inlineChar

inlineChar :: LP Char
inlineChar = satisfy $ \c ->
  not (c == '\\' || c == '$' || c == '%' || c == '^' || c == '_' ||
       c == '&'  || c == '~' || c == '#' || c == '{' || c == '}' ||
       c == '^'  || c == '\'' || c == '`' || c == '-' || c == ']' ||
       c == ' '  || c == '\t' || c == '\n' )

environment :: LP Blocks
environment = do
  controlSeq "begin"
  name <- braced
  case M.lookup name environments of
       Just p      -> p <|> rawEnv name
       Nothing     -> rawEnv name

rawEnv :: String -> LP Blocks
rawEnv name = do
  let addBegin x = "\\begin{" ++ name ++ "}" ++ x
  parseRaw <- stateParseRaw `fmap` getState
  if parseRaw
     then (rawBlock "latex" . addBegin) <$>
            (withRaw (env name blocks) >>= applyMacros' . snd)
     else env name blocks

-- | Replace "include" commands with file contents.
handleIncludes :: String -> IO String
handleIncludes [] = return []
handleIncludes ('\\':xs) =
  case runParser include defaultParserState "input" ('\\':xs) of
       Right (fs, rest) -> do let getfile f = catch (UTF8.readFile f)
                                               (\_ -> return "")
                              yss <- mapM getfile fs
                              (intercalate "\n" yss ++) `fmap`
                                handleIncludes rest
       _  -> case runParser (verbCmd <|> verbatimEnv) defaultParserState
                   "input" ('\\':xs) of
                    Right (r, rest) -> (r ++) `fmap` handleIncludes rest
                    _               -> ('\\':) `fmap` handleIncludes xs
handleIncludes (x:xs) = (x:) `fmap` handleIncludes xs

include :: LP ([FilePath], String)
include = do
  name <- controlSeq "include" <|> controlSeq "usepackage"
  skipopts
  fs <- (splitBy (==',')) <$> braced
  rest <- getInput
  let fs' = if name == "include"
               then map (flip replaceExtension ".tex") fs
               else map (flip replaceExtension ".sty") fs
  return (fs', rest)

verbCmd :: LP (String, String)
verbCmd = do
  (_,r) <- withRaw $ do
             controlSeq "verb"
             c <- anyChar
             manyTill anyChar (char c)
  rest <- getInput
  return (r, rest)

verbatimEnv :: LP (String, String)
verbatimEnv = do
  (_,r) <- withRaw $ do
             controlSeq "begin"
             name <- braced
             guard $ name == "verbatim" || name == "Verbatim" ||
                     name == "lstlisting" || name == "minted"
             verbEnv name
  rest <- getInput
  return (r,rest)

rawLaTeXBlock :: GenParser Char ParserState String
rawLaTeXBlock = snd <$> withRaw (environment <|> blockCommand)

rawLaTeXInline :: GenParser Char ParserState Inline
rawLaTeXInline = do
  (res, raw) <- withRaw inlineCommand
  if res == mempty
     then return (Str "")
     else RawInline "latex" <$> (applyMacros' raw)

environments :: M.Map String (LP Blocks)
environments = M.fromList
  [ ("document", env "document" blocks <* skipMany anyChar)
  , ("letter", env "letter" letter_contents)
  , ("center", env "center" blocks)
  , ("tabular", env "tabular" simpTable)
  , ("quote", blockQuote <$> env "quote" blocks)
  , ("quotation", blockQuote <$> env "quotation" blocks)
  , ("verse", blockQuote <$> env "verse" blocks)
  , ("itemize", bulletList <$> listenv "itemize" (many item))
  , ("description", definitionList <$> listenv "description" (many descItem))
  , ("enumerate", ordered_list)
  , ("code", failUnlessLHS *>
      (codeBlockWith ("",["sourceCode","literate","haskell"],[]) <$>
        verbEnv "code"))
  , ("verbatim", codeBlock <$> (verbEnv "verbatim"))
  , ("Verbatim", codeBlock <$> (verbEnv "Verbatim"))
  , ("lstlisting", codeBlock <$> (verbEnv "lstlisting"))
  , ("minted", liftA2 (\l c -> codeBlockWith ("",[l],[]) c)
            (grouped (many1 $ satisfy (/= '}'))) (verbEnv "minted"))
  , ("displaymath", mathEnv Nothing "displaymath")
  , ("equation", mathEnv Nothing "equation")
  , ("equation*", mathEnv Nothing "equation*")
  , ("gather", mathEnv (Just "gathered") "gather")
  , ("gather*", mathEnv (Just "gathered") "gather*")
  , ("multline", mathEnv (Just "gathered") "multline")
  , ("multline*", mathEnv (Just "gathered") "multline*")
  , ("eqnarray", mathEnv (Just "aligned") "eqnarray")
  , ("eqnarray*", mathEnv (Just "aligned") "eqnarray*")
  , ("align", mathEnv (Just "aligned") "align")
  , ("align*", mathEnv (Just "aligned") "align*")
  , ("alignat", mathEnv (Just "aligned") "alignat")
  , ("alignat*", mathEnv (Just "aligned") "alignat*")
  ]

letter_contents :: LP Blocks
letter_contents = do
  bs <- blocks
  st <- getState
  -- add signature (author) and address (title)
  let addr = case stateTitle st of
                  []   -> mempty
                  x    -> para $ trimInlines $ fromList x
  updateState $ \s -> s{ stateAuthors = [], stateTitle = [] }
  return $ addr <> bs -- sig added by \closing

closing :: LP Blocks
closing = do
  contents <- tok
  st <- getState
  let sigs = case stateAuthors st of
                  []   -> mempty
                  xs   -> para $ trimInlines $ fromList
                               $ intercalate [LineBreak] xs
  return $ para (trimInlines contents) <> sigs

item :: LP Blocks
item = blocks *> controlSeq "item" *> skipopts *> blocks

loose_item :: LP Blocks
loose_item = do
  ctx <- stateParserContext `fmap` getState
  if ctx == ListItemState
     then mzero
     else return mempty

descItem :: LP (Inlines, [Blocks])
descItem = do
  blocks -- skip blocks before item
  controlSeq "item"
  optional sp
  ils <- opt
  bs <- blocks
  return (ils, [bs])

env :: String -> LP a -> LP a
env name p = p <* (controlSeq "end" *> braced >>= guard . (== name))

listenv :: String -> LP a -> LP a
listenv name p = try $ do
  oldCtx <- stateParserContext `fmap` getState
  updateState $ \st -> st{ stateParserContext = ListItemState }
  res <- env name p
  updateState $ \st -> st{ stateParserContext = oldCtx }
  return res

mathEnv :: Maybe String -> String -> LP Blocks
mathEnv innerEnv name = para <$> mathDisplay (inner <$> verbEnv name)
   where inner x = case innerEnv of
                      Nothing -> x
                      Just y  -> "\\begin{" ++ y ++ "}\n" ++ x ++
                                    "\\end{" ++ y ++ "}"

verbEnv :: String -> LP String
verbEnv name = do
  skipopts
  optional blankline
  let endEnv = try $ controlSeq "end" *> braced >>= guard . (== name)
  res <- manyTill anyChar endEnv
  return $ stripTrailingNewlines res

ordered_list :: LP Blocks
ordered_list = do
  optional sp
  (_, style, delim) <- option (1, DefaultStyle, DefaultDelim) $
                              try $ char '[' *> anyOrderedListMarker <* char ']'
  spaces
  optional $ try $ controlSeq "setlength" *> grouped (controlSeq "itemindent") *> braced
  spaces
  start <- option 1 $ try $ do controlSeq "setcounter"
                               grouped (string "enum" *> many1 (oneOf "iv"))
                               optional sp
                               num <- grouped (many1 digit)
                               spaces
                               return $ (read num + 1 :: Int)
  bs <- listenv "enumerate" (many item)
  return $ orderedListWith (start, style, delim) bs

paragraph :: LP Blocks
paragraph = do
  x <- mconcat <$> many1 inline
  if x == mempty
     then return mempty
     else return $ para $ trimInlines x

preamble :: LP Blocks
preamble = mempty <$> manyTill preambleBlock beginDoc
  where beginDoc = lookAhead $ controlSeq "begin" *> string "{document}"
        preambleBlock =  (mempty <$ comment)
                     <|> (mempty <$ sp)
                     <|> (mempty <$ blanklines)
                     <|> (mempty <$ macro)
                     <|> blockCommand
                     <|> (mempty <$ anyControlSeq)
                     <|> (mempty <$ braced)
                     <|> (mempty <$ anyChar)

-------

-- citations

addPrefix :: [Inline] -> [Citation] -> [Citation]
addPrefix p (k:ks)   = k {citationPrefix = p ++ citationPrefix k} : ks
addPrefix _ _ = []

addSuffix :: [Inline] -> [Citation] -> [Citation]
addSuffix s ks@(_:_) =
  let k  = last ks
      s' = case s of
                (Str (c:_):_)
                  | not (isPunctuation c || isSpace c) -> Str "," : Space : s
                _                                      -> s
  in  init ks ++ [k {citationSuffix = citationSuffix k ++ s'}]
addSuffix _ _ = []

simpleCiteArgs :: LP [Citation]
simpleCiteArgs = try $ do
  first  <- optionMaybe $ toList <$> opt
  second <- optionMaybe $ toList <$> opt
  char '{'
  keys <- manyTill citationLabel (char '}')
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

citationLabel :: LP String
citationLabel  = trim <$>
  (many1 (satisfy $ \c -> c /=',' && c /='}') <* optional (char ',') <* optional sp)

cites :: CitationMode -> Bool -> LP [Citation]
cites mode multi = try $ do
  cits <- if multi
             then many1 simpleCiteArgs
             else count 1 simpleCiteArgs
  let (c:cs) = concat cits
  return $ case mode of
        AuthorInText   -> c {citationMode = mode} : cs
        _              -> map (\a -> a {citationMode = mode}) (c:cs)

citation :: String -> CitationMode -> Bool -> LP Inlines
citation name mode multi = do
  (c,raw) <- withRaw $ cites mode multi
  return $ cite c (rawInline "latex" $ "\\" ++ name ++ raw)

complexNatbibCitation :: CitationMode -> LP Inlines
complexNatbibCitation mode = try $ do
  let ils = (toList . trimInlines . mconcat) <$>
              many (notFollowedBy (oneOf "\\};") >> inline)
  let parseOne = try $ do
                   skipSpaces
                   pref  <- ils
                   cit' <- inline -- expect a citation
                   let citlist = toList cit'
                   cits' <- case citlist of
                                 [Cite cs _] -> return cs
                                 _           -> mzero
                   suff  <- ils
                   skipSpaces
                   optional $ char ';'
                   return $ addPrefix pref $ addSuffix suff $ cits'
  (c:cits, raw) <- withRaw $ grouped parseOne
  return $ cite (c{ citationMode = mode }:cits)
           (rawInline "latex" $ "\\citetext" ++ raw)

-- tables

parseAligns :: LP [Alignment]
parseAligns = try $ do
  char '{'
  optional $ char '|'
  let cAlign = AlignCenter <$ char 'c'
  let lAlign = AlignLeft <$ char 'l'
  let rAlign = AlignRight <$ char 'r'
  let alignChar = optional sp *> (cAlign <|> lAlign <|> rAlign)
  aligns' <- sepEndBy alignChar (optional $ char '|')
  spaces
  char '}'
  spaces
  return aligns'

hline :: LP ()
hline = () <$ (try $ spaces >> controlSeq "hline")

lbreak :: LP ()
lbreak = () <$ (try $ spaces *> controlSeq "\\")

amp :: LP ()
amp = () <$ (try $ spaces *> char '&')

parseTableRow :: Int  -- ^ number of columns
              -> LP [Blocks]
parseTableRow cols = try $ do
  let tableCellInline = notFollowedBy (amp <|> lbreak) >> inline
  let tableCell = (plain . trimInlines . mconcat) <$> many tableCellInline
  cells' <- sepBy tableCell amp
  guard $ length cells' == cols
  spaces
  return cells'

simpTable :: LP Blocks
simpTable = try $ do
  spaces
  aligns <- parseAligns
  let cols = length aligns
  optional hline
  header' <- option [] $ try (parseTableRow cols <* lbreak <* hline)
  rows <- sepEndBy (parseTableRow cols) (lbreak <* optional hline)
  spaces
  let header'' = if null header'
                    then replicate cols mempty
                    else header'
  lookAhead $ controlSeq "end" -- make sure we're at end
  return $ table mempty (zip aligns (repeat 0)) header'' rows

