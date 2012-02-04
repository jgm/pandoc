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
import Data.Char ( chr, ord )
import Control.Monad
import Text.Pandoc.Builder
import Data.Char (isLetter)
import Control.Applicative
import Data.Monoid
import System.FilePath (replaceExtension)
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
        cs   -> string cs <* optional sp
  return name

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

grouped :: Monoid a => LP a -> LP a
grouped parser = try $ char '{' *> (mconcat <$> manyTill parser (char '}'))

braced :: LP String
braced = char '{' *> (concat <$> manyTill
         (  many1 (satisfy (\c -> c /= '\\' && c /= '}' && c /= '{'))
        <|> try (string "\\}")
        <|> try (string "\\{")
        <|> ((\x -> "{" ++ x ++ "}") <$> braced)
        <|> count 1 anyChar
         ) (char '}'))

bracketed :: Monoid a => LP a -> LP a
bracketed parser = try $ char '[' *> (mconcat <$> manyTill parser (char ']'))

trim :: String -> String
trim = removeLeadingTrailingSpace

mathDisplay :: LP String -> LP Inlines
mathDisplay p = displayMath <$> (try p >>= applyMacros' . trim)

mathInline :: LP String -> LP Inlines
mathInline p = math <$> (try p >>= applyMacros')

double_quote :: LP Inlines
double_quote = (doubleQuoted . mconcat) <$>
  (try $ string "``" *> manyTill inline (try $ string "''"))

single_quote :: LP Inlines
single_quote = (singleQuoted . mconcat) <$>
  (try $ char '`' *> manyTill inline (try $ char '\'' >> notFollowedBy letter))

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
     <|> (mathDisplay $ string "$$" *> manyTill anyChar (try $ string "$$"))
     <|> (mathInline  $ char '$' *> manyTill anyChar (char '$'))
     <|> (superscript <$> (char '^' *> tok))
     <|> (subscript <$> (char '_' *> tok))
     <|> (failUnlessLHS *> char '|' *> doLHSverb)
     <|> (str <$> count 1 tildeEscape)
     <|> (str <$> string "]")
     <|> (str <$> count 1 (satisfy (\c -> c /= '\\' && c /='\n' && c /='}' && c /='{'))) -- eat random leftover characters

inlines :: LP Inlines
inlines = mconcat <$> many (notFollowedBy (char '}') *> inline)

block :: LP Blocks
block = (mempty <$ comment)
    <|> (mempty <$ ((spaceChar <|> blankline) *> spaces))
    <|> environment
    <|> mempty <$ macro -- TODO improve macros, make them work everywhere
    <|> blockCommand
    <|> grouped block
    <|> paragraph


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

blockCommands :: M.Map String (LP Blocks)
blockCommands = M.fromList
  [ ("par", pure mempty)
  , ("title", mempty <$ (tok >>= addTitle))
  , ("subtitle", mempty <$ (tok >>= addSubtitle))
  , ("author", mempty <$ authors)
  , ("date", mempty <$ (tok >>= addDate))
  , ("maketitle", pure mempty)
  -- \ignore{} is used conventionally in literate haskell for definitions
  -- that are to be processed by the compiler but not printed.
  , ("ignore", mempty <$ tok)
  , ("hyperdef", mempty <$ (tok *> tok))
  , ("chapter", updateState (\s -> s{ stateHasChapters = True }) *> section 0)
  , ("section", section 1)
  , ("subsection", section 2)
  , ("subsubsection", section 3)
  , ("paragraph", section 4)
  , ("subparagraph", section 5)
  , ("opening", (para . trimInlines) <$> tok)
  , ("closing", (para . trimInlines) <$> tok)
  , ("rule", optional opt *> tok *> tok *> pure horizontalRule)
  , ("begin", mzero)   -- these are here so they won't be interpreted as inline
  , ("end", mzero)
  , ("item", loose_item)
  , ("documentclass", optional opt *> braced *> preamble)
  -- should be parsed by macro, but we need this
  -- here so these aren't parsed as inline
  , ("newcommand", mempty <$ (tok *> optional opt *> tok))
  , ("renewcommand", mempty <$ (tok *> optional opt *> tok))
  , ("newenvironment", mempty <$ (tok *> tok *> tok))
  , ("renewenvironment", mempty <$ (tok *> tok *> tok))
  , ("special", pure mempty)
  , ("pdfannot", pure mempty)
  , ("pdfstringdef", pure mempty)
  , ("index", pure mempty)
  , ("bibliography", pure mempty)
  ]

addTitle :: Inlines -> LP ()
addTitle tit = updateState (\s -> s{ stateTitle = toList tit })

addSubtitle :: Inlines -> LP ()
addSubtitle tit = updateState (\s -> s{ stateTitle = stateTitle s ++
                        toList (str ":" <> linebreak <> tit) })

authors :: LP ()
authors = try $ do
  char '{'
  let oneAuthor = mconcat <$> many1 (notFollowedBy' (controlSeq "and") >> inline)
  auths <- sepBy oneAuthor (controlSeq "and")
  updateState (\s -> s { stateAuthors = map (normalizeSpaces . toList) auths })

addDate :: Inlines -> LP ()
addDate dat = updateState (\s -> s{ stateDate = toList dat })

section :: Int -> LP Blocks
section lvl = do
  hasChapters <- stateHasChapters `fmap` getState
  let lvl' = if hasChapters then lvl + 1 else lvl
  optional sp
  optional opt
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
                           Nothing
                             | parseRaw  ->
                                (rawInline "latex" . (('\\':name') ++)) <$>
                                 (withRaw (optional opt *> many braced)
                                      >>= applyMacros' . snd)
                             | otherwise -> return mempty

isBlockCommand :: String -> Bool
isBlockCommand s = maybe False (const True) $ M.lookup s blockCommands

inlineCommands :: M.Map String (LP Inlines)
inlineCommands = M.fromList
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
  , ("cc", lit "ç")
  , ("cC", lit "Ç")
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
  , ("sect", lit "§")
  , ("`", option (str "`") $ try $ tok >>= accent grave)
  , ("'", option (str "'") $ try $ tok >>= accent acute)
  , ("^", option (str "^") $ try $ tok >>= accent hat)
  , ("~", option (str "~") $ try $ tok >>= accent circ)
  , ("\"", option (str "\"") $ try $ tok >>= accent umlaut)
  , ("i", lit "i")
  , ("\\", linebreak <$ optional (bracketed inline *> optional sp))
  , (",", pure mempty)
  , ("@", pure mempty)
  , (" ", lit "\160")
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
  , ("includegraphics", optional opt *> (unescapeURL <$> braced) >>=
       (\src -> pure (image src "" (str "image"))))
  , ("cite", citation NormalCitation False)
  , ("citep", citation NormalCitation False)
  , ("citep*", citation NormalCitation False)
  , ("citeal", citation NormalCitation False)
  , ("citealp", citation NormalCitation False)
  , ("citealp*", citation NormalCitation False)
  , ("autocite", citation NormalCitation False)
  , ("footcite", citation NormalCitation False)
  , ("parencite", citation NormalCitation False)
  , ("supercite", citation NormalCitation False)
  , ("footcitetext", citation NormalCitation False)
  , ("citeyearpar", citation SuppressAuthor False)
  , ("citeyear", citation SuppressAuthor False)
  , ("autocite*", citation SuppressAuthor False)
  , ("cite*", citation SuppressAuthor False)
  , ("parencite*", citation SuppressAuthor False)
  , ("textcite", citation AuthorInText False)
  , ("citet", citation AuthorInText False)
  , ("citet*", citation AuthorInText False)
  , ("citealt", citation AuthorInText False)
  , ("citealt*", citation AuthorInText False)
  , ("textcites", citation AuthorInText True)
  , ("cites", citation NormalCitation True)
  , ("autocites", citation NormalCitation True)
  , ("footcites", citation NormalCitation True)
  , ("parencites", citation NormalCitation True)
  , ("supercites", citation NormalCitation True)
  , ("footcitetexts", citation NormalCitation True)
  , ("Autocite", citation NormalCitation False)
  , ("Footcite", citation NormalCitation False)
  , ("Parencite", citation NormalCitation False)
  , ("Supercite", citation NormalCitation False)
  , ("Footcitetext", citation NormalCitation False)
  , ("Citeyearpar", citation SuppressAuthor False)
  , ("Citeyear", citation SuppressAuthor False)
  , ("Autocite*", citation SuppressAuthor False)
  , ("Cite*", citation SuppressAuthor False)
  , ("Parencite*", citation SuppressAuthor False)
  , ("Textcite", citation AuthorInText False)
  , ("Textcites", citation AuthorInText True)
  , ("Cites", citation NormalCitation True)
  , ("Autocites", citation NormalCitation True)
  , ("Footcites", citation NormalCitation True)
  , ("Parencites", citation NormalCitation True)
  , ("Supercites", citation NormalCitation True)
  , ("Footcitetexts", citation NormalCitation True)
  , ("citetext", complexNatbibCitation NormalCitation)
  , ("citeauthor", (try (tok *> optional sp *> controlSeq "citetext") *>
                        complexNatbibCitation AuthorInText)
                   <|> citation AuthorInText False)
  ]

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
acute 'a' = 'á'
acute 'e' = 'é'
acute 'i' = 'í'
acute 'o' = 'ó'
acute 'u' = 'ú'
acute c = c

hat :: Char -> Char
hat 'A' = 'Â'
hat 'E' = 'Ê'
hat 'I' = 'Î'
hat 'O' = 'Ô'
hat 'U' = 'Û'
hat 'a' = 'ã'
hat 'e' = 'ê'
hat 'i' = 'î'
hat 'o' = 'ô'
hat 'u' = 'û'
hat c = c

circ :: Char -> Char
circ 'A' = 'Ã'
circ 'O' = 'Õ'
circ 'o' = 'õ'
circ 'N' = 'Ñ'
circ 'n' = 'ñ'
circ c   = c

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

tok :: LP Inlines
tok = try $ grouped inline <|> inlineCommand <|> str <$> (count 1 $ inlineChar)

opt :: LP Inlines
opt = bracketed inline <* optional sp

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
       Right (f, rest) -> do ys <- catch (readFile (replaceExtension f ".tex"))
                                    (\e -> warn
                                      ("could not open included file `" ++
                                       f ++ "': " ++ show e) >> return "")
                             (ys ++) `fmap` handleIncludes rest
       _  -> case runParser verbatimEnv defaultParserState "input" ('\\':xs) of
                    Right (r, rest) -> (r ++) `fmap` handleIncludes rest
                    _               -> ('\\':) `fmap` handleIncludes xs
handleIncludes (x:xs) = (x:) `fmap` handleIncludes xs

include :: LP (FilePath, String)
include = do
  controlSeq "include"
  f <- braced
  rest <- getInput
  return (f, rest)

verbatimEnv :: LP (String, String)
verbatimEnv = do
  (_,r) <- withRaw $ do
             controlSeq "begin"
             name <- braced
             guard $ name == "verbatim" || name == "Verbatim" ||
                     name == "lstlisting"
             verbEnv name
  rest <- getInput
  return (r,rest)

-- | Parse any LaTeX environment and return a string containing
-- the whole literal environment as raw TeX.
rawLaTeXBlock :: GenParser Char ParserState String
rawLaTeXBlock =
  (rawLaTeXEnvironment <|> (snd <$> withRaw blockCommand)) >>= applyMacros'

rawLaTeXEnvironment :: GenParser Char ParserState String
rawLaTeXEnvironment = try $ do
  controlSeq "begin"
  name <- braced
  let addBegin x = "\\begin{" ++ name ++ "}" ++ x
  addBegin <$> (withRaw (env name blocks) >>= applyMacros' . snd)

rawLaTeXInline :: GenParser Char ParserState Inline
rawLaTeXInline = do
  (res, raw) <- withRaw inlineCommand
  if res == mempty
     then return (Str "")
     else RawInline "latex" <$> (applyMacros' raw)

environments :: M.Map String (LP Blocks)
environments = M.fromList
  [ ("document", env "document" blocks)
  , ("letter", env "letter" blocks)
  , ("center", env "center" blocks)
  , ("tabular", env "tabular" simpTable)
  , ("quote", blockQuote <$> env "quote" blocks)
  , ("quotation", blockQuote <$> env "quotation" blocks)
  , ("itemize", bulletList <$> listenv "itemize" (many item))
  , ("description", definitionList <$> listenv "description" (many descItem))
  , ("enumerate", ordered_list)
  , ("code", failUnlessLHS *>
      (codeBlockWith ("",["sourceCode","literate","haskell"],[]) <$>
        verbEnv "code"))
  , ("verbatim", codeBlock <$> (verbEnv "verbatim"))
  , ("Verbatim", codeBlock <$> (verbEnv "Verbatim"))
  , ("lstlisting", codeBlock <$> (verbEnv "listlisting"))
  , ("displaymath", mathEnv Nothing "displaymath")
  , ("equation", mathEnv Nothing "equation")
  , ("equation*", mathEnv Nothing "equation*")
  , ("gather", mathEnv (Just "gathered") "gather")
  , ("gather*", mathEnv (Just "gathered") "gather*")
  , ("multiline", mathEnv (Just "gathered") "multiline")
  , ("multiline*", mathEnv (Just "gathered") "multiline*")
  , ("eqnarray", mathEnv (Just "aligned*") "eqnarray")
  , ("eqnarray*", mathEnv (Just "aligned*") "eqnarray*")
  , ("align", mathEnv (Just "aligned*") "align")
  , ("align*", mathEnv (Just "aligned*") "align*")
  , ("alignat", mathEnv (Just "aligned*") "alignat")
  , ("alignat*", mathEnv (Just "aligned*") "alignat*")
  ]

item :: LP Blocks
item = blocks *> controlSeq "item" *> optional opt *> blocks

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
  optional opt
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

addPrefix :: Inlines -> [Citation] -> [Citation]
addPrefix p (k:ks)   = k {citationPrefix = toList p ++ citationPrefix k} : ks
addPrefix _ _ = []

addSuffix :: Inlines -> [Citation] -> [Citation]
addSuffix s ks@(_:_) =
  let k = last ks
  in  init ks ++ [k {citationSuffix = citationSuffix k ++ toList s}]
addSuffix _ _ = []

simpleCiteArgs :: LP [Citation]
simpleCiteArgs = try $ do
  first  <- optionMaybe opt
  second <- optionMaybe opt
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

citation :: CitationMode -> Bool -> LP Inlines
citation mode multi = (flip cite mempty) <$> cites mode multi

complexNatbibCitation :: CitationMode -> LP Inlines
complexNatbibCitation mode = try $ do
  let ils = (trimInlines . mconcat) <$>
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
  (c:cits) <- grouped parseOne
  return $ cite (c{ citationMode = mode }:cits) mempty

-- tables

parseAligns :: LP [Alignment]
parseAligns = try $ do
  char '{'
  optional $ char '|'
  let cAlign = char 'c' >> return AlignCenter
  let lAlign = char 'l' >> return AlignLeft
  let rAlign = char 'r' >> return AlignRight
  let alignChar = optional sp *> (cAlign <|> lAlign <|> rAlign)
  aligns' <- sepEndBy alignChar (optional $ char '|')
  spaces
  char '}'
  spaces
  return aligns'

hline :: LP ()
hline = () <$ (try $ spaces >> controlSeq "hline")

parseTableRow :: Int  -- ^ number of columns
              -> LP [Blocks]
parseTableRow cols = try $ do
  let amp = try $ spaces *> string "&"
  let tableCellInline = notFollowedBy (amp <|> controlSeq "\\") >> inline
  cells' <- sepBy ((plain . trimInlines . mconcat) <$> many tableCellInline) amp
  guard $ length cells' == cols
  spaces
  optional $ controlSeq "\\"
  return cells'

parseTableHeader :: Int   -- ^ number of columns
                 -> LP [Blocks]
parseTableHeader cols = try $ parseTableRow cols <* hline

simpTable :: LP Blocks
simpTable = try $ do
  spaces
  aligns <- parseAligns
  let cols = length aligns
  optional hline
  header' <- option [] $ parseTableHeader cols
  rows <- many (parseTableRow cols <* optional hline)
  spaces
  let header'' = if null header'
                    then replicate cols mempty
                    else header'
  return $ table mempty (zip aligns (repeat 0)) header'' rows

