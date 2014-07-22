{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-
Copyright (C) 2006-2014 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2014 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of LaTeX to 'Pandoc' document.
-}
module Text.Pandoc.Readers.LaTeX ( readLaTeX,
                                   rawLaTeXInline,
                                   rawLaTeXBlock,
                                   inlineCommand,
                                   handleIncludes
                                 ) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.Shared
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding ((<|>), many, optional, space,
                                   mathDisplay, mathInline)
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Char ( chr, ord )
import Control.Monad.Trans (lift)
import Control.Monad
import Text.Pandoc.Builder
import Data.Char (isLetter, isAlphaNum)
import Control.Applicative
import Data.Monoid
import Data.Maybe (fromMaybe)
import System.Environment (getEnv)
import System.FilePath (replaceExtension, (</>))
import Data.List (intercalate, intersperse)
import qualified Data.Map as M
import qualified Control.Exception as E
import System.FilePath (takeExtension, addExtension)
import Text.Pandoc.Highlighting (fromListingsLanguage)

-- | Parse LaTeX from string and return 'Pandoc' document.
readLaTeX :: ReaderOptions -- ^ Reader options
          -> String        -- ^ String to parse (assumes @'\n'@ line endings)
          -> Pandoc
readLaTeX opts = readWith parseLaTeX def{ stateOptions = opts }

parseLaTeX :: LP Pandoc
parseLaTeX = do
  bs <- blocks
  eof
  st <- getState
  let meta = stateMeta st
  let (Pandoc _ bs') = doc bs
  return $ Pandoc meta bs'

type LP = Parser [Char] ParserState

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
        <|> (try $ newline <* lookAhead anyChar <* notFollowedBy blankline)

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
  optional newline
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

mathDisplay :: LP String -> LP Inlines
mathDisplay p = displayMath <$> (try p >>= applyMacros' . trim)

mathInline :: LP String -> LP Inlines
mathInline p = math <$> (try p >>= applyMacros')

mathChars :: LP String
mathChars = concat <$>
  many (   many1 (satisfy (\c -> c /= '$' && c /='\\'))
      <|> (\c -> ['\\',c]) <$> (try $ char '\\' *> anyChar)
       )

quoted' :: (Inlines -> Inlines) -> LP String -> LP () -> LP Inlines
quoted' f starter ender = do
  startchs <- starter
  try ((f . mconcat) <$> manyTill inline ender) <|> lit startchs

double_quote :: LP Inlines
double_quote =
  (   quoted' doubleQuoted (try $ string "``") (void $ try $ string "''")
  <|> quoted' doubleQuoted (string "“")        (void $ char '”')
  -- the following is used by babel for localized quotes:
  <|> quoted' doubleQuoted (try $ string "\"`") (void $ try $ string "\"'")
  <|> quoted' doubleQuoted (string "\"")       (void $ char '"')
  )

single_quote :: LP Inlines
single_quote =
  (  quoted' singleQuoted (string "`") (try $ char '\'' >> notFollowedBy letter)
  <|> quoted' singleQuoted (string "‘") (try $ char '’' >> notFollowedBy letter)
  )

inline :: LP Inlines
inline = (mempty <$ comment)
     <|> (space  <$ sp)
     <|> inlineText
     <|> inlineCommand
     <|> inlineGroup
     <|> (char '-' *> option (str "-")
           ((char '-') *> option (str "–") (str "—" <$ char '-')))
     <|> double_quote
     <|> single_quote
     <|> (str "”" <$ try (string "''"))
     <|> (str "”" <$ char '”')
     <|> (str "’" <$ char '\'')
     <|> (str "’" <$ char '’')
     <|> (str "\160" <$ char '~')
     <|> (mathDisplay $ string "$$" *> mathChars <* string "$$")
     <|> (mathInline  $ char '$' *> mathChars <* char '$')
     <|> (superscript <$> (char '^' *> tok))
     <|> (subscript <$> (char '_' *> tok))
     <|> (guardEnabled Ext_literate_haskell *> char '|' *> doLHSverb)
     <|> (str . (:[]) <$> tildeEscape)
     <|> (str . (:[]) <$> oneOf "[]")
     <|> (str . (:[]) <$> oneOf "#&") -- TODO print warning?
     -- <|> (str <$> count 1 (satisfy (\c -> c /= '\\' && c /='\n' && c /='}' && c /='{'))) -- eat random leftover characters

inlines :: LP Inlines
inlines = mconcat <$> many (notFollowedBy (char '}') *> inline)

inlineGroup :: LP Inlines
inlineGroup = do
  ils <- grouped inline
  if isNull ils
     then return mempty
     else return $ spanWith nullAttr ils
          -- we need the span so we can detitlecase bibtex entries;
          -- we need to know when something is {C}apitalized

block :: LP Blocks
block = (mempty <$ comment)
    <|> (mempty <$ ((spaceChar <|> newline) *> spaces))
    <|> environment
    <|> macro
    <|> blockCommand
    <|> paragraph
    <|> grouped block
    <|> (mempty <$ char '&')  -- loose & in table environment


blocks :: LP Blocks
blocks = mconcat <$> many block

blockCommand :: LP Blocks
blockCommand = try $ do
  name <- anyControlSeq
  guard $ name /= "begin" && name /= "end"
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
                 (getOption readerParseRaw >>= guard >> (withRaw optargs))

ignoreBlocks :: String -> (String, LP Blocks)
ignoreBlocks name = (name, doraw <|> (mempty <$ optargs))
  where optargs = skipopts *> skipMany (try $ optional sp *> braced)
        contseq = '\\':name
        doraw = (rawBlock "latex" . (contseq ++) . snd) <$>
                 (getOption readerParseRaw >>= guard >> (withRaw optargs))

blockCommands :: M.Map String (LP Blocks)
blockCommands = M.fromList $
  [ ("par", mempty <$ skipopts)
  , ("title", mempty <$ (skipopts *> tok >>= addMeta "title"))
  , ("subtitle", mempty <$ (skipopts *> tok >>= addMeta "subtitle"))
  , ("author", mempty <$ (skipopts *> authors))
  -- -- in letter class, temp. store address & sig as title, author
  , ("address", mempty <$ (skipopts *> tok >>= addMeta "address"))
  , ("signature", mempty <$ (skipopts *> authors))
  , ("date", mempty <$ (skipopts *> tok >>= addMeta "date"))
  -- sectioning
  , ("chapter", updateState (\s -> s{ stateHasChapters = True })
                      *> section nullAttr 0)
  , ("chapter*", updateState (\s -> s{ stateHasChapters = True })
                      *> section ("",["unnumbered"],[]) 0)
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
  --
  , ("hrule", pure horizontalRule)
  , ("rule", skipopts *> tok *> tok *> pure horizontalRule)
  , ("item", skipopts *> loose_item)
  , ("documentclass", skipopts *> braced *> preamble)
  , ("centerline", (para . trimInlines) <$> (skipopts *> tok))
  , ("caption", skipopts *> tok >>= setCaption)
  , ("PandocStartInclude", startInclude)
  , ("PandocEndInclude", endInclude)
  , ("bibliography", mempty <$ (skipopts *> braced >>=
                                addMeta "bibliography" . splitBibs))
  , ("addbibresource", mempty <$ (skipopts *> braced >>=
                                addMeta "bibliography" . splitBibs))
  ] ++ map ignoreBlocks
  -- these commands will be ignored unless --parse-raw is specified,
  -- in which case they will appear as raw latex blocks
  [ "newcommand", "renewcommand", "newenvironment", "renewenvironment"
    -- newcommand, etc. should be parsed by macro, but we need this
    -- here so these aren't parsed as inline commands to ignore
  , "special", "pdfannot", "pdfstringdef"
  , "bibliographystyle"
  , "maketitle", "makeindex", "makeglossary"
  , "addcontentsline", "addtocontents", "addtocounter"
     -- \ignore{} is used conventionally in literate haskell for definitions
     -- that are to be processed by the compiler but not printed.
  , "ignore"
  , "hyperdef"
  , "markboth", "markright", "markleft"
  , "hspace", "vspace"
  ]

addMeta :: ToMetaValue a => String -> a -> LP ()
addMeta field val = updateState $ \st ->
  st{ stateMeta = addMetaField field val $ stateMeta st }

splitBibs :: String -> [Inlines]
splitBibs = map (str . flip replaceExtension "bib" . trim) . splitBy (==',')

setCaption :: Inlines -> LP Blocks
setCaption ils = do
  updateState $ \st -> st{ stateCaption = Just ils }
  return mempty

resetCaption :: LP ()
resetCaption = updateState $ \st -> st{ stateCaption = Nothing }

authors :: LP ()
authors = try $ do
  char '{'
  let oneAuthor = mconcat <$>
       many1 (notFollowedBy' (controlSeq "and") >>
               (inline <|> mempty <$ blockCommand))
               -- skip e.g. \vspace{10pt}
  auths <- sepBy oneAuthor (controlSeq "and")
  char '}'
  addMeta "author" (map trimInlines auths)

section :: Attr -> Int -> LP Blocks
section (ident, classes, kvs) lvl = do
  hasChapters <- stateHasChapters `fmap` getState
  let lvl' = if hasChapters then lvl + 1 else lvl
  skipopts
  contents <- grouped inline
  lab <- option ident $ try (spaces >> controlSeq "label" >> spaces >> braced)
  attr' <- registerHeader (lab, classes, kvs) contents
  return $ headerWith attr' lvl' contents

inlineCommand :: LP Inlines
inlineCommand = try $ do
  name <- anyControlSeq
  guard $ name /= "begin" && name /= "end"
  guard $ not $ isBlockCommand name
  parseRaw <- getOption readerParseRaw
  star <- option "" (string "*")
  let name' = name ++ star
  let raw = do
        rawargs <- withRaw (skipopts *> option "" dimenarg *> many braced)
        let rawcommand = '\\' : name ++ star ++ snd rawargs
        transformed <- applyMacros' rawcommand
        if transformed /= rawcommand
           then parseFromString inlines transformed
           else if parseRaw
                   then return $ rawInline "latex" rawcommand
                   else return mempty
  case M.lookup name' inlineCommands of
       Just p      -> p <|> raw
       Nothing     -> case M.lookup name inlineCommands of
                           Just p    -> p <|> raw
                           Nothing   -> raw

unlessParseRaw :: LP ()
unlessParseRaw = getOption readerParseRaw >>= guard . not

isBlockCommand :: String -> Bool
isBlockCommand s = maybe False (const True) $ M.lookup s blockCommands

inlineCommands :: M.Map String (LP Inlines)
inlineCommands = M.fromList $
  [ ("emph", extractSpaces emph <$> tok)
  , ("textit", extractSpaces emph <$> tok)
  , ("textsl", extractSpaces emph <$> tok)
  , ("textsc", extractSpaces smallcaps <$> tok)
  , ("sout", extractSpaces strikeout <$> tok)
  , ("textsuperscript", extractSpaces superscript <$> tok)
  , ("textsubscript", extractSpaces subscript <$> tok)
  , ("textbackslash", lit "\\")
  , ("backslash", lit "\\")
  , ("slash", lit "/")
  , ("textbf", extractSpaces strong <$> tok)
  , ("textnormal", extractSpaces (spanWith ("",["nodecor"],[])) <$> tok)
  , ("ldots", lit "…")
  , ("dots", lit "…")
  , ("mdots", lit "…")
  , ("sim", lit "~")
  , ("label", unlessParseRaw >> (inBrackets <$> tok))
  , ("ref", unlessParseRaw >> (inBrackets <$> tok))
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
  , ("em", extractSpaces emph <$> inlines)
  , ("it", extractSpaces emph <$> inlines)
  , ("sl", extractSpaces emph <$> inlines)
  , ("bf", extractSpaces strong <$> inlines)
  , ("rm", inlines)
  , ("itshape", extractSpaces emph <$> inlines)
  , ("slshape", extractSpaces emph <$> inlines)
  , ("scshape", extractSpaces smallcaps <$> inlines)
  , ("bfseries", extractSpaces strong <$> inlines)
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
  , ("`", option (str "`") $ try $ tok >>= accent grave)
  , ("'", option (str "'") $ try $ tok >>= accent acute)
  , ("^", option (str "^") $ try $ tok >>= accent circ)
  , ("~", option (str "~") $ try $ tok >>= accent tilde)
  , ("\"", option (str "\"") $ try $ tok >>= accent umlaut)
  , (".", option (str ".") $ try $ tok >>= accent dot)
  , ("=", option (str "=") $ try $ tok >>= accent macron)
  , ("c", option (str "c") $ try $ tok >>= accent cedilla)
  , ("v", option (str "v") $ try $ tok >>= accent hacek)
  , ("u", option (str "u") $ try $ tok >>= accent breve)
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
  , ("Verb", doverb)
  , ("texttt", (code . stringify . toList) <$> tok)
  , ("url", (unescapeURL <$> braced) >>= \url ->
       pure (link url "" (str url)))
  , ("href", (unescapeURL <$> braced <* optional sp) >>= \url ->
       tok >>= \lab ->
         pure (link url "" lab))
  , ("includegraphics", skipopts *> (unescapeURL <$> braced) >>= mkImage)
  , ("enquote", enquote)
  , ("cite", citation "cite" AuthorInText False)
  , ("citep", citation "citep" NormalCitation False)
  , ("citep*", citation "citep*" NormalCitation False)
  , ("citeal", citation "citeal" NormalCitation False)
  , ("citealp", citation "citealp" NormalCitation False)
  , ("citealp*", citation "citealp*" NormalCitation False)
  , ("autocite", citation "autocite" NormalCitation False)
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
  , ("Footcite", citation "Footcite" NormalCitation False)
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
  , ("Footcites", citation "Footcites" NormalCitation True)
  , ("Parencites", citation "Parencites" NormalCitation True)
  , ("Supercites", citation "Supercites" NormalCitation True)
  , ("Footcitetexts", inNote <$> citation "Footcitetexts" NormalCitation True)
  , ("citetext", complexNatbibCitation NormalCitation)
  , ("citeauthor", (try (tok *> optional sp *> controlSeq "citetext") *>
                        complexNatbibCitation AuthorInText)
                   <|> citation "citeauthor" AuthorInText False)
  , ("nocite", mempty <$ (citation "nocite" NormalCitation False >>=
                          addMeta "nocite"))
  ] ++ map ignoreInlines
  -- these commands will be ignored unless --parse-raw is specified,
  -- in which case they will appear as raw latex blocks:
  [ "noindent", "index" ]

mkImage :: String -> LP Inlines
mkImage src = do
   let alt = str "image"
   case takeExtension src of
        "" -> do
              defaultExt <- getOption readerDefaultImageExtension
              return $ image (addExtension src defaultExt) "" alt
        _  -> return $ image src "" alt

inNote :: Inlines -> Inlines
inNote ils =
  note $ para $ ils <> str "."

unescapeURL :: String -> String
unescapeURL ('\\':x:xs) | isEscapable x = x:unescapeURL xs
  where isEscapable c = c `elem` "#$%&~_^\\{}"
unescapeURL (x:xs) = x:unescapeURL xs
unescapeURL [] = ""

enquote :: LP Inlines
enquote = do
  skipopts
  context <- stateQuoteContext <$> getState
  if context == InDoubleQuote
     then singleQuoted <$> withQuoteContext InSingleQuote tok
     else doubleQuoted <$> withQuoteContext InDoubleQuote tok

doverb :: LP Inlines
doverb = do
  marker <- anyChar
  code <$> manyTill (satisfy (/='\n')) (char marker)

doLHSverb :: LP Inlines
doLHSverb = codeWith ("",["haskell"],[]) <$> manyTill (satisfy (/='\n')) (char '|')

lit :: String -> LP Inlines
lit = pure . str

accent :: (Char -> String) -> Inlines -> LP Inlines
accent f ils =
  case toList ils of
       (Str (x:xs) : ys) -> return $ fromList $ (Str (f x ++ xs) : ys)
       []                -> mzero
       _                 -> return ils

grave :: Char -> String
grave 'A' = "À"
grave 'E' = "È"
grave 'I' = "Ì"
grave 'O' = "Ò"
grave 'U' = "Ù"
grave 'a' = "à"
grave 'e' = "è"
grave 'i' = "ì"
grave 'o' = "ò"
grave 'u' = "ù"
grave c   = [c]

acute :: Char -> String
acute 'A' = "Á"
acute 'E' = "É"
acute 'I' = "Í"
acute 'O' = "Ó"
acute 'U' = "Ú"
acute 'Y' = "Ý"
acute 'a' = "á"
acute 'e' = "é"
acute 'i' = "í"
acute 'o' = "ó"
acute 'u' = "ú"
acute 'y' = "ý"
acute 'C' = "Ć"
acute 'c' = "ć"
acute 'L' = "Ĺ"
acute 'l' = "ĺ"
acute 'N' = "Ń"
acute 'n' = "ń"
acute 'R' = "Ŕ"
acute 'r' = "ŕ"
acute 'S' = "Ś"
acute 's' = "ś"
acute 'Z' = "Ź"
acute 'z' = "ź"
acute c   = [c]

circ :: Char -> String
circ 'A' = "Â"
circ 'E' = "Ê"
circ 'I' = "Î"
circ 'O' = "Ô"
circ 'U' = "Û"
circ 'a' = "â"
circ 'e' = "ê"
circ 'i' = "î"
circ 'o' = "ô"
circ 'u' = "û"
circ 'C' = "Ĉ"
circ 'c' = "ĉ"
circ 'G' = "Ĝ"
circ 'g' = "ĝ"
circ 'H' = "Ĥ"
circ 'h' = "ĥ"
circ 'J' = "Ĵ"
circ 'j' = "ĵ"
circ 'S' = "Ŝ"
circ 's' = "ŝ"
circ 'W' = "Ŵ"
circ 'w' = "ŵ"
circ 'Y' = "Ŷ"
circ 'y' = "ŷ"
circ c   = [c]

tilde :: Char -> String
tilde 'A' = "Ã"
tilde 'a' = "ã"
tilde 'O' = "Õ"
tilde 'o' = "õ"
tilde 'I' = "Ĩ"
tilde 'i' = "ĩ"
tilde 'U' = "Ũ"
tilde 'u' = "ũ"
tilde 'N' = "Ñ"
tilde 'n' = "ñ"
tilde c   = [c]

umlaut :: Char -> String
umlaut 'A' = "Ä"
umlaut 'E' = "Ë"
umlaut 'I' = "Ï"
umlaut 'O' = "Ö"
umlaut 'U' = "Ü"
umlaut 'a' = "ä"
umlaut 'e' = "ë"
umlaut 'i' = "ï"
umlaut 'o' = "ö"
umlaut 'u' = "ü"
umlaut c   = [c]

dot :: Char -> String
dot 'C' = "Ċ"
dot 'c' = "ċ"
dot 'E' = "Ė"
dot 'e' = "ė"
dot 'G' = "Ġ"
dot 'g' = "ġ"
dot 'I' = "İ"
dot 'Z' = "Ż"
dot 'z' = "ż"
dot c   = [c]

macron :: Char -> String
macron 'A' = "Ā"
macron 'E' = "Ē"
macron 'I' = "Ī"
macron 'O' = "Ō"
macron 'U' = "Ū"
macron 'a' = "ā"
macron 'e' = "ē"
macron 'i' = "ī"
macron 'o' = "ō"
macron 'u' = "ū"
macron c   = [c]

cedilla :: Char -> String
cedilla 'c' = "ç"
cedilla 'C' = "Ç"
cedilla 's' = "ş"
cedilla 'S' = "Ş"
cedilla 't' = "ţ"
cedilla 'T' = "Ţ"
cedilla 'e' = "ȩ"
cedilla 'E' = "Ȩ"
cedilla 'h' = "ḩ"
cedilla 'H' = "Ḩ"
cedilla 'o' = "o̧"
cedilla 'O' = "O̧"
cedilla c   = [c]

hacek :: Char -> String
hacek 'A' = "Ǎ"
hacek 'a' = "ǎ"
hacek 'C' = "Č"
hacek 'c' = "č"
hacek 'D' = "Ď"
hacek 'd' = "ď"
hacek 'E' = "Ě"
hacek 'e' = "ě"
hacek 'G' = "Ǧ"
hacek 'g' = "ǧ"
hacek 'H' = "Ȟ"
hacek 'h' = "ȟ"
hacek 'I' = "Ǐ"
hacek 'i' = "ǐ"
hacek 'j' = "ǰ"
hacek 'K' = "Ǩ"
hacek 'k' = "ǩ"
hacek 'L' = "Ľ"
hacek 'l' = "ľ"
hacek 'N' = "Ň"
hacek 'n' = "ň"
hacek 'O' = "Ǒ"
hacek 'o' = "ǒ"
hacek 'R' = "Ř"
hacek 'r' = "ř"
hacek 'S' = "Š"
hacek 's' = "š"
hacek 'T' = "Ť"
hacek 't' = "ť"
hacek 'U' = "Ǔ"
hacek 'u' = "ǔ"
hacek 'Z' = "Ž"
hacek 'z' = "ž"
hacek c   = [c]

breve :: Char -> String
breve 'A' = "Ă"
breve 'a' = "ă"
breve 'E' = "Ĕ"
breve 'e' = "ĕ"
breve 'G' = "Ğ"
breve 'g' = "ğ"
breve 'I' = "Ĭ"
breve 'i' = "ĭ"
breve 'O' = "Ŏ"
breve 'o' = "ŏ"
breve 'U' = "Ŭ"
breve 'u' = "ŭ"
breve c   = [c]

tok :: LP Inlines
tok = try $ grouped inline <|> inlineCommand <|> str <$> (count 1 $ inlineChar)

opt :: LP Inlines
opt = bracketed inline <* optional sp

skipopts :: LP ()
skipopts = skipMany opt

inlineText :: LP Inlines
inlineText = str <$> many1 inlineChar

inlineChar :: LP Char
inlineChar = noneOf "\\$%^_&~#{}^'`\"‘’“”-[] \t\n"

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
  parseRaw <- getOption readerParseRaw
  if parseRaw
     then (rawBlock "latex" . addBegin) <$>
            (withRaw (env name blocks) >>= applyMacros' . snd)
     else env name blocks

----

type IncludeParser = ParserT [Char] [String] IO String

-- | Replace "include" commands with file contents.
handleIncludes :: String -> IO String
handleIncludes s = do
  res <- runParserT includeParser' [] "input" s
  case res of
       Right s'    -> return s'
       Left e      -> error $ show e

includeParser' :: IncludeParser
includeParser' =
  concat <$> many (comment' <|> escaped' <|> blob' <|> include'
                   <|> startMarker' <|> endMarker'
                   <|> verbCmd' <|> verbatimEnv' <|> backslash')

comment' :: IncludeParser
comment' = do
  char '%'
  xs <- manyTill anyChar newline
  return ('%':xs ++ "\n")

escaped' :: IncludeParser
escaped' = try $ string "\\%" <|> string "\\\\"

verbCmd' :: IncludeParser
verbCmd' = fmap snd <$>
  withRaw $ try $ do
             string "\\verb"
             c <- anyChar
             manyTill anyChar (char c)

verbatimEnv' :: IncludeParser
verbatimEnv' = fmap snd <$>
  withRaw $ try $ do
             string "\\begin"
             name <- braced'
             guard $ name `elem` ["verbatim", "Verbatim", "lstlisting",
                                  "minted", "alltt"]
             manyTill anyChar (try $ string $ "\\end{" ++ name ++ "}")

blob' :: IncludeParser
blob' = try $ many1 (noneOf "\\%")

backslash' :: IncludeParser
backslash' = string "\\"

braced' :: IncludeParser
braced' = try $ char '{' *> manyTill (satisfy (/='}')) (char '}')

include' :: IncludeParser
include' = do
  fs' <- try $ do
              char '\\'
              name <- try (string "include")
                  <|> try (string "input")
                  <|> string "usepackage"
              -- skip options
              skipMany $ try $ char '[' *> (manyTill anyChar (char ']'))
              fs <- (map trim . splitBy (==',')) <$> braced'
              return $ if name == "usepackage"
                          then map (flip replaceExtension ".sty") fs
                          else map (flip replaceExtension ".tex") fs
  pos <- getPosition
  containers <- getState
  let fn = case containers of
                (f':_) -> f'
                []     -> "input"
  -- now process each include file in order...
  rest <- getInput
  results' <- forM fs' (\f -> do
    when (f `elem` containers) $
      fail "Include file loop!"
    contents <- lift $ readTeXFile f
    return $ "\\PandocStartInclude{" ++ f ++ "}" ++
             contents ++ "\\PandocEndInclude{" ++
             fn ++ "}{" ++ show (sourceLine pos) ++ "}{"
             ++ show (sourceColumn pos) ++ "}")
  setInput $ concat results' ++ rest
  return ""

startMarker' :: IncludeParser
startMarker' = try $ do
  string "\\PandocStartInclude"
  fn <- braced'
  updateState (fn:)
  setPosition $ newPos fn 1 1
  return $ "\\PandocStartInclude{" ++ fn ++ "}"

endMarker' :: IncludeParser
endMarker' = try $ do
  string "\\PandocEndInclude"
  fn <- braced'
  ln <- braced'
  co <- braced'
  updateState tail
  setPosition $ newPos fn (fromMaybe 1 $ safeRead ln) (fromMaybe 1 $ safeRead co)
  return $ "\\PandocEndInclude{" ++ fn ++ "}{" ++ ln ++ "}{" ++
               co ++ "}"

readTeXFile :: FilePath -> IO String
readTeXFile f = do
  texinputs <- E.catch (getEnv "TEXINPUTS") $ \(_ :: E.SomeException) ->
                   return "."
  let ds = splitBy (==':') texinputs
  readFileFromDirs ds f

readFileFromDirs :: [FilePath] -> FilePath -> IO String
readFileFromDirs [] _ = return ""
readFileFromDirs (d:ds) f =
  E.catch (UTF8.readFile $ d </> f) $ \(_ :: E.SomeException) ->
    readFileFromDirs ds f

----

keyval :: LP (String, String)
keyval = try $ do
  key <- many1 alphaNum
  val <- option "" $ char '=' >> many1 alphaNum
  skipMany spaceChar
  optional (char ',')
  skipMany spaceChar
  return (key, val)


keyvals :: LP [(String, String)]
keyvals = try $ char '[' *> manyTill keyval (char ']')

alltt :: String -> LP Blocks
alltt t = walk strToCode <$> parseFromString blocks
  (substitute " " "\\ " $ substitute "%" "\\%" $
   concat $ intersperse "\\\\\n" $ lines t)
  where strToCode (Str s) = Code nullAttr s
        strToCode x       = x

rawLaTeXBlock :: Parser [Char] ParserState String
rawLaTeXBlock = snd <$> try (withRaw (environment <|> blockCommand))

rawLaTeXInline :: Parser [Char] ParserState Inline
rawLaTeXInline = do
  raw <- (snd <$> withRaw inlineCommand) <|> (snd <$> withRaw blockCommand)
  RawInline "latex" <$> applyMacros' raw

addImageCaption :: Blocks -> LP Blocks
addImageCaption = walkM go
  where go (Image alt (src,tit)) = do
          mbcapt <- stateCaption <$> getState
          case mbcapt of
               Just ils -> return (Image (toList ils) (src, "fig:"))
               Nothing  -> return (Image alt (src,tit))
        go x = return x

addTableCaption :: Blocks -> LP Blocks
addTableCaption = walkM go
  where go (Table c als ws hs rs) = do
          mbcapt <- stateCaption <$> getState
          case mbcapt of
               Just ils -> return (Table (toList ils) als ws hs rs)
               Nothing  -> return (Table c als ws hs rs)
        go x = return x

environments :: M.Map String (LP Blocks)
environments = M.fromList
  [ ("document", env "document" blocks <* skipMany anyChar)
  , ("letter", env "letter" letter_contents)
  , ("figure", env "figure" $
         resetCaption *> skipopts *> blocks >>= addImageCaption)
  , ("center", env "center" blocks)
  , ("table",  env "table" $
         resetCaption *> skipopts *> blocks >>= addTableCaption)
  , ("tabular", env "tabular" simpTable)
  , ("quote", blockQuote <$> env "quote" blocks)
  , ("quotation", blockQuote <$> env "quotation" blocks)
  , ("verse", blockQuote <$> env "verse" blocks)
  , ("itemize", bulletList <$> listenv "itemize" (many item))
  , ("description", definitionList <$> listenv "description" (many descItem))
  , ("enumerate", ordered_list)
  , ("alltt", alltt =<< verbEnv "alltt")
  , ("code", guardEnabled Ext_literate_haskell *>
      (codeBlockWith ("",["sourceCode","literate","haskell"],[]) <$>
        verbEnv "code"))
  , ("verbatim", codeBlock <$> (verbEnv "verbatim"))
  , ("Verbatim",   do options <- option [] keyvals
                      let kvs = [ (if k == "firstnumber"
                                      then "startFrom"
                                      else k, v) | (k,v) <- options ]
                      let classes = [ "numberLines" |
                                      lookup "numbers" options == Just "left" ]
                      let attr = ("",classes,kvs)
                      codeBlockWith attr <$> (verbEnv "Verbatim"))
  , ("lstlisting", do options <- option [] keyvals
                      let kvs = [ (if k == "firstnumber"
                                      then "startFrom"
                                      else k, v) | (k,v) <- options ]
                      let classes = [ "numberLines" |
                                      lookup "numbers" options == Just "left" ]
                                 ++ maybe [] (:[]) (lookup "language" options
                                         >>= fromListingsLanguage)
                      let attr = (fromMaybe "" (lookup "label" options),classes,kvs)
                      codeBlockWith attr <$> (verbEnv "lstlisting"))
  , ("minted",     do options <- option [] keyvals
                      lang <- grouped (many1 $ satisfy (/='}'))
                      let kvs = [ (if k == "firstnumber"
                                      then "startFrom"
                                      else k, v) | (k,v) <- options ]
                      let classes = [ lang | not (null lang) ] ++
                                    [ "numberLines" |
                                      lookup "linenos" options == Just "true" ]
                      let attr = ("",classes,kvs)
                      codeBlockWith attr <$> (verbEnv "minted"))
  , ("obeylines", parseFromString
                  (para . trimInlines . mconcat <$> many inline) =<<
                  intercalate "\\\\\n" . lines <$> verbEnv "obeylines")
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
  let addr = case lookupMeta "address" (stateMeta st) of
                  Just (MetaBlocks [Plain xs]) ->
                     para $ trimInlines $ fromList xs
                  _ -> mempty
  return $ addr <> bs -- sig added by \closing

closing :: LP Blocks
closing = do
  contents <- tok
  st <- getState
  let extractInlines (MetaBlocks [Plain ys]) = ys
      extractInlines (MetaBlocks [Para ys ]) = ys
      extractInlines _          = []
  let sigs = case lookupMeta "author" (stateMeta st) of
                  Just (MetaList xs) ->
                    para $ trimInlines $ fromList $
                      intercalate [LineBreak] $ map extractInlines xs
                  _ -> mempty
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
env name p = p <*
  (try (controlSeq "end" *> braced >>= guard . (== name))
    <?> ("\\end{" ++ name ++ "}"))

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
  x <- trimInlines . mconcat <$> many1 inline
  if x == mempty
     then return mempty
     else return $ para x

preamble :: LP Blocks
preamble = mempty <$> manyTill preambleBlock beginDoc
  where beginDoc = lookAhead $ try $ controlSeq "begin" *> string "{document}"
        preambleBlock =  (void comment)
                     <|> (void sp)
                     <|> (void blanklines)
                     <|> (void macro)
                     <|> (void blockCommand)
                     <|> (void anyControlSeq)
                     <|> (void braced)
                     <|> (void anyChar)

-------

-- citations

addPrefix :: [Inline] -> [Citation] -> [Citation]
addPrefix p (k:ks)   = k {citationPrefix = p ++ citationPrefix k} : ks
addPrefix _ _ = []

addSuffix :: [Inline] -> [Citation] -> [Citation]
addSuffix s ks@(_:_) =
  let k = last ks
  in  init ks ++ [k {citationSuffix = citationSuffix k ++ s}]
addSuffix _ _ = []

simpleCiteArgs :: LP [Citation]
simpleCiteArgs = try $ do
  first  <- optionMaybe $ toList <$> opt
  second <- optionMaybe $ toList <$> opt
  char '{'
  optional sp
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
citationLabel  = optional sp *>
  (many1 (satisfy isBibtexKeyChar)
          <* optional sp
          <* optional (char ',')
          <* optional sp)
  where isBibtexKeyChar c = isAlphaNum c || c `elem` ".:;?!`'()/*@_+=-[]*"

cites :: CitationMode -> Bool -> LP [Citation]
cites mode multi = try $ do
  cits <- if multi
             then many1 simpleCiteArgs
             else count 1 simpleCiteArgs
  let cs = concat cits
  return $ case mode of
        AuthorInText -> case cs of
                             (c:rest) -> c {citationMode = mode} : rest
                             []       -> []
        _            -> map (\a -> a {citationMode = mode}) cs

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
  let maybeBar = skipMany $ sp <|> () <$ char '|' <|> () <$ try (string "@{}")
  maybeBar
  let cAlign = AlignCenter <$ char 'c'
  let lAlign = AlignLeft <$ char 'l'
  let rAlign = AlignRight <$ char 'r'
  let parAlign = AlignLeft <$ (char 'p' >> braced)
  let alignChar = cAlign <|> lAlign <|> rAlign <|> parAlign
  aligns' <- sepEndBy alignChar maybeBar
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
  cells' <- sepBy1 tableCell amp
  let numcells = length cells'
  guard $ numcells <= cols && numcells >= 1
  guard $ cells' /= [mempty]
  -- note:  a & b in a three-column table leaves an empty 3rd cell:
  let cells'' = cells' ++ replicate (cols - numcells) mempty
  spaces
  return cells''

simpTable :: LP Blocks
simpTable = try $ do
  spaces
  aligns <- parseAligns
  let cols = length aligns
  optional hline
  header' <- option [] $ try (parseTableRow cols <* lbreak <* hline)
  rows <- sepEndBy (parseTableRow cols) (lbreak <* optional hline)
  spaces
  skipMany (comment *> spaces)
  let header'' = if null header'
                    then replicate cols mempty
                    else header'
  lookAhead $ controlSeq "end" -- make sure we're at end
  return $ table mempty (zip aligns (repeat 0)) header'' rows

startInclude :: LP Blocks
startInclude = do
  fn <- braced
  setPosition $ newPos fn 1 1
  return mempty

endInclude :: LP Blocks
endInclude = do
  fn <- braced
  ln <- braced
  co <- braced
  setPosition $ newPos fn (fromMaybe 1 $ safeRead ln) (fromMaybe 1 $ safeRead co)
  return mempty
