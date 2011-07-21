{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of LaTeX to 'Pandoc' document.
-}
module Text.Pandoc.Readers.LaTeX ( 
                                  readLaTeX,
                                  rawLaTeXInline,
                                  rawLaTeXEnvironment'
                                 ) where

import Text.ParserCombinators.Parsec
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Parsing
import Data.Maybe ( fromMaybe )
import Data.Char ( chr, toUpper )
import Data.List ( intercalate, isPrefixOf, isSuffixOf )
import Control.Monad

-- | Parse LaTeX from string and return 'Pandoc' document.
readLaTeX :: ParserState   -- ^ Parser state, including options for parser
          -> String        -- ^ String to parse (assumes @'\n'@ line endings)
          -> Pandoc
readLaTeX = readWith parseLaTeX

-- characters with special meaning
specialChars :: [Char]
specialChars = "\\`$%^&_~#{}[]\n \t|<>'\"-"

--
-- utility functions
--

-- | Returns text between brackets and its matching pair.
bracketedText :: Char -> Char -> GenParser Char st [Char]
bracketedText openB closeB = do
  result <- charsInBalanced' openB closeB
  return $ [openB] ++ result ++ [closeB]

-- | Returns an option or argument of a LaTeX command.
optOrArg :: GenParser Char st [Char]
optOrArg = try $ spaces >> (bracketedText '{' '}' <|> bracketedText '[' ']')

-- | True if the string begins with '{'.
isArg :: [Char] -> Bool
isArg ('{':_) = True
isArg _       = False

-- | Returns list of options and arguments of a LaTeX command.
commandArgs :: GenParser Char st [[Char]]
commandArgs = many optOrArg

-- | Parses LaTeX command, returns (name, star, list of options or arguments).
command :: GenParser Char st ([Char], [Char], [[Char]])
command = do
  char '\\'
  name <- many1 letter
  star <- option "" (string "*")  -- some commands have starred versions
  args <- commandArgs
  return (name, star, args)

begin :: [Char] -> GenParser Char st [Char]
begin name = try $ do
  string "\\begin"
  spaces
  char '{'
  string name
  char '}'
  optional commandArgs
  spaces
  return name

end :: [Char] -> GenParser Char st [Char]
end name = try $ do
  string "\\end"
  spaces
  char '{'
  string name
  char '}'
  return name

-- | Returns a list of block elements containing the contents of an
-- environment.
environment :: [Char] -> GenParser Char ParserState [Block]
environment name = try $ begin name >> spaces >> manyTill block (end name) >>~ spaces

anyEnvironment :: GenParser Char ParserState Block
anyEnvironment =  try $ do
  string "\\begin"
  spaces
  char '{'
  name <- many letter
  star <- option "" (string "*") -- some environments have starred variants
  char '}'
  optional commandArgs
  spaces
  contents <- manyTill block (end (name ++ star))
  spaces
  return $ BlockQuote contents

--
-- parsing documents
--

-- | Process LaTeX preamble, extracting metadata.
processLaTeXPreamble :: GenParser Char ParserState ()
processLaTeXPreamble = do
  try $ string "\\documentclass"
  skipMany $ bibliographic <|> macro <|> commentBlock <|> skipChar

-- | Parse LaTeX and return 'Pandoc'.
parseLaTeX :: GenParser Char ParserState Pandoc
parseLaTeX = do
  spaces
  skipMany $ comment >> spaces
  blocks <- try (processLaTeXPreamble >> environment "document")
           <|> (many block >>~ (spaces >> eof))
  state <- getState
  let blocks' = filter (/= Null) blocks
  let title' = stateTitle state
  let authors' = stateAuthors state
  let date' = stateDate state
  return $ Pandoc (Meta title' authors' date')  blocks'

--
-- parsing blocks
--

parseBlocks :: GenParser Char ParserState [Block]
parseBlocks = spaces >> many block

block :: GenParser Char ParserState Block
block = choice [ hrule
               , codeBlock
               , header
               , list
               , blockQuote
               , simpleTable
               , commentBlock
               , macro
               , bibliographic
               , para
               , itemBlock
               , unknownEnvironment
               , ignore
               , unknownCommand
               ] <?> "block"

--
-- header blocks
--

header :: GenParser Char ParserState Block
header = section <|> chapter

chapter :: GenParser Char ParserState Block
chapter = try $ do
  string "\\chapter"
  result <- headerWithLevel 1
  updateState $ \s -> s{ stateHasChapters = True }
  return result

section :: GenParser Char ParserState Block
section = try $ do
  char '\\'
  subs <- many (try (string "sub"))
  base <- try (string "section" >> return 1) <|> (string "paragraph" >> return 4)
  st <- getState
  let lev = if stateHasChapters st
               then length subs + base + 1
               else length subs + base
  headerWithLevel lev

headerWithLevel :: Int -> GenParser Char ParserState Block
headerWithLevel lev = try $ do
  spaces
  optional (char '*')
  spaces
  optional $ bracketedText '[' ']' -- alt title
  spaces
  char '{'
  title' <- manyTill inline (char '}')
  spaces
  return $ Header lev (normalizeSpaces title')

--
-- hrule block
--

hrule :: GenParser Char st Block
hrule = oneOfStrings [ "\\begin{center}\\rule{3in}{0.4pt}\\end{center}\n\n", 
                       "\\newpage" ] >> spaces >> return HorizontalRule

-- tables

simpleTable :: GenParser Char ParserState Block
simpleTable = try $ do
  string "\\begin"
  spaces
  string "{tabular}"
  spaces
  aligns <- parseAligns
  let cols = length aligns
  optional hline
  header' <- option [] $ parseTableHeader cols
  rows <- many (parseTableRow cols >>~ optional hline)
  spaces
  end "tabular"
  spaces
  let header'' = if null header'
                    then replicate cols []
                    else header'
  return $ Table [] aligns (replicate cols 0) header'' rows

hline :: GenParser Char st ()
hline = try $ spaces >> string "\\hline" >> return ()

parseAligns :: GenParser Char ParserState [Alignment]
parseAligns = try $ do
  char '{'
  optional $ char '|'
  let cAlign = char 'c' >> return AlignCenter
  let lAlign = char 'l' >> return AlignLeft
  let rAlign = char 'r' >> return AlignRight
  let alignChar = cAlign <|> lAlign <|> rAlign
  aligns' <- sepEndBy alignChar (optional $ char '|')
  char '}'
  spaces
  return aligns'

parseTableHeader :: Int   -- ^ number of columns
                 -> GenParser Char ParserState [TableCell]
parseTableHeader cols = try $ do
  cells' <- parseTableRow cols
  hline
  return cells'

parseTableRow :: Int  -- ^ number of columns
              -> GenParser Char ParserState [TableCell]
parseTableRow cols = try $ do
  let tableCellInline = notFollowedBy (char '&' <|>
                          (try $ char '\\' >> char '\\')) >> inline
  cells' <- sepBy (spaces >> liftM ((:[]) . Plain . normalizeSpaces)
             (many tableCellInline)) (char '&')
  guard $ length cells' == cols
  spaces
  (try $ string "\\\\" >> spaces) <|>
    (lookAhead (end "tabular") >> return ())
  return cells'

--
-- code blocks
--

codeBlock :: GenParser Char ParserState Block
codeBlock = codeBlockWith "verbatim" <|> codeBlockWith "Verbatim" <|> codeBlockWith "lstlisting" <|> lhsCodeBlock
-- Note:  Verbatim is from fancyvrb.

codeBlockWith :: String -> GenParser Char st Block
codeBlockWith env = try $ do
  string "\\begin"
  spaces                      -- don't use begin function because it
  string $ "{" ++ env ++ "}"  -- gobbles whitespace; we want to gobble
  optional blanklines         -- blank lines, but not leading space
  contents <- manyTill anyChar (try (string $ "\\end{" ++ env ++ "}"))
  spaces
  let classes = if env == "code" then ["haskell"] else []
  return $ CodeBlock ("",classes,[]) (stripTrailingNewlines contents)

lhsCodeBlock :: GenParser Char ParserState Block
lhsCodeBlock = do
  failUnlessLHS
  (CodeBlock (_,_,_) cont) <- codeBlockWith "code"
  return $ CodeBlock ("", ["sourceCode","literate","haskell"], []) cont

--
-- block quotes
--

blockQuote :: GenParser Char ParserState Block
blockQuote = (environment "quote" <|> environment "quotation") >>~ spaces >>= 
             return . BlockQuote

--
-- list blocks
--

list :: GenParser Char ParserState Block
list = bulletList <|> orderedList <|> definitionList <?> "list"

listItem :: GenParser Char ParserState ([Inline], [Block])
listItem = try $ do
  ("item", _, args) <- command
  spaces
  state <- getState
  let oldParserContext = stateParserContext state
  updateState (\s -> s {stateParserContext = ListItemState})
  blocks <- many block
  updateState (\s -> s {stateParserContext = oldParserContext})
  opt <- case args of
           ([x]) | "[" `isPrefixOf` x && "]" `isSuffixOf` x -> 
                       parseFromString (many inline) $ tail $ init x
           _        -> return []
  return (opt, blocks)

orderedList :: GenParser Char ParserState Block
orderedList = try $ do
  string "\\begin"
  spaces
  string "{enumerate}"
  spaces
  (_, style, delim) <- option (1, DefaultStyle, DefaultDelim) $
                              try $ do failIfStrict
                                       char '['
                                       res <- anyOrderedListMarker
                                       char ']'
                                       return res
  spaces
  option "" $ try $ do string "\\setlength{\\itemindent}"
                       char '{'
                       manyTill anyChar (char '}')
  spaces
  start <- option 1 $ try $ do failIfStrict
                               string "\\setcounter{enum"
                               many1 (oneOf "iv")
                               string "}{"
                               num <- many1 digit
                               char '}' 
                               spaces
                               return $ (read num) + 1
  items <- many listItem
  end "enumerate"
  spaces
  return $ OrderedList (start, style, delim) $ map snd items

bulletList :: GenParser Char ParserState Block
bulletList = try $ do
  begin "itemize"
  items <- many listItem
  end "itemize"
  spaces
  return (BulletList $ map snd items)

definitionList :: GenParser Char ParserState Block
definitionList = try $ do
  begin "description"
  items <- many listItem
  end "description"
  spaces
  return $ DefinitionList $ map (\(t,d) -> (t,[d])) items

--
-- paragraph block
--

para :: GenParser Char ParserState Block
para = do
  res <- many1 inline
  spaces
  return $ if null (filter (`notElem` [Str "", Space]) res)
              then Null
              else Para $ normalizeSpaces res

--
-- title authors date
--

bibliographic :: GenParser Char ParserState Block
bibliographic = choice [ maketitle, title, subtitle, authors, date ]

maketitle :: GenParser Char st Block
maketitle = try (string "\\maketitle") >> spaces >> return Null

title :: GenParser Char ParserState Block
title = try $ do
  string "\\title{"
  tit <- manyTill inline (char '}')
  spaces
  updateState (\state -> state { stateTitle = tit })
  return Null

subtitle :: GenParser Char ParserState Block
subtitle = try $ do
  string "\\subtitle{"
  tit <- manyTill inline (char '}')
  spaces
  updateState (\state -> state { stateTitle = stateTitle state ++
                                   Str ":" : LineBreak : tit })
  return Null

authors :: GenParser Char ParserState Block
authors = try $ do
  string "\\author{"
  let andsep = try $ string "\\and" >> notFollowedBy letter >>
                     spaces >> return '&'
  raw <- sepBy (many $ notFollowedBy (char '}' <|> andsep) >> inline) andsep
  let authors' = map normalizeSpaces raw
  char '}'
  spaces
  updateState (\s -> s { stateAuthors = authors' })
  return Null

date :: GenParser Char ParserState Block
date = try $ do
  string "\\date{"
  date' <- manyTill inline (char '}')
  spaces
  updateState (\state -> state { stateDate = normalizeSpaces date' })
  return Null

--
-- item block
-- for use in unknown environments that aren't being parsed as raw latex
--

-- this forces items to be parsed in different blocks
itemBlock :: GenParser Char ParserState Block
itemBlock = try $ do
  ("item", _, args) <- command
  state <- getState
  if stateParserContext state == ListItemState
     then fail "item should be handled by list block"
     else if null args 
             then return Null
             else return $ Plain [Str (stripFirstAndLast (head args))]

--
-- raw LaTeX 
--

-- | Parse any LaTeX environment and return a Para block containing
-- the whole literal environment as raw TeX.
rawLaTeXEnvironment :: GenParser Char st Block
rawLaTeXEnvironment = do
  contents <- rawLaTeXEnvironment'
  spaces
  return $ RawBlock "latex" contents

-- | Parse any LaTeX environment and return a string containing
-- the whole literal environment as raw TeX.
rawLaTeXEnvironment' :: GenParser Char st String 
rawLaTeXEnvironment' = try $ do
  string "\\begin"
  spaces
  char '{'
  name <- many1 letter
  star <- option "" (string "*") -- for starred variants
  let name' = name ++ star
  char '}'
  args <- option [] commandArgs
  let argStr = concat args
  contents <- manyTill (choice [ (many1 (noneOf "\\")), 
                                 rawLaTeXEnvironment',
                                 string "\\" ]) 
                       (end name')
  return $ "\\begin{" ++ name' ++ "}" ++ argStr ++ 
                 concat contents ++ "\\end{" ++ name' ++ "}"

unknownEnvironment :: GenParser Char ParserState Block
unknownEnvironment = try $ do
  state <- getState
  result <- if stateParseRaw state -- check whether we should include raw TeX 
               then rawLaTeXEnvironment -- if so, get whole raw environment
               else anyEnvironment      -- otherwise just the contents
  return result

-- \ignore{} is used conventionally in literate haskell for definitions
-- that are to be processed by the compiler but not printed.
ignore :: GenParser Char ParserState Block
ignore = try $ do
  ("ignore", _, _) <- command
  spaces
  return Null

demacro :: (String, String, [String]) -> GenParser Char ParserState Inline
demacro (n,st,args) = try $ do
  let raw = "\\" ++ n ++ st ++ concat args
  s' <- applyMacros' raw
  if raw == s'
     then return $ RawInline "latex" raw
     else do
       inp <- getInput
       setInput $ s' ++ inp
       return $ Str ""

unknownCommand :: GenParser Char ParserState Block
unknownCommand = try $ do
  spaces
  notFollowedBy' $ oneOfStrings ["\\begin","\\end","\\item"]
  state <- getState
  when (stateParserContext state == ListItemState) $
     notFollowedBy' (string "\\item")
  if stateParseRaw state
     then command >>= demacro >>= return . Plain . (:[])
     else do
        (name, _, args) <- command
        spaces
        unless (name `elem` commandsToIgnore) $ do
        -- put arguments back in input to be parsed
          inp <- getInput
          setInput $ intercalate " " args ++ inp
        return Null

commandsToIgnore :: [String]
commandsToIgnore = ["special","pdfannot","pdfstringdef", "index","bibliography"]

skipChar :: GenParser Char ParserState Block
skipChar = do
  satisfy (/='\\') <|>
    (notFollowedBy' (try $
                     string "\\begin" >> spaces >> string "{document}") >>
     anyChar)
  spaces
  return Null

commentBlock :: GenParser Char st Block
commentBlock = many1 (comment >> spaces) >> return Null

-- 
-- inline
--

inline :: GenParser Char ParserState Inline
inline =  choice [ str
                 , endline
                 , whitespace
                 , quoted
                 , apostrophe
                 , strong
                 , math
                 , ellipses
                 , emDash
                 , enDash
                 , hyphen
                 , emph
                 , strikeout
                 , superscript
                 , subscript
                 , code
                 , url
                 , link
                 , image
                 , footnote
                 , linebreak
                 , accentedChar
                 , nonbreakingSpace
                 , cite
                 , specialChar
                 , ensureMath
                 , rawLaTeXInline'
                 , escapedChar
                 , unescapedChar
                 , comment
                 ] <?> "inline"


-- latex comment
comment :: GenParser Char st Inline
comment = try $ char '%' >> manyTill anyChar newline >> spaces >> return (Str "")

accentedChar :: GenParser Char st Inline
accentedChar = normalAccentedChar <|> specialAccentedChar

normalAccentedChar :: GenParser Char st Inline
normalAccentedChar = try $ do
  char '\\'
  accent <- oneOf "'`^\"~"
  character <- (try $ char '{' >> letter >>~ char '}') <|> letter
  let table = fromMaybe [] $ lookup character accentTable 
  let result = case lookup accent table of
                 Just num  -> chr num
                 Nothing   -> '?'
  return $ Str [result]

-- an association list of letters and association list of accents
-- and decimal character numbers.
accentTable :: [(Char, [(Char, Int)])]
accentTable = 
  [ ('A', [('`', 192), ('\'', 193), ('^', 194), ('~', 195), ('"', 196)]),
    ('E', [('`', 200), ('\'', 201), ('^', 202), ('"', 203)]),
    ('I', [('`', 204), ('\'', 205), ('^', 206), ('"', 207)]),
    ('N', [('~', 209)]),
    ('O', [('`', 210), ('\'', 211), ('^', 212), ('~', 213), ('"', 214)]),
    ('U', [('`', 217), ('\'', 218), ('^', 219), ('"', 220)]),
    ('a', [('`', 224), ('\'', 225), ('^', 227), ('"', 228)]),
    ('e', [('`', 232), ('\'', 233), ('^', 234), ('"', 235)]),
    ('i', [('`', 236), ('\'', 237), ('^', 238), ('"', 239)]),
    ('n', [('~', 241)]),
    ('o', [('`', 242), ('\'', 243), ('^', 244), ('~', 245), ('"', 246)]),
    ('u', [('`', 249), ('\'', 250), ('^', 251), ('"', 252)]) ]

specialAccentedChar :: GenParser Char st Inline
specialAccentedChar = choice [ ccedil, aring, iuml, szlig, aelig, lslash,
                               oslash, pound, euro, copyright, sect ]

ccedil :: GenParser Char st Inline
ccedil = try $ do
  char '\\'
  letter' <- oneOfStrings ["cc", "cC"]
  let num = if letter' == "cc" then 231 else 199
  return $ Str [chr num]

aring :: GenParser Char st Inline
aring = try $ do
  char '\\'
  letter' <- oneOfStrings ["aa", "AA"]
  let num = if letter' == "aa" then 229 else 197
  return $ Str [chr num]

iuml :: GenParser Char st Inline
iuml = try (string "\\\"") >> oneOfStrings ["\\i", "{\\i}"] >> 
       return (Str [chr 239])

szlig :: GenParser Char st Inline
szlig = try (string "\\ss") >> return (Str [chr 223])

oslash :: GenParser Char st Inline
oslash = try $ do
  char '\\'
  letter' <- choice [char 'o', char 'O']
  let num = if letter' == 'o' then 248 else 216
  return $ Str [chr num]

lslash :: GenParser Char st Inline
lslash = try $ do
  cmd <- oneOfStrings ["{\\L}","{\\l}","\\L ","\\l "]
  return $ if 'l' `elem` cmd
              then Str "\x142"
              else Str "\x141"

aelig :: GenParser Char st Inline
aelig = try $ do
  char '\\'
  letter' <- oneOfStrings ["ae", "AE"]
  let num = if letter' == "ae" then 230 else 198
  return $ Str [chr num]

pound :: GenParser Char st Inline
pound = try (string "\\pounds") >> return (Str [chr 163])

euro :: GenParser Char st Inline
euro = try (string "\\euro") >> return (Str [chr 8364])

copyright :: GenParser Char st Inline
copyright = try (string "\\copyright") >> return (Str [chr 169])

sect :: GenParser Char st Inline
sect = try (string "\\S") >> return (Str [chr 167])

escapedChar :: GenParser Char st Inline
escapedChar = do
  result <- escaped (oneOf specialChars)
  return $ if result == Str "\n" then Str " " else result

-- nonescaped special characters
unescapedChar :: GenParser Char st Inline
unescapedChar = oneOf "`$^&_#{}[]|<>" >>= return . (\c -> Str [c])

specialChar :: GenParser Char st Inline
specialChar = choice [ spacer, interwordSpace,
                       backslash, tilde, caret,
                       bar, lt, gt, doubleQuote ]

spacer :: GenParser Char st Inline
spacer = try (string "\\,") >> return (Str "")

interwordSpace :: GenParser Char st Inline
interwordSpace = try (string "\\ ") >> return (Str "\160")

backslash :: GenParser Char st Inline
backslash = try (string "\\textbackslash") >> optional (try $ string "{}") >> return (Str "\\")

tilde :: GenParser Char st Inline
tilde = try (string "\\ensuremath{\\sim}") >> return (Str "~")

caret :: GenParser Char st Inline
caret = try (string "\\^{}") >> return (Str "^")

bar :: GenParser Char st Inline
bar = try (string "\\textbar") >> optional (try $ string "{}") >> return (Str "\\")

lt :: GenParser Char st Inline
lt = try (string "\\textless") >> optional (try $ string "{}") >> return (Str "<")

gt :: GenParser Char st Inline
gt = try (string "\\textgreater") >> optional (try $ string "{}") >> return (Str ">")

doubleQuote :: GenParser Char st Inline
doubleQuote = char '"' >> return (Str "\"")

code :: GenParser Char ParserState Inline
code = code1 <|> code2 <|> code3 <|> lhsInlineCode

code1 :: GenParser Char st Inline
code1 = try $ do 
  string "\\verb"
  marker <- anyChar
  result <- manyTill anyChar (char marker)
  return $ Code nullAttr $ removeLeadingTrailingSpace result

code2 :: GenParser Char st Inline
code2 = try $ do
  string "\\texttt{"
  result <- manyTill (noneOf "\\\n~$%^&{}") (char '}')
  return $ Code nullAttr result

code3 :: GenParser Char st Inline
code3 = try $ do 
  string "\\lstinline"
  marker <- anyChar
  result <- manyTill anyChar (char marker)
  return $ Code nullAttr $ removeLeadingTrailingSpace result

lhsInlineCode :: GenParser Char ParserState Inline
lhsInlineCode = try $ do
  failUnlessLHS
  char '|'
  result <- manyTill (noneOf "|\n") (char '|')
  return $ Code ("",["haskell"],[]) result

emph :: GenParser Char ParserState Inline
emph = try $ oneOfStrings [ "\\emph{", "\\textit{" ] >>
             manyTill inline (char '}') >>= return . Emph

strikeout :: GenParser Char ParserState Inline
strikeout = try $ string "\\sout{" >> manyTill inline (char '}') >>=
                  return . Strikeout

superscript :: GenParser Char ParserState Inline
superscript = try $ string "\\textsuperscript{" >> 
                    manyTill inline (char '}') >>= return . Superscript

-- note: \textsubscript isn't a standard latex command, but we use
-- a defined version in pandoc.
subscript :: GenParser Char ParserState Inline
subscript = try $ string "\\textsubscript{" >> manyTill inline (char '}') >>=
                  return . Subscript

apostrophe :: GenParser Char ParserState Inline
apostrophe = char '\'' >> return Apostrophe

quoted :: GenParser Char ParserState Inline
quoted = doubleQuoted <|> singleQuoted

singleQuoted :: GenParser Char ParserState Inline
singleQuoted = enclosed singleQuoteStart singleQuoteEnd inline >>=
               return . Quoted SingleQuote . normalizeSpaces

doubleQuoted :: GenParser Char ParserState Inline
doubleQuoted = enclosed doubleQuoteStart doubleQuoteEnd inline >>=
               return . Quoted DoubleQuote . normalizeSpaces

singleQuoteStart :: GenParser Char st Char
singleQuoteStart = char '`'

singleQuoteEnd :: GenParser Char st ()
singleQuoteEnd = try $ char '\'' >> notFollowedBy alphaNum

doubleQuoteStart :: CharParser st String
doubleQuoteStart = string "``"

doubleQuoteEnd :: CharParser st String
doubleQuoteEnd = try $ string "''"

ellipses :: GenParser Char st Inline
ellipses = try $ do
  char '\\'
  optional $ char 'l'
  string "dots"
  optional $ try $ string "{}"
  return Ellipses

enDash :: GenParser Char st Inline
enDash = try (string "--") >> return EnDash

emDash :: GenParser Char st Inline
emDash = try (string "---") >> return EmDash

hyphen :: GenParser Char st Inline
hyphen = char '-' >> return (Str "-")

strong :: GenParser Char ParserState Inline
strong = try (string "\\textbf{") >> manyTill inline (char '}') >>=
         return . Strong

whitespace :: GenParser Char st Inline
whitespace = many1 (oneOf " \t") >> return Space

nonbreakingSpace :: GenParser Char st Inline
nonbreakingSpace = char '~' >> return (Str "\160")

-- hard line break
linebreak :: GenParser Char st Inline
linebreak = try $ do
  string "\\\\"
  optional $ bracketedText '[' ']'  -- e.g. \\[10pt]
  spaces
  return LineBreak

str :: GenParser Char st Inline
str = many1 (noneOf specialChars) >>= return . Str

-- endline internal to paragraph
endline :: GenParser Char st Inline
endline = try $ newline >> notFollowedBy blankline >> return Space

-- math
math :: GenParser Char ParserState Inline
math =   (math3 >>= applyMacros' >>= return . Math DisplayMath)
     <|> (math1 >>= applyMacros' >>= return . Math InlineMath)
     <|> (math2 >>= applyMacros' >>= return . Math InlineMath)
     <|> (math4 >>= applyMacros' >>= return . Math DisplayMath)
     <|> (math5 >>= applyMacros' >>= return . Math DisplayMath)
     <|> (math6 >>= applyMacros' >>= return . Math DisplayMath)
     <?> "math"

math1 :: GenParser Char st String 
math1 = try $ char '$' >> manyTill anyChar (char '$')

math2 :: GenParser Char st String
math2 = try $ string "\\(" >> manyTill anyChar (try $ string "\\)")

math3 :: GenParser Char st String 
math3 = try $ char '$' >> math1 >>~ char '$'

math4 :: GenParser Char st String
math4 = try $ do
  name <- begin "displaymath" <|> begin "equation" <|> begin "equation*" <|>
           begin "gather" <|> begin "gather*" <|> begin "gathered" <|>
             begin "multline" <|> begin "multline*"
  manyTill anyChar (end name)

math5 :: GenParser Char st String
math5 = try $ (string "\\[") >> spaces >> manyTill anyChar (try $ string "\\]")

math6 :: GenParser Char st String
math6 = try $ do
  name <- begin "eqnarray" <|> begin "eqnarray*" <|> begin "align" <|>
           begin "align*" <|> begin "alignat" <|> begin "alignat*" <|>
             begin "split" <|> begin "aligned" <|> begin "alignedat"
  res <- manyTill anyChar (end name)
  return $ filter (/= '&') res  -- remove alignment codes

ensureMath :: GenParser Char st Inline
ensureMath = try $ do
  (n, _, args) <- command
  guard $ n == "ensuremath" && not (null args)
  return $ Math InlineMath $ tail $ init $ head args

--
-- links and images
--

url :: GenParser Char ParserState Inline
url = try $ do
  string "\\url"
  url' <- charsInBalanced '{' '}'
  return $ Link [Code ("",["url"],[]) url'] (escapeURI url', "")

link :: GenParser Char ParserState Inline
link = try $ do
  string "\\href{"
  url' <- manyTill anyChar (char '}')
  char '{'
  label' <- manyTill inline (char '}') 
  return $ Link (normalizeSpaces label') (escapeURI url', "")

image :: GenParser Char ParserState Inline
image = try $ do
  ("includegraphics", _, args) <- command
  let args' = filter isArg args -- filter out options
  let (src,tit) = case args' of
                       []    -> ("", "")
                       (x:_) -> (stripFirstAndLast x, "")
  return $ Image [Str "image"] (escapeURI src, tit)

footnote :: GenParser Char ParserState Inline
footnote = try $ do
  (name, _, (contents:[])) <- command
  if ((name == "footnote") || (name == "thanks"))
     then string ""
     else fail "not a footnote or thanks command"
  let contents' = stripFirstAndLast contents
  -- parse the extracted block, which may contain various block elements:
  rest <- getInput
  setInput $ contents'
  blocks <- parseBlocks
  setInput rest
  return $ Note blocks

-- | citations
cite :: GenParser Char ParserState Inline
cite = simpleCite <|> complexNatbibCites

simpleCiteArgs :: GenParser Char ParserState [Citation]
simpleCiteArgs = try $ do
  first  <- optionMaybe $ (char '[') >> manyTill inline (char ']')
  second <- optionMaybe $ (char '[') >> manyTill inline (char ']')
  char '{'
  keys <- many1Till citationLabel (char '}')
  let (pre, suf) = case (first  , second ) of
        (Just s , Nothing) -> ([], s )
        (Just s , Just t ) -> (s , t )
        _                  -> ([], [])
      conv k = Citation { citationId      = k
                        , citationPrefix  = []
                        , citationSuffix  = []
                        , citationMode    = NormalCitation
                        , citationHash    = 0
                        , citationNoteNum = 0
                        }
  return $ addPrefix pre $ addSuffix suf $ map conv keys


simpleCite :: GenParser Char ParserState Inline
simpleCite = try $ do
  char '\\'
  let biblatex     = [a ++ "cite" | a <- ["auto", "foot", "paren", "super", ""]]
                     ++ ["footcitetext"]
      normal       = ["cite" ++ a ++ b | a <- ["al", ""], b <- ["p", "p*", ""]]
                     ++ biblatex
      supress      = ["citeyearpar", "citeyear", "autocite*", "cite*", "parencite*"]
      intext       = ["textcite"] ++ ["cite" ++ a ++ b | a <- ["al", ""], b <- ["t", "t*"]]
      mintext      = ["textcites"]
      mnormal      = map (++ "s") biblatex
      cmdend       = notFollowedBy (letter <|> char '*')
      capit []     = []
      capit (x:xs) = toUpper x : xs
      addUpper xs  = xs ++ map capit xs
      toparser l t = try $ oneOfStrings (addUpper l) >> cmdend >> return t
  (mode, multi) <-  toparser normal  (NormalCitation, False)
                <|> toparser supress (SuppressAuthor, False)
                <|> toparser intext  (AuthorInText  , False)
                <|> toparser mnormal (NormalCitation, True )
                <|> toparser mintext (AuthorInText  , True )
  cits <- if multi then
            many1 simpleCiteArgs
          else
            simpleCiteArgs >>= \c -> return [c]
  let (c:cs) = concat cits
      cits'  = case mode of
        AuthorInText   -> c {citationMode = mode} : cs
        _              -> map (\a -> a {citationMode = mode}) (c:cs)
  return $ Cite cits' []

complexNatbibCites :: GenParser Char ParserState Inline
complexNatbibCites = complexNatbibTextual <|> complexNatbibParenthetical

complexNatbibTextual :: GenParser Char ParserState Inline
complexNatbibTextual = try $ do
  string "\\citeauthor{"
  manyTill (noneOf "}") (char '}')
  skipSpaces
  Cite (c:cs) _ <- complexNatbibParenthetical
  return $ Cite (c {citationMode = AuthorInText} : cs) []


complexNatbibParenthetical :: GenParser Char ParserState Inline
complexNatbibParenthetical = try $ do
  string "\\citetext{"
  cits <- many1Till parseOne (char '}')
  return $ Cite (concat cits) []
  where
    parseOne = do
                 skipSpaces
                 pref           <- many (notFollowedBy (oneOf "\\}") >> inline)
                 (Cite cites _) <- simpleCite
                 suff           <- many (notFollowedBy (oneOf "\\};") >> inline)
                 skipSpaces
                 optional $ char ';'
                 return $ addPrefix pref $ addSuffix suff $ cites

addPrefix :: [Inline] -> [Citation] -> [Citation]
addPrefix p (k:ks)   = k {citationPrefix = p ++ citationPrefix k} : ks
addPrefix _ _ = []

addSuffix :: [Inline] -> [Citation] -> [Citation]
addSuffix s ks@(_:_) = let k = last ks
                        in init ks ++ [k {citationSuffix = citationSuffix k ++ s}]
addSuffix _ _ = []

citationLabel :: GenParser Char ParserState String
citationLabel  = do
  res <- many1 $ noneOf ",}"
  optional $ char ','
  return $ removeLeadingTrailingSpace res

-- | Parse any LaTeX inline command and return it in a raw TeX inline element.
rawLaTeXInline' :: GenParser Char ParserState Inline
rawLaTeXInline' = do
  notFollowedBy' $ oneOfStrings ["\\begin", "\\end", "\\item", "\\ignore",
                                 "\\section"]
  rawLaTeXInline

-- | Parse any LaTeX command and return it in a raw TeX inline element.
rawLaTeXInline :: GenParser Char ParserState Inline
rawLaTeXInline = try $ do
  state <- getState
  if stateParseRaw state
     then command >>= demacro
     else do
        (name,st,args) <- command
        x <- demacro (name,st,args)
        unless (x == Str "" || name `elem` commandsToIgnore) $ do
          inp <- getInput
          setInput $ intercalate " " args ++ inp
        return $ Str ""
