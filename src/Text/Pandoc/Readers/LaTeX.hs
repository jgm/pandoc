-- | Converts LaTeX to 'Pandoc' document.
module Text.Pandoc.Readers.LaTeX ( 
                                  readLaTeX,
                                  rawLaTeXInline,
                                  rawLaTeXEnvironment
                                 ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Shared 
import Maybe ( fromMaybe )
import Char ( chr )

-- | Parse LaTeX from string and return 'Pandoc' document.
readLaTeX :: ParserState   -- ^ Parser state, including options for parser
          -> String        -- ^ String to parse
          -> Pandoc
readLaTeX = readWith parseLaTeX

-- for testing
testString = testStringWith parseLaTeX

-- characters with special meaning
specialChars = "\\$%&^&_~#{}\n \t|<>"

--
-- utility functions
--

-- | Change quotation marks in a string back to "basic" quotes.
normalizeQuotes :: String -> String
normalizeQuotes = gsub "''" "\"" . gsub "`" "'"

-- | Change LaTeX En dashes between digits to hyphens.
normalizeDashes :: String -> String
normalizeDashes = gsub "([0-9])--([0-9])" "\\1-\\2"

normalizePunctuation :: String -> String
normalizePunctuation = normalizeDashes . normalizeQuotes

-- | Returns command option (between []) if any, or empty string.
commandOpt = option "" (between (char '[') (char ']') (many1 (noneOf "]")))

-- | Returns text between brackets and its matching pair.
bracketedText = try (do
  char '{'
  result <- many (choice [ try (do{ char '\\'; 
                                    b <- oneOf "{}"; 
                                    return (['\\', b])}), -- escaped bracket
                           count 1 (noneOf "{}"),
                           do {text <- bracketedText; return ("{" ++ text ++ "}")} ])
  char '}'
  return (concat result))

-- | Parses list of arguments of LaTeX command.
commandArgs = many bracketedText

-- | Parses LaTeX command, returns (name, star, option, list of arguments).
command = try (do
  char '\\'
  name <- many1 alphaNum
  star <- option "" (string "*")  -- some commands have starred versions
  opt <- commandOpt
  args <- commandArgs
  return (name, star, opt, args))

begin name = try (do
  string "\\begin{"
  string name
  char '}'
  option "" commandOpt 
  option [] commandArgs
  spaces
  return name)

end name = try (do
  string "\\end{"
  string name
  char '}'
  spaces
  return name)

-- | Returns a list of block elements containing the contents of an environment.
environment name = try (do
  begin name
  spaces
  contents <- manyTill block (end name)
  return contents)

anyEnvironment =  try (do
  string "\\begin{"
  name <- many alphaNum 
  star <- option "" (string "*") -- some environments have starred variants
  char '}'
  option "" commandOpt 
  option [] commandArgs
  spaces
  contents <- manyTill block (end (name ++ star))
  return (BlockQuote contents))

--
-- parsing documents
--

-- | Skip everything up through \begin{document}
skipLaTeXHeader = try (do
  manyTill anyChar (begin "document")
  spaces
  return "")

-- | Parse LaTeX and return 'Pandoc'.
parseLaTeX = do
  option "" skipLaTeXHeader            -- if parsing a fragment, this might not be present
  blocks <- parseBlocks
  spaces
  option "" (string "\\end{document}") -- if parsing a fragment, this might not be present
  spaces
  eof
  state <- getState
  let keyBlocks = stateKeyBlocks state 
  let noteBlocks = stateNoteBlocks state
  let blocks' = filter (/= Null) blocks
  return (Pandoc (Meta [] [] "") (blocks' ++ (reverse noteBlocks) ++ (reverse keyBlocks)))

--
-- parsing blocks
--

parseBlocks = do
  spaces
  result <- many block
  return result

block = choice [ hrule, codeBlock, header, list, blockQuote, mathBlock, comment, 
                 bibliographic, para, specialEnvironment, itemBlock, unknownEnvironment, 
                 unknownCommand ] <?> "block"

--
-- header blocks
--

header = choice (map headerLevel (enumFromTo 1 5)) <?> "header"

headerLevel n = try (do
  let subs = concat $ replicate (n - 1) "sub"
  string ("\\" ++ subs ++ "section")
  option ' ' (char '*')
  char '{'
  title <- manyTill inline (char '}')
  spaces
  return (Header n (normalizeSpaces title)))

--
-- hrule block
--

hrule = try (do
  oneOfStrings [ "\\begin{center}\\rule{3in}{0.4pt}\\end{center}\n\n", "\\newpage" ]
  spaces
  return HorizontalRule)

--
-- code blocks
--

codeBlock = try (do
  string "\\begin{verbatim}"  -- don't use begin function because it gobbles whitespace
  option "" blanklines        -- we want to gobble blank lines, but not leading space
  contents <- manyTill anyChar (try (string "\\end{verbatim}"))
  spaces
  return (CodeBlock (stripTrailingNewlines contents)))

--
-- block quotes
--

blockQuote = choice [ blockQuote1, blockQuote2 ] <?> "blockquote"

blockQuote1 = try (do
  blocks <- environment "quote"
  spaces
  return (BlockQuote blocks))

blockQuote2 = try (do
  blocks <- environment "quotation"
  spaces
  return (BlockQuote blocks))

--
-- math block
--

mathBlock = mathBlockWith (begin "equation") (end "equation") <|> 
            mathBlockWith (begin "displaymath") (end "displaymath") <|>
            mathBlockWith (string "\\[") (string "\\]") <?> "math block"

mathBlockWith start end = try (do
  start
  spaces
  result <- manyTill anyChar end
  spaces
  return (BlockQuote [Para [TeX ("$" ++ result ++ "$")]]))

--
-- list blocks
--

list = bulletList <|> orderedList <?> "list"

listItem = try (do
    ("item", _, _, _) <- command
    spaces
    state <- getState
    let oldParserContext = stateParserContext state
    updateState (\state -> state {stateParserContext = ListItemState})
    blocks <- many block
    updateState (\state -> state {stateParserContext = oldParserContext})
    return blocks)

orderedList = try (do
    begin "enumerate"
    spaces
    items <- many listItem
    end "enumerate"
    spaces
    return (OrderedList items))

bulletList = try (do
    begin "itemize"
    spaces
    items <- many listItem
    end "itemize"
    spaces
    return (BulletList items))

--
-- paragraph block
--

para = try (do 
  result <- many1 inline
  spaces
  return (Para (normalizeSpaces result)))

--
-- title authors date
--

bibliographic = choice [ maketitle, title, authors, date ]

maketitle = try (do
  string "\\maketitle"
  spaces
  return Null)

title = try (do
  string "\\title{"
  tit <- manyTill inline (char '}')
  spaces
  updateState (\state -> state { stateTitle = tit })
  return Null)

authors = try (do
  string "\\author{"
  authors <- manyTill anyChar (char '}')
  spaces
  let authors' = map removeLeadingTrailingSpace $ lines $ gsub "\\\\" "\n" authors
  updateState (\state -> state { stateAuthors = authors' })
  return Null)

date = try (do
  string "\\date{"
  date' <- manyTill anyChar (char '}')
  spaces
  updateState (\state -> state { stateDate = date' })
  return Null)

--
-- item block
-- for use in unknown environments that aren't being parsed as raw latex
--

-- this forces items to be parsed in different blocks
itemBlock = try (do
  ("item", _, opt, _) <- command
  state <- getState
  if (stateParserContext state == ListItemState) then 
      fail "item should be handled by list block"
    else
      if null opt then
          return Null
        else
          return (Plain [Str opt]))

--
-- raw LaTeX 
--

specialEnvironment = do  -- these are always parsed as raw
  followedBy' (choice (map (\name -> begin name)  ["tabular", "figure", "tabbing", "eqnarry", 
                                                   "picture", "table", "verse", "theorem"]))
  rawLaTeXEnvironment

-- | Parse any LaTeX environment and return a Para block containing 
-- the whole literal environment as raw TeX.
rawLaTeXEnvironment :: GenParser Char st Block
rawLaTeXEnvironment = try (do
  string "\\begin"
  char '{'
  name <- many1 alphaNum
  star <- option "" (string "*") -- for starred variants
  let name' = name ++ star
  char '}'
  opt <- option "" commandOpt 
  args <- option [] commandArgs
  let optStr = if (null opt) then "" else "[" ++ opt ++ "]"
  let argStr = concatMap (\arg -> ("{" ++ arg ++ "}")) args
  contents <- manyTill (choice [(many1 (noneOf "\\")), 
                                (do{ (Para [TeX str]) <- rawLaTeXEnvironment; return str }),
                                string "\\"]) (end name')
  spaces
  return (Para [TeX ("\\begin{" ++ name' ++ "}" ++ optStr ++ argStr ++ 
                                (concat contents) ++ "\\end{" ++ name' ++ "}")]))

unknownEnvironment = try (do
  state <- getState
  result <- if stateParseRaw state then  -- check to see whether we should include raw TeX
                rawLaTeXEnvironment      -- if so, get the whole raw environment
            else
                anyEnvironment           -- otherwise just the contents
  return result)

unknownCommand = try (do
  notFollowedBy' (string "\\end{itemize}")
  notFollowedBy' (string "\\end{enumerate}")
  notFollowedBy' (string "\\end{document}")
  (name, star, opt, args) <- command
  spaces
  let optStr = if null opt then "" else "[" ++ opt ++ "]"
  let argStr = concatMap (\arg -> ("{" ++ arg ++ "}")) args
  state <- getState
  if (name == "item") && ((stateParserContext state) == ListItemState) then
      fail "should not be parsed as raw"
    else
      string ""
  if stateParseRaw state then
      return (Plain [TeX ("\\" ++ name ++ star ++ optStr ++ argStr)])
    else
      return (Plain [Str (joinWithSep " " args)]))

-- latex comment
comment = try (do
  char '%'
  result <- manyTill anyChar newline
  spaces
  return Null)

-- 
-- inline
--

inline =  choice [ strong, emph, ref, lab, code, linebreak, math, ldots, accentedChar, 
                 specialChar, specialInline, escapedChar, unescapedChar, str, 
                 endline, whitespace ] <?> "inline"

specialInline = choice [ link, image, footnote, rawLaTeXInline ] <?> 
                "link, raw TeX, note, or image"

ldots = try (do
  string "\\ldots"
  return (Str "..."))

accentedChar = normalAccentedChar <|> specialAccentedChar

normalAccentedChar = try (do
  char '\\'
  accent <- oneOf "'`^\"~"
  character <- choice [ between (char '{') (char '}') anyChar, anyChar ]
  let table = fromMaybe [] $ lookup character accentTable 
  let result = case lookup accent table of
                 Just num  -> chr num
                 Nothing   -> '?'
  return (Str [result]))

-- an association list of letters and association list of accents
-- and decimal character numbers.
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

specialAccentedChar = choice [ ccedil, aring, iuml, szlig, aelig, oslash, pound, 
                               euro, copyright, sect ]

ccedil = try (do
  char '\\'
  letter <- choice [try (string "cc"), try (string "cC")]
  let num = if letter == "cc" then 231 else 199
  return (Str [chr num]))

aring = try (do
  char '\\'
  letter <- choice [try (string "aa"), try (string "AA")]
  let num = if letter == "aa" then 229 else 197
  return (Str [chr num]))

iuml = try (do
  string "\\\""
  choice [try (string "\\i"), try (string "{\\i}")]
  return (Str [chr 239]))

icirc = try (do
  string "\\^"
  choice [try (string "\\i"), try (string "{\\i}")]
  return (Str [chr 238]))

szlig = try (do
  string "\\ss"
  return (Str [chr 223]))

oslash = try (do
  char '\\'
  letter <- choice [char 'o', char 'O']
  let num = if letter == 'o' then 248 else 216
  return (Str [chr num]))

aelig = try (do
  char '\\'
  letter <- choice [try (string "ae"), try (string "AE")]
  let num = if letter == "ae" then 230 else 198
  return (Str [chr num]))

pound = try (do 
  string "\\pounds"
  return (Str [chr 163]))

euro = try (do
  string "\\euro"
  return (Str [chr 8364]))

copyright = try (do
  string "\\copyright"
  return (Str [chr 169]))

sect = try (do
  string "\\S"
  return (Str [chr 167]))

escapedChar = escaped (oneOf " $%^&_#{}")

unescapedChar = do  -- ignore standalone, nonescaped special characters
  oneOf "$^&_#{}|<>"
  return (Str "")

specialChar = choice [ backslash, bar, lt, gt ]

backslash = try (do 
  string "\\textbackslash"
  return (Str "\\"))

bar = try (do
  string "\\textbar"
  return (Str "\\"))

lt = try (do
  string "\\textless"
  return (Str "<"))

gt = try (do
  string "\\textgreater"
  return (Str ">"))

code = try (do 
  string "\\verb"
  marker <- anyChar
  result <- manyTill anyChar (char marker)
  let result' = removeLeadingTrailingSpace result
  return (Code result'))

emph = try (do 
  oneOfStrings [ "\\emph{", "\\textit{" ]
  result <- manyTill inline (char '}')
  return (Emph result))

lab = try (do
  string "\\label{"
  result <- manyTill anyChar (char '}')
  return (Str ("(" ++ result ++ ")")))

ref = try (do
  string "\\ref{"
  result <- manyTill anyChar (char '}')
  return (Str (result)))

strong = try (do 
  string "\\textbf{"
  result <- manyTill inline (char '}')
  return (Strong result))

whitespace = do
  many1 (oneOf "~ \t")
  return Space

-- hard line break
linebreak = try (do
  string "\\\\"
  return LineBreak)

str = do 
  result <- many1 (noneOf specialChars)
  return (Str (normalizePunctuation result))

-- endline internal to paragraph
endline = try (do
  newline
  notFollowedBy blankline
  return Space)

-- math
math = math1 <|> math2 <?> "math"

math1 = try (do
  char '$'
  result <- many (noneOf "$")
  char '$'
  return (TeX ("$" ++ result ++ "$")))

math2 = try (do
  string "\\("
  result <- many (noneOf "$")
  string "\\)"
  return (TeX ("$" ++ result ++ "$")))

--
-- links and images
--

link = try (do
  string "\\href{"
  url <- manyTill anyChar (char '}')
  char '{'
  label <- manyTill inline (char '}') 
  ref <- generateReference url ""
  return (Link (normalizeSpaces label) ref))

image = try (do
  ("includegraphics", _, _, (src:lst)) <- command
  return (Image [Str "image"] (Src src "")))

footnote = try (do
  ("footnote", _, _, (contents:[])) <- command
  let blocks = case runParser parseBlocks defaultParserState "footnote" contents of
                 Left err -> error $ "Input:\n" ++ show contents ++
                             "\nError:\n" ++ show err
                 Right result -> result
  state <- getState
  let notes = stateNoteBlocks state
  let nextRef  = case notes of
                   []                   -> "1"
                   (Note ref body):rest -> (show ((read ref) + 1))
  setState (state { stateNoteBlocks = (Note nextRef blocks):notes })
  return (NoteRef nextRef))

-- | Parse any LaTeX command and return it in a raw TeX inline element.
rawLaTeXInline :: GenParser Char ParserState Inline
rawLaTeXInline = try (do
  (name, star, opt, args) <- command
  let optStr = if (null opt) then "" else "[" ++ opt ++ "]"
  let argStr = concatMap (\arg -> "{" ++ arg ++ "}") args
  state <- getState
  if ((name == "begin") || (name == "end") || (name == "item")) then
      fail "not an inline command" 
    else
      string ""
  return (TeX ("\\" ++ name ++ star ++ optStr ++ argStr)))
