import Control.Monad.Except (throwError)
import Control.Monad (guard)
import Data.Default -- def is there
import Data.Functor.Identity
import Text.Pandoc.Builder (Blocks, Inlines, trimInlines)
import qualified Text.Pandoc.Builder as B (doc, toList, headerWith, str, space)
import Text.Pandoc.Class (PandocMonad, report, PandocIO, runIO)
import Text.Pandoc.Definition (Pandoc, nullAttr)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Logging (LogMessage(ParsingTrace))
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Parsing (readWithM, ParserT, stateOptions, ParserState, blanklines, registerHeader, spaceChar)
import Text.Parsec.Char (spaces, char, anyChar, newline, string, noneOf)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Combinator (eof, choice, many1, manyTill, count, skipMany1)
import Text.Parsec.Pos (sourceColumn)
import Text.Parsec.Prim (many, getPosition, try, runParserT)
-- imports for tests
import Text.Parsec.String (Parser)
import Text.Parsec (parse)
import Text.Parsec.Char (oneOf, space)
import Text.Parsec.Combinator (lookAhead)
import Text.Parsec.Prim ((<|>), (<?>), skipMany)
import Test.HUnit

readVimwiki :: PandocMonad m => ReaderOptions -> String -> m Pandoc
readVimwiki opts s = do
  res <- readWithM parseVimwiki def{ stateOptions = opts } s
  case res of
       Left e -> throwError e
       Right result -> return result

type VwParser = ParserT [Char] ParserState
--type VwParser = ParserT [Char] ParserState -- for test


-- constants
specialChars :: [Char]
specialChars = "=*-#[]_~"

spaceChars :: [Char]
spaceChars = " \t\n"

-- main parser

parseVimwiki :: PandocMonad m => VwParser m Pandoc
parseVimwiki = do
  bs <- mconcat <$> many block
  spaces
  eof
  return $ B.doc bs

-- block parser

block :: PandocMonad m => VwParser m Blocks
block = do
  pos <- getPosition
  res <- choice [ mempty <$ blanklines
                , header
{--                , bulletList
                , orderedList
                , hrule
                , table
                , blockQuote
                , para
                , preformatted
                , blockMath --}
                ]
  report $ ParsingTrace (take 60 $ show $ B.toList res) pos
  return res

header :: PandocMonad m => VwParser m Blocks
bulletList :: PandocMonad m => VwParser m Blocks
orderedList :: PandocMonad m => VwParser m Blocks
hrule :: PandocMonad m => VwParser m Blocks
table :: PandocMonad m => VwParser m Blocks
para :: PandocMonad m => VwParser m Blocks
blockQuote :: PandocMonad m => VwParser m Blocks
preformatted :: PandocMonad m => VwParser m Blocks
blockMath :: PandocMonad m => VwParser m Blocks

guardColumnOne :: PandocMonad m => VwParser m ()
guardColumnOne = getPosition >>= \pos -> guard (sourceColumn pos == 1)

{--
header = do
  attr <- registerHeader nullAttr contents
  return $ B.headerWith attr lev contents
    where
      contents = B.str <$> contents'
      where 
        contents' = do
          many 
          --}
--header = undefined
header = try $ do
  many whitespace
  eqs <- many1 (char '=')
  whitespace
  let lev = length eqs
  guard $ lev <= 6
  -- contents <- trimInlines . mconcat <$> manyTill inline (try (whitespace >> (string eqs)))
  -- contents <- trimInlines . mconcat <$> manyTill inline (string eqs)
  -- contents <- mconcat <$> manyTill inline (string eqs)
  contents <- trimInlines . mconcat <$> manyTill inline (try $ whitespace >> (string eqs) >> many whitespace >> (char '\n'))
  attr <- registerHeader nullAttr contents
  --return $ B.headerWith attr lev contents
  return $ B.headerWith attr lev contents

bulletList = undefined
orderedList = undefined
hrule = undefined
table = undefined
blockQuote = undefined
para = undefined
preformatted = undefined
blockMath = undefined

-- inline parser

inline :: PandocMonad m => VwParser m Inlines
inline = choice[whitespace
             ,  str
             ,  special
             ]
{--inline = choice [ whitespace
                , bareURL
                , strong
                , emph
                , strikeout
                , code
                , intLink -- handles anchors, links with or without descriptions, loca dirs, links with thumbnails
                , extLink -- handles file, local etc.
                , image
                , inlineMath
                , comment
                , tag
                ]--}

str :: PandocMonad m => VwParser m Inlines
whitespace :: PandocMonad m => VwParser m Inlines
special :: PandocMonad m => VwParser m Inlines
bareURL :: PandocMonad m => VwParser m Inlines
strong :: PandocMonad m => VwParser m Inlines
emph :: PandocMonad m => VwParser m Inlines
strikeout :: PandocMonad m => VwParser m Inlines
code :: PandocMonad m => VwParser m Inlines
intLink :: PandocMonad m => VwParser m Inlines
extLink :: PandocMonad m => VwParser m Inlines
image :: PandocMonad m => VwParser m Inlines
inlineMath :: PandocMonad m => VwParser m Inlines
comment :: PandocMonad m => VwParser m Inlines
tag :: PandocMonad m => VwParser m Inlines

--str = B.str <$> many1 (noneOf $ specialChars ++ spaceChars)
str = B.str <$> (many1 $ noneOf $ spaceChars ++ specialChars)
whitespace = B.space <$ (skipMany1 spaceChar)
special = B.str <$> count 1 (oneOf specialChars)
bareURL  = undefined
{--
strong = do
  char '*'
  lookAhead (noneOf spaceChars)
  x <- manyTill inline $ try $ lookAhead $ (noneOf " \t\n") >> (char '*') >> (oneOf $ spaceChars ++ specialChars)
  y <- anyChar
  (return $ x ++ [y])
  --}
strong = undefined
emph  = undefined
strikeout  = undefined
code  = undefined
intLink  = undefined
extLink  = undefined
image  = undefined
inlineMath  = undefined
comment  = undefined
tag  = undefined

-- tests

-- *Main> runIO (readVimwiki (def :: ReaderOptions) "==2==" :: PandocIO Pandoc)
-- Right (Pandoc (Meta {unMeta = fromList []}) [Header 2 ("",[],[]) [Str "2"]])

runParser :: VwParser PandocIO a -> String -> PandocIO a
runParser p s = do
  res <- readWithM p def{ stateOptions = def :: ReaderOptions } s
  case res of
       Left e -> throwError e
       Right result -> return result

testP :: VwParser PandocIO a -> String -> IO (Either PandocError a)
testP p s = runIO $ runParser p (s ++ "\n")

--type Parser = Parsec String ()
{--runP :: VwParser PandocIO a -> [Char] -> IO (Either PandocError a)
runP p opts s = do
  res <- readWithM p def{ stateOptions = opts } s
  case res of
       Left e -> throwError e
       Right result -> return result--}
--runP p s = runIO (mapLeft (PandocParsecError s) `liftM` runParserT p def{ stateOptions = def :: ReaderOptions } "test" s)
--
simpleParse :: Parser a -> String -> Either ParseError a
simpleParse p s = parse p "" (s ++ "\n")

--header' :: Parser String
--header' = manyTill anyChar (try ((oneOf " \t") >> (string "===")))

--header' :: Parser String
--header' = manyTill anyChar (string "===")

header' :: Parser String
header' = do
  spaces
  eqs <- many1 $ char '='
  let lev = length eqs
  guard $ lev <= 6
  space
  --manyTill anyChar $ try $ space >> string (eqs ++ "\n")
  manyTill anyChar $ try $ space >> string eqs >> (skipMany spaceChar) >> char '\n'


header'' :: Parser [Inlines]
header'' = manyTill (B.str <$> (many1 anyChar)) (string "===")

strikeout' :: Parser String
strikeout' = do
  string "~~"
  manyTill anyChar $ try $ lookAhead $ (string "~~") >> (oneOf $ spaceChars ++ specialChars)

emph' :: Parser String
emph' = do
  char '_'
  lookAhead (noneOf " \t\n")
  x <- manyTill anyChar $ try $ lookAhead $ (noneOf " \t\n") >> (char '_') >> (oneOf $ spaceChars ++ specialChars)
  y <- anyChar
  (return $ x ++ [y]) 

strong' :: Parser String
strong' = do
  char '*'
  lookAhead (noneOf " \t\n")
  x <- manyTill anyChar $ try $ lookAhead $ (noneOf " \t\n") >> (char '*') >> (oneOf $ spaceChars ++ specialChars)
  y <- anyChar
  (return $ x ++ [y])

testStrong' :: IO Counts
testStrong' = 
  runTestTT $ TestList
    [ TestCase (assertEqual "" (simpleParse strong' "*23*") (Right "23")),
      TestCase (assertEqual "" (simpleParse strong' "*2 3*~   *_") (Right "2 3"))
    ]
-- other tests: *a*a fails; 

testHeader' =
  runTestTT $ TestList
    [ TestCase (assertEqual "" (simpleParse header' " = a b= c =") (Right "a b= c")),
      TestCase (assertEqual "" (simpleParse header' " == a b= c ==") (Right "a b= c")),
      TestCase (assertEqual "" (simpleParse header' " === a b==c ===") (Right "a b==c")),
      TestCase (assertEqual "" (simpleParse header' " = a b =c =") (Right "a b =c"))
    ]
-- other tests: " ======= a b= c =======   ", " === a b= c ====   " fail

{-- ???
testHeader =
  runTestTT $ TestList
    [ TestCase (assertEqual "" (testP header "     = a = b =") (Right (Many {unMany = fromList [Header 1 ("",[],[]) [Str "a",Space,Str "=",Space,Str "b"]]}))),
      TestCase (assertEqual "" (testP header "     ====== a =** b ======") (Right (Many {unMany = fromList [Header 6 ("",[],[]) [Str "a",Space,Str "=**",Space,Str "b"]]}))),
      TestCase (assertEqual "" (testP header "     ======= a =** b =======") (Left (PandocParsecError "     ======= a =** b =======\n" "source" (line 1, column 14):
unexpected "a"))),
      TestCase (assertEqual "" (testP header "     === a = b =") (Left (PandocParsecError "     === a = b =\n" "source" (line 1, column 17):
unexpected "\n"))),
      TestCase (assertEqual "" (testP header "= a = b =a") (Left (PandocParsecError "= a = b =a\n" "source" (line 1, column 11):
unexpected "\n")))
    ]
    --}
  --manyTill (noneOf "*") (try ((noneOf " \t\n") >> (char '*')))
  --manyTill anyChar (try ((noneOf " \t\n") >> (char '*')))
  --x <- manyTill anyChar $ try $ lookAhead $ (noneOf " \t\n") >> (char '*') >> oneOf " \t\n*"
  --y <- anyChar
  --return $ x ++ [y]

{--p :: Parser String
p = do 
  x <- manyTill anyChar $ try $ lookAhead $ (noneOf " \t\n") >> (char '*') >> oneOf " \t\n*"
  y <- anyChar
  return $ x ++ [y]--}
{--
p :: Parser Char
p = do
  x <- char 'a' 
  char ' '
  return x
  --}
