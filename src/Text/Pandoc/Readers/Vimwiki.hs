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

readVimwiki :: PandocMonad m => ReaderOptions -> String -> m Pandoc
readVimwiki opts s = do
  res <- readWithM parseVimwiki def{ stateOptions = opts } s
  case res of
       Left e -> throwError e
       Right result -> return result

type VwParser = ParserT [Char] ParserState


-- constants
specialChars :: [Char]
specialChars = "="

spaceChars :: [Char]
spaceChars = " \t"

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

header = try $ do
  many whitespace
  eqs <- many1 (char '=')
  whitespace
  let lev = length eqs
  guard $ lev <= 6
  contents <- trimInlines . mconcat <$> manyTill inline (try (whitespace >> (string eqs)))
  --contents <- trimInlines . mconcat <$> manyTill inline (string eqs)
  attr <- registerHeader nullAttr contents
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
inline = choice[str
             ,  whitespace
             ]
{--inline = choice [ whitespace
                , bareURL
                , bold
                , italic
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
bareURL :: PandocMonad m => VwParser m Inlines
bold :: PandocMonad m => VwParser m Inlines
italic :: PandocMonad m => VwParser m Inlines
strikeout :: PandocMonad m => VwParser m Inlines
code :: PandocMonad m => VwParser m Inlines
intLink :: PandocMonad m => VwParser m Inlines
extLink :: PandocMonad m => VwParser m Inlines
image :: PandocMonad m => VwParser m Inlines
inlineMath :: PandocMonad m => VwParser m Inlines
comment :: PandocMonad m => VwParser m Inlines
tag :: PandocMonad m => VwParser m Inlines

str = B.str <$> many1 (noneOf $ specialChars ++ spaceChars)
whitespace = B.space <$ (skipMany1 spaceChar)
bareURL  = undefined
bold  = undefined
italic  = undefined
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
testP p s = runIO $ runParser p s

--type Parser = Parsec String ()
{--runP :: VwParser PandocIO a -> [Char] -> IO (Either PandocError a)
runP p opts s = do
  res <- readWithM p def{ stateOptions = opts } s
  case res of
       Left e -> throwError e
       Right result -> return result--}
--runP p s = runIO (mapLeft (PandocParsecError s) `liftM` runParserT p def{ stateOptions = def :: ReaderOptions } "test" s)
