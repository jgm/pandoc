import Control.Monad.Except (throwError)
import Control.Monad (guard)
import Data.Default -- def is there
import Text.Pandoc.Builder (doc, Blocks, Inlines, toList, trimInlines, headerWith)
import Text.Pandoc.Class (PandocMonad, report, PandocIO, runIO)
import Text.Pandoc.Definition (Pandoc, nullAttr)
import Text.Pandoc.Logging (LogMessage(ParsingTrace))
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Parsing (readWithM, ParserT, stateOptions, ParserState, blanklines, registerHeader)
import Text.Parsec.Char (spaces, char)
import Text.Parsec.Combinator (eof, choice, many1, manyTill, count)
import Text.Parsec.Pos (sourceColumn)
import Text.Parsec.Prim (many, getPosition, try)

readVimwiki :: PandocMonad m => ReaderOptions -> String -> m Pandoc
readVimwiki opts s = do
  res <- readWithM parseVimwiki def{ stateOptions = opts } (s ++ "\n")
  case res of
       Left e -> throwError e
       Right result -> return result

type VwParser = ParserT [Char] ParserState

-- main parser

parseVimwiki :: PandocMonad m => VwParser m Pandoc
parseVimwiki = do
  bs <- mconcat <$> many block
  spaces
  eof
  return $ doc bs

-- block parser

block :: PandocMonad m => VwParser m Blocks
block = do
  pos <- getPosition
  res <- choice [ mempty <$ blanklines
                , header
                , bulletList
                , orderedList
                , hrule
                , table
                , blockQuote
                , para
                , preformatted
                , blockMath
                ]
  report $ ParsingTrace (take 60 $ show $ toList res) pos
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
  guardColumnOne
  eqs <- many1 (char '=')
  let lev = length eqs
  guard $ lev <= 6
  contents <- trimInlines . mconcat <$> manyTill inline (count lev $ char '=')
  attr <- registerHeader nullAttr contents
  return $ headerWith attr lev contents

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
inline = choice [ whitespace
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
                ]

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

whitespace  = undefined
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

