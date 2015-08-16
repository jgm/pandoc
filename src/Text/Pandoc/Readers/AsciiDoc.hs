-- | == Parses AsciiDoc files
--
-- We based the parser on the following <http://asciidoctor.org/docs/user-manual/ specification>
-- We tried also to check it against the CLI implementation (ruby gem asciidoctor, on Mac)
--
-- ==== Supported features
--
--    * Titles of any level
--    * Literal paragraphs
--    * Horizontal rules
--    * Simple hyperlink support (with alias but no alias formatting)
--    * Strong section (bold)
--    * Emphasized section (italic)
--    * The rest default to plain paragraph
--    * Page Breaks
module Text.Pandoc.Readers.AsciiDoc (readAsciiDoc) where

import Data.Maybe
import Control.Monad (liftM)

import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Error
import qualified Text.Pandoc.Builder as B

-- import Debug.Trace (trace)
-- import Text.Printf (printf)

type AsciiDocParser = Parser String ParserState

readAsciiDoc :: ReaderOptions
             -> String
             -> (Either PandocError Pandoc)
readAsciiDoc opts s =
  (readWith parseAsciiDoc) def{ stateOptions = opts } (s ++ "\n\n")

parseAsciiDoc :: AsciiDocParser Pandoc
parseAsciiDoc = do
  blocks <- parseBlocks
  st <- getState
  let bs = B.toList $ runF blocks st
  return $ Pandoc nullMeta bs

parseBlocks :: AsciiDocParser (F B.Blocks)
parseBlocks = mconcat <$> manyTill block eof

block :: AsciiDocParser (F B.Blocks)
block = choice [ mempty <$ blanklines
               , mempty <$ pageBreak
               , title
               , literalParagraph
 --            , documentTitle
 --            , explicitId
               , hrule
 --            , list
 --            , labeledLine
 --            , labeledMultiLine
 --            , image
 --            , blockCode
 --            , citation -- inline
 --            , table
               , paragraph
               ] <?> "block"

-- | An horizontal rule is a line containing only ', and at least 3
hruleAsciiDoc :: AsciiDocParser (F B.Blocks)
hruleAsciiDoc = try $ do
  count 3 (char '\'')
  skipMany (char '\'')
  skipMany spaceChar
  newline
  return $ return $ B.horizontalRule

-- | The markdown style hrule is composed of exactly 3 '-' (dash)
-- or exactly 3 '*' (star)
-- there should not be any leading space on the line
-- there can be 0 or 1 space in between the chars
hruleMarkdown :: AsciiDocParser (F B.Blocks)
hruleMarkdown = try $ do
  choice [ (try $ string "---")
        , (try $ string "- - -")
        , (try $ string "***")
        , (try $ string "* * *")
    ] <?> "markdown style hrule"
  skipMany spaceChar
  newline
  return $ return $ B.horizontalRule

hrule :: AsciiDocParser (F B.Blocks)
hrule = try $ do
  -- Horizontal rules always start the line
  pos <- getPosition
  if (sourceColumn pos) /= 1
    then unexpected "hrule always start the line"
    else hruleAsciiDoc <|> hruleMarkdown

pageBreak :: AsciiDocParser [Char]
pageBreak = try $ do
  count 3 (char '<')

paragraph :: AsciiDocParser (F B.Blocks)
paragraph = do
  paraText <- mconcat <$> many1 inline
  return $ do
    paraText' <- paraText
    return $ B.para paraText'

title :: AsciiDocParser (F B.Blocks)
title = (titleWithUnderline '=' 1) <|>
        (titleWithUnderline '-' 2) <|>
        (titleWithPrefix '=') <|>
        (titleWithPrefix '#')

titleWithPrefix :: Char -> AsciiDocParser (F B.Blocks)
titleWithPrefix c = try $ do
  posBefore <- getPosition
  many1 (char c)
  posAfter <- getPosition
  let level = (sourceColumn posAfter) - (sourceColumn posBefore)
  space
  lineText <- anyLine
  -- AsciiDoc does not support title of level > 5 (counts begins at 0 in the doc)
  if level > 6
    then unexpected "too long title"
    else return $ return $ (B.headerWith nullAttr level (B.str lineText))

titleWithUnderline :: Char -- ^ the char used to underline the title
                   -> Int  -- ^ the level of the title corresponding to the underlining char
                   -> AsciiDocParser (F B.Blocks)
titleWithUnderline uCar level = try $ do
  titleText <- anyLine
  titleUChar <- many1 (char uCar)
  newline
  -- By experimenting with the asciidoc CLI, it seems that the "underline title"
  -- feature works only if the underline line is of the same length as the title
  -- text, with a margin of 2 characters
  -- Does not seem to be documented though
  if ((length titleUChar) >= (length titleText) - 2) && ((length titleUChar) <= (length titleText) + 2)
    then return $ return $ (B.headerWith nullAttr level (B.str titleText))
    else unexpected "bad number of title underlying characters"

blockQuoteStart :: AsciiDocParser Char
blockQuoteStart = try $ char ' '

blockQuote :: AsciiDocParser [String]
blockQuote = try $ do
  lookAhead blockQuoteStart -- 1st line
  let blockLine = many1Till notEndline newline
  manyTill blockLine (try $ blanklines <|> (newline >> many1 blankline))

notEndline :: AsciiDocParser Char
notEndline = satisfy (/= '\n')

-- TODO: convert inner blockLines
literalParagraph :: AsciiDocParser (F B.Blocks)
literalParagraph = try $ do
    blockLines <- blockQuote
    let unecessarySpaces = minimum $ map (length . takeWhile (==' ')) blockLines
    let unindent = drop unecessarySpaces
    return . return $ B.blockQuote $ B.plain $ B.fromList $ map (Str . unindent) blockLines

inline :: AsciiDocParser (F B.Inlines)
inline = do
  notFollowedBy pageBreak
  choice [
    whitespace
    , endline
    , bold
    , emph
    , link
    , str
    -- specialChar MUST be after str, which catches the alphanum string
    , specialChar
    ] <?> "inlines"

-- | Parses inline elements enclosed inside markers
-- The starting marker must not be followed by a space
-- The ending marker must not be preceded by a space
-- The ending marker must not be followed by an alphanumeric
enclosedBetween :: AsciiDocParser String
         -> AsciiDocParser (F B.Inlines)
enclosedBetween delimiter =
  mconcat <$> (
    try $
      (delimiter >> (notFollowedBy spaceChar))
      >>
      (manyTill
        ((spacePrecededClosingDelimiter delimiter) <|> inline)
        (delimiter >> (notFollowedBy alphaNum))
      )
    )

spacePrecededClosingDelimiter :: AsciiDocParser String
                              -> AsciiDocParser (F B.Inlines)
spacePrecededClosingDelimiter delimiter = try $ do
  del <- spaceChar >> delimiter
  return $ return $ B.space B.<> (B.str del)

emph :: AsciiDocParser (F B.Inlines)
emph = (liftM (liftM B.emph) (enclosedBetween (string "_")))

bold :: AsciiDocParser (F B.Inlines)
bold = (liftM (liftM B.strong) (enclosedBetween (string "*")))

commonURLScheme :: [String]
-- Careful to keep https before http for pattern matching
-- TODO: gbataille - mailto need a different parser
commonURLScheme = ["https", "http", "ftp", "irc", "mailto"]

link :: AsciiDocParser (F B.Inlines)
link = try $ do
  protocol <- oneOfStrings commonURLScheme
  sep <- string "://"
  domains <- many1 subDomain
  let domains' = mconcat domains
  extension <- many1 $ noneOf " .\n\t/["
  rest <- many $ noneOf " \n\t["
  mbAlias <- optionMaybe . try  $ (string "[") *> (mconcat <$> (manyTill inline (string "]")))
  let fullURL = protocol ++ sep ++ domains' ++ extension ++ rest
  return $ (return $ B.link (fullURL)
                 (domains' ++ extension))
                 <*> (linkAlias mbAlias (pure (B.str fullURL)))

linkAlias :: Maybe (F B.Inlines)  -- ^ parsed alias, if any (can be empty)
          -> F B.Inlines          -- ^ default value
          -> F B.Inlines
linkAlias mbAlias defaultValue = do
  let falias = fromMaybe defaultValue mbAlias
  alias <- falias
  unfDefault <- defaultValue
  return $ defaultIfEmpty alias unfDefault

defaultIfEmpty :: B.Inlines -- ^ value
               -> B.Inlines -- ^ default
               -> B.Inlines
defaultIfEmpty value defaultValue
  | B.isNull value = defaultValue
  | otherwise    = value

subDomain :: AsciiDocParser (String)
subDomain = try $ do
  domain <- (many1 $ noneOf ". \n\t/")
  dot <- (string ".")
  return (domain ++ dot)

endline :: AsciiDocParser (F B.Inlines)
endline = try $ do
  newline
  notFollowedBy blankline
  return $ return $ B.space

str :: AsciiDocParser (F B.Inlines)
str = try $ do
  wordText <- many1 alphaNum
  return $ return $ B.str wordText

-- | Definition sufficient like that ONLY because the alphanum parser is invoked before
specialChar :: AsciiDocParser (F B.Inlines)
specialChar = try $ do
  c <- nonspaceChar
  return $ return $ B.str [c]

whitespace :: AsciiDocParser (F B.Inlines)
whitespace = try $ do
  spaceChar
  return $ return $ B.space
