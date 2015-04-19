-- | == Parses AsciiDoc files
--
-- While we tried to follow the <http://asciidoctor.org/docs/user-manual/ specification>
-- we tried also to check it against the CLI implementation (on Mac) and did not implement
-- some of the features described but that don't exist, including
--
--    * Markdown style horizontal rule
--    * Markdown style titles using '#'
module Text.Pandoc.Readers.AsciiDoc where

import Data.Monoid (mconcat, mempty)
import Control.Applicative ((<$>), (<$))

import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import qualified Text.Pandoc.Builder as B

-- import Debug.Trace (trace)
-- import Text.Printf (printf)

type AsciiDocParser = Parser String ParserState

readAsciiDoc :: ReaderOptions
             -> String
             -> Pandoc
-- readAsciiDoc opts s = Pandoc (Meta (Map.singleton "foo" (MetaString "bar"))) []
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
block = do
  -- tr <- getOption readerTrace
  -- pos <- getPosition
  res <- choice [ mempty <$ blanklines
               , title
               , literalParagraph
--                , documentTitle
--                , explicitId
               , hrule
--                , pageBreak
--                , list
--                , labeledLine
--                , labeledMultiLine
--                , links
--                , image
--                , blockCode
-- --               , citation -- inline
--                , table
                , paragraph
               ] <?> "block"
  return res

-- | An horizontal rule is a line containing only ', and at least 3
hrule :: AsciiDocParser (F B.Blocks)
hrule = try $ do
  -- Horizontal rules always start the line
  pos <- getPosition
  if (sourceColumn pos) /= 1
    then unexpected "hrule always start the line"
    else do
      count 3 (char '\'')
      skipMany (char '\'')
      newline
      return $ return $ B.horizontalRule

paragraph :: AsciiDocParser (F B.Blocks)
paragraph = do
  paraText <- mconcat <$> many1 inline
  return $ do
    paraText' <- paraText
    return $ B.para paraText'

title :: AsciiDocParser (F B.Blocks)
title = (titleWithUnderline '=' 1) <|>
        (titleWithUnderline '-' 2) <|>
        titleWithPrefix

titleWithPrefix :: AsciiDocParser (F B.Blocks)
titleWithPrefix = try $ do
  posBefore <- getPosition
  many1 (char '=')
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
inline = choice [
  whitespace
  , endline
  , str
  -- specialChar MUST be after str, which catches the alphanum string
  , specialChar
  ] <?> "inlines"

endline :: AsciiDocParser (F B.Inlines)
endline = try $ do
  newline
  notFollowedBy blankline
  return $ return $ B.space

str :: AsciiDocParser (F B.Inlines)
str = try $ do
  wordText <- many1 alphaNum
  return $ return $ B.str wordText

specialChar :: AsciiDocParser (F B.Inlines)
specialChar = try $ do
  c <- noneOf "\n "
  return $ return $ B.str [c]

-- stopOnInlineBlockElem :: AsciiDocParser (F B.Inlines)
-- stopOnInlineBlockElem = do
--   hrule
--   unexpected "Stops the inline parsing"

whitespace :: AsciiDocParser (F B.Inlines)
whitespace = try $ do
  spaceChar
  return $ return $ B.space
