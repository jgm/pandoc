module Text.Pandoc.Readers.AsciiDoc (
  readAsciiDoc
) where

import qualified Data.Sequence as Seq
import Data.Monoid (mconcat, mempty)
import Control.Applicative ((<$>), (<$))

import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import qualified Text.Pandoc.Builder as B

import Debug.Trace (trace)
import Text.Printf (printf)

type AsciiDocParser = Parser String ParserState

readAsciiDoc :: ReaderOptions
             -> String
             -> Pandoc
-- readAsciiDoc opts s = Pandoc (Meta (Map.singleton "foo" (MetaString "bar"))) []
readAsciiDoc opts s =
  (readWith parseAsciiDoc) def{ stateOptions = opts } (s ++ "\n\n")

parseAsciiDoc :: AsciiDocParser Pandoc
parseAsciiDoc = do
  -- markdown allows raw HTML
  blocks <- parseBlocks
  st <- getState
  let Pandoc _ bs = B.doc $ runF blocks st
  return $ Pandoc nullMeta bs

parseBlocks :: AsciiDocParser (F B.Blocks)
parseBlocks = mconcat <$> manyTill block eof

block :: AsciiDocParser (F B.Blocks)
block = do
  -- tr <- getOption readerTrace
  -- pos <- getPosition
  res <- choice [ mempty <$ blanklines
                -- TODO: gbataille - remove
             --   , return <$> (fmap (B.Many . Seq.singleton . Para . (:[]) . Str) anyLine)
--                , literalParagraph
               , title
--                , documentTitle
--                , explicitId
--                , hrule
--                , pageBreak
--                , list
--                , labeledLine
--                , labeledMultiLine
--                , links
--                , image
--                , blockCode
-- --               , citation -- inline
--                , table
                , fmap (return . B.Many . Seq.singleton . Para . (:[]) . Str) paragraph
               ] <?> "wtf***"
  -- when tr $ do
  --   st <- getState
  --   trace (printf "line %d: %s" (sourceLine pos)
  --          (take 60 $ show $ B.toList $ runF res st)) (return ())
  return res

-- anyLine :: AsciiDocParser (F B.Blocks)
-- anyLine = anyChar


paragraph :: AsciiDocParser String
paragraph = manyTill (anyChar) (try $ newline >> many1 blankline)

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
  str <- anyLine
  -- AsciiDoc does not support title of level > 5 (counts begins at 0 in the doc)
  if level > 6
    then unexpected "too long title"
    else return $ return $ (B.headerWith nullAttr level ((B.Many . Seq.singleton . Str) str))

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
    then return $ return $ (B.headerWith nullAttr level ((B.Many . Seq.singleton . Str) titleText))
    else unexpected "bad number of title underlying characters"
