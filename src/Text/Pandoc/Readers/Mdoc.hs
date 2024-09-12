{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Mdoc
   Copyright   : 
   License     : GNU GPL, version 2 or above

   Maintainer  : a
   Stability   : WIP
   Portability : portable

Conversion of man to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Mdoc (readMdoc) where

import Data.Default (Default)
import Control.Monad (mplus, guard, void, when)
import Control.Monad.Except (throwError)
import Data.List (intersperse)
import qualified Data.Text as T
import Text.Pandoc.Definition (Pandoc(Pandoc), Meta)
import Text.Pandoc.Builder (Blocks, Inlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad(..))
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Readers.Mdoc.Lex as Lex  -- TODO explicit imports
import Text.Parsec (modifyState)
import qualified Text.Pandoc.Parsing as P
import qualified Data.Foldable as Foldable
import Text.Pandoc.Shared (stringify)

data MdocSection
  = ShName
  | ShSynopsis
  | ShOther
  deriving (Show, Eq)

data ManState = ManState { readerOptions   :: ReaderOptions
                         , metadata        :: Meta
                         , tableCellsPlain :: Bool
                         , progName :: Maybe T.Text
                         , currentSection :: MdocSection
                         } deriving Show

instance Default ManState where
  def = ManState { readerOptions   = def
                 , metadata        = B.nullMeta
                 , tableCellsPlain = True
                 , currentSection = ShOther
                 , progName = Nothing }

type MdocParser m = P.ParsecT [MdocToken] ManState m


-- | Read man (troff) from an input string and return a Pandoc document.
readMdoc :: (PandocMonad m, ToSources a)
        => ReaderOptions
        -> a
        -> m Pandoc
readMdoc opts s = do
  let Sources inps = toSources s
  tokenz <- mconcat <$> mapM (uncurry lexMdoc) inps
  let state = def {readerOptions = opts} :: ManState
  eitherdoc <- readWithMTokens parseMdoc state
     (Foldable.toList . unRoffTokens $ tokenz)
  either (throwError . fromParsecError (Sources inps)) return eitherdoc


readWithMTokens :: PandocMonad m
        => ParsecT [MdocToken] ManState m a  -- ^ parser
        -> ManState                         -- ^ initial state
        -> [MdocToken]                       -- ^ input
        -> m (Either ParseError a)
readWithMTokens parser state input =
  runParserT parser state "source" input


parseMdoc :: PandocMonad m => MdocParser m Pandoc
parseMdoc = do
  optional parsePrologue
  bs <- many parseBlock <* eof
  meta <- metadata <$> getState
  let (Pandoc _ blocks) = B.doc $ mconcat bs
  return $ Pandoc meta blocks

msatisfy :: Monad m
         => (MdocToken -> Bool) -> P.ParsecT [MdocToken] st m MdocToken
msatisfy predic = P.tokenPrim show nextPos testTok
  where
    testTok t     = if predic t then Just t else Nothing
    nextPos _ _ (Macro _ pos':_) = pos'
    nextPos _ _ (Lit _ pos':_) = pos'
    nextPos _ _ (Str _ pos':_) = pos'
    nextPos _ _ (Delim _ _ pos':_) = pos'
    nextPos _ _ (Blank pos':_) = pos'
    nextPos _ _ (Tbl _ _ pos':_) = pos'
    nextPos a _ (Eol{}:x:xs) = nextPos a x xs
    nextPos pos _ [Eol] = pos
    nextPos pos _ [] = pos

macro :: PandocMonad m => T.Text -> MdocParser m MdocToken
macro name = msatisfy t where
  t (Macro n _) = n == name
  t _ = False

emptyMacro :: PandocMonad m => T.Text -> MdocParser m MdocToken
emptyMacro n = macro n <* eol

delim :: PandocMonad m => DelimSide -> MdocParser m MdocToken
delim side = msatisfy t where
  t (Delim s _ _) = side == s
  t _ = False

str :: PandocMonad m => MdocParser m MdocToken
str = msatisfy t where
  t Str{} = True
  t _ = False

lit :: PandocMonad m => MdocParser m MdocToken
lit = msatisfy t where
  t Lit{} = True
  t _ = False

arg :: PandocMonad m => MdocParser m MdocToken
arg = msatisfy t where
  t Lit{} = True
  t Macro{} = True
  t _ = False

literal :: PandocMonad m => T.Text -> MdocParser m MdocToken
literal n = msatisfy t where
  t (Lit n' _) = n == n'
  t _ = False

blank :: PandocMonad m => MdocParser m MdocToken
blank = msatisfy t where
  t Blank{} = True
  t _ = False

eol :: PandocMonad m => MdocParser m ()
eol = void $ msatisfy t where
  t Eol{} = True
  t _ = False

inlineContextEnd :: PandocMonad m => MdocParser m ()
inlineContextEnd = eof <|> (void . lookAhead $ msatisfy t) where
  t Eol{} = True
  t Macro{} = True
  t Str{} = True -- shouldn't be lexed
  t Tbl{} = True -- shouldn't be lexed
  t Blank{} = True -- shouldn't be lexed
  t Lit{} = False
  t Delim{} = False

argsToInlines :: PandocMonad m => MdocParser m Inlines
argsToInlines = do
  ls <- manyTill arg eol
  let strs = map (B.str . toString) ls
  return $ mconcat $ intersperse B.space strs

parsePrologue :: PandocMonad m => MdocParser m ()
parsePrologue = do
  macro "Dd"
  date <- argsToInlines
  macro "Dt"
  (Lit title _) <- lit
  (Lit section _) <- lit
  eol
  emptyMacro "Os"
  let adjust = B.setMeta "title" (B.str title) . B.setMeta "date" date . B.setMeta "section" (B.str section)
  modifyState $ \s -> s{metadata = adjust $ metadata s}

shToSectionMode :: T.Text -> MdocSection
shToSectionMode "NAME" = ShName
shToSectionMode "SYNOPSIS" = ShSynopsis
shToSectionMode _ = ShOther

parseHeader :: PandocMonad m => MdocParser m Blocks
parseHeader = do
  (Macro m _) <- macro "Sh" <|> macro "Ss"
  txt <- argsToInlines
  let lvl = if m == "Sh" then 1 else 2
  when (lvl == 1) $ modifyState $ \s -> s{currentSection = (shToSectionMode . stringify) txt}
  return $ B.header lvl txt

parseNameSection :: PandocMonad m => MdocParser m Blocks
parseNameSection = do
  sec <- currentSection <$> getState
  guard $ sec == ShName
  macro "Nm" <|> macro "Fn"
  pname <- toString <$> arg
  -- TODO multiple Nm macros and delimiters
  eol
  macro "Nd"
  desc <- argsToInlines
  modifyState $ \s -> s{progName = mplus (progName s) (Just pname)}
  return $ B.para $ B.code pname <> B.space <> "—" <> B.space <> desc

parseSynopsisSection :: PandocMonad m => MdocParser m Blocks
parseSynopsisSection = do
  sec <- currentSection <$> getState
  guard $ sec == ShSynopsis
  return mempty

-- parseStr doesn't use B.text because roff(7) specifies that
-- whitespace in text lines is treated literally.
-- XXX is this what we actually want?
parseStr :: PandocMonad m => MdocParser m Inlines
parseStr = do
  (Str txt _) <- str
  return $ B.str txt

parseLit :: PandocMonad m => MdocParser m Inlines
parseLit = do
  (Lit txt _) <- lit
  return $ B.str txt

parseDelim :: PandocMonad m => DelimSide -> MdocParser m Inlines
parseDelim pos = do
  (Delim _ txt _) <- delim pos
  return $ B.str txt

litsToText :: PandocMonad m => MdocParser m Inlines
litsToText = do
  ls <- many1 lit
  let strs = map (B.str . toString) ls
  return $ mconcat $ intersperse B.space strs

simpleInline :: PandocMonad m => T.Text -> (Inlines -> Inlines) -> MdocParser m Inlines
simpleInline nm xform = do
  macro nm
  segs <- manyTill segment inlineContextEnd
  return $ mconcat $ intersperse B.space segs
 where
   segment = do
      openDelim <- mconcat <$> many (parseDelim Open)
      inlines <- option mempty litsToText
      closeDelim <- mconcat <$> many (parseDelim Close)
      let xform' x = if null x then mempty else xform x
      return $ openDelim <> xform' inlines <> closeDelim


-- Sy: callable, parsed, >0 arguments
-- mandoc -T html formats Sy with a <b> tag, since it's not really
-- semantically <strong>, but Strong is our best option in Pandoc
parseSy :: PandocMonad m => MdocParser m Inlines
parseSy = simpleInline "Sy" B.strong

parseEm :: PandocMonad m => MdocParser m Inlines
parseEm = simpleInline "Em" B.emph

parseInlineMacro :: PandocMonad m => MdocParser m Inlines
parseInlineMacro = choice [ parseSy, parseEm ]

-- TODO this doesn't handle inline macros being interrupted
-- by other ones yet, but the lexer doesn't handle it yet
-- either
parseInline :: PandocMonad m => MdocParser m Inlines
parseInline = parseStr <|> (parseInlineMacro <* eol)

parseInlines :: PandocMonad m => MdocParser m Inlines
parseInlines = mconcat . intersperse B.space <$> many1 parseInline

parsePara :: PandocMonad m => MdocParser m Blocks
parsePara = do
  optional (emptyMacro "Pp")
  B.para . B.trimInlines <$> parseInlines

skipBlanks :: PandocMonad m => MdocParser m Blocks
skipBlanks = many1 blank *> mempty

parseBlock :: PandocMonad m => MdocParser m Blocks
parseBlock = choice [ -- parseList
                    -- , parseDefinitionList
                    parseHeader
                    , parseNameSection
                    , parseSynopsisSection
                    , parsePara
                    -- , parseTable
                    --, parseCodeBlock
                    , skipBlanks
                    -- , parseBlockQuote
                    -- , parseNewParagraph
                    -- , skipUnknownMacro
                    ]

