{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Mdoc
   Copyright   : 
   License     : GNU GPL, version 2 or above

   Maintainer  : a
   Stability   : WIP
   Portability : portable

Conversion of mdoc to 'Pandoc' document.
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

  {- As a general principle, if mandoc -T lint issues a WARNING admonition
     or worse about a construct, I consider it fair game for this reader to
     do something different than what mandoc does with it, including bailing
     out instead of recovering. -}

data MdocSection
  = ShName
  | ShSynopsis
  | ShAuthors
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


-- | Read mdoc from an input string and return a Pandoc document.
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
  return $ spacify strs

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
shToSectionMode "AUTHORS" = ShAuthors
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
  return $ spacify strs

litsAndDelimsToText :: PandocMonad m => MdocParser m Inlines
litsAndDelimsToText = do
  ods <- mconcat <$> many (parseDelim Open)
  ls <- many lit
  cds <- mconcat <$> if null ods && null ls
                        then many1 (parseDelim Close)
                        else many (parseDelim Close)
  let strs = map (B.str . toString) ls
  return $ ods <> spacify strs <> cds

delimitedArgs :: PandocMonad m => MdocParser m x -> MdocParser m (Inlines, x, Inlines)
delimitedArgs p = do
    openDelim <- mconcat <$> many (parseDelim Open)
    inlines <- p
    closeDelim <- mconcat <$> many (parseDelim Close)
    return (openDelim, inlines, closeDelim)

-- TODO extract further?
simpleInline :: PandocMonad m => T.Text -> (Inlines -> Inlines) -> MdocParser m Inlines
simpleInline nm xform = do
  macro nm
  segs <- manyTill segment inlineContextEnd
  return $ spacify segs
 where
   segment = do
      (openDelim, inlines, closeDelim) <- delimitedArgs $ option mempty litsToText
      return $ openDelim <> xform inlines <> closeDelim

lineEnclosure :: PandocMonad m => T.Text -> (Inlines -> Inlines) -> MdocParser m Inlines
lineEnclosure nm xform = do
  macro nm
  --- XXX wtf
  (first, further, finally) <- delimitedArgs
    (manyTill
      (parseInlineMacro <|> (try (litsAndDelimsToText <* notFollowedBy eol))
        <|> litsToText) (lookAhead (many (delim Close) *> eol)))
  return $ first <> xform (spacify further) <> finally

noSpace :: Inlines
noSpace = B.rawInline "mdoc" "Ns"

data SpacifyState = SpacifyState { accum :: [Inlines], prev :: Inlines, ns :: Bool }
instance Default SpacifyState where def = SpacifyState [] mempty False

foldNoSpaces :: [Inlines] -> [Inlines]
foldNoSpaces xs = (finalize . foldl go def) xs
  where
    go :: SpacifyState -> Inlines -> SpacifyState
    go s x
      | ns s && x == noSpace = s
      | ns s                 = s{prev = prev s <> x, ns = False}
      |         x == noSpace = s{ns = True}
      | null (prev s)        = s{prev = x}
      | otherwise            = s{accum = accum s <> [prev s], prev = x}
    finalize s
      | null (prev s) = accum s
      | otherwise     = accum s <> [prev s]

spacify :: [Inlines] -> Inlines
spacify = mconcat . intersperse B.space . foldNoSpaces

{- Compatibility note: mandoc permits, and doesn't warn on, "vertical" macros
 (Pp, Bl/El, Bd/Ed) inside of "horizontal" block partial-explicit quotations
like Do/Dc. However there are no OpenBSD manual pages that employ such markup
and it doesn't look right when rendered. We don't attempt to consume anything
but pandoc inlines inside of these multiline enclosures. -}
multilineEnclosure :: PandocMonad m => T.Text -> T.Text -> (Inlines -> Inlines) -> MdocParser m Inlines
multilineEnclosure op cl xform = do
  macro op
  (first, further, finally) <- delimitedArgs (manyTill parseInlines (macro cl))
  return $ first <> xform (spacify further) <> finally

eliminateEmpty :: (Inlines -> Inlines) -> Inlines -> Inlines
eliminateEmpty x y = if null y then mempty else x y

cls :: T.Text -> B.Attr
cls x = (mempty, [x], mempty)

-- Sy: callable, parsed, >0 arguments
-- mandoc -T html formats Sy with a <b> tag, since it's not really
-- semantically <strong>, but Strong is our best option in Pandoc
parseSy :: PandocMonad m => MdocParser m Inlines
parseSy = simpleInline "Sy" (eliminateEmpty B.strong)

parseEm :: PandocMonad m => MdocParser m Inlines
parseEm = simpleInline "Em" (eliminateEmpty B.emph)

parseQl :: PandocMonad m => MdocParser m Inlines
parseQl = lineEnclosure "Ql" $ B.codeWith (cls "Ql") . stringify

parseDq :: PandocMonad m => MdocParser m Inlines
parseDq = lineEnclosure "Dq" B.doubleQuoted

parseDo :: PandocMonad m => MdocParser m Inlines
parseDo = ptrace $ multilineEnclosure "Do" "Dc" B.doubleQuoted

parseSq :: PandocMonad m => MdocParser m Inlines
parseSq = lineEnclosure "Sq" B.singleQuoted

parseQq :: PandocMonad m => MdocParser m Inlines
parseQq = lineEnclosure "Qq" $ \x -> "\"" <> x <> "\""

parsePq :: PandocMonad m => MdocParser m Inlines
parsePq = lineEnclosure "Pq" $ \x -> "(" <> x <> ")"

parseBq :: PandocMonad m => MdocParser m Inlines
parseBq = lineEnclosure "Bq" $ \x -> "[" <> x <> "]"

-- For our purposes this probably behaves identically to Bq
-- in most circumstances but I might need to do something
-- special with it in SYNOPSIS
parseOp :: PandocMonad m => MdocParser m Inlines
parseOp = lineEnclosure "Op" $ \x -> "[" <> x <> "]"

parseBrq :: PandocMonad m => MdocParser m Inlines
parseBrq = lineEnclosure "Brq" $ \x -> "{" <> x <> "}"

parseAq :: PandocMonad m => MdocParser m Inlines
parseAq = lineEnclosure "Aq" $ \x -> "⟨" <> x <> "⟩"

parseNm :: PandocMonad m => MdocParser m Inlines
parseNm = do
  mnm <- (progName <$> getState)
  case mnm of
    Nothing -> do
      (_, nm, _) <- lookAhead $ delimitedArgs $ option mempty litsToText
      guard $ not (null nm)
      simpleInline "Nm" ok
    Just nm -> simpleInline "Nm" $ \x ->
      if null x
         then B.codeWith (cls "Nm") nm
         else ok x
  where ok = B.codeWith (cls "Nm") . stringify

-- Xr
parseXr :: PandocMonad m => MdocParser m Inlines
parseXr = do
  macro "Xr"
  (open, (name, section), close) <- delimitedArgs f
  let ref = name <> "(" <> section <> ")"
  return $ open <> B.spanWith (cls "Xr") (B.str ref) <> close
    where
      f = do
        n <- lit <?> "Xr manual name"
        s <- lit <?> "Xr manual section"
        return (toString n, toString s)

parseNs :: PandocMonad m => MdocParser m Inlines
parseNs = macro "Ns" >> return noSpace

-- TODO should possibly rename this function b/c some of these are
-- Mdoc block partial-implicit macros. Unclear if this distinction
-- is going to be relevant.
parseInlineMacro :: PandocMonad m => MdocParser m Inlines
parseInlineMacro =
  choice
    [ parseSy,
      parseEm,
      parseNm,
      parseXr,
      parseQl,
      parseOp,
      parseSq,
      parseDq,
      parseQq,
      parsePq,
      parseBq,
      parseBrq,
      parseAq,
      parseDo,
      parseNs
    ]

parseInline :: PandocMonad m => MdocParser m Inlines
parseInline = parseStr  <|>
  ((parseInlineMacro <|> litsAndDelimsToText) <* optional eol)


-- TODO probably need some kind of fold to deal with Ns
parseInlines :: PandocMonad m => MdocParser m Inlines
parseInlines = spacify <$> many1 parseInline

parsePara :: PandocMonad m => MdocParser m Blocks
parsePara = do
  optional (emptyMacro "Pp" <|> emptyMacro "Lp")  -- Lp: deprecated synonym for Pp
  B.para . B.trimInlines <$> parseInlines

-- CodeBlocks can't contain any other markup, but mdoc
-- still interprets control lines within .Bd -literal
-- blocks. Just ignoring this for now and failing if
-- we get any control lines inside a Bd literal
parseCodeBlock :: PandocMonad m => MdocParser m Blocks
parseCodeBlock = do
  macro "Bd" -- TODO will need to hoist
  literal "-literal"
  optional (literal "-offset" *> lit)
  optional (literal "-compact")
  eol
  l <- T.unlines . map toString <$> many (str <|> blank)
  emptyMacro "Ed"
  return $ B.codeBlock l


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
                    , parseCodeBlock
                    , skipBlanks
                    -- , parseBlockQuote
                    -- , parseNewParagraph
                    -- , skipUnknownMacro
                    ]

