{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Functor (($>))
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

data MdocState = MdocState
    { readerOptions :: ReaderOptions
    , metadata :: Meta
    , tableCellsPlain :: Bool
    , spacingMode :: Bool
    , progName :: Maybe T.Text
    , currentSection :: MdocSection
    }
    deriving (Show)

instance Default MdocState where
    def =
        MdocState
            { readerOptions = def
            , metadata = B.nullMeta
            , tableCellsPlain = True
            , spacingMode = True
            , currentSection = ShOther
            , progName = Nothing
            }

type MdocParser m = P.ParsecT [MdocToken] MdocState m


-- | Read mdoc from an input string and return a Pandoc document.
readMdoc :: (PandocMonad m, ToSources a)
        => ReaderOptions
        -> a
        -> m Pandoc
readMdoc opts s = do
  let Sources inps = toSources s
  tokenz <- mconcat <$> mapM (uncurry lexMdoc) inps
  let state = def {readerOptions = opts} :: MdocState
  eitherdoc <- readWithMTokens parseMdoc state
     (Foldable.toList . unRoffTokens $ tokenz)
  either (throwError . fromParsecError (Sources inps)) return eitherdoc


readWithMTokens :: PandocMonad m
        => ParsecT [MdocToken] MdocState m a  -- ^ parser
        -> MdocState                         -- ^ initial state
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

anyMacro :: PandocMonad m => MdocParser m MdocToken
anyMacro = msatisfy t where
  t (Macro _ _) = True
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

newControlContext :: MdocToken -> Bool
newControlContext Eol{} = True
newControlContext Macro{} = True
newControlContext Str{} = True
newControlContext Tbl{} = True
newControlContext Blank{} = True
newControlContext Lit{} = False
newControlContext Delim{} = False


inlineContextEnd :: PandocMonad m => MdocParser m ()
inlineContextEnd = eof <|> (void . lookAhead $ msatisfy newControlContext)

argsToInlines :: PandocMonad m => MdocParser m Inlines
argsToInlines = do
  ls <- manyTill arg eol
  let strs = map (B.str . toString) ls
  spacify strs

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
  (Macro m _) <- lookAhead $ macro "Sh" <|> macro "Ss"
  txt <- lineEnclosure m id
  eol
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
  -- TODO actually implement this
  manyTill (anyToken) (lookAhead (macro "Sh"))
  return mempty

parseStr :: PandocMonad m => MdocParser m Inlines
parseStr = do
  (Str txt _) <- str
  return $ B.text txt

-- Multiple consecutive strs within a block always need to get spaces between
-- them and then packed up together, because text lines are never affected by
-- the spacing mode. XXX but apparently this isn't actually true for mandoc,
-- so I'm not sure what's correct here yet.
parseStrs :: PandocMonad m => MdocParser m Inlines
parseStrs = do
  txt <- many1 parseStr
  return $ mconcat $ intersperse B.space txt

parseDelim :: PandocMonad m => DelimSide -> MdocParser m Inlines
parseDelim pos = do
  (Delim _ txt _) <- delim pos
  return $ B.str txt

litsToText :: PandocMonad m => MdocParser m [T.Text]
litsToText = do
  ls <- many1 lit
  return $ map toString ls

litsToInlines :: PandocMonad m => MdocParser m Inlines
litsToInlines = do
  ls <- many1 lit
  let strs = map (B.str . toString) ls
  spacify strs

litsAndDelimsToInlines :: PandocMonad m => MdocParser m Inlines
litsAndDelimsToInlines = do
  (o, ls, c) <- delimitedArgs $ many lit
  guard $ not (null o && null ls && null c)
  strs <- spacify $ map (B.str . toString) ls
  return $ o <> strs <> c

openingDelimiters :: PandocMonad m => MdocParser m Inlines
openingDelimiters = do
    openDelim <- mconcat <$> many (parseDelim Open)
    omids <- pipes
    let omid | null omids = mempty
             | otherwise = omids <> B.space
    return $ openDelim <> omid

pipes :: PandocMonad m => MdocParser m Inlines
pipes = many (parseDelim Middle) >>= spacify

closingDelimiters :: PandocMonad m => MdocParser m Inlines
closingDelimiters = do
    cmids <- pipes
    let cmid | null cmids = mempty
             | otherwise = B.space <> cmids
    closeDelim <- mconcat <$> many (parseDelim Close)
    return $ cmid <> closeDelim

delimitedArgs :: PandocMonad m => MdocParser m x -> MdocParser m (Inlines, x, Inlines)
delimitedArgs p = do
    openDelim <- openingDelimiters
    inlines <- p
    closeDelim <- closingDelimiters
    return (openDelim, inlines, closeDelim)

-- TODO extract further?
simpleInline :: PandocMonad m => T.Text -> (Inlines -> Inlines) -> MdocParser m Inlines
simpleInline nm xform = do
  macro nm
  segs <- manyTill segment inlineContextEnd
  spacify segs
 where
   segment = do
      (openDelim, inlines, closeDelim) <- delimitedArgs $ option mempty litsToInlines
      return $ openDelim <> xform inlines <> closeDelim

codeLikeInline :: PandocMonad m => T.Text -> MdocParser m Inlines
codeLikeInline nm = simpleInline nm (eliminateEmpty (B.codeWith (cls nm) . stringify))

spanLikeInline :: PandocMonad m => T.Text -> MdocParser m Inlines
spanLikeInline nm = simpleInline nm (eliminateEmpty (B.spanWith (cls nm)))

lineEnclosure :: PandocMonad m => T.Text -> (Inlines -> Inlines) -> MdocParser m Inlines
lineEnclosure nm xform = do
  macro nm
  --- XXX wtf
  (first, further, finally) <- delimitedArgs
    (manyTill
      (parseInlineMacro <|> (try (litsAndDelimsToInlines <* notFollowedBy eol))
        <|> litsToInlines) (lookAhead (many (macro "Ns" <|> delim Close) *> eol)))
  further' <- spacify further
  return $ first <> xform further' <> finally

noSpace :: Inlines
noSpace = B.rawInline "mdoc" "Ns"

apMacro :: Inlines
apMacro = B.rawInline "mdoc" "Ap"

smOff :: Inlines
smOff = B.rawInline "mdoc" "Sm off"

smOn :: Inlines
smOn = B.rawInline "mdoc" "Sm on"

data SpacifyState = SpacifyState
  { accum :: [Inlines],
    prev :: Inlines,
    ns :: Bool,
    sm :: Bool
  }

instance Default SpacifyState where
  def = SpacifyState [] mempty False True

foldNoSpaces :: [Inlines] -> [Inlines]
foldNoSpaces xs = (finalize . foldl go def) xs
  where
    go :: SpacifyState -> Inlines -> SpacifyState
    go s x
      | ns s && x == noSpace = s
      |         x == apMacro = s{prev = prev s <> "'", ns = True}
      |         x == noSpace = s{ns = True}
      |         x == smOn    = s{sm = True}
      | sm s && x == smOff   = s{accum = accum s <> [prev s], prev = mempty, sm = False}
      | ns s                 = s{prev = prev s <> x, ns = False}
      | not (sm s)           = s{prev = prev s <> x}
      | null (prev s)        = s{prev = x}
      | otherwise            = s{accum = accum s <> [prev s], prev = x}
    finalize s
      | null (prev s) = accum s
      | otherwise     = accum s <> [prev s]

spacify :: PandocMonad m => [Inlines] -> MdocParser m Inlines
spacify x = do
  mode <- spacingMode <$> getState
  return (go mode x)
  where
    go True = mconcat . intersperse B.space . foldNoSpaces
    go False = mconcat . foldNoSpaces

{- Compatibility note: mandoc permits, and doesn't warn on, "vertical" macros
 (Pp, Bl/El, Bd/Ed) inside of "horizontal" block partial-explicit quotations
like Do/Dc. However there are no OpenBSD manual pages that employ such markup
and it doesn't look right when rendered. We don't attempt to consume anything
but pandoc inlines inside of these multiline enclosures. -}
multilineEnclosure :: PandocMonad m => T.Text -> T.Text -> (Inlines -> Inlines) -> MdocParser m Inlines
multilineEnclosure op cl xform = do
  macro op
  openDelim <- mconcat <$> many (parseDelim Open)
  optional eol
  contents <- many parseInline
  (macro cl <?> show cl)
  closeDelim <- mconcat <$> many (parseDelim Close)
  optional eol
  contents' <- spacify contents
  return $ openDelim <> xform contents' <> closeDelim

eliminateEmpty :: (Inlines -> Inlines) -> Inlines -> Inlines
eliminateEmpty x y = if null y then mempty else x y

cls :: T.Text -> B.Attr
cls x = (mempty, [x], mempty)

-- mandoc -T html formats Sy with a <b> tag, since it's not really
-- semantically <strong>, but Strong is our best option in Pandoc
parseSy :: PandocMonad m => MdocParser m Inlines
parseSy = simpleInline "Sy" (eliminateEmpty B.strong)

parseEm :: PandocMonad m => MdocParser m Inlines
parseEm = simpleInline "Em" (eliminateEmpty B.emph)

parseNo :: PandocMonad m => MdocParser m Inlines
parseNo = simpleInline "No" (eliminateEmpty id)

parseEv :: PandocMonad m => MdocParser m Inlines
parseEv = codeLikeInline "Ev"

parseAd :: PandocMonad m => MdocParser m Inlines
parseAd = spanLikeInline "Ad"

parseMs :: PandocMonad m => MdocParser m Inlines
parseMs = spanLikeInline "Ms"

-- I'm not sure why mandoc inserts a ~ when Mt is missing an argument,
-- but it does, and it doesn't issue a warning, so that quirk is
-- retained.
parseMt :: PandocMonad m => MdocParser m Inlines
parseMt = simpleInline "Mt" mailto
  where mailto x | null x = B.link ("mailto:~") "" "~"
                 | otherwise = B.link ("mailto:" <> stringify x) "" x

parsePa :: PandocMonad m => MdocParser m Inlines
parsePa = simpleInline "Pa" p
  where p x | null x = B.spanWith (cls "Pa") "~"
            | otherwise = B.spanWith (cls "Pa") x

parseFl :: PandocMonad m => MdocParser m Inlines
parseFl = do
  macro "Fl"
  start <- option mempty (emptyWithDelim <|> flfl <|> emptyWithMacro <|> emptyEmpty)
  segs <- manyTill segment inlineContextEnd
  spacify ([start] <> segs)
 where
   emptyWithDelim = do
     lookAhead $ many1 (delim Middle <|> delim Close)
     ds <- closingDelimiters
     return $ fl "-" <> ds
   flfl = do
     lookAhead (macro "Fl")
     x:xs <- B.toList <$> parseFl
     let xx = B.codeWith (cls "Fl") $ "-" <> stringify x
     return $ xx <> B.fromList xs
   emptyWithMacro = do
     lookAhead anyMacro
     rest <- parseInlines
     return $ fl "-" <> rest
   emptyEmpty = lookAhead eol $> fl "-"
   segment = do
      (openDelim, inlines, closeDelim) <- delimitedArgs $ option mempty litsToText
      inner <- (spacify . (map fl) . flags) inlines
      return $ openDelim <> inner <> closeDelim
   fl = B.codeWith (cls "Fl")
   flags [] = ["-"]
   flags xs = map ("-" <>) xs

parseAr :: PandocMonad m => MdocParser m Inlines
parseAr = simpleInline "Ar" ar
  where ar x | null x = B.codeWith (cls "variable") "file ..."
             | otherwise = B.codeWith (cls "variable") $ stringify x


parseCm :: PandocMonad m => MdocParser m Inlines
parseCm = codeLikeInline "Cm"

parseIc :: PandocMonad m => MdocParser m Inlines
parseIc = codeLikeInline "Ic"

parseEr :: PandocMonad m => MdocParser m Inlines
parseEr = codeLikeInline "Er"

parseCd :: PandocMonad m => MdocParser m Inlines
parseCd = codeLikeInline "Cd"

parseQl :: PandocMonad m => MdocParser m Inlines
parseQl = lineEnclosure "Ql" $ B.codeWith (cls "Ql") . stringify

parseDq :: PandocMonad m => MdocParser m Inlines
parseDq = lineEnclosure "Dq" B.doubleQuoted

parseDo :: PandocMonad m => MdocParser m Inlines
parseDo = multilineEnclosure "Do" "Dc" B.doubleQuoted

parseSq :: PandocMonad m => MdocParser m Inlines
parseSq = lineEnclosure "Sq" B.singleQuoted

parseSo :: PandocMonad m => MdocParser m Inlines
parseSo = multilineEnclosure "So" "Sc" B.singleQuoted

parseQq :: PandocMonad m => MdocParser m Inlines
parseQq = lineEnclosure "Qq" $ \x -> "\"" <> x <> "\""

parseQo :: PandocMonad m => MdocParser m Inlines
parseQo = multilineEnclosure "Qo" "Qc" $ \x -> "\"" <> x <> "\""

parsePq :: PandocMonad m => MdocParser m Inlines
parsePq = lineEnclosure "Pq" $ \x -> "(" <> x <> ")"

parsePo :: PandocMonad m => MdocParser m Inlines
parsePo = multilineEnclosure "Po" "Pc" $ \x -> "(" <> x <> ")"

parseBq :: PandocMonad m => MdocParser m Inlines
parseBq = lineEnclosure "Bq" $ \x -> "[" <> x <> "]"

parseBo :: PandocMonad m => MdocParser m Inlines
parseBo = multilineEnclosure "Bo" "Bc" $ \x -> "[" <> x <> "]"

-- For our purposes this probably behaves identically to Bq
-- in most circumstances but I might need to do something
-- special with it in SYNOPSIS
parseOp :: PandocMonad m => MdocParser m Inlines
parseOp = lineEnclosure "Op" $ \x -> "[" <> x <> "]"

parseOo :: PandocMonad m => MdocParser m Inlines
parseOo =  multilineEnclosure "Oo" "Oc" $ \x -> "[" <> x <> "]"

parseBrq :: PandocMonad m => MdocParser m Inlines
parseBrq = lineEnclosure "Brq" $ \x -> "{" <> x <> "}"

parseBro :: PandocMonad m => MdocParser m Inlines
parseBro = multilineEnclosure "Bro" "Brc" $ \x -> "{" <> x <> "}"

parseAq :: PandocMonad m => MdocParser m Inlines
parseAq = lineEnclosure "Aq" $ \x -> "⟨" <> x <> "⟩"

parseAo :: PandocMonad m => MdocParser m Inlines
parseAo = multilineEnclosure "Ao" "Ac" $ \x -> "⟨" <> x <> "⟩"

parseNm :: PandocMonad m => MdocParser m Inlines
parseNm = do
  macro "Nm"
  mnm <- (progName <$> getState)
  (op, rg, cl) <- delimitedArgs $ option mempty litsToInlines
  return $ case (mnm, rg) of
    (Just nm, x) | null x ->
      op <> ok nm <> cl
    (_, x) ->
      op <> (ok . stringify) x <> cl
  where
    ok = B.codeWith (cls "Nm")


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

parseIn :: PandocMonad m => MdocParser m Inlines
parseIn = do
  macro "In"
  openClose <- closingDelimiters
  openOpen <- openingDelimiters
  header <- toString <$> lit
  close <- closingDelimiters
  return $ open openClose openOpen <> B.codeWith (cls "In") ("<" <> header <> ">") <> close
  where
    open a b
      | null a = b
      | null b = a
      | otherwise = a <> B.space <> b

parseLk :: PandocMonad m => MdocParser m Inlines
parseLk = do
  macro "Lk"
  openClose <- closingDelimiters
  openOpen <- openingDelimiters
  url <- toString <$> lit
  inner <- many segment >>= spacify
  close <- closingDelimiters
  let label | null inner = B.str url
            | otherwise = inner
  return $ open openClose openOpen <> B.link url "" label <> close
  where
    open a b
      | null a = b
      | null b = a
      | otherwise = a <> B.space <> b
    end = msatisfy newControlContext
    segment = do
      a <- openingDelimiters
      m <- option mempty litsToInlines
      z <-
        try (closingDelimiters <* notFollowedBy end)
          <|> option mempty pipes
      guard $ not $ all null [a, m, z]
      return $ a <> m <> z


parseNs :: PandocMonad m => MdocParser m Inlines
parseNs = macro "Ns" >> return noSpace

parseAp :: PandocMonad m => MdocParser m Inlines
parseAp = macro "Ap" >> return apMacro

-- parseAp :: PandocMonad m => MdocParser m Inlines
-- parseAp = do
--   macro "Ap"
--   return $ B.singleton apMacro


-- TODO should possibly rename this function b/c some of these are
-- Mdoc block partial-implicit macros. Unclear if this distinction
-- is going to be relevant.
parseInlineMacro :: PandocMonad m => MdocParser m Inlines
parseInlineMacro =
  choice
    [ parseSy,
      parseEm,
      parseLk,
      parseEv,
      parseMt,
      parsePa,
      parseFl,
      parseCm,
      parseIc,
      parseEr,
      parseCd,
      parseAd,
      parseMs,
      parseAr,
      parseIn,
      parseNo,
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
      parseSo,
      parseDo,
      parseQo,
      parsePo,
      parseBo,
      parseBro,
      parseAo,
      parseOo,
      parseAp,
      parseNs
    ]

parseInline :: PandocMonad m => MdocParser m Inlines
parseInline = parseStrs <|> (controlLine >>= spacify)
  where
    controlLine = many1 ((parseInlineMacro <|> litsAndDelimsToInlines) <* optional eol)

parseInlines :: PandocMonad m => MdocParser m Inlines
parseInlines = many1 (parseSmToggle <|> parseInline) >>= spacify

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

parseSmToggle :: PandocMonad m => MdocParser m Inlines
parseSmToggle = do
  macro "Sm"
  cur <- spacingMode <$> getState
  mode <- optionMaybe (literal "on" $> True <|> literal "off" $> False)
  eol
  let newMode = update mode cur
  modifyState $ \s -> s{spacingMode = newMode}
  return $ if newMode then smOn else smOff
  where
    update = \case
      Nothing -> not
      Just x -> const x

skipUnknownMacro :: PandocMonad m => MdocParser m Blocks
skipUnknownMacro = anyMacro *> manyTill anyToken eol $> mempty

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
                    , skipUnknownMacro
                    ]

