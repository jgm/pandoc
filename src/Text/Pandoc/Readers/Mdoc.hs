{-# LANGUAGE CPP  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{- |
   Module      : Text.Pandoc.Readers.Mdoc
   Copyright   : © 2024 Evan Silberman
   License     : GNU GPL, version 2 or above

   Maintainer  : Evan Silberman <evan@jklol.net>
   Stability   : WIP
   Portability : portable

Conversion of mdoc to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Mdoc (readMdoc) where

import Data.Char (isAsciiLower, toUpper)
import Data.Default (Default)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Control.Monad (mplus, guard, void, when, unless)
import Control.Monad.Except (throwError)
#if MIN_VERSION_base(4,19,0)
import Data.List (intersperse, unsnoc)
#else
import Data.List (intersperse)
#endif
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.Pandoc.Definition (Pandoc(Pandoc), Meta)
import Text.Pandoc.Builder (Blocks, Inlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad(..))
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (uncons)
import Text.Pandoc.Logging
import Text.Pandoc.Readers.Mdoc.Lex
import Text.Pandoc.Readers.Mdoc.Standards
import Text.Parsec (modifyState)
import qualified Text.Pandoc.Parsing as P
import qualified Data.Foldable as Foldable
import Text.Pandoc.Shared (stringify)

#if !MIN_VERSION_base(4,19,0)
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
#endif

  {- As a general principle, if mandoc -T lint issues a WARNING admonition
     or worse about a construct, I consider it fair game for this reader to
     do something different than what mandoc does with it, including bailing
     out instead of recovering. -}

data MdocSection
  = ShName
  | ShSynopsis
  | ShAuthors
  | ShSeeAlso
  | ShOther
  deriving (Show, Eq)

-- Declaration order is important: this is the order fields of a reference
-- are printed by mandoc
data ReferenceField =
  Author
  | ArticleTitle
  | BookTitle
  | Publisher
  | Journal
  | TechReportTitle
  | IssueNumber
  | VolumeNumber
  | Url
  | Pages
  | Institution
  | PubLocation
  | PubDate
  | Optional
  deriving (Show, Eq, Ord, Enum)

-- mandoc allows specifying multiple of _any_ reference field, and just
-- prints them all out in document order, even though authors are the only
-- field where this is the documented behavior. There's no lint warning
-- about this either. I'd prefer to do last-one-wins for the non-author
-- fields, which would presumably make it easier to transform the
-- bibliographic data into something else, but for now all I'm doing is
-- printing the references out the same way mandoc does.
type MdocReference = M.Map ReferenceField [T.Text]

data MdocState = MdocState
    { readerOptions :: ReaderOptions
    , metadata :: Meta
    , tableCellsPlain :: Bool
    , spacingMode :: Bool
    , authorNameSplit :: Bool
    , inLineEnclosure :: Bool
    , progName :: Maybe T.Text
    , currentSection :: MdocSection
    , currentReference :: MdocReference
    , logMessages :: [LogMessage]
    }
    deriving (Show)

instance Default MdocState where
    def =
        MdocState
            { readerOptions = def
            , metadata = B.nullMeta
            , tableCellsPlain = True
            , spacingMode = True
            , authorNameSplit = False
            , inLineEnclosure = False
            , currentSection = ShOther
            , currentReference = M.empty
            , progName = Nothing
            , logMessages = []
            }

instance HasLogMessages MdocState where
  addLogMessage msg st = st{ logMessages = msg : logMessages st }
  getLogMessages st = reverse $ logMessages st

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
     (Foldable.toList . unMdocTokens $ tokenz)
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
  reportLogMessages
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
newControlContext Blank{} = True
newControlContext Lit{} = False
newControlContext Delim{} = False


inlineContextEnd :: PandocMonad m => MdocParser m ()
inlineContextEnd = eof <|> (void . lookAhead $ msatisfy newControlContext)

sectionEnd :: PandocMonad m => MdocParser m ()
sectionEnd = eof <|> (void . lookAhead $ macro "Sh")

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
  arch <- optionMaybe (toString <$> lit)
  eol
  emptyMacro "Os"
  let adjust = B.setMeta "title" (B.str title)
             . B.setMeta "date" date
             . B.setMeta "section" (B.str section)
             . maybe id (B.setMeta "architecture" . B.str) arch
  modifyState $ \s -> s{metadata = adjust $ metadata s}

shToSectionMode :: T.Text -> MdocSection
shToSectionMode "NAME" = ShName
shToSectionMode "SYNOPSIS" = ShSynopsis
shToSectionMode "AUTHORS" = ShAuthors
shToSectionMode "SEE ALSO" = ShSeeAlso
shToSectionMode _ = ShOther

parseHeader :: PandocMonad m => MdocParser m Blocks
parseHeader = do
  (Macro m _) <- lookAhead $ macro "Sh" <|> macro "Ss"
  txt <- lineEnclosure m id
  let lvl = if m == "Sh" then 1 else 2
  when (lvl == 1) $ modifyState $ \s -> s{currentSection = (shToSectionMode . stringify) txt}
  return $ B.header lvl txt

parseNameSection :: PandocMonad m => MdocParser m Blocks
parseNameSection = do
  sec <- currentSection <$> getState
  guard $ sec == ShName
  nms <- mconcat . intersperse B.space <$> many nameNm
  macro "Nd"
  desc <- argsToInlines
  return $ B.para $ nms <> B.space <> "—" <> B.space <> desc
 where
   nameNm = do
     macro "Nm"
     nms <- many1 aNm
     eol
     return $ mconcat $ intersperse B.space nms
   comma = msatisfy $ \case
     (Delim _ "," _) -> True
     _ -> False
   aNm = do
     nm <- toString <$> lit
     c <- option mempty (toString <$> comma)
     modifyState $ \s -> s{progName = mplus (progName s) (Just nm)}
     return $ B.code nm <> B.str c

parseSynopsisSection :: PandocMonad m => MdocParser m Blocks
parseSynopsisSection = do
  sec <- currentSection <$> getState
  guard $ sec == ShSynopsis
  parseSynopsis sectionEnd

parseMiniSynopsis :: PandocMonad m => MdocParser m Blocks
parseMiniSynopsis = do
  macro "nr"
  literal "nS"
  literal "1"
  eol
  parseSynopsis (sectionEnd <|> end)
  where
    end = do
      macro "nr"
      literal "nS"
      literal "0"
      eol
      return ()

parseSynopsis :: PandocMonad m => MdocParser m () -> MdocParser m Blocks
parseSynopsis end = do
  bs <- manyTill synopsisBlock end
  return $ mconcat bs
  where
    synopsisGroup p = B.lineBlock <$> many1 p <* optional (emptyMacro "Pp")
    synopsisBlock = synopsisGroup parseInvocation
                <|> synopsisGroup (parseCd <* optional eol)
                <|> synopsisGroup (parseIn <* optional eol)
                <|> synopsisGroup (parseFd <* optional eol)
                <|> synopsisGroup (parseVt <* optional eol)
                <|> try parseSignature
                <|> parseWeirdSignature
                <|> parseRegularBlock
    parseInvocation = do
      nm <- parseNm
      optional eol
      rest <- many synopsisInline
      spacify (nm:rest)
    parseSignature = do
      ft <- parseFt <* optional eol
      sig <- (parseFn <|> parseFo) <* optional eol
      return $ B.lineBlock [ft, sig <> ";"]
    -- e.g. OpenBSD MB_CUR_MAX(3), mild abuse of notation for Ft
    parseWeirdSignature = do
      ft <- parseFt <* optional eol
      rest <- many synopsisInline
      line <- spacify (ft:rest)
      return $ B.lineBlock [line]
    synopsisInline = parseSmToggle <|> parseStrs <|> (controlLine >>= spacify) <?> "synopsis inlines"
    safeEol = do
      amNested <- inLineEnclosure <$> getState
      unless amNested $ optional eol
    controlLine = many1 ((choice otherInlineMacros <|> litsAndDelimsToInlines) <* safeEol)

parseSeeAlsoSection :: PandocMonad m => MdocParser m Blocks
parseSeeAlsoSection = do
  sec <- currentSection <$> getState
  guard $ sec == ShSeeAlso
  blocks <- many1Till parseSeeAlsoBlock sectionEnd
  return $ mconcat blocks
  where
    parseSeeAlsoBlock = parseRegularBlock <|> (B.para <$> parseRs)

-- roff(7) says "In text lines, whitespace is preserved within a line." I
-- considered following this rule but it really cuts against the grain of what
-- Pandoc writers want to work with, for no clear benefit. This isn't wholly
-- inconsistent with mandoc, because it makes no effort to render multiple
-- consecutive spaces from the source document in HTML. Hence I call B.text
-- instead of B.str
parseStr :: PandocMonad m => MdocParser m Inlines
parseStr = do
  (Str txt _) <- str
  return $ B.text txt

-- It's unclear whether consecutive text lines ought to be affected by the
-- spacing mode. mdoc(7) claims that:
--
-- > By default, spacing is on. When switched off, no white space is
-- > inserted between macro arguments and between the output generated from
-- > adjacent macros, but text lines still get normal spacing between words
-- > **and sentences.**
--
-- (emphasis added)
-- This implied to me that while spacing is off, consecutive text lines
-- would have spacing between them as normal. In fact, in mandoc's
-- implementation, they do not:
--
--     text
--     .Sm off
--     text.
--     text
--     .Sm on
--     text
--
-- renders as
--
--     text text.text text
--
-- (The "." is in there since the allusion in the documentation to
-- sentences made me wonder if that made a difference; it doesn't.)
--
-- I've chosen to adopt my interpretation of the documented behavior, rather
-- than mandoc's implementation. Multiple consecutive strs within a block get
-- spaces between them and then packed up together, and text lines are not
-- affected by the spacing mode.
--
-- Reported at https://inbox.vuxu.org/mandoc-discuss/369KFE6SHMXSE.3PS4387AYEFB5@silby.fyi/T/
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
    addSpace <- spacingMode <$> getState
    let omid | null omids = mempty
             | addSpace = omids <> B.space
             | otherwise = omids
    return $ openDelim <> omid

pipes :: PandocMonad m => MdocParser m Inlines
pipes = many (parseDelim Middle) >>= spacify

closingDelimiters :: PandocMonad m => MdocParser m Inlines
closingDelimiters = do
    cmids <- pipes
    addSpace <- spacingMode <$> getState
    let cmid | null cmids = mempty
             | addSpace = B.space <> cmids
             | otherwise = cmids
    closeDelim <- mconcat <$> many (parseDelim Close)
    return $ cmid <> closeDelim

delimitedArgs :: PandocMonad m => MdocParser m x -> MdocParser m (Inlines, x, Inlines)
delimitedArgs p = do
    openDelim <- openingDelimiters
    inlines <- p
    closeDelim <- closingDelimiters
    return (openDelim, inlines, closeDelim)

simpleInline :: PandocMonad m => T.Text -> (Inlines -> Inlines) -> MdocParser m Inlines
simpleInline nm xform = do
  macro nm
  segs <- manyTill segment inlineContextEnd
  spacify segs
 where
   segment = do
      (openDelim, inlines, closeDelim) <- delimitedArgs $ option mempty litsToInlines
      return $ openDelim <> xform inlines <> closeDelim

codeLikeInline' :: PandocMonad m => T.Text -> T.Text -> MdocParser m Inlines
codeLikeInline' nm cl = simpleInline nm (eliminateEmpty (B.codeWith (cls cl) . stringify))

codeLikeInline :: PandocMonad m => T.Text -> MdocParser m Inlines
codeLikeInline nm = codeLikeInline' nm nm

spanLikeInline :: PandocMonad m => T.Text -> MdocParser m Inlines
spanLikeInline nm = simpleInline nm (eliminateEmpty (B.spanWith (cls nm)))

-- One-line enclosures need a little bit of state so that we don't parse
-- the closing delimiters that follow nested one-line or multiline
-- enclosures; the closing delimiters are meant to go after the close of
-- the outermost enclosure. Hence we respect and set inLineEnclosure.
lineEnclosure :: PandocMonad m => T.Text -> (Inlines -> Inlines) -> MdocParser m Inlines
lineEnclosure nm xform = do
  macro nm
  amNested <- inLineEnclosure <$> getState
  modifyState $ \s -> s{inLineEnclosure = True}
  first <- openingDelimiters
  further <-
    (manyTill
      (parseInlineMacro
        <|> (try (litsAndDelimsToInlines <* notFollowedBy eol))
        <|> litsToInlines
        <|> openingDelimiters)
      lineEnclosureContextEnd)
  further' <- spacify further
  finally <- if amNested then mempty else closingDelimiters <* optional eol
  modifyState $ \s -> s{inLineEnclosure = amNested}
  return $ first <> xform further' <> finally
  where
    lineEnclosureContextEnd =
      try $
        void (lookAhead (macro "Ta"))
        <|> lookAhead (many (macro "Ns" <|> delim Close) *> eol)


-- The Ns, Ap, and Sm macros affect the automatic insertion of spaces between
-- macro arguments that occurs by default. We parse these macros to RawInlines
-- that we then eliminate in foldNoSpaces. If any of these macros end up
-- in the final AST returned by readMdoc, it's a bug.

noSpace :: Inlines
noSpace = B.rawInline "mdoc" "Ns"

apMacro :: Inlines
apMacro = B.rawInline "mdoc" "Ap"

smOff :: Inlines
smOff = B.rawInline "mdoc" "Sm off"

smOn :: Inlines
smOn = B.rawInline "mdoc" "Sm on"

-- Accumulator for eliminating of Ns, Ap, and Sm macros from a list of 'Inlines'
data SpacifyState = SpacifyState
  { accum :: [Inlines],  -- already-folded 'Inlines'
    prev :: Inlines,  -- content we might be appending further content to
    ns :: Bool,  -- True when we've read an Ns and are waiting to concatenate content to prev
    sm :: Bool  -- True when spacing mode is on
  }

instance Default SpacifyState where
  def = SpacifyState [] mempty False True

-- Given a list of 'Inlines'es, concatenate consecutive elements that shouldn't
-- have a 'Space' inserted between them based on changes to the spacing mode,
-- Ap macros, and Ns macros.
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

-- Add any necessary spaces between individual 'Inlines' in a list.
-- Respects the spacing mode status. This should more or less
-- always get applied to any list of 'Inlines' before doing anything
-- else with it.
spacify :: PandocMonad m => [Inlines] -> MdocParser m Inlines
spacify x = do
  mode <- spacingMode <$> getState
  return (go mode x)
  where
    go True = mconcat . intersperse B.space . foldNoSpaces
    go False = mconcat . foldNoSpaces

-- Compatibility note: mandoc permits, and doesn't warn on, "vertical" macros
-- (Pp, Bl/El, Bd/Ed) inside of "horizontal" block partial-explicit quotations
-- like Do/Dc. However there are no OpenBSD manual pages that employ such markup
-- and it doesn't look right when rendered. We don't attempt to consume anything
-- but pandoc inlines inside of these multiline enclosures.
multilineEnclosure :: PandocMonad m => T.Text -> T.Text -> (Inlines -> Inlines) -> MdocParser m Inlines
multilineEnclosure op cl xform = do
  macro op
  amNested <- inLineEnclosure <$> getState
  -- we're now "protected" from any outer enclosure or .It
  modifyState $ \s -> s{inLineEnclosure = False}
  openDelim <- mconcat <$> many (parseDelim Open)
  optional eol
  contents <- parseInlines
  (macro cl <?> show cl)
  closeDelim <-
    if amNested
       then mempty
       else mconcat <$> many (parseDelim Close) <* optional eol
  modifyState $ \s -> s{inLineEnclosure = amNested}
  return $ openDelim <> xform contents <> closeDelim

parseEo :: PandocMonad m => MdocParser m Inlines
parseEo = do
  macro "Eo"
  odel <- del
  optional eol
  inner <- parseInlines
  macro "Ec"
  cdel <- del
  optional eol
  return $ odel <> inner <> cdel
  where
    del = B.str . toString <$> (arg <|> delim Open <|> delim Middle <|> delim Close)

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

-- Deprecated, mandoc doesn't style this at all
parseTn :: PandocMonad m => MdocParser m Inlines
parseTn = simpleInline "Tn" (eliminateEmpty id)

parseLi :: PandocMonad m => MdocParser m Inlines
parseLi = codeLikeInline "Li"

parseEv :: PandocMonad m => MdocParser m Inlines
parseEv = codeLikeInline "Ev"

parseDv :: PandocMonad m => MdocParser m Inlines
parseDv = codeLikeInline "Dv"

parseAd :: PandocMonad m => MdocParser m Inlines
parseAd = spanLikeInline "Ad"

parseVa :: PandocMonad m => MdocParser m Inlines
parseVa = codeLikeInline' "Va" "variable"

parseVt :: PandocMonad m => MdocParser m Inlines
parseVt = codeLikeInline' "Vt" "variable"

parseAn :: PandocMonad m => MdocParser m Inlines
parseAn = try anSplit <|> anRegular
  where
    anSplit = do
      macro "An"
      mode <- literal "-split" $> True <|> literal "-nosplit" $> False
      modifyState $ \s -> s{authorNameSplit = mode}
      return mempty
    anRegular = do
      an <- spanLikeInline "An"
      spl <- authorNameSplit <$> getState
      return $ (if spl then B.linebreak else mempty) <> an

parseMs :: PandocMonad m => MdocParser m Inlines
parseMs = spanLikeInline "Ms"

-- TODO implement internal reference links
parseSx :: PandocMonad m => MdocParser m Inlines
parseSx = spanLikeInline "Sx"

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

-- There's a number of unique-looking cases for Fl parsing so I am just
-- handling them very explicitly instead of trying to generalize anything
-- enough to handle it. Could conceivably be better.
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
     rest <- parseInline
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

parseDl :: PandocMonad m => MdocParser m Blocks
parseDl = do
  inner <- lineEnclosure "Dl" id
  return $ B.codeBlock (stringify inner)

parseD1 :: PandocMonad m => MdocParser m Blocks
parseD1 = do
  inner <- lineEnclosure "D1" id
  return $ B.divWith (cls "display") $ B.plain inner

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

parseFd :: PandocMonad m => MdocParser m Inlines
parseFd = codeLikeInline "Fd"

parseFt :: PandocMonad m => MdocParser m Inlines
parseFt = codeLikeInline' "Ft" "variable"

-- The output here is comparable to mandoc's HTML output, which doesn't tag
-- the commas/parentheses. Is this questionable from a pandoc POV?
formatFunction :: T.Text -> [Inlines] -> Inlines
formatFunction nm args = B.codeWith (cls "Fn") nm <> "(" <> args' <> ")"
  where
    args' = mconcat $ intersperse (", ") args

parseFn :: PandocMonad m => MdocParser m Inlines
parseFn = do
  macro "Fn"
  (op, (nm, args), cl) <- delimitedArgs f
  return $ op <> formatFunction nm (fmap (B.codeWith (cls "variable")) args) <> cl
  where
    f = do
      nm <- toString <$> lit
      args <- option [] litsToText
      return (nm, args)

parseFa :: PandocMonad m => MdocParser m Inlines
parseFa = codeLikeInline' "Fa" "variable"

parseFo :: PandocMonad m => MdocParser m Inlines
parseFo = do
  macro "Fo"
  nm <- toString <$> lit
  eol
  args <- many (parseFa <* eol)
  macro "Fc"
  return $ formatFunction nm args

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

-- This is a raw roff request but it appears sometimes in mdoc
-- manuals and is easy enough to handle
parsebr :: PandocMonad m => MdocParser m Inlines
parsebr = emptyMacro "br" >> return B.linebreak

parseNs :: PandocMonad m => MdocParser m Inlines
parseNs = macro "Ns" >> return noSpace

-- Per mdoc(7), Pf prefix macro [argument ...] is equivalent to
-- No \&prefix Ns macro [argument ...] and because of the way
-- spacify works, the easiest thing to do is just push an Ns onto
-- the input
parsePf :: PandocMonad m => MdocParser m Inlines
parsePf = do
  macro "Pf"
  t <- toString <$> anyToken
  rest <- getInput
  pos <- getPosition
  setInput $ (Macro "Ns" pos):rest
  return $ B.str t

parseAp :: PandocMonad m => MdocParser m Inlines
parseAp = macro "Ap" >> return apMacro

parseEx :: PandocMonad m => MdocParser m Inlines
parseEx = do
  macro "Ex"
  literal "-std"
  args <- fmap toString <$> many lit
  pn <- progName <$> getState
  eol
  return $ "The"
          <> B.space
          <> utils pn args
          <> B.space
          <> "0 on success, and >0 if an error occurs."
  where
    nm = B.codeWith (cls "Nm")
    sing = "utility exits"
    plur = "utilities exit"
    utils (Just x) [] = nm x <> B.space <> sing
    utils _ [x] = nm x <> B.space <> sing
    utils _ [x,y] = nm x <> B.space <> "and" <> B.space <> nm y <> B.space <> plur
    utils pn xs =
      case (pn, unsnoc xs) of
        (Nothing, Nothing) -> sing
        (_, Just (hd, end)) -> mconcat ((intersperse (", ") . fmap nm) hd) <> ", and " <> nm end <> B.space <> plur
        (Just p, Nothing) -> nm p <> B.space <> sing


parseRv :: (PandocMonad m) => MdocParser m Inlines
parseRv = do
  macro "Rv"
  literal "-std"
  args <- fmap toString <$> many lit
  pn <- progName <$> getState
  eol
  return $ go pn args
  where
    nm a = B.codeWith (cls "Fn") a <> "()"
    nothing = "Upon successful completion, the value 0 is returned;"
    sing = "function returns"
    plur = "functions return"
    success = "the value 0 if successful;"
    errno =
      "otherwise the value -1 is returned and the global variable"
        <> B.codeWith (cls "variable") "errno"
        <> "is set to indicate the error."
    message conj =
      "The"
        <> B.space
        <> conj
        <> B.space
        <> success
        <> B.space
        <> errno
    go (Just x) [] = message (nm x <> B.space <> sing)
    go _ [x] = message (nm x <> B.space <> sing)
    go _ [x, y] = message (nm x <> B.space <> "and" <> B.space <> nm y <> B.space <> plur)
    go pn xs =
      case (pn, unsnoc xs) of
        (Nothing, Nothing) -> nothing <> B.space <> errno
        (_, Just (hd, end)) -> message (mconcat ((intersperse (", ") . fmap nm) hd) <> ", and " <> nm end <> B.space <> plur)
        (Just p, Nothing) -> message (nm p <> B.space <> sing)

parseSt :: PandocMonad m => MdocParser m Inlines
parseSt = do
  macro "St"
  (Lit std pos) <- lit
  case standard std of
    Nothing -> do
      logMessage $ SkippedContent ("unrecognized argument to St: " <> std) pos
      return mempty
    Just t -> return $ B.text t

-- TODO incorporate well-known library description and linker options
-- from mandoc lib.in expected in FreeBSD LIBRARY section, at minimum.
parseLb :: PandocMonad m => MdocParser m Inlines
parseLb = do
  macro "Lb"
  library <- toString <$> lit
  return $ "library" <> B.space <> B.doubleQuoted (B.str library)

unixVersion :: PandocMonad m => T.Text -> T.Text -> MdocParser m Inlines
unixVersion m s = do
  macro m
  (o, v, c) <- delimitedArgs (option mempty (toString <$> lit))
  return $ o <> B.str s <> f v <> c
  where
    f v | T.null v = mempty
        | otherwise = B.space <> B.str v

parseAt :: PandocMonad m => MdocParser m Inlines
parseAt = do
  macro "At"
  (o, v, c) <-  delimitedArgs (optionMaybe (toString <$> lit))
  let v' = maybe "AT&T UNIX" attVer v
  return $ o <> B.text v' <> c
  where
    isVersion x = x `elem` ["1", "2", "3", "4", "5", "6", "7"]
    isRelease x = x `elem` ["1", "2", "3", "4"]
    attVer (T.stripPrefix "v" -> Just ver)
      | isVersion ver = "Version " <> ver <> " AT&T UNIX"
    attVer "32v" = "Version 7 AT&T UNIX/32V"
    attVer "III" = "AT&T System III UNIX"
    attVer (T.stripPrefix "V." -> Just release)
      | isRelease release = "AT&T System V Release " <> release <> " UNIX"
    attVer "V" = "AT&T System V UNIX"
    attVer x = "AT&T UNIX " <> x

parseBsx :: PandocMonad m => MdocParser m Inlines
parseBsx = unixVersion "Bsx" "BSD/OS"

parseBx :: PandocMonad m => MdocParser m Inlines
parseBx = do
  macro "Bx"
  (o, v, c) <- delimitedArgs zeroToTwoLits
  return $ o <> bsd v <> c
  where
    zeroToTwoLits = do
      toks <- try (count 2 lit) <|> count 1 lit <|> count 0 lit
      return $ toString <$> toks
    bsd [] = B.str "BSD"
    bsd [x] = B.str $ x <> "BSD"
    bsd (x:y:_) = B.str (x <> "BSD" <> "-" <> T.toTitle y)

parseDx :: PandocMonad m => MdocParser m Inlines
parseDx = unixVersion "Dx" "DragonFly"

parseFx :: PandocMonad m => MdocParser m Inlines
parseFx = unixVersion "Fx" "FreeBSD"

-- This dance to capitalize a letter at the end of a NetBSD
-- version matches what mandoc does to the argument of .Nx.
-- See mandoc mdoc_validate.c r1.350
-- Curiously, there's little easy-to-find evidence of what
-- these lettered releases actually are, other than
-- references in man page history sections to 0.9A etc.
parseNx :: PandocMonad m => MdocParser m Inlines
parseNx = do
  macro "Nx"
  (o, v, c) <-  delimitedArgs (option mempty (toString <$> lit))
  return $ o <> "NetBSD" <> f v <> c
  where
    f v | T.null v = mempty
        | otherwise = B.space <> B.str (fromRight v $ readWith earlyNetBSDVersion () v)
    earlyNetBSDVersion = do
      major <- oneOf "01"
      dot <- char '.'
      minor <- digit
      ltr <- satisfy isAsciiLower
      return $ T.pack [major, dot, minor, toUpper ltr]

parseOx :: PandocMonad m => MdocParser m Inlines
parseOx = unixVersion "Ox" "OpenBSD"

parseUx :: PandocMonad m => MdocParser m Inlines
parseUx = macro "Ux" >> return (B.str "UNIX")

parseInlineMacro :: PandocMonad m => MdocParser m Inlines
parseInlineMacro = choice (synopsisTopicMacros <> otherInlineMacros) <?> "inline macro"

-- These macros always start a new line in SYNOPSIS
synopsisTopicMacros :: PandocMonad m => [MdocParser m Inlines]
synopsisTopicMacros =
    [parseNm, parseCd, parseFd, parseFn, parseFo, parseIn, parseVt, parseFt]

otherInlineMacros :: PandocMonad m => [MdocParser m Inlines]
otherInlineMacros =
    [ parseSy,
      parseEm,
      parseLk,
      parseLi,
      parseEv,
      parseDv,
      parseMt,
      parsePa,
      parseFl,
      parseCm,
      parseIc,
      parseEr,
      parseAd,
      parseVa,
      parseAn,
      parseMs,
      parseSx,
      parseAr,
      parseFa,
      parseNo,
      parseTn,
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
      parseEo,
      parseSo,
      parseDo,
      parseQo,
      parsePo,
      parseBo,
      parseBro,
      parseAo,
      parseOo,
      parseBf,
      parseRsInline,
      parseEx,
      parseRv,
      parseSt,
      parseLb,
      parseAt,
      parseBsx,
      parseBx,
      parseDx,
      parseFx,
      parseNx,
      parseOx,
      parseUx,
      parsebr,
      parseAp,
      parsePf,
      parseNs,
      skipUnsupportedInlines
    ]

parseInline :: PandocMonad m => MdocParser m Inlines
parseInline = parseStrs <|> (controlLine >>= spacify) <?> "text lines or inline macros"
  where
    safeEol = do
      amNested <- inLineEnclosure <$> getState
      unless amNested $ optional eol
    controlLine = many1 ((parseInlineMacro <|> litsAndDelimsToInlines) <* safeEol)

parseInlines :: PandocMonad m => MdocParser m Inlines
parseInlines = many1 (parseSmToggle <|> parseInline) >>= spacify

-- Lp is a deprecated synonym for Pp
parsePara :: PandocMonad m => MdocParser m Blocks
parsePara = B.para . B.trimInlines <$> parseInlines <*
    optional (emptyMacro "Pp" <|> emptyMacro "Lp")

-- Indented display blocks are visually similar to block quotes
-- but rarely carry those semantics. I'm just putting things in
-- divs. Centered is discouraged and rarely seen.
parseDisplay :: PandocMonad m => MdocParser m Blocks
parseDisplay = do
  literal "-filled" <|> literal "-ragged" <|> literal "-centered"
  many $ (literal "-offset" *> lit) <|> (literal "-compact")
  eol
  B.divWith (cls "display") . mconcat <$> many parseRegularBlock

-- This is something of a best-effort interpretation of the -unfilled
-- display block type. The main difference with mandoc is probably
-- that newlines inside of multiline enclosures won't be preserved.
parseUnfilled :: PandocMonad m => MdocParser m Blocks
parseUnfilled = do
  literal "-unfilled"
  many $ (literal "-offset" *> lit) <|> (literal "-compact")
  eol
  lns <- many $ Just <$> parseStrPreserveSpace
            <|> Nothing <$ parseSmToggle
            <|> Just <$> parseInline
            <|> Just "" <$ emptyMacro "Pp"
  return $ B.lineBlock (catMaybes lns)
  where
    parseStrPreserveSpace = (B.str . toString) <$> str <|> (blank *> mempty)

parseCodeBlock :: PandocMonad m => MdocParser m Blocks
parseCodeBlock = do
  literal "-literal"
  many $ (literal "-offset" *> lit) <|> (literal "-compact")
  eol
  lns <- many $ Just . toString <$> (str <|> blank)
            <|> Nothing <$ parseSmToggle
            <|> Just . stringify <$> parseInline
            <|> Just "" <$ emptyMacro "Pp"
  return $ B.codeBlock (T.unlines (catMaybes lns))

parseBd :: PandocMonad m => MdocParser m Blocks
parseBd = do
  macro "Bd"
  blk <- parseCodeBlock <|> parseDisplay <|> parseUnfilled
  emptyMacro "Ed"
  return blk

-- This is a bit of a best effort version. Hypothetically multiple blocks
-- could occur inside a Bf and this should be a stateful thing but I don't
-- know if that's observed in the wild.
parseBf :: PandocMonad m => MdocParser m Inlines
parseBf = do
  macro "Bf"
  xform <-   B.strong <$ (literal "Sy" <|> literal "-symbolic")
         <|> B.emph   <$ (literal "Em" <|> literal "-emphasis")
         <|> code     <$ (literal "Li" <|> literal "-literal")
  eol
  ins <- parseInlines
  emptyMacro "Ef"
  return $ xform ins
  where
    code = B.code . stringify

skipListArgument :: (PandocMonad m) => MdocParser m ()
skipListArgument =
  void $ choice
    [ literal "-width" *> lit,
      literal "-offset" *> lit,
      literal "-compact"
    ]

parseItemList :: PandocMonad m => MdocParser m Blocks
parseItemList = do
  f <- (choice (map literal ["-bullet", "-dash", "-hyphen", "-item"]) $> B.bulletList)
       <|> literal "-enum" $> B.orderedList
  many skipListArgument
  eol
  items <- many bulletItem
  return $ f items
  where
    bulletItem = do
      emptyMacro "It"
      mconcat <$> many parseRegularBlock

-- Despite some ambiguous documentation to the contrary the Xo/Xc macros
-- only seem genuinely useful in an .It head, and it's not clear what if
-- anything it means to use them somewhere else in a contemporary mdoc manual.
-- See https://inbox.vuxu.org/mandoc-discuss/2UKLZW0DL8BSM.2IIO9W4HSUSRR@silby.fyi/T/
-- for more blathering.
parseDefinitionList :: PandocMonad m => MdocParser m Blocks
parseDefinitionList = do
  headParser <- (choice . map literal) ["-hang", "-inset", "-ohang", "-tag"] $> parsedHead <|> literal "-diag" $> diagHead
  many skipListArgument
  eol
  items <- many (parseSmToggle *> mempty <|> dlItem headParser)
  return $ B.definitionList items
  where
    parsedHead = try xoListHead <|> eolListHead
    eolListHead = do
      modifyState $ \s -> s{inLineEnclosure = True}
      inner <- parseInlines
      eol
      modifyState $ \s -> s{inLineEnclosure = False}
      return inner
    diagHead = argsToInlines
    dlItem hed = do
      -- Some manuals have an evidently useless .Pp before .It
      -- e.g. OpenBSD ld(1), just deal with it.
      many ((void . emptyMacro) "Pp" <|> skipUnsupportedMacro "Tg")
      macro "It"
      dt <- hed
      dd <- mconcat <$> many parseRegularBlock
      return (dt, [dd])
    xoListHead = do
      before <- option mempty parseInline
      macro "Xo"
      optional eol
      after <- many1Till parseInlines (emptyMacro "Xc")
      spacify (before:after)

-- TODO support implicit rows:
--   If the first line of the body of a -column list is not an It macro line,
--   It contexts spanning one input line each are implied until an It macro
--   line is encountered
-- and support literal tabs
parseColumnList :: PandocMonad m => MdocParser m Blocks
parseColumnList = do
  literal "-column"
  many skipListArgument
  many $ arg <|> delim Open <|> delim Middle <|> delim Close
  eol
  rows <- many listRow
  return $ B.simpleTable [] rows
  where
    listRow = do
      optional (emptyMacro "Pp")
      macro "It"
      fmap B.plain <$> sepBy (parseInlines <|> pure mempty) (macro "Ta" <* optional eol)

parseBl :: PandocMonad m => MdocParser m Blocks
parseBl = do
  macro "Bl"
  blk <- parseItemList <|> parseDefinitionList <|> parseColumnList
  emptyMacro "El"
  return blk

referenceField :: PandocMonad m => T.Text -> ReferenceField -> MdocParser m ()
referenceField m field = do
  macro m
  reference <- currentReference <$> getState
  contents <- stringify <$> litsAndDelimsToInlines
  eol
  modifyState $ \s -> s{currentReference = M.insertWith (++) field [contents] reference}
  return ()

parsePercentA :: PandocMonad m => MdocParser m ()
parsePercentA = referenceField "%A" Author

parsePercentB :: PandocMonad m => MdocParser m ()
parsePercentB = referenceField "%B" BookTitle

parsePercentC :: PandocMonad m => MdocParser m ()
parsePercentC = referenceField "%C" PubLocation

parsePercentD :: PandocMonad m => MdocParser m ()
parsePercentD = referenceField "%D" PubDate

parsePercentI :: PandocMonad m => MdocParser m ()
parsePercentI = referenceField "%I" Publisher

parsePercentJ :: PandocMonad m => MdocParser m ()
parsePercentJ = referenceField "%J" Journal

parsePercentN :: PandocMonad m => MdocParser m ()
parsePercentN = referenceField "%N" IssueNumber

parsePercentO :: PandocMonad m => MdocParser m ()
parsePercentO = referenceField "%O" Optional

parsePercentP :: PandocMonad m => MdocParser m ()
parsePercentP = referenceField "%P" Pages

parsePercentQ :: PandocMonad m => MdocParser m ()
parsePercentQ = referenceField "%Q" Institution

parsePercentR :: PandocMonad m => MdocParser m ()
parsePercentR = referenceField "%R" TechReportTitle

parsePercentT :: PandocMonad m => MdocParser m ()
parsePercentT = referenceField "%T" ArticleTitle

parsePercentU :: PandocMonad m => MdocParser m ()
parsePercentU = referenceField "%U" Url

parsePercentV :: PandocMonad m => MdocParser m ()
parsePercentV = referenceField "%V" VolumeNumber

parseReferenceField :: PandocMonad m => MdocParser m ()
parseReferenceField =
  choice [
      parsePercentA,
      parsePercentB,
      parsePercentC,
      parsePercentD,
      parsePercentI,
      parsePercentJ,
      parsePercentN,
      parsePercentO,
      parsePercentP,
      parsePercentQ,
      parsePercentR,
      parsePercentT,
      parsePercentU,
      parsePercentV
    ]

parseRsInline :: PandocMonad m => MdocParser m Inlines
parseRsInline = do
  sec <- currentSection <$> getState
  guard $ sec /= ShSeeAlso
  parseRs

parseRs :: PandocMonad m => MdocParser m Inlines
parseRs = do
  emptyMacro "Rs"
  modifyState $ \s -> s{currentReference = M.empty}
  many1 parseReferenceField
  emptyMacro "Re"
  ref <- currentReference <$> getState
  -- TODO formatting fields correctly
  return $ B.text $ (M.foldl f mempty ref) <> "."
  where join v = T.concat (intersperse ", " v)
        f a v | T.null a = join v
              | otherwise = a <> ", " <> join v

-- mandoc's roff(7) says "Blank text lines, which may include whitespace,
-- are only permitted within literal contexts." mandoc -T lint warns about
-- blank lines and inserts a roff `sp` request, which is handled
-- differently depending on the output format. My read is that mandoc
-- considers the handling of a blank line in non-literal context in mdoc(7)
-- to be undefined. The Mdoc reader thus ignores blank input lines outside
-- of -literal and -unfilled displays.
skipBlanks :: PandocMonad m => MdocParser m Blocks
skipBlanks = many1 blank *> mempty

-- By default, mdoc is in "spacing mode", where horizontal space is added
-- between macro contents. The Sm macro turns it off and on. When we encounter
-- the Sm macro, we both modify the parser state and we emit a sentinel value
-- that spacify/foldNoSpaces uses to handle cases where spacing mode gets
-- turned off and on within a stretch of inlines.
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

skipUnsupportedMacro :: PandocMonad m => T.Text -> MdocParser m ()
skipUnsupportedMacro nm = do
  (Macro _ pos) <- macro nm
  manyTill anyToken eol
  logMessage $ SkippedContent ("unsupported macro: " <> nm) pos


skipUnsupportedInlines :: PandocMonad m => MdocParser m Inlines
skipUnsupportedInlines = choice
      [ skipUnsupportedMacro "Tg",
        skipUnsupportedMacro "Bk",
        skipUnsupportedMacro "Ek"
      ] *> mempty

skipUnknownMacro :: PandocMonad m => MdocParser m Blocks
skipUnknownMacro = do
  pos <- getPosition
  m <- anyMacro
  manyTill anyToken eol
  logMessage $ SkippedContent ("unsupported macro: " <> toString m) pos
  return mempty


parseRegularBlock :: PandocMonad m => MdocParser m Blocks
parseRegularBlock =
  choice
    [ parseDl
    , parseD1
    , parsePara
    , emptyMacro "Pp" *> mempty
    , parseBd
    , parseBl
    , skipBlanks
    ]

parseBlock :: (PandocMonad m) => MdocParser m Blocks
parseBlock =
  choice
    [ parseHeader
    , parseNameSection
    , parseSynopsisSection
    , parseSeeAlsoSection
    , parseMiniSynopsis
    , parseRegularBlock
    , skipUnknownMacro
    ]
