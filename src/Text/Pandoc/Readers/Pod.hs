{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{- |
   Module      : Text.Pandoc.Readers.Pod
   Copyright   : © 2024 Evan Silberman
   License     : GNU GPL, version 2 or above

   Maintainer  : Evan Silberman <evan@jklol.net>
   Stability   : WIP
   Portability : portable

Conversion of Pod to 'Pandoc' documents
-}
module Text.Pandoc.Readers.Pod (readPod) where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Data.Char (isAsciiUpper, digitToInt)
import Data.Default (Default)
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Parsing.General (isSpaceChar)
import Text.Pandoc.XML (lookupEntity)
import Text.Pandoc.Class.PandocMonad (PandocMonad(..))
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Error
import Text.Pandoc.Builder (Blocks, Inlines)
import qualified Text.Pandoc.Builder as B
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Pandoc.Shared (stringify, textToIdentifier, tshow)
import Data.Set (Set)
import Data.Functor (($>))
import Data.Maybe (listToMaybe, fromMaybe)
import Numeric (readOct)

data PodState = PodState
  { logMessages :: [LogMessage]
  , headerIds :: Set T.Text
  , options :: ReaderOptions
} deriving (Show)

instance HasLogMessages PodState where
  addLogMessage msg st = st{ logMessages = msg : logMessages st }
  getLogMessages st = reverse $ logMessages st

instance HasIdentifierList PodState where
  extractIdentifierList = headerIds
  updateIdentifierList f st = st{headerIds = f (headerIds st)}

instance HasReaderOptions PodState where
  extractReaderOptions = options

instance Default PodState where
  def = PodState
    { logMessages = []
    , headerIds = mempty
    , options = def
    }

data PodLinkDestination = LinkUrl Inlines T.Text
                        | LinkMan Inlines (Maybe Inlines)
                        | LinkInternal Inlines
                        deriving (Show)

defaultLinkName :: PodLinkDestination -> Inlines
defaultLinkName (LinkUrl inl _) = inl
defaultLinkName (LinkMan nm (Just sec)) = B.doubleQuoted sec <> " in " <> nm
defaultLinkName (LinkMan nm Nothing) = nm
defaultLinkName (LinkInternal sec) = B.doubleQuoted sec

type PodParser m = ParsecT Sources PodState m

readPod :: (PandocMonad m, ToSources a)
        => ReaderOptions
        -> a
        -> m Pandoc
readPod _ s = do
  let sources = toSources s
  p <- readWithM parsePod def sources
  case p of
    Right result -> return result
    Left e       -> throwError e

parsePod :: PandocMonad m => PodParser m Pandoc
parsePod = do
  -- We don't actually start processing Pod until we encounter a Pod command.
  -- If we never encounter a Pod command, the document is still valid Pod, it
  -- just contains no content.
  notPod
  bs <- manyTill block eof
  reportLogMessages
  return $ B.doc $ mconcat bs

block :: PandocMonad m => PodParser m Blocks
block = verbatim  <|> paragraph <|> command <?> "Pod paragraph"

command :: PandocMonad m => PodParser m Blocks
command = do
    try (char '=' >> notFollowedBy (string "item" <|> string "back" <|> string "end"))
    header <|> pod <|> cut <|> over <|> for <|> begin <|> encoding <?> "Pod command"

cmd :: PandocMonad m => T.Text -> PodParser m ()
cmd nm = do
  textStr nm
  notFollowedBy nonspaceChar
  void $ many spaceChar

encoding :: PandocMonad m => PodParser m Blocks
encoding = do
  cmd "encoding"
  anyLine
  logMessage $ IgnoredElement "=encoding; Pandoc requires UTF-8 input"
  return mempty

header :: PandocMonad m => PodParser m Blocks
header = do
  string "head"
  dig <- oneOf "123456"
  void blankline <|> skipMany1 spaceChar
  ins <- inlines
  attrs <- registerHeader B.nullAttr ins
  optional blanklines
  return $ B.headerWith attrs (digitToInt dig) ins

pod :: PandocMonad m => PodParser m Blocks
pod = do
  cmd "pod"
  optional (try inlines)
  optional blanklines
  return mempty

cut :: PandocMonad m => PodParser m Blocks
cut = cmd "cut" *> notPod

notPod :: PandocMonad m => PodParser m Blocks
notPod = do
  manyTill anyLine (eof <|> void (try (lookAhead (char '=' *> letter))))
  return mempty

over :: PandocMonad m => PodParser m Blocks
over = do
  cmd "over"
  anyLine
  blanklines
  optional $ try (char '=' *> cut)
  bs <- list <|> blockquote
  string "=back" <* blanklines
  return bs

list :: PandocMonad m => PodParser m Blocks
list = try bulletList <|> try orderedList <|> definitionList

bulletList :: PandocMonad m => PodParser m Blocks
bulletList = B.bulletList <$> many1 (item (many spaceChar *> optional (char '*')))

orderedList :: PandocMonad m => PodParser m Blocks
orderedList = do
  start <- item1
  more <- many orderedItem
  return $ B.orderedList (start : more)
  where
    item1 = item $ spaces *> char '1' *> optional (char '.')
    orderedItem = item $ spaces *> many digit *> optional (char '.')

item :: PandocMonad m => PodParser m () -> PodParser m Blocks
item p = do
  try (cmd "=item")
  p
  blanklines
  mconcat <$> many block <?> "runaway item"

definitionList :: PandocMonad m => PodParser m Blocks
definitionList = B.definitionList <$> many1 dlItem
  where
    dlItem = do
      try (cmd "=item")
      spaces
      term <- inlines
      blanklines
      -- perlpodspec sez the /section part of a link can refer to either
      -- a header or a dl item, hence treating it as a "header" here
      attrs <- registerHeader B.nullAttr term
      defn <- mconcat <$> many block <?> "runaway dlitem"
      return (B.spanWith attrs term, [defn])

blockquote :: PandocMonad m => PodParser m Blocks
blockquote = B.blockQuote . mconcat <$> many block <?> "runaway blockquote"

paragraph :: PandocMonad m => PodParser m Blocks
paragraph = do
  try (notFollowedBy (char '=' *> letter))
  inl <- inlines
  optional blanklines
  return $ B.para $ B.trimInlines inl

inlines :: PandocMonad m => PodParser m Inlines
inlines = mconcat <$> many1 (format <|> whitespace <|> str)

-- perlpodspec sez:
--   If a Pod processor sees any formatting code other than the ones listed,
--   that processor must by default treat this as an error.
format :: PandocMonad m => PodParser m Inlines
format = try $ do
  ctrl <- satisfy isAsciiUpper
  p <- getPosition
  lookAhead (char '<')
  case ctrl of
    'B' -> B.strong <$> argument
    'C' -> B.code . stringify <$> argument
    'F' -> B.spanWith (mempty, ["filename"], mempty) <$> argument
    'I' -> B.emph <$> argument
    'S' -> argument  -- TODO map nbsps
    'X' -> argument $> mempty
    'Z' -> argument $> mempty

    'E' -> do
      a <- stringify <$> argument
      case entity a of
             -- per spec:
             --   Pod parsers, when faced with some unknown "E<identifier>" code,
             --   shouldn't simply replace it with nullstring (by default, at
             --   least), but may pass it through as a string consisting of the
             --   literal characters E, less-than, identifier, greater-than.
             Nothing -> do
               logMessage $ SkippedContent ("unknown entity " <> a) p
               return $ B.str $ "E<" <> a <> ">"
             Just e -> return $ B.str e

    'L' -> link

    x -> throwError $ PandocParseError $ T.snoc "unknown Pod formatting code " x
  where
    argument = try expandedArg <|> compactArg <?> "argument"
    innerStr =  B.str <$> many1Char (podCharLess ">")
    compactArg = do
      char '<'
      mconcat <$> manyTill (format <|> whitespace <|> innerStr) (char '>')
    expandedArg = do
      openLen <- length <$> many1 (char '<')
      let close = T.pack $ replicate openLen '>'
      skipMany1 spaceChar <|> void blankline
      arg <- mconcat <$> many (format <|> try (whitespace <* notFollowedBy (textStr close)) <|> str)
      many1 spaceChar
      textStr close
      return arg
    -- Some legacy entity names are required to be parsed by Pod formatters
    oct = listToMaybe . readOct @Integer
    entity "apos" = Just "'"
    entity "sol" = Just "/"
    entity "verbar" = Just "|"
    entity "lchevron" = Just "«"
    entity "rchevron" = Just "»"
    entity (T.stripPrefix "0x" -> Just suf) = lookupEntity $ "#x" <> suf
    entity (T.stripPrefix "0" -> Just suf)
        | Just (n, "") <- oct (T.unpack suf) = lookupEntity $ "#" <> tshow n
    entity (TR.decimal @Integer -> Right (x, "")) = lookupEntity $ "#" <> tshow x
    entity x = lookupEntity x

-- god knows there must be a higher order way of writing this thing, where we
-- have multiple different possible parser states within the link argument
-- varying depending on whether the link is expanded or not, but at least I
-- understand what I've done. This would be less wacky with a lexing step.
link :: PandocMonad m => PodParser m Inlines
link = do
  identifier <- textToIdentifier <$> getOption readerExtensions
  (name, dest) <- try expandedLinkArg <|> compactLinkArg
  return $ mkLink identifier name dest
  where
    compactLinkArg = do
      char '<'
      name <- linkName whitespace ">"
      dest <- linkDest whitespace (char '>') ">"
      char '>'
      return (mconcat <$> name, dest)
    expandedLinkArg = do
      openLen <- length <$> many1 (char '<')
      let closeStr = textStr (T.pack $ replicate openLen '>')
      let close = skipMany1 spaceChar *> closeStr
      let sp = try $ many1 spaceChar *> notFollowedBy closeStr $> B.space
      many1 spaceChar
      name <- linkName sp ""
      dest <- linkDest sp close ""
      close
      return (mconcat <$> name, dest)
    mkLink identifier name dest =
      let name' = fromMaybe (defaultLinkName dest) name in
          case dest of
            LinkUrl _ href -> B.link href "" name'
            LinkMan nm Nothing ->  B.linkWith (mempty, mempty, [("manual", stringify nm)]) "" "" name'
            LinkMan nm (Just sc) -> B.linkWith (mempty, mempty, [("manual", stringify nm), ("section", stringify sc)]) "" "" name'
            LinkInternal sc -> B.link ("#" <> identifier (stringify sc)) "" name'

    linkName sp ex = optionMaybe $ try $ many
        (try format
         <|> sp
         <|> B.str <$> many1Char (podCharLess ('|':ex))) <* char '|'
    linkDest sp close ex = try (url ex) <|> internal sp close ex <|> man sp close ex
    -- perlpodspec sez:
    --    Note that you can distinguish URL-links from anything else by the
    --    fact that they match m/\A\w+:[^:\s]\S*\z/.
    -- This is obviously not an RFC-compliant matcher for a URI scheme, but
    -- this is what the specification and the canonical implementation (Pod::Simple)
    -- do for deciding that a link target "looks like" a URL, as opposed to a
    -- manual page reference, so what we are doing here is roughly equivalent
    -- even though it is nonsense
    url ex = do
      scheme <- many1Char (letter <|> digit <|> char '_')
      colon <- T.singleton <$> char ':' <* notFollowedBy (char ':')
      rst <- many (format <|> B.str <$> many1Char (podCharLess ex))
      return $ LinkUrl
                 (B.str scheme <> B.str colon <> mconcat rst)
                 (scheme <> colon <> stringify rst)
    quotedSection sp close ex = do
      let mystr = B.str <$> many1Char (podCharLess ('\"':ex) <|> try (char '"' <* notFollowedBy close))
      char '"'
      ins <- mconcat <$> many1 (format <|> sp <|> mystr)
      char '"'
      return ins
    section sp close ex = try (quotedSection sp close ex) <|> mconcat <$> many1 (format <|> sp <|> B.str <$> many1Char (podCharLess ex))
    internal sp close ex = do
      char '/'
      LinkInternal <$> section sp close ex
    notSlash sp ex = format <|> sp <|> B.str <$> many1Char (podCharLess ('/':ex))
    man sp close ex = do
      page <- mconcat <$> many (notSlash sp ex)
      sec <- optionMaybe $ char '/' *> section sp close ex
      return $ LinkMan page sec

whitespace :: PandocMonad m => PodParser m Inlines
whitespace = try $ do
  many1 spaceChar *> optional newline <|> many spaceChar *> void newline
  notFollowedBy blankline
  return B.space

podCharLess :: PandocMonad m => String -> PodParser m Char
podCharLess exclude = try (satisfy isAsciiUpper <* notFollowedBy (char '<'))
                <|> satisfy (\c -> not (isSpaceChar c || isAsciiUpper c || elem c exclude))

podChar :: PandocMonad m => PodParser m Char
podChar = try (satisfy isAsciiUpper <* notFollowedBy (char '<'))
                <|> satisfy (\c -> not (isSpaceChar c || isAsciiUpper c))

str :: PandocMonad m => PodParser m Inlines
str = B.str <$> many1Char podChar

nonEmptyLine :: PandocMonad m => PodParser m T.Text
nonEmptyLine = try $ do
  pre <- manyChar spaceChar
  something <- T.singleton <$> nonspaceChar
  post <- anyLineNewline
  return $ pre <> something <> post

verbatim :: PandocMonad m => PodParser m Blocks
verbatim = do
  start <- startVerbatimLine
  lns <- many (nonEmptyLine <|>
                     try (do b <- blanklines
                             l <- startVerbatimLine
                             return $ b <> l))
  optional blanklines
  return $ B.codeBlock $ mconcat $ start:lns
  where
    startVerbatimLine = many1Char spaceChar <> nonEmptyLine

-- =begin/=end/=for and data paragraphs
-- The =begin/=end (and single-paragraph =for variant) markers in Pod are
-- designed as an extension point for specific formatters
--
-- this doesn't strictly match the intent of "=begin :ident" pod blocks, which
-- are still meant to be processed specially by the formatter, and only land in
-- the output upon request, i.e. pod2html will process "=begin :html" blocks as
-- Pod and include them in the regular output. Since the regions contain Pod
-- markup it seems to me that the best thing to do is parse the markup and put
-- a classname on it, allowing users to respond as desired with filters.
-- Pandoc doesn't have a built-in concept of parsed Divs that are only rendered
-- to certain formats, just raw blocks.
--
-- perlpodspec allows nesting of =begin/=end regions but we currently don't
-- because it would be annoying and we have something somewhat useful we
-- can do with these blocks which is treat them as RawBlocks, which matches
-- the intent reasonably well, and that gets weirder if we parse a nested
-- structure. It seems unlikely this would be encountered in the wild.

regionIdentifier :: PandocMonad m => PodParser m T.Text
regionIdentifier = many1Char (alphaNum <|> oneOf "-_")

for :: PandocMonad m => PodParser m Blocks
for = do
  string "for"
  many1 spaceChar
  forDiv <|> forData

forDiv :: PandocMonad m => PodParser m Blocks
forDiv = do
  char ':'
  cls <- regionIdentifier
  many1 spaceChar
  B.divWith (mempty, [cls], mempty) <$> paragraph

forData :: PandocMonad m => PodParser m Blocks
forData = do
  fmt <- regionIdentifier
  ln1 <- anyLineNewline
  lns <- many nonEmptyLine
  optional blanklines
  return $ B.rawBlock fmt (T.concat (ln1 : lns))

begin :: PandocMonad m => PodParser m Blocks
begin = do
  cmd "begin"
  beginDiv <|> beginData

beginDiv :: PandocMonad m => PodParser m Blocks
beginDiv = do
  char ':'
  cls <- regionIdentifier
  anyLine  -- "parameters" may appear in this position
  blanklines
  bs <- mconcat <$> many block
  textStr ("=end :" <> cls) <* blanklines
  return $ B.divWith (mempty, [cls], mempty) bs

beginData :: PandocMonad m => PodParser m Blocks
beginData = do
  fmt <- regionIdentifier
  anyLine
  blanklines
  lns <- mconcat <$> many (try rawCut <|> rawLine)
  textStr ("=end " <> fmt) <* blanklines
  return $ B.rawBlock fmt lns
  where
    rawCut = do
      char '=' *> cut
      pod <?> "=pod to close =cut within =begin/=end"
      return mempty
    rawLine = do
      try (notFollowedBy (char '=' *> letter))
      anyLineNewline
