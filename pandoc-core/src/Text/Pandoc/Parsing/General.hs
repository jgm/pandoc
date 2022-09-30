{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{- |
Module      : Text.Pandoc.Parsing.General
Copyright   : © 2006-2022 John MacFarlane
License     : GPL-2.0-or-later
Maintainer  : John MacFarlane <jgm@berkeley.edu>

Parser combinators for pandoc format readers.
-}

module Text.Pandoc.Parsing.General
  ( (<+?>)
  , anyLine
  , anyLineNewline
  , blankline
  , blanklines
  , charRef
  , characterReference
  , charsInBalanced
  , countChar
  , emailAddress
  , enclosed
  , escaped
  , extractIdClass
  , gobbleAtMostSpaces
  , gobbleSpaces
  , indentWith
  , insertIncludedFile
  , isSpaceChar          -- not re-exported from T.P.Parsing
  , lineBlockLines
  , lineClump
  , many1Char
  , many1Till
  , many1TillChar
  , manyChar
  , manyTillChar
  , manyUntil
  , manyUntilChar
  , nested
  , nonspaceChar
  , notFollowedBy'
  , oneOfStrings
  , oneOfStringsCI
  , parseFromString
  , parseFromString'
  , readWith
  , readWithM
  , registerHeader
  , sepBy1'
  , skipSpaces
  , spaceChar
  , stringAnyCase
  , testStringWith
  , textStr
  , token
  , trimInlinesF
  , uri
  , withHorizDisplacement
  , withRaw
  )
where

import Control.Monad
  ( guard
  , join
  , liftM
  , unless
  , void
  , when
  )
import Control.Monad.Except ( MonadError(throwError) )
import Control.Monad.Identity ( Identity(..), MonadPlus(mzero) )
import Data.Char
  ( chr
  , isAlphaNum
  , isAscii
  , isAsciiUpper
  , isSpace
  , ord
  , toLower
  , toUpper
  )
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.HTML.TagSoup.Entity (lookupEntity)
import Text.Pandoc.Asciify (toAsciiText)
import Text.Pandoc.Builder (Attr, Inline(Str), Inlines, trimInlines)
import Text.Pandoc.Class.PandocMonad (PandocMonad, readFileFromDirs, report)
import Text.Pandoc.Logging
  ( LogMessage(CouldNotLoadIncludeFile, DuplicateIdentifier) )
import Text.Pandoc.Options
  ( extensionEnabled
  , Extension(Ext_auto_identifiers, Ext_ascii_identifiers)
  , ReaderOptions(readerTabStop, readerExtensions) )
import Text.Pandoc.Shared (escapeURI, mapLeft, schemes, tshow, uniqueIdent)
import Text.Pandoc.Sources
import Text.Pandoc.XML (fromEntities)
import Text.Parsec
  ( (<|>)
  , ParsecT
  , SourcePos
  , Stream(..)
  , between
  , choice
  , count
  , getInput
  , getPosition
  , getState
  , lookAhead
  , many
  , many1
  , manyTill
  , notFollowedBy
  , option
  , runParserT
  , setInput
  , setPosition
  , skipMany
  , sourceColumn
  , sourceName
  , tokenPrim
  , try
  , unexpected
  , updateState
  )
import Text.Parsec.Pos (initialPos, newPos)
import Text.Pandoc.Error
  ( PandocError(PandocParseError, PandocParsecError) )
import Text.Pandoc.Parsing.Capabilities
import Text.Pandoc.Parsing.State
import Text.Pandoc.Parsing.Types ( Parser, ParserT, Future (..))

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.UTF8 as UTF8 (putStrLn)

-- | Remove whitespace from start and end; just like @'trimInlines'@,
-- but lifted into the 'Future' type.
trimInlinesF :: Future s Inlines -> Future s Inlines
trimInlinesF = liftM trimInlines

-- | Like @count@, but packs its result
countChar :: (Stream s m Char, UpdateSourcePos s Char, Monad m)
          => Int
          -> ParsecT s st m Char
          -> ParsecT s st m Text
countChar n = fmap T.pack . count n

-- | Like @string@, but uses @Text@.
textStr :: (Stream s m Char, UpdateSourcePos s Char)
        => Text -> ParsecT s u m Text
textStr t = string (T.unpack t) $> t


-- | Parse any line of text, returning the contents without the
-- final newline.
anyLine :: Monad m => ParserT Sources st m Text
anyLine = do
  -- This is much faster than:
  -- manyTill anyChar newline
  inp <- getInput
  case inp of
    Sources [] -> mzero
    Sources ((fp,t):inps) ->
      -- we assume that lines don't span different input files
      case T.break (=='\n') t of
           (this, rest)
             | T.null rest
             , not (null inps) ->
                -- line may span different input files, so do it
                 -- character by character
                 T.pack <$> manyTill anyChar newline
             | otherwise -> do --  either end of inputs or newline in rest
                 setInput $ Sources ((fp, rest):inps)
                 char '\n' -- needed so parsec knows we won't match empty string
                           -- and so source pos is updated
                 return this

-- | Parse any line, include the final newline in the output
anyLineNewline :: Monad m => ParserT Sources st m Text
anyLineNewline = (<> "\n") <$> anyLine

-- | Parse indent by specified number of spaces (or equiv. tabs)
indentWith :: (Stream s m Char, UpdateSourcePos s Char)
           => HasReaderOptions st
           => Int -> ParserT s st m Text
indentWith num = do
  tabStop <- getOption readerTabStop
  if num < tabStop
     then countChar num (char ' ')
     else choice [ try (countChar num (char ' '))
                 , try (char '\t' >> indentWith (num - tabStop)) ]

-- | Like @many@, but packs its result.
manyChar :: Stream s m t
         => ParserT s st m Char
         -> ParserT s st m Text
manyChar = fmap T.pack . many

-- | Like @many1@, but packs its result.
many1Char :: Stream s m t
          => ParserT s st m Char
          -> ParserT s st m Text
many1Char = fmap T.pack . many1

-- | Like @manyTill@, but packs its result.
manyTillChar :: Stream s m t
             => ParserT s st m Char
             -> ParserT s st m a
             -> ParserT s st m Text
manyTillChar p = fmap T.pack . manyTill p

-- | Like @manyTill@, but reads at least one item.
many1Till :: (Show end, Stream s m t)
          => ParserT s st m a
          -> ParserT s st m end
          -> ParserT s st m [a]
many1Till p end = do
         notFollowedBy' end
         first <- p
         rest <- manyTill p end
         return (first:rest)

-- | Like @many1Till@, but packs its result
many1TillChar :: (Show end, Stream s m t)
              => ParserT s st m Char
              -> ParserT s st m end
              -> ParserT s st m Text
many1TillChar p = fmap T.pack . many1Till p

-- | Like @manyTill@, but also returns the result of end parser.
manyUntil :: ParserT s u m a
          -> ParserT s u m b
          -> ParserT s u m ([a], b)
manyUntil p end = scan
  where scan =
          (do e <- end
              return ([], e)
          ) <|>
          (do x <- p
              (xs, e) <- scan
              return (x:xs, e))

-- | Like @manyUntil@, but also packs its result.
manyUntilChar :: ParserT s u m Char
              -> ParserT s u m b
              -> ParserT s u m (Text, b)
manyUntilChar p = fmap go . manyUntil p
  where
    go (x, y) = (T.pack x, y)

-- | Like @sepBy1@ from Parsec,
-- but does not fail if it @sep@ succeeds and @p@ fails.
sepBy1' :: ParsecT s u m a
        -> ParsecT s u m sep
        -> ParsecT s u m [a]
sepBy1' p sep = (:) <$> p <*> many (try $ sep >> p)

-- | A more general form of @notFollowedBy@.  This one allows any
-- type of parser to be specified, and succeeds only if that parser fails.
-- It does not consume any input.
notFollowedBy' :: (Show b, Stream s m a) => ParserT s st m b -> ParserT s st m ()
notFollowedBy' p  = try $ join $  do  a <- try p
                                      return (unexpected (show a))
                                  <|>
                                  return (return ())
-- (This version due to Andrew Pimlott on the Haskell mailing list.)

oneOfStrings' :: (Stream s m Char, UpdateSourcePos s Char)
              => (Char -> Char -> Bool) -> [Text] -> ParserT s st m Text
oneOfStrings' f = fmap T.pack . oneOfStrings'' f . fmap T.unpack

-- TODO: This should be re-implemented in a Text-aware way
oneOfStrings'' :: (Stream s m Char, UpdateSourcePos s Char)
               => (Char -> Char -> Bool) -> [String] -> ParserT s st m String
oneOfStrings'' _ []   = Prelude.fail "no strings"
oneOfStrings'' matches strs = try $ do
  c <- anyChar
  let strs' = [xs | (x:xs) <- strs, x `matches` c]
  case strs' of
       []  -> Prelude.fail "not found"
       _   -> (c:) <$> oneOfStrings'' matches strs'
               <|> if "" `elem` strs'
                      then return [c]
                      else Prelude.fail "not found"

-- | Parses one of a list of strings.  If the list contains
-- two strings one of which is a prefix of the other, the longer
-- string will be matched if possible.
oneOfStrings :: (Stream s m Char, UpdateSourcePos s Char)
             => [Text] -> ParserT s st m Text
oneOfStrings = oneOfStrings' (==)

-- | Parses one of a list of strings (tried in order), case insensitive.

-- TODO: This will not be accurate with general Unicode (neither
-- Text.toLower nor Text.toCaseFold can be implemented with a map)
oneOfStringsCI :: (Stream s m Char, UpdateSourcePos s Char)
               => [Text] -> ParserT s st m Text
oneOfStringsCI = oneOfStrings' ciMatch
  where ciMatch x y = toLower' x == toLower' y
        -- this optimizes toLower by checking common ASCII case
        -- first, before calling the expensive unicode-aware
        -- function:
        toLower' c | isAsciiUpper c = chr (ord c + 32)
                   | isAscii c = c
                   | otherwise = toLower c

-- | Parses a space or tab.
spaceChar :: (Stream s m Char, UpdateSourcePos s Char)
          => ParserT s st m Char
spaceChar = satisfy $ \c -> c == ' ' || c == '\t'

-- | Parses a nonspace, nonnewline character.
nonspaceChar :: (Stream s m Char, UpdateSourcePos s Char)
             => ParserT s st m Char
nonspaceChar = satisfy (not . isSpaceChar)

isSpaceChar :: Char -> Bool
isSpaceChar ' '  = True
isSpaceChar '\t' = True
isSpaceChar '\n' = True
isSpaceChar '\r' = True
isSpaceChar _    = False

-- | Skips zero or more spaces or tabs.
skipSpaces :: (Stream s m Char, UpdateSourcePos s Char)
           => ParserT s st m ()
skipSpaces = skipMany spaceChar

-- | Skips zero or more spaces or tabs, then reads a newline.
blankline :: (Stream s m Char, UpdateSourcePos s Char)
          => ParserT s st m Char
blankline = try $ skipSpaces >> newline

-- | Parses one or more blank lines and returns a string of newlines.
blanklines :: (Stream s m Char, UpdateSourcePos s Char)
           => ParserT s st m Text
blanklines = T.pack <$> many1 blankline

-- | Gobble n spaces; if tabs are encountered, expand them
-- and gobble some or all of their spaces, leaving the rest.
gobbleSpaces :: (HasReaderOptions st, Monad m)
             => Int -> ParserT Sources st m ()
gobbleSpaces 0 = return ()
gobbleSpaces n
  | n < 0     = error "gobbleSpaces called with negative number"
  | otherwise = try $ do
      char ' ' <|> eatOneSpaceOfTab
      gobbleSpaces (n - 1)

eatOneSpaceOfTab :: (HasReaderOptions st, Monad m) => ParserT Sources st m Char
eatOneSpaceOfTab = do
  lookAhead (char '\t')
  pos <- getPosition
  tabstop <- getOption readerTabStop
  -- replace the tab on the input stream with spaces
  let numSpaces = tabstop - ((sourceColumn pos - 1) `mod` tabstop)
  inp <- getInput
  setInput $
    case inp of
      Sources [] -> error "eatOneSpaceOfTab - empty Sources list"
      Sources ((fp,t):rest) ->
        -- drop the tab and add spaces
        Sources ((fp, T.replicate numSpaces " " <> T.drop 1 t):rest)
  char ' '

-- | Gobble up to n spaces; if tabs are encountered, expand them
-- and gobble some or all of their spaces, leaving the rest.
gobbleAtMostSpaces :: (HasReaderOptions st, Monad m)
                   => Int -> ParserT Sources st m Int
gobbleAtMostSpaces 0 = return 0
gobbleAtMostSpaces n
  | n < 0     = error "gobbleAtMostSpaces called with negative number"
  | otherwise = option 0 $ do
      char ' ' <|> eatOneSpaceOfTab
      (+ 1) <$> gobbleAtMostSpaces (n - 1)

-- | Parses material enclosed between start and end parsers.
enclosed :: (Show end, Stream s m Char, UpdateSourcePos s Char)
         => ParserT s st m t   -- ^ start parser
         -> ParserT s st m end  -- ^ end parser
         -> ParserT s st m a    -- ^ content parser (to be used repeatedly)
         -> ParserT s st m [a]
enclosed start end parser = try $
  start >> notFollowedBy space >> many1Till parser end

-- | Parse string, case insensitive.
stringAnyCase :: (Stream s m Char, UpdateSourcePos s Char)
              => Text -> ParserT s st m Text
stringAnyCase = fmap T.pack . stringAnyCase' . T.unpack

stringAnyCase' :: (Stream s m Char, UpdateSourcePos s Char)
               => String -> ParserT s st m String
stringAnyCase' [] = string ""
stringAnyCase' (x:xs) = do
  firstChar <- char (toUpper x) <|> char (toLower x)
  rest <- stringAnyCase' xs
  return (firstChar:rest)

-- TODO rewrite by just adding to Sources stream?
-- | Parse contents of 'str' using 'parser' and return result.
parseFromString :: Monad m
                => ParserT Sources st m r
                -> Text
                -> ParserT Sources st m r
parseFromString parser str = do
  oldPos <- getPosition
  oldInput <- getInput
  setInput $ toSources str
  setPosition $ initialPos $ sourceName oldPos <> "_chunk"
  result <- parser
  spaces
  setInput oldInput
  setPosition oldPos
  return result

-- | Like 'parseFromString' but specialized for 'ParserState'.
-- This resets 'stateLastStrPos', which is almost always what we want.
parseFromString' :: (Monad m, HasLastStrPosition u)
                 => ParserT Sources u m a
                 -> Text
                 -> ParserT Sources u m a
parseFromString' parser str = do
  oldLastStrPos <- getLastStrPos <$> getState
  updateState $ setLastStrPos Nothing
  res <- parseFromString parser str
  updateState $ setLastStrPos oldLastStrPos
  return res

-- | Parse raw line block up to and including blank lines.
lineClump :: Monad m => ParserT Sources st m Text
lineClump = blanklines
          <|> (T.unlines <$> many1 (notFollowedBy blankline >> anyLine))

-- | Parse a string of characters between an open character
-- and a close character, including text between balanced
-- pairs of open and close, which must be different. For example,
-- @charsInBalanced '(' ')' anyChar@ will parse "(hello (there))"
-- and return "hello (there)".
charsInBalanced :: (Stream s m Char, UpdateSourcePos s Char) => Char -> Char -> ParserT s st m Char
                -> ParserT s st m Text
charsInBalanced open close parser = try $ do
  char open
  let isDelim c = c == open || c == close
  raw <- many $  T.pack <$> many1 (notFollowedBy (satisfy isDelim) >> parser)
             <|> (do res <- charsInBalanced open close parser
                     return $ T.singleton open <> res <> T.singleton close)
  char close
  return $ T.concat raw

-- old charsInBalanced would be:
-- charsInBalanced open close (noneOf "\n" <|> char '\n' >> notFollowedBy blankline)
-- old charsInBalanced' would be:
-- charsInBalanced open close anyChar

-- Parsers for email addresses and URIs

-- | Parses an email address; returns original and corresponding
-- escaped mailto: URI.
emailAddress :: (Stream s m Char, UpdateSourcePos s Char) => ParserT s st m (Text, Text)
emailAddress = try $ toResult <$> mailbox <*> (char '@' *> domain)
 where toResult mbox dom = let full = fromEntities $ T.pack $ mbox ++ '@':dom
                           in  (full, escapeURI $ "mailto:" <> full)
       mailbox           = intercalate "." <$> (emailWord `sepBy1'` dot)
       domain            = intercalate "." <$> (subdomain `sepBy1'` dot)
       dot               = char '.'
       subdomain         = many1 $ alphaNum <|> innerPunct (=='-')
       -- this excludes some valid email addresses, since an
       -- email could contain e.g. '__', but gives better results
       -- for our purposes, when combined with markdown parsing:
       innerPunct f      = try (satisfy f
                                 <* notFollowedBy (satisfy (not . isAlphaNum)))
       -- technically an email address could begin with a symbol,
       -- but allowing this creates too many problems.
       -- See e.g. https://github.com/jgm/pandoc/issues/2940
       emailWord         = do x <- satisfy isAlphaNum
                              xs <- many (satisfy isEmailChar)
                              return (x:xs)
       isEmailChar c     = isAlphaNum c || isEmailPunct c
       isEmailPunct c    = T.any (== c) "!\"#$%&'*+-/=?^_{|}~;"


uriScheme :: (Stream s m Char, UpdateSourcePos s Char) => ParserT s st m Text
uriScheme = oneOfStringsCI (Set.toList schemes)

-- | Parses a URI. Returns pair of original and URI-escaped version.
uri :: (Stream s m Char, UpdateSourcePos s Char) => ParserT s st m (Text, Text)
uri = try $ do
  scheme <- uriScheme
  char ':'
  -- Avoid parsing e.g. "**Notes:**" as a raw URI:
  notFollowedBy $ satisfy (\c -> c == '*' || c == '_' || c == ']')
  -- We allow sentence punctuation except at the end, since
  -- we don't want the trailing '.' in 'http://google.com.' We want to allow
  -- http://en.wikipedia.org/wiki/State_of_emergency_(disambiguation)
  -- as a URL, while NOT picking up the closing paren in
  -- (http://wikipedia.org). So we include balanced parens in the URL.
  str <- T.concat <$> many1 (uriChunkBetween '(' ')'
                        <|> uriChunkBetween '{' '}'
                        <|> uriChunkBetween '[' ']'
                        <|> T.pack <$> uriChunk)
  str' <- option str $ char '/' >> return (str <> "/")
  let uri' = scheme <> ":" <> fromEntities str'
  return (uri', escapeURI uri')
  where
    isWordChar '#' = True
    isWordChar '$' = True
    isWordChar '%' = True
    isWordChar '+' = True
    isWordChar '/' = True
    isWordChar '@' = True
    isWordChar '\\' = True
    isWordChar '_' = True
    isWordChar '-' = True
    isWordChar '&' = True
    isWordChar '=' = True
    isWordChar c   = isAlphaNum c

    wordChar = satisfy isWordChar
    percentEscaped = try $ (:) <$> char '%' <*> many1 hexDigit
    entity = try $ pure <$> characterReference
    punct = try $ many1 (char ',') <|> fmap pure (satisfy (\c -> not (isSpace c) && c /= '<' && c /= '>'))
    uriChunk = many1 wordChar
           <|> percentEscaped
           <|> entity
           <|> try (punct <* lookAhead (void wordChar <|> void percentEscaped))
    uriChunkBetween l r = try $ do chunk <- between (char l) (char r) uriChunk
                                   return (T.pack $ [l] ++ chunk ++ [r])

-- | Applies a parser, returns tuple of its results and its horizontal
-- displacement (the difference between the source column at the end
-- and the source column at the beginning). Vertical displacement
-- (source row) is ignored.
withHorizDisplacement :: (Stream s m Char, UpdateSourcePos s Char)
                      => ParserT s st m a  -- ^ Parser to apply
                      -> ParserT s st m (a, Int) -- ^ (result, displacement)
withHorizDisplacement parser = do
  pos1 <- getPosition
  result <- parser
  pos2 <- getPosition
  return (result, sourceColumn pos2 - sourceColumn pos1)

-- | Applies a parser and returns the raw string that was parsed,
-- along with the value produced by the parser.
withRaw :: Monad m
        => ParsecT Sources st m a
        -> ParsecT Sources st m (a, Text)
withRaw parser = do
  inps1 <- getInput
  result <- parser
  inps2 <- getInput
  -- 'raw' is the difference between inps1 and inps2
  return (result, sourcesDifference inps1 inps2)

sourcesDifference :: Sources -> Sources -> Text
sourcesDifference (Sources is1) (Sources is2) = go is1 is2
 where
   go inps1 inps2 =
    case (inps1, inps2) of
      ([], _) -> mempty
      (_, []) -> mconcat $ map snd inps1
      ((p1,t1):rest1, (p2, t2):rest2)
        | p1 == p2
        , t1 == t2  -> go rest1 rest2
        | p1 == p2
        , t1 /= t2  -> fromMaybe mempty $ T.stripSuffix t2 t1
        | otherwise -> t1 <> go rest1 inps2

-- | Parses backslash, then applies character parser.
escaped :: (Stream s m Char, UpdateSourcePos s Char)
        => ParserT s st m Char  -- ^ Parser for character to escape
        -> ParserT s st m Char
escaped parser = try $ char '\\' >> parser

-- | Parse character entity.
characterReference :: (Stream s m Char, UpdateSourcePos s Char) => ParserT s st m Char
characterReference = try $ do
  char '&'
  ent <- many1Till nonspaceChar (char ';')
  let ent' = case ent of
                  '#':'X':xs -> '#':'x':xs  -- workaround tagsoup bug
                  '#':_      -> ent
                  _          -> ent ++ ";"
  case lookupEntity ent' of
       Just (c : _) -> return c
       _            -> Prelude.fail "entity not found"

-- | Parses a character reference and returns a Str element.
charRef :: (Stream s m Char, UpdateSourcePos s Char) => ParserT s st m Inline
charRef = Str . T.singleton <$> characterReference

lineBlockLine :: Monad m => ParserT Sources st m Text
lineBlockLine = try $ do
  char '|'
  char ' '
  white <- T.pack <$> many (spaceChar >> return '\160')
  notFollowedBy newline
  line <- anyLine
  continuations <- many (try $ char ' ' >> anyLine)
  return $ white <> T.unwords (line : continuations)

blankLineBlockLine :: (Stream s m Char, UpdateSourcePos s Char) => ParserT s st m Char
blankLineBlockLine = try (char '|' >> blankline)

-- | Parses an RST-style line block and returns a list of strings.
lineBlockLines :: Monad m => ParserT Sources st m [Text]
lineBlockLines = try $ do
  lines' <- many1 (lineBlockLine <|> (T.singleton <$> blankLineBlockLine))
  skipMany blankline
  return lines'


-- | Removes the ParsecT layer from the monad transformer stack
readWithM :: (Monad m, ToSources t)
          => ParserT Sources st m a  -- ^ parser
          -> st                      -- ^ initial state
          -> t                       -- ^ input
          -> m (Either PandocError a)
readWithM parser state input =
    mapLeft (PandocParsecError sources)
      <$> runParserT parser state (initialSourceName sources) sources
 where
   sources = toSources input

-- | Parse a string with a given parser and state
readWith :: ToSources t
         => Parser Sources st a
         -> st
         -> t
         -> Either PandocError a
readWith p t inp = runIdentity $ readWithM p t inp

-- | Parse a string with @parser@ (for testing).
testStringWith :: Show a
               => ParserT Sources ParserState Identity a
               -> Text
               -> IO ()
testStringWith parser str = UTF8.putStrLn $ tshow $
                            readWith parser defaultParserState (toSources str)

-- | Add header to the list of headers in state, together
--  with its associated identifier.  If the identifier is null
--  and the auto_identifiers extension is set, generate a new
--  unique identifier, and update the list of identifiers
--  in state.  Issue a warning if an explicit identifier
--  is encountered that duplicates an earlier identifier
--  (explicit or automatically generated).
registerHeader :: (Stream s m a, HasReaderOptions st,
                   HasLogMessages st, HasIdentifierList st)
               => Attr -> Inlines -> ParserT s st m Attr
registerHeader (ident,classes,kvs) header' = do
  ids <- extractIdentifierList <$> getState
  exts <- getOption readerExtensions
  if T.null ident && Ext_auto_identifiers `extensionEnabled` exts
     then do
       let id' = uniqueIdent exts (B.toList header') ids
       let id'' = if Ext_ascii_identifiers `extensionEnabled` exts
                     then toAsciiText id'
                     else id'
       updateState $ updateIdentifierList $ Set.insert id'
       updateState $ updateIdentifierList $ Set.insert id''
       return (id'',classes,kvs)
     else do
        unless (T.null ident) $ do
          when (ident `Set.member` ids) $ do
            pos <- getPosition
            logMessage $ DuplicateIdentifier ident pos
          updateState $ updateIdentifierList $ Set.insert ident
        return (ident,classes,kvs)

-- This is used to prevent exponential blowups for things like:
-- a**a*a**a*a**a*a**a*a**a*a**a*a**
nested :: Stream s m a
       => ParserT s ParserState m a
       -> ParserT s ParserState m a
nested p = do
  nestlevel <- stateMaxNestingLevel <$>  getState
  guard $ nestlevel > 0
  updateState $ \st -> st{ stateMaxNestingLevel = stateMaxNestingLevel st - 1 }
  res <- p
  updateState $ \st -> st{ stateMaxNestingLevel = nestlevel }
  return res

token :: (Stream s m t)
      => (t -> Text)
      -> (t -> SourcePos)
      -> (t -> Maybe a)
      -> ParsecT s st m a
token pp pos match = tokenPrim (T.unpack . pp) (\_ t _ -> pos t) match

infixr 5 <+?>
(<+?>) :: (Monoid a) => ParserT s st m a -> ParserT s st m a -> ParserT s st m a
a <+?> b = a >>= flip fmap (try b <|> return mempty) . mappend

extractIdClass :: Attr -> Attr
extractIdClass (ident, cls, kvs) = (ident', cls', kvs')
  where
    ident' = fromMaybe ident (lookup "id" kvs)
    cls'   = maybe cls T.words $ lookup "class" kvs
    kvs'   = filter (\(k,_) -> k /= "id" || k /= "class") kvs

insertIncludedFile :: (PandocMonad m, HasIncludeFiles st)
                   => ParserT a st m b -- ^ parser to apply
                   -> (Text -> a) -- ^ convert Text to stream type
                   -> [FilePath]  -- ^ search path (directories)
                   -> FilePath    -- ^ path of file to include
                   -> Maybe Int   -- ^ start line (negative counts from end)
                   -> Maybe Int   -- ^ end line (negative counts from end)
                   -> ParserT a st m b
insertIncludedFile parser toStream dirs f mbstartline mbendline = do
  oldPos <- getPosition
  oldInput <- getInput
  containers <- getIncludeFiles <$> getState
  when (T.pack f `elem` containers) $
    throwError $ PandocParseError $ T.pack $ "Include file loop at " ++ show oldPos
  updateState $ addIncludeFile $ T.pack f
  mbcontents <- readFileFromDirs dirs f
  contents <- case mbcontents of
                   Just s -> return $ exciseLines mbstartline mbendline s
                   Nothing -> do
                     report $ CouldNotLoadIncludeFile (T.pack f) oldPos
                     return ""
  setInput $ toStream contents
  setPosition $ newPos f (fromMaybe 1 mbstartline) 1
  result <- parser
  setInput oldInput
  setPosition oldPos
  updateState dropLatestIncludeFile
  return result

exciseLines :: Maybe Int -> Maybe Int -> Text -> Text
exciseLines Nothing Nothing t = t
exciseLines mbstartline mbendline t =
  T.unlines $ take (endline' - (startline' - 1))
            $ drop (startline' - 1) contentLines
 where
  contentLines = T.lines t
  numLines = length contentLines
  startline' = case mbstartline of
                 Nothing -> 1
                 Just x | x >= 0 -> x
                        | otherwise -> numLines + x -- negative from end
  endline' = case mbendline of
                 Nothing -> numLines
                 Just x | x >= 0 -> x
                        | otherwise -> numLines + x -- negative from end
