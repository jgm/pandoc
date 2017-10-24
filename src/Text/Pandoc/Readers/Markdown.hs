{-# LANGUAGE RelaxedPolyRec      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
Copyright (C) 2006-2017 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Readers.Markdown
   Copyright   : Copyright (C) 2006-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of markdown-formatted plain text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Markdown ( readMarkdown ) where

import Control.Monad
import Control.Monad.Except (throwError)
import Data.Char (isAlphaNum, isPunctuation, isSpace, toLower)
import qualified Data.HashMap.Strict as H
import Data.List (findIndex, intercalate, sortBy, transpose)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Scientific (base10Exponent, coefficient)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Yaml (ParseException (..), YamlException (..), YamlMark (..))
import qualified Data.Yaml as Yaml
import System.FilePath (addExtension, takeExtension)
import Text.HTML.TagSoup
import Text.Pandoc.Builder (Blocks, Inlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad(..), report)
import Text.Pandoc.Definition
import Text.Pandoc.Emoji (emojis)
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (tableWith)
import Text.Pandoc.Readers.HTML (htmlInBalanced, htmlTag, isBlockTag,
                                 isCommentTag, isInlineTag, isTextTag)
import Text.Pandoc.Readers.LaTeX (rawLaTeXBlock,
                                  rawLaTeXInline, applyMacros)
import Text.Pandoc.Shared
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.XML (fromEntities)

type MarkdownParser m = ParserT [Char] ParserState m

-- | Read markdown from an input string and return a Pandoc document.
readMarkdown :: PandocMonad m
             => ReaderOptions -- ^ Reader options
             -> Text      -- ^ String to parse (assuming @'\n'@ line endings)
             -> m Pandoc
readMarkdown opts s = do
  parsed <- (readWithM parseMarkdown) def{ stateOptions = opts }
               (T.unpack (crFilter s) ++ "\n\n")
  case parsed of
    Right result -> return result
    Left e       -> throwError e

--
-- Constants and data structure definitions
--

isBulletListMarker :: Char -> Bool
isBulletListMarker '*' = True
isBulletListMarker '+' = True
isBulletListMarker '-' = True
isBulletListMarker _   = False

isHruleChar :: Char -> Bool
isHruleChar '*' = True
isHruleChar '-' = True
isHruleChar '_' = True
isHruleChar _   = False

setextHChars :: String
setextHChars = "=-"

isBlank :: Char -> Bool
isBlank ' '  = True
isBlank '\t' = True
isBlank '\n' = True
isBlank _    = False

--
-- auxiliary functions
--

-- | Succeeds when we're in list context.
inList :: PandocMonad m => MarkdownParser m ()
inList = do
  ctx <- stateParserContext <$> getState
  guard (ctx == ListItemState)

spnl :: PandocMonad m => ParserT [Char] st m ()
spnl = try $ do
  skipSpaces
  optional newline
  skipSpaces
  notFollowedBy (char '\n')

indentSpaces :: PandocMonad m => MarkdownParser m String
indentSpaces = try $ do
  tabStop <- getOption readerTabStop
  count tabStop (char ' ') <|>
    string "\t" <?> "indentation"

nonindentSpaces :: PandocMonad m => MarkdownParser m String
nonindentSpaces = do
  n <- skipNonindentSpaces
  return $ replicate n ' '

-- returns number of spaces parsed
skipNonindentSpaces :: PandocMonad m => MarkdownParser m Int
skipNonindentSpaces = do
  tabStop <- getOption readerTabStop
  gobbleAtMostSpaces (tabStop - 1) <* notFollowedBy spaceChar

litChar :: PandocMonad m => MarkdownParser m Char
litChar = escapedChar'
       <|> characterReference
       <|> noneOf "\n"
       <|> try (newline >> notFollowedBy blankline >> return ' ')

-- | Parse a sequence of inline elements between square brackets,
-- including inlines between balanced pairs of square brackets.
inlinesInBalancedBrackets :: PandocMonad m => MarkdownParser m (F Inlines)
inlinesInBalancedBrackets = do
  char '['
  pos <- getPosition
  (_, raw) <- withRaw $ charsInBalancedBrackets 1
  guard $ not $ null raw
  parseFromString' (setPosition pos >>
                   trimInlinesF <$> inlines) (init raw)

charsInBalancedBrackets :: PandocMonad m => Int -> MarkdownParser m ()
charsInBalancedBrackets 0 = return ()
charsInBalancedBrackets openBrackets =
      (char '[' >> charsInBalancedBrackets (openBrackets + 1))
  <|> (char ']' >> charsInBalancedBrackets (openBrackets - 1))
  <|> ((  (() <$ code)
     <|> (() <$ (escapedChar'))
     <|> (newline >> notFollowedBy blankline)
     <|> skipMany1 (noneOf "[]`\n\\")
     <|> (() <$ count 1 (oneOf "`\\"))
      ) >> charsInBalancedBrackets openBrackets)

--
-- document structure
--

rawTitleBlockLine :: PandocMonad m => MarkdownParser m String
rawTitleBlockLine = do
  char '%'
  skipSpaces
  first <- anyLine
  rest <- many $ try $ do spaceChar
                          notFollowedBy blankline
                          skipSpaces
                          anyLine
  return $ trim $ unlines (first:rest)

titleLine :: PandocMonad m => MarkdownParser m (F Inlines)
titleLine = try $ do
  raw <- rawTitleBlockLine
  res <- parseFromString' inlines raw
  return $ trimInlinesF res

authorsLine :: PandocMonad m => MarkdownParser m (F [Inlines])
authorsLine = try $ do
  raw <- rawTitleBlockLine
  let sep = (char ';' <* spaces) <|> newline
  let pAuthors = sepEndBy
            (trimInlinesF . mconcat <$> many
                 (try $ notFollowedBy sep >> inline))
            sep
  sequence <$> parseFromString' pAuthors raw

dateLine :: PandocMonad m => MarkdownParser m (F Inlines)
dateLine = try $ do
  raw <- rawTitleBlockLine
  res <- parseFromString' inlines raw
  return $ trimInlinesF res

titleBlock :: PandocMonad m => MarkdownParser m ()
titleBlock = pandocTitleBlock <|> mmdTitleBlock

pandocTitleBlock :: PandocMonad m => MarkdownParser m ()
pandocTitleBlock = try $ do
  guardEnabled Ext_pandoc_title_block
  lookAhead (char '%')
  title <- option mempty titleLine
  author <- option (return []) authorsLine
  date <- option mempty dateLine
  optional blanklines
  let meta' = do title' <- title
                 author' <- author
                 date' <- date
                 return $
                     (if B.isNull title' then id else B.setMeta "title" title')
                   . (if null author' then id else B.setMeta "author" author')
                   . (if B.isNull date' then id else B.setMeta "date" date')
                   $ nullMeta
  updateState $ \st -> st{ stateMeta' = stateMeta' st <> meta' }


yamlMetaBlock :: PandocMonad m => MarkdownParser m (F Blocks)
yamlMetaBlock = try $ do
  guardEnabled Ext_yaml_metadata_block
  pos <- getPosition
  string "---"
  blankline
  notFollowedBy blankline  -- if --- is followed by a blank it's an HRULE
  rawYamlLines <- manyTill anyLine stopLine
  -- by including --- and ..., we allow yaml blocks with just comments:
  let rawYaml = unlines ("---" : (rawYamlLines ++ ["..."]))
  optional blanklines
  case Yaml.decodeEither' $ UTF8.fromString rawYaml of
       Right (Yaml.Object hashmap) -> do
         let alist = H.toList hashmap
         mapM_ (\(k, v) -> do
             if ignorable k
                then return ()
                else do
                  v' <- yamlToMeta v
                  let k' = T.unpack k
                  updateState $ \st -> st{ stateMeta' =
                     (do m <- stateMeta' st
                         -- if there's already a value, leave it unchanged
                         case lookupMeta k' m of
                              Just _ -> return m
                              Nothing -> do
                                v'' <- v'
                                return $ B.setMeta (T.unpack k) v'' m)}
           ) alist
       Right Yaml.Null -> return ()
       Right _ -> do
                   logMessage $
                      CouldNotParseYamlMetadata "not an object"
                      pos
                   return ()
       Left err' -> do
                case err' of
                   InvalidYaml (Just YamlParseException{
                               yamlProblem = problem
                             , yamlContext = _ctxt
                             , yamlProblemMark = Yaml.YamlMark {
                                   yamlLine = yline
                                 , yamlColumn = ycol
                             }}) ->
                        logMessage $ CouldNotParseYamlMetadata
                           problem (setSourceLine
                           (setSourceColumn pos
                              (sourceColumn pos + ycol))
                           (sourceLine pos + 1 + yline))
                   _ -> logMessage $ CouldNotParseYamlMetadata
                           (show err') pos
                return ()
  return mempty

-- ignore fields ending with _
ignorable :: Text -> Bool
ignorable t = (T.pack "_") `T.isSuffixOf` t

toMetaValue :: PandocMonad m
            => Text -> MarkdownParser m (F MetaValue)
toMetaValue x =
  parseFromString' parser' (T.unpack x)
  where parser' = (asInlines <$> ((trimInlinesF . mconcat)
                       <$> (guard (not endsWithNewline)
                             *> manyTill inline eof)))
                  <|> (asBlocks <$> parseBlocks)
        asBlocks p = do
          p' <- p
          return $ MetaBlocks (B.toList p')
        asInlines p = do
          p' <- p
          return $ MetaInlines (B.toList p')
        endsWithNewline = T.pack "\n" `T.isSuffixOf` x
        -- Note: a standard quoted or unquoted YAML value will
        -- not end in a newline, but a "block" set off with
        -- `|` or `>` will.

yamlToMeta :: PandocMonad m
           => Yaml.Value -> MarkdownParser m (F MetaValue)
yamlToMeta (Yaml.String t) = toMetaValue t
yamlToMeta (Yaml.Number n)
  -- avoid decimal points for numbers that don't need them:
  | base10Exponent n >= 0     = return $ return $ MetaString $ show
                                $ coefficient n * (10 ^ base10Exponent n)
  | otherwise                 = return $ return $ MetaString $ show n
yamlToMeta (Yaml.Bool b) = return $ return $ MetaBool b
yamlToMeta (Yaml.Array xs) = do
  xs' <- mapM yamlToMeta (V.toList xs)
  return $ do
    xs'' <- sequence xs'
    return $ B.toMetaValue xs''
yamlToMeta (Yaml.Object o) = do
  let alist = H.toList o
  foldM (\m (k,v) -> do
           if ignorable k
              then return m
              else do
                v' <- yamlToMeta v
                return $ do
                   MetaMap m' <- m
                   v'' <- v'
                   return (MetaMap $ M.insert (T.unpack k) v'' m'))
        (return $ MetaMap M.empty)
        alist
yamlToMeta _ = return $ return $ MetaString ""

stopLine :: PandocMonad m => MarkdownParser m ()
stopLine = try $ (string "---" <|> string "...") >> blankline >> return ()

mmdTitleBlock :: PandocMonad m => MarkdownParser m ()
mmdTitleBlock = try $ do
  guardEnabled Ext_mmd_title_block
  firstPair <- kvPair False
  restPairs <- many (kvPair True)
  let kvPairs = firstPair : restPairs
  blanklines
  updateState $ \st -> st{ stateMeta' = stateMeta' st <>
                             return (Meta $ M.fromList kvPairs) }

kvPair :: PandocMonad m => Bool -> MarkdownParser m (String, MetaValue)
kvPair allowEmpty = try $ do
  key <- many1Till (alphaNum <|> oneOf "_- ") (char ':')
  val <- trim <$> manyTill anyChar
          (try $ newline >> lookAhead (blankline <|> nonspaceChar))
  guard $ allowEmpty || not (null val)
  let key' = concat $ words $ map toLower key
  let val' = MetaBlocks $ B.toList $ B.plain $ B.text $ val
  return (key',val')

parseMarkdown :: PandocMonad m => MarkdownParser m Pandoc
parseMarkdown = do
  optional titleBlock
  blocks <- parseBlocks
  st <- getState
  -- check for notes with no corresponding note references
  let notesUsed = stateNoteRefs st
  let notesDefined = M.keys (stateNotes' st)
  mapM_ (\n -> unless (n `Set.member` notesUsed) $ do
                -- lookup to get sourcepos
                case M.lookup n (stateNotes' st) of
                   Just (pos, _) -> report (NoteDefinedButNotUsed n pos)
                   Nothing -> throwError $
                     PandocShouldNeverHappenError "note not found")
         notesDefined
  let doc = runF (do Pandoc _ bs <- B.doc <$> blocks
                     meta <- stateMeta' st
                     return $ Pandoc meta bs) st
  reportLogMessages
  return doc

referenceKey :: PandocMonad m => MarkdownParser m (F Blocks)
referenceKey = try $ do
  pos <- getPosition
  skipNonindentSpaces
  (_,raw) <- reference
  char ':'
  skipSpaces >> optional newline >> skipSpaces >> notFollowedBy (char '[')
  let sourceURL = liftM unwords $ many $ try $ do
                    skipMany spaceChar
                    notFollowedBy' referenceTitle
                    notFollowedBy' $ guardEnabled Ext_link_attributes >> attributes
                    notFollowedBy' (() <$ reference)
                    many1 $ notFollowedBy space >> litChar
  let betweenAngles = try $ char '<' >> manyTill litChar (char '>')
  src <- try betweenAngles <|> sourceURL
  tit <- option "" referenceTitle
  attr   <- option nullAttr $ try $
              do guardEnabled Ext_link_attributes
                 skipSpaces >> optional newline >> skipSpaces
                 attributes
  addKvs <- option [] $ guardEnabled Ext_mmd_link_attributes
                          >> many (try $ spnl >> keyValAttr)
  blanklines
  let attr'  = extractIdClass $ foldl (\x f -> f x) attr addKvs
      target = (escapeURI $ trimr src, tit)
  st <- getState
  let oldkeys = stateKeys st
  let key = toKey raw
  case M.lookup key oldkeys of
    Just (t,a) | not (t == target && a == attr') ->
      -- We don't warn on two duplicate keys if the targets are also
      -- the same. This can happen naturally with --reference-location=block
      -- or section. See #3701.
      logMessage $ DuplicateLinkReference raw pos
    _ -> return ()
  updateState $ \s -> s { stateKeys = M.insert key (target, attr') oldkeys }
  return $ return mempty

referenceTitle :: PandocMonad m => MarkdownParser m String
referenceTitle = try $ do
  skipSpaces >> optional newline >> skipSpaces
  quotedTitle '"' <|> quotedTitle '\'' <|> charsInBalanced '(' ')' litChar

-- A link title in quotes
quotedTitle :: PandocMonad m => Char -> MarkdownParser m String
quotedTitle c = try $ do
  char c
  notFollowedBy spaces
  let pEnder = try $ char c >> notFollowedBy (satisfy isAlphaNum)
  let regChunk = many1 (noneOf ['\\','\n','&',c]) <|> count 1 litChar
  let nestedChunk = (\x -> [c] ++ x ++ [c]) <$> quotedTitle c
  unwords . words . concat <$> manyTill (nestedChunk <|> regChunk) pEnder

-- | PHP Markdown Extra style abbreviation key.  Currently
-- we just skip them, since Pandoc doesn't have an element for
-- an abbreviation.
abbrevKey :: PandocMonad m => MarkdownParser m (F Blocks)
abbrevKey = do
  guardEnabled Ext_abbreviations
  try $ do
    char '*'
    reference
    char ':'
    skipMany (satisfy (/= '\n'))
    blanklines
    return $ return mempty

noteMarker :: PandocMonad m => MarkdownParser m String
noteMarker = string "[^" >> many1Till (satisfy $ not . isBlank) (char ']')

rawLine :: PandocMonad m => MarkdownParser m String
rawLine = try $ do
  notFollowedBy blankline
  notFollowedBy' $ try $ skipNonindentSpaces >> noteMarker
  optional indentSpaces
  anyLine

rawLines :: PandocMonad m => MarkdownParser m String
rawLines = do
  first <- anyLine
  rest <- many rawLine
  return $ unlines (first:rest)

noteBlock :: PandocMonad m => MarkdownParser m (F Blocks)
noteBlock = try $ do
  pos <- getPosition
  skipNonindentSpaces
  ref <- noteMarker
  char ':'
  optional blankline
  optional indentSpaces
  first <- rawLines
  rest <- many $ try $ blanklines >> indentSpaces >> rawLines
  let raw = unlines (first:rest) ++ "\n"
  optional blanklines
  parsed <- parseFromString' parseBlocks raw
  oldnotes <- stateNotes' <$> getState
  case M.lookup ref oldnotes of
    Just _  -> logMessage $ DuplicateNoteReference ref pos
    Nothing -> return ()
  updateState $ \s -> s { stateNotes' = M.insert ref (pos, parsed) oldnotes }
  return mempty

--
-- parsing blocks
--

parseBlocks :: PandocMonad m => MarkdownParser m (F Blocks)
parseBlocks = mconcat <$> manyTill block eof

block :: PandocMonad m => MarkdownParser m (F Blocks)
block = do
  res <- choice [ mempty <$ blanklines
               , codeBlockFenced
               , yamlMetaBlock
               -- note: bulletList needs to be before header because of
               -- the possibility of empty list items: -
               , bulletList
               , header
               , lhsCodeBlock
               , divHtml
               , divFenced
               , htmlBlock
               , table
               , codeBlockIndented
               , rawTeXBlock
               , lineBlock
               , blockQuote
               , hrule
               , orderedList
               , definitionList
               , noteBlock
               , referenceKey
               , abbrevKey
               , para
               , plain
               ] <?> "block"
  trace (take 60 $ show $ B.toList $ runF res defaultParserState)
  return res

--
-- header blocks
--

header :: PandocMonad m => MarkdownParser m (F Blocks)
header = setextHeader <|> atxHeader <?> "header"

atxChar :: PandocMonad m => MarkdownParser m Char
atxChar = do
  exts <- getOption readerExtensions
  return $ if extensionEnabled Ext_literate_haskell exts
              then '='
              else '#'

atxHeader :: PandocMonad m => MarkdownParser m (F Blocks)
atxHeader = try $ do
  level <- atxChar >>= many1 . char >>= return . length
  notFollowedBy $ guardEnabled Ext_fancy_lists >>
                  (char '.' <|> char ')') -- this would be a list
  guardDisabled Ext_space_in_atx_header <|> notFollowedBy nonspaceChar
  skipSpaces
  (text, raw) <- withRaw $
          trimInlinesF . mconcat <$> many (notFollowedBy atxClosing >> inline)
  attr <- atxClosing
  attr' <- registerHeader attr (runF text defaultParserState)
  guardDisabled Ext_implicit_header_references
    <|> registerImplicitHeader raw attr'
  return $ B.headerWith attr' level <$> text

atxClosing :: PandocMonad m => MarkdownParser m Attr
atxClosing = try $ do
  attr' <- option nullAttr
             (guardEnabled Ext_mmd_header_identifiers >> mmdHeaderIdentifier)
  skipMany . char =<< atxChar
  skipSpaces
  attr <- option attr'
             (guardEnabled Ext_header_attributes >> attributes)
  blanklines
  return attr

setextHeaderEnd :: PandocMonad m => MarkdownParser m Attr
setextHeaderEnd = try $ do
  attr <- option nullAttr
          $ (guardEnabled Ext_mmd_header_identifiers >> mmdHeaderIdentifier)
           <|> (guardEnabled Ext_header_attributes >> attributes)
  blanklines
  return attr

mmdHeaderIdentifier :: PandocMonad m => MarkdownParser m Attr
mmdHeaderIdentifier = do
  (_, raw) <- reference
  let raw' = trim $ stripFirstAndLast raw
  let ident = concat $ words $ map toLower raw'
  let attr = (ident, [], [])
  guardDisabled Ext_implicit_header_references
    <|> registerImplicitHeader raw' attr
  skipSpaces
  return attr

setextHeader :: PandocMonad m => MarkdownParser m (F Blocks)
setextHeader = try $ do
  -- This lookahead prevents us from wasting time parsing Inlines
  -- unless necessary -- it gives a significant performance boost.
  lookAhead $ anyLine >> many1 (oneOf setextHChars) >> blankline
  skipSpaces
  (text, raw) <- withRaw $
       trimInlinesF . mconcat <$> many1 (notFollowedBy setextHeaderEnd >> inline)
  attr <- setextHeaderEnd
  underlineChar <- oneOf setextHChars
  many (char underlineChar)
  blanklines
  let level = (fromMaybe 0 $ findIndex (== underlineChar) setextHChars) + 1
  attr' <- registerHeader attr (runF text defaultParserState)
  guardDisabled Ext_implicit_header_references
    <|> registerImplicitHeader raw attr'
  return $ B.headerWith attr' level <$> text

registerImplicitHeader :: PandocMonad m => String -> Attr -> MarkdownParser m ()
registerImplicitHeader raw attr@(ident, _, _) = do
  let key = toKey $ "[" ++ raw ++ "]"
  updateState (\s -> s { stateHeaderKeys =
                         M.insert key (('#':ident,""), attr) (stateHeaderKeys s) })

--
-- hrule block
--

hrule :: PandocMonad m => ParserT [Char] st m (F Blocks)
hrule = try $ do
  skipSpaces
  start <- satisfy isHruleChar
  count 2 (skipSpaces >> char start)
  skipMany (spaceChar <|> char start)
  newline
  optional blanklines
  return $ return B.horizontalRule

--
-- code blocks
--

indentedLine :: PandocMonad m => MarkdownParser m String
indentedLine = indentSpaces >> anyLineNewline

blockDelimiter :: PandocMonad m
               => (Char -> Bool)
               -> Maybe Int
               -> ParserT [Char] st m Int
blockDelimiter f len = try $ do
  c <- lookAhead (satisfy f)
  case len of
      Just l  -> count l (char c) >> many (char c) >> return l
      Nothing -> count 3 (char c) >> many (char c) >>=
                 return . (+ 3) . length

attributes :: PandocMonad m => MarkdownParser m Attr
attributes = try $ do
  char '{'
  spnl
  attrs <- many (attribute <* spnl)
  char '}'
  return $ foldl (\x f -> f x) nullAttr attrs

attribute :: PandocMonad m => MarkdownParser m (Attr -> Attr)
attribute = identifierAttr <|> classAttr <|> keyValAttr <|> specialAttr

identifier :: PandocMonad m => MarkdownParser m String
identifier = do
  first <- letter
  rest <- many $ alphaNum <|> oneOf "-_:."
  return (first:rest)

identifierAttr :: PandocMonad m => MarkdownParser m (Attr -> Attr)
identifierAttr = try $ do
  char '#'
  result <- identifier
  return $ \(_,cs,kvs) -> (result,cs,kvs)

classAttr :: PandocMonad m => MarkdownParser m (Attr -> Attr)
classAttr = try $ do
  char '.'
  result <- identifier
  return $ \(id',cs,kvs) -> (id',cs ++ [result],kvs)

keyValAttr :: PandocMonad m => MarkdownParser m (Attr -> Attr)
keyValAttr = try $ do
  key <- identifier
  char '='
  val <- enclosed (char '"') (char '"') litChar
     <|> enclosed (char '\'') (char '\'') litChar
     <|> many (escapedChar' <|> noneOf " \t\n\r}")
  return $ \(id',cs,kvs) ->
    case key of
         "id"    -> (val,cs,kvs)
         "class" -> (id',cs ++ words val,kvs)
         _       -> (id',cs,kvs ++ [(key,val)])

specialAttr :: PandocMonad m => MarkdownParser m (Attr -> Attr)
specialAttr = do
  char '-'
  return $ \(id',cs,kvs) -> (id',cs ++ ["unnumbered"],kvs)

rawAttribute :: PandocMonad m => MarkdownParser m String
rawAttribute = do
  char '{'
  skipMany spaceChar
  char '='
  format <- many1 $ satisfy (\c -> isAlphaNum c || c `elem` "-_")
  skipMany spaceChar
  char '}'
  return format

codeBlockFenced :: PandocMonad m => MarkdownParser m (F Blocks)
codeBlockFenced = try $ do
  c <- try (guardEnabled Ext_fenced_code_blocks >> lookAhead (char '~'))
     <|> (guardEnabled Ext_backtick_code_blocks >> lookAhead (char '`'))
  size <- blockDelimiter (== c) Nothing
  skipMany spaceChar
  rawattr <-
     (Left <$> try (guardEnabled Ext_raw_attribute >> rawAttribute))
    <|>
     (Right <$> option ("",[],[])
         (try (guardEnabled Ext_fenced_code_attributes >> attributes)
          <|> ((\x -> ("",[toLanguageId x],[])) <$> many1 nonspaceChar)))
  blankline
  contents <- intercalate "\n" <$>
                 manyTill anyLine (blockDelimiter (== c) (Just size))
  blanklines
  return $ return $
    case rawattr of
          Left syn   -> B.rawBlock syn contents
          Right attr -> B.codeBlockWith attr contents

-- correctly handle github language identifiers
toLanguageId :: String -> String
toLanguageId = map toLower . go
  where go "c++"         = "cpp"
        go "objective-c" = "objectivec"
        go x             = x

codeBlockIndented :: PandocMonad m => MarkdownParser m (F Blocks)
codeBlockIndented = do
  contents <- many1 (indentedLine <|>
                     try (do b <- blanklines
                             l <- indentedLine
                             return $ b ++ l))
  optional blanklines
  classes <- getOption readerIndentedCodeClasses
  return $ return $ B.codeBlockWith ("", classes, []) $
           stripTrailingNewlines $ concat contents

lhsCodeBlock :: PandocMonad m => MarkdownParser m (F Blocks)
lhsCodeBlock = do
  guardEnabled Ext_literate_haskell
  (return . B.codeBlockWith ("",["sourceCode","literate","haskell"],[]) <$>
          (lhsCodeBlockBird <|> lhsCodeBlockLaTeX))
    <|> (return . B.codeBlockWith ("",["sourceCode","haskell"],[]) <$>
          lhsCodeBlockInverseBird)

lhsCodeBlockLaTeX :: PandocMonad m => MarkdownParser m String
lhsCodeBlockLaTeX = try $ do
  string "\\begin{code}"
  manyTill spaceChar newline
  contents <- many1Till anyChar (try $ string "\\end{code}")
  blanklines
  return $ stripTrailingNewlines contents

lhsCodeBlockBird :: PandocMonad m => MarkdownParser m String
lhsCodeBlockBird = lhsCodeBlockBirdWith '>'

lhsCodeBlockInverseBird :: PandocMonad m => MarkdownParser m String
lhsCodeBlockInverseBird = lhsCodeBlockBirdWith '<'

lhsCodeBlockBirdWith :: PandocMonad m => Char -> MarkdownParser m String
lhsCodeBlockBirdWith c = try $ do
  pos <- getPosition
  when (sourceColumn pos /= 1) $ fail "Not in first column"
  lns <- many1 $ birdTrackLine c
  -- if (as is normal) there is always a space after >, drop it
  let lns' = if all (\ln -> null ln || take 1 ln == " ") lns
                then map (drop 1) lns
                else lns
  blanklines
  return $ intercalate "\n" lns'

birdTrackLine :: PandocMonad m => Char -> ParserT [Char] st m String
birdTrackLine c = try $ do
  char c
  -- allow html tags on left margin:
  when (c == '<') $ notFollowedBy letter
  anyLine

--
-- block quotes
--

emailBlockQuoteStart :: PandocMonad m => MarkdownParser m Char
emailBlockQuoteStart = try $ skipNonindentSpaces >> char '>' <* optional (char ' ')

emailBlockQuote :: PandocMonad m => MarkdownParser m [String]
emailBlockQuote = try $ do
  emailBlockQuoteStart
  let emailLine = many $ nonEndline <|> try
                         (endline >> notFollowedBy emailBlockQuoteStart >>
                         return '\n')
  let emailSep = try (newline >> emailBlockQuoteStart)
  first <- emailLine
  rest <- many $ try $ emailSep >> emailLine
  let raw = first:rest
  newline <|> (eof >> return '\n')
  optional blanklines
  return raw

blockQuote :: PandocMonad m => MarkdownParser m (F Blocks)
blockQuote = do
  raw <- emailBlockQuote
  -- parse the extracted block, which may contain various block elements:
  contents <- parseFromString' parseBlocks $ (intercalate "\n" raw) ++ "\n\n"
  return $ B.blockQuote <$> contents

--
-- list blocks
--

bulletListStart :: PandocMonad m => MarkdownParser m ()
bulletListStart = try $ do
  optional newline -- if preceded by a Plain block in a list context
  skipNonindentSpaces
  notFollowedBy' (() <$ hrule)     -- because hrules start out just like lists
  satisfy isBulletListMarker
  gobbleSpaces 1 <|> () <$ lookAhead newline
  try (gobbleAtMostSpaces 3 >> notFollowedBy spaceChar) <|> return ()

orderedListStart :: PandocMonad m
                 => Maybe (ListNumberStyle, ListNumberDelim)
                 -> MarkdownParser m (Int, ListNumberStyle, ListNumberDelim)
orderedListStart mbstydelim = try $ do
  optional newline -- if preceded by a Plain block in a list context
  skipNonindentSpaces
  notFollowedBy $ string "p." >> spaceChar >> digit  -- page number
  (do guardDisabled Ext_fancy_lists
      start <- many1 digit >>= safeRead
      char '.'
      gobbleSpaces 1 <|> () <$ lookAhead newline
      optional $ try (gobbleAtMostSpaces 3 >> notFollowedBy spaceChar)
      return (start, DefaultStyle, DefaultDelim))
   <|>
   (do (num, style, delim) <- maybe
          anyOrderedListMarker
          (\(sty,delim) -> (\start -> (start,sty,delim)) <$>
               orderedListMarker sty delim)
          mbstydelim
       gobbleSpaces 1 <|> () <$ lookAhead newline
       -- if it could be an abbreviated first name,
       -- insist on more than one space
       when (delim == Period && (style == UpperAlpha ||
            (style == UpperRoman &&
             num `elem` [1, 5, 10, 50, 100, 500, 1000]))) $
              () <$ lookAhead (newline <|> spaceChar)
       optional $ try (gobbleAtMostSpaces 3 >> notFollowedBy spaceChar)
       return (num, style, delim))

listStart :: PandocMonad m => MarkdownParser m ()
listStart = bulletListStart <|> (orderedListStart Nothing >> return ())

listLine :: PandocMonad m => Int -> MarkdownParser m String
listLine continuationIndent = try $ do
  notFollowedBy' (do gobbleSpaces continuationIndent
                     skipMany spaceChar
                     listStart)
  notFollowedByHtmlCloser
  optional (() <$ gobbleSpaces continuationIndent)
  listLineCommon

listLineCommon :: PandocMonad m => MarkdownParser m String
listLineCommon = concat <$> manyTill
              (  many1 (satisfy $ \c -> c /= '\n' && c /= '<')
             <|> liftM snd (htmlTag isCommentTag)
             <|> count 1 anyChar
              ) newline

-- parse raw text for one list item, excluding start marker and continuations
rawListItem :: PandocMonad m
            => MarkdownParser m a
            -> MarkdownParser m (String, Int)
rawListItem start = try $ do
  pos1 <- getPosition
  start
  pos2 <- getPosition
  continuationIndent <- (4 <$ guardEnabled Ext_four_space_rule)
                    <|> return (sourceColumn pos2 - sourceColumn pos1)
  first <- listLineCommon
  rest <- many (do notFollowedBy listStart
                   notFollowedBy (() <$ codeBlockFenced)
                   notFollowedBy blankline
                   listLine continuationIndent)
  blanks <- many blankline
  let result = unlines (first:rest) ++ blanks
  return (result, continuationIndent)

-- continuation of a list item - indented and separated by blankline
-- or (in compact lists) endline.
-- note: nested lists are parsed as continuations
listContinuation :: PandocMonad m => Int -> MarkdownParser m String
listContinuation continuationIndent = try $ do
  x <- try $ do
         notFollowedBy blankline
         notFollowedByHtmlCloser
         gobbleSpaces continuationIndent
         anyLineNewline
  xs <- many $ try $ do
         notFollowedBy blankline
         notFollowedByHtmlCloser
         gobbleSpaces continuationIndent <|> notFollowedBy' listStart
         anyLineNewline
  blanks <- many blankline
  return $ concat (x:xs) ++ blanks

notFollowedByHtmlCloser :: PandocMonad m => MarkdownParser m ()
notFollowedByHtmlCloser = do
  inHtmlBlock <- stateInHtmlBlock <$> getState
  case inHtmlBlock of
        Just t  -> notFollowedBy' $ htmlTag (~== TagClose t)
        Nothing -> return ()

listItem :: PandocMonad m
         => MarkdownParser m a
         -> MarkdownParser m (F Blocks)
listItem start = try $ do
  (first, continuationIndent) <- rawListItem start
  continuations <- many (listContinuation continuationIndent)
  -- parsing with ListItemState forces markers at beginning of lines to
  -- count as list item markers, even if not separated by blank space.
  -- see definition of "endline"
  state <- getState
  let oldContext = stateParserContext state
  setState $ state {stateParserContext = ListItemState}
  -- parse the extracted block, which may contain various block elements:
  let raw = concat (first:continuations)
  contents <- parseFromString' parseBlocks raw
  updateState (\st -> st {stateParserContext = oldContext})
  return contents

orderedList :: PandocMonad m => MarkdownParser m (F Blocks)
orderedList = try $ do
  (start, style, delim) <- lookAhead (orderedListStart Nothing)
  unless (style `elem` [DefaultStyle, Decimal, Example] &&
          delim `elem` [DefaultDelim, Period]) $
    guardEnabled Ext_fancy_lists
  when (style == Example) $ guardEnabled Ext_example_lists
  items <- fmap sequence $ many1 $ listItem
                 (orderedListStart (Just (style, delim)))
  start' <- (start <$ guardEnabled Ext_startnum) <|> return 1
  return $ B.orderedListWith (start', style, delim) <$> fmap compactify items

bulletList :: PandocMonad m => MarkdownParser m (F Blocks)
bulletList = do
  items <- fmap sequence $ many1 $ listItem  bulletListStart
  return $ B.bulletList <$> fmap compactify items

-- definition lists

defListMarker :: PandocMonad m => MarkdownParser m ()
defListMarker = do
  sps <- nonindentSpaces
  char ':' <|> char '~'
  tabStop <- getOption readerTabStop
  let remaining = tabStop - (length sps + 1)
  if remaining > 0
     then try (count remaining (char ' ')) <|> string "\t" <|> many1 spaceChar
     else mzero
  return ()

definitionListItem :: PandocMonad m => Bool -> MarkdownParser m (F (Inlines, [Blocks]))
definitionListItem compact = try $ do
  rawLine' <- anyLine
  raw <- many1 $ defRawBlock compact
  term <- parseFromString' (trimInlinesF <$> inlines) rawLine'
  contents <- mapM (parseFromString' parseBlocks . (++"\n")) raw
  optional blanklines
  return $ liftM2 (,) term (sequence contents)

defRawBlock :: PandocMonad m => Bool -> MarkdownParser m String
defRawBlock compact = try $ do
  hasBlank <- option False $ blankline >> return True
  defListMarker
  firstline <- anyLineNewline
  let dline = try
               ( do notFollowedBy blankline
                    notFollowedByHtmlCloser
                    if compact -- laziness not compatible with compact
                       then () <$ indentSpaces
                       else (() <$ indentSpaces)
                             <|> notFollowedBy defListMarker
                    anyLine )
  rawlines <- many dline
  cont <- liftM concat $ many $ try $ do
            trailing <- option "" blanklines
            ln <- indentSpaces >> notFollowedBy blankline >> anyLine
            lns <- many dline
            return $ trailing ++ unlines (ln:lns)
  return $ trimr (firstline ++ unlines rawlines ++ cont) ++
            if hasBlank || not (null cont) then "\n\n" else ""

definitionList :: PandocMonad m => MarkdownParser m (F Blocks)
definitionList = try $ do
  lookAhead (anyLine >>
             optional (blankline >> notFollowedBy (table >> return ())) >>
             -- don't capture table caption as def list!
             defListMarker)
  compactDefinitionList <|> normalDefinitionList

compactDefinitionList :: PandocMonad m => MarkdownParser m (F Blocks)
compactDefinitionList = do
  guardEnabled Ext_compact_definition_lists
  items <- fmap sequence $ many1 $ definitionListItem True
  return $ B.definitionList <$> fmap compactifyDL items

normalDefinitionList :: PandocMonad m => MarkdownParser m (F Blocks)
normalDefinitionList = do
  guardEnabled Ext_definition_lists
  items <- fmap sequence $ many1 $ definitionListItem False
  return $ B.definitionList <$> items

--
-- paragraph block
--

para :: PandocMonad m => MarkdownParser m (F Blocks)
para = try $ do
  exts <- getOption readerExtensions
  result <- trimInlinesF <$> inlines1
  option (B.plain <$> result)
    $ try $ do
            newline
            (blanklines >> return mempty)
              <|> (guardDisabled Ext_blank_before_blockquote >> () <$ lookAhead blockQuote)
              <|> (guardEnabled Ext_backtick_code_blocks >> () <$ lookAhead codeBlockFenced)
              <|> (guardDisabled Ext_blank_before_header >> () <$ lookAhead header)
              <|> (guardEnabled Ext_lists_without_preceding_blankline >>
                       -- Avoid creating a paragraph in a nested list.
                       notFollowedBy' inList >>
                       () <$ lookAhead listStart)
              <|> do guardEnabled Ext_native_divs
                     inHtmlBlock <- stateInHtmlBlock <$> getState
                     case inHtmlBlock of
                          Just "div" -> () <$
                                       lookAhead (htmlTag (~== TagClose "div"))
                          _          -> mzero
            return $ do
              result' <- result
              case B.toList result' of
                   [Image attr alt (src,tit)]
                     | not (null alt) &&
                       Ext_implicit_figures `extensionEnabled` exts ->
                        -- the fig: at beginning of title indicates a figure
                        return $ B.para $ B.singleton
                               $ Image attr alt (src,'f':'i':'g':':':tit)
                   _ -> return $ B.para result'

plain :: PandocMonad m => MarkdownParser m (F Blocks)
plain = fmap B.plain . trimInlinesF <$> inlines1

--
-- raw html
--

htmlElement :: PandocMonad m => MarkdownParser m String
htmlElement = rawVerbatimBlock
          <|> strictHtmlBlock
          <|> liftM snd (htmlTag isBlockTag)

htmlBlock :: PandocMonad m => MarkdownParser m (F Blocks)
htmlBlock = do
  guardEnabled Ext_raw_html
  try (do
      (TagOpen t attrs) <- lookAhead $ fst <$> htmlTag isBlockTag
      (guard (t `elem` ["pre","style","script"]) >>
          (return . B.rawBlock "html") <$> rawVerbatimBlock)
        <|> (do guardEnabled Ext_markdown_attribute
                oldMarkdownAttribute <- stateMarkdownAttribute <$> getState
                markdownAttribute <-
                   case lookup "markdown" attrs of
                        Just "0" -> False <$ updateState (\st -> st{
                                       stateMarkdownAttribute = False })
                        Just _   -> True <$ updateState (\st -> st{
                                       stateMarkdownAttribute = True })
                        Nothing  -> return oldMarkdownAttribute
                res <- if markdownAttribute
                          then rawHtmlBlocks
                          else htmlBlock'
                updateState $ \st -> st{ stateMarkdownAttribute =
                                         oldMarkdownAttribute }
                return res)
        <|> (guardEnabled Ext_markdown_in_html_blocks >> rawHtmlBlocks))
    <|> htmlBlock'

htmlBlock' :: PandocMonad m => MarkdownParser m (F Blocks)
htmlBlock' = try $ do
    first <- htmlElement
    skipMany spaceChar
    optional blanklines
    return $ if null first
                then mempty
                else return $ B.rawBlock "html" first

strictHtmlBlock :: PandocMonad m => MarkdownParser m String
strictHtmlBlock = htmlInBalanced (not . isInlineTag)

rawVerbatimBlock :: PandocMonad m => MarkdownParser m String
rawVerbatimBlock = htmlInBalanced isVerbTag
  where isVerbTag (TagOpen "pre" _)    = True
        isVerbTag (TagOpen "style" _)  = True
        isVerbTag (TagOpen "script" _) = True
        isVerbTag _                    = False

rawTeXBlock :: PandocMonad m => MarkdownParser m (F Blocks)
rawTeXBlock = do
  guardEnabled Ext_raw_tex
  result <- (B.rawBlock "context" . concat <$>
                  rawConTeXtEnvironment `sepEndBy1` blankline)
        <|> (B.rawBlock "latex" . concat <$>
                  rawLaTeXBlock `sepEndBy1` blankline)

  optional blanklines
  return $ return result

rawHtmlBlocks :: PandocMonad m => MarkdownParser m (F Blocks)
rawHtmlBlocks = do
  (TagOpen tagtype _, raw) <- htmlTag isBlockTag
  -- we don't want '<td>    text' to be a code block:
  skipMany spaceChar
  indentlevel <- (blankline >> length <$> many (char ' ')) <|> return 0
  -- try to find closing tag
  -- we set stateInHtmlBlock so that closing tags that can be either block or
  -- inline will not be parsed as inline tags
  oldInHtmlBlock <- stateInHtmlBlock <$> getState
  updateState $ \st -> st{ stateInHtmlBlock = Just tagtype }
  let closer = htmlTag (\x -> x ~== TagClose tagtype)
  let block' = do notFollowedBy' closer
                  gobbleAtMostSpaces indentlevel
                  block
  contents <- mconcat <$> many block'
  result <-
    (closer >>= \(_, rawcloser) -> return (
                return (B.rawBlock "html" $ stripMarkdownAttribute raw) <>
                contents <>
                return (B.rawBlock "html" rawcloser)))
      <|> return (return (B.rawBlock "html" raw) <> contents)
  updateState $ \st -> st{ stateInHtmlBlock = oldInHtmlBlock }
  return result

-- remove markdown="1" attribute
stripMarkdownAttribute :: String -> String
stripMarkdownAttribute s = renderTags' $ map filterAttrib $ parseTags s
  where filterAttrib (TagOpen t as) = TagOpen t
                                        [(k,v) | (k,v) <- as, k /= "markdown"]
        filterAttrib              x = x

--
-- line block
--

lineBlock :: PandocMonad m => MarkdownParser m (F Blocks)
lineBlock = try $ do
  guardEnabled Ext_line_blocks
  lines' <- lineBlockLines >>=
            mapM (parseFromString' (trimInlinesF <$> inlines))
  return $ B.lineBlock <$> sequence lines'

--
-- Tables
--

-- Parse a dashed line with optional trailing spaces; return its length
-- and the length including trailing space.
dashedLine :: PandocMonad m
           => Char
           -> ParserT [Char] st m (Int, Int)
dashedLine ch = do
  dashes <- many1 (char ch)
  sp     <- many spaceChar
  let lengthDashes = length dashes
      lengthSp     = length sp
  return (lengthDashes, lengthDashes + lengthSp)

-- Parse a table header with dashed lines of '-' preceded by
-- one (or zero) line of text.
simpleTableHeader :: PandocMonad m
                  => Bool  -- ^ Headerless table
                  -> MarkdownParser m (F [Blocks], [Alignment], [Int])
simpleTableHeader headless = try $ do
  rawContent  <- if headless
                    then return ""
                    else anyLine
  initSp      <- nonindentSpaces
  dashes      <- many1 (dashedLine '-')
  newline
  let (lengths, lines') = unzip dashes
  let indices  = scanl (+) (length initSp) lines'
  -- If no header, calculate alignment on basis of first row of text
  rawHeads <- liftM (tail . splitStringByIndices (init indices)) $
              if headless
                 then lookAhead anyLine
                 else return rawContent
  let aligns   = zipWith alignType (map (\a -> [a]) rawHeads) lengths
  let rawHeads' = if headless
                     then replicate (length dashes) ""
                     else rawHeads
  heads <- fmap sequence
           $ mapM (parseFromString' (mconcat <$> many plain))
           $ map trim rawHeads'
  return (heads, aligns, indices)

-- Returns an alignment type for a table, based on a list of strings
-- (the rows of the column header) and a number (the length of the
-- dashed line under the rows.
alignType :: [String]
          -> Int
          -> Alignment
alignType [] _ = AlignDefault
alignType strLst len =
  let nonempties = filter (not . null) $ map trimr strLst
      (leftSpace, rightSpace) =
           case sortBy (comparing length) nonempties of
                 (x:_) -> (head x `elem` " \t", length x < len)
                 []    -> (False, False)
  in  case (leftSpace, rightSpace) of
        (True,  False) -> AlignRight
        (False, True)  -> AlignLeft
        (True,  True)  -> AlignCenter
        (False, False) -> AlignDefault

-- Parse a table footer - dashed lines followed by blank line.
tableFooter :: PandocMonad m => MarkdownParser m String
tableFooter = try $ skipNonindentSpaces >> many1 (dashedLine '-') >> blanklines

-- Parse a table separator - dashed line.
tableSep :: PandocMonad m => MarkdownParser m Char
tableSep = try $ skipNonindentSpaces >> many1 (dashedLine '-') >> char '\n'

-- Parse a raw line and split it into chunks by indices.
rawTableLine :: PandocMonad m
             => [Int]
             -> MarkdownParser m [String]
rawTableLine indices = do
  notFollowedBy' (blanklines <|> tableFooter)
  line <- many1Till anyChar newline
  return $ map trim $ tail $
           splitStringByIndices (init indices) line

-- Parse a table line and return a list of lists of blocks (columns).
tableLine :: PandocMonad m
          => [Int]
          -> MarkdownParser m (F [Blocks])
tableLine indices = rawTableLine indices >>=
  fmap sequence . mapM (parseFromString' (mconcat <$> many plain))

-- Parse a multiline table row and return a list of blocks (columns).
multilineRow :: PandocMonad m
             => [Int]
             -> MarkdownParser m (F [Blocks])
multilineRow indices = do
  colLines <- many1 (rawTableLine indices)
  let cols = map unlines $ transpose colLines
  fmap sequence $ mapM (parseFromString' (mconcat <$> many plain)) cols

-- Parses a table caption:  inlines beginning with 'Table:'
-- and followed by blank lines.
tableCaption :: PandocMonad m => MarkdownParser m (F Inlines)
tableCaption = try $ do
  guardEnabled Ext_table_captions
  skipNonindentSpaces
  string ":" <|> string "Table:"
  trimInlinesF <$> inlines1 <* blanklines

-- Parse a simple table with '---' header and one line per row.
simpleTable :: PandocMonad m
            => Bool  -- ^ Headerless table
            -> MarkdownParser m ([Alignment], [Double], F [Blocks], F [[Blocks]])
simpleTable headless = do
  (aligns, _widths, heads', lines') <-
       tableWith (simpleTableHeader headless) tableLine
              (return ())
              (if headless then tableFooter else tableFooter <|> blanklines)
  -- Simple tables get 0s for relative column widths (i.e., use default)
  return (aligns, replicate (length aligns) 0, heads', lines')

-- Parse a multiline table:  starts with row of '-' on top, then header
-- (which may be multiline), then the rows,
-- which may be multiline, separated by blank lines, and
-- ending with a footer (dashed line followed by blank line).
multilineTable :: PandocMonad m
               => Bool -- ^ Headerless table
               -> MarkdownParser m ([Alignment], [Double], F [Blocks], F [[Blocks]])
multilineTable headless =
  tableWith (multilineTableHeader headless) multilineRow blanklines tableFooter

multilineTableHeader :: PandocMonad m
                     => Bool -- ^ Headerless table
                     -> MarkdownParser m (F [Blocks], [Alignment], [Int])
multilineTableHeader headless = try $ do
  unless headless $
     tableSep >> notFollowedBy blankline
  rawContent  <- if headless
                    then return $ repeat ""
                    else many1 $ notFollowedBy tableSep >> anyLine
  initSp      <- nonindentSpaces
  dashes      <- many1 (dashedLine '-')
  newline
  let (lengths, lines') = unzip dashes
  let indices  = scanl (+) (length initSp) lines'
  rawHeadsList <- if headless
                     then liftM (map (:[]) . tail .
                              splitStringByIndices (init indices)) $ lookAhead anyLine
                     else return $ transpose $ map
                           (tail . splitStringByIndices (init indices))
                           rawContent
  let aligns   = zipWith alignType rawHeadsList lengths
  let rawHeads = if headless
                    then replicate (length dashes) ""
                    else map (unlines . map trim) rawHeadsList
  heads <- fmap sequence $
           mapM (parseFromString' (mconcat <$> many plain)) $
             map trim rawHeads
  return (heads, aligns, indices)

-- Parse a grid table:  starts with row of '-' on top, then header
-- (which may be grid), then the rows,
-- which may be grid, separated by blank lines, and
-- ending with a footer (dashed line followed by blank line).
gridTable :: PandocMonad m => Bool -- ^ Headerless table
          -> MarkdownParser m ([Alignment], [Double], F [Blocks], F [[Blocks]])
gridTable headless = gridTableWith' parseBlocks headless

pipeBreak :: PandocMonad m => MarkdownParser m ([Alignment], [Int])
pipeBreak = try $ do
  nonindentSpaces
  openPipe <- (True <$ char '|') <|> return False
  first <- pipeTableHeaderPart
  rest <- many $ sepPipe *> pipeTableHeaderPart
  -- surrounding pipes needed for a one-column table:
  guard $ not (null rest && not openPipe)
  optional (char '|')
  blankline
  return $ unzip (first:rest)

pipeTable :: PandocMonad m => MarkdownParser m ([Alignment], [Double], F [Blocks], F [[Blocks]])
pipeTable = try $ do
  nonindentSpaces
  lookAhead nonspaceChar
  (heads,(aligns, seplengths)) <- (,) <$> pipeTableRow <*> pipeBreak
  let heads' = take (length aligns) <$> heads
  lines' <- many pipeTableRow
  let lines'' = map (take (length aligns) <$>) lines'
  let maxlength = maximum $
       map (\x -> length . stringify $ runF x def) (heads' : lines'')
  numColumns <- getOption readerColumns
  let widths = if maxlength > numColumns
                  then map (\len ->
                           fromIntegral (len + 1) / fromIntegral numColumns)
                             seplengths
                  else replicate (length aligns) 0.0
  return $ (aligns, widths, heads', sequence lines'')

sepPipe :: PandocMonad m => MarkdownParser m ()
sepPipe = try $ do
  char '|' <|> char '+'
  notFollowedBy blankline

-- parse a row, also returning probable alignments for org-table cells
pipeTableRow :: PandocMonad m => MarkdownParser m (F [Blocks])
pipeTableRow = try $ do
  scanForPipe
  skipMany spaceChar
  openPipe <- (True <$ char '|') <|> return False
  -- split into cells
  let chunk = void (code <|> math <|> rawHtmlInline <|> escapedChar <|> rawLaTeXInline')
       <|> void (noneOf "|\n\r")
  let cellContents = ((trim . snd) <$> withRaw (many chunk)) >>=
        parseFromString' pipeTableCell
  cells <- cellContents `sepEndBy1` (char '|')
  -- surrounding pipes needed for a one-column table:
  guard $ not (length cells == 1 && not openPipe)
  blankline
  return $ sequence cells

pipeTableCell :: PandocMonad m => MarkdownParser m (F Blocks)
pipeTableCell =
  (do result <- inlines1
      return $ B.plain <$> result)
    <|> return mempty

pipeTableHeaderPart :: PandocMonad m => ParserT [Char] st m (Alignment, Int)
pipeTableHeaderPart = try $ do
  skipMany spaceChar
  left <- optionMaybe (char ':')
  pipe <- many1 (char '-')
  right <- optionMaybe (char ':')
  skipMany spaceChar
  let len = length pipe + maybe 0 (const 1) left + maybe 0 (const 1) right
  return $
    ((case (left,right) of
       (Nothing,Nothing) -> AlignDefault
       (Just _,Nothing)  -> AlignLeft
       (Nothing,Just _)  -> AlignRight
       (Just _,Just _)   -> AlignCenter), len)

-- Succeed only if current line contains a pipe.
scanForPipe :: PandocMonad m => ParserT [Char] st m ()
scanForPipe = do
  inp <- getInput
  case break (\c -> c == '\n' || c == '|') inp of
       (_,'|':_) -> return ()
       _         -> mzero

-- | Parse a table using 'headerParser', 'rowParser',
-- 'lineParser', and 'footerParser'.  Variant of the version in
-- Text.Pandoc.Parsing.
tableWith :: PandocMonad m
          => MarkdownParser m (F [Blocks], [Alignment], [Int])
          -> ([Int] -> MarkdownParser m (F [Blocks]))
          -> MarkdownParser m sep
          -> MarkdownParser m end
          -> MarkdownParser m ([Alignment], [Double], F [Blocks], F [[Blocks]])
tableWith headerParser rowParser lineParser footerParser = try $ do
    (heads, aligns, indices) <- headerParser
    lines' <- fmap sequence $ rowParser indices `sepEndBy1` lineParser
    footerParser
    numColumns <- getOption readerColumns
    let widths = if (indices == [])
                    then replicate (length aligns) 0.0
                    else widthsFromIndices numColumns indices
    return $ (aligns, widths, heads, lines')

table :: PandocMonad m => MarkdownParser m (F Blocks)
table = try $ do
  frontCaption <- option Nothing (Just <$> tableCaption)
  (aligns, widths, heads, lns) <-
         try (guardEnabled Ext_pipe_tables >> scanForPipe >> pipeTable) <|>
         try (guardEnabled Ext_multiline_tables >>
                multilineTable False) <|>
         try (guardEnabled Ext_simple_tables >>
                (simpleTable True <|> simpleTable False)) <|>
         try (guardEnabled Ext_multiline_tables >>
                multilineTable True) <|>
         try (guardEnabled Ext_grid_tables >>
                (gridTable False <|> gridTable True)) <?> "table"
  optional blanklines
  caption <- case frontCaption of
                  Nothing -> option (return mempty) tableCaption
                  Just c  -> return c
  -- renormalize widths if greater than 100%:
  let totalWidth = sum widths
  let widths' = if totalWidth < 1
                   then widths
                   else map (/ totalWidth) widths
  return $ do
    caption' <- caption
    heads' <- heads
    lns' <- lns
    return $ B.table caption' (zip aligns widths') heads' lns'

--
-- inline
--

inlines :: PandocMonad m => MarkdownParser m (F Inlines)
inlines = mconcat <$> many inline

inlines1 :: PandocMonad m => MarkdownParser m (F Inlines)
inlines1 = mconcat <$> many1 inline

inline :: PandocMonad m => MarkdownParser m (F Inlines)
inline = choice [ whitespace
                , bareURL
                , str
                , endline
                , code
                , strongOrEmph
                , note
                , cite
                , bracketedSpan
                , link
                , image
                , math
                , strikeout
                , subscript
                , superscript
                , inlineNote  -- after superscript because of ^[link](/foo)^
                , autoLink
                , spanHtml
                , rawHtmlInline
                , escapedNewline
                , escapedChar
                , rawLaTeXInline'
                , exampleRef
                , smart
                , return . B.singleton <$> charRef
                , emoji
                , symbol
                , ltSign
                ] <?> "inline"

escapedChar' :: PandocMonad m => MarkdownParser m Char
escapedChar' = try $ do
  char '\\'
  (guardEnabled Ext_all_symbols_escapable >> satisfy (not . isAlphaNum))
     <|> (guardEnabled Ext_angle_brackets_escapable >>
            oneOf "\\`*_{}[]()>#+-.!~\"<>")
     <|> oneOf "\\`*_{}[]()>#+-.!~\""

escapedNewline :: PandocMonad m => MarkdownParser m (F Inlines)
escapedNewline = try $ do
  guardEnabled Ext_escaped_line_breaks
  char '\\'
  lookAhead (char '\n') -- don't consume the newline (see #3730)
  return $ return B.linebreak

escapedChar :: PandocMonad m => MarkdownParser m (F Inlines)
escapedChar = do
  result <- escapedChar'
  case result of
       ' '   -> return $ return $ B.str "\160" -- "\ " is a nonbreaking space
       _     -> return $ return $ B.str [result]

ltSign :: PandocMonad m => MarkdownParser m (F Inlines)
ltSign = do
  guardDisabled Ext_raw_html
    <|> (notFollowedByHtmlCloser >> notFollowedBy' (htmlTag isBlockTag))
  char '<'
  return $ return $ B.str "<"

exampleRef :: PandocMonad m => MarkdownParser m (F Inlines)
exampleRef = try $ do
  guardEnabled Ext_example_lists
  char '@'
  lab <- many1 (alphaNum <|> oneOf "-_")
  return $ do
    st <- askF
    return $ case M.lookup lab (stateExamples st) of
                  Just n  -> B.str (show n)
                  Nothing -> B.str ('@':lab)

symbol :: PandocMonad m => MarkdownParser m (F Inlines)
symbol = do
  result <- noneOf "<\\\n\t "
         <|> try (do lookAhead $ char '\\'
                     notFollowedBy' (() <$ rawTeXBlock)
                     char '\\')
  return $ return $ B.str [result]

-- parses inline code, between n `s and n `s
code :: PandocMonad m => MarkdownParser m (F Inlines)
code = try $ do
  starts <- many1 (char '`')
  skipSpaces
  result <- (trim . concat) <$>
            many1Till (many1 (noneOf "`\n") <|> many1 (char '`') <|>
                       (char '\n' >> notFollowedBy' blankline >> return " "))
                      (try (skipSpaces >> count (length starts) (char '`') >>
                      notFollowedBy (char '`')))
  rawattr <-
     (Left <$> try (guardEnabled Ext_raw_attribute >> rawAttribute))
    <|>
     (Right <$> option ("",[],[])
         (try (guardEnabled Ext_inline_code_attributes >> attributes)))
  return $ return $
    case rawattr of
         Left syn   -> B.rawInline syn result
         Right attr -> B.codeWith attr result

math :: PandocMonad m => MarkdownParser m (F Inlines)
math =  (return . B.displayMath <$> (mathDisplay >>= applyMacros))
     <|> (return . B.math <$> (mathInline >>= applyMacros)) <+?>
               (guardEnabled Ext_smart *> (return <$> apostrophe)
                <* notFollowedBy (space <|> satisfy isPunctuation))

-- Parses material enclosed in *s, **s, _s, or __s.
-- Designed to avoid backtracking.
enclosure :: PandocMonad m
          => Char
          -> MarkdownParser m (F Inlines)
enclosure c = do
  -- we can't start an enclosure with _ if after a string and
  -- the intraword_underscores extension is enabled:
  guardDisabled Ext_intraword_underscores
    <|> guard (c == '*')
    <|> (guard =<< notAfterString)
  cs <- many1 (char c)
  (return (B.str cs) <>) <$> whitespace
    <|> do
        case length cs of
             3 -> three c
             2 -> two   c mempty
             1 -> one   c mempty
             _ -> return (return $ B.str cs)

ender :: PandocMonad m => Char -> Int -> MarkdownParser m ()
ender c n = try $ do
  count n (char c)
  guard (c == '*')
    <|> guardDisabled Ext_intraword_underscores
    <|> notFollowedBy alphaNum

-- Parse inlines til you hit one c or a sequence of two cs.
-- If one c, emit emph and then parse two.
-- If two cs, emit strong and then parse one.
-- Otherwise, emit ccc then the results.
three :: PandocMonad m => Char -> MarkdownParser m (F Inlines)
three c = do
  contents <- mconcat <$> many (notFollowedBy (ender c 1) >> inline)
  (ender c 3 >> updateLastStrPos >> return ((B.strong . B.emph) <$> contents))
    <|> (ender c 2 >> updateLastStrPos >> one c (B.strong <$> contents))
    <|> (ender c 1 >> updateLastStrPos >> two c (B.emph <$> contents))
    <|> return (return (B.str [c,c,c]) <> contents)

-- Parse inlines til you hit two c's, and emit strong.
-- If you never do hit two cs, emit ** plus inlines parsed.
two :: PandocMonad m => Char -> F Inlines -> MarkdownParser m (F Inlines)
two c prefix' = do
  contents <- mconcat <$> many (try $ notFollowedBy (ender c 2) >> inline)
  (ender c 2 >> updateLastStrPos >>
                return (B.strong <$> (prefix' <> contents)))
    <|> return (return (B.str [c,c]) <> (prefix' <> contents))

-- Parse inlines til you hit a c, and emit emph.
-- If you never hit a c, emit * plus inlines parsed.
one :: PandocMonad m => Char -> F Inlines -> MarkdownParser m (F Inlines)
one c prefix' = do
  contents <- mconcat <$> many (  (notFollowedBy (ender c 1) >> inline)
                           <|> try (string [c,c] >>
                                    notFollowedBy (ender c 1) >>
                                    two c mempty) )
  (ender c 1 >> updateLastStrPos >> return (B.emph <$> (prefix' <> contents)))
    <|> return (return (B.str [c]) <> (prefix' <> contents))

strongOrEmph :: PandocMonad m => MarkdownParser m (F Inlines)
strongOrEmph =  enclosure '*' <|> enclosure '_'

-- | Parses a list of inlines between start and end delimiters.
inlinesBetween :: PandocMonad m
               => (Show b)
               => MarkdownParser m a
               -> MarkdownParser m b
               -> MarkdownParser m (F Inlines)
inlinesBetween start end =
  (trimInlinesF . mconcat) <$> try (start >> many1Till inner end)
    where inner      = innerSpace <|> (notFollowedBy' (() <$ whitespace) >> inline)
          innerSpace = try $ whitespace <* notFollowedBy' end

strikeout :: PandocMonad m => MarkdownParser m (F Inlines)
strikeout = fmap B.strikeout <$>
 (guardEnabled Ext_strikeout >> inlinesBetween strikeStart strikeEnd)
    where strikeStart = string "~~" >> lookAhead nonspaceChar
                        >> notFollowedBy (char '~')
          strikeEnd   = try $ string "~~"

superscript :: PandocMonad m => MarkdownParser m (F Inlines)
superscript = fmap B.superscript <$> try (do
  guardEnabled Ext_superscript
  char '^'
  mconcat <$> many1Till (notFollowedBy spaceChar >> inline) (char '^'))

subscript :: PandocMonad m => MarkdownParser m (F Inlines)
subscript = fmap B.subscript <$> try (do
  guardEnabled Ext_subscript
  char '~'
  mconcat <$> many1Till (notFollowedBy spaceChar >> inline) (char '~'))

whitespace :: PandocMonad m => MarkdownParser m (F Inlines)
whitespace = spaceChar >> return <$> (lb <|> regsp) <?> "whitespace"
  where lb = spaceChar >> skipMany spaceChar >> option B.space (endline >> return B.linebreak)
        regsp = skipMany spaceChar >> return B.space

nonEndline :: PandocMonad m => ParserT [Char] st m Char
nonEndline = satisfy (/='\n')

str :: PandocMonad m => MarkdownParser m (F Inlines)
str = do
  result <- many1 (alphaNum <|> try (char '.' <* notFollowedBy (char '.')))
  updateLastStrPos
  (do guardEnabled Ext_smart
      abbrevs <- getOption readerAbbreviations
      if not (null result) && last result == '.' && result `Set.member` abbrevs
         then try (do ils <- whitespace <|> endline
                      lookAhead alphaNum
                      return $ do
                        ils' <- ils
                        if ils' == B.space
                           then return (B.str result <> B.str "\160")
                           else -- linebreak or softbreak
                                return (ils' <> B.str result <> B.str "\160"))
                <|> return (return (B.str result))
         else return (return (B.str result)))
     <|> return (return (B.str result))

-- an endline character that can be treated as a space, not a structural break
endline :: PandocMonad m => MarkdownParser m (F Inlines)
endline = try $ do
  newline
  notFollowedBy blankline
  -- parse potential list-starts differently if in a list:
  notFollowedBy (inList >> listStart)
  guardDisabled Ext_lists_without_preceding_blankline <|> notFollowedBy listStart
  guardEnabled Ext_blank_before_blockquote <|> notFollowedBy emailBlockQuoteStart
  guardEnabled Ext_blank_before_header <|> (notFollowedBy . char =<< atxChar) -- atx header
  guardDisabled Ext_backtick_code_blocks <|>
     notFollowedBy (() <$ (lookAhead (char '`') >> codeBlockFenced))
  guardDisabled Ext_fenced_divs <|>
    do divLevel <- stateFencedDivLevel <$> getState
       guard (divLevel < 1) <|> notFollowedBy fenceEnd
  notFollowedByHtmlCloser
  (eof >> return mempty)
    <|> (guardEnabled Ext_hard_line_breaks >> return (return B.linebreak))
    <|> (guardEnabled Ext_ignore_line_breaks >> return mempty)
    <|> (skipMany spaceChar >> return (return B.softbreak))

--
-- links
--

-- a reference label for a link
reference :: PandocMonad m => MarkdownParser m (F Inlines, String)
reference = do
  guardDisabled Ext_footnotes <|> notFollowedBy' (string "[^")
  guardDisabled Ext_citations <|> notFollowedBy' (string "[@")
  withRaw $ trimInlinesF <$> inlinesInBalancedBrackets

parenthesizedChars :: PandocMonad m => MarkdownParser m [Char]
parenthesizedChars = do
  result <- charsInBalanced '(' ')' litChar
  return $ '(' : result ++ ")"

-- source for a link, with optional title
source :: PandocMonad m => MarkdownParser m (String, String)
source = do
  char '('
  skipSpaces
  let urlChunk =
            try parenthesizedChars
        <|> (notFollowedBy (oneOf " )") >> (count 1 litChar))
        <|> try (many1 spaceChar <* notFollowedBy (oneOf "\"')"))
  let sourceURL = (unwords . words . concat) <$> many urlChunk
  let betweenAngles = try $
         char '<' >> manyTill litChar (char '>')
  src <- try betweenAngles <|> sourceURL
  tit <- option "" $ try $ spnl >> linkTitle
  skipSpaces
  char ')'
  return (escapeURI $ trimr src, tit)

linkTitle :: PandocMonad m => MarkdownParser m String
linkTitle = quotedTitle '"' <|> quotedTitle '\''

link :: PandocMonad m => MarkdownParser m (F Inlines)
link = try $ do
  st <- getState
  guard $ stateAllowLinks st
  setState $ st{ stateAllowLinks = False }
  (lab,raw) <- reference
  setState $ st{ stateAllowLinks = True }
  regLink B.linkWith lab <|> referenceLink B.linkWith (lab,raw)

bracketedSpan :: PandocMonad m => MarkdownParser m (F Inlines)
bracketedSpan = try $ do
  guardEnabled Ext_bracketed_spans
  (lab,_) <- reference
  attr <- attributes
  return $ if isSmallCaps attr
              then B.smallcaps <$> lab
              else B.spanWith attr <$> lab

-- | We treat a span as SmallCaps if class is "smallcaps" (and
-- no other attributes are set or if style is "font-variant:small-caps"
-- (and no other attributes are set).
isSmallCaps :: Attr -> Bool
isSmallCaps ("",["smallcaps"],[]) = True
isSmallCaps ("",[],kvs) =
  case lookup "style" kvs of
       Just s -> map toLower (filter (`notElem` " \t;") s) ==
                    "font-variant:small-caps"
       Nothing -> False
isSmallCaps _ = False

regLink :: PandocMonad m
        => (Attr -> String -> String -> Inlines -> Inlines)
        -> F Inlines
        -> MarkdownParser m (F Inlines)
regLink constructor lab = try $ do
  (src, tit) <- source
  attr <- option nullAttr $
          guardEnabled Ext_link_attributes >> attributes
  return $ constructor attr src tit <$> lab

-- a link like [this][ref] or [this][] or [this]
referenceLink :: PandocMonad m
              => (Attr -> String -> String -> Inlines -> Inlines)
              -> (F Inlines, String)
              -> MarkdownParser m (F Inlines)
referenceLink constructor (lab, raw) = do
  sp <- (True <$ lookAhead (char ' ')) <|> return False
  (_,raw') <- option (mempty, "") $
      lookAhead (try (do guardEnabled Ext_citations
                         guardDisabled Ext_spaced_reference_links <|> spnl
                         normalCite
                         return (mempty, "")))
      <|>
      try ((guardDisabled Ext_spaced_reference_links <|> spnl) >> reference)
  when (raw' == "") $ guardEnabled Ext_shortcut_reference_links
  let labIsRef = raw' == "" || raw' == "[]"
  let key = toKey $ if labIsRef then raw else raw'
  parsedRaw <- parseFromString' inlines raw'
  fallback  <- parseFromString' inlines $ dropBrackets raw
  implicitHeaderRefs <- option False $
                         True <$ guardEnabled Ext_implicit_header_references
  let makeFallback = do
       parsedRaw' <- parsedRaw
       fallback' <- fallback
       return $ B.str "[" <> fallback' <> B.str "]" <>
                (if sp && not (null raw) then B.space else mempty) <>
                parsedRaw'
  return $ do
    keys <- asksF stateKeys
    case M.lookup key keys of
       Nothing        ->
         if implicitHeaderRefs
            then do
              headerKeys <- asksF stateHeaderKeys
              case M.lookup key headerKeys of
                   Just ((src, tit), _) -> constructor nullAttr src tit <$> lab
                   Nothing              -> makeFallback
            else makeFallback
       Just ((src,tit), attr) -> constructor attr src tit <$> lab

dropBrackets :: String -> String
dropBrackets = reverse . dropRB . reverse . dropLB
  where dropRB (']':xs) = xs
        dropRB xs       = xs
        dropLB ('[':xs) = xs
        dropLB xs       = xs

bareURL :: PandocMonad m => MarkdownParser m (F Inlines)
bareURL = try $ do
  guardEnabled Ext_autolink_bare_uris
  getState >>= guard . stateAllowLinks
  (orig, src) <- uri <|> emailAddress
  notFollowedBy $ try $ spaces >> htmlTag (~== TagClose "a")
  return $ return $ B.link src "" (B.str orig)

autoLink :: PandocMonad m => MarkdownParser m (F Inlines)
autoLink = try $ do
  getState >>= guard . stateAllowLinks
  char '<'
  (orig, src) <- uri <|> emailAddress
  -- in rare cases, something may remain after the uri parser
  -- is finished, because the uri parser tries to avoid parsing
  -- final punctuation.  for example:  in `<http://hi---there>`,
  -- the URI parser will stop before the dashes.
  extra <- fromEntities <$> manyTill nonspaceChar (char '>')
  attr  <- option nullAttr $ try $
            guardEnabled Ext_link_attributes >> attributes
  return $ return $ B.linkWith attr (src ++ escapeURI extra) "" (B.str $ orig ++ extra)

image :: PandocMonad m => MarkdownParser m (F Inlines)
image = try $ do
  char '!'
  (lab,raw) <- reference
  defaultExt <- getOption readerDefaultImageExtension
  let constructor attr' src = case takeExtension src of
                          "" -> B.imageWith attr' (addExtension src defaultExt)
                          _  -> B.imageWith attr' src
  regLink constructor lab <|> referenceLink constructor (lab,raw)

note :: PandocMonad m => MarkdownParser m (F Inlines)
note = try $ do
  guardEnabled Ext_footnotes
  ref <- noteMarker
  updateState $ \st -> st{ stateNoteRefs = Set.insert ref (stateNoteRefs st) }
  return $ do
    notes <- asksF stateNotes'
    case M.lookup ref notes of
        Nothing       -> return $ B.str $ "[^" ++ ref ++ "]"
        Just (_pos, contents) -> do
          st <- askF
          -- process the note in a context that doesn't resolve
          -- notes, to avoid infinite looping with notes inside
          -- notes:
          let contents' = runF contents st{ stateNotes' = M.empty }
          return $ B.note contents'

inlineNote :: PandocMonad m => MarkdownParser m (F Inlines)
inlineNote = try $ do
  guardEnabled Ext_inline_notes
  char '^'
  contents <- inlinesInBalancedBrackets
  return $ B.note . B.para <$> contents

rawLaTeXInline' :: PandocMonad m => MarkdownParser m (F Inlines)
rawLaTeXInline' = try $ do
  guardEnabled Ext_raw_tex
  lookAhead (char '\\')
  notFollowedBy' rawConTeXtEnvironment
  s <- rawLaTeXInline
  return $ return $ B.rawInline "tex" s -- "tex" because it might be context

rawConTeXtEnvironment :: PandocMonad m => ParserT [Char] st m String
rawConTeXtEnvironment = try $ do
  string "\\start"
  completion <- inBrackets (letter <|> digit <|> spaceChar)
               <|> (many1 letter)
  contents <- manyTill (rawConTeXtEnvironment <|> (count 1 anyChar))
                       (try $ string "\\stop" >> string completion)
  return $ "\\start" ++ completion ++ concat contents ++ "\\stop" ++ completion

inBrackets :: PandocMonad m => (ParserT [Char] st m Char) -> ParserT [Char] st m String
inBrackets parser = do
  char '['
  contents <- many parser
  char ']'
  return $ "[" ++ contents ++ "]"

spanHtml :: PandocMonad m => MarkdownParser m (F Inlines)
spanHtml = try $ do
  guardEnabled Ext_native_spans
  (TagOpen _ attrs, _) <- htmlTag (~== TagOpen "span" [])
  contents <- mconcat <$> manyTill inline (htmlTag (~== TagClose "span"))
  let ident = fromMaybe "" $ lookup "id" attrs
  let classes = maybe [] words $ lookup "class" attrs
  let keyvals = [(k,v) | (k,v) <- attrs, k /= "id" && k /= "class"]
  return $ if isSmallCaps (ident, classes, keyvals)
              then B.smallcaps <$> contents
              else B.spanWith (ident, classes, keyvals) <$> contents

divHtml :: PandocMonad m => MarkdownParser m (F Blocks)
divHtml = try $ do
  guardEnabled Ext_native_divs
  (TagOpen _ attrs, rawtag) <- htmlTag (~== TagOpen "div" [])
  -- we set stateInHtmlBlock so that closing tags that can be either block or
  -- inline will not be parsed as inline tags
  oldInHtmlBlock <- stateInHtmlBlock <$> getState
  updateState $ \st -> st{ stateInHtmlBlock = Just "div" }
  bls <- option "" (blankline >> option "" blanklines)
  contents <- mconcat <$>
              many (notFollowedBy' (htmlTag (~== TagClose "div")) >> block)
  closed <- option False (True <$ htmlTag (~== TagClose "div"))
  if closed
     then do
       updateState $ \st -> st{ stateInHtmlBlock = oldInHtmlBlock }
       let ident = fromMaybe "" $ lookup "id" attrs
       let classes = maybe [] words $ lookup "class" attrs
       let keyvals = [(k,v) | (k,v) <- attrs, k /= "id" && k /= "class"]
       return $ B.divWith (ident, classes, keyvals) <$> contents
     else -- avoid backtracing
       return $ return (B.rawBlock "html" (rawtag <> bls)) <> contents

divFenced :: PandocMonad m => MarkdownParser m (F Blocks)
divFenced = try $ do
  guardEnabled Ext_fenced_divs
  nonindentSpaces
  string ":::"
  skipMany (char ':')
  skipMany spaceChar
  attribs <- attributes <|> ((\x -> ("",[x],[])) <$> many1 nonspaceChar)
  skipMany spaceChar
  skipMany (char ':')
  blankline
  updateState $ \st -> st{ stateFencedDivLevel = stateFencedDivLevel st + 1 }
  bs <- mconcat <$> manyTill block fenceEnd
  updateState $ \st -> st{ stateFencedDivLevel = stateFencedDivLevel st - 1 }
  return $ B.divWith attribs <$> bs

fenceEnd :: PandocMonad m => MarkdownParser m ()
fenceEnd = try $ do
  nonindentSpaces
  string ":::"
  skipMany (char ':')
  blanklines
  return ()

rawHtmlInline :: PandocMonad m => MarkdownParser m (F Inlines)
rawHtmlInline = do
  guardEnabled Ext_raw_html
  inHtmlBlock <- stateInHtmlBlock <$> getState
  let isCloseBlockTag t = case inHtmlBlock of
                               Just t' -> t ~== TagClose t'
                               Nothing -> False
  mdInHtml <- option False $
                (    guardEnabled Ext_markdown_in_html_blocks
                 <|> guardEnabled Ext_markdown_attribute
                ) >> return True
  (_,result) <- htmlTag $ if mdInHtml
                             then (\x -> isInlineTag x &&
                                         not (isCloseBlockTag x))
                             else not . isTextTag
  return $ return $ B.rawInline "html" result

-- Emoji

emojiChars :: [Char]
emojiChars = ['a'..'z'] ++ ['0'..'9'] ++ ['_','+','-']

emoji :: PandocMonad m => MarkdownParser m (F Inlines)
emoji = try $ do
  guardEnabled Ext_emoji
  char ':'
  emojikey <- many1 (oneOf emojiChars)
  char ':'
  case M.lookup emojikey emojis of
       Just s  -> return (return (B.str s))
       Nothing -> mzero

-- Citations

cite :: PandocMonad m => MarkdownParser m (F Inlines)
cite = do
  guardEnabled Ext_citations
  citations <- textualCite
            <|> do (cs, raw) <- withRaw normalCite
                   return $ (flip B.cite (B.text raw)) <$> cs
  return citations

textualCite :: PandocMonad m => MarkdownParser m (F Inlines)
textualCite = try $ do
  (suppressAuthor, key) <- citeKey
  let first = Citation{ citationId      = key
                      , citationPrefix  = []
                      , citationSuffix  = []
                      , citationMode    = if suppressAuthor
                                             then SuppressAuthor
                                             else AuthorInText
                      , citationNoteNum = 0
                      , citationHash    = 0
                      }
  mbrest <- option Nothing $ try $ spnl >> Just <$> withRaw normalCite
  case mbrest of
       Just (rest, raw) ->
         return $ (flip B.cite (B.text $ '@':key ++ " " ++ raw) . (first:))
               <$> rest
       Nothing   ->
         (do
          (cs, raw) <- withRaw $ bareloc first
          let (spaces',raw') = span isSpace raw
              spc | null spaces' = mempty
                  | otherwise    = B.space
          lab <- parseFromString' inlines $ dropBrackets raw'
          fallback <- referenceLink B.linkWith (lab,raw')
          return $ do
            fallback' <- fallback
            cs' <- cs
            return $
              case B.toList fallback' of
                Link{}:_ -> B.cite [first] (B.str $ '@':key) <> spc <> fallback'
                _        -> B.cite cs' (B.text $ '@':key ++ " " ++ raw))
         <|> return (do st <- askF
                        return $ case M.lookup key (stateExamples st) of
                                 Just n -> B.str (show n)
                                 _      -> B.cite [first] $ B.str $ '@':key)

bareloc :: PandocMonad m => Citation -> MarkdownParser m (F [Citation])
bareloc c = try $ do
  spnl
  char '['
  notFollowedBy $ char '^'
  suff <- suffix
  rest <- option (return []) $ try $ char ';' >> citeList
  spnl
  char ']'
  notFollowedBy $ oneOf "[("
  return $ do
    suff' <- suff
    rest' <- rest
    return $ c{ citationSuffix = B.toList suff' } : rest'

normalCite :: PandocMonad m => MarkdownParser m (F [Citation])
normalCite = try $ do
  char '['
  spnl
  citations <- citeList
  spnl
  char ']'
  return citations

suffix :: PandocMonad m => MarkdownParser m (F Inlines)
suffix = try $ do
  hasSpace <- option False (notFollowedBy nonspaceChar >> return True)
  spnl
  rest <- trimInlinesF . mconcat <$> many (notFollowedBy (oneOf ";]") >> inline)
  return $ if hasSpace
              then (B.space <>) <$> rest
              else rest

prefix :: PandocMonad m => MarkdownParser m (F Inlines)
prefix = trimInlinesF . mconcat <$>
  manyTill inline (char ']' <|> liftM (const ']') (lookAhead citeKey))

citeList :: PandocMonad m => MarkdownParser m (F [Citation])
citeList = fmap sequence $ sepBy1 citation (try $ char ';' >> spnl)

citation :: PandocMonad m => MarkdownParser m (F Citation)
citation = try $ do
  pref <- prefix
  (suppress_author, key) <- citeKey
  suff <- suffix
  return $ do
    x <- pref
    y <- suff
    return $ Citation{ citationId      = key
                     , citationPrefix  = B.toList x
                     , citationSuffix  = B.toList y
                     , citationMode    = if suppress_author
                                            then SuppressAuthor
                                            else NormalCitation
                     , citationNoteNum = 0
                     , citationHash    = 0
                     }

smart :: PandocMonad m => MarkdownParser m (F Inlines)
smart = do
  guardEnabled Ext_smart
  doubleQuoted <|> singleQuoted <|>
    choice (map (return <$>) [apostrophe, dash, ellipses])

singleQuoted :: PandocMonad m => MarkdownParser m (F Inlines)
singleQuoted = try $ do
  singleQuoteStart
  withQuoteContext InSingleQuote $
    fmap B.singleQuoted . trimInlinesF . mconcat <$>
      many1Till inline singleQuoteEnd

-- doubleQuoted will handle regular double-quoted sections, as well
-- as dialogues with an open double-quote without a close double-quote
-- in the same paragraph.
doubleQuoted :: PandocMonad m => MarkdownParser m (F Inlines)
doubleQuoted = try $ do
  doubleQuoteStart
  contents <- mconcat <$> many (try $ notFollowedBy doubleQuoteEnd >> inline)
  (withQuoteContext InDoubleQuote $ doubleQuoteEnd >> return
       (fmap B.doubleQuoted . trimInlinesF $ contents))
   <|> (return $ return (B.str "\8220") <> contents)
