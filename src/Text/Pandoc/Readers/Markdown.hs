{-# LANGUAGE RelaxedPolyRec #-} -- needed for inlinesBetween on GHC < 7
{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2013 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of markdown-formatted plain text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Markdown ( readMarkdown,
                                      readMarkdownWithWarnings ) where

import Data.List ( transpose, sortBy, findIndex, intersperse, intercalate )
import qualified Data.Map as M
import Data.Ord ( comparing )
import Data.Char ( isAlphaNum, toLower )
import Data.Maybe
import Text.Pandoc.Definition
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Yaml as Yaml
import Data.Yaml (ParseException(..), YamlException(..), YamlMark(..))
import qualified Data.HashMap.Strict as H
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.Vector as V
import Text.Pandoc.Builder (Inlines, Blocks, trimInlines, (<>))
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.XML (fromEntities)
import Text.Pandoc.Asciify (toAsciiChar)
import Text.Pandoc.Parsing hiding (tableWith)
import Text.Pandoc.Readers.LaTeX ( rawLaTeXInline, rawLaTeXBlock )
import Text.Pandoc.Readers.HTML ( htmlTag, htmlInBalanced, isInlineTag, isBlockTag,
                                  isTextTag, isCommentTag )
import Text.Pandoc.Biblio (processBiblio)
import Data.Monoid (mconcat, mempty)
import Control.Applicative ((<$>), (<*), (*>), (<$))
import Control.Monad
import System.FilePath (takeExtension, addExtension)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match (tagOpen)
import qualified Data.Set as Set

type MarkdownParser = Parser [Char] ParserState

-- | Read markdown from an input string and return a Pandoc document.
readMarkdown :: ReaderOptions -- ^ Reader options
             -> String        -- ^ String to parse (assuming @'\n'@ line endings)
             -> Pandoc
readMarkdown opts s =
  (readWith parseMarkdown) def{ stateOptions = opts } (s ++ "\n\n")

-- | Read markdown from an input string and return a pair of a Pandoc document
-- and a list of warnings.
readMarkdownWithWarnings :: ReaderOptions -- ^ Reader options
                         -> String        -- ^ String to parse (assuming @'\n'@ line endings)
                         -> (Pandoc, [String])
readMarkdownWithWarnings opts s =
  (readWith parseMarkdownWithWarnings) def{ stateOptions = opts } (s ++ "\n\n")
 where parseMarkdownWithWarnings = do
         doc <- parseMarkdown
         warnings <- stateWarnings <$> getState
         return (doc, warnings)

trimInlinesF :: F Inlines -> F Inlines
trimInlinesF = liftM trimInlines

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

isNull :: F Inlines -> Bool
isNull ils = B.isNull $ runF ils def

spnl :: Parser [Char] st ()
spnl = try $ do
  skipSpaces
  optional newline
  skipSpaces
  notFollowedBy (char '\n')

indentSpaces :: MarkdownParser String
indentSpaces = try $ do
  tabStop <- getOption readerTabStop
  count tabStop (char ' ') <|>
    string "\t" <?> "indentation"

nonindentSpaces :: MarkdownParser String
nonindentSpaces = do
  tabStop <- getOption readerTabStop
  sps <- many (char ' ')
  if length sps < tabStop
     then return sps
     else unexpected "indented line"

skipNonindentSpaces :: MarkdownParser ()
skipNonindentSpaces = do
  tabStop <- getOption readerTabStop
  atMostSpaces (tabStop - 1)

atMostSpaces :: Int -> MarkdownParser ()
atMostSpaces 0 = notFollowedBy (char ' ')
atMostSpaces n = (char ' ' >> atMostSpaces (n-1)) <|> return ()

litChar :: MarkdownParser Char
litChar = escapedChar'
       <|> characterReference
       <|> noneOf "\n"
       <|> try (newline >> notFollowedBy blankline >> return ' ')

-- | Parse a sequence of inline elements between square brackets,
-- including inlines between balanced pairs of square brackets.
inlinesInBalancedBrackets :: MarkdownParser (F Inlines)
inlinesInBalancedBrackets = charsInBalancedBrackets >>=
  parseFromString (trimInlinesF . mconcat <$> many inline)

charsInBalancedBrackets :: MarkdownParser [Char]
charsInBalancedBrackets = do
  char '['
  result <- manyTill (  many1 (noneOf "`[]\n")
                    <|> (snd <$> withRaw code)
                    <|> ((\xs -> '[' : xs ++ "]") <$> charsInBalancedBrackets)
                    <|> count 1 (satisfy (/='\n'))
                    <|> (newline >> notFollowedBy blankline >> return "\n")
                     ) (char ']')
  return $ concat result

--
-- document structure
--

titleLine :: MarkdownParser (F Inlines)
titleLine = try $ do
  char '%'
  skipSpaces
  res <- many $ (notFollowedBy newline >> inline)
             <|> try (endline >> whitespace)
  newline
  return $ trimInlinesF $ mconcat res

authorsLine :: MarkdownParser (F [Inlines])
authorsLine = try $ do
  char '%'
  skipSpaces
  authors <- sepEndBy (many (notFollowedBy (satisfy $ \c ->
                                c == ';' || c == '\n') >> inline))
                       (char ';' <|>
                        try (newline >> notFollowedBy blankline >> spaceChar))
  newline
  return $ sequence $ filter (not . isNull) $ map (trimInlinesF . mconcat) authors

dateLine :: MarkdownParser (F Inlines)
dateLine = try $ do
  char '%'
  skipSpaces
  trimInlinesF . mconcat <$> manyTill inline newline

titleBlock :: MarkdownParser ()
titleBlock = pandocTitleBlock <|> mmdTitleBlock

pandocTitleBlock :: MarkdownParser ()
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
                   ( if B.isNull title' then id else B.setMeta "title" title'
                   . if null author' then id else B.setMeta "author" author'
                   . if B.isNull date' then id else B.setMeta "date" date' )
                   nullMeta
  updateState $ \st -> st{ stateMeta' = stateMeta' st <> meta' }

yamlMetaBlock :: MarkdownParser (F Blocks)
yamlMetaBlock = try $ do
  guardEnabled Ext_yaml_metadata_block
  pos <- getPosition
  string "---"
  blankline
  rawYamlLines <- manyTill anyLine stopLine
  -- by including --- and ..., we allow yaml blocks with just comments:
  let rawYaml = unlines ("---" : (rawYamlLines ++ ["..."]))
  optional blanklines
  opts <- stateOptions <$> getState
  meta' <- case Yaml.decodeEither' $ UTF8.fromString rawYaml of
                Right (Yaml.Object hashmap) -> return $ return $
                         H.foldrWithKey (\k v m ->
                              if ignorable k
                                 then m
                                 else B.setMeta (T.unpack k)
                                            (yamlToMeta opts v) m)
                           nullMeta hashmap
                Right Yaml.Null -> return $ return nullMeta
                Right _ -> do
                            addWarning (Just pos) "YAML header is not an object"
                            return $ return nullMeta
                Left err' -> do
                         case err' of
                            InvalidYaml (Just YamlParseException{
                                        yamlProblem = problem
                                      , yamlContext = _ctxt
                                      , yamlProblemMark = Yaml.YamlMark {
                                            yamlLine = yline
                                          , yamlColumn = ycol
                                      }}) ->
                                 addWarning (Just $ setSourceLine
                                    (setSourceColumn pos
                                       (sourceColumn pos + ycol))
                                    (sourceLine pos + 1 + yline))
                                    $ "Could not parse YAML header: " ++
                                        problem
                            _ -> addWarning (Just pos)
                                    $ "Could not parse YAML header: " ++
                                        show err'
                         return $ return nullMeta
  updateState $ \st -> st{ stateMeta' = stateMeta' st <> meta' }
  return mempty

-- ignore fields ending with _
ignorable :: Text -> Bool
ignorable t = (T.pack "_") `T.isSuffixOf` t

toMetaValue :: ReaderOptions -> Text -> MetaValue
toMetaValue opts x =
  case readMarkdown opts (T.unpack x) of
       Pandoc _ [Plain xs] -> MetaInlines xs
       Pandoc _ [Para xs]
         | endsWithNewline x -> MetaBlocks [Para xs]
         | otherwise         -> MetaInlines xs
       Pandoc _ bs           -> MetaBlocks bs
  where endsWithNewline t = (T.pack "\n") `T.isSuffixOf` t

yamlToMeta :: ReaderOptions -> Yaml.Value -> MetaValue
yamlToMeta opts (Yaml.String t) = toMetaValue opts t
yamlToMeta _    (Yaml.Number n) = MetaString $ show n
yamlToMeta _    (Yaml.Bool b) = MetaBool b
yamlToMeta opts (Yaml.Array xs) = B.toMetaValue $ map (yamlToMeta opts)
                                                $ V.toList xs
yamlToMeta opts (Yaml.Object o) = MetaMap $ H.foldrWithKey (\k v m ->
                                if ignorable k
                                   then m
                                   else M.insert (T.unpack k)
                                           (yamlToMeta opts v) m)
                               M.empty o
yamlToMeta _ _ = MetaString ""

stopLine :: MarkdownParser ()
stopLine = try $ (string "---" <|> string "...") >> blankline >> return ()

mmdTitleBlock :: MarkdownParser ()
mmdTitleBlock = try $ do
  guardEnabled Ext_mmd_title_block
  kvPairs <- many1 kvPair
  blanklines
  updateState $ \st -> st{ stateMeta' = stateMeta' st <>
                             return (Meta $ M.fromList kvPairs) }

kvPair :: MarkdownParser (String, MetaValue)
kvPair = try $ do
  key <- many1Till (alphaNum <|> oneOf "_- ") (char ':')
  val <- manyTill anyChar
          (try $ newline >> lookAhead (blankline <|> nonspaceChar))
  let key' = concat $ words $ map toLower key
  let val' = MetaBlocks $ B.toList $ B.plain $ B.text $ trim val
  return (key',val')

parseMarkdown :: MarkdownParser Pandoc
parseMarkdown = do
  -- markdown allows raw HTML
  updateState $ \state -> state { stateOptions =
                let oldOpts = stateOptions state in
                    oldOpts{ readerParseRaw = True } }
  optional titleBlock
  blocks <- parseBlocks
  st <- getState
  let meta = runF (stateMeta' st) st
  let Pandoc _ bs = B.doc $ runF blocks st
  mbsty <- getOption readerCitationStyle
  refs <- getOption readerReferences
  return $ processBiblio mbsty refs $ Pandoc meta bs

addWarning :: Maybe SourcePos -> String -> MarkdownParser ()
addWarning mbpos msg =
  updateState $ \st -> st{
    stateWarnings = (msg ++ maybe "" (\pos -> " " ++ show pos) mbpos) :
                     stateWarnings st }

referenceKey :: MarkdownParser (F Blocks)
referenceKey = try $ do
  pos <- getPosition
  skipNonindentSpaces
  (_,raw) <- reference
  char ':'
  skipSpaces >> optional newline >> skipSpaces >> notFollowedBy (char '[')
  let sourceURL = liftM unwords $ many $ try $ do
                    notFollowedBy' referenceTitle
                    skipMany spaceChar
                    optional $ newline >> notFollowedBy blankline
                    skipMany spaceChar
                    notFollowedBy' (() <$ reference)
                    many1 $ notFollowedBy space >> litChar
  let betweenAngles = try $ char '<' >>
                       manyTill (escapedChar' <|> litChar) (char '>')
  src <- try betweenAngles <|> sourceURL
  tit <- option "" referenceTitle
  -- currently we just ignore MMD-style link/image attributes
  _kvs <- option [] $ guardEnabled Ext_link_attributes
                      >> many (spnl >> keyValAttr)
  blanklines
  let target = (escapeURI $ trimr src,  tit)
  st <- getState
  let oldkeys = stateKeys st
  let key = toKey raw
  case M.lookup key oldkeys of
    Just _  -> addWarning (Just pos) $ "Duplicate link reference `" ++ raw ++ "'"
    Nothing -> return ()
  updateState $ \s -> s { stateKeys = M.insert key target oldkeys }
  return $ return mempty

referenceTitle :: MarkdownParser String
referenceTitle = try $ do
  skipSpaces >> optional newline >> skipSpaces
  quotedTitle '"' <|> quotedTitle '\'' <|> charsInBalanced '(' ')' litChar

-- A link title in quotes
quotedTitle :: Char -> MarkdownParser String
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
abbrevKey :: MarkdownParser (F Blocks)
abbrevKey = do
  guardEnabled Ext_abbreviations
  try $ do
    char '*'
    reference
    char ':'
    skipMany (satisfy (/= '\n'))
    blanklines
    return $ return mempty

noteMarker :: MarkdownParser String
noteMarker = string "[^" >> many1Till (satisfy $ not . isBlank) (char ']')

rawLine :: MarkdownParser String
rawLine = try $ do
  notFollowedBy blankline
  notFollowedBy' $ try $ skipNonindentSpaces >> noteMarker
  optional indentSpaces
  anyLine

rawLines :: MarkdownParser String
rawLines = do
  first <- anyLine
  rest <- many rawLine
  return $ unlines (first:rest)

noteBlock :: MarkdownParser (F Blocks)
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
  parsed <- parseFromString parseBlocks raw
  let newnote = (ref, parsed)
  oldnotes <- stateNotes' <$> getState
  case lookup ref oldnotes of
    Just _  -> addWarning (Just pos) $ "Duplicate note reference `" ++ ref ++ "'"
    Nothing -> return ()
  updateState $ \s -> s { stateNotes' = newnote : oldnotes }
  return mempty

--
-- parsing blocks
--

parseBlocks :: MarkdownParser (F Blocks)
parseBlocks = mconcat <$> manyTill block eof

block :: MarkdownParser (F Blocks)
block = choice [ mempty <$ blanklines
               , codeBlockFenced
               , yamlMetaBlock
               , guardEnabled Ext_latex_macros *> (macro >>= return . return)
               , header
               , lhsCodeBlock
               , rawTeXBlock
               , divHtml
               , htmlBlock
               , table
               , lineBlock
               , codeBlockIndented
               , blockQuote
               , hrule
               , bulletList
               , orderedList
               , definitionList
               , noteBlock
               , referenceKey
               , abbrevKey
               , para
               , plain
               ] <?> "block"

--
-- header blocks
--

header :: MarkdownParser (F Blocks)
header = setextHeader <|> atxHeader <?> "header"

--  returns unique identifier
addToHeaderList :: Attr -> F Inlines -> MarkdownParser Attr
addToHeaderList (ident,classes,kvs) text = do
  let header' = runF text defaultParserState
  exts <- getOption readerExtensions
  let insert' = M.insertWith (\_new old -> old)
  if null ident && Ext_auto_identifiers `Set.member` exts
     then do
       ids <- stateIdentifiers `fmap` getState
       let id' = uniqueIdent (B.toList header') ids
       let id'' = if Ext_ascii_identifiers `Set.member` exts
                     then catMaybes $ map toAsciiChar id'
                     else id'
       updateState $ \st -> st{
              stateIdentifiers = if id' == id''
                                    then id' : ids
                                    else id' : id'' : ids,
              stateHeaders = insert' header' id' $ stateHeaders st }
       return (id'',classes,kvs)
     else do
        unless (null ident) $
          updateState $ \st -> st{
               stateHeaders = insert' header' ident $ stateHeaders st }
        return (ident,classes,kvs)

atxHeader :: MarkdownParser (F Blocks)
atxHeader = try $ do
  level <- many1 (char '#') >>= return . length
  notFollowedBy $ guardEnabled Ext_fancy_lists >>
                  (char '.' <|> char ')') -- this would be a list
  skipSpaces
  text <- trimInlinesF . mconcat <$> many (notFollowedBy atxClosing >> inline)
  attr <- atxClosing
  attr' <- addToHeaderList attr text
  return $ B.headerWith attr' level <$> text

atxClosing :: MarkdownParser Attr
atxClosing = try $ do
  attr' <- option nullAttr
             (guardEnabled Ext_mmd_header_identifiers >> mmdHeaderIdentifier)
  skipMany (char '#')
  skipSpaces
  attr <- option attr'
             (guardEnabled Ext_header_attributes >> attributes)
  blanklines
  return attr

setextHeaderEnd :: MarkdownParser Attr
setextHeaderEnd = try $ do
  attr <- option nullAttr
          $ (guardEnabled Ext_mmd_header_identifiers >> mmdHeaderIdentifier)
           <|> (guardEnabled Ext_header_attributes >> attributes)
  blanklines
  return attr

mmdHeaderIdentifier :: MarkdownParser Attr
mmdHeaderIdentifier = do
  ident <- stripFirstAndLast . snd <$> reference
  skipSpaces
  return (ident,[],[])

setextHeader :: MarkdownParser (F Blocks)
setextHeader = try $ do
  -- This lookahead prevents us from wasting time parsing Inlines
  -- unless necessary -- it gives a significant performance boost.
  lookAhead $ anyLine >> many1 (oneOf setextHChars) >> blankline
  text <- trimInlinesF . mconcat <$> many1 (notFollowedBy setextHeaderEnd >> inline)
  attr <- setextHeaderEnd
  underlineChar <- oneOf setextHChars
  many (char underlineChar)
  blanklines
  let level = (fromMaybe 0 $ findIndex (== underlineChar) setextHChars) + 1
  attr' <- addToHeaderList attr text
  return $ B.headerWith attr' level <$> text

--
-- hrule block
--

hrule :: Parser [Char] st (F Blocks)
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

indentedLine :: MarkdownParser String
indentedLine = indentSpaces >> anyLine >>= return . (++ "\n")

blockDelimiter :: (Char -> Bool)
               -> Maybe Int
               -> Parser [Char] st Int
blockDelimiter f len = try $ do
  c <- lookAhead (satisfy f)
  case len of
      Just l  -> count l (char c) >> many (char c) >> return l
      Nothing -> count 3 (char c) >> many (char c) >>=
                 return . (+ 3) . length

attributes :: MarkdownParser Attr
attributes = try $ do
  char '{'
  spnl
  attrs <- many (attribute >>~ spnl)
  char '}'
  return $ foldl (\x f -> f x) nullAttr attrs

attribute :: MarkdownParser (Attr -> Attr)
attribute = identifierAttr <|> classAttr <|> keyValAttr <|> specialAttr

identifier :: MarkdownParser String
identifier = do
  first <- letter
  rest <- many $ alphaNum <|> oneOf "-_:."
  return (first:rest)

identifierAttr :: MarkdownParser (Attr -> Attr)
identifierAttr = try $ do
  char '#'
  result <- identifier
  return $ \(_,cs,kvs) -> (result,cs,kvs)

classAttr :: MarkdownParser (Attr -> Attr)
classAttr = try $ do
  char '.'
  result <- identifier
  return $ \(id',cs,kvs) -> (id',cs ++ [result],kvs)

keyValAttr :: MarkdownParser (Attr -> Attr)
keyValAttr = try $ do
  key <- identifier
  char '='
  val <- enclosed (char '"') (char '"') litChar
     <|> enclosed (char '\'') (char '\'') litChar
     <|> many (escapedChar' <|> noneOf " \t\n\r}")
  return $ \(id',cs,kvs) -> (id',cs,kvs ++ [(key,val)])

specialAttr :: MarkdownParser (Attr -> Attr)
specialAttr = do
  char '-'
  return $ \(id',cs,kvs) -> (id',cs ++ ["unnumbered"],kvs)

codeBlockFenced :: MarkdownParser (F Blocks)
codeBlockFenced = try $ do
  c <- try (guardEnabled Ext_fenced_code_blocks >> lookAhead (char '~'))
     <|> (guardEnabled Ext_backtick_code_blocks >> lookAhead (char '`'))
  size <- blockDelimiter (== c) Nothing
  skipMany spaceChar
  attr <- option ([],[],[]) $
            try (guardEnabled Ext_fenced_code_attributes >> attributes)
           <|> ((\x -> ("",[x],[])) <$> identifier)
  blankline
  contents <- manyTill anyLine (blockDelimiter (== c) (Just size))
  blanklines
  return $ return $ B.codeBlockWith attr $ intercalate "\n" contents

codeBlockIndented :: MarkdownParser (F Blocks)
codeBlockIndented = do
  contents <- many1 (indentedLine <|>
                     try (do b <- blanklines
                             l <- indentedLine
                             return $ b ++ l))
  optional blanklines
  classes <- getOption readerIndentedCodeClasses
  return $ return $ B.codeBlockWith ("", classes, []) $
           stripTrailingNewlines $ concat contents

lhsCodeBlock :: MarkdownParser (F Blocks)
lhsCodeBlock = do
  guardEnabled Ext_literate_haskell
  (return . B.codeBlockWith ("",["sourceCode","literate","haskell"],[]) <$>
          (lhsCodeBlockBird <|> lhsCodeBlockLaTeX))
    <|> (return . B.codeBlockWith ("",["sourceCode","haskell"],[]) <$>
          lhsCodeBlockInverseBird)

lhsCodeBlockLaTeX :: MarkdownParser String
lhsCodeBlockLaTeX = try $ do
  string "\\begin{code}"
  manyTill spaceChar newline
  contents <- many1Till anyChar (try $ string "\\end{code}")
  blanklines
  return $ stripTrailingNewlines contents

lhsCodeBlockBird :: MarkdownParser String
lhsCodeBlockBird = lhsCodeBlockBirdWith '>'

lhsCodeBlockInverseBird :: MarkdownParser String
lhsCodeBlockInverseBird = lhsCodeBlockBirdWith '<'

lhsCodeBlockBirdWith :: Char -> MarkdownParser String
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

birdTrackLine :: Char -> Parser [Char] st String
birdTrackLine c = try $ do
  char c
  -- allow html tags on left margin:
  when (c == '<') $ notFollowedBy letter
  anyLine

--
-- block quotes
--

emailBlockQuoteStart :: MarkdownParser Char
emailBlockQuoteStart = try $ skipNonindentSpaces >> char '>' >>~ optional (char ' ')

emailBlockQuote :: MarkdownParser [String]
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

blockQuote :: MarkdownParser (F Blocks)
blockQuote = do
  raw <- emailBlockQuote
  -- parse the extracted block, which may contain various block elements:
  contents <- parseFromString parseBlocks $ (intercalate "\n" raw) ++ "\n\n"
  return $ B.blockQuote <$> contents

--
-- list blocks
--

bulletListStart :: MarkdownParser ()
bulletListStart = try $ do
  optional newline -- if preceded by a Plain block in a list context
  skipNonindentSpaces
  notFollowedBy' (() <$ hrule)     -- because hrules start out just like lists
  satisfy isBulletListMarker
  spaceChar
  skipSpaces

anyOrderedListStart :: MarkdownParser (Int, ListNumberStyle, ListNumberDelim)
anyOrderedListStart = try $ do
  optional newline -- if preceded by a Plain block in a list context
  skipNonindentSpaces
  notFollowedBy $ string "p." >> spaceChar >> digit  -- page number
  (guardDisabled Ext_fancy_lists >>
       do many1 digit
          char '.'
          spaceChar
          return (1, DefaultStyle, DefaultDelim))
   <|> do (num, style, delim) <- anyOrderedListMarker
          -- if it could be an abbreviated first name, insist on more than one space
          if delim == Period && (style == UpperAlpha || (style == UpperRoman &&
             num `elem` [1, 5, 10, 50, 100, 500, 1000]))
             then char '\t' <|> (try $ char ' ' >> spaceChar)
             else spaceChar
          skipSpaces
          return (num, style, delim)

listStart :: MarkdownParser ()
listStart = bulletListStart <|> (anyOrderedListStart >> return ())

-- parse a line of a list item (start = parser for beginning of list item)
listLine :: MarkdownParser String
listLine = try $ do
  notFollowedBy blankline
  notFollowedBy' (do indentSpaces
                     many (spaceChar)
                     listStart)
  chunks <- manyTill (liftM snd (htmlTag isCommentTag) <|> count 1 anyChar) newline
  return $ concat chunks

-- parse raw text for one list item, excluding start marker and continuations
rawListItem :: MarkdownParser a
            -> MarkdownParser String
rawListItem start = try $ do
  start
  first <- listLine
  rest <- many (notFollowedBy listStart >> listLine)
  blanks <- many blankline
  return $ unlines (first:rest) ++ blanks

-- continuation of a list item - indented and separated by blankline
-- or (in compact lists) endline.
-- note: nested lists are parsed as continuations
listContinuation :: MarkdownParser String
listContinuation = try $ do
  lookAhead indentSpaces
  result <- many1 listContinuationLine
  blanks <- many blankline
  return $ concat result ++ blanks

listContinuationLine :: MarkdownParser String
listContinuationLine = try $ do
  notFollowedBy blankline
  notFollowedBy' listStart
  optional indentSpaces
  result <- anyLine
  return $ result ++ "\n"

listItem :: MarkdownParser a
         -> MarkdownParser (F Blocks)
listItem start = try $ do
  first <- rawListItem start
  continuations <- many listContinuation
  -- parsing with ListItemState forces markers at beginning of lines to
  -- count as list item markers, even if not separated by blank space.
  -- see definition of "endline"
  state <- getState
  let oldContext = stateParserContext state
  setState $ state {stateParserContext = ListItemState}
  -- parse the extracted block, which may contain various block elements:
  let raw = concat (first:continuations)
  contents <- parseFromString parseBlocks raw
  updateState (\st -> st {stateParserContext = oldContext})
  return contents

orderedList :: MarkdownParser (F Blocks)
orderedList = try $ do
  (start, style, delim) <- lookAhead anyOrderedListStart
  unless ((style == DefaultStyle || style == Decimal || style == Example) &&
          (delim == DefaultDelim || delim == Period)) $
    guardEnabled Ext_fancy_lists
  when (style == Example) $ guardEnabled Ext_example_lists
  items <- fmap sequence $ many1 $ listItem
                 ( try $ do
                     optional newline -- if preceded by Plain block in a list
                     skipNonindentSpaces
                     orderedListMarker style delim )
  start' <- option 1 $ guardEnabled Ext_startnum >> return start
  return $ B.orderedListWith (start', style, delim) <$> fmap compactify' items

bulletList :: MarkdownParser (F Blocks)
bulletList = do
  items <- fmap sequence $ many1 $ listItem  bulletListStart
  return $ B.bulletList <$> fmap compactify' items

-- definition lists

defListMarker :: MarkdownParser ()
defListMarker = do
  sps <- nonindentSpaces
  char ':' <|> char '~'
  tabStop <- getOption readerTabStop
  let remaining = tabStop - (length sps + 1)
  if remaining > 0
     then count remaining (char ' ') <|> string "\t"
     else mzero
  return ()

definitionListItem :: MarkdownParser (F (Inlines, [Blocks]))
definitionListItem = try $ do
  -- first, see if this has any chance of being a definition list:
  lookAhead (anyLine >> optional blankline >> defListMarker)
  term <- trimInlinesF . mconcat <$> manyTill inline newline
  optional blankline
  raw <- many1 defRawBlock
  state <- getState
  let oldContext = stateParserContext state
  -- parse the extracted block, which may contain various block elements:
  contents <- mapM (parseFromString parseBlocks) raw
  updateState (\st -> st {stateParserContext = oldContext})
  return $ liftM2 (,) term (sequence contents)

defRawBlock :: MarkdownParser String
defRawBlock = try $ do
  defListMarker
  firstline <- anyLine
  rawlines <- many (notFollowedBy blankline >> indentSpaces >> anyLine)
  trailing <- option "" blanklines
  cont <- liftM concat $ many $ do
            lns <- many1 $ notFollowedBy blankline >> indentSpaces >> anyLine
            trl <- option "" blanklines
            return $ unlines lns ++ trl
  return $ firstline ++ "\n" ++ unlines rawlines ++ trailing ++ cont

definitionList :: MarkdownParser (F Blocks)
definitionList = do
  guardEnabled Ext_definition_lists
  items <- fmap sequence $ many1 definitionListItem
  return $ B.definitionList <$> fmap compactify'DL items

compactify'DL :: [(Inlines, [Blocks])] -> [(Inlines, [Blocks])]
compactify'DL items =
  let defs = concatMap snd items
      defBlocks = reverse $ concatMap B.toList defs
      isPara (Para _) = True
      isPara _        = False
  in  case defBlocks of
           (Para x:_) -> if not $ any isPara (drop 1 defBlocks)
                            then let (t,ds) = last items
                                     lastDef = B.toList $ last ds
                                     ds' = init ds ++
                                          [B.fromList $ init lastDef ++ [Plain x]]
                                  in init items ++ [(t, ds')]
                            else items
           _          -> items

--
-- paragraph block
--

para :: MarkdownParser (F Blocks)
para = try $ do
  exts <- getOption readerExtensions
  result <- trimInlinesF . mconcat <$> many1 inline
  option (B.plain <$> result)
    $ try $ do
            newline
            (blanklines >> return mempty)
              <|> (guardDisabled Ext_blank_before_blockquote >> lookAhead blockQuote)
              <|> (guardDisabled Ext_blank_before_header >> lookAhead header)
            return $ do
              result' <- result
              case B.toList result' of
                   [Image alt (src,tit)]
                     | Ext_implicit_figures `Set.member` exts ->
                        -- the fig: at beginning of title indicates a figure
                        return $ B.para $ B.singleton
                               $ Image alt (src,'f':'i':'g':':':tit)
                   _ -> return $ B.para result'

plain :: MarkdownParser (F Blocks)
plain = fmap B.plain . trimInlinesF . mconcat <$> many1 inline

--
-- raw html
--

htmlElement :: MarkdownParser String
htmlElement = strictHtmlBlock <|> liftM snd (htmlTag isBlockTag)

htmlBlock :: MarkdownParser (F Blocks)
htmlBlock = do
  guardEnabled Ext_raw_html
  res <- (guardEnabled Ext_markdown_in_html_blocks >> rawHtmlBlocks)
          <|> htmlBlock'
  return $ return $ B.rawBlock "html" res

htmlBlock' :: MarkdownParser String
htmlBlock' = try $ do
    first <- htmlElement
    finalSpace <- many spaceChar
    finalNewlines <- many newline
    return $ first ++ finalSpace ++ finalNewlines

strictHtmlBlock :: MarkdownParser String
strictHtmlBlock = htmlInBalanced (not . isInlineTag)

rawVerbatimBlock :: MarkdownParser String
rawVerbatimBlock = try $ do
  (TagOpen tag _, open) <- htmlTag (tagOpen (\t ->
                              t == "pre" || t == "style" || t == "script")
                              (const True))
  contents <- manyTill anyChar (htmlTag (~== TagClose tag))
  return $ open ++ contents ++ renderTags [TagClose tag]

rawTeXBlock :: MarkdownParser (F Blocks)
rawTeXBlock = do
  guardEnabled Ext_raw_tex
  result <- (B.rawBlock "latex" <$> rawLaTeXBlock)
        <|> (B.rawBlock "context" <$> rawConTeXtEnvironment)
  spaces
  return $ return result

rawHtmlBlocks :: MarkdownParser String
rawHtmlBlocks = do
  htmlBlocks <- many1 $ try $ do
                          s <- rawVerbatimBlock <|> try (
                                do (t,raw) <- htmlTag isBlockTag
                                   exts <- getOption readerExtensions
                                   -- if open tag, need markdown="1" if
                                   -- markdown_attributes extension is set
                                   case t of
                                        TagOpen _ as
                                          | Ext_markdown_attribute `Set.member`
                                              exts ->
                                                if "markdown" `notElem`
                                                   map fst as
                                                   then mzero
                                                   else return $
                                                     stripMarkdownAttribute raw
                                          | otherwise -> return raw
                                        _ -> return raw )
                          sps <- do sp1 <- many spaceChar
                                    sp2 <- option "" (blankline >> return "\n")
                                    sp3 <- many spaceChar
                                    sp4 <- option "" blanklines
                                    return $ sp1 ++ sp2 ++ sp3 ++ sp4
                          -- note: we want raw html to be able to
                          -- precede a code block, when separated
                          -- by a blank line
                          return $ s ++ sps
  let combined = concat htmlBlocks
  return $ if last combined == '\n' then init combined else combined

-- remove markdown="1" attribute
stripMarkdownAttribute :: String -> String
stripMarkdownAttribute s = renderTags' $ map filterAttrib $ parseTags s
  where filterAttrib (TagOpen t as) = TagOpen t
                                        [(k,v) | (k,v) <- as, k /= "markdown"]
        filterAttrib              x = x

--
-- line block
--

lineBlock :: MarkdownParser (F Blocks)
lineBlock = try $ do
  guardEnabled Ext_line_blocks
  lines' <- lineBlockLines >>=
            mapM (parseFromString (trimInlinesF . mconcat <$> many inline))
  return $ B.para <$> (mconcat $ intersperse (return B.linebreak) lines')

--
-- Tables
--

-- Parse a dashed line with optional trailing spaces; return its length
-- and the length including trailing space.
dashedLine :: Char
           -> Parser [Char] st (Int, Int)
dashedLine ch = do
  dashes <- many1 (char ch)
  sp     <- many spaceChar
  return $ (length dashes, length $ dashes ++ sp)

-- Parse a table header with dashed lines of '-' preceded by
-- one (or zero) line of text.
simpleTableHeader :: Bool  -- ^ Headerless table
                  -> MarkdownParser (F [Blocks], [Alignment], [Int])
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
           $ mapM (parseFromString (mconcat <$> many plain))
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
                 (x:_)  -> (head x `elem` " \t", length x < len)
                 []     -> (False, False)
  in  case (leftSpace, rightSpace) of
        (True,  False)   -> AlignRight
        (False, True)    -> AlignLeft
        (True,  True)    -> AlignCenter
        (False, False)   -> AlignDefault

-- Parse a table footer - dashed lines followed by blank line.
tableFooter :: MarkdownParser String
tableFooter = try $ skipNonindentSpaces >> many1 (dashedLine '-') >> blanklines

-- Parse a table separator - dashed line.
tableSep :: MarkdownParser Char
tableSep = try $ skipNonindentSpaces >> many1 (dashedLine '-') >> char '\n'

-- Parse a raw line and split it into chunks by indices.
rawTableLine :: [Int]
             -> MarkdownParser [String]
rawTableLine indices = do
  notFollowedBy' (blanklines <|> tableFooter)
  line <- many1Till anyChar newline
  return $ map trim $ tail $
           splitStringByIndices (init indices) line

-- Parse a table line and return a list of lists of blocks (columns).
tableLine :: [Int]
          -> MarkdownParser (F [Blocks])
tableLine indices = rawTableLine indices >>=
  fmap sequence . mapM (parseFromString (mconcat <$> many plain))

-- Parse a multiline table row and return a list of blocks (columns).
multilineRow :: [Int]
             -> MarkdownParser (F [Blocks])
multilineRow indices = do
  colLines <- many1 (rawTableLine indices)
  let cols = map unlines $ transpose colLines
  fmap sequence $ mapM (parseFromString (mconcat <$> many plain)) cols

-- Parses a table caption:  inlines beginning with 'Table:'
-- and followed by blank lines.
tableCaption :: MarkdownParser (F Inlines)
tableCaption = try $ do
  guardEnabled Ext_table_captions
  skipNonindentSpaces
  string ":" <|> string "Table:"
  trimInlinesF . mconcat <$> many1 inline <* blanklines

-- Parse a simple table with '---' header and one line per row.
simpleTable :: Bool  -- ^ Headerless table
            -> MarkdownParser ([Alignment], [Double], F [Blocks], F [[Blocks]])
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
multilineTable :: Bool -- ^ Headerless table
               -> MarkdownParser ([Alignment], [Double], F [Blocks], F [[Blocks]])
multilineTable headless =
  tableWith (multilineTableHeader headless) multilineRow blanklines tableFooter

multilineTableHeader :: Bool -- ^ Headerless table
                     -> MarkdownParser (F [Blocks], [Alignment], [Int])
multilineTableHeader headless = try $ do
  if headless
     then return '\n'
     else tableSep >>~ notFollowedBy blankline
  rawContent  <- if headless
                    then return $ repeat ""
                    else many1
                         (notFollowedBy tableSep >> many1Till anyChar newline)
  initSp      <- nonindentSpaces
  dashes      <- many1 (dashedLine '-')
  newline
  let (lengths, lines') = unzip dashes
  let indices  = scanl (+) (length initSp) lines'
  rawHeadsList <- if headless
                     then liftM (map (:[]) . tail .
                              splitStringByIndices (init indices)) $ lookAhead anyLine
                     else return $ transpose $ map
                           (\ln -> tail $ splitStringByIndices (init indices) ln)
                           rawContent
  let aligns   = zipWith alignType rawHeadsList lengths
  let rawHeads = if headless
                    then replicate (length dashes) ""
                    else map unwords rawHeadsList
  heads <- fmap sequence $
           mapM (parseFromString (mconcat <$> many plain)) $
             map trim rawHeads
  return (heads, aligns, indices)

-- Parse a grid table:  starts with row of '-' on top, then header
-- (which may be grid), then the rows,
-- which may be grid, separated by blank lines, and
-- ending with a footer (dashed line followed by blank line).
gridTable :: Bool -- ^ Headerless table
          -> MarkdownParser ([Alignment], [Double], F [Blocks], F [[Blocks]])
gridTable headless =
  tableWith (gridTableHeader headless) gridTableRow
            (gridTableSep '-') gridTableFooter

gridTableSplitLine :: [Int] -> String -> [String]
gridTableSplitLine indices line = map removeFinalBar $ tail $
  splitStringByIndices (init indices) $ trimr line

gridPart :: Char -> Parser [Char] st (Int, Int)
gridPart ch = do
  dashes <- many1 (char ch)
  char '+'
  return (length dashes, length dashes + 1)

gridDashedLines :: Char -> Parser [Char] st [(Int,Int)]
gridDashedLines ch = try $ char '+' >> many1 (gridPart ch) >>~ blankline

removeFinalBar :: String -> String
removeFinalBar =
  reverse . dropWhile (`elem` " \t") . dropWhile (=='|') . reverse

-- | Separator between rows of grid table.
gridTableSep :: Char -> MarkdownParser Char
gridTableSep ch = try $ gridDashedLines ch >> return '\n'

-- | Parse header for a grid table.
gridTableHeader :: Bool -- ^ Headerless table
                -> MarkdownParser (F [Blocks], [Alignment], [Int])
gridTableHeader headless = try $ do
  optional blanklines
  dashes <- gridDashedLines '-'
  rawContent  <- if headless
                    then return $ repeat ""
                    else many1
                         (notFollowedBy (gridTableSep '=') >> char '|' >>
                           many1Till anyChar newline)
  if headless
     then return ()
     else gridTableSep '=' >> return ()
  let lines'   = map snd dashes
  let indices  = scanl (+) 0 lines'
  let aligns   = replicate (length lines') AlignDefault
  -- RST does not have a notion of alignments
  let rawHeads = if headless
                    then replicate (length dashes) ""
                    else map unwords $ transpose
                       $ map (gridTableSplitLine indices) rawContent
  heads <- fmap sequence $ mapM (parseFromString parseBlocks . trim) rawHeads
  return (heads, aligns, indices)

gridTableRawLine :: [Int] -> MarkdownParser [String]
gridTableRawLine indices = do
  char '|'
  line <- many1Till anyChar newline
  return (gridTableSplitLine indices line)

-- | Parse row of grid table.
gridTableRow :: [Int]
             -> MarkdownParser (F [Blocks])
gridTableRow indices = do
  colLines <- many1 (gridTableRawLine indices)
  let cols = map ((++ "\n") . unlines . removeOneLeadingSpace) $
               transpose colLines
  fmap compactify' <$> fmap sequence (mapM (parseFromString parseBlocks) cols)

removeOneLeadingSpace :: [String] -> [String]
removeOneLeadingSpace xs =
  if all startsWithSpace xs
     then map (drop 1) xs
     else xs
   where startsWithSpace ""     = True
         startsWithSpace (y:_) = y == ' '

-- | Parse footer for a grid table.
gridTableFooter :: MarkdownParser [Char]
gridTableFooter = blanklines

pipeTable :: MarkdownParser ([Alignment], [Double], F [Blocks], F [[Blocks]])
pipeTable = try $ do
  let pipeBreak = nonindentSpaces *> optional (char '|') *>
                      pipeTableHeaderPart `sepBy1` sepPipe <*
                      optional (char '|') <* blankline
  (heads,aligns) <- try ( pipeBreak >>= \als ->
                     return (return $ replicate (length als) mempty, als))
                  <|> ( pipeTableRow >>= \row -> pipeBreak >>= \als ->

                          return (row, als) )
  lines' <- sequence <$> many1 pipeTableRow
  let widths = replicate (length aligns) 0.0
  return $ (aligns, widths, heads, lines')

sepPipe :: MarkdownParser ()
sepPipe = try $ do
  char '|' <|> char '+'
  notFollowedBy blankline

-- parse a row, also returning probable alignments for org-table cells
pipeTableRow :: MarkdownParser (F [Blocks])
pipeTableRow = do
  nonindentSpaces
  optional (char '|')
  let cell = mconcat <$>
                 many (notFollowedBy (blankline <|> char '|') >> inline)
  first <- cell
  sepPipe
  rest <- cell `sepBy1` sepPipe
  optional (char '|')
  blankline
  let cells  = sequence (first:rest)
  return $ do
    cells' <- cells
    return $ map
        (\ils ->
           case trimInlines ils of
                 ils' | B.isNull ils' -> mempty
                      | otherwise   -> B.plain $ ils') cells'

pipeTableHeaderPart :: Parser [Char] st Alignment
pipeTableHeaderPart = try $ do
  skipMany spaceChar
  left <- optionMaybe (char ':')
  many1 (char '-')
  right <- optionMaybe (char ':')
  skipMany spaceChar
  return $
    case (left,right) of
      (Nothing,Nothing) -> AlignDefault
      (Just _,Nothing)  -> AlignLeft
      (Nothing,Just _)  -> AlignRight
      (Just _,Just _)   -> AlignCenter

-- Succeed only if current line contains a pipe.
scanForPipe :: Parser [Char] st ()
scanForPipe = do
  inp <- getInput
  case break (\c -> c == '\n' || c == '|') inp of
       (_,'|':_) -> return ()
       _         -> mzero

-- | Parse a table using 'headerParser', 'rowParser',
-- 'lineParser', and 'footerParser'.  Variant of the version in
-- Text.Pandoc.Parsing.
tableWith :: MarkdownParser (F [Blocks], [Alignment], [Int])
          -> ([Int] -> MarkdownParser (F [Blocks]))
          -> MarkdownParser sep
          -> MarkdownParser end
          -> MarkdownParser ([Alignment], [Double], F [Blocks], F [[Blocks]])
tableWith headerParser rowParser lineParser footerParser = try $ do
    (heads, aligns, indices) <- headerParser
    lines' <- fmap sequence $ rowParser indices `sepEndBy1` lineParser
    footerParser
    numColumns <- getOption readerColumns
    let widths = if (indices == [])
                    then replicate (length aligns) 0.0
                    else widthsFromIndices numColumns indices
    return $ (aligns, widths, heads, lines')

table :: MarkdownParser (F Blocks)
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
                  Nothing  -> option (return mempty) tableCaption
                  Just c   -> return c
  return $ do
    caption' <- caption
    heads' <- heads
    lns' <- lns
    return $ B.table caption' (zip aligns widths) heads' lns'

--
-- inline
--

inline :: MarkdownParser (F Inlines)
inline = choice [ whitespace
                , bareURL
                , str
                , endline
                , code
                , strongOrEmph
                , note
                , cite
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
                , escapedChar
                , rawLaTeXInline'
                , exampleRef
                , smart
                , return . B.singleton <$> charRef
                , symbol
                , ltSign
                ] <?> "inline"

escapedChar' :: MarkdownParser Char
escapedChar' = try $ do
  char '\\'
  (guardEnabled Ext_all_symbols_escapable >> satisfy (not . isAlphaNum))
     <|> oneOf "\\`*_{}[]()>#+-.!~\""

escapedChar :: MarkdownParser (F Inlines)
escapedChar = do
  result <- escapedChar'
  case result of
       ' '   -> return $ return $ B.str "\160" -- "\ " is a nonbreaking space
       '\n'  -> guardEnabled Ext_escaped_line_breaks >>
                return (return B.linebreak)  -- "\[newline]" is a linebreak
       _     -> return $ return $ B.str [result]

ltSign :: MarkdownParser (F Inlines)
ltSign = do
  guardDisabled Ext_raw_html
    <|> guardDisabled Ext_markdown_in_html_blocks
    <|> (notFollowedBy' rawHtmlBlocks >> return ())
  char '<'
  return $ return $ B.str "<"

exampleRef :: MarkdownParser (F Inlines)
exampleRef = try $ do
  guardEnabled Ext_example_lists
  char '@'
  lab <- many1 (alphaNum <|> oneOf "-_")
  return $ do
    st <- askF
    return $ case M.lookup lab (stateExamples st) of
                  Just n    -> B.str (show n)
                  Nothing   -> B.str ('@':lab)

symbol :: MarkdownParser (F Inlines)
symbol = do
  result <- noneOf "<\\\n\t "
         <|> try (do lookAhead $ char '\\'
                     notFollowedBy' (() <$ rawTeXBlock)
                     char '\\')
  return $ return $ B.str [result]

-- parses inline code, between n `s and n `s
code :: MarkdownParser (F Inlines)
code = try $ do
  starts <- many1 (char '`')
  skipSpaces
  result <- many1Till (many1 (noneOf "`\n") <|> many1 (char '`') <|>
                       (char '\n' >> notFollowedBy' blankline >> return " "))
                      (try (skipSpaces >> count (length starts) (char '`') >>
                      notFollowedBy (char '`')))
  attr <- option ([],[],[]) (try $ guardEnabled Ext_inline_code_attributes >>
                                   optional whitespace >> attributes)
  return $ return $ B.codeWith attr $ trim $ concat result

math :: MarkdownParser (F Inlines)
math =  (return . B.displayMath <$> (mathDisplay >>= applyMacros'))
     <|> (return . B.math <$> (mathInline >>= applyMacros'))

mathDisplay :: MarkdownParser String
mathDisplay =
      (guardEnabled Ext_tex_math_dollars >> mathDisplayWith "$$" "$$")
  <|> (guardEnabled Ext_tex_math_single_backslash >>
       mathDisplayWith "\\[" "\\]")
  <|> (guardEnabled Ext_tex_math_double_backslash >>
       mathDisplayWith "\\\\[" "\\\\]")

mathDisplayWith :: String -> String -> MarkdownParser String
mathDisplayWith op cl = try $ do
  string op
  many1Till (noneOf "\n" <|> (newline >>~ notFollowedBy' blankline)) (try $ string cl)

mathInline :: MarkdownParser String
mathInline =
      (guardEnabled Ext_tex_math_dollars >> mathInlineWith "$" "$")
  <|> (guardEnabled Ext_tex_math_single_backslash >>
       mathInlineWith "\\(" "\\)")
  <|> (guardEnabled Ext_tex_math_double_backslash >>
       mathInlineWith "\\\\(" "\\\\)")

mathInlineWith :: String -> String -> MarkdownParser String
mathInlineWith op cl = try $ do
  string op
  notFollowedBy space
  words' <- many1Till (count 1 (noneOf "\n\\")
                   <|> (char '\\' >> anyChar >>= \c -> return ['\\',c])
                   <|> count 1 newline <* notFollowedBy' blankline
                       *> return " ")
              (try $ string cl)
  notFollowedBy digit  -- to prevent capture of $5
  return $ concat words'

-- Parses material enclosed in *s, **s, _s, or __s.
-- Designed to avoid backtracking.
enclosure :: Char
          -> MarkdownParser (F Inlines)
enclosure c = do
  cs <- many1 (char c)
  (return (B.str cs) <>) <$> whitespace
    <|> case length cs of
             3  -> three c
             2  -> two   c mempty
             1  -> one   c mempty
             _  -> return (return $ B.str cs)

-- Parse inlines til you hit one c or a sequence of two cs.
-- If one c, emit emph and then parse two.
-- If two cs, emit strong and then parse one.
three :: Char -> MarkdownParser (F Inlines)
three c = do
  contents <- mconcat <$> many (notFollowedBy (char c) >> inline)
  (try (string [c,c,c]) >> return ((B.strong . B.emph) <$> contents))
    <|> (try (string [c,c]) >> one c (B.strong <$> contents))
    <|> (char c >> two c (B.emph <$> contents))
    <|> return (return (B.str [c,c,c]) <> contents)

-- Parse inlines til you hit two c's, and emit strong.
-- If you never do hit two cs, emit ** plus inlines parsed.
two :: Char -> F Inlines -> MarkdownParser (F Inlines)
two c prefix' = do
  let ender = try $ string [c,c]
  contents <- mconcat <$> many (try $ notFollowedBy ender >> inline)
  (ender >> return (B.strong <$> (prefix' <> contents)))
    <|> return (return (B.str [c,c]) <> (prefix' <> contents))

-- Parse inlines til you hit a c, and emit emph.
-- If you never hit a c, emit * plus inlines parsed.
one :: Char -> F Inlines -> MarkdownParser (F Inlines)
one c prefix' = do
  contents <- mconcat <$> many (  (notFollowedBy (char c) >> inline)
                           <|> try (string [c,c] >>
                                    notFollowedBy (char c) >>
                                    two c prefix') )
  (char c >> return (B.emph <$> (prefix' <> contents)))
    <|> return (return (B.str [c]) <> (prefix' <> contents))

strongOrEmph :: MarkdownParser (F Inlines)
strongOrEmph =  enclosure '*' <|> (checkIntraword >> enclosure '_')
  where  checkIntraword = do
           exts <- getOption readerExtensions
           when (Ext_intraword_underscores `Set.member` exts) $ do
             pos <- getPosition
             lastStrPos <- stateLastStrPos <$> getState
             guard $ lastStrPos /= Just pos

-- | Parses a list of inlines between start and end delimiters.
inlinesBetween :: (Show b)
               => MarkdownParser a
               -> MarkdownParser b
               -> MarkdownParser (F Inlines)
inlinesBetween start end =
  (trimInlinesF . mconcat) <$> try (start >> many1Till inner end)
    where inner      = innerSpace <|> (notFollowedBy' (() <$ whitespace) >> inline)
          innerSpace = try $ whitespace >>~ notFollowedBy' end

strikeout :: MarkdownParser (F Inlines)
strikeout = fmap B.strikeout <$>
 (guardEnabled Ext_strikeout >> inlinesBetween strikeStart strikeEnd)
    where strikeStart = string "~~" >> lookAhead nonspaceChar
                        >> notFollowedBy (char '~')
          strikeEnd   = try $ string "~~"

superscript :: MarkdownParser (F Inlines)
superscript = fmap B.superscript <$> try (do
  guardEnabled Ext_superscript
  char '^'
  mconcat <$> many1Till (notFollowedBy spaceChar >> inline) (char '^'))

subscript :: MarkdownParser (F Inlines)
subscript = fmap B.subscript <$> try (do
  guardEnabled Ext_subscript
  char '~'
  mconcat <$> many1Till (notFollowedBy spaceChar >> inline) (char '~'))

whitespace :: MarkdownParser (F Inlines)
whitespace = spaceChar >> return <$> (lb <|> regsp) <?> "whitespace"
  where lb = spaceChar >> skipMany spaceChar >> option B.space (endline >> return B.linebreak)
        regsp = skipMany spaceChar >> return B.space

nonEndline :: Parser [Char] st Char
nonEndline = satisfy (/='\n')

str :: MarkdownParser (F Inlines)
str = do
  result <- many1 alphaNum
  pos <- getPosition
  updateState $ \s -> s{ stateLastStrPos = Just pos }
  let spacesToNbr = map (\c -> if c == ' ' then '\160' else c)
  isSmart <- getOption readerSmart
  if isSmart
     then case likelyAbbrev result of
               []        -> return $ return $ B.str result
               xs        -> choice (map (\x ->
                               try (string x >> oneOf " \n" >>
                                    lookAhead alphaNum >>
                                    return (return $ B.str
                                                  $ result ++ spacesToNbr x ++ "\160"))) xs)
                           <|> (return $ return $ B.str result)
     else return $ return $ B.str result

-- | if the string matches the beginning of an abbreviation (before
-- the first period, return strings that would finish the abbreviation.
likelyAbbrev :: String -> [String]
likelyAbbrev x =
  let abbrevs = [ "Mr.", "Mrs.", "Ms.", "Capt.", "Dr.", "Prof.",
                  "Gen.", "Gov.", "e.g.", "i.e.", "Sgt.", "St.",
                  "vol.", "vs.", "Sen.", "Rep.", "Pres.", "Hon.",
                  "Rev.", "Ph.D.", "M.D.", "M.A.", "p.", "pp.",
                  "ch.", "sec.", "cf.", "cp."]
      abbrPairs = map (break (=='.')) abbrevs
  in  map snd $ filter (\(y,_) -> y == x) abbrPairs

-- an endline character that can be treated as a space, not a structural break
endline :: MarkdownParser (F Inlines)
endline = try $ do
  newline
  notFollowedBy blankline
  guardEnabled Ext_blank_before_blockquote <|> notFollowedBy emailBlockQuoteStart
  guardEnabled Ext_blank_before_header <|> notFollowedBy (char '#') -- atx header
  -- parse potential list-starts differently if in a list:
  st <- getState
  when (stateParserContext st == ListItemState) $ do
     notFollowedBy' bulletListStart
     notFollowedBy' anyOrderedListStart
  (guardEnabled Ext_hard_line_breaks >> return (return B.linebreak))
    <|> (guardEnabled Ext_ignore_line_breaks >> return mempty)
    <|> (return $ return B.space)

--
-- links
--

-- a reference label for a link
reference :: MarkdownParser (F Inlines, String)
reference = do notFollowedBy' (string "[^")   -- footnote reference
               withRaw $ trimInlinesF <$> inlinesInBalancedBrackets

parenthesizedChars :: MarkdownParser [Char]
parenthesizedChars = do
  result <- charsInBalanced '(' ')' litChar
  return $ '(' : result ++ ")"

-- source for a link, with optional title
source :: MarkdownParser (String, String)
source = do
  char '('
  skipSpaces
  let urlChunk = try $ notFollowedBy (oneOf "\"')") >>
                          (parenthesizedChars <|> count 1 litChar)
  let sourceURL = (unwords . words . concat) <$> many urlChunk
  let betweenAngles = try $
         char '<' >> manyTill litChar (char '>')
  src <- try betweenAngles <|> sourceURL
  tit <- option "" $ try $ spnl >> linkTitle
  skipSpaces
  char ')'
  return (escapeURI $ trimr src, tit)

linkTitle :: MarkdownParser String
linkTitle = quotedTitle '"' <|> quotedTitle '\''

link :: MarkdownParser (F Inlines)
link = try $ do
  st <- getState
  guard $ stateAllowLinks st
  setState $ st{ stateAllowLinks = False }
  (lab,raw) <- reference
  setState $ st{ stateAllowLinks = True }
  regLink B.link lab <|> referenceLink B.link (lab,raw)

regLink :: (String -> String -> Inlines -> Inlines)
        -> F Inlines -> MarkdownParser (F Inlines)
regLink constructor lab = try $ do
  (src, tit) <- source
  return $ constructor src tit <$> lab

-- a link like [this][ref] or [this][] or [this]
referenceLink :: (String -> String -> Inlines -> Inlines)
              -> (F Inlines, String) -> MarkdownParser (F Inlines)
referenceLink constructor (lab, raw) = do
  sp <- (True <$ lookAhead (char ' ')) <|> return False
  (ref,raw') <- try
           (skipSpaces >> optional (newline >> skipSpaces) >> reference)
           <|> return (mempty, "")
  let labIsRef = raw' == "" || raw' == "[]"
  let key = toKey $ if labIsRef then raw else raw'
  parsedRaw <- parseFromString (mconcat <$> many inline) raw'
  fallback <- parseFromString (mconcat <$> many inline) $ dropBrackets raw
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
       Nothing        -> do
         headers <- asksF stateHeaders
         ref' <- if labIsRef then lab else ref
         if implicitHeaderRefs
            then case M.lookup ref' headers of
                   Just ident -> constructor ('#':ident) "" <$> lab
                   Nothing    -> makeFallback
            else makeFallback
       Just (src,tit) -> constructor src tit <$> lab

dropBrackets :: String -> String
dropBrackets = reverse . dropRB . reverse . dropLB
  where dropRB (']':xs) = xs
        dropRB xs = xs
        dropLB ('[':xs) = xs
        dropLB xs = xs

bareURL :: MarkdownParser (F Inlines)
bareURL = try $ do
  guardEnabled Ext_autolink_bare_uris
  (orig, src) <- uri <|> emailAddress
  return $ return $ B.link src "" (B.str orig)

autoLink :: MarkdownParser (F Inlines)
autoLink = try $ do
  char '<'
  (orig, src) <- uri <|> emailAddress
  -- in rare cases, something may remain after the uri parser
  -- is finished, because the uri parser tries to avoid parsing
  -- final punctuation.  for example:  in `<http://hi---there>`,
  -- the URI parser will stop before the dashes.
  extra <- fromEntities <$> manyTill nonspaceChar (char '>')
  return $ return $ B.link (src ++ escapeURI extra) "" (B.str $ orig ++ extra)

image :: MarkdownParser (F Inlines)
image = try $ do
  char '!'
  (lab,raw) <- reference
  defaultExt <- getOption readerDefaultImageExtension
  let constructor src = case takeExtension src of
                              "" -> B.image (addExtension src defaultExt)
                              _  -> B.image src
  regLink constructor lab <|> referenceLink constructor (lab,raw)

note :: MarkdownParser (F Inlines)
note = try $ do
  guardEnabled Ext_footnotes
  ref <- noteMarker
  return $ do
    notes <- asksF stateNotes'
    case lookup ref notes of
        Nothing       -> return $ B.str $ "[^" ++ ref ++ "]"
        Just contents -> do
          st <- askF
          -- process the note in a context that doesn't resolve
          -- notes, to avoid infinite looping with notes inside
          -- notes:
          let contents' = runF contents st{ stateNotes' = [] }
          return $ B.note contents'

inlineNote :: MarkdownParser (F Inlines)
inlineNote = try $ do
  guardEnabled Ext_inline_notes
  char '^'
  contents <- inlinesInBalancedBrackets
  return $ B.note . B.para <$> contents

rawLaTeXInline' :: MarkdownParser (F Inlines)
rawLaTeXInline' = try $ do
  guardEnabled Ext_raw_tex
  lookAhead $ char '\\' >> notFollowedBy' (string "start") -- context env
  RawInline _ s <- rawLaTeXInline
  return $ return $ B.rawInline "tex" s
  -- "tex" because it might be context or latex

rawConTeXtEnvironment :: Parser [Char] st String
rawConTeXtEnvironment = try $ do
  string "\\start"
  completion <- inBrackets (letter <|> digit <|> spaceChar)
               <|> (many1 letter)
  contents <- manyTill (rawConTeXtEnvironment <|> (count 1 anyChar))
                       (try $ string "\\stop" >> string completion)
  return $ "\\start" ++ completion ++ concat contents ++ "\\stop" ++ completion

inBrackets :: (Parser [Char] st Char) -> Parser [Char] st String
inBrackets parser = do
  char '['
  contents <- many parser
  char ']'
  return $ "[" ++ contents ++ "]"

spanHtml :: MarkdownParser (F Inlines)
spanHtml = try $ do
  guardEnabled Ext_markdown_in_html_blocks
  (TagOpen _ attrs, _) <- htmlTag (~== TagOpen "span" [])
  contents <- mconcat <$> manyTill inline (htmlTag (~== TagClose "span"))
  let ident = maybe "" id $ lookup "id" attrs
  let classes = maybe [] words $ lookup "class" attrs
  let keyvals = [(k,v) | (k,v) <- attrs, k /= "id" && k /= "class"]
  return $ B.spanWith (ident, classes, keyvals) <$> contents

divHtml :: MarkdownParser (F Blocks)
divHtml = try $ do
  guardEnabled Ext_markdown_in_html_blocks
  (TagOpen _ attrs, _) <- htmlTag (~== TagOpen "div" [])
  contents <- mconcat <$> manyTill block (htmlTag (~== TagClose "div"))
  let ident = maybe "" id $ lookup "id" attrs
  let classes = maybe [] words $ lookup "class" attrs
  let keyvals = [(k,v) | (k,v) <- attrs, k /= "id" && k /= "class"]
  return $ B.divWith (ident, classes, keyvals) <$> contents

rawHtmlInline :: MarkdownParser (F Inlines)
rawHtmlInline = do
  guardEnabled Ext_raw_html
  mdInHtml <- option False $
                guardEnabled Ext_markdown_in_html_blocks >> return True
  (_,result) <- htmlTag $ if mdInHtml
                             then isInlineTag
                             else not . isTextTag
  return $ return $ B.rawInline "html" result

-- Citations

cite :: MarkdownParser (F Inlines)
cite = do
  guardEnabled Ext_citations
  citations <- textualCite <|> (fmap (flip B.cite unknownC) <$> normalCite)
  return citations

unknownC :: Inlines
unknownC = B.str "???"

textualCite :: MarkdownParser (F Inlines)
textualCite = try $ do
  (_, key) <- citeKey
  let first = Citation{ citationId      = key
                      , citationPrefix  = []
                      , citationSuffix  = []
                      , citationMode    = AuthorInText
                      , citationNoteNum = 0
                      , citationHash    = 0
                      }
  mbrest <- option Nothing $ try $ spnl >> Just <$> normalCite
  case mbrest of
       Just rest -> return $ (flip B.cite unknownC . (first:)) <$> rest
       Nothing   -> (fmap (flip B.cite unknownC) <$> bareloc first) <|>
                    return (do st <- askF
                               return $ case M.lookup key (stateExamples st) of
                                              Just n -> B.str (show n)
                                              _      -> B.cite [first] unknownC)

bareloc :: Citation -> MarkdownParser (F [Citation])
bareloc c = try $ do
  spnl
  char '['
  suff <- suffix
  rest <- option (return []) $ try $ char ';' >> citeList
  spnl
  char ']'
  return $ do
    suff' <- suff
    rest' <- rest
    return $ c{ citationSuffix = B.toList suff' } : rest'

normalCite :: MarkdownParser (F [Citation])
normalCite = try $ do
  char '['
  spnl
  citations <- citeList
  spnl
  char ']'
  return citations

citeKey :: MarkdownParser (Bool, String)
citeKey = try $ do
  suppress_author <- option False (char '-' >> return True)
  char '@'
  first <- letter
  let internal p = try $ p >>~ lookAhead (letter <|> digit)
  rest <- many $ letter <|> digit <|> internal (oneOf ":.#$%&-_+?<>~/")
  let key = first:rest
  return (suppress_author, key)

suffix :: MarkdownParser (F Inlines)
suffix = try $ do
  hasSpace <- option False (notFollowedBy nonspaceChar >> return True)
  spnl
  rest <- trimInlinesF . mconcat <$> many (notFollowedBy (oneOf ";]") >> inline)
  return $ if hasSpace
              then (B.space <>) <$> rest
              else rest

prefix :: MarkdownParser (F Inlines)
prefix = trimInlinesF . mconcat <$>
  manyTill inline (char ']' <|> liftM (const ']') (lookAhead citeKey))

citeList :: MarkdownParser (F [Citation])
citeList = fmap sequence $ sepBy1 citation (try $ char ';' >> spnl)

citation :: MarkdownParser (F Citation)
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

smart :: MarkdownParser (F Inlines)
smart = do
  getOption readerSmart >>= guard
  doubleQuoted <|> singleQuoted <|>
    choice (map (return . B.singleton <$>) [apostrophe, dash, ellipses])

singleQuoted :: MarkdownParser (F Inlines)
singleQuoted = try $ do
  singleQuoteStart
  withQuoteContext InSingleQuote $
    fmap B.singleQuoted . trimInlinesF . mconcat <$>
      many1Till inline singleQuoteEnd

-- doubleQuoted will handle regular double-quoted sections, as well
-- as dialogues with an open double-quote without a close double-quote
-- in the same paragraph.
doubleQuoted :: MarkdownParser (F Inlines)
doubleQuoted = try $ do
  doubleQuoteStart
  contents <- mconcat <$> many (try $ notFollowedBy doubleQuoteEnd >> inline)
  (withQuoteContext InDoubleQuote $ doubleQuoteEnd >> return
       (fmap B.doubleQuoted . trimInlinesF $ contents))
   <|> (return $ return (B.str "\8220") <> contents)

