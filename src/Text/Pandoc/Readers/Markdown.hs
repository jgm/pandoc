{-# LANGUAGE RelaxedPolyRec #-} -- needed for inlinesBetween on GHC < 7
{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright (C) 2006-2015 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2015 John MacFarlane
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
import Data.Scientific (coefficient, base10Exponent)
import Data.Ord ( comparing )
import Data.Char ( isSpace, isAlphaNum, toLower )
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
import Text.Pandoc.Builder (Inlines, Blocks, trimInlines)
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.XML (fromEntities)
import Text.Pandoc.Parsing hiding (tableWith)
import Text.Pandoc.Readers.LaTeX ( rawLaTeXInline, rawLaTeXBlock )
import Text.Pandoc.Readers.HTML ( htmlTag, htmlInBalanced, isInlineTag, isBlockTag,
                                  isTextTag, isCommentTag )
import Control.Monad
import System.FilePath (takeExtension, addExtension)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match (tagOpen)
import qualified Data.Set as Set
import Text.Printf (printf)
import Debug.Trace (trace)
import Text.Pandoc.Compat.Monoid ((<>))
import Text.Pandoc.Error

type MarkdownParser = Parser [Char] ParserState

-- | Read markdown from an input string and return a Pandoc document.
readMarkdown :: ReaderOptions -- ^ Reader options
             -> String        -- ^ String to parse (assuming @'\n'@ line endings)
             -> Either PandocError Pandoc
readMarkdown opts s =
  (readWith parseMarkdown) def{ stateOptions = opts } (s ++ "\n\n")

-- | Read markdown from an input string and return a pair of a Pandoc document
-- and a list of warnings.
readMarkdownWithWarnings :: ReaderOptions -- ^ Reader options
                         -> String        -- ^ String to parse (assuming @'\n'@ line endings)
                         -> Either PandocError (Pandoc, [String])
readMarkdownWithWarnings opts s =
    (readWithWarnings parseMarkdown) def{ stateOptions = opts } (s ++ "\n\n")

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

-- | Succeeds when we're in list context.
inList :: MarkdownParser ()
inList = do
  ctx <- stateParserContext <$> getState
  guard (ctx == ListItemState)

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

-- returns number of spaces parsed
skipNonindentSpaces :: MarkdownParser Int
skipNonindentSpaces = do
  tabStop <- getOption readerTabStop
  atMostSpaces (tabStop - 1) <* notFollowedBy (char ' ')

atMostSpaces :: Int -> MarkdownParser Int
atMostSpaces n
  | n > 0     = (char ' ' >> (+1) <$> atMostSpaces (n-1)) <|> return 0
  | otherwise = return 0

litChar :: MarkdownParser Char
litChar = escapedChar'
       <|> characterReference
       <|> noneOf "\n"
       <|> try (newline >> notFollowedBy blankline >> return ' ')

-- | Parse a sequence of inline elements between square brackets,
-- including inlines between balanced pairs of square brackets.
inlinesInBalancedBrackets :: MarkdownParser (F Inlines)
inlinesInBalancedBrackets = do
  char '['
  (_, raw) <- withRaw $ charsInBalancedBrackets 1
  guard $ not $ null raw
  parseFromString (trimInlinesF . mconcat <$> many inline) (init raw)

charsInBalancedBrackets :: Int -> MarkdownParser ()
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
                     (if B.isNull title' then id else B.setMeta "title" title')
                   . (if null author' then id else B.setMeta "author" author')
                   . (if B.isNull date' then id else B.setMeta "date" date')
                   $ nullMeta
  updateState $ \st -> st{ stateMeta' = stateMeta' st <> meta' }

yamlMetaBlock :: MarkdownParser (F Blocks)
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
  opts <- stateOptions <$> getState
  meta' <- case Yaml.decodeEither' $ UTF8.fromString rawYaml of
                Right (Yaml.Object hashmap) -> return $ return $
                         H.foldrWithKey (\k v m ->
                              if ignorable k
                                 then m
                                 else case yamlToMeta opts v of
                                        Left _  -> m
                                        Right v' -> B.setMeta (T.unpack k) v' m)
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

toMetaValue :: ReaderOptions -> Text -> Either PandocError MetaValue
toMetaValue opts x = toMeta <$> readMarkdown opts' (T.unpack x)
  where
    toMeta p =
      case p of
        Pandoc _ [Plain xs]  -> MetaInlines xs
        Pandoc _ [Para xs]
         | endsWithNewline x -> MetaBlocks [Para xs]
         | otherwise         -> MetaInlines xs
        Pandoc _ bs           -> MetaBlocks bs
    endsWithNewline t = T.pack "\n" `T.isSuffixOf` t
    opts' = opts{readerExtensions=readerExtensions opts `Set.difference` meta_exts}
    meta_exts = Set.fromList [ Ext_pandoc_title_block
                             , Ext_mmd_title_block
                             , Ext_yaml_metadata_block
                             ]

yamlToMeta :: ReaderOptions -> Yaml.Value -> Either PandocError MetaValue
yamlToMeta opts (Yaml.String t) = toMetaValue opts t
yamlToMeta _    (Yaml.Number n)
  -- avoid decimal points for numbers that don't need them:
  | base10Exponent n >= 0     = return $ MetaString $ show
                                $ coefficient n * (10 ^ base10Exponent n)
  | otherwise                 = return $ MetaString $ show n
yamlToMeta _    (Yaml.Bool b) = return $ MetaBool b
yamlToMeta opts (Yaml.Array xs) = B.toMetaValue <$> mapM (yamlToMeta opts)
                                                  (V.toList xs)
yamlToMeta opts (Yaml.Object o) = MetaMap <$> H.foldrWithKey (\k v m ->
                                if ignorable k
                                   then m
                                   else (do
                                    v' <- yamlToMeta opts v
                                    m' <- m
                                    return (M.insert (T.unpack k) v' m')))
                                (return M.empty) o
yamlToMeta _ _ = return $ MetaString ""

stopLine :: MarkdownParser ()
stopLine = try $ (string "---" <|> string "...") >> blankline >> return ()

mmdTitleBlock :: MarkdownParser ()
mmdTitleBlock = try $ do
  guardEnabled Ext_mmd_title_block
  firstPair <- kvPair False
  restPairs <- many (kvPair True)
  let kvPairs = firstPair : restPairs
  blanklines
  updateState $ \st -> st{ stateMeta' = stateMeta' st <>
                             return (Meta $ M.fromList kvPairs) }

kvPair :: Bool -> MarkdownParser (String, MetaValue)
kvPair allowEmpty = try $ do
  key <- many1Till (alphaNum <|> oneOf "_- ") (char ':')
  val <- trim <$> manyTill anyChar
          (try $ newline >> lookAhead (blankline <|> nonspaceChar))
  guard $ allowEmpty || not (null val)
  let key' = concat $ words $ map toLower key
  let val' = MetaBlocks $ B.toList $ B.plain $ B.text $ val
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
  return $ Pandoc meta bs

referenceKey :: MarkdownParser (F Blocks)
referenceKey = try $ do
  pos <- getPosition
  skipNonindentSpaces
  (_,raw) <- reference
  char ':'
  skipSpaces >> optional newline >> skipSpaces >> notFollowedBy (char '[')
  let sourceURL = liftM unwords $ many $ try $ do
                    skipMany spaceChar
                    notFollowedBy' referenceTitle
                    notFollowedBy' (() <$ reference)
                    many1 $ notFollowedBy space >> litChar
  let betweenAngles = try $ char '<' >> manyTill litChar (char '>')
  src <- try betweenAngles <|> sourceURL
  tit <- option "" referenceTitle
  -- currently we just ignore MMD-style link/image attributes
  _kvs <- option [] $ guardEnabled Ext_link_attributes
                      >> many (try $ spnl >> keyValAttr)
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
block = do
  tr <- getOption readerTrace
  pos <- getPosition
  res <- choice [ mempty <$ blanklines
               , codeBlockFenced
               , yamlMetaBlock
               , guardEnabled Ext_latex_macros *> (macro >>= return . return)
               -- note: bulletList needs to be before header because of
               -- the possibility of empty list items: -
               , bulletList
               , header
               , lhsCodeBlock
               , divHtml
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
  when tr $ do
    st <- getState
    trace (printf "line %d: %s" (sourceLine pos)
           (take 60 $ show $ B.toList $ runF res st)) (return ())
  return res

--
-- header blocks
--

header :: MarkdownParser (F Blocks)
header = setextHeader <|> atxHeader <?> "header"

atxChar :: MarkdownParser Char
atxChar = do
  exts <- getOption readerExtensions
  return $ if Set.member Ext_literate_haskell exts
    then '=' else '#'

atxHeader :: MarkdownParser (F Blocks)
atxHeader = try $ do
  level <- atxChar >>= many1 . char >>= return . length
  notFollowedBy $ guardEnabled Ext_fancy_lists >>
                  (char '.' <|> char ')') -- this would be a list
  skipSpaces
  (text, raw) <- withRaw $
          trimInlinesF . mconcat <$> many (notFollowedBy atxClosing >> inline)
  attr <- atxClosing
  attr'@(ident,_,_) <- registerHeader attr (runF text defaultParserState)
  guardDisabled Ext_implicit_header_references
    <|> registerImplicitHeader raw ident
  return $ B.headerWith attr' level <$> text

atxClosing :: MarkdownParser Attr
atxClosing = try $ do
  attr' <- option nullAttr
             (guardEnabled Ext_mmd_header_identifiers >> mmdHeaderIdentifier)
  skipMany . char =<< atxChar
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
  skipSpaces
  (text, raw) <- withRaw $
       trimInlinesF . mconcat <$> many1 (notFollowedBy setextHeaderEnd >> inline)
  attr <- setextHeaderEnd
  underlineChar <- oneOf setextHChars
  many (char underlineChar)
  blanklines
  let level = (fromMaybe 0 $ findIndex (== underlineChar) setextHChars) + 1
  attr'@(ident,_,_) <- registerHeader attr (runF text defaultParserState)
  guardDisabled Ext_implicit_header_references
    <|> registerImplicitHeader raw ident
  return $ B.headerWith attr' level <$> text

registerImplicitHeader :: String -> String -> MarkdownParser ()
registerImplicitHeader raw ident = do
  let key = toKey $ "[" ++ raw ++ "]"
  updateState (\s -> s { stateHeaderKeys =
                         M.insert key ('#':ident,"") (stateHeaderKeys s) })

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
  attrs <- many (attribute <* spnl)
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
  return $ \(id',cs,kvs) ->
    case key of
         "id"    -> (val,cs,kvs)
         "class" -> (id',cs ++ words val,kvs)
         _       -> (id',cs,kvs ++ [(key,val)])

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
           <|> ((\x -> ("",[toLanguageId x],[])) <$> many1 nonspaceChar)
  blankline
  contents <- manyTill anyLine (blockDelimiter (== c) (Just size))
  blanklines
  return $ return $ B.codeBlockWith attr $ intercalate "\n" contents

-- correctly handle github language identifiers
toLanguageId :: String -> String
toLanguageId = map toLower . go
  where go "c++" = "cpp"
        go "objective-c" = "objectivec"
        go x = x

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
emailBlockQuoteStart = try $ skipNonindentSpaces >> char '>' <* optional (char ' ')

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
  startpos <- sourceColumn <$> getPosition
  skipNonindentSpaces
  notFollowedBy' (() <$ hrule)     -- because hrules start out just like lists
  satisfy isBulletListMarker
  endpos <- sourceColumn <$> getPosition
  tabStop <- getOption readerTabStop
  lookAhead (newline <|> spaceChar)
  () <$ atMostSpaces (tabStop - (endpos - startpos))

anyOrderedListStart :: MarkdownParser (Int, ListNumberStyle, ListNumberDelim)
anyOrderedListStart = try $ do
  optional newline -- if preceded by a Plain block in a list context
  startpos <- sourceColumn <$> getPosition
  skipNonindentSpaces
  notFollowedBy $ string "p." >> spaceChar >> digit  -- page number
  res <- do guardDisabled Ext_fancy_lists
            start <- many1 digit >>= safeRead
            char '.'
            return (start, DefaultStyle, DefaultDelim)
     <|> do (num, style, delim) <- anyOrderedListMarker
            -- if it could be an abbreviated first name,
            -- insist on more than one space
            when (delim == Period && (style == UpperAlpha ||
                 (style == UpperRoman &&
                  num `elem` [1, 5, 10, 50, 100, 500, 1000]))) $
               () <$ spaceChar
            return (num, style, delim)
  endpos <- sourceColumn <$> getPosition
  tabStop <- getOption readerTabStop
  lookAhead (newline <|> spaceChar)
  atMostSpaces (tabStop - (endpos - startpos))
  return res

listStart :: MarkdownParser ()
listStart = bulletListStart <|> (anyOrderedListStart >> return ())

listLine :: MarkdownParser String
listLine = try $ do
  notFollowedBy' (do indentSpaces
                     many spaceChar
                     listStart)
  notFollowedByHtmlCloser
  optional (() <$ indentSpaces)
  listLineCommon

listLineCommon :: MarkdownParser String
listLineCommon = concat <$> manyTill
              (  many1 (satisfy $ \c -> c /= '\n' && c /= '<')
             <|> liftM snd (htmlTag isCommentTag)
             <|> count 1 anyChar
              ) newline

-- parse raw text for one list item, excluding start marker and continuations
rawListItem :: MarkdownParser a
            -> MarkdownParser String
rawListItem start = try $ do
  start
  first <- listLineCommon
  rest <- many (notFollowedBy listStart >> notFollowedBy blankline >> listLine)
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

notFollowedByHtmlCloser :: MarkdownParser ()
notFollowedByHtmlCloser = do
  inHtmlBlock <- stateInHtmlBlock <$> getState
  case inHtmlBlock of
        Just t  -> notFollowedBy' $ htmlTag (~== TagClose t)
        Nothing -> return ()

listContinuationLine :: MarkdownParser String
listContinuationLine = try $ do
  notFollowedBy blankline
  notFollowedBy' listStart
  notFollowedByHtmlCloser
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
  unless (style `elem` [DefaultStyle, Decimal, Example] &&
          delim `elem` [DefaultDelim, Period]) $
    guardEnabled Ext_fancy_lists
  when (style == Example) $ guardEnabled Ext_example_lists
  items <- fmap sequence $ many1 $ listItem
                 ( try $ do
                     optional newline -- if preceded by Plain block in a list
                     startpos <- sourceColumn <$> getPosition
                     skipNonindentSpaces
                     res <- orderedListMarker style delim
                     endpos <- sourceColumn <$> getPosition
                     tabStop <- getOption readerTabStop
                     lookAhead (newline <|> spaceChar)
                     atMostSpaces (tabStop - (endpos - startpos))
                     return res )
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
     then try (count remaining (char ' ')) <|> string "\t" <|> many1 spaceChar
     else mzero
  return ()

definitionListItem :: Bool -> MarkdownParser (F (Inlines, [Blocks]))
definitionListItem compact = try $ do
  rawLine' <- anyLine
  raw <- many1 $ defRawBlock compact
  term <- parseFromString (trimInlinesF . mconcat <$> many inline) rawLine'
  contents <- mapM (parseFromString parseBlocks . (++"\n")) raw
  optional blanklines
  return $ liftM2 (,) term (sequence contents)

defRawBlock :: Bool -> MarkdownParser String
defRawBlock compact = try $ do
  hasBlank <- option False $ blankline >> return True
  defListMarker
  firstline <- anyLine
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
  return $ trimr (firstline ++ "\n" ++ unlines rawlines ++ cont) ++
            if hasBlank || not (null cont) then "\n\n" else ""

definitionList :: MarkdownParser (F Blocks)
definitionList = try $ do
  lookAhead (anyLine >>
             optional (blankline >> notFollowedBy (table >> return ())) >>
             -- don't capture table caption as def list!
             defListMarker)
  compactDefinitionList <|> normalDefinitionList

compactDefinitionList :: MarkdownParser (F Blocks)
compactDefinitionList = do
  guardEnabled Ext_compact_definition_lists
  items <- fmap sequence $ many1 $ definitionListItem True
  return $ B.definitionList <$> fmap compactify'DL items

normalDefinitionList :: MarkdownParser (F Blocks)
normalDefinitionList = do
  guardEnabled Ext_definition_lists
  items <- fmap sequence $ many1 $ definitionListItem False
  return $ B.definitionList <$> items

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
htmlElement = rawVerbatimBlock
          <|> strictHtmlBlock
          <|> liftM snd (htmlTag isBlockTag)

htmlBlock :: MarkdownParser (F Blocks)
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

htmlBlock' :: MarkdownParser (F Blocks)
htmlBlock' = try $ do
    first <- htmlElement
    skipMany spaceChar
    optional blanklines
    return $ return $ B.rawBlock "html" first

strictHtmlBlock :: MarkdownParser String
strictHtmlBlock = htmlInBalanced (not . isInlineTag)

rawVerbatimBlock :: MarkdownParser String
rawVerbatimBlock = try $ do
  (TagOpen tag _, open) <- htmlTag (tagOpen (flip elem
                                                  ["pre", "style", "script"])
                              (const True))
  contents <- manyTill anyChar (htmlTag (~== TagClose tag))
  return $ open ++ contents ++ renderTags' [TagClose tag]

rawTeXBlock :: MarkdownParser (F Blocks)
rawTeXBlock = do
  guardEnabled Ext_raw_tex
  result <- (B.rawBlock "latex" . concat <$>
                  rawLaTeXBlock `sepEndBy1` blankline)
        <|> (B.rawBlock "context" . concat <$>
                  rawConTeXtEnvironment `sepEndBy1` blankline)
  spaces
  return $ return result

rawHtmlBlocks :: MarkdownParser (F Blocks)
rawHtmlBlocks = do
  (TagOpen tagtype _, raw) <- htmlTag isBlockTag
  -- try to find closing tag
  -- we set stateInHtmlBlock so that closing tags that can be either block or
  -- inline will not be parsed as inline tags
  oldInHtmlBlock <- stateInHtmlBlock <$> getState
  updateState $ \st -> st{ stateInHtmlBlock = Just tagtype }
  let closer = htmlTag (\x -> x ~== TagClose tagtype)
  contents <- mconcat <$> many (notFollowedBy' closer >> block)
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
  let lengthDashes = length dashes
      lengthSp     = length sp
  return (lengthDashes, lengthDashes + lengthSp)

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
  let lengthDashes = length dashes
  return (lengthDashes, lengthDashes + 1)

gridDashedLines :: Char -> Parser [Char] st [(Int,Int)]
gridDashedLines ch = try $ char '+' >> many1 (gridPart ch) <* blankline

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
                    else map (unlines . map trim) $ transpose
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

pipeBreak :: MarkdownParser ([Alignment], [Int])
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

pipeTable :: MarkdownParser ([Alignment], [Double], F [Blocks], F [[Blocks]])
pipeTable = try $ do
  nonindentSpaces
  lookAhead nonspaceChar
  (heads,(aligns, seplengths)) <- (,) <$> pipeTableRow <*> pipeBreak
  (lines', rawRows) <- unzip <$> many (withRaw pipeTableRow)
  let maxlength = maximum $ map length rawRows
  numColumns <- getOption readerColumns
  let widths = if maxlength > numColumns
                  then map (\len ->
                           fromIntegral (len + 1) / fromIntegral numColumns)
                             seplengths
                  else replicate (length aligns) 0.0
  return $ (aligns, widths, heads, sequence lines')

sepPipe :: MarkdownParser ()
sepPipe = try $ do
  char '|' <|> char '+'
  notFollowedBy blankline

-- parse a row, also returning probable alignments for org-table cells
pipeTableRow :: MarkdownParser (F [Blocks])
pipeTableRow = do
  skipMany spaceChar
  openPipe <- (True <$ char '|') <|> return False
  let cell = mconcat <$>
                 many (notFollowedBy (blankline <|> char '|') >> inline)
  first <- cell
  rest <- many $ sepPipe *> cell
  -- surrounding pipes needed for a one-column table:
  guard $ not (null rest && not openPipe)
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

pipeTableHeaderPart :: Parser [Char] st (Alignment, Int)
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
    <|> (notFollowedByHtmlCloser >> notFollowedBy' (htmlTag isBlockTag))
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
     <|> (return . B.math <$> (mathInline >>= applyMacros')) <+?>
               ((getOption readerSmart >>= guard) *> (return <$> apostrophe)
                <* notFollowedBy space)

-- Parses material enclosed in *s, **s, _s, or __s.
-- Designed to avoid backtracking.
enclosure :: Char
          -> MarkdownParser (F Inlines)
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
             3  -> three c
             2  -> two   c mempty
             1  -> one   c mempty
             _  -> return (return $ B.str cs)

ender :: Char -> Int -> MarkdownParser ()
ender c n = try $ do
  count n (char c)
  guard (c == '*')
    <|> guardDisabled Ext_intraword_underscores
    <|> notFollowedBy alphaNum

-- Parse inlines til you hit one c or a sequence of two cs.
-- If one c, emit emph and then parse two.
-- If two cs, emit strong and then parse one.
-- Otherwise, emit ccc then the results.
three :: Char -> MarkdownParser (F Inlines)
three c = do
  contents <- mconcat <$> many (notFollowedBy (ender c 1) >> inline)
  (ender c 3 >> return ((B.strong . B.emph) <$> contents))
    <|> (ender c 2 >> one c (B.strong <$> contents))
    <|> (ender c 1 >> two c (B.emph <$> contents))
    <|> return (return (B.str [c,c,c]) <> contents)

-- Parse inlines til you hit two c's, and emit strong.
-- If you never do hit two cs, emit ** plus inlines parsed.
two :: Char -> F Inlines -> MarkdownParser (F Inlines)
two c prefix' = do
  contents <- mconcat <$> many (try $ notFollowedBy (ender c 2) >> inline)
  (ender c 2 >> return (B.strong <$> (prefix' <> contents)))
    <|> return (return (B.str [c,c]) <> (prefix' <> contents))

-- Parse inlines til you hit a c, and emit emph.
-- If you never hit a c, emit * plus inlines parsed.
one :: Char -> F Inlines -> MarkdownParser (F Inlines)
one c prefix' = do
  contents <- mconcat <$> many (  (notFollowedBy (ender c 1) >> inline)
                           <|> try (string [c,c] >>
                                    notFollowedBy (ender c 1) >>
                                    two c mempty) )
  (ender c 1 >> return (B.emph <$> (prefix' <> contents)))
    <|> return (return (B.str [c]) <> (prefix' <> contents))

strongOrEmph :: MarkdownParser (F Inlines)
strongOrEmph =  enclosure '*' <|> enclosure '_'

-- | Parses a list of inlines between start and end delimiters.
inlinesBetween :: (Show b)
               => MarkdownParser a
               -> MarkdownParser b
               -> MarkdownParser (F Inlines)
inlinesBetween start end =
  (trimInlinesF . mconcat) <$> try (start >> many1Till inner end)
    where inner      = innerSpace <|> (notFollowedBy' (() <$ whitespace) >> inline)
          innerSpace = try $ whitespace <* notFollowedBy' end

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
  updateLastStrPos
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
  -- parse potential list-starts differently if in a list:
  notFollowedBy (inList >> listStart)
  guardDisabled Ext_lists_without_preceding_blankline <|> notFollowedBy listStart
  guardEnabled Ext_blank_before_blockquote <|> notFollowedBy emailBlockQuoteStart
  guardEnabled Ext_blank_before_header <|> (notFollowedBy . char =<< atxChar) -- atx header
  guardDisabled Ext_backtick_code_blocks <|>
     notFollowedBy (() <$ (lookAhead (char '`') >> codeBlockFenced))
  notFollowedByHtmlCloser
  (eof >> return mempty)
    <|> (guardEnabled Ext_hard_line_breaks >> return (return B.linebreak))
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
  (_,raw') <- option (mempty, "") $
      lookAhead (try (spnl >> normalCite >> return (mempty, "")))
      <|>
      try (spnl >> reference)
  when (raw' == "") $ guardEnabled Ext_shortcut_reference_links
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
       Nothing        ->
         if implicitHeaderRefs
            then do
              headerKeys <- asksF stateHeaderKeys
              case M.lookup key headerKeys of
                   Just (src, tit) -> constructor src tit <$> lab
                   Nothing         -> makeFallback
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
  getState >>= guard . stateAllowLinks
  (orig, src) <- uri <|> emailAddress
  notFollowedBy $ try $ spaces >> htmlTag (~== TagClose "a")
  return $ return $ B.link src "" (B.str orig)

autoLink :: MarkdownParser (F Inlines)
autoLink = try $ do
  getState >>= guard . stateAllowLinks
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
  guardEnabled Ext_native_spans
  (TagOpen _ attrs, _) <- htmlTag (~== TagOpen "span" [])
  contents <- mconcat <$> manyTill inline (htmlTag (~== TagClose "span"))
  let ident = fromMaybe "" $ lookup "id" attrs
  let classes = maybe [] words $ lookup "class" attrs
  let keyvals = [(k,v) | (k,v) <- attrs, k /= "id" && k /= "class"]
  case lookup "style" keyvals of
       Just s | null ident && null classes &&
            map toLower (filter (`notElem` " \t;") s) ==
                 "font-variant:small-caps"
         -> return $ B.smallcaps <$> contents
       _ -> return $ B.spanWith (ident, classes, keyvals) <$> contents

divHtml :: MarkdownParser (F Blocks)
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

rawHtmlInline :: MarkdownParser (F Inlines)
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

-- Citations

cite :: MarkdownParser (F Inlines)
cite = do
  guardEnabled Ext_citations
  citations <- textualCite
            <|> do (cs, raw) <- withRaw normalCite
                   return $ (flip B.cite (B.text raw)) <$> cs
  return citations

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
          lab <- parseFromString (mconcat <$> many inline) $ dropBrackets raw'
          fallback <- referenceLink B.link (lab,raw')
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

bareloc :: Citation -> MarkdownParser (F [Citation])
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

normalCite :: MarkdownParser (F [Citation])
normalCite = try $ do
  char '['
  spnl
  citations <- citeList
  spnl
  char ']'
  return citations

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
    choice (map (return <$>) [apostrophe, dash, ellipses])

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
