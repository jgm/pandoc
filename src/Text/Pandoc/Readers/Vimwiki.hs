{-
  Copyright (C) 2017 Yuchen Pei <me@ypei.me>

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
   Module      : Text.Pandoc.Readers.Vimwiki
   Copyright   : Copyright (C) 2017 Yuchen Pei
   License     : GNU GPL, version 2 or above

   Maintainer  : Yuchen Pei <me@ypei.me>
   Stability   : alpha
   Portability : portable

Conversion of vimwiki text to 'Pandoc' document.
-}
{--
 progress:
* block parsers:
    * [X] header
        * [ ] centered header
    * [X] hrule
    * [X] comment
    * [X] blockquote
    * [X] preformatted
        * [ ] with attributes
    * [X] displaymath - a bit buggy
    * [X] bulletlist / orderedlist - a bit buggy with nested lists
        * [ ] orderedlist with 1., i., a) etc identification.
        * [ ] multilines
        * [ ] mixed tab / space indentation
        * [ ] todo lists
    * [X] table
        * [ ] centered table
    * [X] paragraph
    * [ ] definition list
* inline parsers:
    * [X] bareURL
    * [X] strong
    * [X] emph
    * [X] strikeout
    * [X] code
    * [X] link
        * [ ] with thumbnails
    * [X] image
        * [ ] with attributes
    * [X] inline math
    * [X] tag
* misc:
    * [ ] `TODO:` mark
    * [ ] placeholders
        * [ ] %title and %date -> metadata
        * [ ] %template
--}

module Text.Pandoc.Readers.Vimwiki ( readVimwiki
                                 ) where
import Control.Monad.Except (throwError)
import Control.Monad (guard)
import Data.Default -- def is there
import Data.Functor.Identity
import Data.Maybe
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Data.Text (strip)
import Text.Pandoc.Builder (Blocks, Inlines, trimInlines)
import qualified Text.Pandoc.Builder as B (doc, toList, headerWith, str, space, strong, emph, strikeout, code, link, image, spanWith, math, para, horizontalRule, blockQuote, codeBlock, displayMath, bulletList, plain, orderedList, simpleTable)
import Text.Pandoc.Class (PandocMonad, report, PandocIO, runIO)
import Text.Pandoc.Definition (Pandoc, nullAttr, Inline(Space))
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Logging (LogMessage(ParsingTrace))
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Parsing (readWithM, ParserT, stateOptions, ParserState, blanklines, registerHeader, spaceChar, stateAllowLinks, emailAddress, guardEnabled, uri)
import Text.Parsec.Char (spaces, char, anyChar, newline, string, noneOf)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Combinator (eof, choice, many1, manyTill, count, skipMany1, notFollowedBy)
import Text.Parsec.Pos (sourceColumn)
import Text.Parsec.Prim (many, getPosition, try, runParserT)
-- imports for tests
import Text.Parsec.String (Parser)
import Text.Parsec (parse)
import Text.Parsec.Char (oneOf, space)
import Text.Parsec.Combinator (lookAhead, between)
import Text.Parsec.Prim ((<|>), (<?>), skipMany)
import Test.HUnit
import Text.Pandoc.Options (Extension(Ext_autolink_bare_uris))

readVimwiki :: PandocMonad m => ReaderOptions -> String -> m Pandoc
readVimwiki opts s = do
  res <- readWithM parseVimwiki def{ stateOptions = opts } s
  case res of
       Left e -> throwError e
       Right result -> return result

type VwParser = ParserT [Char] ParserState


-- constants

specialChars :: [Char]
specialChars = "=*-#[]_~{`$|:"

-- spaceChar is the parser of only " \t"
-- space and spaces from Parsec.Char are *any* space characters including '\n'
-- newline is '\n'
spaceChars :: [Char] 
spaceChars = " \t\n"

-- main parser

parseVimwiki :: PandocMonad m => VwParser m Pandoc
parseVimwiki = do
  bs <- mconcat <$> many block
  spaces
  eof
  return $ B.doc bs

-- block parser

block :: PandocMonad m => VwParser m Blocks
block = do
  pos <- getPosition
  res <- choice [ mempty <$ blanklines
                , header
                , hrule
                , comment
                , blockQuote
                , preformatted
                , displayMath 
                , mixedList
                , table
                , para
                ]
  report $ ParsingTrace (take 60 $ show $ B.toList res) pos
  return res

header :: PandocMonad m => VwParser m Blocks
hrule :: PandocMonad m => VwParser m Blocks
comment :: PandocMonad m => VwParser m Blocks
displayMath :: PandocMonad m => VwParser m Blocks
para :: PandocMonad m => VwParser m Blocks
mixedList :: PandocMonad m => VwParser m Blocks
orderedList :: PandocMonad m => VwParser m Blocks
table :: PandocMonad m => VwParser m Blocks
blockQuote :: PandocMonad m => VwParser m Blocks
preformatted :: PandocMonad m => VwParser m Blocks

header = try $ do
  many whitespace
  eqs <- many1 (char '=')
  whitespace
  let lev = length eqs
  guard $ lev <= 6
  contents <- trimInlines . mconcat <$> manyTill inline (try $ whitespace >> (string eqs) >> many whitespace >> (char '\n')) -- consider blankline in Parsing to replace many whitespace >> char '\n'
  attr <- registerHeader nullAttr contents
  return $ B.headerWith attr lev contents
para = try $ do
  contents <- trimInlines . mconcat <$> many1 inline
  if all (==Space) contents
     then return mempty
     else return $ B.para contents
hrule = try $ do
  string "----" >> many (char '-') >> newline
  return B.horizontalRule
comment = try $ do
  string "%%" >> many (noneOf "\n") >> newline
  return mempty
blockQuote = try $ do
  string "    " 
  contents <- para
  return $ B.blockQuote contents
preformatted = try $ do
  many spaceChar >> string "{{{" >> many (noneOf "\n") >> lookAhead newline
  contents <- manyTill anyChar (try (char '\n' >> many spaceChar >> string "}}}" >> many spaceChar >> newline))
  if (not $ contents == "") && (head contents == '\n')
     then return $ B.codeBlock (tail contents)
     else return $ B.codeBlock contents
displayMath = try $ do
  many spaceChar >> string "{{$"
  mathTag <- choice [mathTagParser, emptyParser]
  contents <- manyTill anyChar (try (char '\n' >> many spaceChar >> string "}}$" >> many spaceChar >> newline))
  let contentsWithTags
        | mathTag == "" = "\\[" ++ contents ++ "\\]"
        | otherwise     = "\\begin{" ++ mathTag ++ "}" ++ contents ++ "\\end{" ++ mathTag ++ "}"
  return $ B.para $ B.displayMath contentsWithTags

mixedList = try $ do
  (bl, _) <- mixedList' 0
  return $ head bl

-- |mixedList testing:
-- *Main> testP mixedList "* *1 2*\n  *    _4 5_ \n  * https://www.google.com  \n * $a^2$"
-- Right (Many {unMany = fromList [BulletList [[Plain [Strong [Str "1",Space,Str "2"]]],[BulletList [[Plain [Emph [Str "4",Space,Str "5"],Space]],[Plain [Link ("",[],[]) [Str "https://www.google.com"] ("https://www.google.com",""),Space]]]],[Plain [Math InlineMath "a^2"]]]]})

-- FIXME: there is some problem with the list levels, e.g. 
{--
* 1\n            * 1.1 
yields
[BulletList
 [[Plain [Str "1"]
  ,BulletList
   [[Plain [Str "1.1"]]]]]]
in pandoc -f markdown -t native, but here we have
Right (Pandoc (Meta {unMeta = fromList []}) [BulletList [[Plain [Str "1"]],[BulletList [[Plain [Str "1.1"]]]]]])
   --}
mixedList' :: PandocMonad m => Int -> VwParser m ([Blocks], Int)
mixedList' prevLev = do
  listSpaces <- listSpacesParser <|> emptyParser
  let curLev = length listSpaces
  if curLev < prevLev
     then return ([], curLev)
     else do
          many spaceChar  -- change to spaceChar
          c <- oneOf "*-#" 
          many spaceChar  -- change to spaceChar
          curLine <- B.plain <$> mconcat <$> (manyTill inline (char '\n'))
          let listBuilder = fromJust $ listType c
          (subList, lowLev) <- (mixedList' curLev)
          if lowLev >= curLev
             then do
                  (sameLevList, endLev) <- (mixedList' lowLev)
                  let curList = (curLine:subList) ++ sameLevList
                  if curLev > prevLev
                     then return ([listBuilder curList], endLev)
                     else return (curList, endLev)
             else do
                  let (curList, endLev) = (curLine:subList, lowLev)
                  if curLev > prevLev
                     then return ([listBuilder curList], endLev)
                     else return (curList, endLev)
                             --}
--OrderedList (1,DefaultStyle,DefaultDelim) [[Plain [Strong [Str "1",Space,Str "2"]]],[BulletList [[Plain [Emph [Str "4",Space,Str "5"],Space]],[Plain [Link ("",[],[]) [Str "https://www.google.com"] ("https://www.google.com",""),Space]]]],[Plain [Math InlineMath "a^2"]]]
--OrderedList (1,DefaultStyle,DefaultDelim) [[Plain [Str "1"]],[Plain [Str "2"]]]
-- | mixedList' testing:
-- *Main> testP (mixedList' 0) "   * 1\n* 2"
-- Right ([Many {unMany = fromList [BulletList [[Plain [Str "1"]]]]}],1)
-- *Main> testP (mixedList' 0) "* hello\n* hi"
-- Right ([Many {unMany = fromList [BulletList [[Plain [Str "hello"]],[Plain [Str "hi"]]]]}],0)
-- *Main> testP (mixedList' 0) "* 1\n  * 3\n * 2"
-- Right ([Many {unMany = fromList [BulletList [[Plain [Str "1"]],[BulletList [[Plain [Str "3"]]]],[Plain [Str "2"]]]]}],0)

listType :: Char -> Maybe ([Blocks] -> Blocks)
listType '*' = Just B.bulletList
listType '-' = Just B.bulletList
listType '#' = Just B.orderedList
listType _ = Nothing


listSpacesParser :: PandocMonad m => VwParser m String
listSpacesParser = try $ lookAhead $ do
  s <- manyTill spaceChar (oneOf "*-#" >> many spaceChar)
  return $ ' ':s


orderedList = undefined

--many need trimInlines
table = try $ do
  th <- tableRow
  many tableHeaderSeparator
  trs <- many tableRow
  return $ B.simpleTable th trs

-- | table test:
-- *Main> testP table "|a|$b$|\n|_c c_|d|\n|e|f|"
-- Right (Many {unMany = fromList [Table [] [AlignDefault,AlignDefault] [0.0,0.0] [[Plain [Str "a"]],[Plain [Math InlineMath "b"]]] [[[Plain [Emph [Str "c",Space,Str "c"]]],[Plain [Str "d"]]],[[Plain [Str "e"]],[Plain [Str "f"]]]]]})

tableHeaderSeparator :: PandocMonad m => VwParser m ()
tableHeaderSeparator = try $ do
  many spaceChar >> char '|' >> many1 ((many1 $ char '-') >> char '|') >> spaceChar >> char '\n'
  return ()
  
tableRow :: PandocMonad m => VwParser m [Blocks]
tableRow = try $ do
  many spaceChar >> char '|'
  s <- lookAhead $ manyTill anyChar (try (char '|' >> many spaceChar >> char '\n')) -- perhaps the last part can be an end of line parser
  guard $ not $ "||" `isInfixOf` ("|" ++ s ++ "|")
  tr <- many tableCell
  many spaceChar >> char '\n'
  return tr

tableCell :: PandocMonad m => VwParser m Blocks
tableCell = try $ do
  B.plain <$> mconcat <$> (manyTill inline (char '|'))

-- inline parser

inline :: PandocMonad m => VwParser m Inlines
inline = choice[whitespace
             ,  bareURL
             ,  str
             ,  strong
             ,  emph
             ,  strikeout
             ,  code
             ,  link
             ,  image
             ,  inlineMath
             ,  tag
             ,  special
             ]--}

str :: PandocMonad m => VwParser m Inlines
whitespace :: PandocMonad m => VwParser m Inlines
special :: PandocMonad m => VwParser m Inlines
bareURL :: PandocMonad m => VwParser m Inlines
strong :: PandocMonad m => VwParser m Inlines
emph :: PandocMonad m => VwParser m Inlines
strikeout :: PandocMonad m => VwParser m Inlines
code :: PandocMonad m => VwParser m Inlines
link :: PandocMonad m => VwParser m Inlines
image :: PandocMonad m => VwParser m Inlines
inlineMath :: PandocMonad m => VwParser m Inlines
tag :: PandocMonad m => VwParser m Inlines

--str = B.str <$> many1 (noneOf $ specialChars ++ spaceChars)
str = B.str <$> (many1 $ noneOf $ spaceChars ++ specialChars)
whitespace = B.space <$ (skipMany1 spaceChar)
special = B.str <$> count 1 (oneOf specialChars)
bareURL = try $ do
  (orig, src) <- uri <|> emailAddress
  return $ B.link src "" (B.str orig)
--{--
strong = try $ do
  s <- lookAhead $ between (char '*') (char '*') (many1 $ noneOf "*")
  guard $ (not $ (head s) `elem` spaceChars) && (not $ (last s) `elem` spaceChars)
  char '*'
  contents <- mconcat <$> (manyTill inline $ (char '*') >> (lookAhead $ oneOf $ spaceChars ++ specialChars))
  return $ B.strong contents
emph = try $ do
  s <- lookAhead $ between (char '_') (char '_') (many1 $ noneOf "_")
  guard $ (not $ (head s) `elem` spaceChars) && (not $ (last s) `elem` spaceChars)
  char '_'
  contents <- mconcat <$> (manyTill inline $ (char '_') >> (lookAhead $ oneOf $ spaceChars ++ specialChars))
  return $ B.emph contents
strikeout = try $ do
  string "~~"
  contents <- mconcat <$> (manyTill inline $ string $ "~~")
  return $ B.strikeout contents
code = try $ do
  char '`'
  contents <- manyTill anyChar (char '`')
  return $ B.code contents
link = try $ do -- haven't implemented link with thumbnails
  string "[["
  contents <- lookAhead $ manyTill anyChar (string "]]")
  case '|' `elem` contents of 
                  False -> return $ B.link contents "link" (B.str contents)
                  True  -> do 
                    url <- manyTill anyChar $ char '|'
                    lab <- mconcat <$> (manyTill inline $ string "]]")
                    return $ B.link url "link" lab
image = try $ do -- yet to implement one with attributes
  string "{{"
  contents <- manyTill anyChar $ string $ "}}"
  return $ B.image contents "" (B.str "")
inlineMath = try $ do
  char '$'
  contents <- manyTill anyChar (char '$')
  return $ B.math contents
tag = try $ do
  char ':'
  s <- manyTill (noneOf spaceChars) (try (char ':' >> space))
  guard $ not $ "::" `isInfixOf` (":" ++ s ++ ":")
  return $ mconcat $ concat $ (makeTagSpan <$> (splitOn ":" s)) -- returns tag1 >> tag2 >> ... >> tagn

-- helper functions and parsers
{--
splitAtSeparater :: [Char] -> ([Char], [Char])
splitAtSeparater xs = go "" xs
  where 
    go xs ys
      | ys == "" = (xs, ys)
      | head ys == '|' = (xs, tail ys)
      | otherwise = go (xs ++ [head ys]) (tail ys)
      --}

makeTagSpan :: String -> [Inlines]
makeTagSpan s = 
  [B.spanWith ('-' : s, [], []) (B.str ""), B.spanWith (s, ["tag"], []) (B.str s)]

mathTagParser :: PandocMonad m => VwParser m String
mathTagParser = do
  s <- try $ lookAhead (char '%' >> (manyTill (noneOf spaceChars) (try $ char '%' >> many (noneOf $ '%':spaceChars) >> space)))
  char '%' >> string s >> char '%'
  return s

emptyParser :: PandocMonad m => VwParser m String
emptyParser = return ""

  
-- tests

-- *Main> runIO (readVimwiki (def :: ReaderOptions) "==2==" :: PandocIO Pandoc)
-- Right (Pandoc (Meta {unMeta = fromList []}) [Header 2 ("",[],[]) [Str "2"]])

runParser :: VwParser PandocIO a -> String -> PandocIO a
runParser p s = do
  res <- readWithM p def{ stateOptions = def :: ReaderOptions } s
  case res of
       Left e -> throwError e
       Right result -> return result

testP :: VwParser PandocIO a -> String -> IO (Either PandocError a)
testP p s = runIO $ runParser p (s ++ "\n")
--testP p s = runIO $ runParser p s

--type Parser = Parsec String ()
{--runP :: VwParser PandocIO a -> [Char] -> IO (Either PandocError a)
runP p opts s = do
  res <- readWithM p def{ stateOptions = opts } s
  case res of
       Left e -> throwError e
       Right result -> return result--}
--runP p s = runIO (mapLeft (PandocParsecError s) `liftM` runParserT p def{ stateOptions = def :: ReaderOptions } "test" s)
--
simpleParse :: Parser a -> String -> Either ParseError a
simpleParse p s = parse p "" (s ++ "\n")

header' :: Parser String
header' = do
  spaces
  eqs <- many1 $ char '='
  let lev = length eqs
  guard $ lev <= 6
  space
  manyTill anyChar $ try $ space >> string eqs >> (skipMany spaceChar) >> char '\n'


header'' :: Parser [Inlines]
header'' = manyTill (B.str <$> (many1 anyChar)) (string "===")

strikeout' :: Parser String
strikeout' = do
  string "~~"
  manyTill anyChar $ try $ lookAhead $ (string "~~") >> (oneOf $ spaceChars ++ specialChars)

emph' :: Parser String
emph' = do
  char '_'
  lookAhead (noneOf " \t\n")
  x <- manyTill anyChar $ try $ lookAhead $ (noneOf " \t\n") >> (char '_') >> (oneOf $ spaceChars ++ specialChars)
  y <- anyChar
  (return $ x ++ [y]) 

strong' :: Parser String
strong' = do
  s <- between (char '*') (char '*') (many1 $ noneOf "*")
  lookAhead (oneOf $ spaceChars ++ specialChars)
  guard $ (not $ (head s) `elem` spaceChars) && (not $ (last s) `elem` spaceChars)
  return s

testStrong' :: IO Counts
testStrong' = 
  runTestTT $ TestList
    [ TestCase (assertEqual "" (simpleParse strong' "*23*") (Right "23")),
      TestCase (assertEqual "" (simpleParse strong' "*2 3*~   *_") (Right "2 3"))
    ]

testHeader' =
  runTestTT $ TestList
    [ TestCase (assertEqual "" (simpleParse header' " = a b= c =") (Right "a b= c")),
      TestCase (assertEqual "" (simpleParse header' " == a b= c ==") (Right "a b= c")),
      TestCase (assertEqual "" (simpleParse header' " === a b==c ===") (Right "a b==c")),
      TestCase (assertEqual "" (simpleParse header' " = a b =c =") (Right "a b =c"))
    ]
-- other tests: " ======= a b= c =======   ", " === a b= c ====   " fail

{-- ???
testHeader =
  runTestTT $ TestList
    [ TestCase (assertEqual "" (testP header "     = a = b =") (Right (Many {unMany = fromList [Header 1 ("",[],[]) [Str "a",Space,Str "=",Space,Str "b"]]}))),
      TestCase (assertEqual "" (testP header "     ====== a =** b ======") (Right (Many {unMany = fromList [Header 6 ("",[],[]) [Str "a",Space,Str "=**",Space,Str "b"]]}))),
      TestCase (assertEqual "" (testP header "     ======= a =** b =======") (Left (PandocParsecError "     ======= a =** b =======\n" "source" (line 1, column 14):
unexpected "a"))),
      TestCase (assertEqual "" (testP header "     === a = b =") (Left (PandocParsecError "     === a = b =\n" "source" (line 1, column 17):
unexpected "\n"))),
      TestCase (assertEqual "" (testP header "= a = b =a") (Left (PandocParsecError "= a = b =a\n" "source" (line 1, column 11):
unexpected "\n")))
    ]
    --}
