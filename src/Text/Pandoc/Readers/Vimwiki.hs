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
    * [X] hrule
    * [X] comment
    * [X] blockquote 
    * [X] preformatted 
    * [X] displaymath 
    * [X] bulletlist / orderedlist 
        * [ ] orderedlist with 1., i., a) etc identification.
        * [ ] todo lists -- see https://github.com/LarsEKrueger/pandoc-vimwiki
    * [X] table
        * [ ] centered table -- there does not seem to be a table builder in Pandoc.Builder that accepts attributes
        * [O] colspan and rowspan -- pandoc limitation, see issue #1024
    * [X] paragraph
    * [ ] definition list
* inline parsers:
    * [X] bareURL
    * [X] strong
    * [X] emph
    * [X] strikeout
    * [X] code
    * [X] link
    * [X] image
    * [X] inline math
    * [X] tag
    * [ ] sub- and super-scripts
* misc:
    * [X] `TODO:` mark
    * [ ] placeholders: %title and %date -> metadata, %template -> template
--}

module Text.Pandoc.Readers.Vimwiki ( readVimwiki
                                 ) where
import Control.Monad.Except (throwError)
import Control.Monad (guard)
import Data.Default 
import Data.Maybe
import Data.Monoid ((<>))
import Data.List (isInfixOf)
import Text.Pandoc.Builder (Blocks, Inlines, trimInlines, fromList, toList)
import qualified Text.Pandoc.Builder as B (doc, toList, headerWith, str, space, strong, emph, strikeout, code, link, image, spanWith, math, para, horizontalRule, blockQuote, displayMath, bulletList, plain, orderedList, simpleTable, softbreak, codeBlockWith, imageWith)
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Definition (Pandoc, Inline(Space), Block(BulletList, OrderedList), Attr)
import Text.Pandoc.Logging (LogMessage(ParsingTrace))
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Parsing (readWithM, ParserT, stateOptions, ParserState, blanklines, registerHeader, spaceChar, emailAddress, uri)
import Text.Pandoc.Shared (splitBy, stripFirstAndLast, stringify)
import Text.Parsec.Char (spaces, char, anyChar, newline, string, noneOf)
import Text.Parsec.Combinator (eof, choice, many1, manyTill, count, skipMany1, notFollowedBy)
import Text.Parsec.Prim (many, getPosition, try)
import Text.Parsec.Char (oneOf, space)
import Text.Parsec.Combinator (lookAhead, between)
import Text.Parsec.Prim ((<|>))

-- for testing: to REMOVE
import Text.Pandoc.Class (PandocIO, runIO)
import Text.Pandoc.Parsing (ParserT)
import Data.Default
import Text.Parsec.String (Parser)
import Text.Pandoc.Error (PandocError)
import Text.Parsec.Error (ParseError)
import Text.Parsec (parse)

runParser :: VwParser PandocIO a -> String -> PandocIO a
runParser p s = do
  res <- readWithM p def{ stateOptions = def :: ReaderOptions } s
  case res of
       Left e -> throwError e
       Right result -> return result

testP :: VwParser PandocIO a -> String -> IO (Either PandocError a)
testP p s = runIO $ runParser p (s ++ "\n")

simpleParse :: Parser a -> String -> Either ParseError a
simpleParse p s = parse p "" (s ++ "\n")
--end of to REMOVE

readVimwiki :: PandocMonad m => ReaderOptions -> String -> m Pandoc
readVimwiki opts s = do
  res <- readWithM parseVimwiki def{ stateOptions = opts } s
  case res of
       Left e -> throwError e
       Right result -> return result

type VwParser = ParserT [Char] ParserState


-- constants

specialChars :: [Char]
specialChars = "=*-#[]_~{}`$|:%"

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
                , mempty <$ comment
                , mixedList
                , blockQuote
                , preformatted
                , displayMath 
                , table
                , para
                ]
  report $ ParsingTrace (take 60 $ show $ B.toList res) pos -- remove B.
  return res

blockML :: PandocMonad m => VwParser m Blocks
blockML = choice [preformatted, displayMath, table]

header :: PandocMonad m => VwParser m Blocks
header = try $ do
  sp <- many spaceChar 
  eqs <- many1 (char '=')
  spaceChar
  let lev = length eqs
  guard $ lev <= 6
  contents <- trimInlines . mconcat <$> manyTill inline (try $ spaceChar >> (string eqs) >> many spaceChar >> (char '\n')) 
  attr <- registerHeader (makeId contents, (if sp == "" then [] else ["justcenter"]), []) contents
  return $ B.headerWith attr lev contents

para :: PandocMonad m => VwParser m Blocks
para = try $ do
  contents <- trimInlines . mconcat <$> many1 inline
  if all (==Space) (toList contents)
     then return mempty
     else return $ B.para contents

hrule :: PandocMonad m => VwParser m Blocks
hrule = try $ do
  string "----" >> many (char '-') >> newline
  return B.horizontalRule

comment :: PandocMonad m => VwParser m ()
comment = try $ do
  many spaceChar >> string "%%" >> many (noneOf "\n") >> (lookAhead newline)
  return ()

blockQuote :: PandocMonad m => VwParser m Blocks
blockQuote = try $ do
  string "    "
  contents <- trimInlines . mconcat <$> many1 inlineBQ
  if all (==Space) (toList contents)
     then return mempty
     else return $ B.blockQuote $ B.plain contents

preformatted :: PandocMonad m => VwParser m Blocks
preformatted = try $ do
  many spaceChar >> string "{{{" 
  attrText <- many (noneOf "\n") 
  lookAhead newline
  contents <- manyTill anyChar (try (char '\n' >> many spaceChar >> string "}}}" >> many spaceChar >> newline))
  if (not $ contents == "") && (head contents == '\n')
     then return $ B.codeBlockWith (makeAttr attrText) (tail contents)
     else return $ B.codeBlockWith (makeAttr attrText) contents

makeAttr :: String -> Attr
makeAttr s = 
  let xs = splitBy (`elem` " \t") s in
    ("", [], catMaybes $ map nameValue xs)

nameValue :: String -> Maybe (String, String)
nameValue s = 
  let t = splitBy (== '=') s in
    if length t /= 2 
      then Nothing
      else let (a, b) = (head t, last t) in
             if ((length b) < 2) || ((head b, last b) /= ('"', '"'))
               then Nothing
               else Just (a, stripFirstAndLast b)


displayMath :: PandocMonad m => VwParser m Blocks
displayMath = try $ do
  many spaceChar >> string "{{$"
  mathTag <- choice [mathTagParser, emptyParser]
  contents <- manyTill anyChar (try (char '\n' >> many spaceChar >> string "}}$" >> many spaceChar >> newline))
  let contentsWithTags
        | mathTag == "" = "\\[" ++ contents ++ "\n\\]"
        | otherwise     = "\\begin{" ++ mathTag ++ "}" ++ contents ++ "\n\\end{" ++ mathTag ++ "}"
  return $ B.plain $ B.str contentsWithTags

mixedList :: PandocMonad m => VwParser m Blocks
mixedList = try $ do
  (bl, _) <- mixedList' 0
  return $ head bl

mixedList' :: PandocMonad m => Int -> VwParser m ([Blocks], Int)
mixedList' prevLev = do
  listSpaces <- listSpacesParser <|> emptyParser -- use option instead
  let curLev = length listSpaces
  if curLev < prevLev
     then return ([], curLev)
     else do
          many spaceChar  
          c <- oneOf "*-#" 
          --many1 spaceChar  
          --curLine <- B.plain <$> trimInlines . mconcat <$> (many inlineML)
          curLine <- listItemContent
          --return ([curLine], 0)
          let listBuilder = fromJust $ listType c
          (subList, lowLev) <- (mixedList' curLev)
          if lowLev >= curLev
             then do
                  (sameLevList, endLev) <- (mixedList' lowLev)
                  let curList = (combineList curLine subList) ++ sameLevList
                  if curLev > prevLev
                     then return ([listBuilder curList], endLev)
                     else return (curList, endLev)
             else do
                  let (curList, endLev) = ((combineList curLine subList), lowLev)
                  if curLev > prevLev
                     then return ([listBuilder curList], endLev)
                     else return (curList, endLev)
                     --}

plainInlineML' :: PandocMonad m => VwParser m Blocks
plainInlineML' = do
  x <- spaceChar >> (B.plain <$> trimInlines . mconcat <$> (many inlineML))
  newline
  return x

plainInlineML :: PandocMonad m => VwParser m Blocks
plainInlineML = (notFollowedBy listStart) >> plainInlineML'


listItemContent :: PandocMonad m => VwParser m Blocks
listItemContent = try $ do
  x <- plainInlineML'
  y <- many p2
  z <- p6
  return $ mconcat $ x:y++[z]
p2 :: PandocMonad m => VwParser m Blocks
p2 = try $ do -- bbbbbi
  y <- many1 blockML
  x <- plainInlineML
  return $ mconcat $ y ++ [x]
p6 :: PandocMonad m => VwParser m Blocks -- bbbbbbbbb
p6 = try $ (mconcat <$> many blockML)


followedByNewline :: PandocMonad m => VwParser m a -> VwParser m a
followedByNewline ip = do
  t <- ip
  newline
  return t

combineList :: Blocks -> [Blocks] -> [Blocks]
combineList x [y] = case toList y of
                            [BulletList z] -> [fromList $ (toList x) ++ [BulletList z]]
                            [OrderedList attr z] -> [fromList $ (toList x) ++ [OrderedList attr z]]
                            _ -> x:[y]
combineList x xs = x:xs


listType :: Char -> Maybe ([Blocks] -> Blocks)
listType '*' = Just B.bulletList
listType '-' = Just B.bulletList
listType '#' = Just B.orderedList
listType _ = Nothing


listSpacesParser :: PandocMonad m => VwParser m String
listSpacesParser = try $ lookAhead $ do
  s <- listStart
  return $ ' ':s

listStart :: PandocMonad m => VwParser m String
listStart = manyTill spaceChar (oneOf "*-#" >> many1 spaceChar)

--listIdentifier :: [VwParser m String]
--listIdentifier = [string "*", string "-", string "#", string "1." 

--many need trimInlines
table :: PandocMonad m => VwParser m Blocks
table = try $ do
  th <- tableRow
  many tableHeaderSeparator 
  trs <- many tableRow
  return $ B.simpleTable th trs

tableHeaderSeparator :: PandocMonad m => VwParser m ()
tableHeaderSeparator = try $ do
  many spaceChar >> char '|' >> many1 ((many1 $ char '-') >> char '|') >> many spaceChar >> char '\n'
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
inline = choice $ (whitespace endlineP):inlineList

inlineList :: PandocMonad m => [VwParser m Inlines]
inlineList = [  bareURL
             ,  todoMark
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
             ]
-- inline parser for blockquotes
inlineBQ :: PandocMonad m => VwParser m Inlines
inlineBQ = choice $ (whitespace endlineBQ):inlineList

-- inline parser for mixedlists
inlineML :: PandocMonad m => VwParser m Inlines
inlineML = choice $ (whitespace endlineML):inlineList

str :: PandocMonad m => VwParser m Inlines
str = B.str <$> (many1 $ noneOf $ spaceChars ++ specialChars)

whitespace :: PandocMonad m => VwParser m () -> VwParser m Inlines
whitespace endline = B.space <$ (skipMany1 spaceChar <|> (try (newline >> comment)))
         <|> B.softbreak <$ endline

special :: PandocMonad m => VwParser m Inlines
special = B.str <$> count 1 (oneOf specialChars)

bareURL :: PandocMonad m => VwParser m Inlines
bareURL = try $ do
  (orig, src) <- uri <|> emailAddress
  return $ B.link src "" (B.str orig)

strong :: PandocMonad m => VwParser m Inlines
strong = try $ do
  s <- lookAhead $ between (char '*') (char '*') (many1 $ noneOf "*")
  guard $ (not $ (head s) `elem` spaceChars) && (not $ (last s) `elem` spaceChars)
  char '*'
  contents <- mconcat <$> (manyTill inline $ (char '*') >> (lookAhead $ oneOf $ spaceChars ++ specialChars))
  return $ (B.spanWith ((makeId contents), [], []) mempty) <> (B.strong contents)

makeId :: Inlines -> String -- move this function
makeId i = concat (stringify <$> (toList i))

emph :: PandocMonad m => VwParser m Inlines
emph = try $ do
  s <- lookAhead $ between (char '_') (char '_') (many1 $ noneOf "_")
  guard $ (not $ (head s) `elem` spaceChars) && (not $ (last s) `elem` spaceChars)
  char '_'
  contents <- mconcat <$> (manyTill inline $ (char '_') >> (lookAhead $ oneOf $ spaceChars ++ specialChars))
  return $ B.emph contents

strikeout :: PandocMonad m => VwParser m Inlines
strikeout = try $ do
  string "~~"
  contents <- mconcat <$> (manyTill inline $ string $ "~~")
  return $ B.strikeout contents

code :: PandocMonad m => VwParser m Inlines
code = try $ do
  char '`'
  contents <- manyTill anyChar (char '`')
  return $ B.code contents

link :: PandocMonad m => VwParser m Inlines
link = try $ do -- haven't implemented link with thumbnails
  string "[["
  contents <- lookAhead $ manyTill anyChar (string "]]")
  case '|' `elem` contents of 
                  False -> do
                    manyTill anyChar (string "]]") -- try
                    return $ B.link contents "link" (B.str contents)
                  True  -> do 
                    url <- manyTill anyChar $ char '|'
                    lab <- mconcat <$> (manyTill inline $ string "]]")
                    return $ B.link url "link" lab

image :: PandocMonad m => VwParser m Inlines
image = try $ do -- yet to implement one with attributes
  string "{{"
  contentText <- lookAhead $ manyTill (noneOf "\n") (try $ string "}}")
  images $ length $ filter (== '|') contentText

images :: PandocMonad m => Int -> VwParser m Inlines
images k
  | k == 0 = do 
           imgurl <- manyTill anyChar (try $ string "}}")
           return $ B.image imgurl "" (B.str "")
  | k == 1 = do
           imgurl <- manyTill anyChar (char '|')
           alt <- mconcat <$> (manyTill inline $ (try $ string "}}"))
           return $ B.image imgurl "" alt
  | k == 2 = do
           imgurl <- manyTill anyChar (char '|')
           alt <- mconcat <$> (manyTill inline $ char '|')
           attrText <- manyTill anyChar (try $ string "}}")
           return $ B.imageWith (makeAttr attrText) imgurl "" alt
  | otherwise = do
           imgurl <- manyTill anyChar (char '|')
           alt <- mconcat <$> (manyTill inline $ char '|')
           attrText <- manyTill anyChar (char '|')
           manyTill anyChar (try $ string "}}")
           return $ B.imageWith (makeAttr attrText) imgurl "" alt

inlineMath :: PandocMonad m => VwParser m Inlines
inlineMath = try $ do
  char '$'
  contents <- manyTill anyChar (char '$')
  return $ B.str contents

tag :: PandocMonad m => VwParser m Inlines
tag = try $ do
  char ':'
  s <- manyTill (noneOf spaceChars) (try (char ':' >> space))
  guard $ not $ "::" `isInfixOf` (":" ++ s ++ ":")
  return $ mconcat $ concat $ (makeTagSpan <$> (splitBy (==':') s)) 

todoMark :: PandocMonad m => VwParser m Inlines
todoMark = try $ do
  string "TODO:"
  return $ B.spanWith ("", ["todo"], []) (B.str "TODO:")

-- helper functions and parsers
endlineP :: PandocMonad m => VwParser m ()
endlineP = () <$ try (newline <* nFBTTBSB <* notFollowedBy blockQuote)

endlineBQ :: PandocMonad m => VwParser m ()
endlineBQ = () <$ try (newline <* nFBTTBSB <* string "    ")

endlineML :: PandocMonad m => VwParser m ()
endlineML = () <$ try (newline <* nFBTTBSB <* many1 spaceChar)

--- nFBTTBSB is short for notFollowedByThingsThatBreakSoftBreaks 
nFBTTBSB :: PandocMonad m => VwParser m ()
nFBTTBSB =
    notFollowedBy newline <*
    notFollowedBy hrule <*
    notFollowedBy tableRow <*
    notFollowedBy header <*
    notFollowedBy listStart <*
    notFollowedBy preformatted <*
    notFollowedBy displayMath

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
