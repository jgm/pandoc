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
[X]: implemented
[O]: not implemented
* block parsers:
    * [X] header
    * [X] hrule
    * [X] comment
    * [X] blockquote
    * [X] preformatted -- using codeblock
    * [X] displaymath
    * [X] bulletlist / orderedlist
        * [X] todo lists -- using span.
    * [X] table
        * [X] centered table -- using div
        * [O] colspan and rowspan -- see issue #1024
    * [X] paragraph
    * [X] definition list
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
    * [X] sub- and super-scripts
* misc:
    * [X] `TODO:` mark
    * [X] metadata placeholders: %title and %date
    * [O] control placeholders: %template and %nohtml -- ignored
--}

module Text.Pandoc.Readers.Vimwiki ( readVimwiki
                                 ) where
import Control.Monad.Except (throwError)
import Control.Monad (guard)
import Data.Default
import Data.Maybe
import Data.Monoid ((<>))
import Data.List (isInfixOf, isPrefixOf)
import Data.Text (Text, unpack)
import Text.Pandoc.Builder (Blocks, Inlines, trimInlines, fromList, toList)
import qualified Text.Pandoc.Builder
  as B (headerWith, str, space, strong, emph, strikeout, code, link, image,
        spanWith, para, horizontalRule, blockQuote, bulletList, plain,
        orderedList, simpleTable, softbreak, codeBlockWith, imageWith, divWith,
        setMeta, definitionList, superscript, subscript, displayMath, 
        math)
import Text.Pandoc.Class (PandocMonad(..))
import Text.Pandoc.Definition (Pandoc(..), Inline(Space),
  Block(BulletList, OrderedList), Attr, nullMeta, Meta, ListNumberStyle(..),
  ListNumberDelim(..))
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Parsing (readWithM, ParserT, stateOptions, ParserState,
  stateMeta', blanklines, registerHeader, spaceChar, emailAddress, uri, F, runF,
  orderedListMarker, many1Till)
import Text.Pandoc.Shared (splitBy, stripFirstAndLast, stringify, crFilter)
import Text.Parsec.Char (spaces, char, anyChar, newline, string, noneOf,
  alphaNum)
import Text.Parsec.Combinator (eof, choice, many1, manyTill, count, skipMany1,
  notFollowedBy, option)
import Text.Parsec.Prim (many, try, updateState, getState)
import Text.Parsec.Char (oneOf, space)
import Text.Parsec.Combinator (lookAhead, between)
import Text.Parsec.Prim ((<|>))

readVimwiki :: PandocMonad m => ReaderOptions -> Text -> m Pandoc
readVimwiki opts s = do
  res <- readWithM parseVimwiki def{ stateOptions = opts }
            (unpack (crFilter s))
  case res of
       Left e -> throwError e
       Right result -> return result

type VwParser = ParserT [Char] ParserState


-- constants

specialChars :: [Char]
specialChars = "=*-#[]_~{}`$|:%^,"

spaceChars :: [Char]
spaceChars = " \t\n"

-- main parser

parseVimwiki :: PandocMonad m => VwParser m Pandoc
parseVimwiki = do
  bs <- mconcat <$> many block
  spaces
  eof
  st <- getState
  let meta = runF (stateMeta' st) st
  return $ Pandoc meta (toList bs)

-- block parser

block :: PandocMonad m => VwParser m Blocks
block = do
  res <- choice [ mempty <$ blanklines
                , header
                , hrule
                , mempty <$ comment
                , mixedList
                , preformatted
                , displayMath
                , table
                , mempty <$ placeholder
                , blockQuote
                , definitionList
                , para
                ]
  trace (take 60 $ show $ toList res)
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
  contents <- trimInlines . mconcat <$> manyTill inline (try $ spaceChar
    >> (string eqs) >> many spaceChar >> newline)
  attr <- registerHeader (makeId contents,
    (if sp == "" then [] else ["justcenter"]), []) contents
  return $ B.headerWith attr lev contents

para :: PandocMonad m => VwParser m Blocks
para = try $ do
  contents <- trimInlines . mconcat <$> many1 inline
  if all (==Space) (toList contents)
     then return mempty
     else return $ B.para contents

hrule :: PandocMonad m => VwParser m Blocks
hrule = try $ B.horizontalRule <$ (string "----" >> many (char '-') >> newline)

comment :: PandocMonad m => VwParser m ()
comment = try $ do
  many spaceChar >> string "%%" >> many (noneOf "\n")
  return ()

blockQuote :: PandocMonad m => VwParser m Blocks
blockQuote = try $ do
  string "    "
  contents <- trimInlines . mconcat <$> many1 inlineBQ
  if all (==Space) (toList contents)
     then return mempty
     else return $ B.blockQuote $ B.plain contents

definitionList :: PandocMonad m => VwParser m Blocks
definitionList = try $
  B.definitionList <$> (many1 (dlItemWithDT <|> dlItemWithoutDT))

dlItemWithDT :: PandocMonad m => VwParser m (Inlines, [Blocks])
dlItemWithDT = do
  dt <- definitionTerm
  dds <- many definitionDef
  return $ (dt, dds)

dlItemWithoutDT :: PandocMonad m => VwParser m (Inlines, [Blocks])
dlItemWithoutDT = do
  dds <- many1 definitionDef
  return $ (mempty, dds)

definitionDef :: PandocMonad m => VwParser m Blocks
definitionDef = try $
  (notFollowedBy definitionTerm) >> many spaceChar
    >> (definitionDef1 <|> definitionDef2)

definitionDef1 :: PandocMonad m => VwParser m Blocks
definitionDef1 = try $ mempty <$ defMarkerE

definitionDef2 :: PandocMonad m => VwParser m Blocks
definitionDef2 = try $ B.plain <$>
  (defMarkerM >> (trimInlines . mconcat <$> many inline') <* newline)


definitionTerm :: PandocMonad m => VwParser m Inlines
definitionTerm = try $ do
  x <- definitionTerm1 <|> definitionTerm2
  guard $ (stringify x /= "")
  return x

definitionTerm1 :: PandocMonad m => VwParser m Inlines
definitionTerm1 = try $
  trimInlines . mconcat <$> manyTill inline' (try $ defMarkerE)

definitionTerm2 :: PandocMonad m => VwParser m Inlines
definitionTerm2 = try $ trimInlines . mconcat <$> manyTill inline'
  (try $ lookAhead $ (defMarkerM >> notFollowedBy hasDefMarkerM))

defMarkerM :: PandocMonad m => VwParser m Char
defMarkerM = string "::" >> spaceChar

defMarkerE :: PandocMonad m => VwParser m Char
defMarkerE = string "::" >> newline

hasDefMarkerM :: PandocMonad m => VwParser m String
hasDefMarkerM = manyTill (noneOf "\n") (try defMarkerM)

preformatted :: PandocMonad m => VwParser m Blocks
preformatted = try $ do
  many spaceChar >> string "{{{"
  attrText <- many (noneOf "\n")
  lookAhead newline
  contents <- manyTill anyChar (try (char '\n' >> many spaceChar >> string "}}}"
    >> many spaceChar >> newline))
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
  mathTag <- option "" mathTagParser
  many space
  contents <- manyTill anyChar (try (char '\n' >> many spaceChar >> string "}}$"
    >> many spaceChar >> newline))
  let contentsWithTags
        | mathTag == "" = contents
        | otherwise     = "\\begin{" ++ mathTag ++ "}\n" ++ contents
                          ++ "\n\\end{" ++ mathTag ++ "}"
  return $ B.para $ B.displayMath contentsWithTags


mathTagLaTeX :: String -> String
mathTagLaTeX s = case s of
   "equation" -> ""
   "equation*" -> ""
   "gather" -> "gathered"
   "gather*" -> "gathered"
   "multline" -> "gathered"
   "multline*" -> "gathered"
   "eqnarray" -> "aligned"
   "eqnarray*" -> "aligned"
   "align" -> "aligned"
   "align*" -> "aligned"
   "alignat" -> "aligned"
   "alignat*" -> "aligned"
   _ -> s


mixedList :: PandocMonad m => VwParser m Blocks
mixedList = try $ do
  (bl, _) <- mixedList' (-1)
  return $ head bl

mixedList' :: PandocMonad m => Int -> VwParser m ([Blocks], Int)
mixedList' prevInd = do
  (curInd, builder) <- option (-1, "na") (lookAhead listStart)
  if curInd < prevInd
     then return ([], curInd)
     else do
          listStart
          curLine <- listItemContent
          let listBuilder =
                if builder == "ul" then B.bulletList else B.orderedList
          (subList, lowInd) <- (mixedList' curInd)
          if lowInd >= curInd
             then do
                  (sameIndList, endInd) <- (mixedList' lowInd)
                  let curList = (combineList curLine subList) ++ sameIndList
                  if curInd > prevInd
                     then return ([listBuilder curList], endInd)
                     else return (curList, endInd)
             else do
                  let (curList, endInd) = ((combineList curLine subList),
                                           lowInd)
                  if curInd > prevInd
                     then return ([listBuilder curList], endInd)
                     else return (curList, endInd)

plainInlineML' :: PandocMonad m => Inlines -> VwParser m Blocks
plainInlineML' w = do
  xs <- many inlineML
  newline
  return $ B.plain $ trimInlines $ mconcat $ w:xs

plainInlineML :: PandocMonad m => VwParser m Blocks
plainInlineML = (notFollowedBy listStart) >> spaceChar >> plainInlineML' mempty


listItemContent :: PandocMonad m => VwParser m Blocks
listItemContent = try $ do
  w <- option mempty listTodoMarker
  x <- plainInlineML' w
  y <- many blocksThenInline
  z <- many blockML
  return $ mconcat $ x:y ++ z

blocksThenInline :: PandocMonad m => VwParser m Blocks
blocksThenInline = try $ do
  y <- many1 blockML
  x <- plainInlineML
  return $ mconcat $ y ++ [x]

listTodoMarker :: PandocMonad m => VwParser m Inlines
listTodoMarker = try $ do
  x <- between (many spaceChar >> char '[') (char ']' >> spaceChar)
    (oneOf " .oOX")
  return $ makeListMarkerSpan x

makeListMarkerSpan :: Char -> Inlines
makeListMarkerSpan x =
  let cl = case x of
            ' ' -> "done0"
            '.' -> "done1"
            'o' -> "done2"
            'O' -> "done3"
            'X' -> "done4"
            _   -> ""
    in
      B.spanWith ("", [cl], []) mempty

combineList :: Blocks -> [Blocks] -> [Blocks]
combineList x [y] = case toList y of
                            [BulletList z] -> [fromList $ (toList x)
                                              ++ [BulletList z]]
                            [OrderedList attr z] -> [fromList $ (toList x)
                                                    ++ [OrderedList attr z]]
                            _ -> x:[y]
combineList x xs = x:xs

listStart :: PandocMonad m => VwParser m (Int, String)
listStart = try $ do
  s <- many spaceChar
  listType <- bulletListMarkers <|> orderedListMarkers
  spaceChar
  return (length s, listType)

bulletListMarkers :: PandocMonad m => VwParser m String
bulletListMarkers = "ul" <$ (char '*' <|> char '-')

orderedListMarkers :: PandocMonad m => VwParser m String
orderedListMarkers =
  ("ol" <$ (choice $ (orderedListMarker Decimal Period):(($OneParen)
    <$> orderedListMarker
    <$> [Decimal, LowerRoman, UpperRoman, LowerAlpha, UpperAlpha])))
    <|> ("ol" <$ char '#')

--many need trimInlines
table :: PandocMonad m => VwParser m Blocks
table = try $ do
  indent <- lookAhead (many spaceChar)
  (th, trs) <- table1 <|> table2
  let tab = B.simpleTable th trs
  if indent == ""
    then return tab
    else return $ B.divWith ("", ["center"], []) tab

-- table with header
table1 :: PandocMonad m => VwParser m ([Blocks], [[Blocks]])
table1 = try $ do
  th <- tableRow
  many1 tableHeaderSeparator
  trs <- many tableRow
  return (th, trs)

-- headerless table
table2 :: PandocMonad m => VwParser m ([Blocks], [[Blocks]])
table2 = try $ do
  trs <- many1 tableRow
  return (take (length $ head trs) $ repeat mempty, trs)

tableHeaderSeparator :: PandocMonad m => VwParser m ()
tableHeaderSeparator = try $ do
  many spaceChar >> char '|' >> many1 ((many1 $ char '-') >> char '|')
    >> many spaceChar >> newline
  return ()

tableRow :: PandocMonad m => VwParser m [Blocks]
tableRow = try $ do
  many spaceChar >> char '|'
  s <- lookAhead $ manyTill anyChar (try (char '|' >> many spaceChar
    >> newline))
  guard $ not $ "||" `isInfixOf` ("|" ++ s ++ "|")
  tr <- many tableCell
  many spaceChar >> char '\n'
  return tr

tableCell :: PandocMonad m => VwParser m Blocks
tableCell = try $
  B.plain <$> trimInlines . mconcat <$> (manyTill inline' (char '|'))

placeholder :: PandocMonad m => VwParser m ()
placeholder = try $
  (choice (ph <$> ["title", "date"])) <|> noHtmlPh <|> templatePh

ph :: PandocMonad m => String -> VwParser m ()
ph s = try $ do
  many spaceChar >> (string $ '%':s) >> spaceChar
  contents <- (trimInlines . mconcat <$> (manyTill inline (lookAhead newline)))
    --use lookAhead because of placeholder in the whitespace parser
  let meta' = return $ B.setMeta s contents nullMeta :: F Meta
  updateState $ \st -> st { stateMeta' = stateMeta' st <> meta' }

noHtmlPh :: PandocMonad m => VwParser m ()
noHtmlPh = try $
  () <$ (many spaceChar >> string "%nohtml" >> many spaceChar
    >> (lookAhead newline))

templatePh :: PandocMonad m => VwParser m ()
templatePh = try $
  () <$ (many spaceChar >> string "%template" >> (many $ noneOf "\n")
    >> (lookAhead newline))

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
             ,  superscript
             ,  subscript
             ,  special
             ]

-- inline parser without softbreaks or comment breaks
inline' :: PandocMonad m => VwParser m Inlines
inline' = choice $ whitespace':inlineList

-- inline parser for blockquotes
inlineBQ :: PandocMonad m => VwParser m Inlines
inlineBQ = choice $ (whitespace endlineBQ):inlineList

-- inline parser for mixedlists
inlineML :: PandocMonad m => VwParser m Inlines
inlineML = choice $ (whitespace endlineML):inlineList

str :: PandocMonad m => VwParser m Inlines
str = B.str <$> (many1 $ noneOf $ spaceChars ++ specialChars)

whitespace :: PandocMonad m => VwParser m () -> VwParser m Inlines
whitespace endline = B.space <$ (skipMany1 spaceChar <|>
                                 (try (newline >> (comment <|> placeholder))))
         <|> B.softbreak <$ endline

whitespace' :: PandocMonad m => VwParser m Inlines
whitespace' = B.space <$ skipMany1 spaceChar

special :: PandocMonad m => VwParser m Inlines
special = B.str <$> count 1 (oneOf specialChars)

bareURL :: PandocMonad m => VwParser m Inlines
bareURL = try $ do
  (orig, src) <- uri <|> emailAddress
  return $ B.link src "" (B.str orig)

strong :: PandocMonad m => VwParser m Inlines
strong = try $ do
  s <- lookAhead $ between (char '*') (char '*') (many1 $ noneOf "*")
  guard $ (not $ (head s) `elem` spaceChars)
             && (not $ (last s) `elem` spaceChars)
  char '*'
  contents <- mconcat <$> (manyTill inline' $ char '*'
    >> notFollowedBy alphaNum)
  return $ (B.spanWith ((makeId contents), [], []) mempty)
    <> (B.strong contents)

makeId :: Inlines -> String
makeId i = concat (stringify <$> (toList i))

emph :: PandocMonad m => VwParser m Inlines
emph = try $ do
  s <- lookAhead $ between (char '_') (char '_') (many1 $ noneOf "_")
  guard $ (not $ (head s) `elem` spaceChars)
          && (not $ (last s) `elem` spaceChars)
  char '_'
  contents <- mconcat <$> (manyTill inline' $ char '_'
    >> notFollowedBy alphaNum)
  return $ B.emph contents

strikeout :: PandocMonad m => VwParser m Inlines
strikeout = try $ do
  string "~~"
  contents <- mconcat <$> (many1Till inline' $ string $ "~~")
  return $ B.strikeout contents

code :: PandocMonad m => VwParser m Inlines
code = try $ do
  char '`'
  contents <- many1Till (noneOf "\n") (char '`')
  return $ B.code contents

superscript :: PandocMonad m => VwParser m Inlines
superscript = try $
  B.superscript <$> mconcat <$> (char '^' >> many1Till inline' (char '^'))

subscript :: PandocMonad m => VwParser m Inlines
subscript = try $
  B.subscript <$> mconcat <$> (string ",,"
    >> many1Till inline' (try $ string ",,"))

link :: PandocMonad m => VwParser m Inlines
link = try $ do
  string "[["
  contents <- lookAhead $ manyTill anyChar (string "]]")
  case '|' `elem` contents of
                  False -> do
                    manyTill anyChar (string "]]")
-- not using try here because [[hell]o]] is not rendered as a link in vimwiki
                    return $ B.link (procLink contents) "" (B.str contents)
                  True  -> do
                    url <- manyTill anyChar $ char '|'
                    lab <- mconcat <$> (manyTill inline $ string "]]")
                    return $ B.link (procLink url) "" lab

image :: PandocMonad m => VwParser m Inlines
image = try $ do
  string "{{"
  contentText <- lookAhead $ manyTill (noneOf "\n") (try $ string "}}")
  images $ length $ filter (== '|') contentText

images :: PandocMonad m => Int -> VwParser m Inlines
images k
  | k == 0 = do
           imgurl <- manyTill anyChar (try $ string "}}")
           return $ B.image (procImgurl imgurl) "" (B.str "")
  | k == 1 = do
           imgurl <- manyTill anyChar (char '|')
           alt <- mconcat <$> (manyTill inline $ (try $ string "}}"))
           return $ B.image (procImgurl imgurl) "" alt
  | k == 2 = do
           imgurl <- manyTill anyChar (char '|')
           alt <- mconcat <$> (manyTill inline $ char '|')
           attrText <- manyTill anyChar (try $ string "}}")
           return $ B.imageWith (makeAttr attrText) (procImgurl imgurl) "" alt
  | otherwise = do
           imgurl <- manyTill anyChar (char '|')
           alt <- mconcat <$> (manyTill inline $ char '|')
           attrText <- manyTill anyChar (char '|')
           manyTill anyChar (try $ string "}}")
           return $ B.imageWith (makeAttr attrText) (procImgurl imgurl) "" alt

procLink' :: String -> String
procLink' s
  | ((take 6 s) == "local:") = "file" ++ (drop 5 s)
  | ((take 6 s) == "diary:") = "diary/" ++ (drop 6 s) ++ ".html"
  | or ((`isPrefixOf` s) <$> [ "http:", "https:", "ftp:", "file:", "mailto:",
                              "news:", "telnet:" ])
                             = s
  | s == ""                  = ""
  | (last s) == '/'          = s
  | otherwise                = s ++ ".html"

procLink :: String -> String
procLink s = procLink' x ++ y
  where (x, y) = break (=='#') s

procImgurl :: String -> String
procImgurl s = if ((take 6 s) == "local:") then "file" ++ (drop 5 s) else s

inlineMath :: PandocMonad m => VwParser m Inlines
inlineMath = try $ do
  char '$'
  contents <- many1Till (noneOf "\n") (char '$')
  return $ B.math contents

tag :: PandocMonad m => VwParser m Inlines
tag = try $ do
  char ':'
  s <- manyTill (noneOf spaceChars) (try (char ':' >> (lookAhead space)))
  guard $ not $ "::" `isInfixOf` (":" ++ s ++ ":")
  let ss = splitBy (==':') s
  return $ mconcat $ (makeTagSpan' $ head ss):(makeTagSpan <$> (tail ss))

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
    notFollowedBy displayMath <*
    notFollowedBy hasDefMarker

hasDefMarker :: PandocMonad m => VwParser m ()
hasDefMarker = () <$ (manyTill (noneOf "\n") (string "::" >> oneOf spaceChars))

makeTagSpan' :: String -> Inlines
makeTagSpan' s = B.spanWith ('-' : s, [], []) (B.str "") <>
                  B.spanWith (s, ["tag"], []) (B.str s)

makeTagSpan :: String -> Inlines
makeTagSpan s = (B.space) <> (makeTagSpan' s)

mathTagParser :: PandocMonad m => VwParser m String
mathTagParser = do
  s <- try $ lookAhead (char '%' >> (manyTill (noneOf spaceChars)
    (try $ char '%' >> many (noneOf $ '%':spaceChars) >> space)))
  char '%' >> string s >> char '%'
  return $ mathTagLaTeX s
