{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{- |
   Module      : Text.Pandoc.Readers.Vimwiki
   Copyright   : Copyright (C) 2017-2020 Yuchen Pei
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
import Control.Monad (guard)
import Control.Monad.Except (throwError)
import Data.Default
import Data.List (isInfixOf)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Builder (Blocks, Inlines, fromList, toList, trimInlines)
import qualified Text.Pandoc.Builder as B (blockQuote, bulletList, code,
                                           codeBlockWith, definitionList,
                                           displayMath, divWith, emph,
                                           headerWith, horizontalRule, image,
                                           imageWith, link, math, orderedList,
                                           para, plain, setMeta, simpleTable,
                                           softbreak, space, spanWith, str,
                                           strikeout, strong, subscript,
                                           superscript)
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Definition (Attr, Block (BulletList, OrderedList),
                               Inline (Space), ListNumberDelim (..),
                               ListNumberStyle (..), Pandoc (..),
                               nullMeta)
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Parsing (ParserState, ParserT, blanklines, emailAddress,
                            many1Till, orderedListMarker, readWithM,
                            registerHeader, spaceChar, stateMeta,
                            stateOptions, uri, manyTillChar, manyChar, textStr,
                            many1Char, countChar, many1TillChar,
                            alphaNum, anyChar, char, newline, noneOf, oneOf,
                            space, spaces, string)
import Text.Pandoc.Sources (ToSources(..), Sources)
import Text.Pandoc.Shared (splitTextBy, stringify, stripFirstAndLast,
                           isURI, tshow)
import Text.Parsec.Combinator (between, choice, eof, lookAhead, many1,
                               manyTill, notFollowedBy, option, skipMany1)
import Text.Parsec.Prim (getState, many, try, updateState, (<|>))

readVimwiki :: (PandocMonad m, ToSources a)
            => ReaderOptions
            -> a
            -> m Pandoc
readVimwiki opts s = do
  let sources = toSources s
  res <- readWithM parseVimwiki def{ stateOptions = opts } sources
  case res of
       Left e       -> throwError e
       Right result -> return result

type VwParser = ParserT Sources ParserState


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
  let meta = stateMeta st
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
  trace (T.take 60 $ tshow $ toList res)
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
    >> string eqs >> many spaceChar >> newline)
  attr <- registerHeader (makeId contents,
    ["justcenter" | not (null sp)], []) contents
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
  B.definitionList <$> many1 (dlItemWithDT <|> dlItemWithoutDT)

dlItemWithDT :: PandocMonad m => VwParser m (Inlines, [Blocks])
dlItemWithDT = do
  dt <- definitionTerm
  dds <- many definitionDef
  return (dt, dds)

dlItemWithoutDT :: PandocMonad m => VwParser m (Inlines, [Blocks])
dlItemWithoutDT = do
  dds <- many1 definitionDef
  return (mempty, dds)

definitionDef :: PandocMonad m => VwParser m Blocks
definitionDef = try $
  notFollowedBy definitionTerm >> many spaceChar
    >> (definitionDef1 <|> definitionDef2)

definitionDef1 :: PandocMonad m => VwParser m Blocks
definitionDef1 = try $ mempty <$ defMarkerE

definitionDef2 :: PandocMonad m => VwParser m Blocks
definitionDef2 = try $ B.plain <$>
  (defMarkerM >> (trimInlines . mconcat <$> many inline') <* newline)


definitionTerm :: PandocMonad m => VwParser m Inlines
definitionTerm = try $ do
  x <- definitionTerm1 <|> definitionTerm2
  guard (stringify x /= "")
  return x

definitionTerm1 :: PandocMonad m => VwParser m Inlines
definitionTerm1 = try $
  trimInlines . mconcat <$> manyTill inline' (try defMarkerE)

definitionTerm2 :: PandocMonad m => VwParser m Inlines
definitionTerm2 = try $ trimInlines . mconcat <$> manyTill inline'
  (try $lookAhead (defMarkerM >> notFollowedBy hasDefMarkerM))

defMarkerM :: PandocMonad m => VwParser m Char
defMarkerM = string "::" >> spaceChar

defMarkerE :: PandocMonad m => VwParser m Char
defMarkerE = string "::" >> newline

hasDefMarkerM :: PandocMonad m => VwParser m Text
hasDefMarkerM = manyTillChar (noneOf "\n") (try defMarkerM)

preformatted :: PandocMonad m => VwParser m Blocks
preformatted = try $ do
  many spaceChar >> string "{{{"
  attrText <- manyChar (noneOf "\n")
  lookAhead newline
  contents <- manyTillChar anyChar (try (char '\n' >> many spaceChar >> string "}}}"
    >> many spaceChar >> newline))
  if (contents /= "") && (T.head contents == '\n')
     then return $ B.codeBlockWith (makeAttr attrText) (T.tail contents)
     else return $ B.codeBlockWith (makeAttr attrText) contents

makeAttr :: Text -> Attr
makeAttr s =
  let xs = splitTextBy (`elem` (" \t" :: String)) s in
    ("", syntax xs, mapMaybe nameValue xs)

syntax :: [Text] -> [Text]
syntax (s:_) | not $ T.isInfixOf "=" s = [s]
syntax _ = []

nameValue :: Text -> Maybe (Text, Text)
nameValue s =
  let t = splitTextBy (== '=') s in
    if length t /= 2
      then Nothing
      else let (a, b) = (head t, last t) in
             if (T.length b < 2) || ((T.head b, T.last b) /= ('"', '"'))
               then Nothing
               else Just (a, stripFirstAndLast b)


displayMath :: PandocMonad m => VwParser m Blocks
displayMath = try $ do
  many spaceChar >> string "{{$"
  mathTag <- option "" mathTagParser
  many space
  contents <- manyTillChar anyChar (try (char '\n' >> many spaceChar >> string "}}$"
    >> many spaceChar >> newline))
  let contentsWithTags
        | mathTag == "" = contents
        | otherwise     = "\\begin{" <> mathTag <> "}\n" <> contents
                          <> "\n\\end{" <> mathTag <> "}"
  return $ B.para $ B.displayMath contentsWithTags


mathTagLaTeX :: Text -> Text
mathTagLaTeX s = case s of
   "equation"  -> ""
   "equation*" -> ""
   "gather"    -> "gathered"
   "gather*"   -> "gathered"
   "multline"  -> "gathered"
   "multline*" -> "gathered"
   "eqnarray"  -> "aligned"
   "eqnarray*" -> "aligned"
   "align"     -> "aligned"
   "align*"    -> "aligned"
   "alignat"   -> "aligned"
   "alignat*"  -> "aligned"
   _           -> s


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
          (subList, lowInd) <- mixedList' curInd
          if lowInd >= curInd
             then do
                  (sameIndList, endInd) <- mixedList' lowInd
                  let curList = combineList curLine subList ++ sameIndList
                  if curInd > prevInd
                     then return ([listBuilder curList], endInd)
                     else return (curList, endInd)
             else do
                  let (curList, endInd) = (combineList curLine subList,
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
plainInlineML = notFollowedBy listStart >> spaceChar >> plainInlineML' mempty


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
                            [BulletList z] -> [fromList $ toList x
                                              ++ [BulletList z]]
                            [OrderedList attr z] -> [fromList $ toList x
                                                    ++ [OrderedList attr z]]
                            _ -> x:[y]
combineList x xs = x:xs

listStart :: PandocMonad m => VwParser m (Int, Text)
listStart = try $ do
  s <- many spaceChar
  listType <- bulletListMarkers <|> orderedListMarkers
  spaceChar
  return (length s, listType)

bulletListMarkers :: PandocMonad m => VwParser m Text
bulletListMarkers = "ul" <$ (char '*' <|> char '-')

orderedListMarkers :: PandocMonad m => VwParser m Text
orderedListMarkers =
  ("ol" <$choice (orderedListMarker Decimal Period:(($OneParen) . orderedListMarker <$> [Decimal, LowerRoman, UpperRoman, LowerAlpha, UpperAlpha])))
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
  return (replicate (length $ head trs) mempty, trs)

tableHeaderSeparator :: PandocMonad m => VwParser m ()
tableHeaderSeparator = try $ do
  many spaceChar >> char '|' >> many1 (many1 (char '-') >> char '|')
    >> many spaceChar >> newline
  return ()

tableRow :: PandocMonad m => VwParser m [Blocks]
tableRow = try $ do
  many spaceChar >> char '|'
  s <- lookAhead $ manyTill anyChar (try (char '|' >> many spaceChar
    >> newline))
  guard $ not $ "||" `isInfixOf` ("|" ++ s ++ "|")
  many tableCell <* many spaceChar <* char '\n'

tableCell :: PandocMonad m => VwParser m Blocks
tableCell = try $
  B.plain . trimInlines . mconcat <$> manyTill inline' (char '|')

placeholder :: PandocMonad m => VwParser m ()
placeholder = try $
  choice (ph <$> ["title", "date"]) <|> noHtmlPh <|> templatePh

ph :: PandocMonad m => Text -> VwParser m ()
ph s = try $ do
  many spaceChar >> textStr (T.cons '%' s) >> spaceChar
  contents <- trimInlines . mconcat <$> manyTill inline (lookAhead newline)
    --use lookAhead because of placeholder in the whitespace parser
  let meta' = B.setMeta s contents nullMeta
  -- this order ensures that later values will be ignored in favor
  -- of earlier ones:
  updateState $ \st -> st { stateMeta = meta' <> stateMeta st }

noHtmlPh :: PandocMonad m => VwParser m ()
noHtmlPh = try $
  () <$ many spaceChar <* string "%nohtml" <* many spaceChar
    <* lookAhead newline

templatePh :: PandocMonad m => VwParser m ()
templatePh = try $
  () <$ many spaceChar <* string "%template" <* many (noneOf "\n")
    <* lookAhead newline

-- inline parser

inline :: PandocMonad m => VwParser m Inlines
inline = choice $ whitespace endlineP:inlineList

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
inlineBQ = choice $ whitespace endlineBQ:inlineList

-- inline parser for mixedlists
inlineML :: PandocMonad m => VwParser m Inlines
inlineML = choice $ whitespace endlineML:inlineList

str :: PandocMonad m => VwParser m Inlines
str = B.str <$> many1Char (noneOf $ spaceChars ++ specialChars)

whitespace :: PandocMonad m => VwParser m () -> VwParser m Inlines
whitespace endline = B.space <$ (skipMany1 spaceChar <|>
                                 try (newline >> (comment <|> placeholder)))
         <|> B.softbreak <$ endline

whitespace' :: PandocMonad m => VwParser m Inlines
whitespace' = B.space <$ skipMany1 spaceChar

special :: PandocMonad m => VwParser m Inlines
special = B.str <$> countChar 1 (oneOf specialChars)

bareURL :: PandocMonad m => VwParser m Inlines
bareURL = try $ do
  (orig, src) <- uri <|> emailAddress
  return $ B.link src "" (B.str orig)

strong :: PandocMonad m => VwParser m Inlines
strong = try $ do
  s <- lookAhead $ between (char '*') (char '*') (many1 $ noneOf "*")
  guard $ (head s `notElem` spaceChars)
             && (last s `notElem` spaceChars)
  char '*'
  contents <- mconcat <$>manyTill inline' (char '*'
    >> notFollowedBy alphaNum)
  return $ B.spanWith (makeId contents, [], []) mempty
    <> B.strong contents

makeId :: Inlines -> Text
makeId i = T.concat (stringify <$> toList i)

emph :: PandocMonad m => VwParser m Inlines
emph = try $ do
  s <- lookAhead $ between (char '_') (char '_') (many1 $ noneOf "_")
  guard $ (head s `notElem` spaceChars)
          && (last s `notElem` spaceChars)
  char '_'
  contents <- mconcat <$>manyTill inline' (char '_'
    >> notFollowedBy alphaNum)
  return $ B.emph contents

strikeout :: PandocMonad m => VwParser m Inlines
strikeout = try $ do
  string "~~"
  contents <- mconcat <$>many1Till inline' (string "~~")
  return $ B.strikeout contents

code :: PandocMonad m => VwParser m Inlines
code = try $ do
  char '`'
  contents <- many1TillChar (noneOf "\n") (char '`')
  return $ B.code contents

superscript :: PandocMonad m => VwParser m Inlines
superscript = try $
  B.superscript . mconcat <$> (char '^' >> many1Till inline' (char '^'))

subscript :: PandocMonad m => VwParser m Inlines
subscript = try $
  B.subscript . mconcat <$> (string ",,"
    >> many1Till inline' (try $ string ",,"))

link :: PandocMonad m => VwParser m Inlines
link = try $ do
  string "[["
  contents <- lookAhead $ manyTillChar anyChar (string "]]")
  if T.any (== '|') contents
                  then do
                    url <- manyTillChar anyChar $ char '|'
                    lab <- mconcat <$> manyTill inline (string "]]")
                    let tit = if isURI url
                                 then ""
                                 else "wikilink"
                    return $ B.link (procLink url) tit lab
                  else do
                    manyTill anyChar (string "]]")
-- not using try here because [[hell]o]] is not rendered as a link in vimwiki
                    let tit = if isURI contents
                                 then ""
                                 else "wikilink"
                    return $ B.link (procLink contents) tit (B.str contents)

image :: PandocMonad m => VwParser m Inlines
image = try $ do
  string "{{"
  contentText <- lookAhead $ manyTill (noneOf "\n") (try $ string "}}")
  images $ length $ filter (== '|') contentText

images :: PandocMonad m => Int -> VwParser m Inlines
images k
  | k == 0 = do
           imgurl <- manyTillChar anyChar (try $ string "}}")
           return $ B.image (procImgurl imgurl) "" (B.str "")
  | k == 1 = do
           imgurl <- manyTillChar anyChar (char '|')
           alt <- mconcat <$> manyTill inline (try $ string "}}")
           return $ B.image (procImgurl imgurl) "" alt
  | k == 2 = do
           imgurl <- manyTillChar anyChar (char '|')
           alt <- mconcat <$> manyTill inline (char '|')
           attrText <- manyTillChar anyChar (try $ string "}}")
           return $ B.imageWith (makeAttr attrText) (procImgurl imgurl) "" alt
  | otherwise = do
           imgurl <- manyTillChar anyChar (char '|')
           alt <- mconcat <$> manyTill inline (char '|')
           attrText <- manyTillChar anyChar (char '|')
           manyTill anyChar (try $ string "}}")
           return $ B.imageWith (makeAttr attrText) (procImgurl imgurl) "" alt

procLink' :: Text -> Text
procLink' s
  | T.take 6 s == "local:" = "file" <> T.drop 5 s
  | T.take 6 s == "diary:" = "diary/" <> T.drop 6 s
  | or ((`T.isPrefixOf` s) <$> [ "http:", "https:", "ftp:", "file:", "mailto:",
                              "news:", "telnet:" ])
                             = s
  | s == ""                  = ""
  | T.last s == '/'          = s
  | otherwise                = s

procLink :: Text -> Text
procLink s = procLink' x <> y
  where (x, y) = T.break (=='#') s

procImgurl :: Text -> Text
procImgurl s = if T.take 6 s == "local:" then "file" <> T.drop 5 s else s

inlineMath :: PandocMonad m => VwParser m Inlines
inlineMath = try $
  B.math <$ char '$' <*> many1TillChar (noneOf "\n") (char '$')

tag :: PandocMonad m => VwParser m Inlines
tag = try $ do
  char ':'
  s <- manyTillChar (noneOf spaceChars) (try (char ':' >> lookAhead space))
  guard $ not $ "::" `T.isInfixOf` (":" <> s <> ":")
  let ss = splitTextBy (==':') s
  return $ mconcat $ makeTagSpan' (head ss):(makeTagSpan <$> tail ss)

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
hasDefMarker = () <$ manyTill (noneOf "\n") (string "::" >> oneOf spaceChars)

makeTagSpan' :: Text -> Inlines
makeTagSpan' s = B.spanWith (T.cons '-' s, [], []) (B.str "") <>
                  B.spanWith (s, ["tag"], []) (B.str s)

makeTagSpan :: Text -> Inlines
makeTagSpan s = B.space <> makeTagSpan' s

mathTagParser :: PandocMonad m => VwParser m Text
mathTagParser = do
  s <- try $ lookAhead (char '%' >> manyTillChar (noneOf spaceChars)
    (try $ char '%' >> many (noneOf $ '%':spaceChars) >> space))
  char '%' >> textStr s >> char '%'
  return $ mathTagLaTeX s
