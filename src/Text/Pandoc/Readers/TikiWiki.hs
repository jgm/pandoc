{-# LANGUAGE RelaxedPolyRec, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
   Module      : Text.Pandoc.Readers.TikiWiki
   Copyright   : Copyright (C) 2017 Robin Lee Powell
   License     : GPLv2

   Maintainer  : Robin Lee Powell <robinleepowell@gmail.com>
   Stability   : alpha
   Portability : portable

Conversion of TikiWiki text to 'Pandoc' document.
-}

module Text.Pandoc.Readers.TikiWiki ( readTikiWiki
                                    ) where

import Control.Monad
import Control.Monad.Except (throwError)
import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (enclosed, nested)
import Text.Printf (printf)
import Text.Pandoc.XML (fromEntities)
import Text.Pandoc.Class (PandocMonad(..), CommonState(..))
import Text.Pandoc.Shared (crFilter)
import Text.Pandoc.Logging (Verbosity(..))
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as T

-- | Read TikiWiki from an input string and return a Pandoc document.
readTikiWiki :: PandocMonad m
          => ReaderOptions
          -> Text
          -> m Pandoc
readTikiWiki opts s = do
  res <- readWithM parseTikiWiki def{ stateOptions = opts }
             (T.unpack (crFilter s) ++ "\n\n")
  case res of
       Left e  -> throwError e
       Right d -> return d

type TikiWikiParser = ParserT [Char] ParserState

--
-- utility functions
--

tryMsg :: PandocMonad m => String -> TikiWikiParser m a -> TikiWikiParser m a
tryMsg msg p = try p <?> msg

skip :: PandocMonad m => TikiWikiParser m a -> TikiWikiParser m ()
skip parser = parser >> return ()

nested :: PandocMonad m => TikiWikiParser m a -> TikiWikiParser m a
nested p = do
  nestlevel <- stateMaxNestingLevel <$>  getState
  guard $ nestlevel > 0
  updateState $ \st -> st{ stateMaxNestingLevel = stateMaxNestingLevel st - 1 }
  res <- p
  updateState $ \st -> st{ stateMaxNestingLevel = nestlevel }
  return res

--
-- main parser
--

parseTikiWiki :: PandocMonad m => TikiWikiParser m Pandoc
parseTikiWiki = do
  bs <- mconcat <$> many block
  spaces
  eof
  return $ B.doc bs

block :: PandocMonad m => TikiWikiParser m B.Blocks
block = do
  verbosity <- getsCommonState stVerbosity
  pos <- getPosition
  res <- mempty <$ skipMany1 blankline
         <|> blockElements
         <|> para
  skipMany blankline
  when (verbosity >= INFO) $ do
    trace (printf "line %d: %s" (sourceLine pos) (take 60 $ show $ B.toList res))
  return res

blockElements :: PandocMonad m => TikiWikiParser m B.Blocks
blockElements = choice [ table
                       , hr
                       , header
                       , mixedList
                       , definitionList
                       , codeMacro
                       ]

-- top
-- ----
-- bottom
--
-- ----
--
hr :: PandocMonad m => TikiWikiParser m B.Blocks
hr = try $ do
  string "----"
  many (char '-')
  newline
  return $ B.horizontalRule

-- ! header
--
-- !! header level two
--
-- !!! header level 3
--
header :: PandocMonad m => TikiWikiParser m B.Blocks
header = tryMsg "header" $ do
  level <- many1 (char '!') >>= return . length
  guard $ level <= 6
  skipSpaces
  content <- B.trimInlines . mconcat <$> manyTill inline newline
  attr <- registerHeader nullAttr content
  return $ B.headerWith attr level $ content

tableRow :: PandocMonad m => TikiWikiParser m [B.Blocks]
tableRow = try $ do
--  row <- sepBy1 (many1Till inline $ oneOf "\n|") (try $ string "|" <* notFollowedBy (oneOf "|\n")) 
--  return $ map (B.plain . mconcat) row
  row <- sepBy1 ((many1 $ noneOf "\n|") >>= parseColumn) (try $ string "|" <* notFollowedBy (oneOf "|\n")) 
  return $ map B.plain row
  where
    parseColumn x = do
      parsed <- parseFromString (many1 inline) x
      return $ mconcat parsed



-- Tables:
--
-- ||foo||
--
-- ||row1-column1|row1-column2||row2-column1|row2-column2||
--
-- ||row1-column1|row1-column2
-- row2-column1|row2-column2||
--
-- ||row1-column1|row1-column2
-- row2-column1|row2-column2||row3-column1|row3-column2||
--
-- || Orange | Apple     | more
--  Bread  | Pie       | more
--  Butter | Ice cream | and more ||
-- 
table :: PandocMonad m => TikiWikiParser m B.Blocks
table = try $ do
  string "||"
  rows <- sepBy1 tableRow (try $ string "\n" <|> (string "||" <* notFollowedBy (string "\n")))
  string "||"
  newline
  -- return $ B.simpleTable (headers rows) $ trace ("rows: " ++ (show rows)) rows
  return $ B.simpleTable (headers rows) $ rows
  where
    -- The headers are as many empty srings as the number of columns
    -- in the first row
    headers rows = map (B.plain . B.str) $ take (length $ rows !! 0) $ repeat ""

para :: PandocMonad m => TikiWikiParser m B.Blocks
para = many1Till inline endOfParaElement >>= return . result . mconcat
 where
   endOfParaElement = lookAhead $ endOfInput <|> endOfPara <|> newBlockElement
   endOfInput       = try $ skipMany blankline >> skipSpaces >> eof
   endOfPara        = try $ blankline >> skipMany1 blankline
   newBlockElement  = try $ blankline >> skip blockElements
   result content   = if F.all (==Space) content
                      then mempty
                      else B.para $ B.trimInlines content

-- ;item 1: definition 1
-- ;item 2: definition 2-1
-- + definition 2-2
-- ;item ''3'': definition ''3''
--
definitionList :: PandocMonad m => TikiWikiParser m B.Blocks
definitionList = tryMsg "definitionList" $ do
  elements <- many1 $ parseDefinitionListItem
  return $ B.definitionList elements
  where
    parseDefinitionListItem :: PandocMonad m => TikiWikiParser m (B.Inlines, [B.Blocks])
    parseDefinitionListItem = do
      skipSpaces >> char ';' <* skipSpaces
      term <- many1Till inline $ char ':' <* skipSpaces
      line <- listItemLine 1
      return $ (mconcat term, [B.plain line])

data ListType = None | Numbered | Bullet deriving (Ord, Eq, Show)

data ListNesting = LN { lntype :: ListType, lnnest :: Int } deriving (Ord, Eq, Show)

-- The first argument is a stack (most recent == head) of our list
-- nesting status; the list type and the nesting level; if we're in
-- a number list in a bullet list it'd be
-- [LN Numbered 2, LN Bullet 1]
--
-- Mixed list example:
--
-- # one
-- # two
-- ** two point one
-- ** two point two
-- # three
-- # four
--
mixedList :: PandocMonad m => TikiWikiParser m B.Blocks
mixedList = try $ do
  items <- try $ many1 listItem
  return $ mconcat $ fixListNesting $ spanFoldUpList (LN None 0) items

-- See the "Handling Lists" section of DESIGN-CODE for why this
-- function exists.  It's to post-process the lists and do some
-- mappends.
--
-- We need to walk the tree two items at a time, so we can see what
-- we're going to join *to* before we get there.
--
-- Because of that, it seemed easier to do it by hand than to try to
-- figre out a fold or something.
fixListNesting :: [B.Blocks] -> [B.Blocks]
fixListNesting [] = []
fixListNesting (first:[]) = [recurseOnList first]
-- fixListNesting nestall | trace ("\n\nfixListNesting: " ++ (show nestall)) False = undefined
-- fixListNesting nestall@(first:second:rest) = 
fixListNesting (first:second:rest) = 
  let secondBlock = head $ B.toList second in
    case secondBlock of
      BulletList _ -> fixListNesting $ [(mappend (recurseOnList first) (recurseOnList second))] ++ rest
      OrderedList _ _ -> fixListNesting $ [(mappend (recurseOnList first) (recurseOnList second))] ++ rest
      _ -> [recurseOnList first] ++ fixListNesting (second:rest)

-- This function walks the Block structure for fixListNesting,
-- because it's a bit complicated, what with converting to and from
-- lists and so on.
recurseOnList :: B.Blocks -> B.Blocks
-- recurseOnList item | trace ("rOL: " ++ (show $ length $ B.toList item) ++ ", " ++ (show $ B.toList item)) False = undefined
recurseOnList items
  | (length $ B.toList items) == 1 =
    let itemBlock = head $ B.toList items in
      case itemBlock of
        BulletList listItems -> B.bulletList $ fixListNesting $ map B.fromList listItems
        OrderedList _ listItems -> B.orderedList $ fixListNesting $ map B.fromList listItems
        _ -> items

  -- The otherwise works because we constructed the blocks, and we
  -- know for a fact that no mappends have been run on them; each
  -- Blocks consists of exactly one Block.
  --
  -- Anything that's not like that has already been processed by
  -- fixListNesting; don't bother to process it again.
  | otherwise = items


-- Turn the list if list items into a tree by breaking off the first
-- item, splitting the remainder of the list into items that are in
-- the tree of the first item and those that aren't, wrapping the
-- tree of the first item in its list time, and recursing on both
-- sections.
spanFoldUpList :: ListNesting -> [(ListNesting, B.Blocks)] -> [B.Blocks]
spanFoldUpList _ [] = []
spanFoldUpList ln (first:[]) =
  listWrap ln (fst first) [snd first]
spanFoldUpList ln (first:rest) =
  let (span1, span2) = span (splitListNesting (fst first)) rest
      newTree1 = listWrap ln (fst first) $ [snd first] ++ spanFoldUpList (fst first) span1
      newTree2 = spanFoldUpList ln span2
  in
    newTree1 ++ newTree2

-- Decide if the second item should be in the tree of the first
-- item, which is true if the second item is at a deeper nesting
-- level and of the same type.
splitListNesting :: ListNesting -> (ListNesting, B.Blocks) -> Bool
splitListNesting ln1 (ln2, _) =
  if (lnnest ln1) < (lnnest ln2) then
    True
  else
    if ln1 == ln2 then
      True
    else
      False

-- If we've moved to a deeper nesting level, wrap the new level in
-- the appropriate type of list.
listWrap :: ListNesting -> ListNesting -> [B.Blocks] -> [B.Blocks]
listWrap upperLN curLN retTree =
  if upperLN == curLN then
    retTree
  else
    case lntype curLN of
      None -> []
      Bullet -> [B.bulletList retTree]
      Numbered -> [B.orderedList retTree]

listItem :: PandocMonad m => TikiWikiParser m (ListNesting, B.Blocks)
listItem = choice [
    bulletItem
  , numberedItem
  ]


-- * Start each line
-- * with an asterisk (*).
-- ** More asterisks gives deeper
-- *** and deeper levels.
--
bulletItem :: PandocMonad m => TikiWikiParser m (ListNesting, B.Blocks)
bulletItem = try $ do
  prefix <- many1 $ char '*'
  many1 $ char ' '
  content <- listItemLine (length prefix)
  return $ (LN Bullet (length prefix), B.plain content)

-- # Start each line
-- # with a number (1.).
-- ## More number signs gives deeper
-- ### and deeper
--
numberedItem :: PandocMonad m => TikiWikiParser m (ListNesting, B.Blocks)
numberedItem = try $ do
  prefix <- many1 $ char '#'
  many1 $ char ' '
  content <- listItemLine (length prefix)
  return $ (LN Numbered (length prefix), B.plain content)

listItemLine :: PandocMonad m => Int -> TikiWikiParser m B.Inlines
listItemLine nest = lineContent >>= parseContent >>= return
  where
    lineContent = do
      content <- anyLine
      continuation <- optionMaybe listContinuation
      return $ filterSpaces content ++ "\n" ++ (maybe "" id continuation)
    filterSpaces = reverse . dropWhile (== ' ') . reverse
    listContinuation = string (take nest (repeat '+')) >> lineContent
    parseContent x = do
      parsed <- parseFromString (many1 inline) x
      return $ mconcat parsed

-- Turn the CODE macro attributes into Pandoc code block attributes.
mungeAttrs :: [(String, String)] -> (String, [String], [(String, String)])
mungeAttrs rawAttrs = ("", classes, rawAttrs)
  where
    -- "colors" is TikiWiki CODE macro for "name of language to do
    -- highlighting for"; turn the value into a class
    color = fromMaybe "" $ lookup "colors" rawAttrs
    -- ln = 1 means line numbering.  It's also the default.  So we
    -- emit numberLines as a class unless ln = 0
    lnRaw = fromMaybe "1" $ lookup "ln" rawAttrs
    ln = if lnRaw == "0" then
            ""
         else
            "numberLines"
    classes = filter (/= "") [color, ln]

codeMacro :: PandocMonad m => TikiWikiParser m B.Blocks
codeMacro = try $ do
  string "{CODE("
  rawAttrs <- macroAttrs
  string ")}"
  body <- manyTill anyChar (try (string "{CODE}"))
  newline
  if length rawAttrs > 0
    then
      return $ B.codeBlockWith (mungeAttrs rawAttrs) body
    else
      return $ B.codeBlock body


--
-- inline parsers
--

inline :: PandocMonad m => TikiWikiParser m B.Inlines
inline = choice [ whitespace
                , noparse
                , strong
                , emph
                , nbsp
                , image
                , htmlComment
                , strikeout
                , code
                , wikiLink
                , notExternalLink
                , externalLink
                , superTag
                , superMacro
                , subTag
                , subMacro
                , escapedChar
                , colored
                , centered
                , underlined
                , boxed
                , breakChars
                , str
                , symbol
                ] <?> "inline"

whitespace :: PandocMonad m => TikiWikiParser m B.Inlines
whitespace = (lb <|> regsp) >>= return
  where lb = try $ skipMany spaceChar >> linebreak >> return B.space
        regsp = try $ skipMany1 spaceChar >> return B.space

-- UNSUPPORTED, as there doesn't seem to be any facility in calibre
-- for this
nbsp :: PandocMonad m => TikiWikiParser m B.Inlines
nbsp = try $ do
  string "~hs~" 
  return $ B.str " NOT SUPPORTED BEGIN: ~hs~ (non-breaking space) :END "

-- UNSUPPORTED, as the desired behaviour (that the data be
-- *retained* and stored as a comment) doesn't exist in calibre, and
-- silently throwing data out seemed bad.
htmlComment :: PandocMonad m => TikiWikiParser m B.Inlines
htmlComment = try $ do
  string "~hc~" 
  inner <- many1 $ noneOf "~"
  string "~/hc~"
  return $ B.str $ " NOT SUPPORTED: ~hc~ (html comment opener) BEGIN: " ++ inner ++ " ~/hc~ :END "

linebreak :: PandocMonad m => TikiWikiParser m B.Inlines
linebreak = newline >> notFollowedBy newline >> (lastNewline <|> innerNewline)
  where lastNewline  = eof >> return mempty
        innerNewline = return B.space

between :: (Monoid c, PandocMonad m, Show b) => TikiWikiParser m a -> TikiWikiParser m b -> (TikiWikiParser m b -> TikiWikiParser m c) -> TikiWikiParser m c
between start end p =
  mconcat <$> try (start >> notFollowedBy whitespace >> many1Till (p end) end)

enclosed :: (Monoid b, PandocMonad m, Show a) => TikiWikiParser m a -> (TikiWikiParser m a -> TikiWikiParser m b) -> TikiWikiParser m b
enclosed sep p = between sep (try $ sep <* endMarker) p
  where
    endMarker   = lookAhead $ skip endSpace <|> skip (oneOf ".,!?:)|'_") <|> eof
    endSpace    = (spaceChar <|> newline) >> return B.space


nestedInlines :: (Show a, PandocMonad m) => TikiWikiParser m a -> TikiWikiParser m B.Inlines
nestedInlines end = innerSpace <|> nestedInline
  where
    innerSpace   = try $ whitespace <* (notFollowedBy end)
    nestedInline = notFollowedBy whitespace >> nested inline

-- {img attId="39" imalign="right" link="http://info.tikiwiki.org" alt="Panama Hat"}
--
-- {img attId="37", thumb="mouseover", styleimage="border", desc="150"}
--
-- {img src="img/wiki_up/393px-Pears.jpg" thumb="y" imalign="center" stylebox="border" button="y" desc="Pretty pears" max="200" rel="box"}
--
image :: PandocMonad m => TikiWikiParser m B.Inlines
image = try $ do
  string "{img "
  rawAttrs <- sepEndBy1 imageAttr spaces
  string "}"
  let src = fromMaybe "" $ lookup "src" rawAttrs
  let title = fromMaybe src $ lookup "desc" rawAttrs
  let alt = fromMaybe title $ lookup "alt" rawAttrs
  let classes = map fst $ filter (\(_,b) -> b == "" || b == "y") rawAttrs
  if length src > 0
    then
      return $ B.imageWith ("", classes, rawAttrs) src title (B.str alt)
    else
      return $ B.str $ " NOT SUPPORTED: image without src attribute BEGIN: {img " ++ (printAttrs rawAttrs) ++ "} :END "
  where
    printAttrs attrs = intercalate " " $ map (\(a, b) -> a ++ "=\"" ++ b ++ "\"") attrs

imageAttr :: PandocMonad m => TikiWikiParser m (String, String)
imageAttr = try $ do
  key <- many1 (noneOf "=} \t\n")
  char '='
  optional $ char '"'
  value <- many1 (noneOf "}\"\n")
  optional $ char '"'
  optional $ char ','
  return (key, value)


-- __strong__
strong :: PandocMonad m => TikiWikiParser m B.Inlines
strong = try $ enclosed (string "__") nestedInlines >>= return . B.strong

-- ''emph''
emph :: PandocMonad m => TikiWikiParser m B.Inlines
emph = try $ enclosed (string "''") nestedInlines >>= return . B.emph

-- ~246~
escapedChar :: PandocMonad m => TikiWikiParser m B.Inlines
escapedChar = try $ do
  string "~"
  inner <- many1 $ oneOf "0123456789"
  string "~"
  return $ B.str $ [(toEnum ((read inner) :: Int)) :: Char]

-- UNSUPPORTED, as there doesn't seem to be any facility in calibre
-- for this
centered :: PandocMonad m => TikiWikiParser m B.Inlines
centered = try $ do
  string "::"
  inner <- many1 $ noneOf ":\n"
  string "::"
  return $ B.str $ " NOT SUPPORTED: :: (centered) BEGIN: ::" ++ inner ++ ":: :END "

-- UNSUPPORTED, as there doesn't seem to be any facility in calibre
-- for this
colored :: PandocMonad m => TikiWikiParser m B.Inlines
colored = try $ do
  string "~~"
  inner <- many1 $ noneOf "~\n"
  string "~~"
  return $ B.str $ " NOT SUPPORTED: ~~ (colored) BEGIN: ~~" ++ inner ++ "~~ :END "

-- UNSUPPORTED, as there doesn't seem to be any facility in calibre
-- for this
underlined :: PandocMonad m => TikiWikiParser m B.Inlines
underlined = try $ do
  string "==="
  inner <- many1 $ noneOf "=\n"
  string "==="
  return $ B.str $ " NOT SUPPORTED: ==== (underlined) BEGIN: ===" ++ inner ++ "=== :END "

-- UNSUPPORTED, as there doesn't seem to be any facility in calibre
-- for this
boxed :: PandocMonad m => TikiWikiParser m B.Inlines
boxed = try $ do
  string "^"
  inner <- many1 $ noneOf "^\n"
  string "^"
  return $ B.str $ " NOT SUPPORTED: ^ (boxed) BEGIN: ^" ++ inner ++ "^ :END "

-- --text--
strikeout :: PandocMonad m => TikiWikiParser m B.Inlines
strikeout = try $ enclosed (string "--") nestedInlines >>= return . B.strikeout

nestedString :: (Show a, PandocMonad m) => TikiWikiParser m a -> TikiWikiParser m String
nestedString end = innerSpace <|> (count 1 nonspaceChar)
  where
    innerSpace = try $ many1 spaceChar <* notFollowedBy end

breakChars :: PandocMonad m => TikiWikiParser m B.Inlines
breakChars = try $ string "%%%" >> return B.linebreak

-- superscript: foo{TAG(tag=>sup)}super{TAG}foo / bar{SUP()}super2{SUP}bar
superTag :: PandocMonad m => TikiWikiParser m B.Inlines
superTag = try $ between (string "{TAG(tag=>sup)}") (string "{TAG}") nestedString >>= return . B.superscript . B.text . fromEntities

superMacro :: PandocMonad m => TikiWikiParser m B.Inlines
superMacro = try $ do
  string "{SUP("
  manyTill anyChar (string ")}")
  body <- manyTill anyChar (string "{SUP}")
  return $ B.superscript $ B.text body

-- subscript: baz{TAG(tag=>sub)}sub{TAG}qux / qux{SUB()}sub2{SUB}qux
subTag :: PandocMonad m => TikiWikiParser m B.Inlines
subTag = try $ between (string "{TAG(tag=>sub)}") (string "{TAG}") nestedString >>= return . B.subscript . B.text . fromEntities

subMacro :: PandocMonad m => TikiWikiParser m B.Inlines
subMacro = try $ do
  string "{SUB("
  manyTill anyChar (string ")}")
  body <- manyTill anyChar (string "{SUB}")
  return $ B.subscript $ B.text body

-- -+text+-
code :: PandocMonad m => TikiWikiParser m B.Inlines
code = try $ between (string "-+") (string "+-") nestedString >>= return . B.code . fromEntities

macroAttr :: PandocMonad m => TikiWikiParser m (String, String)
macroAttr = try $ do
  key <- many1 (noneOf "=)")
  char '='
  optional $ char '"'
  value <- many1 (noneOf " )\"")
  optional $ char '"'
  return (key, value)

macroAttrs :: PandocMonad m => TikiWikiParser m [(String, String)]
macroAttrs = try $ do
  attrs <- sepEndBy macroAttr spaces
  return attrs

-- ~np~ __not bold__ ~/np~
noparse :: PandocMonad m => TikiWikiParser m B.Inlines
noparse = try $ do
  string "~np~"
  body <- manyTill anyChar (string "~/np~")
  return $ B.str body

str :: PandocMonad m => TikiWikiParser m B.Inlines
str = (many1 alphaNum <|> count 1 characterReference) >>= return . B.str

symbol :: PandocMonad m => TikiWikiParser m B.Inlines
symbol = count 1 nonspaceChar >>= return . B.str

-- [[not a link]
notExternalLink :: PandocMonad m => TikiWikiParser m B.Inlines
notExternalLink = try $ do
  start <- string "[["
  body <- many (noneOf "\n[]")
  end <- string "]"
  return $ B.text (start ++ body ++ end)

-- [http://www.somesite.org url|Some Site title]
-- ((internal link))
--
-- The ((...)) wiki links and [...] external links are handled
-- exactly the same; this abstracts that out
makeLink :: PandocMonad m => String -> String -> String -> TikiWikiParser m B.Inlines
makeLink start middle end = try $ do
  st <- getState
  guard $ stateAllowLinks st
  setState $ st{ stateAllowLinks = False }
  (url, title, anchor) <- wikiLinkText start middle end
  parsedTitle <- parseFromString (many1 inline) title
  setState $ st{ stateAllowLinks = True }
  return $ B.link (url++anchor) "" $ mconcat $ parsedTitle

wikiLinkText :: PandocMonad m => String -> String -> String -> TikiWikiParser m (String, String, String)
wikiLinkText start middle end = do
  string start
  url <- many1 (noneOf $ middle ++ "\n")
  seg1 <- option url linkContent
  seg2 <- option "" linkContent
  string end
  if seg2 /= ""
    then
      return (url, seg2, seg1)
    else
      return (url, seg1, "")
  where
    linkContent      = do
      (char '|')
      mystr <- many (noneOf middle)
      return $ mystr

externalLink :: PandocMonad m => TikiWikiParser m B.Inlines
externalLink = makeLink "[" "]|" "]"

-- NB: this wiki linking is unlikely to work for anyone besides me
-- (rlpowell); it happens to work for me because my Hakyll code has
-- post-processing that treats pandoc .md titles as valid link
-- targets, so something like
-- [see also this other post](My Other Page) is perfectly valid.
wikiLink :: PandocMonad m => TikiWikiParser m B.Inlines
wikiLink = makeLink "((" ")|" "))"

