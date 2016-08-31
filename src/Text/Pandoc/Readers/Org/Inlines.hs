{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2014-2016 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

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
   Module      : Text.Pandoc.Readers.Org.Options
   Copyright   : Copyright (C) 2014-2016 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Parsers for Org-mode inline elements.
-}
module Text.Pandoc.Readers.Org.Inlines
  ( inline
  , inlines
  , addToNotesTable
  , linkTarget
  ) where

import           Text.Pandoc.Readers.Org.BlockStarts
import           Text.Pandoc.Readers.Org.ParserState
import           Text.Pandoc.Readers.Org.Parsing
import           Text.Pandoc.Readers.Org.Shared
                   ( cleanLinkString, isImageFilename, rundocBlockClass
                   , toRundocAttrib, translateLang )

import qualified Text.Pandoc.Builder as B
import           Text.Pandoc.Builder ( Inlines )
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.LaTeX ( inlineCommand, rawLaTeXInline )
import           Text.TeXMath ( readTeX, writePandoc, DisplayType(..) )
import qualified Text.TeXMath.Readers.MathML.EntityMap as MathMLEntityMap

import           Prelude hiding (sequence)
import           Control.Monad ( guard, mplus, mzero, when, void )
import           Data.Char ( isAlphaNum, isSpace )
import           Data.List ( intersperse )
import           Data.Maybe ( fromMaybe )
import qualified Data.Map as M
import           Data.Monoid ( (<>) )
import           Data.Traversable (sequence)

--
-- Functions acting on the parser state
--
recordAnchorId :: String -> OrgParser ()
recordAnchorId i = updateState $ \s ->
  s{ orgStateAnchorIds = i : (orgStateAnchorIds s) }

pushToInlineCharStack :: Char -> OrgParser ()
pushToInlineCharStack c = updateState $ \s ->
  s{ orgStateEmphasisCharStack = c:orgStateEmphasisCharStack s }

popInlineCharStack :: OrgParser ()
popInlineCharStack = updateState $ \s ->
  s{ orgStateEmphasisCharStack = drop 1 . orgStateEmphasisCharStack $ s }

surroundingEmphasisChar :: OrgParser [Char]
surroundingEmphasisChar =
  take 1 . drop 1 . orgStateEmphasisCharStack <$> getState

startEmphasisNewlinesCounting :: Int -> OrgParser ()
startEmphasisNewlinesCounting maxNewlines = updateState $ \s ->
  s{ orgStateEmphasisNewlines = Just maxNewlines }

decEmphasisNewlinesCount :: OrgParser ()
decEmphasisNewlinesCount = updateState $ \s ->
  s{ orgStateEmphasisNewlines = (\n -> n - 1) <$> orgStateEmphasisNewlines s }

newlinesCountWithinLimits :: OrgParser Bool
newlinesCountWithinLimits = do
  st <- getState
  return $ ((< 0) <$> orgStateEmphasisNewlines st) /= Just True

resetEmphasisNewlines :: OrgParser ()
resetEmphasisNewlines = updateState $ \s ->
  s{ orgStateEmphasisNewlines = Nothing }

addToNotesTable :: OrgNoteRecord -> OrgParser ()
addToNotesTable note = do
  oldnotes <- orgStateNotes' <$> getState
  updateState $ \s -> s{ orgStateNotes' = note:oldnotes }

-- | Parse a single Org-mode inline element
inline :: OrgParser (F Inlines)
inline =
  choice [ whitespace
         , linebreak
         , cite
         , footnote
         , linkOrImage
         , anchor
         , inlineCodeBlock
         , str
         , endline
         , emphasizedText
         , code
         , math
         , displayMath
         , verbatim
         , subscript
         , superscript
         , inlineLaTeX
         , exportSnippet
         , smart
         , symbol
         ] <* (guard =<< newlinesCountWithinLimits)
  <?> "inline"

-- | Read the rest of the input as inlines.
inlines :: OrgParser (F Inlines)
inlines = trimInlinesF . mconcat <$> many1 inline

-- treat these as potentially non-text when parsing inline:
specialChars :: [Char]
specialChars = "\"$'()*+-,./:;<=>@[\\]^_{|}~"


whitespace :: OrgParser (F Inlines)
whitespace = pure B.space <$ skipMany1 spaceChar
                          <* updateLastPreCharPos
                          <* updateLastForbiddenCharPos
             <?> "whitespace"

linebreak :: OrgParser (F Inlines)
linebreak = try $ pure B.linebreak <$ string "\\\\" <* skipSpaces <* newline

str :: OrgParser (F Inlines)
str = return . B.str <$> many1 (noneOf $ specialChars ++ "\n\r ")
      <* updateLastStrPos

-- | An endline character that can be treated as a space, not a structural
-- break.  This should reflect the values of the Emacs variable
-- @org-element-pagaraph-separate@.
endline :: OrgParser (F Inlines)
endline = try $ do
  newline
  notFollowedBy blankline
  notFollowedBy' exampleLineStart
  notFollowedBy' hline
  notFollowedBy' noteMarker
  notFollowedBy' tableStart
  notFollowedBy' drawerStart
  notFollowedBy' headerStart
  notFollowedBy' metaLineStart
  notFollowedBy' latexEnvStart
  notFollowedBy' commentLineStart
  notFollowedBy' bulletListStart
  notFollowedBy' orderedListStart
  decEmphasisNewlinesCount
  guard =<< newlinesCountWithinLimits
  updateLastPreCharPos
  return . return $ B.softbreak


--
-- Citations
--

-- The state of citations is a bit confusing due to the lack of an official
-- syntax and multiple syntaxes coexisting.  The pandocOrgCite syntax was the
-- first to be implemented here and is almost identical to Markdown's citation
-- syntax.  The org-ref package is in wide use to handle citations, but the
-- syntax is a bit limiting and not quite as simple to write.  The
-- semi-offical Org-mode citation syntax is based on John MacFarlane's Pandoc
-- sytax and Org-oriented enhancements contributed by Richard Lawrence and
-- others.  It's dubbed Berkeley syntax due the place of activity of its main
-- contributors.  All this should be consolidated once an official Org-mode
-- citation syntax has emerged.

cite :: OrgParser (F Inlines)
cite = try $ berkeleyCite <|> do
  guardEnabled Ext_citations
  (cs, raw) <- withRaw $ choice
               [ pandocOrgCite
               , orgRefCite
               , berkeleyTextualCite
               ]
  return $ (flip B.cite (B.text raw)) <$> cs

-- | A citation in Pandoc Org-mode style (@[prefix \@citekey suffix]@).
pandocOrgCite :: OrgParser (F [Citation])
pandocOrgCite = try $
  char '[' *> skipSpaces *> citeList <* skipSpaces <* char ']'

orgRefCite :: OrgParser (F [Citation])
orgRefCite = try $ choice
  [ normalOrgRefCite
  , fmap (:[]) <$> linkLikeOrgRefCite
  ]

normalOrgRefCite :: OrgParser (F [Citation])
normalOrgRefCite = try $ do
  mode <- orgRefCiteMode
  sequence <$> sepBy1 (orgRefCiteList mode) (char ',')
 where
   -- | A list of org-ref style citation keys, parsed as citation of the given
   -- citation mode.
   orgRefCiteList :: CitationMode -> OrgParser (F Citation)
   orgRefCiteList citeMode = try $ do
     key <- orgRefCiteKey
     returnF $ Citation
      { citationId      = key
      , citationPrefix  = mempty
      , citationSuffix  = mempty
      , citationMode    = citeMode
      , citationNoteNum = 0
      , citationHash    = 0
      }

-- | Read an Berkeley-style Org-mode citation.  Berkeley citation style was
-- develop and adjusted to Org-mode style by John MacFarlane and Richard
-- Lawrence, respectively, both philosophers at UC Berkeley.
berkeleyCite :: OrgParser (F Inlines)
berkeleyCite = try $ do
  bcl <- berkeleyCitationList
  return $ do
    parens <- berkeleyCiteParens <$> bcl
    prefix <- berkeleyCiteCommonPrefix <$> bcl
    suffix <- berkeleyCiteCommonSuffix <$> bcl
    citationList <- berkeleyCiteCitations <$> bcl
    return $
      if parens
      then toCite
           . maybe id (\p -> alterFirst (prependPrefix p)) prefix
           . maybe id (\s -> alterLast  (appendSuffix  s)) suffix
           $ citationList
      else maybe mempty (<> " ") prefix
             <> (toListOfCites $ map toInTextMode citationList)
             <> maybe mempty (", " <>) suffix
 where
   toCite :: [Citation] -> Inlines
   toCite cs = B.cite cs mempty

   toListOfCites :: [Citation] -> Inlines
   toListOfCites = mconcat . intersperse ", " . map (\c -> B.cite [c] mempty)

   toInTextMode :: Citation -> Citation
   toInTextMode c = c { citationMode = AuthorInText }

   alterFirst, alterLast :: (a -> a) -> [a] -> [a]
   alterFirst _ []     = []
   alterFirst f (c:cs) = (f c):cs
   alterLast  f = reverse . alterFirst f . reverse

   prependPrefix, appendSuffix :: Inlines -> Citation -> Citation
   prependPrefix pre c = c { citationPrefix = B.toList pre <> citationPrefix c }
   appendSuffix  suf c = c { citationSuffix = citationSuffix c <> B.toList suf }

data BerkeleyCitationList = BerkeleyCitationList
  { berkeleyCiteParens :: Bool
  , berkeleyCiteCommonPrefix :: Maybe Inlines
  , berkeleyCiteCommonSuffix :: Maybe Inlines
  , berkeleyCiteCitations :: [Citation]
  }
berkeleyCitationList :: OrgParser (F BerkeleyCitationList)
berkeleyCitationList = try $ do
  char '['
  parens <- choice [ False <$ berkeleyBareTag, True <$ berkeleyParensTag ]
  char ':'
  skipSpaces
  commonPrefix <- optionMaybe (try $ citationListPart <* char ';')
  citations    <- citeList
  commonSuffix <- optionMaybe (try $ citationListPart)
  char ']'
  return (BerkeleyCitationList parens
    <$> sequence commonPrefix
    <*> sequence commonSuffix
    <*> citations)
 where
   citationListPart :: OrgParser (F Inlines)
   citationListPart = fmap (trimInlinesF . mconcat) . try . many1 $ do
     notFollowedBy' citeKey
     notFollowedBy (oneOf ";]")
     inline

berkeleyBareTag :: OrgParser ()
berkeleyBareTag = try $ void berkeleyBareTag'

berkeleyParensTag :: OrgParser ()
berkeleyParensTag = try . void $ enclosedByPair '(' ')' berkeleyBareTag'

berkeleyBareTag' :: OrgParser ()
berkeleyBareTag' = try $ void (string "cite")

berkeleyTextualCite :: OrgParser (F [Citation])
berkeleyTextualCite = try $ do
  (suppressAuthor, key) <- citeKey
  returnF . return $ Citation
    { citationId      = key
    , citationPrefix  = mempty
    , citationSuffix  = mempty
    , citationMode    = if suppressAuthor then SuppressAuthor else AuthorInText
    , citationNoteNum = 0
    , citationHash    = 0
    }

-- The following is what a Berkeley-style bracketed textual citation parser
-- would look like.  However, as these citations are a subset of Pandoc's Org
-- citation style, this isn't used.
-- berkeleyBracketedTextualCite :: OrgParser (F [Citation])
-- berkeleyBracketedTextualCite = try . (fmap head) $
--   enclosedByPair '[' ']' berkeleyTextualCite

-- | Read a link-like org-ref style citation.  The citation includes pre and
-- post text.  However, multiple citations are not possible due to limitations
-- in the syntax.
linkLikeOrgRefCite :: OrgParser (F Citation)
linkLikeOrgRefCite = try $ do
  _    <- string "[["
  mode <- orgRefCiteMode
  key  <- orgRefCiteKey
  _    <- string "]["
  pre  <- trimInlinesF . mconcat <$> manyTill inline (try $ string "::")
  spc  <- option False (True <$ spaceChar)
  suf  <- trimInlinesF . mconcat <$> manyTill inline (try $ string "]]")
  return $ do
    pre' <- pre
    suf' <- suf
    return Citation
      { citationId      = key
      , citationPrefix  = B.toList pre'
      , citationSuffix  = B.toList (if spc then B.space <> suf' else suf')
      , citationMode    = mode
      , citationNoteNum = 0
      , citationHash    = 0
      }

-- | Read a citation key.  The characters allowed in citation keys are taken
-- from the `org-ref-cite-re` variable in `org-ref.el`.
orgRefCiteKey :: OrgParser String
orgRefCiteKey = try . many1 . satisfy $ \c ->
                  isAlphaNum c || c `elem` ("-_:\\./"::String)

-- | Supported citation types.  Only a small subset of org-ref types is
-- supported for now.  TODO: rewrite this, use LaTeX reader as template.
orgRefCiteMode :: OrgParser CitationMode
orgRefCiteMode =
  choice $ map (\(s, mode) -> mode <$ try (string s <* char ':'))
    [ ("cite", AuthorInText)
    , ("citep", NormalCitation)
    , ("citep*", NormalCitation)
    , ("citet", AuthorInText)
    , ("citet*", AuthorInText)
    , ("citeyear", SuppressAuthor)
    ]

citeList :: OrgParser (F [Citation])
citeList = sequence <$> sepEndBy1 citation (try $ char ';' *> skipSpaces)

citation :: OrgParser (F Citation)
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
 where
   prefix = trimInlinesF . mconcat <$>
            manyTill inline (char ']' <|> (']' <$ lookAhead citeKey))
   suffix = try $ do
     hasSpace <- option False (notFollowedBy nonspaceChar >> return True)
     skipSpaces
     rest <- trimInlinesF . mconcat <$>
             many (notFollowedBy (oneOf ";]") *> inline)
     return $ if hasSpace
              then (B.space <>) <$> rest
              else rest

footnote :: OrgParser (F Inlines)
footnote = try $ inlineNote <|> referencedNote

inlineNote :: OrgParser (F Inlines)
inlineNote = try $ do
  string "[fn:"
  ref <- many alphaNum
  char ':'
  note <- fmap B.para . trimInlinesF . mconcat <$> many1Till inline (char ']')
  when (not $ null ref) $
       addToNotesTable ("fn:" ++ ref, note)
  return $ B.note <$> note

referencedNote :: OrgParser (F Inlines)
referencedNote = try $ do
  ref <- noteMarker
  return $ do
    notes <- asksF orgStateNotes'
    case lookup ref notes of
      Nothing   -> return $ B.str $ "[" ++ ref ++ "]"
      Just contents  -> do
        st <- askF
        let contents' = runF contents st{ orgStateNotes' = [] }
        return $ B.note contents'

linkOrImage :: OrgParser (F Inlines)
linkOrImage = explicitOrImageLink
              <|> selflinkOrImage
              <|> angleLink
              <|> plainLink
              <?> "link or image"

explicitOrImageLink :: OrgParser (F Inlines)
explicitOrImageLink = try $ do
  char '['
  srcF   <- applyCustomLinkFormat =<< possiblyEmptyLinkTarget
  title  <- enclosedRaw (char '[') (char ']')
  title' <- parseFromString (mconcat <$> many inline) title
  char ']'
  return $ do
    src <- srcF
    case cleanLinkString title of
      Just imgSrc | isImageFilename imgSrc ->
        pure $ B.link src "" $ B.image imgSrc mempty mempty
      _ ->
        linkToInlinesF src =<< title'

selflinkOrImage :: OrgParser (F Inlines)
selflinkOrImage = try $ do
  src <- char '[' *> linkTarget <* char ']'
  return $ linkToInlinesF src (B.str src)

plainLink :: OrgParser (F Inlines)
plainLink = try $ do
  (orig, src) <- uri
  returnF $ B.link src "" (B.str orig)

angleLink :: OrgParser (F Inlines)
angleLink = try $ do
  char '<'
  link <- plainLink
  char '>'
  return link

linkTarget :: OrgParser String
linkTarget = enclosedByPair '[' ']' (noneOf "\n\r[]")

possiblyEmptyLinkTarget :: OrgParser String
possiblyEmptyLinkTarget = try linkTarget <|> ("" <$ string "[]")

applyCustomLinkFormat :: String -> OrgParser (F String)
applyCustomLinkFormat link = do
  let (linkType, rest) = break (== ':') link
  return $ do
    formatter <- M.lookup linkType <$> asksF orgStateLinkFormatters
    return $ maybe link ($ drop 1 rest) formatter

-- | Take a link and return a function which produces new inlines when given
-- description inlines.
linkToInlinesF :: String -> Inlines -> F Inlines
linkToInlinesF linkStr =
  case linkStr of
    ""      -> pure . B.link mempty ""       -- wiki link (empty by convention)
    ('#':_) -> pure . B.link linkStr ""      -- document-local fraction
    _       -> case cleanLinkString linkStr of
                 (Just cleanedLink) -> if isImageFilename cleanedLink
                                       then const . pure $ B.image cleanedLink "" ""
                                       else pure . B.link cleanedLink ""
                 Nothing -> internalLink linkStr  -- other internal link

internalLink :: String -> Inlines -> F Inlines
internalLink link title = do
  anchorB <- (link `elem`) <$> asksF orgStateAnchorIds
  if anchorB
    then return $ B.link ('#':link) "" title
    else return $ B.emph title

-- | Parse an anchor like @<<anchor-id>>@ and return an empty span with
-- @anchor-id@ set as id.  Legal anchors in org-mode are defined through
-- @org-target-regexp@, which is fairly liberal.  Since no link is created if
-- @anchor-id@ contains spaces, we are more restrictive in what is accepted as
-- an anchor.

anchor :: OrgParser (F Inlines)
anchor =  try $ do
  anchorId <- parseAnchor
  recordAnchorId anchorId
  returnF $ B.spanWith (solidify anchorId, [], []) mempty
 where
       parseAnchor = string "<<"
                     *> many1 (noneOf "\t\n\r<>\"' ")
                     <* string ">>"
                     <* skipSpaces

-- | Replace every char but [a-zA-Z0-9_.-:] with a hypen '-'.  This mirrors
-- the org function @org-export-solidify-link-text@.

solidify :: String -> String
solidify = map replaceSpecialChar
 where replaceSpecialChar c
           | isAlphaNum c    = c
           | c `elem` ("_.-:" :: String) = c
           | otherwise       = '-'

-- | Parses an inline code block and marks it as an babel block.
inlineCodeBlock :: OrgParser (F Inlines)
inlineCodeBlock = try $ do
  string "src_"
  lang <- many1 orgArgWordChar
  opts <- option [] $ enclosedByPair '[' ']' inlineBlockOption
  inlineCode <- enclosedByPair '{' '}' (noneOf "\n\r")
  let attrClasses = [translateLang lang, rundocBlockClass]
  let attrKeyVal  = map toRundocAttrib (("language", lang) : opts)
  returnF $ B.codeWith ("", attrClasses, attrKeyVal) inlineCode
 where
   inlineBlockOption :: OrgParser (String, String)
   inlineBlockOption = try $ do
     argKey <- orgArgKey
     paramValue <- option "yes" orgInlineParamValue
     return (argKey, paramValue)

   orgInlineParamValue :: OrgParser String
   orgInlineParamValue = try $
     skipSpaces
       *> notFollowedBy (char ':')
       *> many1 (noneOf "\t\n\r ]")
       <* skipSpaces


emphasizedText :: OrgParser (F Inlines)
emphasizedText = do
  state <- getState
  guard . exportEmphasizedText . orgStateExportSettings $ state
  try $ choice
    [ emph
    , strong
    , strikeout
    , underline
    ]

enclosedByPair :: Char          -- ^ opening char
               -> Char          -- ^ closing char
               -> OrgParser a   -- ^ parser
               -> OrgParser [a]
enclosedByPair s e p = char s *> many1Till p (char e)

emph      :: OrgParser (F Inlines)
emph      = fmap B.emph         <$> emphasisBetween '/'

strong    :: OrgParser (F Inlines)
strong    = fmap B.strong       <$> emphasisBetween '*'

strikeout :: OrgParser (F Inlines)
strikeout = fmap B.strikeout    <$> emphasisBetween '+'

-- There is no underline, so we use strong instead.
underline :: OrgParser (F Inlines)
underline = fmap B.strong       <$> emphasisBetween '_'

verbatim  :: OrgParser (F Inlines)
verbatim  = return . B.code     <$> verbatimBetween '='

code      :: OrgParser (F Inlines)
code      = return . B.code     <$> verbatimBetween '~'

subscript   :: OrgParser (F Inlines)
subscript   = fmap B.subscript   <$> try (char '_' *> subOrSuperExpr)

superscript :: OrgParser (F Inlines)
superscript = fmap B.superscript <$> try (char '^' *> subOrSuperExpr)

math      :: OrgParser (F Inlines)
math      = return . B.math      <$> choice [ math1CharBetween '$'
                                            , mathStringBetween '$'
                                            , rawMathBetween "\\(" "\\)"
                                            ]

displayMath :: OrgParser (F Inlines)
displayMath = return . B.displayMath <$> choice [ rawMathBetween "\\[" "\\]"
                                                , rawMathBetween "$$"  "$$"
                                                ]

updatePositions :: Char
                -> OrgParser Char
updatePositions c = do
  when (c `elem` emphasisPreChars) updateLastPreCharPos
  when (c `elem` emphasisForbiddenBorderChars) updateLastForbiddenCharPos
  return c

symbol :: OrgParser (F Inlines)
symbol = return . B.str . (: "") <$> (oneOf specialChars >>= updatePositions)

emphasisBetween :: Char
                -> OrgParser (F Inlines)
emphasisBetween c = try $ do
  startEmphasisNewlinesCounting emphasisAllowedNewlines
  res <- enclosedInlines (emphasisStart c) (emphasisEnd c)
  isTopLevelEmphasis <- null . orgStateEmphasisCharStack <$> getState
  when isTopLevelEmphasis
       resetEmphasisNewlines
  return res

verbatimBetween :: Char
                -> OrgParser String
verbatimBetween c = try $
  emphasisStart c *>
  many1TillNOrLessNewlines 1 verbatimChar (emphasisEnd c)
 where
   verbatimChar = noneOf "\n\r" >>= updatePositions

-- | Parses a raw string delimited by @c@ using Org's math rules
mathStringBetween :: Char
                  -> OrgParser String
mathStringBetween c = try $ do
  mathStart c
  body <- many1TillNOrLessNewlines mathAllowedNewlines
                                   (noneOf (c:"\n\r"))
                                   (lookAhead $ mathEnd c)
  final <- mathEnd c
  return $ body ++ [final]

-- | Parse a single character between @c@ using math rules
math1CharBetween :: Char
                -> OrgParser String
math1CharBetween c = try $ do
  char c
  res <- noneOf $ c:mathForbiddenBorderChars
  char c
  eof <|> () <$ lookAhead (oneOf mathPostChars)
  return [res]

rawMathBetween :: String
               -> String
               -> OrgParser String
rawMathBetween s e = try $ string s *> manyTill anyChar (try $ string e)

-- | Parses the start (opening character) of emphasis
emphasisStart :: Char -> OrgParser Char
emphasisStart c = try $ do
  guard =<< afterEmphasisPreChar
  guard =<< notAfterString
  char c
  lookAhead (noneOf emphasisForbiddenBorderChars)
  pushToInlineCharStack c
  return c

-- | Parses the closing character of emphasis
emphasisEnd :: Char -> OrgParser Char
emphasisEnd c = try $ do
  guard =<< notAfterForbiddenBorderChar
  char c
  eof <|> () <$ lookAhead acceptablePostChars
  updateLastStrPos
  popInlineCharStack
  return c
 where acceptablePostChars =
           surroundingEmphasisChar >>= \x -> oneOf (x ++ emphasisPostChars)

mathStart :: Char -> OrgParser Char
mathStart c = try $
  char c <* notFollowedBy' (oneOf (c:mathForbiddenBorderChars))

mathEnd :: Char -> OrgParser Char
mathEnd c = try $ do
  res <- noneOf (c:mathForbiddenBorderChars)
  char c
  eof <|> () <$ lookAhead (oneOf mathPostChars)
  return res


enclosedInlines :: OrgParser a
                -> OrgParser b
                -> OrgParser (F Inlines)
enclosedInlines start end = try $
  trimInlinesF . mconcat <$> enclosed start end inline

enclosedRaw :: OrgParser a
            -> OrgParser b
            -> OrgParser String
enclosedRaw start end = try $
  start *> (onSingleLine <|> spanningTwoLines)
 where onSingleLine = try $ many1Till (noneOf "\n\r") end
       spanningTwoLines = try $
         anyLine >>= \f -> mappend (f <> " ") <$> onSingleLine

-- | Like many1Till, but parses at most @n+1@ lines.  @p@ must not consume
--   newlines.
many1TillNOrLessNewlines :: Int
                         -> OrgParser Char
                         -> OrgParser a
                         -> OrgParser String
many1TillNOrLessNewlines n p end = try $
  nMoreLines (Just n) mempty >>= oneOrMore
 where
   nMoreLines Nothing  cs = return cs
   nMoreLines (Just 0) cs = try $ (cs ++) <$> finalLine
   nMoreLines k        cs = try $ (final k cs <|> rest k cs)
                                  >>= uncurry nMoreLines
   final _ cs = (\x -> (Nothing,      cs ++ x)) <$> try finalLine
   rest  m cs = (\x -> (minus1 <$> m, cs ++ x ++ "\n")) <$> try (manyTill p newline)
   finalLine = try $ manyTill p end
   minus1 k = k - 1
   oneOrMore cs = guard (not $ null cs) *> return cs

-- Org allows customization of the way it reads emphasis.  We use the defaults
-- here (see, e.g., the Emacs Lisp variable `org-emphasis-regexp-components`
-- for details).

-- | Chars allowed to occur before emphasis (spaces and newlines are ok, too)
emphasisPreChars :: [Char]
emphasisPreChars = "\t \"'({"

-- | Chars allowed at after emphasis
emphasisPostChars :: [Char]
emphasisPostChars = "\t\n !\"'),-.:;?\\}"

-- | Chars not allowed at the (inner) border of emphasis
emphasisForbiddenBorderChars :: [Char]
emphasisForbiddenBorderChars = "\t\n\r \"',"

-- | The maximum number of newlines within
emphasisAllowedNewlines :: Int
emphasisAllowedNewlines = 1

-- LaTeX-style math: see `org-latex-regexps` for details

-- | Chars allowed after an inline ($...$) math statement
mathPostChars :: [Char]
mathPostChars = "\t\n \"'),-.:;?"

-- | Chars not allowed at the (inner) border of math
mathForbiddenBorderChars :: [Char]
mathForbiddenBorderChars = "\t\n\r ,;.$"

-- | Maximum number of newlines in an inline math statement
mathAllowedNewlines :: Int
mathAllowedNewlines = 2

-- | Whether we are right behind a char allowed before emphasis
afterEmphasisPreChar :: OrgParser Bool
afterEmphasisPreChar = do
  pos <- getPosition
  lastPrePos <- orgStateLastPreCharPos <$> getState
  return . fromMaybe True $ (== pos) <$> lastPrePos

-- | Whether the parser is right after a forbidden border char
notAfterForbiddenBorderChar :: OrgParser Bool
notAfterForbiddenBorderChar = do
  pos <- getPosition
  lastFBCPos <- orgStateLastForbiddenCharPos <$> getState
  return $ lastFBCPos /= Just pos

-- | Read a sub- or superscript expression
subOrSuperExpr :: OrgParser (F Inlines)
subOrSuperExpr = try $
  choice [ id                   <$> charsInBalanced '{' '}' (noneOf "\n\r")
         , enclosing ('(', ')') <$> charsInBalanced '(' ')' (noneOf "\n\r")
         , simpleSubOrSuperString
         ] >>= parseFromString (mconcat <$> many inline)
 where enclosing (left, right) s = left : s ++ [right]

simpleSubOrSuperString :: OrgParser String
simpleSubOrSuperString = try $ do
  state <- getState
  guard . exportSubSuperscripts . orgStateExportSettings $ state
  choice [ string "*"
         , mappend <$> option [] ((:[]) <$> oneOf "+-")
                   <*> many1 alphaNum
         ]

inlineLaTeX :: OrgParser (F Inlines)
inlineLaTeX = try $ do
  cmd <- inlineLaTeXCommand
  maybe mzero returnF $
     parseAsMath cmd `mplus` parseAsMathMLSym cmd `mplus` parseAsInlineLaTeX cmd
 where
   parseAsMath :: String -> Maybe Inlines
   parseAsMath cs = B.fromList <$> texMathToPandoc cs

   parseAsInlineLaTeX :: String -> Maybe Inlines
   parseAsInlineLaTeX cs = maybeRight $ runParser inlineCommand state "" cs

   parseAsMathMLSym :: String -> Maybe Inlines
   parseAsMathMLSym cs = B.str <$> MathMLEntityMap.getUnicode (clean cs)
    -- drop initial backslash and any trailing "{}"
    where clean = dropWhileEnd (`elem` ("{}" :: String)) . drop 1

   state :: ParserState
   state = def{ stateOptions = def{ readerParseRaw = True }}

   texMathToPandoc :: String -> Maybe [Inline]
   texMathToPandoc cs = (maybeRight $ readTeX cs) >>= writePandoc DisplayInline

maybeRight :: Either a b -> Maybe b
maybeRight = either (const Nothing) Just

inlineLaTeXCommand :: OrgParser String
inlineLaTeXCommand = try $ do
  rest <- getInput
  case runParser rawLaTeXInline def "source" rest of
    Right (RawInline _ cs) -> do
      -- drop any trailing whitespace, those are not be part of the command as
      -- far as org mode is concerned.
      let cmdNoSpc = dropWhileEnd isSpace cs
      let len = length cmdNoSpc
      count len anyChar
      return cmdNoSpc
    _ -> mzero

-- Taken from Data.OldList.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

exportSnippet :: OrgParser (F Inlines)
exportSnippet = try $ do
  string "@@"
  format <- many1Till (alphaNum <|> char '-') (char ':')
  snippet <- manyTill anyChar (try $ string "@@")
  returnF $ B.rawInline format snippet

smart :: OrgParser (F Inlines)
smart = do
  getOption readerSmart >>= guard
  doubleQuoted <|> singleQuoted <|>
    choice (map (return <$>) [orgApostrophe, orgDash, orgEllipses])
  where
    orgDash = do
      guard =<< getExportSetting exportSpecialStrings
      dash <* updatePositions '-'
    orgEllipses = do
      guard =<< getExportSetting exportSpecialStrings
      ellipses <* updatePositions '.'
    orgApostrophe =
          (char '\'' <|> char '\8217') <* updateLastPreCharPos
                                       <* updateLastForbiddenCharPos
                                       *> return (B.str "\x2019")

singleQuoted :: OrgParser (F Inlines)
singleQuoted = try $ do
  guard =<< getExportSetting exportSmartQuotes
  singleQuoteStart
  updatePositions '\''
  withQuoteContext InSingleQuote $
    fmap B.singleQuoted . trimInlinesF . mconcat <$>
      many1Till inline (singleQuoteEnd <* updatePositions '\'')

-- doubleQuoted will handle regular double-quoted sections, as well
-- as dialogues with an open double-quote without a close double-quote
-- in the same paragraph.
doubleQuoted :: OrgParser (F Inlines)
doubleQuoted = try $ do
  guard =<< getExportSetting exportSmartQuotes
  doubleQuoteStart
  updatePositions '"'
  contents <- mconcat <$> many (try $ notFollowedBy doubleQuoteEnd >> inline)
  (withQuoteContext InDoubleQuote $ (doubleQuoteEnd <* updateLastForbiddenCharPos) >> return
       (fmap B.doubleQuoted . trimInlinesF $ contents))
   <|> (return $ return (B.str "\8220") <> contents)
