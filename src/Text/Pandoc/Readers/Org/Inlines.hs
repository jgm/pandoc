{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2014-2017 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

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
   Module      : Text.Pandoc.Readers.Org.Inlines
   Copyright   : Copyright (C) 2014-2017 Albert Krewinkel
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

import Text.Pandoc.Readers.Org.BlockStarts (endOfBlock, noteMarker)
import Text.Pandoc.Readers.Org.ParserState
import Text.Pandoc.Readers.Org.Parsing
import Text.Pandoc.Readers.Org.Shared (cleanLinkString, isImageFilename,
                                       originalLang, translateLang)

import Text.Pandoc.Builder (Inlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Readers.LaTeX (inlineCommand, rawLaTeXInline)
import Text.TeXMath (DisplayType (..), readTeX, writePandoc)
import qualified Text.TeXMath.Readers.MathML.EntityMap as MathMLEntityMap

import Control.Monad (guard, mplus, mzero, unless, void, when)
import Control.Monad.Trans (lift)
import Data.Char (isAlphaNum, isSpace)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Traversable (sequence)
import Prelude hiding (sequence)

--
-- Functions acting on the parser state
--
recordAnchorId :: PandocMonad m => String -> OrgParser m ()
recordAnchorId i = updateState $ \s ->
  s{ orgStateAnchorIds = i : orgStateAnchorIds s }

pushToInlineCharStack :: PandocMonad m => Char -> OrgParser m ()
pushToInlineCharStack c = updateState $ \s ->
  s{ orgStateEmphasisCharStack = c:orgStateEmphasisCharStack s }

popInlineCharStack :: PandocMonad m => OrgParser m ()
popInlineCharStack = updateState $ \s ->
  s{ orgStateEmphasisCharStack = drop 1 . orgStateEmphasisCharStack $ s }

surroundingEmphasisChar :: PandocMonad m => OrgParser m [Char]
surroundingEmphasisChar =
  take 1 . drop 1 . orgStateEmphasisCharStack <$> getState

startEmphasisNewlinesCounting :: PandocMonad m => Int -> OrgParser m ()
startEmphasisNewlinesCounting maxNewlines = updateState $ \s ->
  s{ orgStateEmphasisNewlines = Just maxNewlines }

decEmphasisNewlinesCount :: PandocMonad m => OrgParser m ()
decEmphasisNewlinesCount = updateState $ \s ->
  s{ orgStateEmphasisNewlines = (\n -> n - 1) <$> orgStateEmphasisNewlines s }

newlinesCountWithinLimits :: PandocMonad m => OrgParser m Bool
newlinesCountWithinLimits = do
  st <- getState
  return $ ((< 0) <$> orgStateEmphasisNewlines st) /= Just True

resetEmphasisNewlines :: PandocMonad m => OrgParser m ()
resetEmphasisNewlines = updateState $ \s ->
  s{ orgStateEmphasisNewlines = Nothing }

addToNotesTable :: PandocMonad m => OrgNoteRecord -> OrgParser m ()
addToNotesTable note = do
  oldnotes <- orgStateNotes' <$> getState
  updateState $ \s -> s{ orgStateNotes' = note:oldnotes }

-- | Parse a single Org-mode inline element
inline :: PandocMonad m => OrgParser m (F Inlines)
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
         , macro
         , smart
         , symbol
         ] <* (guard =<< newlinesCountWithinLimits)
  <?> "inline"

-- | Read the rest of the input as inlines.
inlines :: PandocMonad m => OrgParser m (F Inlines)
inlines = trimInlinesF . mconcat <$> many1 inline

-- treat these as potentially non-text when parsing inline:
specialChars :: [Char]
specialChars = "\"$'()*+-,./:;<=>@[\\]^_{|}~"


whitespace :: PandocMonad m => OrgParser m (F Inlines)
whitespace = pure B.space <$ skipMany1 spaceChar
                          <* updateLastPreCharPos
                          <* updateLastForbiddenCharPos
             <?> "whitespace"

linebreak :: PandocMonad m => OrgParser m (F Inlines)
linebreak = try $ pure B.linebreak <$ string "\\\\" <* skipSpaces <* newline

str :: PandocMonad m => OrgParser m (F Inlines)
str = return . B.str <$> many1 (noneOf $ specialChars ++ "\n\r ")
      <* updateLastStrPos

-- | An endline character that can be treated as a space, not a structural
-- break.  This should reflect the values of the Emacs variable
-- @org-element-pagaraph-separate@.
endline :: PandocMonad m => OrgParser m (F Inlines)
endline = try $ do
  newline
  notFollowedBy' endOfBlock
  decEmphasisNewlinesCount
  guard =<< newlinesCountWithinLimits
  updateLastPreCharPos
  returnF B.softbreak


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

cite :: PandocMonad m => OrgParser m (F Inlines)
cite = try $ berkeleyCite <|> do
  guardEnabled Ext_citations
  (cs, raw) <- withRaw $ choice
               [ pandocOrgCite
               , orgRefCite
               , berkeleyTextualCite
               ]
  return $ flip B.cite (B.text raw) <$> cs

-- | A citation in Pandoc Org-mode style (@[prefix \@citekey suffix]@).
pandocOrgCite :: PandocMonad m => OrgParser m (F [Citation])
pandocOrgCite = try $
  char '[' *> skipSpaces *> citeList <* skipSpaces <* char ']'

orgRefCite :: PandocMonad m => OrgParser m (F [Citation])
orgRefCite = try $ choice
  [ normalOrgRefCite
  , fmap (:[]) <$> linkLikeOrgRefCite
  ]

normalOrgRefCite :: PandocMonad m => OrgParser m (F [Citation])
normalOrgRefCite = try $ do
  mode <- orgRefCiteMode
  firstCitation <- orgRefCiteList mode
  moreCitations <- many (try $ char ',' *> orgRefCiteList mode)
  return . sequence $ firstCitation : moreCitations
 where
  -- | A list of org-ref style citation keys, parsed as citation of the given
  -- citation mode.
  orgRefCiteList :: PandocMonad m => CitationMode -> OrgParser m (F Citation)
  orgRefCiteList citeMode = try $ do
    key <- orgRefCiteKey
    returnF Citation
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
berkeleyCite :: PandocMonad m => OrgParser m (F Inlines)
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
           . maybe id (alterFirst . prependPrefix) prefix
           . maybe id (alterLast . appendSuffix) suffix
           $ citationList
      else maybe mempty (<> " ") prefix
             <> toListOfCites (map toInTextMode citationList)
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
   alterFirst f (c:cs) = f c : cs
   alterLast  f = reverse . alterFirst f . reverse

   prependPrefix, appendSuffix :: Inlines -> Citation -> Citation
   prependPrefix pre c = c { citationPrefix = B.toList pre <> citationPrefix c }
   appendSuffix  suf c = c { citationSuffix = citationSuffix c <> B.toList suf }

data BerkeleyCitationList = BerkeleyCitationList
  { berkeleyCiteParens       :: Bool
  , berkeleyCiteCommonPrefix :: Maybe Inlines
  , berkeleyCiteCommonSuffix :: Maybe Inlines
  , berkeleyCiteCitations    :: [Citation]
  }
berkeleyCitationList :: PandocMonad m => OrgParser m (F BerkeleyCitationList)
berkeleyCitationList = try $ do
  char '['
  parens <- choice [ False <$ berkeleyBareTag, True <$ berkeleyParensTag ]
  char ':'
  skipSpaces
  commonPrefix <- optionMaybe (try $ citationListPart <* char ';')
  citations    <- citeList
  commonSuffix <- optionMaybe (try citationListPart)
  char ']'
  return (BerkeleyCitationList parens
    <$> sequence commonPrefix
    <*> sequence commonSuffix
    <*> citations)
 where
   citationListPart :: PandocMonad m => OrgParser m (F Inlines)
   citationListPart = fmap (trimInlinesF . mconcat) . try . many1 $ do
     notFollowedBy' citeKey
     notFollowedBy (oneOf ";]")
     inline

berkeleyBareTag :: PandocMonad m => OrgParser m ()
berkeleyBareTag = try $ void berkeleyBareTag'

berkeleyParensTag :: PandocMonad m => OrgParser m ()
berkeleyParensTag = try . void $ enclosedByPair '(' ')' berkeleyBareTag'

berkeleyBareTag' :: PandocMonad m => OrgParser m ()
berkeleyBareTag' = try $ void (string "cite")

berkeleyTextualCite :: PandocMonad m => OrgParser m (F [Citation])
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
-- berkeleyBracketedTextualCite :: PandocMonad m => OrgParser m (F [Citation])
-- berkeleyBracketedTextualCite = try . (fmap head) $
--   enclosedByPair '[' ']' berkeleyTextualCite

-- | Read a link-like org-ref style citation.  The citation includes pre and
-- post text.  However, multiple citations are not possible due to limitations
-- in the syntax.
linkLikeOrgRefCite :: PandocMonad m => OrgParser m (F Citation)
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
orgRefCiteKey :: PandocMonad m => OrgParser m String
orgRefCiteKey =
  let citeKeySpecialChars = "-_:\\./," :: String
      isCiteKeySpecialChar c = c `elem` citeKeySpecialChars
      isCiteKeyChar c = isAlphaNum c || isCiteKeySpecialChar c
      endOfCitation = try $ do
        many $ satisfy isCiteKeySpecialChar
        satisfy $ not . isCiteKeyChar
  in try $ satisfy isCiteKeyChar `many1Till` lookAhead endOfCitation


-- | Supported citation types.  Only a small subset of org-ref types is
-- supported for now.  TODO: rewrite this, use LaTeX reader as template.
orgRefCiteMode :: PandocMonad m => OrgParser m CitationMode
orgRefCiteMode =
  choice $ map (\(s, mode) -> mode <$ try (string s <* char ':'))
    [ ("cite", AuthorInText)
    , ("citep", NormalCitation)
    , ("citep*", NormalCitation)
    , ("citet", AuthorInText)
    , ("citet*", AuthorInText)
    , ("citeyear", SuppressAuthor)
    ]

citeList :: PandocMonad m => OrgParser m (F [Citation])
citeList = sequence <$> sepEndBy1 citation (try $ char ';' *> skipSpaces)

citation :: PandocMonad m => OrgParser m (F Citation)
citation = try $ do
  pref <- prefix
  (suppress_author, key) <- citeKey
  suff <- suffix
  return $ do
    x <- pref
    y <- suff
    return Citation
      { citationId      = key
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

footnote :: PandocMonad m => OrgParser m (F Inlines)
footnote = try $ inlineNote <|> referencedNote

inlineNote :: PandocMonad m => OrgParser m (F Inlines)
inlineNote = try $ do
  string "[fn:"
  ref <- many alphaNum
  char ':'
  note <- fmap B.para . trimInlinesF . mconcat <$> many1Till inline (char ']')
  unless (null ref) $
       addToNotesTable ("fn:" ++ ref, note)
  return $ B.note <$> note

referencedNote :: PandocMonad m => OrgParser m (F Inlines)
referencedNote = try $ do
  ref <- noteMarker
  return $ do
    notes <- asksF orgStateNotes'
    case lookup ref notes of
      Nothing   -> return . B.str $ "[" ++ ref ++ "]"
      Just contents  -> do
        st <- askF
        let contents' = runF contents st{ orgStateNotes' = [] }
        return $ B.note contents'

linkOrImage :: PandocMonad m => OrgParser m (F Inlines)
linkOrImage = explicitOrImageLink
              <|> selflinkOrImage
              <|> angleLink
              <|> plainLink
              <?> "link or image"

explicitOrImageLink :: PandocMonad m => OrgParser m (F Inlines)
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
        pure . B.link src "" $ B.image imgSrc mempty mempty
      _ ->
        linkToInlinesF src =<< title'

selflinkOrImage :: PandocMonad m => OrgParser m (F Inlines)
selflinkOrImage = try $ do
  src <- char '[' *> linkTarget <* char ']'
  return $ linkToInlinesF src (B.str src)

plainLink :: PandocMonad m => OrgParser m (F Inlines)
plainLink = try $ do
  (orig, src) <- uri
  returnF $ B.link src "" (B.str orig)

angleLink :: PandocMonad m => OrgParser m (F Inlines)
angleLink = try $ do
  char '<'
  link <- plainLink
  char '>'
  return link

linkTarget :: PandocMonad m => OrgParser m String
linkTarget = enclosedByPair '[' ']' (noneOf "\n\r[]")

possiblyEmptyLinkTarget :: PandocMonad m => OrgParser m String
possiblyEmptyLinkTarget = try linkTarget <|> ("" <$ string "[]")

applyCustomLinkFormat :: String -> OrgParser m (F String)
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

anchor :: PandocMonad m => OrgParser m (F Inlines)
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
inlineCodeBlock :: PandocMonad m => OrgParser m (F Inlines)
inlineCodeBlock = try $ do
  string "src_"
  lang <- many1 orgArgWordChar
  opts <- option [] $ enclosedByPair '[' ']' inlineBlockOption
  inlineCode <- enclosedByPair '{' '}' (noneOf "\n\r")
  let attrClasses = [translateLang lang]
  let attrKeyVal  = originalLang lang <> opts
  returnF $ B.codeWith ("", attrClasses, attrKeyVal) inlineCode
 where
   inlineBlockOption :: PandocMonad m => OrgParser m (String, String)
   inlineBlockOption = try $ do
     argKey <- orgArgKey
     paramValue <- option "yes" orgInlineParamValue
     return (argKey, paramValue)

   orgInlineParamValue :: PandocMonad m => OrgParser m String
   orgInlineParamValue = try $
     skipSpaces
       *> notFollowedBy (char ':')
       *> many1 (noneOf "\t\n\r ]")
       <* skipSpaces


emphasizedText :: PandocMonad m => OrgParser m (F Inlines)
emphasizedText = do
  state <- getState
  guard . exportEmphasizedText . orgStateExportSettings $ state
  try $ choice
    [ emph
    , strong
    , strikeout
    , underline
    ]

enclosedByPair :: PandocMonad m
               => Char          -- ^ opening char
               -> Char          -- ^ closing char
               -> OrgParser m a   -- ^ parser
               -> OrgParser m [a]
enclosedByPair s e p = char s *> many1Till p (char e)

emph      :: PandocMonad m => OrgParser m (F Inlines)
emph      = fmap B.emph         <$> emphasisBetween '/'

strong    :: PandocMonad m => OrgParser m (F Inlines)
strong    = fmap B.strong       <$> emphasisBetween '*'

strikeout :: PandocMonad m => OrgParser m (F Inlines)
strikeout = fmap B.strikeout    <$> emphasisBetween '+'

-- There is no underline, so we use strong instead.
underline :: PandocMonad m => OrgParser m (F Inlines)
underline = fmap B.strong       <$> emphasisBetween '_'

verbatim  :: PandocMonad m => OrgParser m (F Inlines)
verbatim  = return . B.code     <$> verbatimBetween '='

code      :: PandocMonad m => OrgParser m (F Inlines)
code      = return . B.code     <$> verbatimBetween '~'

subscript   :: PandocMonad m => OrgParser m (F Inlines)
subscript   = fmap B.subscript   <$> try (char '_' *> subOrSuperExpr)

superscript :: PandocMonad m => OrgParser m (F Inlines)
superscript = fmap B.superscript <$> try (char '^' *> subOrSuperExpr)

math      :: PandocMonad m => OrgParser m (F Inlines)
math      = return . B.math      <$> choice [ math1CharBetween '$'
                                            , mathStringBetween '$'
                                            , rawMathBetween "\\(" "\\)"
                                            ]

displayMath :: PandocMonad m => OrgParser m (F Inlines)
displayMath = return . B.displayMath <$> choice [ rawMathBetween "\\[" "\\]"
                                                , rawMathBetween "$$"  "$$"
                                                ]

updatePositions :: PandocMonad m
                => Char
                -> OrgParser m Char
updatePositions c = do
  when (c `elem` emphasisPreChars) updateLastPreCharPos
  when (c `elem` emphasisForbiddenBorderChars) updateLastForbiddenCharPos
  return c

symbol :: PandocMonad m => OrgParser m (F Inlines)
symbol = return . B.str . (: "") <$> (oneOf specialChars >>= updatePositions)

emphasisBetween :: PandocMonad m
                => Char
                -> OrgParser m (F Inlines)
emphasisBetween c = try $ do
  startEmphasisNewlinesCounting emphasisAllowedNewlines
  res <- enclosedInlines (emphasisStart c) (emphasisEnd c)
  isTopLevelEmphasis <- null . orgStateEmphasisCharStack <$> getState
  when isTopLevelEmphasis
       resetEmphasisNewlines
  return res

verbatimBetween :: PandocMonad m
                => Char
                -> OrgParser m String
verbatimBetween c = try $
  emphasisStart c *>
  many1TillNOrLessNewlines 1 verbatimChar (emphasisEnd c)
 where
   verbatimChar = noneOf "\n\r" >>= updatePositions

-- | Parses a raw string delimited by @c@ using Org's math rules
mathStringBetween :: PandocMonad m
                  => Char
                  -> OrgParser m String
mathStringBetween c = try $ do
  mathStart c
  body <- many1TillNOrLessNewlines mathAllowedNewlines
                                   (noneOf (c:"\n\r"))
                                   (lookAhead $ mathEnd c)
  final <- mathEnd c
  return $ body ++ [final]

-- | Parse a single character between @c@ using math rules
math1CharBetween :: PandocMonad m
                 => Char
                -> OrgParser m String
math1CharBetween c = try $ do
  char c
  res <- noneOf $ c:mathForbiddenBorderChars
  char c
  eof <|> () <$ lookAhead (oneOf mathPostChars)
  return [res]

rawMathBetween :: PandocMonad m
               => String
               -> String
               -> OrgParser m String
rawMathBetween s e = try $ string s *> manyTill anyChar (try $ string e)

-- | Parses the start (opening character) of emphasis
emphasisStart :: PandocMonad m => Char -> OrgParser m Char
emphasisStart c = try $ do
  guard =<< afterEmphasisPreChar
  guard =<< notAfterString
  char c
  lookAhead (noneOf emphasisForbiddenBorderChars)
  pushToInlineCharStack c
  -- nested inlines are allowed, so mark this position as one which might be
  -- followed by another inline.
  updateLastPreCharPos
  return c

-- | Parses the closing character of emphasis
emphasisEnd :: PandocMonad m => Char -> OrgParser m Char
emphasisEnd c = try $ do
  guard =<< notAfterForbiddenBorderChar
  char c
  eof <|> () <$ lookAhead acceptablePostChars
  updateLastStrPos
  popInlineCharStack
  return c
 where acceptablePostChars =
           surroundingEmphasisChar >>= \x -> oneOf (x ++ emphasisPostChars)

mathStart :: PandocMonad m => Char -> OrgParser m Char
mathStart c = try $
  char c <* notFollowedBy' (oneOf (c:mathForbiddenBorderChars))

mathEnd :: PandocMonad m => Char -> OrgParser m Char
mathEnd c = try $ do
  res <- noneOf (c:mathForbiddenBorderChars)
  char c
  eof <|> () <$ lookAhead (oneOf mathPostChars)
  return res


enclosedInlines :: (PandocMonad m, Show b) => OrgParser m a
                -> OrgParser m b
                -> OrgParser m (F Inlines)
enclosedInlines start end = try $
  trimInlinesF . mconcat <$> enclosed start end inline

enclosedRaw :: (PandocMonad m, Show b) => OrgParser m a
            -> OrgParser m b
            -> OrgParser m String
enclosedRaw start end = try $
  start *> (onSingleLine <|> spanningTwoLines)
 where onSingleLine = try $ many1Till (noneOf "\n\r") end
       spanningTwoLines = try $
         anyLine >>= \f -> mappend (f <> " ") <$> onSingleLine

-- | Like many1Till, but parses at most @n+1@ lines.  @p@ must not consume
--   newlines.
many1TillNOrLessNewlines :: PandocMonad m => Int
                         -> OrgParser m Char
                         -> OrgParser m a
                         -> OrgParser m String
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
emphasisPostChars = "\t\n !\"'),-.:;?\\}["

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
afterEmphasisPreChar :: PandocMonad m => OrgParser m Bool
afterEmphasisPreChar = do
  pos <- getPosition
  lastPrePos <- orgStateLastPreCharPos <$> getState
  return . fromMaybe True $ (== pos) <$> lastPrePos

-- | Whether the parser is right after a forbidden border char
notAfterForbiddenBorderChar :: PandocMonad m => OrgParser m Bool
notAfterForbiddenBorderChar = do
  pos <- getPosition
  lastFBCPos <- orgStateLastForbiddenCharPos <$> getState
  return $ lastFBCPos /= Just pos

-- | Read a sub- or superscript expression
subOrSuperExpr :: PandocMonad m => OrgParser m (F Inlines)
subOrSuperExpr = try $
  choice [ charsInBalanced '{' '}' (noneOf "\n\r")
         , enclosing ('(', ')') <$> charsInBalanced '(' ')' (noneOf "\n\r")
         , simpleSubOrSuperString
         ] >>= parseFromString (mconcat <$> many inline)
 where enclosing (left, right) s = left : s ++ [right]

simpleSubOrSuperString :: PandocMonad m => OrgParser m String
simpleSubOrSuperString = try $ do
  state <- getState
  guard . exportSubSuperscripts . orgStateExportSettings $ state
  choice [ string "*"
         , mappend <$> option [] ((:[]) <$> oneOf "+-")
                   <*> many1 alphaNum
         ]

inlineLaTeX :: PandocMonad m => OrgParser m (F Inlines)
inlineLaTeX = try $ do
  cmd <- inlineLaTeXCommand
  ils <- (lift . lift) $ parseAsInlineLaTeX cmd
  maybe mzero returnF $
     parseAsMath cmd `mplus` parseAsMathMLSym cmd `mplus` ils
 where
   parseAsMath :: String -> Maybe Inlines
   parseAsMath cs = B.fromList <$> texMathToPandoc cs

   parseAsInlineLaTeX :: PandocMonad m => String -> m (Maybe Inlines)
   parseAsInlineLaTeX cs = maybeRight <$> runParserT inlineCommand state "" cs

   parseAsMathMLSym :: String -> Maybe Inlines
   parseAsMathMLSym cs = B.str <$> MathMLEntityMap.getUnicode (clean cs)
    -- drop initial backslash and any trailing "{}"
    where clean = dropWhileEnd (`elem` ("{}" :: String)) . drop 1

   state :: ParserState
   state = def{ stateOptions = def{ readerExtensions =
                    enableExtension Ext_raw_tex (readerExtensions def) } }

   texMathToPandoc :: String -> Maybe [Inline]
   texMathToPandoc cs = maybeRight (readTeX cs) >>= writePandoc DisplayInline

maybeRight :: Either a b -> Maybe b
maybeRight = either (const Nothing) Just

inlineLaTeXCommand :: PandocMonad m => OrgParser m String
inlineLaTeXCommand = try $ do
  rest <- getInput
  st <- getState
  parsed <- (lift . lift) $ runParserT rawLaTeXInline st "source" rest
  case parsed of
    Right cs -> do
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

exportSnippet :: PandocMonad m => OrgParser m (F Inlines)
exportSnippet = try $ do
  string "@@"
  format <- many1Till (alphaNum <|> char '-') (char ':')
  snippet <- manyTill anyChar (try $ string "@@")
  returnF $ B.rawInline format snippet

macro :: PandocMonad m => OrgParser m (F Inlines)
macro = try $ do
  recursionDepth <- orgStateMacroDepth <$> getState
  guard $ recursionDepth < 15
  string "{{{"
  name <- many alphaNum
  args <- ([] <$ string "}}}")
          <|> char '(' *> argument `sepBy` char ',' <* eoa
  expander <- lookupMacro name <$> getState
  case expander of
    Nothing -> mzero
    Just fn -> do
      updateState $ \s -> s { orgStateMacroDepth = recursionDepth + 1 }
      res <- parseFromString (mconcat <$> many inline) $ fn args
      updateState $ \s -> s { orgStateMacroDepth = recursionDepth }
      return res
 where
  argument = many $ notFollowedBy eoa *> noneOf ","
  eoa = string ")}}}"

smart :: PandocMonad m => OrgParser m (F Inlines)
smart = choice [doubleQuoted, singleQuoted, orgApostrophe, orgDash, orgEllipses]
  where
    orgDash = do
      guardOrSmartEnabled =<< getExportSetting exportSpecialStrings
      pure <$> dash <* updatePositions '-'
    orgEllipses = do
      guardOrSmartEnabled =<< getExportSetting exportSpecialStrings
      pure <$> ellipses <* updatePositions '.'
    orgApostrophe = do
      guardEnabled Ext_smart
      (char '\'' <|> char '\8217') <* updateLastPreCharPos
                                   <* updateLastForbiddenCharPos
      returnF (B.str "\x2019")

guardOrSmartEnabled :: PandocMonad m => Bool -> OrgParser m ()
guardOrSmartEnabled b = do
  smartExtension <- extensionEnabled Ext_smart <$> getOption readerExtensions
  guard (b || smartExtension)

singleQuoted :: PandocMonad m => OrgParser m (F Inlines)
singleQuoted = try $ do
  guardOrSmartEnabled =<< getExportSetting exportSmartQuotes
  singleQuoteStart
  updatePositions '\''
  withQuoteContext InSingleQuote $
    fmap B.singleQuoted . trimInlinesF . mconcat <$>
      many1Till inline (singleQuoteEnd <* updatePositions '\'')

-- doubleQuoted will handle regular double-quoted sections, as well
-- as dialogues with an open double-quote without a close double-quote
-- in the same paragraph.
doubleQuoted :: PandocMonad m => OrgParser m (F Inlines)
doubleQuoted = try $ do
  guardOrSmartEnabled =<< getExportSetting exportSmartQuotes
  doubleQuoteStart
  updatePositions '"'
  contents <- mconcat <$> many (try $ notFollowedBy doubleQuoteEnd >> inline)
  let doubleQuotedContent = withQuoteContext InDoubleQuote $ do
        doubleQuoteEnd
        updateLastForbiddenCharPos
        return . fmap B.doubleQuoted . trimInlinesF $ contents
  let leftQuoteAndContent = return $ pure (B.str "\8220") <> contents
  doubleQuotedContent <|> leftQuoteAndContent
