{-# LANGUAGE OverloadedStrings   #-}
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
   Module      : Text.Pandoc.Readers.LaTeX
   Copyright   : Copyright (C) 2006-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of LaTeX to 'Pandoc' document.
-}
module Text.Pandoc.Readers.LaTeX ( readLaTeX,
                                   rawLaTeXInline,
                                   rawLaTeXBlock,
                                   inlineCommand,
                                 ) where

import Control.Applicative (many, optional, (<|>))
import Control.Monad
import Control.Monad.Except (throwError)
import Data.Char (chr, isAlphaNum, isLetter, ord, isDigit)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList)
import Safe (minimumDef)
import System.FilePath (addExtension, replaceExtension, takeExtension)
import Text.Pandoc.Builder
import Text.Pandoc.Class (PandocMonad, PandocPure, lookupEnv, readFileFromDirs,
                          report, setResourcePath, getResourcePath,
                          runIOorExplode, PandocIO)
import Text.Pandoc.Highlighting (fromListingsLanguage, languagesByExtension)
import Text.Pandoc.ImageSize (numUnit, showFl)
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (many, optional, withRaw,
                            mathInline, mathDisplay,
                            space, (<|>), spaces, blankline)
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Pandoc.Error (PandocError(PandocParsecError))

import Text.Pandoc.Extensions (getDefaultExtensions)
import Debug.Trace (traceShowId)

-- | Parse LaTeX from string and return 'Pandoc' document.
readLaTeX :: PandocMonad m
          => ReaderOptions -- ^ Reader options
          -> Text        -- ^ String to parse (assumes @'\n'@ line endings)
          -> m Pandoc
readLaTeX opts ltx = do
  parsed <- runParserT parseLaTeX def{ stateOptions = opts } "source"
               (tokenize (crFilter ltx))
  case parsed of
    Right result -> return result
    Left e       -> throwError $ PandocParsecError (T.unpack ltx) e

parseLaTeX :: PandocMonad m => LP m Pandoc
parseLaTeX = do
  bs <- blocks
  eof
  st <- getState
  let meta = stateMeta st
  let doc' = doc bs
  let headerLevel (Header n _ _) = [n]
      headerLevel _ = []
  let bottomLevel = minimumDef 1 $ query headerLevel doc'
  let adjustHeaders m (Header n attr ils) = Header (n+m) attr ils
      adjustHeaders _ x = x
  let (Pandoc _ bs') =
       -- handle the case where you have \part or \chapter
       (if bottomLevel < 1
           then walk (adjustHeaders (1 - bottomLevel))
           else id) doc'
  return $ Pandoc meta bs'

testParser :: LP PandocIO a -> Text -> IO a
testParser p t = do
  res <- runIOorExplode (runParserT p defaultParserState{
            stateOptions = def{ readerExtensions =
              getDefaultExtensions "latex" }} "name" (tokenize t))
  case res of
       Left e  -> error (show e)
       Right r -> return r

type LP m = ParserT [Tok] ParserState m

rawLaTeXBlock :: PandocMonad m => ParserT String ParserState m String
rawLaTeXBlock = mzero

rawLaTeXInline :: PandocMonad m => ParserT String ParserState m Inline
rawLaTeXInline = mzero

inlineCommand :: PandocMonad m => ParserT String ParserState m Inlines
inlineCommand = mzero

data TokType = CtrlSeq | Spaces | Newline | Symbol | Word | Comment |
               Esc1    | Esc2
     deriving (Eq, Ord, Show)

data Tok = Tok (Line, Column) TokType Text
     deriving (Eq, Ord, Show)

tokenize :: Text -> [Tok]
tokenize = totoks (1, 1)

totoks :: (Line, Column) -> Text -> [Tok]
totoks (lin,col) t =
  case T.uncons t of
       Nothing        -> []
       Just (c, rest)
         | c == '\n' ->
           Tok (lin, col) Newline (T.singleton '\n')
           : totoks (lin + 1,1) rest
         | isSpaceOrTab c ->
           let (sps, rest') = T.span isSpaceOrTab t
           in  Tok (lin, col) Spaces sps
               : totoks (lin, col + T.length sps) rest'
         | isAlphaNum c ->
           let (ws, rest') = T.span isAlphaNum t
           in  Tok (lin, col) Word ws
               : totoks (lin, col + T.length ws) rest'
         | c == '%' ->
           let (cs, rest') = T.break (== '\n') rest
           in  Tok (lin, col) Comment (T.singleton '%' <> cs)
               : totoks (lin, col + 1 + T.length cs) rest'
         | c == '\\' ->
           case T.uncons rest of
                Nothing -> [Tok (lin, col) Symbol (T.singleton c)]
                Just (d, rest')
                  | isLetter d ->
                      let (ws, rest'') = T.span isLetter rest
                      in  Tok (lin, col) CtrlSeq (T.singleton '\\' <> ws)
                          : totoks (lin, col + 1 + T.length ws) rest''
                  | d == '\t' || d == '\n' ->
                      Tok (lin, col) Symbol (T.singleton '\\')
                      : totoks (lin, col + 1) rest
                  | otherwise  ->
                      Tok (lin, col) CtrlSeq (T.pack [c,d])
                      : totoks (lin, col + 2) rest'
         | c == '^' ->
           case T.uncons rest of
                Just ('^', rest') ->
                  case T.uncons rest' of
                       Just (d, rest'')
                         | isLowerHex d ->
                           case T.uncons rest'' of
                                Just (e, rest''') | isLowerHex e ->
                                  Tok (lin, col) Esc2 (T.pack ['^','^',d,e])
                                  : totoks (lin, col + 4) rest'''
                                _ ->
                                  Tok (lin, col) Esc1 (T.pack ['^','^',d])
                                  : totoks (lin, col + 3) rest''
                         | d < '\128' ->
                                  Tok (lin, col) Esc1 (T.pack ['^','^',d])
                                  : totoks (lin, col + 3) rest''
                       _ -> [Tok (lin, col) Symbol (T.singleton '^'),
                             Tok (lin, col + 1) Symbol (T.singleton '^')]
                _ -> Tok (lin, col) Symbol (T.singleton '^')
                     : totoks (lin, col + 1) rest
         | otherwise ->
           Tok (lin, col) Symbol (T.singleton c) : totoks (lin, col + 1) rest

  where isSpaceOrTab ' '  = True
        isSpaceOrTab '\t' = True
        isSpaceOrTab _    = False

isLowerHex :: Char -> Bool
isLowerHex x = x >= '0' && x <= '9' || x >= 'a' && x <= 'f'

untokenize :: [Tok] -> Text
untokenize = mconcat . map untoken

untoken :: Tok -> Text
untoken (Tok _ _ t) = t

satisfyTok :: Monad m
           => (Tok -> Bool)
           -> ParserT [Tok] u m Tok
satisfyTok f = tokenPrim (T.unpack . untoken) updatePos matcher
  where matcher t | f t       = Just t
                  | otherwise = Nothing
        updatePos :: SourcePos -> Tok -> [Tok] -> SourcePos
        updatePos spos _ (Tok (lin,col) _ _ : _) =
          setSourceColumn (setSourceLine spos lin) col
        updatePos spos _ [] = spos

anyControlSeq :: PandocMonad m => LP m Tok
anyControlSeq = satisfyTok isCtrlSeq <* optional sp
  where isCtrlSeq (Tok _ CtrlSeq _) = True
        isCtrlSeq _                 = False

anySymbol :: PandocMonad m => LP m Tok
anySymbol = satisfyTok isSym
  where isSym (Tok _ Symbol _) = True
        isSym _                = False

spaces :: PandocMonad m => LP m ()
spaces = skipMany (satisfyTok (tokTypeIn [Comment, Spaces, Newline]))

spaces1 :: PandocMonad m => LP m ()
spaces1 = skipMany1 (satisfyTok (tokTypeIn [Comment, Spaces, Newline]))

tokTypeIn :: [TokType] -> Tok -> Bool
tokTypeIn toktypes (Tok _ tt _) = tt `elem` toktypes

controlSeq :: PandocMonad m => Text -> LP m Tok
controlSeq name = satisfyTok isNamed
  where isNamed (Tok _ CtrlSeq n) = n == T.singleton '\\' <> name
        isNamed _ = False

symbol :: PandocMonad m => Char -> LP m Tok
symbol c = satisfyTok isc
  where isc (Tok _ Symbol d) = case T.uncons d of
                                    Just (c',_) -> c == c'
                                    _ -> False
        isc _ = False

symbolIn :: PandocMonad m => [Char] -> LP m Tok
symbolIn cs = satisfyTok isInCs
  where isInCs (Tok _ Symbol d) = case T.uncons d of
                                       Just (c,_) -> c `elem` cs
                                       _ -> False
        isInCs _ = False

sp :: PandocMonad m => LP m ()
sp = whitespace <|> endline

whitespace :: PandocMonad m => LP m ()
whitespace = () <$ satisfyTok isSpaceTok
  where isSpaceTok (Tok _ Spaces _) = True
        isSpaceTok _ = False

newlineTok :: PandocMonad m => LP m ()
newlineTok = () <$ satisfyTok isNewlineTok

isNewlineTok :: Tok -> Bool
isNewlineTok (Tok _ Newline _) = True
isNewlineTok _ = False

comment :: PandocMonad m => LP m ()
comment = () <$ satisfyTok isCommentTok
  where isCommentTok (Tok _ Comment _) = True
        isCommentTok _ = False

anyTok :: PandocMonad m => LP m Tok
anyTok = satisfyTok (const True)

endline :: PandocMonad m => LP m ()
endline = try $ do
  newlineTok
  lookAhead anyTok
  notFollowedBy blankline

blankline :: PandocMonad m => LP m ()
blankline = try $ do
  skipMany (satisfyTok (tokTypeIn [Spaces, Newline]))
  newlineTok

primEscape :: PandocMonad m => LP m Char
primEscape = do
  Tok _ toktype t <- satisfyTok (tokTypeIn [Esc1, Esc2])
  case toktype of
       Esc1 -> case T.uncons t of
                    Just (c, _)
                      | c >= '\64' && c <= '\127' -> return (chr (ord c - 64))
                      | otherwise                 -> return (chr (ord c + 64))
                    Nothing -> fail "Empty content of Esc1"
       Esc2 -> case safeRead ('0':'x':T.unpack (T.drop 2 t)) of
                    Just x -> return (chr x)
                    Nothing -> fail $ "Could not read: " ++ T.unpack t
       _    -> fail "Expected an Esc1 or Esc2 token" -- should not happen

bgroup :: PandocMonad m => LP m ()
bgroup = try $ do
  skipMany sp
  symbol '{' <|> controlSeq "bgroup" <|> controlSeq "begingroup"
  return ()

egroup :: PandocMonad m => LP m ()
egroup = () <$ (symbol '}' <|> controlSeq "egroup" <|> controlSeq "endgroup")

grouped :: (PandocMonad m,  Monoid a) => LP m a -> LP m a
grouped parser = try $ do
  bgroup
  -- first we check for an inner 'grouped', because
  -- {{a,b}} should be parsed the same as {a,b}
  try (grouped parser <* egroup) <|> (mconcat <$> manyTill parser egroup)

braced :: PandocMonad m => LP m [Tok]
braced = try $ do
  bgroup
  -- {{a,b}} should be parsed the same as {a,b}
  try (braced <* egroup) <|> manyTill anyTok egroup

bracketed :: PandocMonad m => Monoid a => LP m a -> LP m a
bracketed parser = try $ do
  symbol '['
  mconcat <$> manyTill parser (symbol ']')

dimenarg :: PandocMonad m => LP m Text
dimenarg = try $ do
  ch  <- option False $ True <$ symbol '='
  Tok _ _ s <- satisfyTok isWordTok
  guard $ (T.take 2 (T.reverse s)) `elem`
           ["pt","pc","in","bp","cm","mm","dd","cc","sp"]
  let num = T.take (T.length s - 2) s
  guard $ T.length num > 0
  guard $ T.all isDigit num
  return $ T.pack ['=' | ch] <> s

-- inline elements:

inlineText :: PandocMonad m => LP m Inlines
inlineText = (str . T.unpack . untoken) <$> satisfyTok isWordTok

isWordTok :: Tok -> Bool
isWordTok (Tok _ Word _) = True
isWordTok _ = False

inlineGroup :: PandocMonad m => LP m Inlines
inlineGroup = do
  ils <- grouped inline
  if isNull ils
     then return mempty
     else return $ spanWith nullAttr ils
          -- we need the span so we can detitlecase bibtex entries;
          -- we need to know when something is {C}apitalized

doLHSverb :: PandocMonad m => LP m Inlines
doLHSverb =
  (codeWith ("",["haskell"],[]) . T.unpack . untokenize)
    <$> manyTill (satisfyTok (not . isNewlineTok)) (symbol '|')

lit :: String -> LP m Inlines
lit = pure . str

doubleQuote :: PandocMonad m => LP m Inlines
doubleQuote = do
       quoted' doubleQuoted (try $ count 2 $ symbol '`')
                            (void $ try $ count 2 $ symbol '\'')
   <|> quoted' doubleQuoted ((:[]) <$> symbol '“') (void $ symbol '”')
   -- the following is used by babel for localized quotes:
   <|> quoted' doubleQuoted (try $ sequence [symbol '"', symbol '`'])
                            (void $ try $ sequence [symbol '"', symbol '\''])
   <|> quoted' doubleQuoted ((:[]) <$> symbol '"')
                            (void $ symbol '"')

singleQuote :: PandocMonad m => LP m Inlines
singleQuote = do
       quoted' singleQuoted ((:[]) <$> symbol '`')
                            (try $ symbol '\'' >>
                                  notFollowedBy (satisfyTok startsWithLetter))
   <|> quoted' singleQuoted ((:[]) <$> symbol '‘')
                            (try $ symbol '’' >>
                                  notFollowedBy (satisfyTok startsWithLetter))
  where startsWithLetter (Tok _ Word t) =
          case T.uncons t of
               Just (c, _) | isLetter c -> True
               _ -> False
        startsWithLetter _ = False

quoted' :: PandocMonad m
        => (Inlines -> Inlines)
        -> LP m [Tok]
        -> LP m ()
        -> LP m Inlines
quoted' f starter ender = do
  startchs <- (T.unpack . untokenize) <$> starter
  smart <- extensionEnabled Ext_smart <$> getOption readerExtensions
  if smart
     then do
       ils <- many (notFollowedBy ender >> inline)
       (ender >> return (f (mconcat ils))) <|>
            (<> mconcat ils) <$>
                    lit (case startchs of
                              "``" -> "“"
                              "`"  -> "‘"
                              cs   -> cs)
     else lit startchs

enquote :: PandocMonad m => LP m Inlines
enquote = do
  skipopts
  context <- stateQuoteContext <$> getState
  if context == InDoubleQuote
     then singleQuoted <$> withQuoteContext InSingleQuote tok
     else doubleQuoted <$> withQuoteContext InDoubleQuote tok

doverb :: PandocMonad m => LP m Inlines
doverb = do
  Tok _ Symbol t <- anySymbol
  marker <- case T.uncons t of
              Just (c, ts) | T.null ts -> return c
              _ -> mzero
  (code . T.unpack . untokenize) <$> manyTill (verbTok marker) (symbol marker)

verbTok :: PandocMonad m => Char -> LP m Tok
verbTok stopchar = do
  t@(Tok (lin, col) toktype txt) <- satisfyTok (not . isNewlineTok)
  case T.findIndex (== stopchar) txt of
       Nothing -> return t
       Just i  -> do
         let (t1, t2) = T.splitAt i txt
         inp <- getInput
         setInput $ Tok (lin, col + i) Symbol (T.singleton stopchar)
                  : (totoks (lin, col + i + 1) (T.drop 1 t2)) ++ inp
         return $ Tok (lin, col) toktype t1

dolstinline :: PandocMonad m => LP m Inlines
dolstinline = do
  options <- option [] keyvals
  let classes = maybeToList $ lookup "language" options >>= fromListingsLanguage
  Tok _ Symbol t <- anySymbol
  marker <- case T.uncons t of
              Just (c, ts) | T.null ts -> return c
              _ -> mzero
  let stopchar = if marker == '{' then '}' else marker
  (codeWith ("",classes,[]) . T.unpack . untokenize) <$>
    manyTill (verbTok stopchar) (symbol stopchar)

keyval :: PandocMonad m => LP m (String, String)
keyval = try $ do
  Tok _ Word key <- satisfyTok isWordTok
  let isSpecSym (Tok _ Symbol t) = t `elem` [".",":","-","|","\\"]
      isSpecSym _ = False
  val <- option [] $ do
           symbol '='
           braced <|> (many1 (satisfyTok isWordTok <|> satisfyTok isSpecSym))
  optional sp
  optional (symbol ',')
  optional sp
  return (T.unpack key, T.unpack . untokenize $ val)

keyvals :: PandocMonad m => LP m [(String, String)]
keyvals = try $ symbol '[' >> manyTill keyval (symbol ']')

accent :: (Char -> String) -> Inlines -> LP m Inlines
accent f ils =
  case toList ils of
       (Str (x:xs) : ys) -> return $ fromList (Str (f x ++ xs) : ys)
       []                -> mzero
       _                 -> return ils

grave :: Char -> String
grave 'A' = "À"
grave 'E' = "È"
grave 'I' = "Ì"
grave 'O' = "Ò"
grave 'U' = "Ù"
grave 'a' = "à"
grave 'e' = "è"
grave 'i' = "ì"
grave 'o' = "ò"
grave 'u' = "ù"
grave c   = [c]

acute :: Char -> String
acute 'A' = "Á"
acute 'E' = "É"
acute 'I' = "Í"
acute 'O' = "Ó"
acute 'U' = "Ú"
acute 'Y' = "Ý"
acute 'a' = "á"
acute 'e' = "é"
acute 'i' = "í"
acute 'o' = "ó"
acute 'u' = "ú"
acute 'y' = "ý"
acute 'C' = "Ć"
acute 'c' = "ć"
acute 'L' = "Ĺ"
acute 'l' = "ĺ"
acute 'N' = "Ń"
acute 'n' = "ń"
acute 'R' = "Ŕ"
acute 'r' = "ŕ"
acute 'S' = "Ś"
acute 's' = "ś"
acute 'Z' = "Ź"
acute 'z' = "ź"
acute c   = [c]

circ :: Char -> String
circ 'A' = "Â"
circ 'E' = "Ê"
circ 'I' = "Î"
circ 'O' = "Ô"
circ 'U' = "Û"
circ 'a' = "â"
circ 'e' = "ê"
circ 'i' = "î"
circ 'o' = "ô"
circ 'u' = "û"
circ 'C' = "Ĉ"
circ 'c' = "ĉ"
circ 'G' = "Ĝ"
circ 'g' = "ĝ"
circ 'H' = "Ĥ"
circ 'h' = "ĥ"
circ 'J' = "Ĵ"
circ 'j' = "ĵ"
circ 'S' = "Ŝ"
circ 's' = "ŝ"
circ 'W' = "Ŵ"
circ 'w' = "ŵ"
circ 'Y' = "Ŷ"
circ 'y' = "ŷ"
circ c   = [c]

tilde :: Char -> String
tilde 'A' = "Ã"
tilde 'a' = "ã"
tilde 'O' = "Õ"
tilde 'o' = "õ"
tilde 'I' = "Ĩ"
tilde 'i' = "ĩ"
tilde 'U' = "Ũ"
tilde 'u' = "ũ"
tilde 'N' = "Ñ"
tilde 'n' = "ñ"
tilde c   = [c]

umlaut :: Char -> String
umlaut 'A' = "Ä"
umlaut 'E' = "Ë"
umlaut 'I' = "Ï"
umlaut 'O' = "Ö"
umlaut 'U' = "Ü"
umlaut 'a' = "ä"
umlaut 'e' = "ë"
umlaut 'i' = "ï"
umlaut 'o' = "ö"
umlaut 'u' = "ü"
umlaut c   = [c]

hungarumlaut :: Char -> String
hungarumlaut 'A' = "A̋"
hungarumlaut 'E' = "E̋"
hungarumlaut 'I' = "I̋"
hungarumlaut 'O' = "Ő"
hungarumlaut 'U' = "Ű"
hungarumlaut 'Y' = "ӳ"
hungarumlaut 'a' = "a̋"
hungarumlaut 'e' = "e̋"
hungarumlaut 'i' = "i̋"
hungarumlaut 'o' = "ő"
hungarumlaut 'u' = "ű"
hungarumlaut 'y' = "ӳ"
hungarumlaut c   = [c]

dot :: Char -> String
dot 'C' = "Ċ"
dot 'c' = "ċ"
dot 'E' = "Ė"
dot 'e' = "ė"
dot 'G' = "Ġ"
dot 'g' = "ġ"
dot 'I' = "İ"
dot 'Z' = "Ż"
dot 'z' = "ż"
dot c   = [c]

macron :: Char -> String
macron 'A' = "Ā"
macron 'E' = "Ē"
macron 'I' = "Ī"
macron 'O' = "Ō"
macron 'U' = "Ū"
macron 'a' = "ā"
macron 'e' = "ē"
macron 'i' = "ī"
macron 'o' = "ō"
macron 'u' = "ū"
macron c   = [c]

cedilla :: Char -> String
cedilla 'c' = "ç"
cedilla 'C' = "Ç"
cedilla 's' = "ş"
cedilla 'S' = "Ş"
cedilla 't' = "ţ"
cedilla 'T' = "Ţ"
cedilla 'e' = "ȩ"
cedilla 'E' = "Ȩ"
cedilla 'h' = "ḩ"
cedilla 'H' = "Ḩ"
cedilla 'o' = "o̧"
cedilla 'O' = "O̧"
cedilla c   = [c]

hacek :: Char -> String
hacek 'A' = "Ǎ"
hacek 'a' = "ǎ"
hacek 'C' = "Č"
hacek 'c' = "č"
hacek 'D' = "Ď"
hacek 'd' = "ď"
hacek 'E' = "Ě"
hacek 'e' = "ě"
hacek 'G' = "Ǧ"
hacek 'g' = "ǧ"
hacek 'H' = "Ȟ"
hacek 'h' = "ȟ"
hacek 'I' = "Ǐ"
hacek 'i' = "ǐ"
hacek 'j' = "ǰ"
hacek 'K' = "Ǩ"
hacek 'k' = "ǩ"
hacek 'L' = "Ľ"
hacek 'l' = "ľ"
hacek 'N' = "Ň"
hacek 'n' = "ň"
hacek 'O' = "Ǒ"
hacek 'o' = "ǒ"
hacek 'R' = "Ř"
hacek 'r' = "ř"
hacek 'S' = "Š"
hacek 's' = "š"
hacek 'T' = "Ť"
hacek 't' = "ť"
hacek 'U' = "Ǔ"
hacek 'u' = "ǔ"
hacek 'Z' = "Ž"
hacek 'z' = "ž"
hacek c   = [c]

breve :: Char -> String
breve 'A' = "Ă"
breve 'a' = "ă"
breve 'E' = "Ĕ"
breve 'e' = "ĕ"
breve 'G' = "Ğ"
breve 'g' = "ğ"
breve 'I' = "Ĭ"
breve 'i' = "ĭ"
breve 'O' = "Ŏ"
breve 'o' = "ŏ"
breve 'U' = "Ŭ"
breve 'u' = "ŭ"
breve c   = [c]

toksToString :: [Tok] -> String
toksToString = T.unpack . untokenize

mathDisplay :: String -> Inlines
mathDisplay = displayMath . trim
-- TODO we shouldn't need applyMacros
-- mathDisplay p = displayMath <$> (try p >>= applyMacros' . trim)

mathInline :: String -> Inlines
mathInline = math . trim
-- TODO we shouldn't need applyMacros
-- mathInline p = math <$> (try p >>= applyMacros')

dollarsMath :: PandocMonad m => LP m Inlines
dollarsMath = do
  symbol '$'
  display <- option False (True <$ symbol '$')
  contents <- many1Till anyTok (symbol '$')
  when display (() <$ symbol '$')
  let constructor = if display then mathDisplay else mathInline
  return $ constructor $ toksToString contents

inlineCommand' :: PandocMonad m => LP m Inlines
inlineCommand' = try $ do
  cs@(Tok _ CtrlSeq t) <- anyControlSeq
  let name = T.drop 1 t
  guard $ name /= "begin" && name /= "end"
  mbstar <- option Nothing (Just <$> symbol '*')
  let names = name : maybe [] (const [name <> "*"]) mbstar
  optional $ try $ symbol '{' >> symbol '}'
  let raw = do
       guard $ not (isBlockCommand name)
       (_, rawargs) <- withRaw
               (skipangles *> skipopts *> option "" dimenarg *> many braced)
       let rawcommand = T.unpack $ untokenize $
                             cs : maybe [] (:[]) mbstar ++ rawargs
       (guardEnabled Ext_raw_tex >> return (rawInline "latex" rawcommand))
         <|> ignore rawcommand
       -- transformed <- applyMacros' rawcommand -- TODO or get rid of
       -- exts <- getOption readerExtensions
       -- if transformed /= rawcommand
       --    then parseFromString' inlines transformed
       --    else if extensionEnabled Ext_raw_tex exts
       --            then return $ rawInline "latex" rawcommand
       --            else ignore rawcommand
  lookupListDefault raw names inlineCommands

tok :: PandocMonad m => LP m Inlines
tok = try $ grouped inline <|> inlineCommand' <|> singleChar
  where singleChar = do
          Tok (lin,col) toktype t <- satisfyTok (tokTypeIn [Word, Symbol])
          if T.length t > 1
             then do
               let (t1, t2) = (T.take 1 t, T.drop 1 t)
               inp <- getInput
               setInput $ (Tok (lin, col + 1) toktype t2) : inp
               return $ str (T.unpack t1)
             else return $ str (T.unpack t)

opt :: PandocMonad m => LP m Inlines
opt = bracketed inline

rawopt :: PandocMonad m => LP m Text
rawopt = do
  symbol '['
  inner <- untokenize <$> manyTill anyTok (symbol ']')
  optional sp
  return $ "[" <> inner <> "]"

skipopts :: PandocMonad m => LP m ()
skipopts = skipMany rawopt

-- eat an optional argument and one or more arguments in braces
ignoreInlines :: PandocMonad m => Text -> (Text, LP m Inlines)
ignoreInlines name = (name, p)
  where p = do oa <- optargs
               let rawCommand = '\\' : T.unpack (name <> oa)
               let doraw = guardRaw >> return (rawInline "latex" rawCommand)
               doraw <|> ignore rawCommand

guardRaw :: PandocMonad m => LP m ()
guardRaw = getOption readerExtensions >>= guard . extensionEnabled Ext_raw_tex

optargs :: PandocMonad m => LP m Text
optargs = do
  (_, raw) <- withRaw (skipopts *> skipMany (try $ optional sp *> braced))
  return $ untokenize raw

-- opts in angle brackets are used in beamer
rawangle :: PandocMonad m => LP m ()
rawangle = try $ do
  symbol '<'
  () <$ manyTill anyTok (symbol '>')

skipangles :: PandocMonad m => LP m ()
skipangles = skipMany rawangle

ignore :: (Monoid a, PandocMonad m) => String -> ParserT s u m a
ignore raw = do
  pos <- getPosition
  report $ SkippedContent raw pos
  return mempty

withRaw :: PandocMonad m => LP m a -> LP m (a, [Tok])
withRaw parser = do
  inp <- getInput
  result <- parser
  nxt <- option (Tok (0,0) Word "") (lookAhead anyTok)
  let raw = takeWhile (/= nxt) inp
  return (result, raw)

inBrackets :: Inlines -> Inlines
inBrackets x = str "[" <> x <> str "]"

unescapeURL :: String -> String
unescapeURL ('\\':x:xs) | isEscapable x = x:unescapeURL xs
  where isEscapable c = c `elem` ("#$%&~_^\\{}" :: String)
unescapeURL (x:xs) = x:unescapeURL xs
unescapeURL [] = ""

mathEnvWith :: PandocMonad m
            => (Inlines -> a) -> Maybe String -> String -> LP m a
mathEnvWith f innerEnv name = f . mathDisplay . inner <$> mathEnv name
   where inner x = case innerEnv of
                      Nothing -> x
                      Just y  -> "\\begin{" ++ y ++ "}\n" ++ x ++
                                    "\\end{" ++ y ++ "}"

mathEnv :: PandocMonad m => String -> LP m String
mathEnv name = do
  skipopts
  optional blankline
  let endEnv = try $ controlSeq "end" *>
                     (T.unpack . untokenize <$> braced) >>= guard . (== name)
  res <- manyTill anyTok endEnv
  return $ stripTrailingNewlines $ T.unpack $ untokenize res

inlineEnvironment :: PandocMonad m => LP m Inlines
inlineEnvironment = try $ do
  controlSeq "begin"
  name <- untokenize <$> braced
  M.findWithDefault mzero name inlineEnvironments

inlineEnvironments :: PandocMonad m => M.Map Text (LP m Inlines)
inlineEnvironments = M.fromList [
    ("displaymath", mathEnvWith id Nothing "displaymath")
  , ("math", math <$> mathEnv "math")
  , ("equation", mathEnvWith id Nothing "equation")
  , ("equation*", mathEnvWith id Nothing "equation*")
  , ("gather", mathEnvWith id (Just "gathered") "gather")
  , ("gather*", mathEnvWith id (Just "gathered") "gather*")
  , ("multline", mathEnvWith id (Just "gathered") "multline")
  , ("multline*", mathEnvWith id (Just "gathered") "multline*")
  , ("eqnarray", mathEnvWith id (Just "aligned") "eqnarray")
  , ("eqnarray*", mathEnvWith id (Just "aligned") "eqnarray*")
  , ("align", mathEnvWith id (Just "aligned") "align")
  , ("align*", mathEnvWith id (Just "aligned") "align*")
  , ("alignat", mathEnvWith id (Just "aligned") "alignat")
  , ("alignat*", mathEnvWith id (Just "aligned") "alignat*")
  ]

inlineCommands :: PandocMonad m => M.Map Text (LP m Inlines)
inlineCommands = M.fromList $
  [ ("emph", extractSpaces emph <$> tok)
  , ("textit", extractSpaces emph <$> tok)
  , ("textsl", extractSpaces emph <$> tok)
  , ("textsc", extractSpaces smallcaps <$> tok)
  , ("textsf", extractSpaces (spanWith ("",["sans-serif"],[])) <$> tok)
  , ("textmd", extractSpaces (spanWith ("",["medium"],[])) <$> tok)
  , ("textrm", extractSpaces (spanWith ("",["roman"],[])) <$> tok)
  , ("textup", extractSpaces (spanWith ("",["upright"],[])) <$> tok)
  , ("texttt", ttfamily)
  , ("sout", extractSpaces strikeout <$> tok)
  , ("textsuperscript", extractSpaces superscript <$> tok)
  , ("textsubscript", extractSpaces subscript <$> tok)
  , ("textbackslash", lit "\\")
  , ("backslash", lit "\\")
  , ("slash", lit "/")
  , ("textbf", extractSpaces strong <$> tok)
  , ("textnormal", extractSpaces (spanWith ("",["nodecor"],[])) <$> tok)
  , ("ldots", lit "…")
  , ("vdots", lit "\8942")
  , ("dots", lit "…")
  , ("mdots", lit "…")
  , ("sim", lit "~")
  , ("label", rawInlineOr "label" (inBrackets <$> tok))
  , ("ref", rawInlineOr "ref" (inBrackets <$> tok))
  , ("textgreek", tok)
  , ("sep", lit ",")
  , ("cref", rawInlineOr "cref" (inBrackets <$> tok))  -- from cleveref.sty
  , ("(", mathInline . toksToString <$> manyTill anyTok (controlSeq ")"))
  , ("[", mathDisplay . toksToString <$> manyTill anyTok (controlSeq "]"))
  , ("ensuremath", mathInline . toksToString <$> braced)
  , ("texorpdfstring", (\_ x -> x) <$> tok <*> tok)
  , ("P", lit "¶")
  , ("S", lit "§")
  , ("$", lit "$")
  , ("%", lit "%")
  , ("&", lit "&")
  , ("#", lit "#")
  , ("_", lit "_")
  , ("{", lit "{")
  , ("}", lit "}")
  -- old TeX commands
  , ("em", extractSpaces emph <$> inlines)
  , ("it", extractSpaces emph <$> inlines)
  , ("sl", extractSpaces emph <$> inlines)
  , ("bf", extractSpaces strong <$> inlines)
  , ("rm", inlines)
  , ("itshape", extractSpaces emph <$> inlines)
  , ("slshape", extractSpaces emph <$> inlines)
  , ("scshape", extractSpaces smallcaps <$> inlines)
  , ("bfseries", extractSpaces strong <$> inlines)
  , ("/", pure mempty) -- italic correction
  , ("aa", lit "å")
  , ("AA", lit "Å")
  , ("ss", lit "ß")
  , ("o", lit "ø")
  , ("O", lit "Ø")
  , ("L", lit "Ł")
  , ("l", lit "ł")
  , ("ae", lit "æ")
  , ("AE", lit "Æ")
  , ("oe", lit "œ")
  , ("OE", lit "Œ")
  , ("pounds", lit "£")
  , ("euro", lit "€")
  , ("copyright", lit "©")
  , ("textasciicircum", lit "^")
  , ("textasciitilde", lit "~")
  , ("H", try $ tok >>= accent hungarumlaut)
  , ("`", option (str "`") $ try $ tok >>= accent grave)
  , ("'", option (str "'") $ try $ tok >>= accent acute)
  , ("^", option (str "^") $ try $ tok >>= accent circ)
  , ("~", option (str "~") $ try $ tok >>= accent tilde)
  , ("\"", option (str "\"") $ try $ tok >>= accent umlaut)
  , (".", option (str ".") $ try $ tok >>= accent dot)
  , ("=", option (str "=") $ try $ tok >>= accent macron)
  , ("c", option (str "c") $ try $ tok >>= accent cedilla)
  , ("v", option (str "v") $ try $ tok >>= accent hacek)
  , ("u", option (str "u") $ try $ tok >>= accent breve)
  , ("i", lit "i")
  , ("\\", linebreak <$ (optional (bracketed inline) *> spaces))
  , (",", lit "\8198")
  , ("@", pure mempty)
  , (" ", lit "\160")
  , ("ps", pure $ str "PS." <> space)
  , ("TeX", lit "TeX")
  , ("LaTeX", lit "LaTeX")
  , ("bar", lit "|")
  , ("textless", lit "<")
  , ("textgreater", lit ">")
  , ("thanks", note <$> grouped block)
  , ("footnote", note <$> grouped block)
  , ("verb", doverb)
  , ("lstinline", dolstinline)
  , ("Verb", doverb)
  , ("url", ((unescapeURL . T.unpack . untokenize) <$> braced) >>= \url ->
                  pure (link url "" (str url)))
--  , ("href", (unescapeURL <$> braced <* optional sp) >>= \url ->
--       tok >>= \lab ->
--         pure (link url "" lab))
--  , ("includegraphics", do options <- option [] keyvals
--                           src <- unescapeURL . removeDoubleQuotes <$> braced
--                           mkImage options src)
  , ("enquote", enquote)
--  , ("cite", citation "cite" NormalCitation False)
--  , ("Cite", citation "Cite" NormalCitation False)
--  , ("citep", citation "citep" NormalCitation False)
--  , ("citep*", citation "citep*" NormalCitation False)
--  , ("citeal", citation "citeal" NormalCitation False)
--  , ("citealp", citation "citealp" NormalCitation False)
--  , ("citealp*", citation "citealp*" NormalCitation False)
--  , ("autocite", citation "autocite" NormalCitation False)
--  , ("smartcite", citation "smartcite" NormalCitation False)
--  , ("footcite", inNote <$> citation "footcite" NormalCitation False)
--  , ("parencite", citation "parencite" NormalCitation False)
--  , ("supercite", citation "supercite" NormalCitation False)
--  , ("footcitetext", inNote <$> citation "footcitetext" NormalCitation False)
--  , ("citeyearpar", citation "citeyearpar" SuppressAuthor False)
--  , ("citeyear", citation "citeyear" SuppressAuthor False)
--  , ("autocite*", citation "autocite*" SuppressAuthor False)
--  , ("cite*", citation "cite*" SuppressAuthor False)
--  , ("parencite*", citation "parencite*" SuppressAuthor False)
--  , ("textcite", citation "textcite" AuthorInText False)
--  , ("citet", citation "citet" AuthorInText False)
--  , ("citet*", citation "citet*" AuthorInText False)
--  , ("citealt", citation "citealt" AuthorInText False)
--  , ("citealt*", citation "citealt*" AuthorInText False)
--  , ("textcites", citation "textcites" AuthorInText True)
--  , ("cites", citation "cites" NormalCitation True)
--  , ("autocites", citation "autocites" NormalCitation True)
--  , ("footcites", inNote <$> citation "footcites" NormalCitation True)
--  , ("parencites", citation "parencites" NormalCitation True)
--  , ("supercites", citation "supercites" NormalCitation True)
--  , ("footcitetexts", inNote <$> citation "footcitetexts" NormalCitation True)
--  , ("Autocite", citation "Autocite" NormalCitation False)
--  , ("Smartcite", citation "Smartcite" NormalCitation False)
--  , ("Footcite", citation "Footcite" NormalCitation False)
--  , ("Parencite", citation "Parencite" NormalCitation False)
--  , ("Supercite", citation "Supercite" NormalCitation False)
--  , ("Footcitetext", inNote <$> citation "Footcitetext" NormalCitation False)
--  , ("Citeyearpar", citation "Citeyearpar" SuppressAuthor False)
--  , ("Citeyear", citation "Citeyear" SuppressAuthor False)
--  , ("Autocite*", citation "Autocite*" SuppressAuthor False)
--  , ("Cite*", citation "Cite*" SuppressAuthor False)
--  , ("Parencite*", citation "Parencite*" SuppressAuthor False)
--  , ("Textcite", citation "Textcite" AuthorInText False)
--  , ("Textcites", citation "Textcites" AuthorInText True)
--  , ("Cites", citation "Cites" NormalCitation True)
--  , ("Autocites", citation "Autocites" NormalCitation True)
--  , ("Footcites", citation "Footcites" NormalCitation True)
--  , ("Parencites", citation "Parencites" NormalCitation True)
--  , ("Supercites", citation "Supercites" NormalCitation True)
--  , ("Footcitetexts", inNote <$> citation "Footcitetexts" NormalCitation True)
--  , ("citetext", complexNatbibCitation NormalCitation)
--  , ("citeauthor", (try (tok *> optional sp *> controlSeq "citetext") *>
--                        complexNatbibCitation AuthorInText)
--                   <|> citation "citeauthor" AuthorInText False)
--  , ("nocite", mempty <$ (citation "nocite" NormalCitation False >>=
--                          addMeta "nocite"))
  , ("hypertarget", braced >> tok)
--  -- siuntix
--  , ("SI", dosiunitx)
  -- hyphenat
  , ("bshyp", lit "\\\173")
  , ("fshyp", lit "/\173")
  , ("dothyp", lit ".\173")
  , ("colonhyp", lit ":\173")
  , ("hyp", lit "-")
  , ("nohyphens", tok)
  , ("textnhtt", ttfamily)
  , ("nhttfamily", ttfamily)
  -- fontawesome
  , ("faCheck", lit "\10003")
  , ("faClose", lit "\10007")
  ] ++ map ignoreInlines
  -- these commands will be ignored unless --parse-raw is specified,
  -- in which case they will appear as raw latex blocks:
  [ "index"
  , "hspace"
  , "vspace"
  , "newpage"
  , "clearpage"
  , "pagebreak"
  ]

ttfamily :: PandocMonad m => LP m Inlines
ttfamily = (code . stringify . toList) <$> tok

rawInlineOr :: PandocMonad m => Text -> LP m Inlines -> LP m Inlines
rawInlineOr name' fallback = do
  parseRaw <- extensionEnabled Ext_raw_tex <$> getOption readerExtensions
  if parseRaw
     then rawInline "latex" <$> getRawCommand name'
     else fallback

getRawCommand :: PandocMonad m => Text -> LP m String
getRawCommand name' = do
  (_, rawargs) <- withRaw
     (many (try (optional sp *> opt)) *>
      option "" (try (optional sp *> dimenarg)) *>
      many braced)
  return $ '\\' : T.unpack (name' <> untokenize rawargs)

isBlockCommand :: Text -> Bool
isBlockCommand s =
  s `M.member` (blockCommands :: M.Map Text (LP PandocPure Blocks))

lookupListDefault :: (Ord k) => v -> [k] -> M.Map k v -> v
lookupListDefault d = (fromMaybe d .) . lookupList
  where
  lookupList l m = msum $ map (`M.lookup` m) l

inline :: PandocMonad m => LP m Inlines
inline = (mempty <$ comment)
     <|> (space  <$ whitespace)
     <|> (softbreak <$ endline)
     <|> inlineText
     <|> inlineCommand'
     <|> inlineEnvironment
     <|> inlineGroup
     <|> (symbol '-' *>
           option (str "-") (symbol '-' *>
             option (str "–") (str "—" <$ symbol '-')))
     <|> doubleQuote
     <|> singleQuote
     <|> (str "”" <$ try (symbol '\'' >> symbol '\''))
     <|> (str "”" <$ symbol '”')
     <|> (str "’" <$ symbol '\'')
     <|> (str "’" <$ symbol '’')
     <|> (str "\160" <$ symbol '~')
     <|> dollarsMath
     <|> (guardEnabled Ext_literate_haskell *> symbol '|' *> doLHSverb)
     <|> (str . (:[]) <$> primEscape)
     <|> (do res <- symbolIn "#&~^'`\"[]"
             pos <- getPosition
             let s = T.unpack (untoken res)
             report $ ParsingUnescaped s pos
             return $ str s)

inlines :: PandocMonad m => LP m Inlines
inlines = mconcat <$> many inline

-- block elements:

paragraph :: PandocMonad m => LP m Blocks
paragraph = do
  x <- trimInlines . mconcat <$> many1 inline
  if x == mempty
     then return mempty
     else return $ para x

include :: PandocMonad m => LP m Blocks
include = do
  (Tok _ _ name) <- controlSeq "include" <|> controlSeq "input" <|>
                    controlSeq "subfile" <|> controlSeq "usepackage"
  skipMany $ bracketed inline -- skip options
  fs <- (map trim . splitBy (==',') . T.unpack . untokenize) <$> braced
  let fs' = if name == "\\usepackage"
               then map (maybeAddExtension ".sty") fs
               else map (maybeAddExtension ".tex") fs
  dirs <- (splitBy (==':') . fromMaybe ".") <$> lookupEnv "TEXINPUTS"
  mconcat <$> mapM (insertIncludedFile blocks (tokenize . T.pack) dirs) fs'

maybeAddExtension :: String -> FilePath -> FilePath
maybeAddExtension ext fp =
  if null (takeExtension fp)
     then addExtension fp ext
     else fp

ignoreBlocks :: PandocMonad m => Text -> (Text, LP m Blocks)
ignoreBlocks name = (name, p)
  where
    p = do oa <- optargs
           let rawCommand = '\\' : T.unpack (name <> oa)
           let doraw = guardRaw >> return (rawBlock "latex" rawCommand)
           doraw <|> ignore rawCommand

addMeta :: PandocMonad m => ToMetaValue a => String -> a -> LP m ()
addMeta field val = updateState $ \st ->
   st{ stateMeta = addMetaField field val $ stateMeta st }

authors :: PandocMonad m => LP m ()
authors = try $ do
  bgroup
  let oneAuthor = mconcat <$>
       many1 (notFollowedBy' (controlSeq "and") >>
               (inline <|> mempty <$ blockCommand))
               -- skip e.g. \vspace{10pt}
  auths <- sepBy oneAuthor (controlSeq "and")
  egroup
  addMeta "author" (map trimInlines auths)

section :: PandocMonad m => Attr -> Int -> LP m Blocks
section (ident, classes, kvs) lvl = do
  skipopts
  contents <- grouped inline
  lab <- option ident $
          try (spaces >> controlSeq "label"
               >> spaces >> toksToString <$> braced)
  attr' <- registerHeader (lab, classes, kvs) contents
  return $ headerWith attr' lvl contents

blockCommand :: PandocMonad m => LP m Blocks
blockCommand = try $ do
  Tok _ CtrlSeq name <- anyControlSeq
  guard $ name /= "\\begin" && name /= "\\end"
  star <- option "" ("*" <$ symbol '*' <* optional sp)
  let name' = name <> star
  let names = name : [name' | name' /= name]
  let raw = rawBlock "latex" <$> getRawCommand name'
        -- TODO do we still need this?
        -- rawcommand <- getRawCommand name'
        -- transformed <- applyMacros' rawcommand
        -- guard $ transformed /= rawcommand
        -- notFollowedBy $ parseFromString' inlines transformed
        -- parseFromString' blocks transformed
  lookupListDefault raw names blockCommands

closing :: PandocMonad m => LP m Blocks
closing = do
  contents <- tok
  st <- getState
  let extractInlines (MetaBlocks [Plain ys]) = ys
      extractInlines (MetaBlocks [Para ys ]) = ys
      extractInlines _                       = []
  let sigs = case lookupMeta "author" (stateMeta st) of
                  Just (MetaList xs) ->
                    para $ trimInlines $ fromList $
                      intercalate [LineBreak] $ map extractInlines xs
                  _ -> mempty
  return $ para (trimInlines contents) <> sigs

blockCommands :: PandocMonad m => M.Map Text (LP m Blocks)
blockCommands = M.fromList $
   [ ("par", mempty <$ skipopts)
   , ("parbox",  braced >> grouped blocks)
   , ("title", mempty <$ (skipopts *>
                           (grouped inline >>= addMeta "title")
                       <|> (grouped block >>= addMeta "title")))
   , ("subtitle", mempty <$ (skipopts *> tok >>= addMeta "subtitle"))
   , ("author", mempty <$ (skipopts *> authors))
   -- -- in letter class, temp. store address & sig as title, author
   , ("address", mempty <$ (skipopts *> tok >>= addMeta "address"))
   , ("signature", mempty <$ (skipopts *> authors))
   , ("date", mempty <$ (skipopts *> tok >>= addMeta "date"))
   -- Koma-script metadata commands
   , ("dedication", mempty <$ (skipopts *> tok >>= addMeta "dedication"))
   -- sectioning
   , ("part", section nullAttr (-1))
   , ("part*", section nullAttr (-1))
   , ("chapter", section nullAttr 0)
   , ("chapter*", section ("",["unnumbered"],[]) 0)
   , ("section", section nullAttr 1)
   , ("section*", section ("",["unnumbered"],[]) 1)
   , ("subsection", section nullAttr 2)
   , ("subsection*", section ("",["unnumbered"],[]) 2)
   , ("subsubsection", section nullAttr 3)
   , ("subsubsection*", section ("",["unnumbered"],[]) 3)
   , ("paragraph", section nullAttr 4)
   , ("paragraph*", section ("",["unnumbered"],[]) 4)
   , ("subparagraph", section nullAttr 5)
   , ("subparagraph*", section ("",["unnumbered"],[]) 5)
   -- beamer slides
   , ("frametitle", section nullAttr 3)
   , ("framesubtitle", section nullAttr 4)
   -- letters
   , ("opening", (para . trimInlines) <$> (skipopts *> tok))
   , ("closing", skipopts *> closing)
   --
   , ("hrule", pure horizontalRule)
   , ("strut", pure mempty)
   , ("rule", skipopts *> tok *> tok *> pure horizontalRule)
--   , ("item", skipopts *> looseItem)
--   , ("documentclass", skipopts *> braced *> preamble)
   , ("centerline", (para . trimInlines) <$> (skipopts *> tok))
--   , ("caption", skipopts *> setCaption)
--   , ("bibliography", mempty <$ (skipopts *> braced >>=
--                                 addMeta "bibliography" . splitBibs))
--   , ("addbibresource", mempty <$ (skipopts *> braced >>=
--                                 addMeta "bibliography" . splitBibs))
--   -- includes
--   , ("lstinputlisting", inputListing)
--   , ("graphicspath", graphicsPath)
--   -- hyperlink
   , ("hypertarget", braced >> grouped block)
   ] ++ map ignoreBlocks
   -- these commands will be ignored unless --parse-raw is specified,
   -- in which case they will appear as raw latex blocks
   [ "newcommand", "renewcommand", "newenvironment", "renewenvironment"
     -- newcommand, etc. should be parsed by macro, but we need this
     -- here so these aren't parsed as inline commands to ignore
   , "special", "pdfannot", "pdfstringdef"
   , "bibliographystyle"
   , "maketitle", "makeindex", "makeglossary"
   , "addcontentsline", "addtocontents", "addtocounter"
      -- \ignore{} is used conventionally in literate haskell for definitions
      -- that are to be processed by the compiler but not printed.
   , "ignore"
   , "hyperdef"
   , "markboth", "markright", "markleft"
   , "hspace", "vspace"
   , "newpage"
   , "clearpage"
   , "pagebreak"
   ]

environments :: PandocMonad m => M.Map Text (LP m Blocks)
environments = M.fromList
   [ ("document", env "document" blocks)
--   , ("abstract", mempty <$ (env "abstract" blocks >>= addMeta "abstract"))
--   , ("letter", env "letter" letterContents)
--   , ("minipage", env "minipage" $
--          skipopts *> spaces' *> optional braced *> spaces' *> blocks)
--   , ("figure", env "figure" $ skipopts *> figure)
--   , ("subfigure", env "subfigure" $ skipopts *> tok *> figure)
--   , ("center", env "center" blocks)
--   , ("longtable",  env "longtable" $
--          resetCaption *> simpTable "longtable" False >>= addTableCaption)
--   , ("table",  env "table" $
--          resetCaption *> skipopts *> blocks >>= addTableCaption)
--   , ("tabular*", env "tabular" $ simpTable "tabular*" True)
--   , ("tabularx", env "tabularx" $ simpTable "tabularx" True)
--   , ("tabular", env "tabular"  $ simpTable "tabular" False)
--   , ("quote", blockQuote <$> env "quote" blocks)
--   , ("quotation", blockQuote <$> env "quotation" blocks)
--   , ("verse", blockQuote <$> env "verse" blocks)
--   , ("itemize", bulletList <$> listenv "itemize" (many item))
--   , ("description", definitionList <$> listenv "description" (many descItem))
--   , ("enumerate", orderedList')
--   , ("alltt", alltt =<< verbEnv "alltt")
--   , ("code", guardEnabled Ext_literate_haskell *>
--       (codeBlockWith ("",["sourceCode","literate","haskell"],[]) <$>
--         verbEnv "code"))
--   , ("comment", mempty <$ verbEnv "comment")
--   , ("verbatim", codeBlock <$> verbEnv "verbatim")
--   , ("Verbatim", fancyverbEnv "Verbatim")
--   , ("BVerbatim", fancyverbEnv "BVerbatim")
--   , ("lstlisting", do attr <- parseListingsOptions <$> option [] keyvals
--                       codeBlockWith attr <$> verbEnv "lstlisting")
--   , ("minted",     do options <- option [] keyvals
--                       lang <- grouped (many1 $ satisfy (/='}'))
--                       let kvs = [ (if k == "firstnumber"
--                                       then "startFrom"
--                                       else k, v) | (k,v) <- options ]
--                       let classes = [ lang | not (null lang) ] ++
--                                     [ "numberLines" |
--                                       lookup "linenos" options == Just "true" ]
--                       let attr = ("",classes,kvs)
--                       codeBlockWith attr <$> verbEnv "minted")
--   , ("obeylines", parseFromString
--                   (para . trimInlines . mconcat <$> many inline) =<<
--                   intercalate "\\\\\n" . lines <$> verbEnv "obeylines")
--   , ("displaymath", mathEnvWith para Nothing "displaymath")
--   , ("equation", mathEnvWith para Nothing "equation")
--   , ("equation*", mathEnvWith para Nothing "equation*")
--   , ("gather", mathEnvWith para (Just "gathered") "gather")
--   , ("gather*", mathEnvWith para (Just "gathered") "gather*")
--   , ("multline", mathEnvWith para (Just "gathered") "multline")
--   , ("multline*", mathEnvWith para (Just "gathered") "multline*")
--   , ("eqnarray", mathEnvWith para (Just "aligned") "eqnarray")
--   , ("eqnarray*", mathEnvWith para (Just "aligned") "eqnarray*")
--   , ("align", mathEnvWith para (Just "aligned") "align")
--   , ("align*", mathEnvWith para (Just "aligned") "align*")
--   , ("alignat", mathEnvWith para (Just "aligned") "alignat")
--   , ("alignat*", mathEnvWith para (Just "aligned") "alignat*")
--   , ("tikzpicture", rawVerbEnv "tikzpicture")
   ]

environment :: PandocMonad m => LP m Blocks
environment = do
  controlSeq "begin"
  name <- untokenize <$> braced
  M.findWithDefault mzero name environments
    <|> rawEnv name

env :: PandocMonad m => Text -> LP m a -> LP m a
env name p = p <*
  (try (controlSeq "end" *> braced >>= guard . (== name) . untokenize)
    <?> ("\\end{" ++ T.unpack name ++ "}"))

rawEnv :: PandocMonad m => Text -> LP m Blocks
rawEnv name = do
  exts <- getOption readerExtensions
  let parseRaw = extensionEnabled Ext_raw_tex exts
  rawOptions <- mconcat <$> many rawopt
  let beginCommand = "\\begin{" <> name <> "}" <> rawOptions
  pos1 <- getPosition
  (bs, raw) <- withRaw $ env name blocks
  if parseRaw
     then return $ rawBlock "latex"
                 $ T.unpack $ beginCommand <> untokenize raw
     else do
       unless parseRaw $ do
         report $ SkippedContent (T.unpack beginCommand) pos1
       pos2 <- getPosition
       report $ SkippedContent ("\\end{" ++ T.unpack name ++ "}") pos2
       return bs

  -- raw' <- applyMacros' $ beginCommand <> untokenize raw
  -- if raw' /= beginCommand ++ raw
  --    then parseFromString' blocks raw'
  --    else if parseRaw
  --         then return $ rawBlock "latex" $ beginCommand ++ raw'
  --         else do
  --           unless parseRaw $ do
  --             report $ SkippedContent beginCommand pos1
  --           pos2 <- getPosition
  --           report $ SkippedContent ("\\end{" ++ name ++ "}") pos2
  --           return bs

block :: PandocMonad m => LP m Blocks
block = (mempty <$ spaces1)
    <|> environment
    <|> include
    -- <|> macro
    <|> blockCommand
    <|> paragraph
    <|> grouped block

blocks :: PandocMonad m => LP m Blocks
blocks = mconcat <$> many block

{-
-- graphicsPath :: PandocMonad m => LP m Blocks
-- graphicsPath = do
--   ps <- bgroup *> (manyTill braced egroup)
--   getResourcePath >>= setResourcePath . (++ ps)
--   return mempty
-- 
-- addMeta :: PandocMonad m => ToMetaValue a => String -> a -> LP m ()
-- addMeta field val = updateState $ \st ->
--   st{ stateMeta = addMetaField field val $ stateMeta st }
-- 
-- splitBibs :: String -> [Inlines]
-- splitBibs = map (str . flip replaceExtension "bib" . trim) . splitBy (==',')
-- 
-- setCaption :: PandocMonad m => LP m Blocks
-- setCaption = do
--   ils <- tok
--   mblabel <- option Nothing $
--                try $ spaces' >> controlSeq "label" >> (Just <$> tok)
--   let ils' = case mblabel of
--                   Just lab -> ils <> spanWith
--                                 ("",[],[("data-label", stringify lab)]) mempty
--                   Nothing  -> ils
--   updateState $ \st -> st{ stateCaption = Just ils' }
--   return mempty
-- 
-- resetCaption :: PandocMonad m => LP m ()
-- resetCaption = updateState $ \st -> st{ stateCaption = Nothing }
-- 
-- mkImage :: PandocMonad m => [(String, String)] -> String -> LP m Inlines
-- mkImage options src = do
--    let replaceTextwidth (k,v) = case numUnit v of
--                                   Just (num, "\\textwidth") -> (k, showFl (num * 100) ++ "%")
--                                   _ -> (k, v)
--    let kvs = map replaceTextwidth $ filter (\(k,_) -> k `elem` ["width", "height"]) options
--    let attr = ("",[], kvs)
--    let alt = str "image"
--    case takeExtension src of
--         "" -> do
--               defaultExt <- getOption readerDefaultImageExtension
--               return $ imageWith attr (addExtension src defaultExt) "" alt
--         _  -> return $ imageWith attr src "" alt
-- 
-- inNote :: Inlines -> Inlines
-- inNote ils =
--   note $ para $ ils <> str "."
-- 
- 
-- -- converts e.g. \SI{1}[\$]{} to "$ 1" or \SI{1}{\euro} to "1 €"
-- dosiunitx :: PandocMonad m => LP m Inlines
-- dosiunitx = do
--   skipopts
--   value <- tok
--   valueprefix <- option "" $ char '[' >> (mconcat <$> manyTill tok (char ']'))
--   unit <- tok
--   let emptyOr160 "" = ""
--       emptyOr160 _  = "\160"
--   return . mconcat $ [valueprefix, 
--                       emptyOr160 valueprefix,
--                       value, 
--                       emptyOr160 unit,
--                       unit]
-- 
-- skipangles :: PandocMonad m => LP m ()
-- skipangles = skipMany rawangle
-- 
-- inlineText :: PandocMonad m => LP m Inlines
-- inlineText = str <$> many1 inlineChar
-- 
-- inlineChar :: PandocMonad m => LP m Char
-- inlineChar = noneOf "\\$%&~#{}^'`\"‘’“”-[] \t\n"
-- 
-- 
-- inlineEnvironment :: PandocMonad m => LP m Inlines
-- inlineEnvironment = try $ do
--   controlSeq "begin"
--   name <- braced
--   M.findWithDefault mzero name inlineEnvironments
-- 
-- 
-- rawVerbEnv :: PandocMonad m => String -> LP m Blocks
-- rawVerbEnv name = do
--   pos <- getPosition
--   (_, raw) <- withRaw $ verbEnv name
--   let raw' = "\\begin{tikzpicture}" ++ raw
--   exts <- getOption readerExtensions
--   let parseRaw = extensionEnabled Ext_raw_tex exts
--   if parseRaw
--      then return $ rawBlock "latex" raw'
--      else do
--        report $ SkippedContent raw' pos
--        return mempty
-- 
-- ----
-- 
-- alltt :: PandocMonad m => String -> LP m Blocks
-- alltt t = walk strToCode <$> parseFromString' blocks
--   (substitute " " "\\ " $ substitute "%" "\\%" $
--    intercalate "\\\\\n" $ lines t)
--   where strToCode (Str s) = Code nullAttr s
--         strToCode x       = x
-- 
-- rawLaTeXBlock :: PandocMonad m => ParserT String ParserState String
-- rawLaTeXBlock = snd <$> try (withRaw (environment <|> blockCommand))
-- 
-- rawLaTeXInline :: PandocMonad m => ParserT String ParserState Inline
-- rawLaTeXInline = do
--   raw <- (snd <$> withRaw inlineCommand)
--      <|> (snd <$> withRaw inlineEnvironment)
--      <|> (snd <$> withRaw blockCommand)
--   RawInline "latex" <$> applyMacros' raw
-- 
-- addImageCaption :: PandocMonad m => Blocks -> LP m Blocks
-- addImageCaption = walkM go
--   where go (Image attr alt (src,tit))
--             | not ("fig:" `isPrefixOf` tit) = do
--           mbcapt <- stateCaption <$> getState
--           return $ case mbcapt of
--                Just ils -> Image attr (toList ils) (src, "fig:" ++ tit)
--                Nothing  -> Image attr alt (src,tit)
--         go x = return x
-- 
-- addTableCaption :: PandocMonad m => Blocks -> LP m Blocks
-- addTableCaption = walkM go
--   where go (Table c als ws hs rs) = do
--           mbcapt <- stateCaption <$> getState
--           return $ case mbcapt of
--                Just ils -> Table (toList ils) als ws hs rs
--                Nothing  -> Table c als ws hs rs
--         go x = return x
-- 
-- 
-- figure :: PandocMonad m => LP m Blocks
-- figure = try $ do
--   resetCaption
--   blocks >>= addImageCaption
-- 
-- letterContents :: PandocMonad m => LP m Blocks
-- letterContents = do
--   bs <- blocks
--   st <- getState
--   -- add signature (author) and address (title)
--   let addr = case lookupMeta "address" (stateMeta st) of
--                   Just (MetaBlocks [Plain xs]) ->
--                      para $ trimInlines $ fromList xs
--                   _ -> mempty
--   return $ addr <> bs -- sig added by \closing
-- 
-- 
-- item :: PandocMonad m => LP m Blocks
-- item = blocks *> controlSeq "item" *> skipopts *> blocks
-- 
-- looseItem :: PandocMonad m => LP m Blocks
-- looseItem = do
--   ctx <- stateParserContext `fmap` getState
--   if ctx == ListItemState
--      then mzero
--      else return mempty
-- 
-- descItem :: PandocMonad m => LP m (Inlines, [Blocks])
-- descItem = do
--   blocks -- skip blocks before item
--   controlSeq "item"
--   optional sp
--   ils <- opt
--   bs <- blocks
--   return (ils, [bs])
-- 
-- 
-- listenv :: PandocMonad m => String -> LP m a -> LP m a
-- listenv name p = try $ do
--   oldCtx <- stateParserContext `fmap` getState
--   updateState $ \st -> st{ stateParserContext = ListItemState }
--   res <- env name p
--   updateState $ \st -> st{ stateParserContext = oldCtx }
--   return res
-- 
-- mathEnvWith :: PandocMonad m
--             => (Inlines -> a) -> Maybe String -> String -> LP m a
-- mathEnvWith f innerEnv name = f <$> mathDisplay (inner <$> mathEnv name)
--    where inner x = case innerEnv of
--                       Nothing -> x
--                       Just y  -> "\\begin{" ++ y ++ "}\n" ++ x ++
--                                     "\\end{" ++ y ++ "}"
-- 
-- mathEnv :: PandocMonad m => String -> LP m String
-- mathEnv name = do
--   skipopts
--   optional blankline
--   let endEnv = try $ controlSeq "end" *> braced >>= guard . (== name)
--       charMuncher = skipMany comment *>
--                        (many1 (noneOf "\\%") <|> try (string "\\%")
--                            <|> try (string "\\\\") <|> count 1 anyChar)
--   res <- concat <$> manyTill charMuncher endEnv
--   return $ stripTrailingNewlines res
-- 
-- verbEnv :: PandocMonad m => String -> LP m String
-- verbEnv name = do
--   skipopts
--   optional blankline
--   let endEnv = try $ controlSeq "end" *> braced >>= guard . (== name)
--       charMuncher = anyChar
--   res <- manyTill charMuncher endEnv
--   return $ stripTrailingNewlines res
-- 
-- fancyverbEnv :: PandocMonad m => String -> LP m Blocks
-- fancyverbEnv name = do
--   options <- option [] keyvals
--   let kvs = [ (if k == "firstnumber"
--                   then "startFrom"
--                   else k, v) | (k,v) <- options ]
--   let classes = [ "numberLines" |
--                   lookup "numbers" options == Just "left" ]
--   let attr = ("",classes,kvs)
--   codeBlockWith attr <$> verbEnv name
-- 
-- orderedList' :: PandocMonad m => LP m Blocks
-- orderedList' = try $ do
--   optional sp
--   (_, style, delim) <- option (1, DefaultStyle, DefaultDelim) $
--                               try $ char '[' *> anyOrderedListMarker <* char ']'
--   spaces
--   optional $ try $ controlSeq "setlength" *> grouped (controlSeq "itemindent") *> braced
--   spaces
--   start <- option 1 $ try $ do controlSeq "setcounter"
--                                grouped (string "enum" *> many1 (oneOf "iv"))
--                                optional sp
--                                num <- grouped (many1 digit)
--                                spaces
--                                return (read num + 1 :: Int)
--   bs <- listenv "enumerate" (many item)
--   return $ orderedListWith (start, style, delim) bs
-- 
-- paragraph :: PandocMonad m => LP m Blocks
-- paragraph = do
--   x <- trimInlines . mconcat <$> many1 inline
--   if x == mempty
--      then return mempty
--      else return $ para x
-- 
-- preamble :: PandocMonad m => LP m Blocks
-- preamble = mempty <$> manyTill preambleBlock beginDoc
--   where beginDoc = lookAhead $ try $ controlSeq "begin" *> string "{document}"
--         preambleBlock =  void comment
--                      <|> void sp
--                      <|> void blanklines
--                      <|> void include
--                      <|> void macro
--                      <|> void blockCommand
--                      <|> void anyControlSeq
--                      <|> void braced
--                      <|> void anyChar
-- 
-- -------
-- 
-- -- citations
-- 
-- addPrefix :: [Inline] -> [Citation] -> [Citation]
-- addPrefix p (k:ks) = k {citationPrefix = p ++ citationPrefix k} : ks
-- addPrefix _ _      = []
-- 
-- addSuffix :: [Inline] -> [Citation] -> [Citation]
-- addSuffix s ks@(_:_) =
--   let k = last ks
--   in  init ks ++ [k {citationSuffix = citationSuffix k ++ s}]
-- addSuffix _ _ = []
-- 
-- simpleCiteArgs :: PandocMonad m => LP m [Citation]
-- simpleCiteArgs = try $ do
--   first  <- optionMaybe $ toList <$> opt
--   second <- optionMaybe $ toList <$> opt
--   keys <- try $ bgroup *> (manyTill citationLabel egroup)
--   let (pre, suf) = case (first  , second ) of
--         (Just s , Nothing) -> (mempty, s )
--         (Just s , Just t ) -> (s , t )
--         _                  -> (mempty, mempty)
--       conv k = Citation { citationId      = k
--                         , citationPrefix  = []
--                         , citationSuffix  = []
--                         , citationMode    = NormalCitation
--                         , citationHash    = 0
--                         , citationNoteNum = 0
--                         }
--   return $ addPrefix pre $ addSuffix suf $ map conv keys
-- 
-- citationLabel :: PandocMonad m => LP m String
-- citationLabel  = optional sp *>
--   (many1 (satisfy isBibtexKeyChar)
--           <* optional sp
--           <* optional (char ',')
--           <* optional sp)
--   where isBibtexKeyChar c = isAlphaNum c || c `elem` (".:;?!`'()/*@_+=-[]" :: String)
-- 
-- cites :: PandocMonad m => CitationMode -> Bool -> LP m [Citation]
-- cites mode multi = try $ do
--   cits <- if multi
--              then many1 simpleCiteArgs
--              else count 1 simpleCiteArgs
--   let cs = concat cits
--   return $ case mode of
--         AuthorInText -> case cs of
--                              (c:rest) -> c {citationMode = mode} : rest
--                              []       -> []
--         _            -> map (\a -> a {citationMode = mode}) cs
-- 
-- citation :: PandocMonad m => String -> CitationMode -> Bool -> LP m Inlines
-- citation name mode multi = do
--   (c,raw) <- withRaw $ cites mode multi
--   return $ cite c (rawInline "latex" $ "\\" ++ name ++ raw)
-- 
-- complexNatbibCitation :: PandocMonad m => CitationMode -> LP m Inlines
-- complexNatbibCitation mode = try $ do
--   let ils = (toList . trimInlines . mconcat) <$>
--               many (notFollowedBy (oneOf "\\};") >> inline)
--   let parseOne = try $ do
--                    skipSpaces
--                    pref  <- ils
--                    cit' <- inline -- expect a citation
--                    let citlist = toList cit'
--                    cits' <- case citlist of
--                                  [Cite cs _] -> return cs
--                                  _           -> mzero
--                    suff  <- ils
--                    skipSpaces
--                    optional $ char ';'
--                    return $ addPrefix pref $ addSuffix suff cits'
--   (c:cits, raw) <- withRaw $ grouped parseOne
--   return $ cite (c{ citationMode = mode }:cits)
--            (rawInline "latex" $ "\\citetext" ++ raw)
-- 
-- -- tables
-- 
-- parseAligns :: PandocMonad m => LP m [(Alignment, Double, (String, String))]
-- parseAligns = try $ do
--   bgroup
--   let maybeBar = skipMany $ sp <|> () <$ char '|' <|> () <$ (char '@' >> braced)
--   maybeBar
--   let cAlign = AlignCenter <$ char 'c'
--   let lAlign = AlignLeft <$ char 'l'
--   let rAlign = AlignRight <$ char 'r'
--   let parAlign = AlignLeft <$ char 'p'
--   -- algins from tabularx
--   let xAlign = AlignLeft <$ char 'X'
--   let mAlign = AlignLeft <$ char 'm'
--   let bAlign = AlignLeft <$ char 'b'
--   let alignChar = cAlign <|> lAlign <|> rAlign <|> parAlign
--                <|> xAlign <|> mAlign <|> bAlign
--   let alignPrefix = char '>' >> braced
--   let alignSuffix = char '<' >> braced
--   let colWidth = try $ do
--         char '{'
--         ds <- many1 (oneOf "0123456789.")
--         spaces
--         string "\\linewidth"
--         char '}'
--         case safeRead ds of
--               Just w  -> return w
--               Nothing -> return 0.0
--   let alignSpec = do
--         spaces
--         pref <- option "" alignPrefix
--         spaces
--         al <- alignChar
--         width <- colWidth <|> option 0.0 (do s <- braced
--                                              pos <- getPosition
--                                              report $ SkippedContent s pos
--                                              return 0.0)
--         spaces
--         suff <- option "" alignSuffix
--         return (al, width, (pref, suff))
--   aligns' <- sepEndBy alignSpec maybeBar
--   spaces
--   egroup
--   spaces
--   return $ aligns'
-- 
-- hline :: PandocMonad m => LP m ()
-- hline = try $ do
--   spaces'
--   controlSeq "hline" <|>
--     -- booktabs rules:
--     controlSeq "toprule" <|>
--     controlSeq "bottomrule" <|>
--     controlSeq "midrule" <|>
--     controlSeq "endhead" <|>
--     controlSeq "endfirsthead"
--   spaces'
--   optional $ bracketed (many1 (satisfy (/=']')))
--   return ()
-- 
-- lbreak :: PandocMonad m => LP m ()
-- lbreak = () <$ try (spaces' *>
--                     (controlSeq "\\" <|> controlSeq "tabularnewline") <*
--                     spaces')
-- 
-- amp :: PandocMonad m => LP m ()
-- amp = () <$ try (spaces' *> char '&' <* spaces')
-- 
-- parseTableRow :: PandocMonad m
--               => String   -- ^ table environment name
--               -> [(String, String)] -- ^ pref/suffixes
--               -> LP m [Blocks]
-- parseTableRow envname prefsufs = try $ do
--   let cols = length prefsufs
--   let tableCellRaw = concat <$> many
--          (do notFollowedBy amp
--              notFollowedBy lbreak
--              notFollowedBy $ () <$ try (string ("\\end{" ++ envname ++ "}"))
--              many1 (noneOf "&%\n\r\\")
--                   <|> try (string "\\&")
--                   <|> count 1 anyChar)
--   let plainify bs = case toList bs of
--                          [Para ils] -> plain (fromList ils)
--                          _          -> bs
--   rawcells <- sepBy1 tableCellRaw amp
--   guard $ length rawcells == cols
--   let rawcells' = zipWith (\c (p, s) -> p ++ trim c ++ s) rawcells prefsufs
--   let tableCell = plainify <$> blocks
--   cells' <- mapM (parseFromString' tableCell) rawcells'
--   let numcells = length cells'
--   guard $ numcells <= cols && numcells >= 1
--   guard $ cells' /= [mempty]
--   -- note:  a & b in a three-column table leaves an empty 3rd cell:
--   let cells'' = cells' ++ replicate (cols - numcells) mempty
--   spaces'
--   return cells''
-- 
-- spaces' :: PandocMonad m => LP m ()
-- spaces' = spaces *> skipMany (comment *> spaces)
-- 
-- simpTable :: PandocMonad m => String -> Bool -> LP m Blocks
-- simpTable envname hasWidthParameter = try $ do
--   when hasWidthParameter $ () <$ (spaces' >> tok)
--   skipopts
--   colspecs <- parseAligns
--   let (aligns, widths, prefsufs) = unzip3 colspecs
--   let cols = length colspecs
--   optional $ controlSeq "caption" *> skipopts *> setCaption
--   optional lbreak
--   spaces'
--   skipMany hline
--   spaces'
--   header' <- option [] $ try (parseTableRow envname prefsufs <*
--                                    lbreak <* many1 hline)
--   spaces'
--   rows <- sepEndBy (parseTableRow envname prefsufs)
--                     (lbreak <* optional (skipMany hline))
--   spaces'
--   optional $ controlSeq "caption" *> skipopts *> setCaption
--   optional lbreak
--   spaces'
--   let header'' = if null header'
--                     then replicate cols mempty
--                     else header'
--   lookAhead $ controlSeq "end" -- make sure we're at end
--   return $ table mempty (zip aligns widths) header'' rows
-- 
-- removeDoubleQuotes :: String -> String
-- removeDoubleQuotes ('"':xs) =
--   case reverse xs of
--        '"':ys -> reverse ys
--        _      -> '"':xs
-- removeDoubleQuotes xs = xs
-- 
-- inputListing :: PandocMonad m => LP m Blocks
-- inputListing = do
--   pos <- getPosition
--   options <- option [] keyvals
--   f <- filter (/='"') <$> braced
--   dirs <- (splitBy (==':') . fromMaybe ".") <$> lookupEnv "TEXINPUTS"
--   mbCode <- readFileFromDirs dirs f
--   codeLines <- case mbCode of
--                       Just s -> return $ lines s
--                       Nothing -> do
--                         report $ CouldNotLoadIncludeFile f pos
--                         return []
--   let (ident,classes,kvs) = parseListingsOptions options
--   let language = case lookup "language" options >>= fromListingsLanguage of
--                       Just l -> [l]
--                       Nothing -> take 1 $ languagesByExtension (takeExtension f)
--   let firstline = fromMaybe 1 $ lookup "firstline" options >>= safeRead
--   let lastline = fromMaybe (length codeLines) $
--                        lookup "lastline" options >>= safeRead
--   let codeContents = intercalate "\n" $ take (1 + lastline - firstline) $
--                        drop (firstline - 1) codeLines
--   return $ codeBlockWith (ident,ordNub (classes ++ language),kvs) codeContents
-- 
-- parseListingsOptions :: [(String, String)] -> Attr
-- parseListingsOptions options =
--   let kvs = [ (if k == "firstnumber"
--                   then "startFrom"
--                   else k, v) | (k,v) <- options ]
--       classes = [ "numberLines" |
--                   lookup "numbers" options == Just "left" ]
--              ++ maybeToList (lookup "language" options
--                      >>= fromListingsLanguage)
--   in  (fromMaybe "" (lookup "label" options), classes, kvs)
-}
