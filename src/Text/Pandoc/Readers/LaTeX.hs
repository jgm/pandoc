{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
                                   applyMacros,
                                   rawLaTeXInline,
                                   rawLaTeXBlock,
                                   inlineCommand,
                                   tokenize,
                                   untokenize
                                 ) where

import Control.Applicative (many, optional, (<|>))
import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.Trans (lift)
import Data.Char (chr, isAlphaNum, isDigit, isLetter, ord, toLower)
import Data.Default
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Safe (minimumDef)
import System.FilePath (addExtension, replaceExtension, takeExtension)
import Text.Pandoc.BCP47 (Lang (..), renderLang)
import Text.Pandoc.Builder
import Text.Pandoc.Class (PandocMonad, PandocPure, getResourcePath, lookupEnv,
                          readFileFromDirs, report, setResourcePath,
                          setTranslations, translateTerm)
import Text.Pandoc.Error (PandocError (PandocMacroLoop, PandocParseError, PandocParsecError))
import Text.Pandoc.Highlighting (fromListingsLanguage, languagesByExtension)
import Text.Pandoc.ImageSize (numUnit, showFl)
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (blankline, many, mathDisplay, mathInline,
                            optional, space, spaces, withRaw, (<|>))
import Text.Pandoc.Readers.LaTeX.Types (ExpansionPoint (..), Macro (..),
                                        Tok (..), TokType (..))
import Text.Pandoc.Shared
import qualified Text.Pandoc.Translations as Translations
import Text.Pandoc.Walk
import Text.Parsec.Pos

-- for debugging:
-- import Text.Pandoc.Extensions (getDefaultExtensions)
-- import Text.Pandoc.Class (runIOorExplode, PandocIO)
-- import Debug.Trace (traceShowId)

-- | Parse LaTeX from string and return 'Pandoc' document.
readLaTeX :: PandocMonad m
          => ReaderOptions -- ^ Reader options
          -> Text        -- ^ String to parse (assumes @'\n'@ line endings)
          -> m Pandoc
readLaTeX opts ltx = do
  parsed <- runParserT parseLaTeX def{ sOptions = opts } "source"
               (tokenize "source" (crFilter ltx))
  case parsed of
    Right result -> return result
    Left e       -> throwError $ PandocParsecError (T.unpack ltx) e

parseLaTeX :: PandocMonad m => LP m Pandoc
parseLaTeX = do
  bs <- blocks
  eof
  st <- getState
  let meta = sMeta st
  let doc' = doc bs
  let headerLevel (Header n _ _) = [n]
      headerLevel _              = []
  let bottomLevel = minimumDef 1 $ query headerLevel doc'
  let adjustHeaders m (Header n attr ils) = Header (n+m) attr ils
      adjustHeaders _ x                   = x
  let (Pandoc _ bs') =
       -- handle the case where you have \part or \chapter
       (if bottomLevel < 1
           then walk (adjustHeaders (1 - bottomLevel))
           else id) $
       walk (resolveRefs (sLabels st)) doc'
  return $ Pandoc meta bs'

resolveRefs :: M.Map String [Inline] -> Inline -> Inline
resolveRefs labels x@(Link (ident,classes,kvs) _ _) =
  case (lookup "reference-type" kvs,
        lookup "reference" kvs) of
        (Just "ref", Just lab) ->
          case M.lookup lab labels of
               Just txt -> Link (ident,classes,kvs) txt ('#':lab, "")
               Nothing  -> x
        _ -> x
resolveRefs _ x = x


-- testParser :: LP PandocIO a -> Text -> IO a
-- testParser p t = do
--   res <- runIOorExplode (runParserT p defaultLaTeXState{
--             sOptions = def{ readerExtensions =
--               enableExtension Ext_raw_tex $
--                 getDefaultExtensions "latex" }} "source" (tokenize "source" t))
--   case res of
--        Left e  -> error (show e)
--        Right r -> return r

newtype HeaderNum = HeaderNum [Int]
  deriving (Show)

renderHeaderNum :: HeaderNum -> String
renderHeaderNum (HeaderNum xs) =
  intercalate "." (map show xs)

incrementHeaderNum :: Int -> HeaderNum -> HeaderNum
incrementHeaderNum level (HeaderNum ns) = HeaderNum $
  case reverse (take level (ns ++ repeat 0)) of
       (x:xs) -> reverse (x+1 : xs)
       []     -> []  -- shouldn't happen

data LaTeXState = LaTeXState{ sOptions       :: ReaderOptions
                            , sMeta          :: Meta
                            , sQuoteContext  :: QuoteContext
                            , sMacros        :: M.Map Text Macro
                            , sContainers    :: [String]
                            , sHeaders       :: M.Map Inlines String
                            , sLogMessages   :: [LogMessage]
                            , sIdentifiers   :: Set.Set String
                            , sVerbatimMode  :: Bool
                            , sCaption       :: Maybe Inlines
                            , sInListItem    :: Bool
                            , sInTableCell   :: Bool
                            , sLastHeaderNum :: HeaderNum
                            , sLabels        :: M.Map String [Inline]
                            , sToggles       :: M.Map String Bool
                            }
     deriving Show

defaultLaTeXState :: LaTeXState
defaultLaTeXState = LaTeXState{ sOptions       = def
                              , sMeta          = nullMeta
                              , sQuoteContext  = NoQuote
                              , sMacros        = M.empty
                              , sContainers    = []
                              , sHeaders       = M.empty
                              , sLogMessages   = []
                              , sIdentifiers   = Set.empty
                              , sVerbatimMode  = False
                              , sCaption       = Nothing
                              , sInListItem    = False
                              , sInTableCell   = False
                              , sLastHeaderNum = HeaderNum []
                              , sLabels        = M.empty
                              , sToggles       = M.empty
                              }

instance PandocMonad m => HasQuoteContext LaTeXState m where
  getQuoteContext = sQuoteContext <$> getState
  withQuoteContext context parser = do
    oldState <- getState
    let oldQuoteContext = sQuoteContext oldState
    setState oldState { sQuoteContext = context }
    result <- parser
    newState <- getState
    setState newState { sQuoteContext = oldQuoteContext }
    return result

instance HasLogMessages LaTeXState where
  addLogMessage msg st = st{ sLogMessages = msg : sLogMessages st }
  getLogMessages st = reverse $ sLogMessages st

instance HasIdentifierList LaTeXState where
  extractIdentifierList     = sIdentifiers
  updateIdentifierList f st = st{ sIdentifiers = f $ sIdentifiers st }

instance HasIncludeFiles LaTeXState where
  getIncludeFiles = sContainers
  addIncludeFile f s = s{ sContainers = f : sContainers s }
  dropLatestIncludeFile s = s { sContainers = drop 1 $ sContainers s }

instance HasHeaderMap LaTeXState where
  extractHeaderMap     = sHeaders
  updateHeaderMap f st = st{ sHeaders = f $ sHeaders st }

instance HasMacros LaTeXState where
  extractMacros  st  = sMacros st
  updateMacros f st  = st{ sMacros = f (sMacros st) }

instance HasReaderOptions LaTeXState where
  extractReaderOptions = sOptions

instance HasMeta LaTeXState where
  setMeta field val st =
    st{ sMeta = setMeta field val $ sMeta st }
  deleteMeta field st =
    st{ sMeta = deleteMeta field $ sMeta st }

instance Default LaTeXState where
  def = defaultLaTeXState

type LP m = ParserT [Tok] LaTeXState m

withVerbatimMode :: PandocMonad m => LP m a -> LP m a
withVerbatimMode parser = do
  updateState $ \st -> st{ sVerbatimMode = True }
  result <- parser
  updateState $ \st -> st{ sVerbatimMode = False }
  return result

rawLaTeXParser :: (PandocMonad m, HasMacros s, HasReaderOptions s)
               => LP m a -> ParserT String s m (a, String)
rawLaTeXParser parser = do
  inp <- getInput
  let toks = tokenize "source" $ T.pack inp
  pstate <- getState
  let lstate = def{ sOptions = extractReaderOptions pstate
                  , sMacros = extractMacros pstate }
  let rawparser = (,) <$> withRaw parser <*> getState
  res <- lift $ runParserT rawparser lstate "chunk" toks
  case res of
       Left _    -> mzero
       Right ((val, raw), st) -> do
         updateState (updateMacros (sMacros st <>))
         rawstring <- takeP (T.length (untokenize raw))
         return (val, rawstring)

applyMacros :: (PandocMonad m, HasMacros s, HasReaderOptions s)
            => String -> ParserT String s m String
applyMacros s = (guardDisabled Ext_latex_macros >> return s) <|>
   do let retokenize = doMacros 0 *>
             (toksToString <$> many (satisfyTok (const True)))
      pstate <- getState
      let lstate = def{ sOptions = extractReaderOptions pstate
                      , sMacros  = extractMacros pstate }
      res <- runParserT retokenize lstate "math" (tokenize "math" (T.pack s))
      case res of
           Left e   -> fail (show e)
           Right s' -> return s'

rawLaTeXBlock :: (PandocMonad m, HasMacros s, HasReaderOptions s)
              => ParserT String s m String
rawLaTeXBlock = do
  lookAhead (try (char '\\' >> letter))
  -- we don't want to apply newly defined latex macros to their own
  -- definitions:
  (do (_, raw) <- rawLaTeXParser macroDef
      (guardDisabled Ext_latex_macros >> return raw) <|> return "")
  <|> (do (_, raw) <- rawLaTeXParser (environment <|> blockCommand)
          applyMacros raw)

rawLaTeXInline :: (PandocMonad m, HasMacros s, HasReaderOptions s)
               => ParserT String s m String
rawLaTeXInline = do
  lookAhead (try (char '\\' >> letter) <|> char '$')
  rawLaTeXParser (inlineEnvironment <|> inlineCommand') >>= applyMacros . snd

inlineCommand :: PandocMonad m => ParserT String ParserState m Inlines
inlineCommand = do
  lookAhead (try (char '\\' >> letter) <|> char '$')
  fst <$> rawLaTeXParser (inlineEnvironment <|> inlineCommand')

tokenize :: SourceName -> Text -> [Tok]
tokenize sourcename = totoks (initialPos sourcename)

totoks :: SourcePos -> Text -> [Tok]
totoks pos t =
  case T.uncons t of
       Nothing        -> []
       Just (c, rest)
         | c == '\n' ->
           Tok pos Newline "\n"
           : totoks (setSourceColumn (incSourceLine pos 1) 1) rest
         | isSpaceOrTab c ->
           let (sps, rest') = T.span isSpaceOrTab t
           in  Tok pos Spaces sps
               : totoks (incSourceColumn pos (T.length sps))
                 rest'
         | isAlphaNum c ->
           let (ws, rest') = T.span isAlphaNum t
           in  Tok pos Word ws
               : totoks (incSourceColumn pos (T.length ws)) rest'
         | c == '%' ->
           let (cs, rest') = T.break (== '\n') rest
           in  Tok pos Comment ("%" <> cs)
               : totoks (incSourceColumn pos (1 + T.length cs)) rest'
         | c == '\\' ->
           case T.uncons rest of
                Nothing -> [Tok pos (CtrlSeq " ") "\\"]
                Just (d, rest')
                  | isLetterOrAt d ->
                      -- \makeatletter is common in macro defs;
                      -- ideally we should make tokenization sensitive
                      -- to \makeatletter and \makeatother, but this is
                      -- probably best for now
                      let (ws, rest'') = T.span isLetterOrAt rest
                          (ss, rest''') = T.span isSpaceOrTab rest''
                      in  Tok pos (CtrlSeq ws) ("\\" <> ws <> ss)
                          : totoks (incSourceColumn pos
                               (1 + T.length ws + T.length ss)) rest'''
                  | isSpaceOrTab d || d == '\n' ->
                      let (w1, r1) = T.span isSpaceOrTab rest
                          (w2, (w3, r3)) = case T.uncons r1 of
                                          Just ('\n', r2)
                                                  -> (T.pack "\n",
                                                        T.span isSpaceOrTab r2)
                                          _ -> (mempty, (mempty, r1))
                      in  case T.uncons r3 of
                               Just ('\n', _) ->
                                 Tok pos (CtrlSeq " ") ("\\" <> w1)
                                 : totoks (incSourceColumn pos 1) r1
                               _ ->
                                 Tok pos (CtrlSeq " ") ("\\" <> w1 <> w2 <> w3)
                                 : totoks (incSourceColumn pos 1) r3
                  | otherwise  ->
                      Tok pos (CtrlSeq (T.singleton d)) (T.pack [c,d])
                      : totoks (incSourceColumn pos 2) rest'
         | c == '#' ->
           let (t1, t2) = T.span (\d -> d >= '0' && d <= '9') rest
           in  case safeRead (T.unpack t1) of
                    Just i ->
                       Tok pos (Arg i) ("#" <> t1)
                       : totoks (incSourceColumn pos (1 + T.length t1)) t2
                    Nothing ->
                       Tok pos Symbol ("#")
                       : totoks (incSourceColumn pos 1) t2
         | c == '^' ->
           case T.uncons rest of
                Just ('^', rest') ->
                  case T.uncons rest' of
                       Just (d, rest'')
                         | isLowerHex d ->
                           case T.uncons rest'' of
                                Just (e, rest''') | isLowerHex e ->
                                  Tok pos Esc2 (T.pack ['^','^',d,e])
                                  : totoks (incSourceColumn pos 4) rest'''
                                _ ->
                                  Tok pos Esc1 (T.pack ['^','^',d])
                                  : totoks (incSourceColumn pos 3) rest''
                         | d < '\128' ->
                                  Tok pos Esc1 (T.pack ['^','^',d])
                                  : totoks (incSourceColumn pos 3) rest''
                       _ -> Tok pos Symbol ("^") :
                            Tok (incSourceColumn pos 1) Symbol ("^") :
                            totoks (incSourceColumn pos 2) rest'
                _ -> Tok pos Symbol ("^")
                     : totoks (incSourceColumn pos 1) rest
         | otherwise ->
           Tok pos Symbol (T.singleton c) : totoks (incSourceColumn pos 1) rest

isSpaceOrTab :: Char -> Bool
isSpaceOrTab ' '  = True
isSpaceOrTab '\t' = True
isSpaceOrTab _    = False

isLetterOrAt :: Char -> Bool
isLetterOrAt '@' = True
isLetterOrAt c   = isLetter c

isLowerHex :: Char -> Bool
isLowerHex x = x >= '0' && x <= '9' || x >= 'a' && x <= 'f'

untokenize :: [Tok] -> Text
untokenize = mconcat . map untoken

untoken :: Tok -> Text
untoken (Tok _ _ t) = t

satisfyTok :: PandocMonad m => (Tok -> Bool) -> LP m Tok
satisfyTok f =
  try $ do
    res <- tokenPrim (T.unpack . untoken) updatePos matcher
    doMacros 0 -- apply macros on remaining input stream
    return res
  where matcher t | f t       = Just t
                  | otherwise = Nothing
        updatePos :: SourcePos -> Tok -> [Tok] -> SourcePos
        updatePos _spos _ (Tok pos _ _ : _) = pos
        updatePos spos _ []                 = spos

doMacros :: PandocMonad m => Int -> LP m ()
doMacros n = do
  verbatimMode <- sVerbatimMode <$> getState
  unless verbatimMode $ do
    inp <- getInput
    case inp of
         Tok spos (CtrlSeq "begin") _ : Tok _ Symbol "{" :
          Tok _ Word name : Tok _ Symbol "}" : ts
            -> handleMacros spos name ts
         Tok spos (CtrlSeq "end") _ : Tok _ Symbol "{" :
          Tok _ Word name : Tok _ Symbol "}" : ts
            -> handleMacros spos ("end" <> name) ts
         Tok _ (CtrlSeq "expandafter") _ : t : ts
            -> do setInput ts
                  doMacros n
                  getInput >>= setInput . combineTok t
         Tok spos (CtrlSeq name) _ : ts
            -> handleMacros spos name ts
         _ -> return ()
  where combineTok (Tok spos (CtrlSeq name) x) (Tok _ Word w : ts)
          | T.all isLetterOrAt w =
            Tok spos (CtrlSeq (name <> w)) (x1 <> w <> x2) : ts
              where (x1, x2) = T.break isSpaceOrTab x
        combineTok t ts = t:ts
        handleMacros spos name ts = do
                macros <- sMacros <$> getState
                case M.lookup name macros of
                     Nothing -> return ()
                     Just (Macro expansionPoint numargs optarg newtoks) -> do
                       setInput ts
                       let getarg = try $ spaces >> bracedOrToken
                       args <- case optarg of
                                    Nothing -> count numargs getarg
                                    Just o  ->
                                       (:) <$> option o bracketedToks
                                           <*> count (numargs - 1) getarg
                       let addTok (Tok _ (Arg i) _) acc | i > 0
                                                        , i <= numargs =
                                 foldr addTok acc (args !! (i - 1))
                           -- add space if needed after control sequence
                           -- see #4007
                           addTok (Tok _ (CtrlSeq x) txt)
                                  acc@(Tok _ Word _ : _)
                             | not (T.null txt) &&
                               (isLetter (T.last txt)) =
                               Tok spos (CtrlSeq x) (txt <> " ") : acc
                           addTok t acc = setpos spos t : acc
                       ts' <- getInput
                       setInput $ foldr addTok ts' newtoks
                       case expansionPoint of
                            ExpandWhenUsed ->
                              if n > 20  -- detect macro expansion loops
                                 then throwError $ PandocMacroLoop (T.unpack name)
                                 else doMacros (n + 1)
                            ExpandWhenDefined -> return ()


setpos :: SourcePos -> Tok -> Tok
setpos spos (Tok _ tt txt) = Tok spos tt txt

anyControlSeq :: PandocMonad m => LP m Tok
anyControlSeq = satisfyTok isCtrlSeq
  where isCtrlSeq (Tok _ (CtrlSeq _) _) = True
        isCtrlSeq _                     = False

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
  where isNamed (Tok _ (CtrlSeq n) _) = n == name
        isNamed _                     = False

symbol :: PandocMonad m => Char -> LP m Tok
symbol c = satisfyTok isc
  where isc (Tok _ Symbol d) = case T.uncons d of
                                    Just (c',_) -> c == c'
                                    _           -> False
        isc _ = False

symbolIn :: PandocMonad m => [Char] -> LP m Tok
symbolIn cs = satisfyTok isInCs
  where isInCs (Tok _ Symbol d) = case T.uncons d of
                                       Just (c,_) -> c `elem` cs
                                       _          -> False
        isInCs _ = False

sp :: PandocMonad m => LP m ()
sp = whitespace <|> endline

whitespace :: PandocMonad m => LP m ()
whitespace = () <$ satisfyTok isSpaceTok
  where isSpaceTok (Tok _ Spaces _) = True
        isSpaceTok _                = False

newlineTok :: PandocMonad m => LP m ()
newlineTok = () <$ satisfyTok isNewlineTok

isNewlineTok :: Tok -> Bool
isNewlineTok (Tok _ Newline _) = True
isNewlineTok _                 = False

comment :: PandocMonad m => LP m ()
comment = () <$ satisfyTok isCommentTok
  where isCommentTok (Tok _ Comment _) = True
        isCommentTok _                 = False

anyTok :: PandocMonad m => LP m Tok
anyTok = satisfyTok (const True)

endline :: PandocMonad m => LP m ()
endline = try $ do
  newlineTok
  lookAhead anyTok
  notFollowedBy blankline

blankline :: PandocMonad m => LP m ()
blankline = try $ skipMany whitespace *> newlineTok

primEscape :: PandocMonad m => LP m Char
primEscape = do
  Tok _ toktype t <- satisfyTok (tokTypeIn [Esc1, Esc2])
  case toktype of
       Esc1 -> case T.uncons (T.drop 2 t) of
                    Just (c, _)
                      | c >= '\64' && c <= '\127' -> return (chr (ord c - 64))
                      | otherwise                 -> return (chr (ord c + 64))
                    Nothing -> fail "Empty content of Esc1"
       Esc2 -> case safeRead ('0':'x':T.unpack (T.drop 2 t)) of
                    Just x  -> return (chr x)
                    Nothing -> fail $ "Could not read: " ++ T.unpack t
       _    -> fail "Expected an Esc1 or Esc2 token" -- should not happen

bgroup :: PandocMonad m => LP m Tok
bgroup = try $ do
  skipMany sp
  symbol '{' <|> controlSeq "bgroup" <|> controlSeq "begingroup"

egroup :: PandocMonad m => LP m Tok
egroup = symbol '}' <|> controlSeq "egroup" <|> controlSeq "endgroup"

grouped :: (PandocMonad m,  Monoid a) => LP m a -> LP m a
grouped parser = try $ do
  bgroup
  -- first we check for an inner 'grouped', because
  -- {{a,b}} should be parsed the same as {a,b}
  try (grouped parser <* egroup) <|> (mconcat <$> manyTill parser egroup)

braced :: PandocMonad m => LP m [Tok]
braced = bgroup *> braced' 1
  where braced' (n :: Int) =
          handleEgroup n <|> handleBgroup n <|> handleOther n
        handleEgroup n = do
          t <- egroup
          if n == 1
             then return []
             else (t:) <$> braced' (n - 1)
        handleBgroup n = do
          t <- bgroup
          (t:) <$> braced' (n + 1)
        handleOther n = do
          t <- anyTok
          (t:) <$> braced' n

bracketed :: PandocMonad m => Monoid a => LP m a -> LP m a
bracketed parser = try $ do
  symbol '['
  mconcat <$> manyTill parser (symbol ']')

dimenarg :: PandocMonad m => LP m Text
dimenarg = try $ do
  ch  <- option False $ True <$ symbol '='
  Tok _ _ s <- satisfyTok isWordTok
  guard $ T.take 2 (T.reverse s) `elem`
           ["pt","pc","in","bp","cm","mm","dd","cc","sp"]
  let num = T.take (T.length s - 2) s
  guard $ T.length num > 0
  guard $ T.all isDigit num
  return $ T.pack ['=' | ch] <> s

-- inline elements:

word :: PandocMonad m => LP m Inlines
word = (str . T.unpack . untoken) <$> satisfyTok isWordTok

regularSymbol :: PandocMonad m => LP m Inlines
regularSymbol = (str . T.unpack . untoken) <$> satisfyTok isRegularSymbol
  where isRegularSymbol (Tok _ Symbol t) = not $ T.any isSpecial t
        isRegularSymbol _                = False
        isSpecial c = c `Set.member` specialChars

specialChars :: Set.Set Char
specialChars = Set.fromList "#$%&~_^\\{}"

isWordTok :: Tok -> Bool
isWordTok (Tok _ Word _) = True
isWordTok _              = False

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

mkImage :: PandocMonad m => [(String, String)] -> String -> LP m Inlines
mkImage options src = do
   let replaceTextwidth (k,v) =
         case numUnit v of
              Just (num, "\\textwidth") -> (k, showFl (num * 100) ++ "%")
              _                         -> (k, v)
   let kvs = map replaceTextwidth
             $ filter (\(k,_) -> k `elem` ["width", "height"]) options
   let attr = ("",[], kvs)
   let alt = str "image"
   case takeExtension src of
        "" -> do
              defaultExt <- getOption readerDefaultImageExtension
              return $ imageWith attr (addExtension src defaultExt) "" alt
        _  -> return $ imageWith attr src "" alt

doxspace :: PandocMonad m => LP m Inlines
doxspace =
  (space <$ lookAhead (satisfyTok startsWithLetter)) <|> return mempty
  where startsWithLetter (Tok _ Word t) =
          case T.uncons t of
               Just (c, _) | isLetter c -> True
               _           -> False
        startsWithLetter _ = False


-- converts e.g. \SI{1}[\$]{} to "$ 1" or \SI{1}{\euro} to "1 €"
dosiunitx :: PandocMonad m => LP m Inlines
dosiunitx = do
  skipopts
  value <- tok
  valueprefix <- option "" $ bracketed tok
  unit <- tok
  let emptyOr160 "" = ""
      emptyOr160 _  = "\160"
  return . mconcat $ [valueprefix,
                      emptyOr160 valueprefix,
                      value,
                      emptyOr160 unit,
                      unit]

lit :: String -> LP m Inlines
lit = pure . str

removeDoubleQuotes :: Text -> Text
removeDoubleQuotes t =
  Data.Maybe.fromMaybe t $ T.stripPrefix "\"" t >>= T.stripSuffix "\""

doubleQuote :: PandocMonad m => LP m Inlines
doubleQuote =
       quoted' doubleQuoted (try $ count 2 $ symbol '`')
                     (void $ try $ count 2 $ symbol '\'')
   <|> quoted' doubleQuoted ((:[]) <$> symbol '“') (void $ symbol '”')
   -- the following is used by babel for localized quotes:
   <|> quoted' doubleQuoted (try $ sequence [symbol '"', symbol '`'])
                            (void $ try $ sequence [symbol '"', symbol '\''])

singleQuote :: PandocMonad m => LP m Inlines
singleQuote =
       quoted' singleQuoted ((:[]) <$> symbol '`')
                     (try $ symbol '\'' >>
                           notFollowedBy (satisfyTok startsWithLetter))
   <|> quoted' singleQuoted ((:[]) <$> symbol '‘')
                            (try $ symbol '’' >>
                                  notFollowedBy (satisfyTok startsWithLetter))
  where startsWithLetter (Tok _ Word t) =
          case T.uncons t of
               Just (c, _) | isLetter c -> True
               _           -> False
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
  quoteContext <- sQuoteContext <$> getState
  if quoteContext == InDoubleQuote
     then singleQuoted <$> withQuoteContext InSingleQuote tok
     else doubleQuoted <$> withQuoteContext InDoubleQuote tok

doAcronym :: PandocMonad m => String -> LP m Inlines
doAcronym form = do
  acro <- braced
  return . mconcat $ [spanWith ("",[],[("acronym-label", toksToString acro),
    ("acronym-form", "singular+" ++ form)])
    $ str $ toksToString acro]

doAcronymPlural :: PandocMonad m => String -> LP m Inlines
doAcronymPlural form = do
  acro <- braced
  plural <- lit "s"
  return . mconcat $ [spanWith ("",[],[("acronym-label", toksToString acro),
    ("acronym-form", "plural+" ++ form)]) $
   mconcat [str $ toksToString acro, plural]]

doverb :: PandocMonad m => LP m Inlines
doverb = do
  Tok _ Symbol t <- anySymbol
  marker <- case T.uncons t of
              Just (c, ts) | T.null ts -> return c
              _            -> mzero
  withVerbatimMode $
    (code . T.unpack . untokenize) <$>
      manyTill (verbTok marker) (symbol marker)

verbTok :: PandocMonad m => Char -> LP m Tok
verbTok stopchar = do
  t@(Tok pos toktype txt) <- satisfyTok (not . isNewlineTok)
  case T.findIndex (== stopchar) txt of
       Nothing -> return t
       Just i  -> do
         let (t1, t2) = T.splitAt i txt
         inp <- getInput
         setInput $ Tok (incSourceColumn pos i) Symbol (T.singleton stopchar)
                  : totoks (incSourceColumn pos (i + 1)) (T.drop 1 t2) ++ inp
         return $ Tok pos toktype t1

dolstinline :: PandocMonad m => LP m Inlines
dolstinline = do
  options <- option [] keyvals
  let classes = maybeToList $ lookup "language" options >>= fromListingsLanguage
  Tok _ Symbol t <- anySymbol
  marker <- case T.uncons t of
              Just (c, ts) | T.null ts -> return c
              _            -> mzero
  let stopchar = if marker == '{' then '}' else marker
  withVerbatimMode $
    (codeWith ("",classes,[]) . T.unpack . untokenize) <$>
      manyTill (verbTok stopchar) (symbol stopchar)

keyval :: PandocMonad m => LP m (String, String)
keyval = try $ do
  Tok _ Word key <- satisfyTok isWordTok
  let isSpecSym (Tok _ Symbol t) = t /= "]" && t /= ","
      isSpecSym _                = False
  optional sp
  val <- option [] $ do
           symbol '='
           optional sp
           braced <|> many1 (satisfyTok isWordTok <|> satisfyTok isSpecSym
                               <|> anyControlSeq)
  optional sp
  optional (symbol ',')
  optional sp
  return (T.unpack key, T.unpack . untokenize $ val)

keyvals :: PandocMonad m => LP m [(String, String)]
keyvals = try $ symbol '[' >> manyTill keyval (symbol ']')

accent :: PandocMonad m => Char -> (Char -> String) -> LP m Inlines
accent c f = try $ do
  ils <- tok
  case toList ils of
       (Str (x:xs) : ys) -> return $ fromList (Str (f x ++ xs) : ys)
       [Space]           -> return $ str [c]
       []                -> return $ str [c]
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

ogonek :: Char -> String
ogonek 'a' = "ą"
ogonek 'e' = "ę"
ogonek 'o' = "ǫ"
ogonek 'i' = "į"
ogonek 'u' = "ų"
ogonek 'A' = "Ą"
ogonek 'E' = "Ę"
ogonek 'I' = "Į"
ogonek 'O' = "Ǫ"
ogonek 'U' = "Ų"
ogonek c   = [c]

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

mathInline :: String -> Inlines
mathInline = math . trim

dollarsMath :: PandocMonad m => LP m Inlines
dollarsMath = do
  symbol '$'
  display <- option False (True <$ symbol '$')
  contents <- trim . toksToString <$>
               many (notFollowedBy (symbol '$') >> anyTok)
  if display
     then
       mathDisplay contents <$ try (symbol '$' >> symbol '$')
   <|> (guard (null contents) >> return (mathInline ""))
     else mathInline contents <$ symbol '$'

-- citations

addPrefix :: [Inline] -> [Citation] -> [Citation]
addPrefix p (k:ks) = k {citationPrefix = p ++ citationPrefix k} : ks
addPrefix _ _      = []

addSuffix :: [Inline] -> [Citation] -> [Citation]
addSuffix s ks@(_:_) =
  let k = last ks
  in  init ks ++ [k {citationSuffix = citationSuffix k ++ s}]
addSuffix _ _ = []

simpleCiteArgs :: PandocMonad m => LP m [Citation]
simpleCiteArgs = try $ do
  first  <- optionMaybe $ toList <$> opt
  second <- optionMaybe $ toList <$> opt
  keys <- try $ bgroup *> manyTill citationLabel egroup
  let (pre, suf) = case (first  , second ) of
        (Just s , Nothing) -> (mempty, s )
        (Just s , Just t ) -> (s , t )
        _                  -> (mempty, mempty)
      conv k = Citation { citationId      = k
                        , citationPrefix  = []
                        , citationSuffix  = []
                        , citationMode    = NormalCitation
                        , citationHash    = 0
                        , citationNoteNum = 0
                        }
  return $ addPrefix pre $ addSuffix suf $ map conv keys

citationLabel :: PandocMonad m => LP m String
citationLabel  = do
  optional sp
  toksToString <$>
    (many1 (satisfyTok isWordTok <|> symbolIn bibtexKeyChar)
          <* optional sp
          <* optional (symbol ',')
          <* optional sp)
  where bibtexKeyChar = ".:;?!`'()/*@_+=-[]" :: [Char]

cites :: PandocMonad m => CitationMode -> Bool -> LP m [Citation]
cites mode multi = try $ do
  cits <- if multi
             then many1 simpleCiteArgs
             else count 1 simpleCiteArgs
  let cs = concat cits
  return $ case mode of
        AuthorInText -> case cs of
                             (c:rest) -> c {citationMode = mode} : rest
                             []       -> []
        _            -> map (\a -> a {citationMode = mode}) cs

citation :: PandocMonad m => String -> CitationMode -> Bool -> LP m Inlines
citation name mode multi = do
  (c,raw) <- withRaw $ cites mode multi
  return $ cite c (rawInline "latex" $ "\\" ++ name ++ toksToString raw)

handleCitationPart :: Inlines -> [Citation]
handleCitationPart ils =
  let isCite Cite{} = True
      isCite _      = False
      (pref, rest) = break isCite (toList ils)
  in case rest of
          (Cite cs _:suff) -> addPrefix pref $ addSuffix suff cs
          _                -> []

complexNatbibCitation :: PandocMonad m => CitationMode -> LP m Inlines
complexNatbibCitation mode = try $ do
  (cs, raw) <-
    withRaw $ concat <$> do
      bgroup
      items <- mconcat <$>
                many1 (notFollowedBy (symbol ';') >> inline)
                  `sepBy1` (symbol ';')
      egroup
      return $ map handleCitationPart items
  case cs of
       []       -> mzero
       (c:cits) -> return $ cite (c{ citationMode = mode }:cits)
                      (rawInline "latex" $ "\\citetext" ++ toksToString raw)

inNote :: Inlines -> Inlines
inNote ils =
  note $ para $ ils <> str "."

inlineCommand' :: PandocMonad m => LP m Inlines
inlineCommand' = try $ do
  Tok _ (CtrlSeq name) cmd <- anyControlSeq
  guard $ name /= "begin" && name /= "end"
  star <- option "" ("*" <$ symbol '*' <* optional sp)
  let name' = name <> star
  let names = ordNub [name', name] -- check non-starred as fallback
  let raw = do
       guard $ isInlineCommand name || not (isBlockCommand name)
       rawcommand <- getRawCommand name (cmd <> star)
       (guardEnabled Ext_raw_tex >> return (rawInline "latex" rawcommand))
         <|> ignore rawcommand
  lookupListDefault raw names inlineCommands

tok :: PandocMonad m => LP m Inlines
tok = try $ spaces >> grouped inline <|> inlineCommand' <|> singleChar'
  where singleChar' = do
          Tok _ _ t <- singleChar
          return (str (T.unpack t))

singleChar :: PandocMonad m => LP m Tok
singleChar = try $ do
  Tok pos toktype t <- satisfyTok (tokTypeIn [Word, Symbol])
  guard $ not $ toktype == Symbol &&
                T.any (`Set.member` specialChars) t
  if T.length t > 1
     then do
       let (t1, t2) = (T.take 1 t, T.drop 1 t)
       inp <- getInput
       setInput $ Tok (incSourceColumn pos 1) toktype t2 : inp
       return $ Tok pos toktype t1
     else return $ Tok pos toktype t

opt :: PandocMonad m => LP m Inlines
opt = bracketed inline

rawopt :: PandocMonad m => LP m Text
rawopt = do
  inner <- untokenize <$> bracketedToks
  optional sp
  return $ "[" <> inner <> "]"

skipopts :: PandocMonad m => LP m ()
skipopts = skipMany rawopt

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
  nxt <- option (Tok (initialPos "source") Word "") (lookAhead anyTok)
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
            => (Inlines -> a) -> Maybe Text -> Text -> LP m a
mathEnvWith f innerEnv name = f . mathDisplay . inner <$> mathEnv name
   where inner x = case innerEnv of
                        Nothing -> x
                        Just y  -> "\\begin{" ++ T.unpack y ++ "}\n" ++ x ++
                                   "\\end{" ++ T.unpack y ++ "}"

mathEnv :: PandocMonad m => Text -> LP m String
mathEnv name = do
  skipopts
  optional blankline
  res <- manyTill anyTok (end_ name)
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
inlineCommands = M.union inlineLanguageCommands $ M.fromList $
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
  , ("alert", skipangles >> spanWith ("",["alert"],[]) <$> tok) -- beamer
  , ("lq", return (str "‘"))
  , ("rq", return (str "’"))
  , ("textquoteleft", return (str "‘"))
  , ("textquoteright", return (str "’"))
  , ("textquotedblleft", return (str "“"))
  , ("textquotedblright", return (str "”"))
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
  , ("sep", lit ",")
  , ("label", rawInlineOr "label" dolabel)
  , ("ref", rawInlineOr "ref" $ doref "ref")
  , ("cref", rawInlineOr "cref" $ doref "ref")       -- from cleveref.sty
  , ("vref", rawInlineOr "vref" $ doref "ref+page")  -- from varioref.sty
  , ("eqref", rawInlineOr "eqref" $ doref "eqref")   -- from amsmath.sty
  , ("lettrine", optional opt >> extractSpaces (spanWith ("",["lettrine"],[])) <$> tok)
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
  , ("H", accent '\779' hungarumlaut)
  , ("`", accent '`' grave)
  , ("'", accent '\'' acute)
  , ("^", accent '^' circ)
  , ("~", accent '~' tilde)
  , ("\"", accent '\776' umlaut)
  , (".", accent '\775' dot)
  , ("=", accent '\772' macron)
  , ("c", accent '\807' cedilla)
  , ("v", accent 'ˇ' hacek)
  , ("u", accent '\774' breve)
  , ("k", accent '\808' ogonek)
  , ("textogonekcentered", accent '\808' ogonek)
  , ("i", lit "i")
  , ("\\", linebreak <$ (do inTableCell <- sInTableCell <$> getState
                            guard $ not inTableCell
                            optional opt
                            spaces))
  , (",", lit "\8198")
  , ("@", pure mempty)
  , (" ", lit "\160")
  , ("ps", pure $ str "PS." <> space)
  , ("TeX", lit "TeX")
  , ("LaTeX", lit "LaTeX")
  , ("bar", lit "|")
  , ("textless", lit "<")
  , ("textgreater", lit ">")
  , ("thanks", skipopts >> note <$> grouped block)
  , ("footnote", skipopts >> note <$> grouped block)
  , ("verb", doverb)
  , ("lstinline", dolstinline)
  , ("Verb", doverb)
  , ("url", ((unescapeURL . T.unpack . untokenize) <$> braced) >>= \url ->
                  pure (link url "" (str url)))
  , ("href", (unescapeURL . toksToString <$>
                 braced <* optional sp) >>= \url ->
                   tok >>= \lab -> pure (link url "" lab))
  , ("includegraphics", do options <- option [] keyvals
                           src <- unescapeURL . T.unpack .
                                    removeDoubleQuotes . untokenize <$> braced
                           mkImage options src)
  , ("enquote", enquote)
  , ("figurename", doTerm Translations.Figure)
  , ("prefacename", doTerm Translations.Preface)
  , ("refname", doTerm Translations.References)
  , ("bibname", doTerm Translations.Bibliography)
  , ("chaptername", doTerm Translations.Chapter)
  , ("partname", doTerm Translations.Part)
  , ("contentsname", doTerm Translations.Contents)
  , ("listfigurename", doTerm Translations.ListOfFigures)
  , ("listtablename", doTerm Translations.ListOfTables)
  , ("indexname", doTerm Translations.Index)
  , ("abstractname", doTerm Translations.Abstract)
  , ("tablename", doTerm Translations.Table)
  , ("enclname", doTerm Translations.Encl)
  , ("ccname", doTerm Translations.Cc)
  , ("headtoname", doTerm Translations.To)
  , ("pagename", doTerm Translations.Page)
  , ("seename", doTerm Translations.See)
  , ("seealsoname", doTerm Translations.SeeAlso)
  , ("proofname", doTerm Translations.Proof)
  , ("glossaryname", doTerm Translations.Glossary)
  , ("lstlistingname", doTerm Translations.Listing)
  , ("cite", citation "cite" NormalCitation False)
  , ("Cite", citation "Cite" NormalCitation False)
  , ("citep", citation "citep" NormalCitation False)
  , ("citep*", citation "citep*" NormalCitation False)
  , ("citeal", citation "citeal" NormalCitation False)
  , ("citealp", citation "citealp" NormalCitation False)
  , ("citealp*", citation "citealp*" NormalCitation False)
  , ("autocite", citation "autocite" NormalCitation False)
  , ("smartcite", citation "smartcite" NormalCitation False)
  , ("footcite", inNote <$> citation "footcite" NormalCitation False)
  , ("parencite", citation "parencite" NormalCitation False)
  , ("supercite", citation "supercite" NormalCitation False)
  , ("footcitetext", inNote <$> citation "footcitetext" NormalCitation False)
  , ("citeyearpar", citation "citeyearpar" SuppressAuthor False)
  , ("citeyear", citation "citeyear" SuppressAuthor False)
  , ("autocite*", citation "autocite*" SuppressAuthor False)
  , ("cite*", citation "cite*" SuppressAuthor False)
  , ("parencite*", citation "parencite*" SuppressAuthor False)
  , ("textcite", citation "textcite" AuthorInText False)
  , ("citet", citation "citet" AuthorInText False)
  , ("citet*", citation "citet*" AuthorInText False)
  , ("citealt", citation "citealt" AuthorInText False)
  , ("citealt*", citation "citealt*" AuthorInText False)
  , ("textcites", citation "textcites" AuthorInText True)
  , ("cites", citation "cites" NormalCitation True)
  , ("autocites", citation "autocites" NormalCitation True)
  , ("footcites", inNote <$> citation "footcites" NormalCitation True)
  , ("parencites", citation "parencites" NormalCitation True)
  , ("supercites", citation "supercites" NormalCitation True)
  , ("footcitetexts", inNote <$> citation "footcitetexts" NormalCitation True)
  , ("Autocite", citation "Autocite" NormalCitation False)
  , ("Smartcite", citation "Smartcite" NormalCitation False)
  , ("Footcite", citation "Footcite" NormalCitation False)
  , ("Parencite", citation "Parencite" NormalCitation False)
  , ("Supercite", citation "Supercite" NormalCitation False)
  , ("Footcitetext", inNote <$> citation "Footcitetext" NormalCitation False)
  , ("Citeyearpar", citation "Citeyearpar" SuppressAuthor False)
  , ("Citeyear", citation "Citeyear" SuppressAuthor False)
  , ("Autocite*", citation "Autocite*" SuppressAuthor False)
  , ("Cite*", citation "Cite*" SuppressAuthor False)
  , ("Parencite*", citation "Parencite*" SuppressAuthor False)
  , ("Textcite", citation "Textcite" AuthorInText False)
  , ("Textcites", citation "Textcites" AuthorInText True)
  , ("Cites", citation "Cites" NormalCitation True)
  , ("Autocites", citation "Autocites" NormalCitation True)
  , ("Footcites", citation "Footcites" NormalCitation True)
  , ("Parencites", citation "Parencites" NormalCitation True)
  , ("Supercites", citation "Supercites" NormalCitation True)
  , ("Footcitetexts", inNote <$> citation "Footcitetexts" NormalCitation True)
  , ("citetext", complexNatbibCitation NormalCitation)
  , ("citeauthor", (try (tok *> optional sp *> controlSeq "citetext") *>
                        complexNatbibCitation AuthorInText)
                   <|> citation "citeauthor" AuthorInText False)
  , ("nocite", mempty <$ (citation "nocite" NormalCitation False >>=
                          addMeta "nocite"))
  , ("hyperlink", hyperlink)
  , ("hypertarget", hypertargetInline)
  -- glossaries package
  , ("gls", doAcronym "short")
  , ("Gls", doAcronym "short")
  , ("glsdesc", doAcronym "long")
  , ("Glsdesc", doAcronym "long")
  , ("GLSdesc", doAcronym "long")
  , ("acrlong", doAcronym "long")
  , ("Acrlong", doAcronym "long")
  , ("acrfull", doAcronym "full")
  , ("Acrfull", doAcronym "full")
  , ("acrshort", doAcronym "abbrv")
  , ("Acrshort", doAcronym "abbrv")
  , ("glspl", doAcronymPlural "short")
  , ("Glspl", doAcronymPlural "short")
  , ("glsdescplural", doAcronymPlural "long")
  , ("Glsdescplural", doAcronymPlural "long")
  , ("GLSdescplural", doAcronymPlural "long")
  -- acronyms package
  , ("ac", doAcronym "short")
  , ("acf", doAcronym "full")
  , ("acs", doAcronym "abbrv")
  , ("acp", doAcronymPlural "short")
  , ("acfp", doAcronymPlural "full")
  , ("acsp", doAcronymPlural "abbrv")
  -- siuntix
  , ("SI", dosiunitx)
  -- hyphenat
  , ("bshyp", lit "\\\173")
  , ("fshyp", lit "/\173")
  , ("dothyp", lit ".\173")
  , ("colonhyp", lit ":\173")
  , ("hyp", lit "-")
  , ("nohyphens", tok)
  , ("textnhtt", ttfamily)
  , ("nhttfamily", ttfamily)
  -- LaTeX colors
  , ("textcolor", coloredInline "color")
  , ("colorbox", coloredInline "background-color")
  -- fontawesome
  , ("faCheck", lit "\10003")
  , ("faClose", lit "\10007")
  -- xspace
  , ("xspace", doxspace)
  -- etoolbox
  , ("ifstrequal", ifstrequal)
  , ("newtoggle", braced >>= newToggle)
  , ("toggletrue", braced >>= setToggle True)
  , ("togglefalse", braced >>= setToggle False)
  , ("iftoggle", try $ ifToggle >> inline)
  -- biblatex misc
  , ("RN", romanNumeralUpper)
  , ("Rn", romanNumeralLower)
  ]

inlineLanguageCommands :: PandocMonad m => M.Map Text (LP m Inlines)
inlineLanguageCommands = M.fromList $ mk <$> M.toList polyglossiaLangToBCP47
  where
    mk (polyglossia, bcp47Func) =
      ("text" <> T.pack polyglossia, inlineLanguage bcp47Func)

inlineLanguage :: PandocMonad m => (String -> Lang) -> LP m Inlines
inlineLanguage bcp47Func = do
  o <- option "" $ (T.unpack . T.filter (\c -> c /= '[' && c /= ']'))
                <$> rawopt
  let lang = renderLang $ bcp47Func o
  extractSpaces (spanWith ("", [], [("lang", lang)])) <$> tok

hyperlink :: PandocMonad m => LP m Inlines
hyperlink = try $ do
  src <- toksToString <$> braced
  lab <- tok
  return $ link ('#':src) "" lab

hypertargetBlock :: PandocMonad m => LP m Blocks
hypertargetBlock = try $ do
  ref <- toksToString <$> braced
  bs <- grouped block
  case toList bs of
       [Header 1 (ident,_,_) _] | ident == ref -> return bs
       _                        -> return $ divWith (ref, [], []) bs

hypertargetInline :: PandocMonad m => LP m Inlines
hypertargetInline = try $ do
  ref <- toksToString <$> braced
  ils <- grouped inline
  return $ spanWith (ref, [], []) ils

romanNumeralUpper :: (PandocMonad m) => LP m Inlines
romanNumeralUpper =
  str . toRomanNumeral <$> romanNumeralArg

romanNumeralLower :: (PandocMonad m) => LP m Inlines
romanNumeralLower =
  str . map toLower . toRomanNumeral <$> romanNumeralArg

romanNumeralArg :: (PandocMonad m) => LP m Int
romanNumeralArg = spaces *> (parser <|> inBraces)
  where
    inBraces = do
      symbol '{'
      spaces
      res <- parser
      spaces
      symbol '}'
      return res
    parser = do
      Tok _ Word s <- satisfyTok isWordTok
      let (digits, rest) = T.span isDigit s
      unless (T.null rest) $
        fail "Non-digits in argument to \\Rn or \\RN"
      safeRead $ T.unpack digits

newToggle :: (Monoid a, PandocMonad m) => [Tok] -> LP m a
newToggle name = do
  updateState $ \st ->
    st{ sToggles = M.insert (toksToString name) False (sToggles st) }
  return mempty

setToggle :: (Monoid a, PandocMonad m) => Bool -> [Tok] -> LP m a
setToggle on name = do
  updateState $ \st ->
    st{ sToggles = M.adjust (const on) (toksToString name) (sToggles st) }
  return mempty

ifToggle :: PandocMonad m => LP m ()
ifToggle = do
  name <- braced
  spaces
  yes <- braced
  spaces
  no <- braced
  toggles <- sToggles <$> getState
  inp <- getInput
  let name' = toksToString name
  case M.lookup name' toggles of
                Just True  -> setInput (yes ++ inp)
                Just False -> setInput (no  ++ inp)
                Nothing    -> do
                  pos <- getPosition
                  report $ UndefinedToggle name' pos
  return ()

doTerm :: PandocMonad m => Translations.Term -> LP m Inlines
doTerm term = str <$> translateTerm term

ifstrequal :: (PandocMonad m, Monoid a) => LP m a
ifstrequal = do
  str1 <- tok
  str2 <- tok
  ifequal <- braced
  ifnotequal <- braced
  if str1 == str2
     then getInput >>= setInput . (ifequal ++)
     else getInput >>= setInput . (ifnotequal ++)
  return mempty

coloredInline :: PandocMonad m => String -> LP m Inlines
coloredInline stylename = do
  skipopts
  color <- braced
  spanWith ("",[],[("style",stylename ++ ": " ++ toksToString color)]) <$> tok

ttfamily :: PandocMonad m => LP m Inlines
ttfamily = (code . stringify . toList) <$> tok

rawInlineOr :: PandocMonad m => Text -> LP m Inlines -> LP m Inlines
rawInlineOr name' fallback = do
  parseRaw <- extensionEnabled Ext_raw_tex <$> getOption readerExtensions
  if parseRaw
     then rawInline "latex" <$> getRawCommand name' ("\\" <> name')
     else fallback

getRawCommand :: PandocMonad m => Text -> Text -> LP m String
getRawCommand name txt = do
  (_, rawargs) <- withRaw $
      case name of
           "write" -> do
             void $ satisfyTok isWordTok -- digits
             void braced
           "titleformat" -> do
             void braced
             skipopts
             void $ count 4 braced
           "def" ->
             void $ manyTill anyTok braced
           _ -> do
             skipangles
             skipopts
             option "" (try (optional sp *> dimenarg))
             void $ many braced
  return $ T.unpack (txt <> untokenize rawargs)

isBlockCommand :: Text -> Bool
isBlockCommand s =
  s `M.member` (blockCommands :: M.Map Text (LP PandocPure Blocks))
  || s `Set.member` treatAsBlock

treatAsBlock :: Set.Set Text
treatAsBlock = Set.fromList
   [ "let", "def", "DeclareRobustCommand"
   , "newcommand", "renewcommand"
   , "newenvironment", "renewenvironment"
   , "providecommand", "provideenvironment"
     -- newcommand, etc. should be parsed by macroDef, but we need this
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
   , "titleformat"
   ]

isInlineCommand :: Text -> Bool
isInlineCommand s =
  s `M.member` (inlineCommands :: M.Map Text (LP PandocPure Inlines))
  || s `Set.member` treatAsInline

treatAsInline :: Set.Set Text
treatAsInline = Set.fromList
  [ "index"
  , "hspace"
  , "vspace"
  , "noindent"
  , "newpage"
  , "clearpage"
  , "pagebreak"
  ]

dolabel :: PandocMonad m => LP m Inlines
dolabel = do
  v <- braced
  let refstr = toksToString v
  return $ spanWith (refstr,[],[("label", refstr)])
    $ inBrackets $ str $ toksToString v

doref :: PandocMonad m => String -> LP m Inlines
doref cls = do
  v <- braced
  let refstr = toksToString v
  return $ linkWith ("",[],[ ("reference-type", cls)
                           , ("reference", refstr)])
                    ('#':refstr)
                    ""
                    (inBrackets $ str refstr)

lookupListDefault :: (Show k, Ord k) => v -> [k] -> M.Map k v -> v
lookupListDefault d = (fromMaybe d .) . lookupList
  where lookupList l m = msum $ map (`M.lookup` m) l

inline :: PandocMonad m => LP m Inlines
inline = (mempty <$ comment)
     <|> (space  <$ whitespace)
     <|> (softbreak <$ endline)
     <|> word
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
     <|> regularSymbol
     <|> (do res <- symbolIn "#^'`\"[]"
             pos <- getPosition
             let s = T.unpack (untoken res)
             report $ ParsingUnescaped s pos
             return $ str s)

inlines :: PandocMonad m => LP m Inlines
inlines = mconcat <$> many inline

-- block elements:

begin_ :: PandocMonad m => Text -> LP m ()
begin_ t = try (do
  controlSeq "begin"
  spaces
  txt <- untokenize <$> braced
  guard (t == txt)) <?> ("\\begin{" ++ T.unpack t ++ "}")

end_ :: PandocMonad m => Text -> LP m ()
end_ t = try (do
  controlSeq "end"
  spaces
  txt <- untokenize <$> braced
  guard $ t == txt) <?> ("\\end{" ++ T.unpack t ++ "}")

preamble :: PandocMonad m => LP m Blocks
preamble = mempty <$ many preambleBlock
  where preambleBlock =  spaces1
                     <|> void include
                     <|> void macroDef
                     <|> void blockCommand
                     <|> void braced
                     <|> (notFollowedBy (begin_ "document") >> void anyTok)

paragraph :: PandocMonad m => LP m Blocks
paragraph = do
  x <- trimInlines . mconcat <$> many1 inline
  if x == mempty
     then return mempty
     else return $ para x

include :: PandocMonad m => LP m Blocks
include = do
  (Tok _ (CtrlSeq name) _) <-
                    controlSeq "include" <|> controlSeq "input" <|>
                    controlSeq "subfile" <|> controlSeq "usepackage"
  skipMany opt
  fs <- (map (T.unpack . removeDoubleQuotes . T.strip) . T.splitOn "," .
         untokenize) <$> braced
  let fs' = if name == "usepackage"
               then map (maybeAddExtension ".sty") fs
               else map (maybeAddExtension ".tex") fs
  dirs <- (splitBy (==':') . fromMaybe ".") <$> lookupEnv "TEXINPUTS"
  mapM_ (insertIncluded dirs) fs'
  return mempty

insertIncluded :: PandocMonad m
               => [FilePath]
               -> FilePath
               -> LP m ()
insertIncluded dirs f = do
  pos <- getPosition
  containers <- getIncludeFiles <$> getState
  when (f `elem` containers) $
    throwError $ PandocParseError $ "Include file loop at " ++ show pos
  updateState $ addIncludeFile f
  mbcontents <- readFileFromDirs dirs f
  contents <- case mbcontents of
                   Just s -> return s
                   Nothing -> do
                     report $ CouldNotLoadIncludeFile f pos
                     return ""
  getInput >>= setInput . (tokenize f (T.pack contents) ++)
  updateState dropLatestIncludeFile

maybeAddExtension :: String -> FilePath -> FilePath
maybeAddExtension ext fp =
  if null (takeExtension fp)
     then addExtension fp ext
     else fp

addMeta :: PandocMonad m => ToMetaValue a => String -> a -> LP m ()
addMeta field val = updateState $ \st ->
   st{ sMeta = addMetaField field val $ sMeta st }

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

macroDef :: PandocMonad m => LP m Blocks
macroDef =
  mempty <$ ((commandDef <|> environmentDef) <* doMacros 0)
  where commandDef = do
          (name, macro') <- newcommand <|> letmacro <|> defmacro
          guardDisabled Ext_latex_macros <|>
           updateState (\s -> s{ sMacros = M.insert name macro' (sMacros s) })
        environmentDef = do
          (name, macro1, macro2) <- newenvironment
          guardDisabled Ext_latex_macros <|>
            do updateState $ \s -> s{ sMacros =
                M.insert name macro1 (sMacros s) }
               updateState $ \s -> s{ sMacros =
                M.insert ("end" <> name) macro2 (sMacros s) }
        -- @\newenvironment{envname}[n-args][default]{begin}{end}@
        -- is equivalent to
        -- @\newcommand{\envname}[n-args][default]{begin}@
        -- @\newcommand{\endenvname}@

letmacro :: PandocMonad m => LP m (Text, Macro)
letmacro = do
  controlSeq "let"
  Tok _ (CtrlSeq name) _ <- anyControlSeq
  optional $ symbol '='
  spaces
  contents <- bracedOrToken
  return (name, Macro ExpandWhenDefined 0 Nothing contents)

defmacro :: PandocMonad m => LP m (Text, Macro)
defmacro = try $ do
  controlSeq "def"
  Tok _ (CtrlSeq name) _ <- anyControlSeq
  numargs <- option 0 $ argSeq 1
  -- we use withVerbatimMode, because macros are to be expanded
  -- at point of use, not point of definition
  contents <- withVerbatimMode bracedOrToken
  return (name, Macro ExpandWhenUsed numargs Nothing contents)

-- Note: we don't yet support fancy things like #1.#2
argSeq :: PandocMonad m => Int -> LP m Int
argSeq n = do
  Tok _ (Arg i) _ <- satisfyTok isArgTok
  guard $ i == n
  argSeq (n+1) <|> return n

isArgTok :: Tok -> Bool
isArgTok (Tok _ (Arg _) _) = True
isArgTok _                 = False

bracedOrToken :: PandocMonad m => LP m [Tok]
bracedOrToken = braced <|> ((:[]) <$> (anyControlSeq <|> singleChar))

newcommand :: PandocMonad m => LP m (Text, Macro)
newcommand = do
  pos <- getPosition
  Tok _ (CtrlSeq mtype) _ <- controlSeq "newcommand" <|>
                             controlSeq "renewcommand" <|>
                             controlSeq "providecommand" <|>
                             controlSeq "DeclareRobustCommand"
  optional $ symbol '*'
  Tok _ (CtrlSeq name) txt <- withVerbatimMode $ anyControlSeq <|>
    (symbol '{' *> spaces *> anyControlSeq <* spaces <* symbol '}')
  spaces
  numargs <- option 0 $ try bracketedNum
  spaces
  optarg <- option Nothing $ Just <$> try bracketedToks
  spaces
  contents <- withVerbatimMode bracedOrToken
  when (mtype == "newcommand") $ do
    macros <- sMacros <$> getState
    case M.lookup name macros of
         Just _  -> report $ MacroAlreadyDefined (T.unpack txt) pos
         Nothing -> return ()
  return (name, Macro ExpandWhenUsed numargs optarg contents)

newenvironment :: PandocMonad m => LP m (Text, Macro, Macro)
newenvironment = do
  pos <- getPosition
  Tok _ (CtrlSeq mtype) _ <- controlSeq "newenvironment" <|>
                             controlSeq "renewenvironment" <|>
                             controlSeq "provideenvironment"
  optional $ symbol '*'
  spaces
  name <- untokenize <$> braced
  spaces
  numargs <- option 0 $ try bracketedNum
  spaces
  optarg <- option Nothing $ Just <$> try bracketedToks
  spaces
  startcontents <- withVerbatimMode bracedOrToken
  spaces
  endcontents <- withVerbatimMode bracedOrToken
  when (mtype == "newenvironment") $ do
    macros <- sMacros <$> getState
    case M.lookup name macros of
         Just _  -> report $ MacroAlreadyDefined (T.unpack name) pos
         Nothing -> return ()
  return (name, Macro ExpandWhenUsed numargs optarg startcontents,
             Macro ExpandWhenUsed 0 Nothing endcontents)

bracketedToks :: PandocMonad m => LP m [Tok]
bracketedToks = do
  symbol '['
  mconcat <$> manyTill (braced <|> (:[]) <$> anyTok) (symbol ']')

bracketedNum :: PandocMonad m => LP m Int
bracketedNum = do
  ds <- untokenize <$> bracketedToks
  case safeRead (T.unpack ds) of
       Just i -> return i
       _      -> return 0

setCaption :: PandocMonad m => LP m Blocks
setCaption = do
  ils <- tok
  mblabel <- option Nothing $
               try $ spaces >> controlSeq "label" >> (Just <$> tok)
  let ils' = case mblabel of
                  Just lab -> ils <> spanWith
                                ("",[],[("label", stringify lab)]) mempty
                  Nothing  -> ils
  updateState $ \st -> st{ sCaption = Just ils' }
  return mempty

looseItem :: PandocMonad m => LP m Blocks
looseItem = do
  inListItem <- sInListItem <$> getState
  guard $ not inListItem
  skipopts
  return mempty

resetCaption :: PandocMonad m => LP m ()
resetCaption = updateState $ \st -> st{ sCaption = Nothing }

section :: PandocMonad m => Bool -> Attr -> Int -> LP m Blocks
section starred (ident, classes, kvs) lvl = do
  skipopts
  contents <- grouped inline
  lab <- option ident $
          try (spaces >> controlSeq "label"
               >> spaces >> toksToString <$> braced)
  let classes' = if starred then "unnumbered" : classes else classes
  unless starred $ do
    hn <- sLastHeaderNum <$> getState
    let num = incrementHeaderNum lvl hn
    updateState $ \st -> st{ sLastHeaderNum = num }
    updateState $ \st -> st{ sLabels = M.insert lab
                            [Str (renderHeaderNum num)]
                            (sLabels st) }
  attr' <- registerHeader (lab, classes', kvs) contents
  return $ headerWith attr' lvl contents

blockCommand :: PandocMonad m => LP m Blocks
blockCommand = try $ do
  Tok _ (CtrlSeq name) txt <- anyControlSeq
  guard $ name /= "begin" && name /= "end"
  star <- option "" ("*" <$ symbol '*' <* optional sp)
  let name' = name <> star
  let names = ordNub [name', name]
  let rawDefiniteBlock = do
        guard $ isBlockCommand name
        rawBlock "latex" <$> getRawCommand name (txt <> star)
  -- heuristic:  if it could be either block or inline, we
  -- treat it if block if we have a sequence of block
  -- commands followed by a newline.  But we stop if we
  -- hit a \startXXX, since this might start a raw ConTeXt
  -- environment (this is important because this parser is
  -- used by the Markdown reader).
  let startCommand = try $ do
        Tok _ (CtrlSeq n) _ <- anyControlSeq
        guard $ "start" `T.isPrefixOf` n
  let rawMaybeBlock = try $ do
        guard $ not $ isInlineCommand name
        curr <- rawBlock "latex" <$> getRawCommand name (txt <> star)
        rest <- many $ notFollowedBy startCommand *> blockCommand
        lookAhead $ blankline <|> startCommand
        return $ curr <> mconcat rest
  let raw = rawDefiniteBlock <|> rawMaybeBlock
  lookupListDefault raw names blockCommands

closing :: PandocMonad m => LP m Blocks
closing = do
  contents <- tok
  st <- getState
  let extractInlines (MetaBlocks [Plain ys]) = ys
      extractInlines (MetaBlocks [Para ys ]) = ys
      extractInlines _                       = []
  let sigs = case lookupMeta "author" (sMeta st) of
                  Just (MetaList xs) ->
                    para $ trimInlines $ fromList $
                      intercalate [LineBreak] $ map extractInlines xs
                  _ -> mempty
  return $ para (trimInlines contents) <> sigs

blockCommands :: PandocMonad m => M.Map Text (LP m Blocks)
blockCommands = M.fromList $
   [ ("par", mempty <$ skipopts)
   , ("parbox",  skipopts >> braced >> grouped blocks)
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
   , ("part", section False nullAttr (-1))
   , ("part*", section True nullAttr (-1))
   , ("chapter", section False nullAttr 0)
   , ("chapter*", section True ("",["unnumbered"],[]) 0)
   , ("section", section False nullAttr 1)
   , ("section*", section True ("",["unnumbered"],[]) 1)
   , ("subsection", section False nullAttr 2)
   , ("subsection*", section True ("",["unnumbered"],[]) 2)
   , ("subsubsection", section False nullAttr 3)
   , ("subsubsection*", section True ("",["unnumbered"],[]) 3)
   , ("paragraph", section False nullAttr 4)
   , ("paragraph*", section True ("",["unnumbered"],[]) 4)
   , ("subparagraph", section False nullAttr 5)
   , ("subparagraph*", section True ("",["unnumbered"],[]) 5)
   -- beamer slides
   , ("frametitle", section False nullAttr 3)
   , ("framesubtitle", section False nullAttr 4)
   -- letters
   , ("opening", (para . trimInlines) <$> (skipopts *> tok))
   , ("closing", skipopts *> closing)
   -- memoir
   , ("plainbreak", braced >> pure horizontalRule)
   , ("plainbreak*", braced >> pure horizontalRule)
   , ("fancybreak", braced >> pure horizontalRule)
   , ("fancybreak*", braced >> pure horizontalRule)
   , ("plainfancybreak", braced >> braced >> braced >> pure horizontalRule)
   , ("plainfancybreak*", braced >> braced >> braced >> pure horizontalRule)
   , ("pfbreak", pure horizontalRule)
   , ("pfbreak*", pure horizontalRule)
   --
   , ("hrule", pure horizontalRule)
   , ("strut", pure mempty)
   , ("rule", skipopts *> tok *> tok *> pure horizontalRule)
   , ("item", looseItem)
   , ("documentclass", skipopts *> braced *> preamble)
   , ("centerline", (para . trimInlines) <$> (skipopts *> tok))
   , ("caption", skipopts *> setCaption)
   , ("bibliography", mempty <$ (skipopts *> braced >>=
         addMeta "bibliography" . splitBibs . toksToString))
   , ("addbibresource", mempty <$ (skipopts *> braced >>=
         addMeta "bibliography" . splitBibs . toksToString))
   -- includes
   , ("lstinputlisting", inputListing)
   , ("graphicspath", graphicsPath)
   -- polyglossia
   , ("setdefaultlanguage", setDefaultLanguage)
   , ("setmainlanguage", setDefaultLanguage)
   -- hyperlink
   , ("hypertarget", hypertargetBlock)
   -- LaTeX colors
   , ("textcolor", coloredBlock "color")
   , ("colorbox", coloredBlock "background-color")
   ]


environments :: PandocMonad m => M.Map Text (LP m Blocks)
environments = M.fromList
   [ ("document", env "document" blocks)
   , ("abstract", mempty <$ (env "abstract" blocks >>= addMeta "abstract"))
   , ("letter", env "letter" letterContents)
   , ("minipage", env "minipage" $
          skipopts *> spaces *> optional braced *> spaces *> blocks)
   , ("figure", env "figure" $ skipopts *> figure)
   , ("subfigure", env "subfigure" $ skipopts *> tok *> figure)
   , ("center", env "center" blocks)
   , ("longtable",  env "longtable" $
          resetCaption *> simpTable "longtable" False >>= addTableCaption)
   , ("table",  env "table" $
          resetCaption *> skipopts *> blocks >>= addTableCaption)
   , ("tabular*", env "tabular" $ simpTable "tabular*" True)
   , ("tabularx", env "tabularx" $ simpTable "tabularx" True)
   , ("tabular", env "tabular"  $ simpTable "tabular" False)
   , ("quote", blockQuote <$> env "quote" blocks)
   , ("quotation", blockQuote <$> env "quotation" blocks)
   , ("verse", blockQuote <$> env "verse" blocks)
   , ("itemize", bulletList <$> listenv "itemize" (many item))
   , ("description", definitionList <$> listenv "description" (many descItem))
   , ("enumerate", orderedList')
   , ("alltt", alltt <$> env "alltt" blocks)
   , ("code", guardEnabled Ext_literate_haskell *>
       (codeBlockWith ("",["sourceCode","literate","haskell"],[]) <$>
         verbEnv "code"))
   , ("comment", mempty <$ verbEnv "comment")
   , ("verbatim", codeBlock <$> verbEnv "verbatim")
   , ("Verbatim", fancyverbEnv "Verbatim")
   , ("BVerbatim", fancyverbEnv "BVerbatim")
   , ("lstlisting", do attr <- parseListingsOptions <$> option [] keyvals
                       codeBlockWith attr <$> verbEnv "lstlisting")
   , ("minted", minted)
   , ("obeylines", obeylines)
   , ("displaymath", mathEnvWith para Nothing "displaymath")
   , ("equation", mathEnvWith para Nothing "equation")
   , ("equation*", mathEnvWith para Nothing "equation*")
   , ("gather", mathEnvWith para (Just "gathered") "gather")
   , ("gather*", mathEnvWith para (Just "gathered") "gather*")
   , ("multline", mathEnvWith para (Just "gathered") "multline")
   , ("multline*", mathEnvWith para (Just "gathered") "multline*")
   , ("eqnarray", mathEnvWith para (Just "aligned") "eqnarray")
   , ("eqnarray*", mathEnvWith para (Just "aligned") "eqnarray*")
   , ("align", mathEnvWith para (Just "aligned") "align")
   , ("align*", mathEnvWith para (Just "aligned") "align*")
   , ("alignat", mathEnvWith para (Just "aligned") "alignat")
   , ("alignat*", mathEnvWith para (Just "aligned") "alignat*")
   , ("tikzpicture", rawVerbEnv "tikzpicture")
   -- etoolbox
   , ("ifstrequal", ifstrequal)
   , ("newtoggle", braced >>= newToggle)
   , ("toggletrue", braced >>= setToggle True)
   , ("togglefalse", braced >>= setToggle False)
   , ("iftoggle", try $ ifToggle >> block)
   ]

environment :: PandocMonad m => LP m Blocks
environment = do
  controlSeq "begin"
  name <- untokenize <$> braced
  M.findWithDefault mzero name environments
    <|> rawEnv name

env :: PandocMonad m => Text -> LP m a -> LP m a
env name p = p <* end_ name

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
       report $ SkippedContent (T.unpack beginCommand) pos1
       pos2 <- getPosition
       report $ SkippedContent ("\\end{" ++ T.unpack name ++ "}") pos2
       return bs

rawVerbEnv :: PandocMonad m => Text -> LP m Blocks
rawVerbEnv name = do
  pos <- getPosition
  (_, raw) <- withRaw $ verbEnv name
  let raw' = "\\begin{tikzpicture}" ++ toksToString raw
  exts <- getOption readerExtensions
  let parseRaw = extensionEnabled Ext_raw_tex exts
  if parseRaw
     then return $ rawBlock "latex" raw'
     else do
       report $ SkippedContent raw' pos
       return mempty

verbEnv :: PandocMonad m => Text -> LP m String
verbEnv name = withVerbatimMode $ do
  skipopts
  optional blankline
  res <- manyTill anyTok (end_ name)
  return $ stripTrailingNewlines $ toksToString res

fancyverbEnv :: PandocMonad m => Text -> LP m Blocks
fancyverbEnv name = do
  options <- option [] keyvals
  let kvs = [ (if k == "firstnumber"
                  then "startFrom"
                  else k, v) | (k,v) <- options ]
  let classes = [ "numberLines" |
                  lookup "numbers" options == Just "left" ]
  let attr = ("",classes,kvs)
  codeBlockWith attr <$> verbEnv name

obeylines :: PandocMonad m => LP m Blocks
obeylines =
  para . fromList . removeLeadingTrailingBreaks .
   walk softBreakToHard . toList <$> env "obeylines" inlines
  where softBreakToHard SoftBreak = LineBreak
        softBreakToHard x         = x
        removeLeadingTrailingBreaks = reverse . dropWhile isLineBreak .
                                      reverse . dropWhile isLineBreak
        isLineBreak LineBreak = True
        isLineBreak _         = False

minted :: PandocMonad m => LP m Blocks
minted = do
  options <- option [] keyvals
  lang <- toksToString <$> braced
  let kvs = [ (if k == "firstnumber"
                  then "startFrom"
                  else k, v) | (k,v) <- options ]
  let classes = [ lang | not (null lang) ] ++
                [ "numberLines" |
                  lookup "linenos" options == Just "true" ]
  let attr = ("",classes,kvs)
  codeBlockWith attr <$> verbEnv "minted"

letterContents :: PandocMonad m => LP m Blocks
letterContents = do
  bs <- blocks
  st <- getState
  -- add signature (author) and address (title)
  let addr = case lookupMeta "address" (sMeta st) of
                  Just (MetaBlocks [Plain xs]) ->
                     para $ trimInlines $ fromList xs
                  _ -> mempty
  return $ addr <> bs -- sig added by \closing

figure :: PandocMonad m => LP m Blocks
figure = try $ do
  resetCaption
  blocks >>= addImageCaption

addImageCaption :: PandocMonad m => Blocks -> LP m Blocks
addImageCaption = walkM go
  where go (Image attr alt (src,tit))
            | not ("fig:" `isPrefixOf` tit) = do
          mbcapt <- sCaption <$> getState
          return $ case mbcapt of
               Just ils -> Image attr (toList ils) (src, "fig:" ++ tit)
               Nothing  -> Image attr alt (src,tit)
        go x = return x

coloredBlock :: PandocMonad m => String -> LP m Blocks
coloredBlock stylename = try $ do
  skipopts
  color <- braced
  notFollowedBy (grouped inline)
  let constructor = divWith ("",[],[("style",stylename ++ ": " ++ toksToString color)])
  constructor <$> grouped block

graphicsPath :: PandocMonad m => LP m Blocks
graphicsPath = do
  ps <- map toksToString <$> (bgroup *> manyTill braced egroup)
  getResourcePath >>= setResourcePath . (++ ps)
  return mempty

splitBibs :: String -> [Inlines]
splitBibs = map (str . flip replaceExtension "bib" . trim) . splitBy (==',')

alltt :: Blocks -> Blocks
alltt = walk strToCode
  where strToCode (Str s)   = Code nullAttr s
        strToCode Space     = RawInline (Format "latex") "\\ "
        strToCode SoftBreak = LineBreak
        strToCode x         = x

parseListingsOptions :: [(String, String)] -> Attr
parseListingsOptions options =
  let kvs = [ (if k == "firstnumber"
                  then "startFrom"
                  else k, v) | (k,v) <- options ]
      classes = [ "numberLines" |
                  lookup "numbers" options == Just "left" ]
             ++ maybeToList (lookup "language" options
                     >>= fromListingsLanguage)
  in  (fromMaybe "" (lookup "label" options), classes, kvs)

inputListing :: PandocMonad m => LP m Blocks
inputListing = do
  pos <- getPosition
  options <- option [] keyvals
  f <- filter (/='"') . toksToString <$> braced
  dirs <- (splitBy (==':') . fromMaybe ".") <$> lookupEnv "TEXINPUTS"
  mbCode <- readFileFromDirs dirs f
  codeLines <- case mbCode of
                      Just s -> return $ lines s
                      Nothing -> do
                        report $ CouldNotLoadIncludeFile f pos
                        return []
  let (ident,classes,kvs) = parseListingsOptions options
  let language = case lookup "language" options >>= fromListingsLanguage of
                      Just l -> [l]
                      Nothing -> take 1 $ languagesByExtension (takeExtension f)
  let firstline = fromMaybe 1 $ lookup "firstline" options >>= safeRead
  let lastline = fromMaybe (length codeLines) $
                       lookup "lastline" options >>= safeRead
  let codeContents = intercalate "\n" $ take (1 + lastline - firstline) $
                       drop (firstline - 1) codeLines
  return $ codeBlockWith (ident,ordNub (classes ++ language),kvs) codeContents

-- lists

item :: PandocMonad m => LP m Blocks
item = void blocks *> controlSeq "item" *> skipopts *> blocks

descItem :: PandocMonad m => LP m (Inlines, [Blocks])
descItem = do
  blocks -- skip blocks before item
  controlSeq "item"
  optional sp
  ils <- opt
  bs <- blocks
  return (ils, [bs])

listenv :: PandocMonad m => Text -> LP m a -> LP m a
listenv name p = try $ do
  oldInListItem <- sInListItem `fmap` getState
  updateState $ \st -> st{ sInListItem = True }
  res <- env name p
  updateState $ \st -> st{ sInListItem = oldInListItem }
  return res

orderedList' :: PandocMonad m => LP m Blocks
orderedList' = try $ do
  spaces
  let markerSpec = do
        symbol '['
        ts <- toksToString <$> manyTill anyTok (symbol ']')
        case runParser anyOrderedListMarker def "option" ts of
             Right r -> return r
             Left _  -> do
               pos <- getPosition
               report $ SkippedContent ("[" ++ ts ++ "]") pos
               return (1, DefaultStyle, DefaultDelim)
  (_, style, delim) <- option (1, DefaultStyle, DefaultDelim) markerSpec
  spaces
  optional $ try $ controlSeq "setlength"
                   *> grouped (count 1 $ controlSeq "itemindent")
                   *> braced
  spaces
  start <- option 1 $ try $ do pos <- getPosition
                               controlSeq "setcounter"
                               ctr <- toksToString <$> braced
                               guard $ "enum" `isPrefixOf` ctr
                               guard $ all (`elem` ['i','v']) (drop 4 ctr)
                               optional sp
                               num <- toksToString <$> braced
                               case safeRead num of
                                    Just i -> return (i + 1 :: Int)
                                    Nothing -> do
                                      report $ SkippedContent
                                        ("\\setcounter{" ++ ctr ++
                                         "}{" ++ num ++ "}") pos
                                      return 1
  bs <- listenv "enumerate" (many item)
  return $ orderedListWith (start, style, delim) bs

-- tables

hline :: PandocMonad m => LP m ()
hline = try $ do
  spaces
  controlSeq "hline" <|>
    -- booktabs rules:
    controlSeq "toprule" <|>
    controlSeq "bottomrule" <|>
    controlSeq "midrule" <|>
    controlSeq "endhead" <|>
    controlSeq "endfirsthead"
  spaces
  optional opt
  return ()

lbreak :: PandocMonad m => LP m Tok
lbreak = (controlSeq "\\" <|> controlSeq "tabularnewline") <* spaces

amp :: PandocMonad m => LP m Tok
amp = symbol '&'

-- Split a Word into individual Symbols (for parseAligns)
splitWordTok :: PandocMonad m => LP m ()
splitWordTok = do
  inp <- getInput
  case inp of
       (Tok spos Word t : rest) ->
         setInput $ map (Tok spos Symbol . T.singleton) (T.unpack t) ++ rest
       _ -> return ()

parseAligns :: PandocMonad m => LP m [(Alignment, Double, ([Tok], [Tok]))]
parseAligns = try $ do
  let maybeBar = skipMany $
        sp <|> () <$ symbol '|' <|> () <$ (symbol '@' >> braced)
  let cAlign = AlignCenter <$ symbol 'c'
  let lAlign = AlignLeft <$ symbol 'l'
  let rAlign = AlignRight <$ symbol 'r'
  let parAlign = AlignLeft <$ symbol 'p'
  -- aligns from tabularx
  let xAlign = AlignLeft <$ symbol 'X'
  let mAlign = AlignLeft <$ symbol 'm'
  let bAlign = AlignLeft <$ symbol 'b'
  let alignChar = splitWordTok *> (  cAlign <|> lAlign <|> rAlign <|> parAlign
                                 <|> xAlign <|> mAlign <|> bAlign )
  let alignPrefix = symbol '>' >> braced
  let alignSuffix = symbol '<' >> braced
  let colWidth = try $ do
        symbol '{'
        ds <- trim . toksToString <$> manyTill anyTok (controlSeq "linewidth")
        spaces
        symbol '}'
        case safeRead ds of
              Just w  -> return w
              Nothing -> return 0.0
  let alignSpec = do
        pref <- option [] alignPrefix
        spaces
        al <- alignChar
        width <- colWidth <|> option 0.0 (do s <- toksToString <$> braced
                                             pos <- getPosition
                                             report $ SkippedContent s pos
                                             return 0.0)
        spaces
        suff <- option [] alignSuffix
        return (al, width, (pref, suff))
  let starAlign = do -- '*{2}{r}' == 'rr', we just expand like a macro
        symbol '*'
        spaces
        ds <- trim . toksToString <$> braced
        spaces
        spec <- braced
        case safeRead ds of
             Just n  -> do
               getInput >>= setInput . (mconcat (replicate n spec) ++)
             Nothing -> fail $ "Could not parse " ++ ds ++ " as number"
  bgroup
  spaces
  maybeBar
  aligns' <- many $ try $ spaces >> optional starAlign >>
                            (alignSpec <* maybeBar)
  spaces
  egroup
  spaces
  return aligns'

parseTableRow :: PandocMonad m
              => Text   -- ^ table environment name
              -> [([Tok], [Tok])] -- ^ pref/suffixes
              -> LP m [Blocks]
parseTableRow envname prefsufs = do
  notFollowedBy (spaces *> end_ envname)
  let cols = length prefsufs
  -- add prefixes and suffixes in token stream:
  let celltoks (pref, suff) = do
        prefpos <- getPosition
        contents <- many (notFollowedBy
                         (() <$ amp <|> () <$ lbreak <|> end_ envname)
                         >> anyTok)
        suffpos <- getPosition
        option [] (count 1 amp)
        return $ map (setpos prefpos) pref ++ contents ++ map (setpos suffpos) suff
  rawcells <- mapM celltoks prefsufs
  oldInput <- getInput
  cells <- mapM (\ts -> setInput ts >> parseTableCell) rawcells
  setInput oldInput
  spaces
  let numcells = length cells
  guard $ numcells <= cols && numcells >= 1
  guard $ cells /= [mempty]
  -- note:  a & b in a three-column table leaves an empty 3rd cell:
  return $ cells ++ replicate (cols - numcells) mempty

parseTableCell :: PandocMonad m => LP m Blocks
parseTableCell = do
  let plainify bs = case toList bs of
                         [Para ils] -> plain (fromList ils)
                         _          -> bs
  updateState $ \st -> st{ sInTableCell = True }
  cells <- plainify <$> blocks
  updateState $ \st -> st{ sInTableCell = False }
  return cells

simpTable :: PandocMonad m => Text -> Bool -> LP m Blocks
simpTable envname hasWidthParameter = try $ do
  when hasWidthParameter $ () <$ (spaces >> tok)
  skipopts
  colspecs <- parseAligns
  let (aligns, widths, prefsufs) = unzip3 colspecs
  let cols = length colspecs
  optional $ controlSeq "caption" *> skipopts *> setCaption
  optional lbreak
  spaces
  skipMany hline
  spaces
  header' <- option [] $ try (parseTableRow envname prefsufs <*
                                   lbreak <* many1 hline)
  spaces
  rows <- sepEndBy (parseTableRow envname prefsufs)
                    (lbreak <* optional (skipMany hline))
  spaces
  optional $ controlSeq "caption" *> skipopts *> setCaption
  optional lbreak
  spaces
  let header'' = if null header'
                    then replicate cols mempty
                    else header'
  lookAhead $ controlSeq "end" -- make sure we're at end
  return $ table mempty (zip aligns widths) header'' rows

addTableCaption :: PandocMonad m => Blocks -> LP m Blocks
addTableCaption = walkM go
  where go (Table c als ws hs rs) = do
          mbcapt <- sCaption <$> getState
          return $ case mbcapt of
               Just ils -> Table (toList ils) als ws hs rs
               Nothing  -> Table c als ws hs rs
        go x = return x


block :: PandocMonad m => LP m Blocks
block = (mempty <$ spaces1)
    <|> environment
    <|> include
    <|> macroDef
    <|> blockCommand
    <|> paragraph
    <|> grouped block

blocks :: PandocMonad m => LP m Blocks
blocks = mconcat <$> many block

setDefaultLanguage :: PandocMonad m => LP m Blocks
setDefaultLanguage = do
  o <- option "" $ (T.unpack . T.filter (\c -> c /= '[' && c /= ']'))
                <$> rawopt
  polylang <- toksToString <$> braced
  case M.lookup polylang polyglossiaLangToBCP47 of
       Nothing -> return mempty -- TODO mzero? warning?
       Just langFunc -> do
         let l = langFunc o
         setTranslations l
         updateState $ setMeta "lang" $ str (renderLang l)
         return mempty

polyglossiaLangToBCP47 :: M.Map String (String -> Lang)
polyglossiaLangToBCP47 = M.fromList
  [ ("arabic", \o -> case filter (/=' ') o of
       "locale=algeria"    -> Lang "ar" "" "DZ" []
       "locale=mashriq"    -> Lang "ar" "" "SY" []
       "locale=libya"      -> Lang "ar" "" "LY" []
       "locale=morocco"    -> Lang "ar" "" "MA" []
       "locale=mauritania" -> Lang "ar" "" "MR" []
       "locale=tunisia"    -> Lang "ar" "" "TN" []
       _                   -> Lang "ar" "" "" [])
  , ("german", \o -> case filter (/=' ') o of
       "spelling=old" -> Lang "de" "" "DE" ["1901"]
       "variant=austrian,spelling=old"
                       -> Lang "de" "" "AT" ["1901"]
       "variant=austrian" -> Lang "de" "" "AT" []
       "variant=swiss,spelling=old"
                       -> Lang "de" "" "CH" ["1901"]
       "variant=swiss" -> Lang "de" "" "CH" []
       _ -> Lang "de" "" "" [])
  , ("lsorbian", \_ -> Lang "dsb" "" "" [])
  , ("greek", \o -> case filter (/=' ') o of
       "variant=poly"    -> Lang "el" "" "polyton" []
       "variant=ancient" -> Lang "grc" "" "" []
       _                 -> Lang "el" "" "" [])
  , ("english", \o -> case filter (/=' ') o of
       "variant=australian" -> Lang "en" "" "AU" []
       "variant=canadian"   -> Lang "en" "" "CA" []
       "variant=british"    -> Lang "en" "" "GB" []
       "variant=newzealand" -> Lang "en" "" "NZ" []
       "variant=american"   -> Lang "en" "" "US" []
       _                    -> Lang "en" "" "" [])
  , ("usorbian", \_ -> Lang "hsb" "" "" [])
  , ("latin", \o -> case filter (/=' ') o of
       "variant=classic" -> Lang "la" "" "" ["x-classic"]
       _                 -> Lang "la" "" "" [])
  , ("slovenian", \_ -> Lang "sl" "" "" [])
  , ("serbianc", \_ -> Lang "sr" "cyrl" "" [])
  , ("pinyin", \_ -> Lang "zh" "Latn" "" ["pinyin"])
  , ("afrikaans", \_ -> Lang "af" "" "" [])
  , ("amharic", \_ -> Lang "am" "" "" [])
  , ("assamese", \_ -> Lang "as" "" "" [])
  , ("asturian", \_ -> Lang "ast" "" "" [])
  , ("bulgarian", \_ -> Lang "bg" "" "" [])
  , ("bengali", \_ -> Lang "bn" "" "" [])
  , ("tibetan", \_ -> Lang "bo" "" "" [])
  , ("breton", \_ -> Lang "br" "" "" [])
  , ("catalan", \_ -> Lang "ca" "" "" [])
  , ("welsh", \_ -> Lang "cy" "" "" [])
  , ("czech", \_ -> Lang "cs" "" "" [])
  , ("coptic", \_ -> Lang "cop" "" "" [])
  , ("danish", \_ -> Lang "da" "" "" [])
  , ("divehi", \_ -> Lang "dv" "" "" [])
  , ("esperanto", \_ -> Lang "eo" "" "" [])
  , ("spanish", \_ -> Lang "es" "" "" [])
  , ("estonian", \_ -> Lang "et" "" "" [])
  , ("basque", \_ -> Lang "eu" "" "" [])
  , ("farsi", \_ -> Lang "fa" "" "" [])
  , ("finnish", \_ -> Lang "fi" "" "" [])
  , ("french", \_ -> Lang "fr" "" "" [])
  , ("friulan", \_ -> Lang "fur" "" "" [])
  , ("irish", \_ -> Lang "ga" "" "" [])
  , ("scottish", \_ -> Lang "gd" "" "" [])
  , ("ethiopic", \_ -> Lang "gez" "" "" [])
  , ("galician", \_ -> Lang "gl" "" "" [])
  , ("hebrew", \_ -> Lang "he" "" "" [])
  , ("hindi", \_ -> Lang "hi" "" "" [])
  , ("croatian", \_ -> Lang "hr" "" "" [])
  , ("magyar", \_ -> Lang "hu" "" "" [])
  , ("armenian", \_ -> Lang "hy" "" "" [])
  , ("interlingua", \_ -> Lang "ia" "" "" [])
  , ("indonesian", \_ -> Lang "id" "" "" [])
  , ("icelandic", \_ -> Lang "is" "" "" [])
  , ("italian", \_ -> Lang "it" "" "" [])
  , ("japanese", \_ -> Lang "jp" "" "" [])
  , ("khmer", \_ -> Lang "km" "" "" [])
  , ("kurmanji", \_ -> Lang "kmr" "" "" [])
  , ("kannada", \_ -> Lang "kn" "" "" [])
  , ("korean", \_ -> Lang "ko" "" "" [])
  , ("lao", \_ -> Lang "lo" "" "" [])
  , ("lithuanian", \_ -> Lang "lt" "" "" [])
  , ("latvian", \_ -> Lang "lv" "" "" [])
  , ("malayalam", \_ -> Lang "ml" "" "" [])
  , ("mongolian", \_ -> Lang "mn" "" "" [])
  , ("marathi", \_ -> Lang "mr" "" "" [])
  , ("dutch", \_ -> Lang "nl" "" "" [])
  , ("nynorsk", \_ -> Lang "nn" "" "" [])
  , ("norsk", \_ -> Lang "no" "" "" [])
  , ("nko", \_ -> Lang "nqo" "" "" [])
  , ("occitan", \_ -> Lang "oc" "" "" [])
  , ("panjabi", \_ -> Lang "pa" "" "" [])
  , ("polish", \_ -> Lang "pl" "" "" [])
  , ("piedmontese", \_ -> Lang "pms" "" "" [])
  , ("portuguese", \_ -> Lang "pt" "" "" [])
  , ("romansh", \_ -> Lang "rm" "" "" [])
  , ("romanian", \_ -> Lang "ro" "" "" [])
  , ("russian", \_ -> Lang "ru" "" "" [])
  , ("sanskrit", \_ -> Lang "sa" "" "" [])
  , ("samin", \_ -> Lang "se" "" "" [])
  , ("slovak", \_ -> Lang "sk" "" "" [])
  , ("albanian", \_ -> Lang "sq" "" "" [])
  , ("serbian", \_ -> Lang "sr" "" "" [])
  , ("swedish", \_ -> Lang "sv" "" "" [])
  , ("syriac", \_ -> Lang "syr" "" "" [])
  , ("tamil", \_ -> Lang "ta" "" "" [])
  , ("telugu", \_ -> Lang "te" "" "" [])
  , ("thai", \_ -> Lang "th" "" "" [])
  , ("turkmen", \_ -> Lang "tk" "" "" [])
  , ("turkish", \_ -> Lang "tr" "" "" [])
  , ("ukrainian", \_ -> Lang "uk" "" "" [])
  , ("urdu", \_ -> Lang "ur" "" "" [])
  , ("vietnamese", \_ -> Lang "vi" "" "" [])
  ]
