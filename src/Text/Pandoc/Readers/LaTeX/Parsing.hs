{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{- |
   Module      : Text.Pandoc.Readers.LaTeX.Parsing
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

General parsing types and functions for LaTeX.
-}
module Text.Pandoc.Readers.LaTeX.Parsing
  ( DottedNum(..)
  , renderDottedNum
  , incrementDottedNum
  , TheoremSpec(..)
  , TheoremStyle(..)
  , LaTeXState(..)
  , defaultLaTeXState
  , LP
  , LaTeXEnv(..)
  , emptyLaTeXEnv
  , askEnv
  , TokStream(..)
  , withVerbatimMode
  , rawLaTeXParser
  , applyMacros
  , tokenize
  , tokenizeSources
  , getInputTokens
  , untokenize
  , untoken
  , satisfyTok
  , peekTok
  , parseFromToks
  , disablingWithRaw
  , doMacros
  , doMacros'
  , setpos
  , anyControlSeq
  , anySymbol
  , isNewlineTok
  , isWordTok
  , isArgTok
  , infile
  , spaces
  , spaces1
  , tokTypeIn
  , controlSeq
  , symbol
  , symbolIn
  , sp
  , whitespace
  , newlineTok
  , comment
  , anyTok
  , singleChar
  , tokWith
  , specialChars
  , endline
  , blankline
  , primEscape
  , bgroup
  , egroup
  , grouped
  , braced
  , braced'
  , bracedUrl
  , bracedOrToken
  , bracketed
  , bracketedToks
  , verbTok
  , parenWrapped
  , dimenarg
  , ignore
  , withRaw
  , keyvals
  , verbEnv
  , begin_
  , end_
  , getRawCommand
  , skipopts
  , rawopt
  , overlaySpecification
  , getNextNumber
  , label
  , setCaption
  , resetCaption
  , env
  , addMeta
  , removeLabel
  ) where

import Control.Applicative (many, (<|>))
import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import Data.Char (chr, isAlphaNum, isDigit, isLetter, ord)
import Data.Default
import Data.List (dropWhileEnd, intercalate, isSuffixOf, unfoldr)
import Numeric (showEFloat, showFFloat)
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import Text.Pandoc.Builder
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Error
         (PandocError (PandocMacroLoop))
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (blankline, many, mathDisplay, mathInline,
                            space, spaces, withRaw, (<|>))
import Text.Pandoc.TeX (ExpansionPoint (..), Macro (..),
                                        ArgSpec (..), Tok (..), TokType (..))
import Text.Pandoc.Shared
import Text.Pandoc.Walk

newtype DottedNum = DottedNum [Int]
  deriving (Show, Eq)

renderDottedNum :: DottedNum -> T.Text
renderDottedNum (DottedNum xs) = T.pack $
  intercalate "." (map show xs)

incrementDottedNum :: Int -> DottedNum -> DottedNum
incrementDottedNum level (DottedNum ns) = DottedNum $
  case reverse (take level (ns ++ repeat 0)) of
       (x:xs) -> reverse (x+1 : xs)
       []     -> []  -- shouldn't happen

data TheoremStyle =
  PlainStyle | DefinitionStyle | RemarkStyle
  deriving (Show, Eq)

data TheoremSpec =
  TheoremSpec
    { theoremName    :: [Tok]
    , theoremStyle   :: TheoremStyle
    , theoremSeries  :: Maybe Text
    , theoremSyncTo  :: Maybe Text
    , theoremNumber  :: Bool
    , theoremLastNum :: DottedNum }
    deriving (Show, Eq)

data LaTeXState = LaTeXState{ sOptions       :: ReaderOptions
                            , sMeta          :: Meta
                            , sQuoteContext  :: QuoteContext
                            , sMacros        :: NonEmpty (M.Map Text Macro)
                            , sContainers    :: [Text]
                            , sLogMessages   :: [LogMessage]
                            , sIdentifiers   :: Set.Set Text
                            , sVerbatimMode  :: Bool
                            , sMathMode      :: Bool
                            , sCaption       :: Maybe Caption
                            , sInListItem    :: Bool
                            , sInTableCell   :: Bool
                            , sLastHeaderNum :: DottedNum
                            , sLastFigureNum :: DottedNum
                            , sLastTableNum  :: DottedNum
                            , sLastNoteNum   :: Int
                            , sFootnoteTexts :: M.Map Int Blocks
                            , sTheoremMap    :: M.Map Text TheoremSpec
                            , sLastTheoremStyle :: TheoremStyle
                            , sLastLabel     :: Maybe Text
                            , sLabels        :: M.Map Text [Inline]
                            , sHasChapters   :: Bool
                            , sToggles       :: M.Map Text Bool
                            , sFileContents  :: M.Map Text Text
                            , sEnableWithRaw :: Bool
                            , sRawTokens     :: [Tok]
                              -- ^ reversed list of tokens consumed
                              -- while at least one withRaw scope is
                              -- active
                            , sRawTokenCount :: !Int
                              -- ^ length of sRawTokens
                            , sRawScopes     :: !Int
                              -- ^ number of active withRaw scopes
                            , sLigatures     :: Bool
                            , sCaseExclusions :: M.Map Text (Set.Set Text)
                              -- ^ words excluded from case changing
                              -- (keys: @upper@, @lower@, @title@)
                            , sLastExpansion :: Maybe (Text, SourcePos, Int)
                              -- ^ name and position of the last macro
                              -- expanded, and the number of consecutive
                              -- times it has been expanded there
                              -- (used to detect expansion loops)
                            }
     deriving Show

defaultLaTeXState :: LaTeXState
defaultLaTeXState = LaTeXState{ sOptions       = def
                              , sMeta          = nullMeta
                              , sQuoteContext  = NoQuote
                              , sMacros        = M.empty :| []
                              , sContainers    = []
                              , sLogMessages   = []
                              , sIdentifiers   = Set.empty
                              , sVerbatimMode  = False
                              , sMathMode      = False
                              , sCaption       = Nothing
                              , sInListItem    = False
                              , sInTableCell   = False
                              , sLastHeaderNum = DottedNum []
                              , sLastFigureNum = DottedNum []
                              , sLastTableNum  = DottedNum []
                              , sLastNoteNum   = 0
                              , sFootnoteTexts = M.empty
                              , sTheoremMap    = M.empty
                              , sLastTheoremStyle = PlainStyle
                              , sLastLabel     = Nothing
                              , sLabels        = M.empty
                              , sHasChapters   = False
                              , sToggles       = M.empty
                              , sFileContents  = M.empty
                              , sEnableWithRaw = True
                              , sRawTokens     = []
                              , sRawTokenCount = 0
                              , sRawScopes     = 0
                              , sLigatures     = True
                              , sCaseExclusions = M.empty
                              , sLastExpansion = Nothing
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

instance HasMacros LaTeXState where
  extractMacros  st  = NonEmpty.head $ sMacros st
  updateMacros f st  = st{ sMacros = f (NonEmpty.head (sMacros st))
                                     :| NonEmpty.tail (sMacros st) }

instance HasReaderOptions LaTeXState where
  extractReaderOptions = sOptions

instance HasMeta LaTeXState where
  setMeta field val st =
    st{ sMeta = setMeta field val $ sMeta st }
  deleteMeta field st =
    st{ sMeta = deleteMeta field $ sMeta st }

instance Default LaTeXState where
  def = defaultLaTeXState

-- The Boolean is True if macros have already been expanded,
-- False if they need expanding.
data TokStream = TokStream !Bool [Tok]
  deriving (Show)

instance Semigroup TokStream where
  (TokStream exp1 ts1) <> (TokStream exp2 ts2) =
    TokStream (if null ts1 then exp2 else exp1) (ts1 <> ts2)

instance Monoid TokStream where
  mempty = TokStream False mempty
  mappend = (<>)

instance Monad m => Stream TokStream m Tok where
  uncons (TokStream _ []) = return Nothing
  uncons (TokStream _ (t:ts)) = return $ Just (t, TokStream False ts)

type LP m = ParsecT TokStream LaTeXState (ReaderT (LaTeXEnv m) m)

-- | Environment holding the command dispatch tables.  Because these
-- tables have types that mention the parser monad, they cannot be
-- top-level constants; passing them in a reader environment ensures
-- they are constructed once per parse rather than once per use.
data LaTeXEnv m = LaTeXEnv
  { envInlineCommands :: M.Map Text (LP m Inlines)
  , envBlockCommands  :: M.Map Text (LP m Blocks)
  , envEnvironments   :: M.Map Text (LP m Blocks)
  }

-- | An environment with empty dispatch tables, for running parsers
-- that do not consult them.
emptyLaTeXEnv :: LaTeXEnv m
emptyLaTeXEnv = LaTeXEnv mempty mempty mempty

-- | Retrieve the command dispatch tables.
askEnv :: Monad m => LP m (LaTeXEnv m)
askEnv = lift ask

withVerbatimMode :: PandocMonad m => LP m a -> LP m a
withVerbatimMode parser = do
  alreadyVerbatimMode <- sVerbatimMode <$> getState
  if alreadyVerbatimMode
     then parser
     else do
       updateState $ \st -> st{ sVerbatimMode = True }
       result <- parser
       updateState $ \st -> st{ sVerbatimMode = False }
       return result

rawLaTeXParser :: (PandocMonad m, HasMacros s, HasReaderOptions s, Show a)
               => LaTeXEnv m -> [Tok] -> LP m () -> LP m a
               -> ParsecT Sources s m (a, Text)
rawLaTeXParser lenv toks parser valParser = do
  pstate <- getState
  let lstate = def{ sOptions = extractReaderOptions pstate }
  let lstate' = lstate { sMacros = extractMacros pstate :| [] }
  let setStartPos = case toks of
                      Tok pos _ _ : _ -> setPosition pos
                      _ -> return ()
  let preparser = setStartPos >> parser
  let rawparser = (,) <$> withRaw valParser <*> getState
  res' <- lift $ flip runReaderT lenv $
                 runParserT (withRaw (preparser >> getPosition))
                            lstate "chunk" $ TokStream False toks
  case res' of
       Left _    -> mzero
       Right (endpos, toks') -> do
         res <- lift $ flip runReaderT lenv $
                       runParserT rawparser lstate' "chunk"
                     $ TokStream False toks'
         case res of
              Left _    -> mzero
              Right ((val, raw), st) -> do
                updateState (updateMacros ((NonEmpty.head (sMacros st)) <>))
                let rawChar = do
                      pos <- getPosition
                      if pos >= endpos
                         then mzero
                         else anyChar
                result <- (guardEnabled Ext_latex_macros
                             >> (untokenize raw <$ skipMany rawChar))
                          <|> T.pack <$> many rawChar
                -- ensure we end with space if input did, see #4442
                let result' =
                      case reverse toks' of
                        (Tok _ (CtrlSeq _) t : _)
                         | " " `T.isSuffixOf` t
                         , not (" " `T.isSuffixOf` result)
                          -> result <> " "
                        _ -> result
                return (val, result')

applyMacros :: (PandocMonad m, HasMacros s, HasReaderOptions s)
            => Text -> ParsecT Sources s m Text
applyMacros s = (guardDisabled Ext_latex_macros >> return s) <|>
   do let retokenize = untokenize <$> many anyTok
      pstate <- getState
      let lstate = def{ sOptions = extractReaderOptions pstate
                      , sMacros  = extractMacros pstate :| [] }
      res <- flip runReaderT emptyLaTeXEnv $
                 runParserT retokenize lstate "math" $
                 TokStream False (tokenize (initialPos "math") s)
      case res of
           Left e   -> Prelude.fail (show e)
           Right s' -> return s'

{-
When tokenize or untokenize change, test with this
QuickCheck property:

> tokUntokRoundtrip :: String -> Bool
> tokUntokRoundtrip s =
>   let t = T.pack s in untokenize (tokenize "random" t) == t
-}

tokenizeSources :: Sources -> [Tok]
tokenizeSources = concatMap tokenizeSource . unSources
 where
   tokenizeSource (pos, t) = tokenize pos t

-- Return tokens from input sources. Ensure that starting position is
-- correct.
getInputTokens :: PandocMonad m => ParsecT Sources s m [Tok]
getInputTokens = do
  pos <- getPosition
  ss <- getInput
  return $
    case ss of
      Sources [] -> []
      Sources ((_,t):rest) -> tokenizeSources $ Sources ((pos,t):rest)

tokenize :: SourcePos -> Text -> [Tok]
tokenize = totoks (TokenizerState False False)
 where
  totoks atIsLetter pos t =
    case T.uncons t of
       Nothing        -> []
       Just (c, rest)
         | c == '\n' ->
           Tok pos Newline "\n"
           : totoks atIsLetter (setSourceColumn (incSourceLine pos 1) 1) rest
         | isSpaceOrTab c ->
           let (sps, rest') = T.span isSpaceOrTab t
           in  Tok pos Spaces sps
               : totoks atIsLetter (incSourceColumn pos (T.length sps))
                 rest'
         | isAlphaNum c ->
           let (ws, rest') = T.span isAlphaNum t
           in  Tok pos Word ws
               : totoks atIsLetter (incSourceColumn pos (T.length ws)) rest'
         | c == '%' ->
           let (cs, rest') = T.break (== '\n') rest
           in  Tok pos Comment ("%" <> cs)
               : totoks atIsLetter (incSourceColumn pos (1 + T.length cs)) rest'
         | c == '\\' ->
           case T.uncons rest of
                Nothing -> [Tok pos (CtrlSeq " ") "\\"]
                Just (d, rest')
                  | isLetter' atIsLetter d ->
                      let (ws, rest'') = T.span (isLetter' atIsLetter) rest
                          (ss, rest''') = T.span isSpaceOrTab rest''
                          atIsLetter' =
                            case ws of
                              "makeatletter" ->
                                atIsLetter{ tsAtIsLetter = True }
                              "makeatother" ->
                                atIsLetter{ tsAtIsLetter = False }
                              "ExplSyntaxOn" ->
                                atIsLetter{ tsExplSyntax = True }
                              "ExplSyntaxOff" ->
                                atIsLetter{ tsExplSyntax = False }
                              _ -> atIsLetter
                      in  Tok pos (CtrlSeq ws) ("\\" <> ws <> ss)
                          : totoks atIsLetter' (incSourceColumn pos
                               (1 + T.length ws + T.length ss)) rest'''
                  | isSpaceOrTab d || d == '\n' ->
                      let (w1, r1) = T.span isSpaceOrTab rest
                          (w2, (w3, r3)) = case T.uncons r1 of
                                          Just ('\n', r2)
                                                  -> (T.pack "\n",
                                                        T.span isSpaceOrTab r2)
                                          _ -> (mempty, (mempty, r1))
                          ws = "\\" <> w1 <> w2 <> w3
                      in  case T.uncons r3 of
                               Just ('\n', _) ->
                                 Tok pos (CtrlSeq " ") ("\\" <> w1)
                                 : totoks atIsLetter
                                    (incSourceColumn pos (T.length ws)) r1
                               _ ->
                                 Tok pos (CtrlSeq " ") ws
                                 : totoks atIsLetter
                                    (incSourceColumn pos (T.length ws)) r3
                  | otherwise  ->
                      Tok pos (CtrlSeq (T.singleton d)) (T.pack [c,d])
                      : totoks atIsLetter (incSourceColumn pos 2) rest'
         | c == '#' ->
           case T.uncons rest of
             Just ('#', t3) ->
               let (t1, t2) = T.span (\d -> d >= '0' && d <= '9') t3
               in  case safeRead t1 of
                        Just i ->
                           Tok pos (DeferredArg i) ("##" <> t1)
                           : totoks atIsLetter
                              (incSourceColumn pos (2 + T.length t1)) t2
                        Nothing -> Tok pos Symbol "#"
                                  : Tok (incSourceColumn pos 1) Symbol "#"
                                  : totoks atIsLetter (incSourceColumn pos 2) t3
             _ ->
               let (t1, t2) = T.span (\d -> d >= '0' && d <= '9') rest
               in  case safeRead t1 of
                        Just i ->
                           Tok pos (Arg i) ("#" <> t1)
                           : totoks atIsLetter
                               (incSourceColumn pos (1 + T.length t1)) t2
                        Nothing -> Tok pos Symbol "#"
                                  : totoks atIsLetter (incSourceColumn pos 1) rest
         | c == '^' ->
           case T.uncons rest of
                Just ('^', rest') ->
                  case T.uncons rest' of
                       Just (d, rest'')
                         | isLowerHex d ->
                           case T.uncons rest'' of
                                Just (e, rest''') | isLowerHex e ->
                                  Tok pos Esc2 (T.pack ['^','^',d,e])
                                  : totoks atIsLetter
                                     (incSourceColumn pos 4) rest'''
                                _ ->
                                  Tok pos Esc1 (T.pack ['^','^',d])
                                  : totoks atIsLetter
                                      (incSourceColumn pos 3) rest''
                         | d < '\128' ->
                                  Tok pos Esc1 (T.pack ['^','^',d])
                                  : totoks atIsLetter
                                     (incSourceColumn pos 3) rest''
                       _ -> Tok pos Symbol "^" :
                            Tok (incSourceColumn pos 1) Symbol "^" :
                            totoks atIsLetter (incSourceColumn pos 2) rest'
                _ -> Tok pos Symbol "^"
                     : totoks atIsLetter (incSourceColumn pos 1) rest
         | otherwise ->
           Tok pos Symbol (T.singleton c) :
             totoks atIsLetter (incSourceColumn pos 1) rest

isSpaceOrTab :: Char -> Bool
isSpaceOrTab ' '  = True
isSpaceOrTab '\t' = True
isSpaceOrTab _    = False

-- | State threaded through the tokenizer: whether @\@@ is a letter
-- (between @\makeatletter@ and @\makeatother@), and whether @:@ and
-- @_@ are letters (between @\ExplSyntaxOn@ and @\ExplSyntaxOff@).
data TokenizerState = TokenizerState
  { tsAtIsLetter :: Bool
  , tsExplSyntax :: Bool
  }

isLetter' :: TokenizerState -> Char -> Bool
isLetter' st '@' = tsAtIsLetter st
isLetter' st ':' = tsExplSyntax st
isLetter' st '_' = tsExplSyntax st
isLetter' _ c = isLetter c

isLetterOrAt :: Char -> Bool
isLetterOrAt '@' = True
isLetterOrAt c   = isLetter c

isLowerHex :: Char -> Bool
isLowerHex x = x >= '0' && x <= '9' || x >= 'a' && x <= 'f'

untokenize :: [Tok] -> Text
untokenize = T.concat . go
  where
    go [] = []
    go (Tok _ (CtrlSeq _) t : ts)
      -- insert space to prevent breaking a control sequence; see #5836
      | Just (_, c) <- T.unsnoc t
      , isLetter c
      , nextStartsWithLetter ts = t : " " : go ts
    go (Tok _ _ t : ts) = t : go ts
    nextStartsWithLetter (Tok _ _ t : ts) =
      case T.uncons t of
        Just (d, _) -> isLetter d
        Nothing     -> nextStartsWithLetter ts
    nextStartsWithLetter [] = False

untoken :: Tok -> Text
untoken (Tok _ _ t) = t

parseFromToks :: PandocMonad m => LP m a -> [Tok] -> LP m a
parseFromToks parser toks = do
  oldInput <- getInput
  setInput $ TokStream False toks
  oldpos <- getPosition
  case toks of
     Tok pos _ _ : _ -> setPosition pos
     _ -> return ()
  -- we ignore existing raw token accumulation (see #9517)
  oldst <- getState
  updateState $ \st -> st{ sRawTokens = []
                         , sRawTokenCount = 0
                         , sRawScopes = 0 }
  result <- parser
  updateState $ \st -> st{ sRawTokens = sRawTokens oldst
                         , sRawTokenCount = sRawTokenCount oldst
                         , sRawScopes = sRawScopes oldst }
  setInput oldInput
  setPosition oldpos
  return result

disablingWithRaw :: PandocMonad m => LP m a -> LP m a
disablingWithRaw parser = do
  oldEnableWithRaw <- sEnableWithRaw <$> getState
  updateState $ \st -> st{ sEnableWithRaw = False }
  result <- parser
  updateState $ \st -> st{ sEnableWithRaw = oldEnableWithRaw }
  return result

satisfyTok :: PandocMonad m => (Tok -> Bool) -> LP m Tok
satisfyTok f = do
    doMacros -- apply macros on remaining input stream
    res <- tokenPrim (T.unpack . untoken) updatePos matcher
    updateState $ \st ->
      if sRawScopes st > 0 && sEnableWithRaw st
         then st{ sRawTokens = res : sRawTokens st
                , sRawTokenCount = sRawTokenCount st + 1 }
         else st
    return $! res
  where matcher t | f t       = Just t
                  | otherwise = Nothing
        updatePos :: SourcePos -> Tok -> TokStream -> SourcePos
        updatePos _spos _ (TokStream _ (Tok pos _ _ : _)) = pos
        updatePos spos (Tok _ _ t) _ = incSourceColumn spos (T.length t)

peekTok :: PandocMonad m => LP m Tok
peekTok = do
  doMacros
  TokStream _ toks <- getInput
  case toks of
    t : _ -> return t
    []    -> mzero

doMacros :: PandocMonad m => LP m ()
doMacros = do
  TokStream macrosExpanded toks <- getInput
  unless macrosExpanded $
    case toks of
      -- only a control sequence at the head of the stream can
      -- trigger macro expansion; in other cases we skip the
      -- state update:
      Tok _ (CtrlSeq _) _ : _ -> do
        st <- getState
        unless (sVerbatimMode st) $
          doMacros' 1 toks >>= setInput . TokStream True
      _ -> return ()

doMacros' :: PandocMonad m => Int -> [Tok] -> LP m [Tok]
doMacros' n inp =
  case inp of
     Tok spos (CtrlSeq "begin") _ : Tok _ Symbol "{" :
      Tok _ Word name : Tok _ Symbol "}" : ts
        -> handleMacros n spos name ts <|> return inp
     Tok spos (CtrlSeq "end") _ : Tok _ Symbol "{" :
      Tok _ Word name : Tok _ Symbol "}" : ts
        -> handleMacros n spos ("end" <> name) ts <|> return inp
     Tok _ (CtrlSeq "expandafter") _ : t : ts
        -> combineTok t <$> doMacros' n ts
     Tok spos (CtrlSeq name) _ : ts
        -> handleMacros n spos name ts <|> return inp
     _ -> return inp

  where
    combineTok (Tok spos (CtrlSeq name) x) (Tok _ Word w : ts)
      | T.all isLetterOrAt w =
        Tok spos (CtrlSeq (name <> w)) (x1 <> w <> x2) : ts
          where (x1, x2) = T.break isSpaceOrTab x
    combineTok t ts = t:ts

    matchTok (Tok _ toktype txt) =
      satisfyTok (\(Tok _ toktype' txt') ->
                    toktype == toktype' &&
                    txt == txt')

    matchPattern toks = try $ mapM_ matchTok toks

    -- the first parameter is the number of the next argument
    -- to be bound (needed for the argspecs that don't carry an
    -- argument number themselves)
    getargs _ argmap [] = return argmap
    getargs num argmap (Pattern toks : rest) = try $ do
       matchPattern toks
       getargs num argmap rest
    getargs _ argmap (ArgNum i : Pattern toks : rest) =
      try $ do
        x <- mconcat <$> manyTill (braced <|> ((:[]) <$> anyTok))
                  (matchPattern toks)
        getargs (i + 1) (M.insert i x argmap) rest
    getargs _ argmap (ArgNum i : rest) = do
      x <- try $ spaces >> bracedOrToken
      getargs (i + 1) (M.insert i x argmap) rest
    getargs num argmap (BoolArg skipSp t@(Tok pos _ _) : rest) = do
      -- the ! modifier (skipSp False) disables space-skipping:
      let sp' = when skipSp sp
      x <- option [Tok pos (CtrlSeq "BooleanFalse") "\\BooleanFalse "]
             ([Tok pos (CtrlSeq "BooleanTrue") "\\BooleanTrue "]
               <$ try (sp' *> matchTok t))
      getargs (num + 1) (M.insert num x argmap) rest
    getargs num argmap (DelimArg skipSp open@(Tok pos _ _) close mbdef
                         : rest) = do
      let sp' = when skipSp sp
      let missing = fromMaybe [Tok pos (CtrlSeq "NoValue") "\\NoValue "] mbdef
      x <- option missing (try (sp' *> delimitedToks open close))
      getargs (num + 1) (M.insert num x argmap) rest
    getargs num argmap (VerbArg : rest) = do
      x <- verbatimArg
      getargs (num + 1) (M.insert num x argmap) rest
    -- xparse 'b' specifier: environment body, grabbed up to the
    -- \end{name} pattern; spaces at the ends are trimmed unless
    -- the ! modifier was used:
    getargs _ argmap (BodyArg doTrim i : Pattern toks : rest) = try $ do
      x <- mconcat <$> manyTill ((snd <$> withRaw (try braced))
                                  <|> ((:[]) <$> anyTok))
                (matchPattern toks)
      let x' = if doTrim then trimSpaceToks x else x
      getargs (i + 1) (M.insert i x' argmap) rest
    getargs num argmap (BodyArg _ i : rest) =
      getargs num argmap (ArgNum i : rest)
    -- xparse 'c' specifier: environment body, grabbed verbatim up
    -- to the \end{name} pattern; blank lines at the ends are
    -- trimmed unless the ! modifier was used:
    getargs _ argmap (VerbBodyArg doTrim i : Pattern toks : rest) = try $ do
      x <- mconcat <$> manyTill ((snd <$> withRaw (try braced))
                                  <|> ((:[]) <$> anyTok))
                (matchPattern toks)
      getargs (i + 1) (M.insert i (verbatimBodyToks doTrim x) argmap) rest
    getargs num argmap (VerbBodyArg _ i : rest) =
      getargs num argmap (ArgNum i : rest)
    getargs num argmap (EmbellishArg embs : rest) = do
      let grab seen
            | length seen == length embs = pure seen
            | otherwise = option seen $ try $ do
                sp
                i <- choice [ i <$ matchTok t
                            | (i, (t, _)) <- zip [(0 :: Int)..] embs
                            , i `notElem` map fst seen ]
                x <- braced <|> count 1 anyTok
                grab ((i, x) : seen)
      seen <- grab []
      let getval i (Tok pos _ _, mbdef) =
            case lookup i seen of
              Just x  -> x
              Nothing ->
                fromMaybe [Tok pos (CtrlSeq "NoValue") "\\NoValue "] mbdef
      let argmap' = foldr (\(i, e) m -> M.insert (num + i) (getval i e) m)
                          argmap (zip [0..] embs)
      getargs (num + length embs) argmap' rest
    getargs num argmap (ProcessedArg procs spec : rest) = do
      newargs <- getargs num M.empty [spec]
      newargs' <- traverse (applyArgProcessors procs) newargs
      getargs (num + M.size newargs) (M.union newargs' argmap) rest

    addTok False _args spos (Tok _ (DeferredArg i) txt) acc =
      Tok spos (Arg i) txt : acc
    addTok False args spos (Tok _ (Arg i) _) acc =
       case M.lookup i args of
            Nothing -> mzero
            Just xs -> foldr (addTok True args spos) acc xs
    -- see #4007
    addTok _ _ spos (Tok _ (CtrlSeq x) txt)
           acc@(Tok _ Word _ : _)
      | not (T.null txt)
      , isLetter (T.last txt) =
        Tok spos (CtrlSeq x) (txt <> " ") : acc
    addTok _ _ spos t acc = setpos spos t : acc

    handleMacros n' spos name ts = do
      when (n' > 20)  -- detect macro expansion loops
        $ throwError $ PandocMacroLoop name
      (macros :| _ ) <- sMacros <$> getState
      case M.lookup name macros of
           -- the result of a special macro may itself begin with a
           -- macro call, so we continue expanding:
           Nothing -> trySpecialMacro name ts >>= doMacros' (n' + 1)
           Just (Macro _scope expansionPoint argspecs optarg newtoks) -> do
             -- A macro whose expansion contains itself after an
             -- unknown command (e.g. \def\small{\@setfontsize\small..})
             -- would be expanded again and again at the same position,
             -- each time in a fresh call to doMacros (#11088):
             lastExp <- sLastExpansion <$> getState
             let expCount = case lastExp of
                              Just (name', spos', i)
                                | name' == name, spos' == spos -> i + 1
                              _ -> 1 :: Int
             when (expCount > 1000) $ throwError $ PandocMacroLoop name
             updateState $ \st -> st{ sLastExpansion =
                                        Just (name, spos, expCount) }
             let getargs' = do
                   args <-
                     (case expansionPoint of
                        ExpandWhenUsed    -> withVerbatimMode
                        ExpandWhenDefined -> id)
                     $ case optarg of
                             Nothing -> getargs 1 M.empty argspecs
                             Just o  -> do
                                x <- option o bracketedToks
                                getargs 2 (M.singleton 1 x) $ drop 1 argspecs
                   TokStream _ rest <- getInput
                   return (args, rest)
             lstate <- getState
             res <- lift $ runParserT getargs' lstate "args" $ TokStream False ts
             case res of
               Left _ -> Prelude.fail $ "Could not parse arguments for " ++
                                T.unpack name
               Right (args', rest) -> do
                 -- An argument default may refer to other arguments
                 -- (e.g. O{#2}); resolve such references (the visited
                 -- list guards against reference cycles):
                 let resolveTok visited t@(Tok _ (Arg j) _)
                       | j `notElem` visited
                       , Just ys <- M.lookup j args'
                       = concatMap (resolveTok (j : visited)) ys
                       | otherwise = [t]
                     resolveTok _ t = [t]
                 let args = M.mapWithKey
                              (\i -> concatMap (resolveTok [i])) args'
                 -- In TeX, a newline in a macro body behaves like a
                 -- space; a single newline at the end of the body,
                 -- followed by a newline in the source, must not be
                 -- mistaken for a blank line (paragraph break):
                 let newtoks' =
                       case reverse newtoks of
                         Tok p Newline _ : rts
                           | not (any isNewlineTok (take 1 rts))
                           , (Tok _ Newline _ : _) <-
                               dropWhile (tokTypeIn [Spaces, Comment]) rest
                           -> reverse (Tok p Spaces " " : rts)
                         _ -> newtoks
                 -- first boolean param is true if we're tokenizing
                 -- an argument (in which case we don't want to
                 -- expand #1 etc.)
                 let result = foldr (addTok False args spos) rest newtoks'
                 case expansionPoint of
                   ExpandWhenUsed    -> doMacros' (n' + 1) result
                   ExpandWhenDefined -> return result

-- | Certain macros do low-level tex manipulations that can't
-- be represented in our Macro type, so we handle them here.
trySpecialMacro :: PandocMonad m => Text -> [Tok] -> LP m [Tok]
trySpecialMacro "xspace" ts = do
  ts' <- doMacros' 1 ts
  case ts' of
    Tok pos Word t : _
      | startsWithAlphaNum t -> return $ Tok pos Spaces " " : ts'
    _ -> return ts'
trySpecialMacro "iftrue" ts = doIf True ts
trySpecialMacro "iffalse" ts = doIf False ts
trySpecialMacro "ifmmode" ts = do
  mathMode <- sMathMode <$> getState
  doIf mathMode ts
trySpecialMacro "ifstrequal" ts = do
  handleIf ifStrequalParser ts
-- xparse (LaTeX3) argument conditionals:
trySpecialMacro "IfNoValueTF" ts = handleIf (xparseIf isNoValueArg True True) ts
trySpecialMacro "IfNoValueT" ts = handleIf (xparseIf isNoValueArg True False) ts
trySpecialMacro "IfNoValueF" ts = handleIf (xparseIf isNoValueArg False True) ts
trySpecialMacro "IfValueTF" ts =
  handleIf (xparseIf (not . isNoValueArg) True True) ts
trySpecialMacro "IfValueT" ts =
  handleIf (xparseIf (not . isNoValueArg) True False) ts
trySpecialMacro "IfValueF" ts =
  handleIf (xparseIf (not . isNoValueArg) False True) ts
trySpecialMacro "IfBooleanTF" ts =
  handleIf (xparseIf isBooleanTrueArg True True) ts
trySpecialMacro "IfBooleanT" ts =
  handleIf (xparseIf isBooleanTrueArg True False) ts
trySpecialMacro "IfBooleanF" ts =
  handleIf (xparseIf isBooleanTrueArg False True) ts
trySpecialMacro "IfBlankTF" ts = handleIf (xparseIf isBlankArg True True) ts
trySpecialMacro "IfBlankT" ts = handleIf (xparseIf isBlankArg True False) ts
trySpecialMacro "IfBlankF" ts = handleIf (xparseIf isBlankArg False True) ts
-- \ProcessList{list}{tokens}: apply tokens to every item of list:
trySpecialMacro "ProcessList" ts = handleIf processListParser ts
-- \UseName{string}: turn string into a csname and execute it:
trySpecialMacro "UseName" ts = handleIf useNameParser ts
-- \ExpandArgs{spec}\cmd{arg1}...: pre-expand the command's
-- arguments as described by the spec:
trySpecialMacro "ExpandArgs" ts = handleIf expandArgsParser ts
-- LaTeX3 expandable evaluators:
trySpecialMacro "inteval" ts = handleEval evalInteval ts
trySpecialMacro "fpeval" ts = handleEval evalFpeval ts
-- dimension expressions are not evaluated; we substitute the
-- expression itself:
trySpecialMacro "dimeval" ts = handleEval (Just . T.strip) ts
trySpecialMacro "skipeval" ts = handleEval (Just . T.strip) ts
trySpecialMacro _ _ = mzero

-- | Parse a conditional of the kind used with xparse commands
-- (@\\IfNoValueTF@ etc.): test the first argument and select the
-- true or false branch.  The Bool parameters indicate whether a
-- true and a false branch, respectively, are to be parsed.
xparseIf :: PandocMonad m => ([Tok] -> Bool) -> Bool -> Bool -> LP m [Tok]
xparseIf test hasTrueBranch hasFalseBranch = do
  -- spaces and comments may intervene between the arguments:
  let grabArg = withVerbatimMode (spaces *> (braced <|> count 1 anyTok))
  let getBranch cond = if cond
                          then grabArg
                          else pure []
  arg <- grabArg
  trueToks <- getBranch hasTrueBranch
  falseToks <- getBranch hasFalseBranch
  TokStream _ rest <- getInput
  return $ (if test arg then trueToks else falseToks) ++ rest

isNoValueArg :: [Tok] -> Bool
isNoValueArg toks =
  case filter (not . tokTypeIn [Spaces, Newline, Comment]) toks of
    [Tok _ (CtrlSeq "NoValue") _] -> True
    [Tok _ Symbol "-", Tok _ Word "NoValue", Tok _ Symbol "-"] -> True
    _ -> False

isBooleanTrueArg :: [Tok] -> Bool
isBooleanTrueArg toks =
  case filter (not . tokTypeIn [Spaces, Newline, Comment]) toks of
    [Tok _ (CtrlSeq "BooleanTrue") _] -> True
    _ -> False

-- | An argument is \"blank\" (in the sense of @\\IfBlankTF@) if it
-- is empty or consists only of blanks.
isBlankArg :: [Tok] -> Bool
isBlankArg = all (tokTypeIn [Spaces, Newline, Comment])

-- | Parser for the arguments of @\\ProcessList{list}{tokens}@:
-- apply the tokens to every item (braced group or single token) of
-- the list.
processListParser :: PandocMonad m => LP m [Tok]
processListParser = withVerbatimMode $ do
  pos <- getPosition
  spaces
  list <- braced <|> count 1 anyTok
  spaces
  fn <- braced <|> count 1 anyTok
  TokStream _ rest <- getInput
  let items = unfoldr tokGroup list
  let braceIt item = Tok pos Symbol "{" : item ++ [Tok pos Symbol "}"]
  return $ concatMap (\item -> fn ++ braceIt item) items ++ rest

-- | Parser for the argument of @\\UseName{string}@: turn the
-- string into a control sequence.
useNameParser :: PandocMonad m => LP m [Tok]
useNameParser = do
  pos <- getPosition
  name <- untokenize <$> withVerbatimMode (spaces *> braced)
  TokStream _ rest <- getInput
  return $ Tok pos (CtrlSeq name) ("\\" <> name <> " ") : rest

-- | Parser for the arguments of @\\ExpandArgs{spec}\\cmd{arg1}...@:
-- transform each argument as described by the corresponding letter
-- of the spec (@c@ = turn a string into a control sequence, @n@ =
-- leave a braced argument unchanged, @N@ = leave a single token
-- unchanged), then put the command before the transformed
-- arguments.
expandArgsParser :: PandocMonad m => LP m [Tok]
expandArgsParser = withVerbatimMode $ do
  spec <- T.unpack . untokenize <$> (spaces *> braced)
  spaces
  cmd <- anyTok
  args <- concat <$> mapM transformArg spec
  TokStream _ rest <- getInput
  return $ cmd : args ++ rest
 where
  transformArg 'c' = do
    pos <- getPosition
    name <- untokenize <$> (spaces *> braced)
    return [Tok pos (CtrlSeq name) ("\\" <> name <> " ")]
  transformArg 'n' = do
    pos <- getPosition
    toks <- spaces *> braced
    return $ Tok pos Symbol "{" : toks ++ [Tok pos Symbol "}"]
  transformArg 'N' = spaces *> count 1 anyTok
  transformArg _ = mzero

ifStrequalParser :: PandocMonad m => LP m [Tok]
ifStrequalParser = do
  str1 <- braced <|> count 1 anyTok
  str2 <- braced <|> count 1 anyTok
  ifequal <- withVerbatimMode (braced <|> count 1 anyTok)
  ifnotequal <- withVerbatimMode (braced <|> count 1 anyTok)
  TokStream _ ts <- getInput
  return $
    if untokenize str1 == untokenize str2
       then ifequal ++ ts
       else ifnotequal ++ ts

handleIf :: PandocMonad m => LP m [Tok] -> [Tok] -> LP m [Tok]
handleIf parser ts = do
  res' <- lift $ runParserT parser defaultLaTeXState "tokens"
               $ TokStream False ts
  case res' of
    Left _ -> Prelude.fail "Could not parse conditional"
    Right ts' -> return ts'

-- | Names of TeX's primitive conditionals.  While scanning for the
-- @\\else@ or @\\fi@ that ends a conditional branch, we need to
-- know which control sequences begin a conditional, so that the
-- @\\else@ and @\\fi@ belonging to nested conditionals are not
-- mistaken for the end of the current one.
primitiveConditionalNames :: Set.Set Text
primitiveConditionalNames = Set.fromList
  [ "if", "ifcase", "ifcat", "ifcsname", "ifdefined", "ifdim"
  , "ifeof", "iffalse", "iffontchar", "ifhbox", "ifhmode"
  , "ifincsname", "ifinner", "ifmmode", "ifnum", "ifodd", "iftrue"
  , "ifvbox", "ifvmode", "ifvoid", "ifx" ]

-- | Handle a conditional like @\\iftrue@: select the branch before
-- @\\else@ (or @\\fi@) if the Bool is True, the else branch
-- otherwise.  The unselected branch is skipped without expansion
-- (as TeX does), keeping nested conditionals balanced.
doIf :: PandocMonad m => Bool -> [Tok] -> LP m [Tok]
doIf b ts = do
  macros <- sMacros <$> getState
  -- Conditionals defined with \newif (or \let from a primitive
  -- conditional) expand to a single conditional token; TeX
  -- recognizes these too when skipping conditional text.
  let isConditionalMacro (Macro _ _ [] Nothing [Tok _ (CtrlSeq n) _]) =
        n `Set.member` primitiveConditionalNames
      isConditionalMacro _ = False
  let conditionals = foldr
        (\m s -> M.foldrWithKey
           (\k v s' -> if isConditionalMacro v
                          then Set.insert k s'
                          else s')
           s m)
        primitiveConditionalNames macros
  case splitConditional conditionals ts of
    Just (ifToks, elseToks, rest) ->
      return $ (if b then ifToks else elseToks) ++ rest
    Nothing -> Prelude.fail "Could not parse conditional"

-- | Split the tokens following a conditional into the tokens
-- before @\\else@ (or @\\fi@), the tokens of the else branch (if
-- any), and the tokens after the matching @\\fi@.  Returns Nothing
-- if there is no matching @\\fi@.
splitConditional :: Set.Set Text -> [Tok] -> Maybe ([Tok], [Tok], [Tok])
splitConditional conditionals = goIf (0 :: Int) id
 where
  goIf _ _ [] = Nothing
  goIf depth acc (t@(Tok _ (CtrlSeq name) _) : rest)
    | name == "fi"
    , depth == 0 = Just (acc [], [], rest)
    | name == "fi" = goIf (depth - 1) (acc . (t:)) rest
    | name == "else"
    , depth == 0 = goElse (acc []) (0 :: Int) id rest
    | name `Set.member` conditionals = goIf (depth + 1) (acc . (t:)) rest
  goIf depth acc (t : rest) = goIf depth (acc . (t:)) rest
  goElse _ _ _ [] = Nothing
  goElse ifToks depth acc (t@(Tok _ (CtrlSeq name) _) : rest)
    | name == "fi"
    , depth == 0 = Just (ifToks, acc [], rest)
    | name == "fi" = goElse ifToks (depth - 1) (acc . (t:)) rest
    | name `Set.member` conditionals =
        goElse ifToks (depth + 1) (acc . (t:)) rest
  goElse ifToks depth acc (t : rest) = goElse ifToks depth (acc . (t:)) rest

-- | Handle a LaTeX3 expandable evaluator (@\inteval@, @\fpeval@,
-- ...): grab the braced argument (macros in it are expanded as it
-- is consumed), evaluate it, and substitute the result.  If the
-- expression cannot be evaluated, substitute the expression text
-- itself and report it.
handleEval :: PandocMonad m => (Text -> Maybe Text) -> [Tok] -> LP m [Tok]
handleEval evaluator ts = do
  lstate <- getState
  res <- lift $ runParserT evalParser lstate "eval" $ TokStream False ts
  case res of
    Left _ -> Prelude.fail "Could not parse evaluator argument"
    Right ts' -> return ts'
 where
  evalParser = do
    pos <- getPosition
    arg <- untokenize <$> braced
    TokStream _ rest <- getInput
    case evaluator arg of
      Just result -> return $ tokenize pos result ++ rest
      Nothing -> do
        report $ SkippedContent ("evaluation of " <> arg) pos
        return $ tokenize pos arg ++ rest

-- | Evaluate an integer expression (@\inteval@): @+ - * / ( )@,
-- with division rounding to the nearest integer (ties away from
-- zero, as in eTeX's @\numexpr@).
evalInteval :: Text -> Maybe Text
evalInteval t = do
  (n, rest) <- pIntExpr t
  guard $ T.null (skipWs rest)
  pure $ T.pack (show n)
 where
  pIntExpr s0 = pIntTerm s0 >>= addLoop
  addLoop (acc, s) =
    case T.uncons (skipWs s) of
      Just ('+', s') -> do (y, s'') <- pIntTerm s'
                           addLoop (acc + y, s'')
      Just ('-', s') -> do (y, s'') <- pIntTerm s'
                           addLoop (acc - y, s'')
      _ -> Just (acc, s)
  pIntTerm s0 = pIntFactor s0 >>= mulLoop
  mulLoop (acc, s) =
    case T.uncons (skipWs s) of
      Just ('*', s') -> do (y, s'') <- pIntFactor s'
                           mulLoop (acc * y, s'')
      Just ('/', s') -> do (y, s'') <- pIntFactor s'
                           guard $ y /= 0
                           mulLoop (divRound acc y, s'')
      _ -> Just (acc, s)
  pIntFactor s0 =
    case T.uncons (skipWs s0) of
      Just ('-', s) -> do (x, s') <- pIntFactor s
                          pure (negate x, s')
      Just ('+', s) -> pIntFactor s
      Just ('(', s) -> do
        (x, s') <- pIntExpr s
        case T.uncons (skipWs s') of
          Just (')', s'') -> pure (x, s'')
          _ -> Nothing
      Just (c, _) | isDigit c ->
        let (ds, s) = T.span isDigit (skipWs s0)
        in do n <- safeRead ds
              pure (n :: Integer, s)
      _ -> Nothing
  divRound a b =
    let (q, r) = a `quotRem` b
    in if 2 * abs r >= abs b
          then q + signum a * signum b
          else q

-- | Evaluate a floating point expression (a practical subset of
-- @\fpeval@): @+ - * / ^ ( )@ (also @**@ for @^@) and decimal
-- literals.
evalFpeval :: Text -> Maybe Text
evalFpeval t0 = do
  let t = T.replace "**" "^" t0
  (x, rest) <- pFpExpr t
  guard $ T.null (skipWs rest)
  guard $ not (isNaN x || isInfinite x)
  pure $ formatFp x
 where
  pFpExpr s0 = pFpTerm s0 >>= addLoop
  addLoop (acc, s) =
    case T.uncons (skipWs s) of
      Just ('+', s') -> do (y, s'') <- pFpTerm s'
                           addLoop (acc + y, s'')
      Just ('-', s') -> do (y, s'') <- pFpTerm s'
                           addLoop (acc - y, s'')
      _ -> Just (acc, s)
  pFpTerm s0 = pFpPow s0 >>= mulLoop
  mulLoop (acc, s) =
    case T.uncons (skipWs s) of
      Just ('*', s') -> do (y, s'') <- pFpPow s'
                           mulLoop (acc * y, s'')
      Just ('/', s') -> do (y, s'') <- pFpPow s'
                           mulLoop (acc / y, s'')
      _ -> Just (acc, s)
  pFpPow s0 = do
    (x, s) <- pFpFactor s0
    case T.uncons (skipWs s) of
      Just ('^', s') -> do (y, s'') <- pFpPow s'  -- right-associative
                           pure (x ** y, s'')
      _ -> pure (x, s)
  pFpFactor s0 =
    case T.uncons (skipWs s0) of
      Just ('-', s) -> do (x, s') <- pFpFactor s
                          pure (negate x, s')
      Just ('+', s) -> pFpFactor s
      Just ('(', s) -> do
        (x, s') <- pFpExpr s
        case T.uncons (skipWs s') of
          Just (')', s'') -> pure (x, s'')
          _ -> Nothing
      Just (c, _) | isLetter c ->
        let (name, s1) = T.span isLetter (skipWs s0)
        in case name of
             "pi"  -> pure (pi, s1)
             "deg" -> pure (pi / 180, s1)  -- one degree in radians
             _ ->
               case T.uncons (skipWs s1) of
                 Just ('(', s2) -> do
                   (args, s3) <- pFpArgs s2
                   x <- applyFn name args
                   pure (x, s3)
                 _ -> do
                   -- prefix application without parentheses,
                   -- e.g. "sqrt 2":
                   (y, s2) <- pFpFactor s1
                   x <- applyFn name [y]
                   pure (x, s2)
      Just (c, _) | isDigit c || c == '.' ->
        let s = skipWs s0
            (ds, s') = T.span (\d -> isDigit d || d == '.') s
            (expt, s'') = case T.uncons s' of
                            Just (e, r) | e == 'e' || e == 'E' ->
                              let (sign, r') =
                                    case T.uncons r of
                                      Just (sg, rr) | sg == '+' || sg == '-'
                                        -> (T.singleton sg, rr)
                                      _ -> ("", r)
                                  (eds, r'') = T.span isDigit r'
                              in if T.null eds
                                    then ("", s')
                                    else ("e" <> sign <> eds, r'')
                            _ -> ("", s')
        in do x <- safeRead (fixup ds <> expt)
              pure (x :: Double, s'')
      _ -> Nothing
  fixup ds -- make the literal readable for Haskell's 'read'
    | "." `T.isPrefixOf` ds = "0" <> fixup' ds
    | otherwise = fixup' ds
  fixup' ds
    | "." `T.isSuffixOf` ds = ds <> "0"
    | otherwise = ds
  -- comma-separated function arguments, ending with ')':
  pFpArgs s0 = do
    (x, s) <- pFpExpr s0
    case T.uncons (skipWs s) of
      Just (',', s') -> do (xs, s'') <- pFpArgs s'
                           pure (x : xs, s'')
      Just (')', s') -> pure ([x], s')
      _ -> Nothing
  applyFn name args =
    case (lookup name unaryFns, args) of
      (Just f, [x]) -> Just (f x)
      _ ->
        case (name, args) of
          ("max", _:_) -> Just (maximum args)
          ("min", _:_) -> Just (minimum args)
          ("atan", [x, y]) -> Just (atan2 x y)
          ("atand", [x, y]) -> Just (unrad (atan2 x y))
          ("round", _) -> rounder (fromInteger . round) args
          ("floor", _) -> rounder (fromInteger . floor) args
          ("ceil", _) -> rounder (fromInteger . ceiling) args
          ("trunc", _) -> rounder (fromInteger . truncate) args
          _ -> Nothing
  -- rounding functions take an optional number of decimal places:
  rounder f [x] = Just (f x)
  rounder f [x, n] | n == fromInteger (round n) =
    let m = 10 ^^ (round n :: Integer)
        -- round to 16 significant digits first (like l3fp), so
        -- that e.g. round(2.345,2) gives 2.34, not 2.35:
        y = read (showEFloat (Just 15) (x * m) "") :: Double
    in Just (f y / m)
  rounder _ _ = Nothing
  rad x = x * pi / 180
  unrad x = x * 180 / pi
  unaryFns :: [(Text, Double -> Double)]
  unaryFns =
    [ ("abs", abs), ("sign", signum), ("sqrt", sqrt)
    , ("exp", exp), ("ln", log)
    , ("fact", \x -> if x >= 0 && x == fromInteger (round x) && x < 171
                        then fromInteger (product [1 .. round x])
                        else 0 / 0)
    , ("sin", sin), ("cos", cos), ("tan", tan)
    , ("cot", recip . tan), ("sec", recip . cos), ("csc", recip . sin)
    , ("asin", asin), ("acos", acos), ("atan", atan)
    , ("acot", atan . recip), ("asec", acos . recip), ("acsc", asin . recip)
    , ("sind", sin . rad), ("cosd", cos . rad), ("tand", tan . rad)
    , ("cotd", recip . tan . rad), ("secd", recip . cos . rad)
    , ("cscd", recip . sin . rad)
    , ("asind", unrad . asin), ("acosd", unrad . acos)
    , ("atand", unrad . atan), ("acotd", unrad . atan . recip)
    , ("asecd", unrad . acos . recip), ("acscd", unrad . asin . recip)
    ]

formatFp :: Double -> Text
formatFp x0
  | x == fromInteger r && abs x < 1e16 = T.pack (show r)
  | otherwise = T.pack (trimZeros (showFFloat Nothing x ""))
 where
  -- l3fp computes with 16 significant decimal digits; round to
  -- that precision so that e.g. 0.1 + 0.2 yields 0.3.
  x = read (showEFloat (Just 15) x0 "") :: Double
  r = round x
  trimZeros s
    | '.' `elem` s = case dropWhileEnd (== '0') s of
                       s' | "." `isSuffixOf` s' -> take (length s' - 1) s'
                          | otherwise -> s'
    | otherwise = s

skipWs :: Text -> Text
skipWs = T.dropWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r')

startsWithAlphaNum :: Text -> Bool
startsWithAlphaNum t =
  case T.uncons t of
       Just (c, _) | isAlphaNum c -> True
       _           -> False

setpos :: SourcePos -> Tok -> Tok
setpos spos (Tok _ tt txt) = Tok spos tt txt

anyControlSeq :: PandocMonad m => LP m Tok
anyControlSeq = satisfyTok isCtrlSeq

isCtrlSeq :: Tok -> Bool
isCtrlSeq (Tok _ (CtrlSeq _) _) = True
isCtrlSeq _                     = False

anySymbol :: PandocMonad m => LP m Tok
anySymbol = satisfyTok isSymbolTok

isSymbolTok :: Tok -> Bool
isSymbolTok (Tok _ Symbol _) = True
isSymbolTok _                = False

isWordTok :: Tok -> Bool
isWordTok (Tok _ Word _) = True
isWordTok _              = False

isArgTok :: Tok -> Bool
isArgTok (Tok _ (Arg _) _) = True
isArgTok _                 = False

infile :: PandocMonad m => SourceName -> LP m Tok
infile reference = satisfyTok (\(Tok source _ _) -> (sourceName source) == reference)

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
sp = do
  optional $ skipMany (whitespace <|> comment)
  optional $ endline  *> skipMany (whitespace <|> comment)

whitespace :: PandocMonad m => LP m ()
whitespace = () <$ satisfyTok isSpaceTok

isSpaceTok :: Tok -> Bool
isSpaceTok (Tok _ Spaces _) = True
isSpaceTok _                = False

newlineTok :: PandocMonad m => LP m ()
newlineTok = () <$ satisfyTok isNewlineTok

isNewlineTok :: Tok -> Bool
isNewlineTok (Tok _ Newline _) = True
isNewlineTok _                 = False

comment :: PandocMonad m => LP m ()
comment = () <$ satisfyTok isCommentTok

isCommentTok :: Tok -> Bool
isCommentTok (Tok _ Comment _) = True
isCommentTok _                 = False

anyTok :: PandocMonad m => LP m Tok
anyTok = satisfyTok (const True)

singleCharTok :: PandocMonad m => LP m Tok
singleCharTok =
  satisfyTok $ \case
     Tok _ Word  t   -> T.length t == 1
     Tok _ Symbol t  -> not (T.any (`Set.member` specialChars) t)
     _               -> False

singleChar :: PandocMonad m => LP m Tok
singleChar = singleCharTok <|> singleCharFromWord
 where
  singleCharFromWord = do
    Tok pos toktype t <- disablingWithRaw $ satisfyTok isWordTok
    let (t1, t2) = (T.take 1 t, T.drop 1 t)
    TokStream macrosExpanded inp <- getInput
    setInput $ TokStream macrosExpanded
             $ Tok pos toktype t1 : Tok (incSourceColumn pos 1) toktype t2 : inp
    anyTok

specialChars :: Set.Set Char
specialChars = Set.fromList "#$%&~_^\\{}"

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
                    Nothing -> Prelude.fail "Empty content of Esc1"
       Esc2 -> case safeRead ("0x" <> T.drop 2 t) of
                    Just x  -> return (chr x)
                    Nothing -> Prelude.fail $ "Could not read: " ++ T.unpack t
       _    -> Prelude.fail "Expected an Esc1 or Esc2 token" -- should not happen

bgroup :: PandocMonad m => LP m Tok
bgroup = try $ do
  optional sp
  t <- symbol '{' <|> controlSeq "bgroup" <|> controlSeq "begingroup"
  -- Add a copy of the macro table to the top of the macro stack,
  -- private for this group. We inherit all the macros defined in
  -- the parent group.
  updateState $ \s -> s{ sMacros = NonEmpty.cons (NonEmpty.head (sMacros s))
                                                 (sMacros s) }
  return t


egroup :: PandocMonad m => LP m Tok
egroup = do
  t <- symbol '}' <|> controlSeq "egroup" <|> controlSeq "endgroup"
  -- remove the group's macro table from the stack
  updateState $ \s -> s{ sMacros = fromMaybe (sMacros s) $
      NonEmpty.nonEmpty (NonEmpty.tail (sMacros s)) }
  return t

grouped :: (PandocMonad m,  Monoid a) => LP m a -> LP m a
grouped parser = try $ do
  bgroup
  -- first we check for an inner 'grouped', because
  -- {{a,b}} should be parsed the same as {a,b}
  try (grouped parser <* egroup) <|> (mconcat <$> manyTill parser egroup)

braced' :: PandocMonad m => LP m Tok -> LP m [Tok]
braced' getTok = symbol '{' *> go (1 :: Int)
 where
  go n = do
    t <- getTok
    case t of
      Tok _ Symbol "}"
        | n > 1     -> (t:) <$> go (n - 1)
        | otherwise -> return []
      Tok _ Symbol "{" -> (t:) <$> go (n + 1)
      _ -> (t:) <$> go n

braced :: PandocMonad m => LP m [Tok]
braced = braced' anyTok

-- URLs require special handling, because they can contain %
-- characters.  So we retonenize comments as we go...
bracedUrl :: PandocMonad m => LP m [Tok]
bracedUrl = braced' (retokenizeComment >> anyTok)

-- For handling URLs, which allow literal % characters...
retokenizeComment :: PandocMonad m => LP m ()
retokenizeComment = (do
  Tok pos Comment txt <- satisfyTok isCommentTok
  let newtoks = tokenize (incSourceColumn pos 1) $ T.tail txt
  TokStream macrosExpanded ts <- getInput
  setInput $ TokStream macrosExpanded ((Tok pos Symbol "%" : newtoks) ++ ts))
    <|> return ()

bracedOrToken :: PandocMonad m => LP m [Tok]
bracedOrToken = braced <|> ((:[]) <$> (anyControlSeq <|> singleChar))

bracketed :: PandocMonad m => Monoid a => LP m a -> LP m a
bracketed parser = try $ do
  symbol '['
  mconcat <$> manyTill parser (symbol ']')

bracketedToks :: PandocMonad m => LP m [Tok]
bracketedToks = do
  symbol '['
  concat <$> manyTill ((snd <$> withRaw (try braced)) <|> count 1 anyTok)
                      (symbol ']')

-- | Tokens between opening and closing delimiter tokens (which are
-- compared by token type and text, ignoring position).  Nested
-- delimiter pairs are balanced (when the delimiters differ), and
-- braced groups are skipped, so delimiters inside braces don't count.
delimitedToks :: PandocMonad m => Tok -> Tok -> LP m [Tok]
delimitedToks open close = matchesTok open *> go (1 :: Int)
 where
  matchesTok (Tok _ toktype txt) =
    satisfyTok (\(Tok _ toktype' txt') -> toktype == toktype' && txt == txt')
  go n = (do ts <- snd <$> withRaw (try braced)
             (ts ++) <$> go n)
     <|> (do t <- matchesTok close
             if n == 1
                then return []
                else (t:) <$> go (n - 1))
     <|> (do t <- matchesTok open
             (t:) <$> go (n + 1))
     <|> (do t <- anyTok
             (t:) <$> go n)

-- | A verbatim argument (xparse @v@ specifier): either a braced
-- group or tokens between two identical delimiter characters.
-- The result is a single Word token containing the raw text, so
-- that its contents are not reinterpreted when substituted.
verbatimArg :: PandocMonad m => LP m [Tok]
verbatimArg = try $ do
  optional sp
  toks <- braced <|> delimited
  case toks of
    [] -> pure []
    Tok pos _ _ : _ -> pure [Tok pos Word (untokenize toks)]
 where
  delimited = do
    Tok _ Symbol t <- anySymbol
    marker <- case T.uncons t of
                Just (c, ts) | T.null ts -> return c
                _            -> mzero
    manyTill (notFollowedBy newlineTok >> verbTok marker) (symbol marker)

-- | Convert a verbatim environment body (xparse @c@ specifier) to
-- tokens for substitution.  The body is typeset verbatim by LaTeX,
-- with each space rendered as the character in slot 32 of the
-- current font (the visible space U+2423 in typewriter fonts) and
-- each source line on its own line; we emulate this by making each
-- line a single Word token (so contents are not reinterpreted),
-- with spaces replaced by U+2423 and lines separated by @\\\\@.
-- Tabs are kept as-is (LaTeX typesets the raw tab character, not a
-- visible space).  Note that the typewriter font is not automatic:
-- it comes from a @\\ttfamily@ or similar in the environment
-- definition.  If the first parameter is True, leading and trailing
-- blank lines are trimmed.
verbatimBodyToks :: Bool -> [Tok] -> [Tok]
verbatimBodyToks _ [] = []
verbatimBodyToks doTrim toks@(Tok pos _ _ : _) =
  intercalate [Tok pos (CtrlSeq "\\") "\\\\"] (map lineToks ls)
 where
  lineToks l
    | T.null l = []
    | otherwise = [Tok pos Word (T.map toVisibleSpace l)]
  toVisibleSpace c = if c == ' ' then '\x2423' else c
  ls = (if doTrim
           then dropWhileEnd isBlankLine . dropWhile isBlankLine
           else id) $ T.lines (untokenize toks)
  isBlankLine = T.all (\c -> c == ' ' || c == '\t')

-- | Apply xparse argument processors (from the @>{...}@ modifier)
-- to a grabbed argument.  Processors are applied from right to left
-- (i.e., the one nearest the argument specifier first).
applyArgProcessors :: PandocMonad m => [[Tok]] -> [Tok] -> LP m [Tok]
applyArgProcessors [] x = pure x
applyArgProcessors (p:ps) x = applyArgProcessors ps x >>= applyArgProcessor p

applyArgProcessor :: PandocMonad m => [Tok] -> [Tok] -> LP m [Tok]
applyArgProcessor proc x =
  case dropWhile spaceLike proc of
    Tok _ (CtrlSeq "TrimSpaces") _ : _ -> pure $ trimSpaceToks x
    Tok pos (CtrlSeq "ReverseBoolean") _ : _ ->
      pure $ case filter (not . spaceLike) x of
        [Tok _ (CtrlSeq "BooleanTrue") _]  ->
          [Tok pos (CtrlSeq "BooleanFalse") "\\BooleanFalse "]
        [Tok _ (CtrlSeq "BooleanFalse") _] ->
          [Tok pos (CtrlSeq "BooleanTrue") "\\BooleanTrue "]
        _ -> x
    Tok pos (CtrlSeq "SplitArgument") _ : ts
      | Just (numtoks, ts') <- tokGroup ts
      , Just numparts <- safeRead (untokenize numtoks)
      , Just (delimtoks, _) <- tokGroup ts'
      , (d : _) <- dropWhile spaceLike delimtoks ->
        let missing = [Tok pos (CtrlSeq "NoValue") "\\NoValue "]
            pieces = take (numparts + 1) $
                       map trimSpaceToks (splitToksOn d x) ++ repeat missing
        in pure $ concatMap (braceGroup pos) pieces
    Tok pos (CtrlSeq "SplitList") _ : ts
      | Just (delimtoks, _) <- tokGroup ts
      , (d : _) <- dropWhile spaceLike delimtoks ->
        pure $ concatMap (braceGroup pos) (map trimSpaceToks (splitToksOn d x))
    Tok pos _ _ : _ -> do
      -- unknown (or malformed) processor: keep the argument unprocessed
      report $ SkippedContent ("processor " <> untokenize proc) pos
      pure x
    [] -> pure x
 where
  spaceLike = tokTypeIn [Spaces, Newline, Comment]
  braceGroup pos ts = Tok pos Symbol "{" : ts ++ [Tok pos Symbol "}"]

-- | Remove space tokens at both ends of a token list.
trimSpaceToks :: [Tok] -> [Tok]
trimSpaceToks = dropWhile isSp . dropWhileEnd isSp
  where isSp = tokTypeIn [Spaces, Newline]

-- | Parse a braced group (or a single token) from the beginning of
-- a token list, skipping leading spaces; return the group's
-- contents and the remaining tokens.
tokGroup :: [Tok] -> Maybe ([Tok], [Tok])
tokGroup ts =
  case dropWhile (tokTypeIn [Spaces, Newline, Comment]) ts of
    Tok _ Symbol "{" : rest -> go (1 :: Int) [] rest
    t : rest -> Just ([t], rest)
    [] -> Nothing
 where
  go _ _ [] = Nothing
  go depth acc (t : rest)
    | isSym "{" t = go (depth + 1) (t : acc) rest
    | isSym "}" t = if depth == 1
                       then Just (reverse acc, rest)
                       else go (depth - 1) (t : acc) rest
    | otherwise = go depth (t : acc) rest
  isSym s (Tok _ Symbol s') = s == s'
  isSym _ _ = False

-- | Split a token list at each depth-0 occurrence of the delimiter
-- token (compared by token type and text).
splitToksOn :: Tok -> [Tok] -> [[Tok]]
splitToksOn (Tok _ dtype dtxt) = go (0 :: Int) []
 where
  go _ acc [] = [reverse acc]
  go depth acc (t@(Tok _ toktype txt) : rest)
    | depth == 0, toktype == dtype, txt == dtxt = reverse acc : go 0 [] rest
    | otherwise =
        let depth' = case t of
                       Tok _ Symbol "{" -> depth + 1
                       Tok _ Symbol "}" -> max 0 (depth - 1)
                       _ -> depth
        in go depth' (t : acc) rest

-- | Any token, but if the token contains @stopchar@, it is split
-- so that the part before @stopchar@ is returned and @stopchar@
-- itself (plus what follows) is left in the input.  Used for
-- parsing verbatim text delimited by @stopchar@.
verbTok :: PandocMonad m => Char -> LP m Tok
verbTok stopchar = do
  t@(Tok pos toktype txt) <- anyTok
  case T.findIndex (== stopchar) txt of
       Nothing -> return t
       Just i  -> do
         let (t1, t2) = T.splitAt i txt
         TokStream macrosExpanded inp <- getInput
         setInput $ TokStream macrosExpanded
                  $ Tok (incSourceColumn pos i) Symbol (T.singleton stopchar)
                  : tokenize (incSourceColumn pos (i + 1)) (T.drop 1 t2) ++ inp
         return $ Tok pos toktype t1

parenWrapped :: PandocMonad m => Monoid a => LP m a -> LP m a
parenWrapped parser = try $ do
  symbol '('
  mconcat <$> manyTill parser (symbol ')')

dimenarg :: PandocMonad m => LP m Text
dimenarg = try $ do
  optional sp
  ch  <- option False $ True <$ symbol '='
  minus <- option "" $ "-" <$ symbol '-'
  s1 <- option ""
        (do Tok _ _ s1 <- satisfyTok isWordTok
            guard (case T.uncons s1 of
                     Just (c,_) -> isDigit c
                     Nothing -> False)
            pure s1)
  s2 <- option "" $ try $ do
          symbol '.'
          Tok _ _ t <-  satisfyTok isWordTok
          return ("." <> t)
  let s = s1 <> s2
  let (num, rest) = T.span (\c -> isDigit c || c == '.') s
  guard $ T.length num > 0
  guard $ rest `elem`
    ["", "pt","pc","in","bp","cm","mm","dd","cc","sp","ex","em",
     "mu", -- "mu" in math mode only
     "px" -- "px" with pdftex and luatex only
    ]
  return $ T.pack ['=' | ch] <> minus <> s

ignore :: (Monoid a, PandocMonad m) => Text -> ParsecT s u m a
ignore raw = do
  pos <- getPosition
  report $ SkippedContent raw pos
  return mempty

withRaw :: PandocMonad m => LP m a -> LP m (a, [Tok])
withRaw parser = do
  startCount <- sRawTokenCount <$> getState
  updateState $ \st -> st{ sRawScopes = sRawScopes st + 1 }
  result <- parser
  st <- getState
  let raw = reverse $ take (sRawTokenCount st - startCount) (sRawTokens st)
  setState $ if sRawScopes st <= 1
                then st{ sRawScopes = 0
                       , sRawTokens = []
                       , sRawTokenCount = 0 }
                else st{ sRawScopes = sRawScopes st - 1 }
  return (result, raw)

keyval :: PandocMonad m => LP m (Text, Text)
keyval = try $ do
  sp
  key <- untokenize <$> many1 (notFollowedBy (symbol '=') >>
                         (symbol '-' <|> symbol '_' <|> satisfyTok isWordTok))
  sp
  val <- option mempty $ do
           symbol '='
           sp
           (untokenize <$> braced) <|>
             (mconcat <$> many1 (
                 (untokenize . snd <$> withRaw braced)
                 <|>
                 (untokenize <$> many1
                      (satisfyTok
                         (\case
                                Tok _ Symbol "]" -> False
                                Tok _ Symbol "," -> False
                                Tok _ Symbol "{" -> False
                                Tok _ Symbol "}" -> False
                                _                -> True)))))
  sp
  optional (symbol ',')
  sp
  return (key, T.strip val)

keyvals :: PandocMonad m => LP m [(Text, Text)]
keyvals = try $ symbol '[' >> manyTill keyval (symbol ']') <* sp

verbEnv :: PandocMonad m => Text -> LP m Text
verbEnv name = withVerbatimMode $ do
  optional blankline
  res <- manyTill anyTok (end_ name)
  return $ stripTrailingNewline
         $ untokenize res

-- Strip single final newline and any spaces following it.
-- Input is unchanged if it doesn't end with newline +
-- optional spaces.
stripTrailingNewline :: Text -> Text
stripTrailingNewline t =
  let (b, e) = T.breakOnEnd "\n" t
  in  if T.all (== ' ') e
         then T.dropEnd 1 b
         else t

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

getRawCommand :: PandocMonad m => Text -> Text -> LP m Text
getRawCommand name txt = do
  (_, rawargs) <- withRaw $
      case name of
           "write" -> do
             void $ many $ satisfyTok isDigitTok -- digits
             void braced
           "titleformat" -> do
             void braced
             skipopts
             void $ count 4 braced
           "def" ->
             void $ manyTill anyTok braced
           "vadjust" ->
             void (manyTill anyTok braced) <|>
                void (satisfyTok isPreTok) -- see #7531
           _ | isFontSizeCommand name -> return ()
             | name `elem` ["hfil", "hfill", "vfil", "vfill",
                            "hfilneg", "vfilneg"] -> return ()
             | name `elem` ["hskip", "vskip", "mskip"] -> do
                 dimenarg
                 skipMany $ try $ do
                   sp
                   satisfyTok $
                     \case
                       Tok _ Word "plus" -> True
                       Tok _ Word "minus" -> True
                       _ -> False
                   dimenarg
             | otherwise -> do
               skipopts
               option "" (try dimenarg)
               void $ many braced
  return $ txt <> untokenize rawargs

isPreTok :: Tok -> Bool
isPreTok (Tok _ Word "pre") = True
isPreTok _ = False

isDigitTok :: Tok -> Bool
isDigitTok (Tok _ Word t) = T.all isDigit t
isDigitTok _              = False

skipopts :: PandocMonad m => LP m ()
skipopts = skipMany (void overlaySpecification <|> void rawopt)

-- opts in angle brackets are used in beamer
overlaySpecification :: PandocMonad m => LP m Text
overlaySpecification = try $ do
  symbol '<'
  t <- untokenize <$> manyTill overlayTok (symbol '>')
  -- see issue #3368
  guard $ not (T.all isLetter t) ||
          t `elem` ["beamer","presentation", "trans",
                    "handout","article", "second"]
  return $ "<" <> t <> ">"

overlayTok :: PandocMonad m => LP m Tok
overlayTok =
  satisfyTok (\case
                    Tok _ Word _       -> True
                    Tok _ Spaces _     -> True
                    Tok _ Symbol c     -> c `elem` ["-","+","@","|",":",","]
                    _                  -> False)

rawopt :: PandocMonad m => LP m Text
rawopt = try $ do
  sp
  inner <- untokenize <$> bracketedToks
  sp
  return $ "[" <> inner <> "]"

isFontSizeCommand :: Text -> Bool
isFontSizeCommand "tiny" = True
isFontSizeCommand "scriptsize" = True
isFontSizeCommand "footnotesize" = True
isFontSizeCommand "small" = True
isFontSizeCommand "normalsize" = True
isFontSizeCommand "large" = True
isFontSizeCommand "Large" = True
isFontSizeCommand "LARGE" = True
isFontSizeCommand "huge" = True
isFontSizeCommand "Huge" = True
isFontSizeCommand _ = False

getNextNumber :: Monad m
              => (LaTeXState -> DottedNum) -> LP m DottedNum
getNextNumber getCurrentNum = do
  st <- getState
  let chapnum =
        case sLastHeaderNum st of
             DottedNum (n:_) | sHasChapters st -> Just n
             _                                 -> Nothing
  return . DottedNum $
    case getCurrentNum st of
       DottedNum [m,n]  ->
         case chapnum of
              Just m' | m' == m   -> [m, n+1]
                      | otherwise -> [m', 1]
              Nothing             -> [1]
                                      -- shouldn't happen
       DottedNum [n]   ->
         case chapnum of
              Just m  -> [m, 1]
              Nothing -> [n + 1]
       _               ->
         case chapnum of
               Just n  -> [n, 1]
               Nothing -> [1]

label :: PandocMonad m => LP m ()
label = do
  controlSeq "label"
  t <- braced
  updateState $ \st -> st{ sLastLabel = Just $ untokenize t }

setCaption :: PandocMonad m => LP m Inlines -> LP m ()
setCaption inline = try $ do
  mbshort <- Just . toList <$> bracketed inline <|> pure Nothing
  ils <- tokWith inline
  optional $ try $ spaces *> label
  updateState $ \st -> st{ sCaption = Just $
                              Caption mbshort [Plain $ toList ils] }

resetCaption :: PandocMonad m => LP m ()
resetCaption = updateState $ \st -> st{ sCaption   = Nothing
                                      , sLastLabel = Nothing }

env :: PandocMonad m => Text -> LP m a -> LP m a
env name p = do
  -- environments are groups as far as macros are concerned,
  -- so we need a local copy of the macro table (see above, bgroup, egroup):
  updateState $ \s -> s{ sMacros = NonEmpty.cons (NonEmpty.head (sMacros s))
                                                 (sMacros s) }
  result <- p
  updateState $ \s -> s{ sMacros = fromMaybe (sMacros s) $
      NonEmpty.nonEmpty (NonEmpty.tail (sMacros s)) }
  end_ name
  return result

tokWith :: PandocMonad m => LP m Inlines -> LP m Inlines
tokWith inlineParser = try $ spaces >>
                                 grouped inlineParser
                            <|> (lookAhead anyControlSeq >> inlineParser)
                            <|> singleChar'
  where singleChar' = do
          Tok _ _ t <- singleChar
          return $ str t

addMeta :: PandocMonad m => ToMetaValue a => Text -> a -> LP m ()
addMeta field val = updateState $ \st ->
   st{ sMeta = addMetaField field val $ sMeta st }

-- remove label spans to avoid duplicated identifier
removeLabel :: Walkable [Inline] a => Text -> a -> a
removeLabel lbl = walk go
 where
  go (Span (_,_,kvs) _ : rest)
    | Just lbl' <- lookup "label" kvs
    , lbl' == lbl = go (dropWhile isSpaceOrSoftBreak rest)
  go (x:xs) = x : go xs
  go [] = []
  isSpaceOrSoftBreak Space = True
  isSpaceOrSoftBreak SoftBreak = True
  isSpaceOrSoftBreak _ = False
