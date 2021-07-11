{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
{- |
   Module      : Text.Pandoc.Readers.LaTeX
   Copyright   : Copyright (C) 2006-2021 John MacFarlane
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
                                   inlineCommand
                                 ) where

import Control.Applicative (many, optional, (<|>))
import Control.Monad
import Control.Monad.Except (throwError)
import Data.Char (isDigit, isLetter, isAlphaNum, toUpper, chr)
import Data.Default
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Skylighting (defaultSyntaxMap)
import System.FilePath (addExtension, replaceExtension, takeExtension)
import Text.Collate.Lang (renderLang)
import Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocPure (PandocPure)
import Text.Pandoc.Class.PandocMonad (PandocMonad (..), getResourcePath,
                                      readFileFromDirs, report,
                                      setResourcePath)
import Text.Pandoc.Error (PandocError (PandocParseError, PandocParsecError))
import Text.Pandoc.Highlighting (languagesByExtension)
import Text.Pandoc.ImageSize (numUnit, showFl)
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (blankline, many, mathDisplay, mathInline,
                            optional, space, spaces, withRaw, (<|>))
import Text.Pandoc.Readers.LaTeX.Types (Tok (..), TokType (..))
import Text.Pandoc.Readers.LaTeX.Parsing
import Text.Pandoc.Readers.LaTeX.Citation (citationCommands, cites)
import Text.Pandoc.Readers.LaTeX.Math (dollarsMath, inlineEnvironments,
                                       inlineEnvironment,
                                       mathDisplay, mathInline,
                                       newtheorem, theoremstyle, proof,
                                       theoremEnvironment)
import Text.Pandoc.Readers.LaTeX.Table (tableEnvironments)
import Text.Pandoc.Readers.LaTeX.Macro (macroDef)
import Text.Pandoc.Readers.LaTeX.Lang (inlineLanguageCommands,
                                       enquoteCommands,
                                       babelLangToBCP47, setDefaultLanguage)
import Text.Pandoc.Readers.LaTeX.SIunitx (siunitxCommands)
import Text.Pandoc.Readers.LaTeX.Inline (acronymCommands, refCommands,
                                         nameCommands, charCommands,
                                         accentCommands,
                                         biblatexInlineCommands,
                                         verbCommands, rawInlineOr,
                                         listingsLanguage)
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Data.List.NonEmpty (nonEmpty)

-- for debugging:
-- import Text.Pandoc.Extensions (getDefaultExtensions)
-- import Text.Pandoc.Class.PandocIO (runIOorExplode, PandocIO)
-- import Debug.Trace (traceShowId)

-- | Parse LaTeX from string and return 'Pandoc' document.
readLaTeX :: (PandocMonad m, ToSources a)
          => ReaderOptions -- ^ Reader options
          -> a             -- ^ Input to parse
          -> m Pandoc
readLaTeX opts ltx = do
  let sources = toSources ltx
  parsed <- runParserT parseLaTeX def{ sOptions = opts } "source"
               (tokenizeSources sources)
  case parsed of
    Right result -> return result
    Left e       -> throwError $ PandocParsecError sources e

parseLaTeX :: PandocMonad m => LP m Pandoc
parseLaTeX = do
  bs <- blocks
  eof
  st <- getState
  let meta = sMeta st
  let doc' = doc bs
  let headerLevel (Header n _ _) = [n]
      headerLevel _              = []
  let bottomLevel = maybe 1 minimum $ nonEmpty $ query headerLevel doc'
  let adjustHeaders m (Header n attr ils) = Header (n+m) attr ils
      adjustHeaders _ x                   = x
  let (Pandoc _ bs') =
       -- handle the case where you have \part or \chapter
       (if bottomLevel < 1
           then walk (adjustHeaders (1 - bottomLevel))
           else id) $
       walk (resolveRefs (sLabels st)) doc'
  return $ Pandoc meta bs'

resolveRefs :: M.Map Text [Inline] -> Inline -> Inline
resolveRefs labels x@(Link (ident,classes,kvs) _ _) =
  case (lookup "reference-type" kvs,
        lookup "reference" kvs) of
        (Just "ref", Just lab) ->
          case M.lookup lab labels of
               Just txt -> Link (ident,classes,kvs) txt ("#" <> lab, "")
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


rawLaTeXBlock :: (PandocMonad m, HasMacros s, HasReaderOptions s)
              => ParserT Sources s m Text
rawLaTeXBlock = do
  lookAhead (try (char '\\' >> letter))
  toks <- getInputTokens
  snd <$> (rawLaTeXParser toks False (macroDef (const mempty)) blocks
      <|> rawLaTeXParser toks True
             (do choice (map controlSeq
                   ["include", "input", "subfile", "usepackage"])
                 skipMany opt
                 braced
                 return mempty) blocks
      <|> rawLaTeXParser toks True
           (environment <|> blockCommand)
           (mconcat <$> many (block <|> beginOrEndCommand)))

-- See #4667 for motivation; sometimes people write macros
-- that just evaluate to a begin or end command, which blockCommand
-- won't accept.
beginOrEndCommand :: PandocMonad m => LP m Blocks
beginOrEndCommand = try $ do
  Tok _ (CtrlSeq name) txt <- anyControlSeq
  guard $ name == "begin" || name == "end"
  (envname, rawargs) <- withRaw braced
  if M.member (untokenize envname)
      (inlineEnvironments :: M.Map Text (LP PandocPure Inlines))
     then mzero
     else return $ rawBlock "latex"
                    (txt <> untokenize rawargs)

rawLaTeXInline :: (PandocMonad m, HasMacros s, HasReaderOptions s)
               => ParserT Sources s m Text
rawLaTeXInline = do
  lookAhead (try (char '\\' >> letter))
  toks <- getInputTokens
  raw <- snd <$>
          (   rawLaTeXParser toks True
              (mempty <$ (controlSeq "input" >> skipMany rawopt >> braced))
              inlines
          <|> rawLaTeXParser toks True (inlineEnvironment <|> inlineCommand')
              inlines
          )
  finalbraces <- mconcat <$> many (try (string "{}")) -- see #5439
  return $ raw <> T.pack finalbraces

inlineCommand :: PandocMonad m => ParserT Sources ParserState m Inlines
inlineCommand = do
  lookAhead (try (char '\\' >> letter))
  toks <- getInputTokens
  fst <$> rawLaTeXParser toks True (inlineEnvironment <|> inlineCommand')
          inlines

-- inline elements:

word :: PandocMonad m => LP m Inlines
word = str . untoken <$> satisfyTok isWordTok

inlineGroup :: PandocMonad m => LP m Inlines
inlineGroup = do
  ils <- grouped inline
  if null ils
     then return mempty
     else return $ spanWith nullAttr ils
          -- we need the span so we can detitlecase bibtex entries;
          -- we need to know when something is {C}apitalized

doLHSverb :: PandocMonad m => LP m Inlines
doLHSverb =
  codeWith ("",["haskell"],[]) . untokenize
    <$> manyTill (satisfyTok (not . isNewlineTok)) (symbol '|')

mkImage :: PandocMonad m => [(Text, Text)] -> Text -> LP m Inlines
mkImage options (T.unpack -> src) = do
   let replaceTextwidth (k,v) =
         case numUnit v of
              Just (num, "\\textwidth") -> (k, showFl (num * 100) <> "%")
              _                         -> (k, v)
   let kvs = map replaceTextwidth
             $ filter (\(k,_) -> k `elem` ["width", "height"]) options
   let attr = ("",[], kvs)
   let alt = str "image"
   defaultExt <- getOption readerDefaultImageExtension
   let exts' = [".pdf", ".png", ".jpg", ".mps", ".jpeg", ".jbig2", ".jb2"]
   let exts  = exts' ++ map (map toUpper) exts'
   let findFile s [] = return s
       findFile s (e:es) = do
         let s' = addExtension s e
         exists <- fileExists s'
         if exists
            then return s'
            else findFile s es
   src' <- case takeExtension src of
               "" | not (T.null defaultExt) -> return $ addExtension src $ T.unpack defaultExt
                  | otherwise -> findFile src exts
               _  -> return src
   return $ imageWith attr (T.pack src') "" alt

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
  startchs <- untokenize <$> starter
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

lit :: Text -> LP m Inlines
lit = pure . str

blockquote :: PandocMonad m => Bool -> Maybe Text -> LP m Blocks
blockquote cvariant mblang = do
  citepar <- if cvariant
                then (\xs -> para (cite xs mempty))
                       <$> cites inline NormalCitation False
                else option mempty $ para <$> bracketed inline
  let lang = mblang >>= babelLangToBCP47
  let langdiv = case lang of
                      Nothing -> id
                      Just l  -> divWith ("",[],[("lang", renderLang l)])
  _closingPunct <- option mempty $ bracketed inline -- currently ignored
  bs <- grouped block
  optional $ symbolIn (".:;?!" :: [Char])  -- currently ignored
  return $ blockQuote . langdiv $ (bs <> citepar)

inlineCommand' :: PandocMonad m => LP m Inlines
inlineCommand' = try $ do
  Tok _ (CtrlSeq name) cmd <- anyControlSeq
  guard $ name /= "begin" && name /= "end" && name /= "and"
  star <- if T.all isAlphaNum name
             then option "" ("*" <$ symbol '*' <* sp)
             else pure ""
  overlay <- option "" overlaySpecification
  let name' = name <> star <> overlay
  let names = ordNub [name', name] -- check non-starred as fallback
  let raw = do
       guard $ isInlineCommand name || not (isBlockCommand name)
       rawcommand <- getRawCommand name (cmd <> star)
       (guardEnabled Ext_raw_tex >> return (rawInline "latex" rawcommand))
         <|> ignore rawcommand
  lookupListDefault raw names inlineCommands

tok :: PandocMonad m => LP m Inlines
tok = tokWith inline

unescapeURL :: Text -> Text
unescapeURL = T.concat . go . T.splitOn "\\"
  where
    isEscapable c = c `elemText` "#$%&~_^\\{}"
    go (x:xs) = x : map unescapeInterior xs
    go []     = []
    unescapeInterior t
      | Just (c, _) <- T.uncons t
      , isEscapable c = t
      | otherwise = "\\" <> t

inlineCommands :: PandocMonad m => M.Map Text (LP m Inlines)
inlineCommands = M.unions
  [ accentCommands tok
  , citationCommands inline
  , siunitxCommands tok
  , acronymCommands
  , refCommands
  , nameCommands
  , verbCommands
  , charCommands
  , enquoteCommands tok
  , inlineLanguageCommands tok
  , biblatexInlineCommands tok
  , rest ]
 where
  rest = M.fromList
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
    , ("alert", skipopts >> spanWith ("",["alert"],[]) <$> tok) -- beamer
    , ("textsuperscript", extractSpaces superscript <$> tok)
    , ("textsubscript", extractSpaces subscript <$> tok)
    , ("textbf", extractSpaces strong <$> tok)
    , ("textnormal", extractSpaces (spanWith ("",["nodecor"],[])) <$> tok)
    , ("underline", underline <$> tok)
    , ("mbox", rawInlineOr "mbox" $ processHBox <$> tok)
    , ("hbox", rawInlineOr "hbox" $ processHBox <$> tok)
    , ("lettrine", rawInlineOr "lettrine" lettrine)
    , ("(", mathInline . untokenize <$> manyTill anyTok (controlSeq ")"))
    , ("[", mathDisplay . untokenize <$> manyTill anyTok (controlSeq "]"))
    , ("ensuremath", mathInline . untokenize <$> braced)
    , ("texorpdfstring", const <$> tok <*> tok)
    -- old TeX commands
    , ("em", extractSpaces emph <$> inlines)
    , ("it", extractSpaces emph <$> inlines)
    , ("sl", extractSpaces emph <$> inlines)
    , ("bf", extractSpaces strong <$> inlines)
    , ("tt", code . stringify . toList <$> inlines)
    , ("rm", inlines)
    , ("itshape", extractSpaces emph <$> inlines)
    , ("slshape", extractSpaces emph <$> inlines)
    , ("scshape", extractSpaces smallcaps <$> inlines)
    , ("bfseries", extractSpaces strong <$> inlines)
    , ("MakeUppercase", makeUppercase <$> tok)
    , ("MakeTextUppercase", makeUppercase <$> tok) -- textcase
    , ("uppercase", makeUppercase <$> tok)
    , ("MakeLowercase", makeLowercase <$> tok)
    , ("MakeTextLowercase", makeLowercase <$> tok)
    , ("lowercase", makeLowercase <$> tok)
    , ("thanks", skipopts >> note <$> grouped block)
    , ("footnote", skipopts >> note <$> grouped block)
    , ("passthrough", tok) -- \passthrough macro used by latex writer
                           -- for listings
    , ("includegraphics", do options <- option [] keyvals
                             src <- braced
                             mkImage options .
                               unescapeURL .
                               removeDoubleQuotes $ untokenize src)
    -- hyperref
    , ("url", (\url -> link url "" (str url)) . unescapeURL . untokenize <$>
                    bracedUrl)
    , ("nolinkurl", code . unescapeURL . untokenize <$> bracedUrl)
    , ("href", do url <- bracedUrl
                  sp
                  link (unescapeURL $ untokenize url) "" <$> tok)
    , ("hyperlink", hyperlink)
    , ("hyperref", hyperref)
    , ("hypertarget", hypertargetInline)
    -- hyphenat
    , ("nohyphens", tok)
    , ("textnhtt", ttfamily)
    , ("nhttfamily", ttfamily)
    -- LaTeX colors
    , ("textcolor", coloredInline "color")
    , ("colorbox", coloredInline "background-color")
    -- etoolbox
    , ("ifstrequal", ifstrequal)
    , ("newtoggle", braced >>= newToggle)
    , ("toggletrue", braced >>= setToggle True)
    , ("togglefalse", braced >>= setToggle False)
    , ("iftoggle", try $ ifToggle >> inline)
    -- include
    , ("input", rawInlineOr "input" $ include "input")
    -- soul package
    , ("ul", underline <$> tok)
    -- ulem package
    , ("uline", underline <$> tok)
    -- plain tex stuff that should just be passed through as raw tex
    , ("ifdim", ifdim)
    ]

lettrine :: PandocMonad m => LP m Inlines
lettrine = do
  optional rawopt
  x <- tok
  y <- tok
  return $ extractSpaces (spanWith ("",["lettrine"],[])) x <> smallcaps y

ifdim :: PandocMonad m => LP m Inlines
ifdim = do
  contents <- manyTill anyTok (controlSeq "fi")
  return $ rawInline "latex" $ "\\ifdim" <> untokenize contents <> "\\fi"

makeUppercase :: Inlines -> Inlines
makeUppercase = fromList . walk (alterStr T.toUpper) . toList

makeLowercase :: Inlines -> Inlines
makeLowercase = fromList . walk (alterStr T.toLower) . toList

alterStr :: (Text -> Text) -> Inline -> Inline
alterStr f (Str xs) = Str (f xs)
alterStr _ x = x

hyperlink :: PandocMonad m => LP m Inlines
hyperlink = try $ do
  src <- untokenize <$> braced
  lab <- tok
  return $ link ("#" <> src) "" lab

hyperref :: PandocMonad m => LP m Inlines
hyperref = try $ do
  url <- (("#" <>) . untokenize <$> try (sp *> bracketedToks <* sp))
       <|> untokenize <$> (bracedUrl <* bracedUrl <* bracedUrl)
  link url "" <$> tok

hypertargetBlock :: PandocMonad m => LP m Blocks
hypertargetBlock = try $ do
  ref <- untokenize <$> braced
  bs <- grouped block
  case toList bs of
       [Header 1 (ident,_,_) _] | ident == ref -> return bs
       _                        -> return $ divWith (ref, [], []) bs

hypertargetInline :: PandocMonad m => LP m Inlines
hypertargetInline = try $ do
  ref <- untokenize <$> braced
  ils <- grouped inline
  return $ spanWith (ref, [], []) ils

newToggle :: (Monoid a, PandocMonad m) => [Tok] -> LP m a
newToggle name = do
  updateState $ \st ->
    st{ sToggles = M.insert (untokenize name) False (sToggles st) }
  return mempty

setToggle :: (Monoid a, PandocMonad m) => Bool -> [Tok] -> LP m a
setToggle on name = do
  updateState $ \st ->
    st{ sToggles = M.adjust (const on) (untokenize name) (sToggles st) }
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
  let name' = untokenize name
  case M.lookup name' toggles of
                Just True  -> setInput (yes ++ inp)
                Just False -> setInput (no  ++ inp)
                Nothing    -> do
                  pos <- getPosition
                  report $ UndefinedToggle name' pos
  return ()

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

coloredInline :: PandocMonad m => Text -> LP m Inlines
coloredInline stylename = do
  skipopts
  color <- braced
  spanWith ("",[],[("style",stylename <> ": " <> untokenize color)]) <$> tok

ttfamily :: PandocMonad m => LP m Inlines
ttfamily = code . stringify . toList <$> tok

processHBox :: Inlines -> Inlines
processHBox = walk convert
  where
    convert Space     = Str $ T.singleton $ chr 160 -- non-breakable space
    convert SoftBreak = Str $ T.singleton $ chr 160 -- non-breakable space
    convert LineBreak = Str ""
    convert x         = x

isBlockCommand :: Text -> Bool
isBlockCommand s =
  s `M.member` (blockCommands :: M.Map Text (LP PandocPure Blocks))
  || s `Set.member` treatAsBlock

treatAsBlock :: Set.Set Text
treatAsBlock = Set.fromList
   [ "special", "pdfannot", "pdfstringdef"
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
   , "listoffigures"
   , "listoftables"
   , "write"
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

lookupListDefault :: (Ord k) => v -> [k] -> M.Map k v -> v
lookupListDefault d = (fromMaybe d .) . lookupList
  where lookupList l m = msum $ map (`M.lookup` m) l

inline :: PandocMonad m => LP m Inlines
inline = do
  Tok pos toktype t <- lookAhead anyTok
  let symbolAsString = str . untoken <$> anySymbol
  let unescapedSymbolAsString =
        do s <- untoken <$> anySymbol
           report $ ParsingUnescaped s pos
           return $ str s
  case toktype of
    Comment     -> mempty <$ comment
    Spaces      -> space <$ whitespace
    Newline     -> softbreak <$ endline
    Word        -> word
    Esc1        -> str . T.singleton <$> primEscape
    Esc2        -> str . T.singleton <$> primEscape
    Symbol      ->
      case t of
        "-"     -> symbol '-' *>
                    option (str "-") (symbol '-' *>
                      option (str "–") (str "—" <$ symbol '-'))
        "'"     -> symbol '\'' *>
                  option (str "’") (str  "”" <$ symbol '\'')
        "~"     -> str "\160" <$ symbol '~'
        "`"     -> doubleQuote <|> singleQuote <|> symbolAsString
        "\""    -> doubleQuote <|> singleQuote <|> symbolAsString
        "“"     -> doubleQuote <|> symbolAsString
        "‘"     -> singleQuote <|> symbolAsString
        "$"     -> dollarsMath <|> unescapedSymbolAsString
        "|"     -> (guardEnabled Ext_literate_haskell *>
                    symbol '|' *> doLHSverb) <|> symbolAsString
        "{"     -> inlineGroup
        "#"     -> unescapedSymbolAsString
        "&"     -> unescapedSymbolAsString
        "_"     -> unescapedSymbolAsString
        "^"     -> unescapedSymbolAsString
        "\\"    -> mzero
        "}"     -> mzero
        _       -> symbolAsString
    CtrlSeq _   -> macroDef (rawInline "latex")
                  <|> inlineCommand'
                  <|> inlineEnvironment
                  <|> inlineGroup
    _           -> mzero

inlines :: PandocMonad m => LP m Inlines
inlines = mconcat <$> many inline

opt :: PandocMonad m => LP m Inlines
opt = do
  toks <- try (sp *> bracketedToks <* sp)
  -- now parse the toks as inlines
  st <- getState
  parsed <- runParserT (mconcat <$> many inline) st "bracketed option" toks
  case parsed of
    Right result -> return result
    Left e       -> throwError $ PandocParsecError (toSources toks) e

-- block elements:

preamble :: PandocMonad m => LP m Blocks
preamble = mconcat <$> many preambleBlock
  where preambleBlock =  (mempty <$ spaces1)
                     <|> macroDef (rawBlock "latex")
                     <|> filecontents
                     <|> (mempty <$ blockCommand)
                     <|> (mempty <$ braced)
                     <|> (do notFollowedBy (begin_ "document")
                             anyTok
                             return mempty)

rule :: PandocMonad m => LP m Blocks
rule = do
  skipopts
  width <- T.takeWhile (\c -> isDigit c || c == '.') . stringify <$> tok
  _thickness <- tok
  -- 0-width rules are used to fix spacing issues:
  case safeRead width of
    Just (0 :: Double) -> return mempty
    _ -> return horizontalRule

paragraph :: PandocMonad m => LP m Blocks
paragraph = do
  x <- trimInlines . mconcat <$> many1 inline
  if x == mempty
     then return mempty
     else return $ para x

rawBlockOr :: PandocMonad m => Text -> LP m Blocks -> LP m Blocks
rawBlockOr name fallback = do
  -- if raw_tex allowed, don't process
  parseRaw <- extensionEnabled Ext_raw_tex <$> getOption readerExtensions
  if parseRaw
     then rawBlock "latex" <$> getRawCommand name ("\\" <> name)
     else fallback

doSubfile :: PandocMonad m => LP m Blocks
doSubfile = do
  skipMany opt
  f <- T.unpack . removeDoubleQuotes . T.strip . untokenize <$> braced
  oldToks <- getInput
  setInput []
  insertIncluded ".tex" f
  bs <- blocks
  eof
  setInput oldToks
  return bs

include :: (PandocMonad m, Monoid a) => Text -> LP m a
include name = do
  skipMany opt
  fs <- map (T.unpack . removeDoubleQuotes . T.strip) . T.splitOn "," .
         untokenize <$> braced
  let defaultExt | name == "usepackage" = ".sty"
                 | otherwise            = ".tex"
  mapM_ (insertIncluded defaultExt) fs
  return mempty

readFileFromTexinputs :: PandocMonad m => FilePath -> LP m (Maybe Text)
readFileFromTexinputs fp = do
  fileContentsMap <- sFileContents <$> getState
  case M.lookup (T.pack fp) fileContentsMap of
    Just t -> return (Just t)
    Nothing -> do
      dirs <- map T.unpack . splitTextBy (==':') . fromMaybe "."
               <$> lookupEnv "TEXINPUTS"
      readFileFromDirs dirs fp

insertIncluded :: PandocMonad m
               => FilePath
               -> FilePath
               -> LP m ()
insertIncluded defaultExtension f' = do
  let f = case takeExtension f' of
                ".tex" -> f'
                ".sty" -> f'
                _      -> addExtension f' defaultExtension
  pos <- getPosition
  containers <- getIncludeFiles <$> getState
  when (T.pack f `elem` containers) $
    throwError $ PandocParseError $ T.pack $ "Include file loop at " ++ show pos
  updateState $ addIncludeFile $ T.pack f
  mbcontents <- readFileFromTexinputs f
  contents <- case mbcontents of
                   Just s -> return s
                   Nothing -> do
                     report $ CouldNotLoadIncludeFile (T.pack f) pos
                     return ""
  getInput >>= setInput . (tokenize f contents ++)
  updateState dropLatestIncludeFile

authors :: PandocMonad m => LP m ()
authors = try $ do
  bgroup
  let oneAuthor = blocksToInlines' . B.toList . mconcat <$> many1 block
  auths <- sepBy oneAuthor (controlSeq "and")
  egroup
  addMeta "author" (map trimInlines auths)

looseItem :: PandocMonad m => LP m Blocks
looseItem = do
  inListItem <- sInListItem <$> getState
  guard $ not inListItem
  skipopts
  return mempty

epigraph :: PandocMonad m => LP m Blocks
epigraph = do
  p1 <- grouped block
  p2 <- grouped block
  return $ divWith ("", ["epigraph"], []) (p1 <> p2)

section :: PandocMonad m => Attr -> Int -> LP m Blocks
section (ident, classes, kvs) lvl = do
  skipopts
  contents <- grouped inline
  lab <- option ident $
          try (spaces >> controlSeq "label"
               >> spaces >> untokenize <$> braced)
  when (lvl == 0) $
    updateState $ \st -> st{ sHasChapters = True }
  unless ("unnumbered" `elem` classes) $ do
    hn <- sLastHeaderNum <$> getState
    hasChapters <- sHasChapters <$> getState
    let lvl' = lvl + if hasChapters then 1 else 0
    let num = incrementDottedNum lvl' hn
    updateState $ \st -> st{ sLastHeaderNum = num
                           , sLabels = M.insert lab
                              [Str (renderDottedNum num)]
                              (sLabels st) }
  attr' <- registerHeader (lab, classes, kvs) contents
  return $ headerWith attr' lvl contents

blockCommand :: PandocMonad m => LP m Blocks
blockCommand = try $ do
  Tok _ (CtrlSeq name) txt <- anyControlSeq
  guard $ name /= "begin" && name /= "end" && name /= "and"
  star <- option "" ("*" <$ symbol '*' <* sp)
  let name' = name <> star
  let names = ordNub [name', name]
  let rawDefiniteBlock = do
        guard $ isBlockCommand name
        rawcontents <- getRawCommand name (txt <> star)
        (guardEnabled Ext_raw_tex >> return (rawBlock "latex" rawcontents))
          <|> ignore rawcontents
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
        rawcontents <- getRawCommand name (txt <> star)
        curr <- (guardEnabled Ext_raw_tex >>
                    return (rawBlock "latex" rawcontents))
                   <|> ignore rawcontents
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

parbox :: PandocMonad m => LP m Blocks
parbox = try $ do
  skipopts
  braced -- size
  oldInTableCell <- sInTableCell <$> getState
  -- see #5711
  updateState $ \st -> st{ sInTableCell = False }
  res <- grouped block
  updateState $ \st -> st{ sInTableCell = oldInTableCell }
  return res

blockCommands :: PandocMonad m => M.Map Text (LP m Blocks)
blockCommands = M.fromList
   [ ("par", mempty <$ skipopts)
   , ("parbox",  parbox)
   , ("title", mempty <$ (skipopts *>
                             (grouped inline >>= addMeta "title")
                         <|> (grouped block >>= addMeta "title")))
   , ("subtitle", mempty <$ (skipopts *> tok >>= addMeta "subtitle"))
   , ("author", mempty <$ (skipopts *> authors))
   -- -- in letter class, temp. store address & sig as title, author
   , ("address", mempty <$ (skipopts *> tok >>= addMeta "address"))
   , ("signature", mempty <$ (skipopts *> authors))
   , ("date", mempty <$ (skipopts *> tok >>= addMeta "date"))
   , ("newtheorem", newtheorem inline)
   , ("theoremstyle", theoremstyle)
   -- KOMA-Script metadata commands
   , ("extratitle", mempty <$ (skipopts *> tok >>= addMeta "extratitle"))
   , ("frontispiece", mempty <$ (skipopts *> tok >>= addMeta "frontispiece"))
   , ("titlehead", mempty <$ (skipopts *> tok >>= addMeta "titlehead"))
   , ("subject", mempty <$ (skipopts *> tok >>= addMeta "subject"))
   , ("publishers", mempty <$ (skipopts *> tok >>= addMeta "publishers"))
   , ("uppertitleback", mempty <$ (skipopts *> tok >>= addMeta "uppertitleback"))
   , ("lowertitleback", mempty <$ (skipopts *> tok >>= addMeta "lowertitleback"))
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
   , ("opening", para . trimInlines <$> (skipopts *> tok))
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
   , ("rule", rule)
   , ("item", looseItem)
   , ("documentclass", skipopts *> braced *> preamble)
   , ("centerline", para . trimInlines <$> (skipopts *> tok))
   , ("caption", mempty <$ setCaption inline)
   , ("bibliography", mempty <$ (skipopts *> braced >>=
         addMeta "bibliography" . splitBibs . untokenize))
   , ("addbibresource", mempty <$ (skipopts *> braced >>=
         addMeta "bibliography" . splitBibs . untokenize))
   , ("endinput", mempty <$ skipMany anyTok)
   -- includes
   , ("lstinputlisting", inputListing)
   , ("inputminted", inputMinted)
   , ("graphicspath", graphicsPath)
   -- polyglossia
   , ("setdefaultlanguage", setDefaultLanguage)
   , ("setmainlanguage", setDefaultLanguage)
   -- hyperlink
   , ("hypertarget", hypertargetBlock)
   -- LaTeX colors
   , ("textcolor", coloredBlock "color")
   , ("colorbox", coloredBlock "background-color")
   -- csquotes
   , ("blockquote", blockquote False Nothing)
   , ("blockcquote", blockquote True Nothing)
   , ("foreignblockquote", braced >>= blockquote False . Just . untokenize)
   , ("foreignblockcquote", braced >>= blockquote True . Just . untokenize)
   , ("hyphenblockquote", braced >>= blockquote False . Just . untokenize)
   , ("hyphenblockcquote", braced >>= blockquote True . Just . untokenize)
   -- include
   , ("include", rawBlockOr "include" $ include "include")
   , ("input", rawBlockOr "input" $ include "input")
   , ("subfile", rawBlockOr "subfile" doSubfile)
   , ("usepackage", rawBlockOr "usepackage" $ include "usepackage")
   -- preamble
   , ("PackageError", mempty <$ (braced >> braced >> braced))
   -- epigraph package
   , ("epigraph", epigraph)
   ]


environments :: PandocMonad m => M.Map Text (LP m Blocks)
environments = M.union (tableEnvironments blocks inline) $
   M.fromList
   [ ("document", env "document" blocks <* skipMany anyTok)
   , ("abstract", mempty <$ (env "abstract" blocks >>= addMeta "abstract"))
   , ("sloppypar", env "sloppypar" blocks)
   , ("letter", env "letter" letterContents)
   , ("minipage", env "minipage" $
          skipopts *> spaces *> optional braced *> spaces *> blocks)
   , ("figure", env "figure" $ skipopts *> figure)
   , ("subfigure", env "subfigure" $ skipopts *> tok *> figure)
   , ("center", divWith ("", ["center"], []) <$> env "center" blocks)
   , ("quote", blockQuote <$> env "quote" blocks)
   , ("quotation", blockQuote <$> env "quotation" blocks)
   , ("verse", blockQuote <$> env "verse" blocks)
   , ("itemize", bulletList <$> listenv "itemize" (many item))
   , ("description", definitionList <$> listenv "description" (many descItem))
   , ("enumerate", orderedList')
   , ("alltt", alltt <$> env "alltt" blocks)
   , ("code", guardEnabled Ext_literate_haskell *>
       (codeBlockWith ("",["haskell","literate"],[]) <$> verbEnv "code"))
   , ("comment", mempty <$ verbEnv "comment")
   , ("verbatim", codeBlock <$> verbEnv "verbatim")
   , ("Verbatim", fancyverbEnv "Verbatim")
   , ("BVerbatim", fancyverbEnv "BVerbatim")
   , ("lstlisting", do attr <- parseListingsOptions <$> option [] keyvals
                       codeBlockWith attr <$> verbEnv "lstlisting")
   , ("minted", minted)
   , ("obeylines", obeylines)
   , ("tikzpicture", rawVerbEnv "tikzpicture")
   , ("tikzcd", rawVerbEnv "tikzcd")
   , ("lilypond", rawVerbEnv "lilypond")
   , ("ly", rawVerbEnv "ly")
   -- amsthm
   , ("proof", proof blocks opt)
   -- etoolbox
   , ("ifstrequal", ifstrequal)
   , ("newtoggle", braced >>= newToggle)
   , ("toggletrue", braced >>= setToggle True)
   , ("togglefalse", braced >>= setToggle False)
   , ("iftoggle", try $ ifToggle >> block)
   ]

filecontents :: PandocMonad m => LP m Blocks
filecontents = try $ do
  controlSeq "begin"
  name <- untokenize <$> braced
  guard $ name == "filecontents" || name == "filecontents*"
  skipopts
  fp <- untokenize <$> braced
  txt <- verbEnv name
  updateState $ \st ->
    st{ sFileContents = M.insert fp txt (sFileContents st) }
  return mempty

environment :: PandocMonad m => LP m Blocks
environment = try $ do
  controlSeq "begin"
  name <- untokenize <$> braced
  M.findWithDefault mzero name environments <|>
    theoremEnvironment blocks opt name <|>
    if M.member name (inlineEnvironments
                       :: M.Map Text (LP PandocPure Inlines))
       then mzero
       else try (rawEnv name) <|> rawVerbEnv name

rawEnv :: PandocMonad m => Text -> LP m Blocks
rawEnv name = do
  exts <- getOption readerExtensions
  let parseRaw = extensionEnabled Ext_raw_tex exts
  rawOptions <- mconcat <$> many rawopt
  let beginCommand = "\\begin{" <> name <> "}" <> rawOptions
  pos1 <- getPosition
  if parseRaw
     then do
       (_, raw) <- withRaw $ env name blocks
       return $ rawBlock "latex"
                 $ beginCommand <> untokenize raw
     else do
       bs <- env name blocks
       report $ SkippedContent beginCommand pos1
       pos2 <- getPosition
       report $ SkippedContent ("\\end{" <> name <> "}") pos2
       return $ divWith ("",[name],[]) bs

rawVerbEnv :: PandocMonad m => Text -> LP m Blocks
rawVerbEnv name = do
  pos <- getPosition
  (_, raw) <- withRaw $ verbEnv name
  let raw' = "\\begin{" <> name <> "}" <> untokenize raw
  exts <- getOption readerExtensions
  let parseRaw = extensionEnabled Ext_raw_tex exts
  if parseRaw
     then return $ rawBlock "latex" raw'
     else do
       report $ SkippedContent raw' pos
       return mempty

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
  attr <- mintedAttr
  codeBlockWith attr <$> verbEnv "minted"

mintedAttr :: PandocMonad m => LP m Attr
mintedAttr = do
  options <- option [] keyvals
  lang <- untokenize <$> braced
  let kvs = [ (if k == "firstnumber"
                  then "startFrom"
                  else k, v) | (k,v) <- options ]
  let classes = [ lang | not (T.null lang) ] ++
                [ "numberLines" |
                  lookup "linenos" options == Just "true" ]
  return ("",classes,kvs)

inputMinted :: PandocMonad m => LP m Blocks
inputMinted = do
  pos <- getPosition
  attr <- mintedAttr
  f <- T.filter (/='"') . untokenize <$> braced
  mbCode <- readFileFromTexinputs (T.unpack f)
  rawcode <- case mbCode of
                  Just s -> return s
                  Nothing -> do
                    report $ CouldNotLoadIncludeFile f pos
                    return ""
  return $ B.codeBlockWith attr rawcode

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
  where go (Image attr@(_, cls, kvs) alt (src,tit))
            | not ("fig:" `T.isPrefixOf` tit) = do
          st <- getState
          let (alt', tit') = case sCaption st of
                               Just ils -> (toList ils, "fig:" <> tit)
                               Nothing  -> (alt, tit)
              attr' = case sLastLabel st of
                        Just lab -> (lab, cls, kvs)
                        Nothing  -> attr
          case attr' of
               ("", _, _)    -> return ()
               (ident, _, _) -> do
                  num <- getNextNumber sLastFigureNum
                  setState
                    st{ sLastFigureNum = num
                      , sLabels = M.insert ident
                                 [Str (renderDottedNum num)] (sLabels st) }
          return $ Image attr' alt' (src, tit')
        go x = return x

coloredBlock :: PandocMonad m => Text -> LP m Blocks
coloredBlock stylename = try $ do
  skipopts
  color <- braced
  notFollowedBy (grouped inline)
  let constructor = divWith ("",[],[("style",stylename <> ": " <> untokenize color)])
  constructor <$> grouped block

graphicsPath :: PandocMonad m => LP m Blocks
graphicsPath = do
  ps <- map (T.unpack . untokenize) <$>
          (bgroup *> spaces *> manyTill (braced <* spaces) egroup)
  getResourcePath >>= setResourcePath . (<> ps)
  return mempty

splitBibs :: Text -> [Inlines]
splitBibs = map (str . T.pack . flip replaceExtension "bib" . T.unpack . trim) . splitTextBy (==',')

alltt :: Blocks -> Blocks
alltt = walk strToCode
  where strToCode (Str s)   = Code nullAttr s
        strToCode Space     = RawInline (Format "latex") "\\ "
        strToCode SoftBreak = LineBreak
        strToCode x         = x

parseListingsOptions :: [(Text, Text)] -> Attr
parseListingsOptions options =
  let kvs = [ (if k == "firstnumber"
                  then "startFrom"
                  else k, v) | (k,v) <- options ]
      classes = [ "numberLines" |
                  lookup "numbers" options == Just "left" ]
             ++ maybeToList (listingsLanguage options)
  in  (fromMaybe "" (lookup "label" options), classes, kvs)

inputListing :: PandocMonad m => LP m Blocks
inputListing = do
  pos <- getPosition
  options <- option [] keyvals
  f <- T.filter (/='"') . untokenize <$> braced
  mbCode <- readFileFromTexinputs (T.unpack f)
  codeLines <- case mbCode of
                      Just s -> return $ T.lines s
                      Nothing -> do
                        report $ CouldNotLoadIncludeFile f pos
                        return []
  let (ident,classes,kvs) = parseListingsOptions options
  let classes' =
        (case listingsLanguage options of
           Nothing -> (take 1 (languagesByExtension defaultSyntaxMap
                                (T.pack $ takeExtension $ T.unpack f)) <>)
           Just _  -> id) classes
  let firstline = fromMaybe 1 $ lookup "firstline" options >>= safeRead
  let lastline = fromMaybe (length codeLines) $
                       lookup "lastline" options >>= safeRead
  let codeContents = T.intercalate "\n" $ take (1 + lastline - firstline) $
                       drop (firstline - 1) codeLines
  return $ codeBlockWith (ident,classes',kvs) codeContents

-- lists

item :: PandocMonad m => LP m Blocks
item = void blocks *> controlSeq "item" *> skipopts *> blocks

descItem :: PandocMonad m => LP m (Inlines, [Blocks])
descItem = do
  blocks -- skip blocks before item
  controlSeq "item"
  sp
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
        ts <- untokenize <$> manyTill anyTok (symbol ']')
        case runParser anyOrderedListMarker def "option" ts of
             Right r -> return r
             Left _  -> do
               pos <- getPosition
               report $ SkippedContent ("[" <> ts <> "]") pos
               return (1, DefaultStyle, DefaultDelim)
  (_, style, delim) <- option (1, DefaultStyle, DefaultDelim) markerSpec
  spaces
  optional $ try $ controlSeq "setlength"
                   *> grouped (count 1 $ controlSeq "itemindent")
                   *> braced
  spaces
  start <- option 1 $ try $ do pos <- getPosition
                               controlSeq "setcounter"
                               ctr <- untokenize <$> braced
                               guard $ "enum" `T.isPrefixOf` ctr
                               guard $ T.all (`elem` ['i','v']) (T.drop 4 ctr)
                               sp
                               num <- untokenize <$> braced
                               case safeRead num of
                                    Just i -> return (i + 1 :: Int)
                                    Nothing -> do
                                      report $ SkippedContent
                                        ("\\setcounter{" <> ctr <>
                                         "}{" <> num <> "}") pos
                                      return 1
  bs <- listenv "enumerate" (many item)
  return $ orderedListWith (start, style, delim) bs

block :: PandocMonad m => LP m Blocks
block = do
  Tok _ toktype _ <- lookAhead anyTok
  res <- (case toktype of
            Newline           -> mempty <$ spaces1
            Spaces            -> mempty <$ spaces1
            Comment           -> mempty <$ spaces1
            Word              -> paragraph
            CtrlSeq "begin"   -> environment
            CtrlSeq _         -> macroDef (rawBlock "latex")
                               <|> blockCommand
            _                 -> mzero)
          <|> paragraph
          <|> grouped block
  trace (T.take 60 $ tshow $ B.toList res)
  return res

blocks :: PandocMonad m => LP m Blocks
blocks = mconcat <$> many block

