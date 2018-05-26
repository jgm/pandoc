{-# LANGUAGE FlexibleContexts #-}
{-
  Copyright (C) 2018 Yan Pashkovsky <yanp.bugz@gmail.com>

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
   Module      : Text.Pandoc.Readers.Man
   Copyright   : Copyright (C) 2018 Yan Pashkovsky
   License     : GNU GPL, version 2 or above

   Maintainer  : Yan Pashkovsky <yanp.bugz@gmail.com>
   Stability   : WIP
   Portability : portable

Conversion of man to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Man (readMan, testFile) where

import Prelude
import Control.Monad (liftM)
import Control.Monad.Except (throwError)
import Data.Default (Default)
import Data.Map (insert)
import Data.Maybe (catMaybes)
import Data.List (intersperse, intercalate)
import qualified Data.Text as T

import Text.Pandoc.Class (PandocMonad(..), runIOorExplode)
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (PandocParsecError))
import Text.Pandoc.Logging (LogMessage(..))
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Shared (crFilter)
import Text.Parsec hiding (tokenPrim)
import Text.Parsec.Char ()
import Text.Parsec.Pos (updatePosString)

--
-- Data Types
--

data FontKind = Regular | Italic | Bold | ItalicBold deriving Show

data MacroKind = KTitle
               | KCodeBlStart
               | KCodeBlEnd
               | KTab
               | KTabEnd
               deriving (Show, Eq)

-- TODO header strings
-- TODO remove MStr
-- TODO filter skipped content
data ManToken = MStr String FontKind
              | MLine [(String, FontKind)]
              | MMaybeLink String
              | MEmptyLine
              | MHeader Int String
              | MMacro MacroKind [String]
              | MUnknownMacro String [String]
              | MComment String
              deriving Show

data EscapeThing = EFont FontKind
                 | EChar Char
                 | ENothing
                 deriving Show

data RoffState = RoffState { fontKind :: FontKind
                           } deriving Show

instance Default RoffState where
  def = RoffState {fontKind = Regular}

type ManLexer m = ParserT [Char] RoffState m
type ManParser m = ParserT [ManToken] ParserState m

----
-- testStrr :: [Char] -> Either PandocError Pandoc
-- testStrr s = runPure $ readMan def (T.pack s)

printPandoc :: Pandoc -> [Char]
printPandoc (Pandoc m content) =
  let ttl = "Pandoc: " ++ (show $ unMeta m)
      cnt = intercalate "\n" $ map show content
  in ttl ++ "\n" ++ cnt

-- strrepr :: Either PandocError Pandoc -> [Char]
-- strrepr obj = case obj of
--   Right x -> printPandoc x
--   Left y -> show y

testFile :: FilePath -> IO ()
testFile fname = do
  cont <- readFile fname
  pand <- runIOorExplode $ readMan def (T.pack cont)
  putStrLn $ printPandoc pand
----


-- | Read man (troff) from an input string and return a Pandoc document.
readMan :: PandocMonad m => ReaderOptions -> T.Text -> m Pandoc
readMan opts txt = do
  eithertokens <- readWithM lexMan def (T.unpack $ crFilter txt)
  case eithertokens of
    Right tokenz -> do
      let state = def {stateOptions = opts} :: ParserState
      eitherdoc <- readWithMTokens parseMan state tokenz
      case eitherdoc of
        Right doc -> return doc
        Left e -> throwError e
    Left e       -> throwError e

  where

  readWithMTokens :: PandocMonad m
          => ParserT [ManToken] ParserState m a  -- ^ parser
          -> ParserState                         -- ^ initial state
          -> [ManToken]                       -- ^ input
          -> m (Either PandocError a)
  readWithMTokens parser state input =
    mapLeft (PandocParsecError . (intercalate "\n") $ show <$> input) `liftM` runParserT parser state "source" input

  mapLeft :: (a -> c) -> Either a b -> Either c b
  mapLeft f (Left x) = Left $ f x
  mapLeft _ (Right r) = Right r

--
-- String -> ManToken function
--

lexMan :: PandocMonad m => ManLexer m [ManToken]
lexMan = many (lexMacro <|> lexLine <|> lexEmptyLine)

parseMan :: PandocMonad m => ManParser m Pandoc
parseMan = do
  let parsers = [ parseBulletList, parseTitle, parsePara, parseSkippedContent
                , parseCodeBlock, parseHeader, parseSkipMacro]
  blocks <- many $ choice parsers
  parserst <- getState
  return $ Pandoc (stateMeta parserst) (filter (not . isNull) blocks)

  where

  isNull Null = True
  isNull _    = False

eofline :: Stream s m Char => ParsecT s u m ()
eofline = (newline >> return ()) <|> eof

spacetab :: Stream s m Char => ParsecT s u m Char
spacetab = char ' ' <|> char '\t'

-- TODO handle more cases
escapeLexer :: PandocMonad m => ManLexer m EscapeThing
escapeLexer = do
  char '\\'
  choice [escChar, escFont]
  where

  escChar :: PandocMonad m => ManLexer m EscapeThing
  escChar =
    let skipSeqs = ["%", "{", "}", "&"]
        subsSeqs = [ ("-", '-'), (" ", ' '), ("\\", '\\'), ("[lq]", '“'), ("[rq]", '”')
                    , ("[em]", '—'), ("[en]", '–') ]
        substitute :: PandocMonad m =>  (String,Char) -> ManLexer m EscapeThing
        substitute (from,to) = try $ string from >> return (EChar to)
        skip :: PandocMonad m =>  String -> ManLexer m EscapeThing
        skip seq' = try $ string seq' >> return ENothing
    in choice $ (substitute <$> subsSeqs) ++
                (skip <$> skipSeqs) ++
              [ char '(' >> anyChar >> return ENothing
              , char '[' >> many alphaNum >> char ']' >> return ENothing
              ]

  escFont :: PandocMonad m => ManLexer m EscapeThing
  escFont = do
    char 'f'
    font <- choice [ letterFont
          , char '(' >> anyChar >> anyChar >> return Regular
          , try (char '[' >> letterFont >>= \f -> char ']'  >> return f)
          , try $ string "[BI]" >> return ItalicBold
          , char '[' >> many letter >> char ']'  >> return Regular
          ]
    modifyState (\r -> r {fontKind = font})
    return $ EFont font

    where

    letterFont :: PandocMonad m => ManLexer m FontKind
    letterFont = choice [
        char 'B' >> return Bold
      , char 'I' >> return Italic
      , char 'P' >> return Regular
      ]

lexMacro :: PandocMonad m => ManLexer m ManToken
lexMacro = do
  char '.' <|> char '\''
  many spacetab
  macroName <- many1 (letter <|> oneOf ['\\', '"'])
  args <- lexArgs
  let joinedArgs = unwords args
  let knownMacro mkind = MMacro mkind args

  let tok = case macroName of
              x | x `elem` ["\\\"", "\\#"] -> MComment joinedArgs
              "TH"   -> knownMacro KTitle
              "IP"   -> knownMacro KTab
              "TP"   -> knownMacro KTab
              "RE"   -> knownMacro KTabEnd
              "nf"   -> knownMacro KCodeBlStart
              "fi"   -> knownMacro KCodeBlEnd
              "B"    -> MStr joinedArgs Bold
              "BR"   -> MMaybeLink joinedArgs
              x | x `elem` ["BI", "IB"] -> MStr joinedArgs ItalicBold
              x | x `elem` ["I", "IR", "RI"]  -> MStr joinedArgs Italic
              "SH"   -> MHeader 2 joinedArgs
              "SS"   -> MHeader 3 joinedArgs
              x | x `elem` [ "P", "PP", "LP", "sp"] -> MEmptyLine
              _      -> MUnknownMacro macroName args
  return tok

  where

  lexArgs :: PandocMonad m => ManLexer m [String]
  lexArgs = do
    args <- many oneArg
    eofline
    return args

    where

    oneArg :: PandocMonad m => ManLexer m String
    oneArg = do
      many1 spacetab
      quotedArg <|> plainArg

    plainArg :: PandocMonad m => ManLexer m String
    plainArg = fmap catMaybes . many1 $ escChar <|> (Just <$> noneOf " \t\n")

    quotedArg :: PandocMonad m => ManLexer m String
    quotedArg = do
      char '"'
      val <- catMaybes <$> many quotedChar
      char '"'
      return val

    quotedChar :: PandocMonad m => ManLexer m (Maybe Char)
    quotedChar = escChar <|> (Just <$> noneOf "\"\n") <|> (Just <$> try (string "\"\"" >> return '"'))

    escChar :: PandocMonad m => ManLexer m (Maybe Char)
    escChar = do
      ec <- escapeLexer
      case ec of
        (EChar c) -> return $ Just c
        _ -> return Nothing

lexLine :: PandocMonad m => ManLexer m ManToken
lexLine = do
  lnparts <- many1 (esc <|> linePart)
  eofline
  return $ MLine $ catMaybes lnparts
  where

  esc :: PandocMonad m => ManLexer m (Maybe (String, FontKind))
  esc = do
    someesc <- escapeLexer
    font <- currentFont
    let rv = case someesc of
               EChar c -> Just ([c], font)
               _ -> Nothing
    return rv

  linePart :: PandocMonad m => ManLexer m (Maybe (String, FontKind))
  linePart = do
    lnpart <- many1 $ noneOf "\n\\"
    font <- currentFont
    return $ Just (lnpart, font)
  
  currentFont :: PandocMonad m => ManLexer m FontKind
  currentFont = do
    RoffState {fontKind = fk} <- getState
    return fk

    
lexEmptyLine :: PandocMonad m => ManLexer m ManToken
lexEmptyLine = char '\n' >> return MEmptyLine

--
-- ManToken parsec functions
--

msatisfy :: (Show t, Stream s m t) => (t -> Bool) -> ParserT s st m t
msatisfy predic = tokenPrim show nextPos testTok
  where
    testTok t     = if predic t then Just t else Nothing
    nextPos pos x _xs  = updatePosString pos (show x)

mstr :: PandocMonad m => ManParser m ManToken
mstr = msatisfy isMStr where
  isMStr (MStr _ _) = True
  isMStr _ = False

mline :: PandocMonad m => ManParser m ManToken
mline = msatisfy isMLine where
  isMLine (MLine _) = True
  isMLine _ = False

mmaybeLink :: PandocMonad m => ManParser m ManToken
mmaybeLink = msatisfy isMMaybeLink where
  isMMaybeLink (MMaybeLink _) = True
  isMMaybeLink _ = False

memplyLine :: PandocMonad m => ManParser m ManToken
memplyLine = msatisfy isMEmptyLine where
  isMEmptyLine MEmptyLine = True
  isMEmptyLine _ = False

mheader :: PandocMonad m => ManParser m ManToken
mheader = msatisfy isMHeader where
  isMHeader (MHeader _ _) = True
  isMHeader _ = False

mmacro :: PandocMonad m => MacroKind -> ManParser m ManToken
mmacro mk = msatisfy isMMacro where
  isMMacro (MMacro mk' _) | mk == mk' = True
                          | otherwise = False
  isMMacro _ = False

mmacroAny :: PandocMonad m => ManParser m ManToken
mmacroAny = msatisfy isMMacro where
  isMMacro (MMacro _ _) = True
  isMMacro _ = False

munknownMacro :: PandocMonad m => ManParser m ManToken
munknownMacro = msatisfy isMUnknownMacro where
  isMUnknownMacro (MUnknownMacro _ _) = True
  isMUnknownMacro _ = False

mcomment :: PandocMonad m => ManParser m ManToken
mcomment = msatisfy isMComment where
  isMComment (MComment _) = True
  isMComment _ = False

--
-- ManToken -> Block functions
--

parseTitle :: PandocMonad m => ManParser m Block
parseTitle = do
  (MMacro _ args) <- mmacro KTitle
  if null args
    then return Null
    else do
         let mantitle = head args
         modifyState (changeTitle mantitle)
         return $ Header 1 nullAttr [Str mantitle]
  where
  changeTitle title pst =
    let meta = stateMeta pst
        metaUp = Meta $ insert "title" (MetaString title) (unMeta meta)
    in
    pst {stateMeta = metaUp}
      
parseSkippedContent :: PandocMonad m => ManParser m Block
parseSkippedContent = do
  tok <- munknownMacro <|> mcomment <|> memplyLine
  onToken tok
  return Null

  where

  onToken :: PandocMonad m => ManToken -> ManParser m ()
  onToken (MUnknownMacro mname _) = do
    pos <- getPosition
    logMessage $ SkippedContent ("Unknown macro: " ++ mname) pos
  onToken _ = return ()

strToInline :: String -> FontKind -> Inline
strToInline s Regular = Str s
strToInline s Italic = Emph [Str s]
strToInline s Bold = Strong [Str s]
strToInline s ItalicBold = Strong [Emph [Str s]]

parsePara :: PandocMonad m => ManParser m Block
parsePara = do
  inls <- many1 (strInl <|> lineInl <|> linkInl <|> comment)
  let withspaces = intersperse [Space] inls
  return $ Para (concat withspaces)

  where

  strInl :: PandocMonad m => ManParser m [Inline]
  strInl = do
    (MStr str fk) <- mstr
    return [strToInline str fk]

  lineInl :: PandocMonad m => ManParser m [Inline]
  lineInl = do
    (MLine fragments) <- mline
    return $ fmap (\(s,f) -> strToInline s f) fragments

  linkInl :: PandocMonad m => ManParser m [Inline]
  linkInl = do
    (MMaybeLink txt) <- mmaybeLink
    let inls = case runParser linkParser () "" txt of
                  Right lnk -> lnk
                  Left _ -> [Strong [Str txt]]
    return inls

    where

    -- assuming man pages are generated from Linux-like repository
    linkParser :: Parsec String () [Inline]
    linkParser = do
      mpage <- many1 (alphaNum <|> char '_')
      spacetab
      char '('
      mansect <- digit
      char ')'
      other <- many anyChar
      let manurl pagename section = "../"++section++"/"++pagename++"."++section
      return $ [ Link nullAttr [Strong [Str mpage]] (manurl mpage [mansect], mpage)
               , Strong [Str $ " ("++[mansect] ++ ")"
               , Str other]
               ]

  comment :: PandocMonad m => ManParser m [Inline]
  comment = mcomment >> return []


parseCodeBlock :: PandocMonad m => ManParser m Block
parseCodeBlock = do
  mmacro KCodeBlStart
  toks <- many (mstr <|> mline <|> mmaybeLink <|> memplyLine <|> munknownMacro <|> mcomment)
  mmacro KCodeBlEnd
  return $ CodeBlock nullAttr (intercalate "\n" . catMaybes $ extractText <$> toks)

  where

  extractText :: ManToken -> Maybe String
  extractText (MStr s _) = Just s
  extractText (MLine ss) = Just . concat $ map fst ss -- TODO maybe unwords?
  extractText (MMaybeLink s) = Just s
  extractText MEmptyLine = Just "" -- string are intercalated with '\n', this prevents double '\n'
  extractText _ = Nothing

parseHeader :: PandocMonad m => ManParser m Block
parseHeader = do
  (MHeader lvl s) <- mheader
  return $ Header lvl nullAttr [Str s]

parseBulletList :: PandocMonad m => ManParser m Block
parseBulletList = BulletList . map (: []) <$> many1 block

  where

  block :: PandocMonad m => ManParser m Block
  block = do
    mmacro KTab
    pars <- parsePara
    many $ mmacro KTabEnd
    return pars

-- In case of weird man file it will be parsed succesfully
parseSkipMacro :: PandocMonad m => ManParser m Block
parseSkipMacro = mmacroAny >> return Null
