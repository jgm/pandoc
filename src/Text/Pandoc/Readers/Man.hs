{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
  Copyright (C) 2018 Yan Pashkovsky <yanp.bugz@gmail.com>
                     and John MacFarlane

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
   Copyright   : Copyright (C) 2018 Yan Pashkovsky and John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Yan Pashkovsky <yanp.bugz@gmail.com>
   Stability   : WIP
   Portability : portable

Conversion of man to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Man (readMan) where

import Prelude
import Safe (lastMay)
import Data.Char (toLower)
import Data.Default (Default)
import Control.Monad (liftM, mzero, guard)
import Control.Monad.Trans (lift)
import Control.Monad.Except (throwError)
import Text.Pandoc.Class (PandocMonad(..), report)
import Data.Maybe (catMaybes, isJust)
import Data.List (intersperse, intercalate)
import qualified Data.Text as T
import Text.Pandoc.Builder as B
import Text.Pandoc.Error (PandocError (PandocParsecError))
import Text.Pandoc.Logging (LogMessage(..))
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Shared (crFilter)
import Text.Pandoc.Readers.Groff  -- TODO explicit imports
import Text.Parsec hiding (tokenPrim)
import qualified Text.Parsec as Parsec
import Text.Parsec.Pos (updatePosString)
import qualified Data.Foldable as Foldable

data ManState = ManState { readerOptions :: ReaderOptions
                         , metadata      :: Meta
                         } deriving Show

instance Default ManState where
  def = ManState { readerOptions = def
                 , metadata      = nullMeta }

type ManParser m = ParserT [GroffToken] ManState m


-- | Read man (troff) from an input string and return a Pandoc document.
readMan :: PandocMonad m => ReaderOptions -> T.Text -> m Pandoc
readMan opts txt = do
  tokenz <- lexGroff (crFilter txt)
  let state = def {readerOptions = opts} :: ManState
  eitherdoc <- readWithMTokens parseMan state
     (Foldable.toList . unGroffTokens $ tokenz)
  either throwError return eitherdoc

readWithMTokens :: PandocMonad m
        => ParserT [GroffToken] ManState m a  -- ^ parser
        -> ManState                         -- ^ initial state
        -> [GroffToken]                       -- ^ input
        -> m (Either PandocError a)
readWithMTokens parser state input =
  let leftF = PandocParsecError . intercalate "\n" $ show <$> input
  in mapLeft leftF `liftM` runParserT parser state "source" input

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right r) = Right r


parseMan :: PandocMonad m => ManParser m Pandoc
parseMan = do
  bs <- many parseBlock <* eof
  meta <- metadata <$> getState
  let (Pandoc _ blocks) = doc $ mconcat bs
  return $ Pandoc meta blocks

parseBlock :: PandocMonad m => ManParser m Blocks
parseBlock = choice [ parseList
                    , parseDefinitionList
                    , parseBlockQuote
                    , parseTitle
                    , parseNewParagraph
                    , parsePara
                    , parseCodeBlock
                    , parseHeader
                    , parseTable
                    , skipUnkownMacro
                    ]

parseTable :: PandocMonad m => ManParser m Blocks
parseTable = do
  let isMTable (MTable{}) = True
      isMTable _          = False
  MTable _opts aligns rows pos <- msatisfy isMTable
  case lastMay aligns of
    Just as -> try (do
      let as' = map (columnTypeToAlignment . columnType) as
      guard $ all isJust as'
      let alignments = catMaybes as'
      let (headerRow', bodyRows') =
            case rows of
              (h:[x]:bs)
               | isHrule x -> (h, bs)
              _ -> ([], rows)
      headerRow <- mapM parseTableCell headerRow'
      bodyRows <- mapM (mapM parseTableCell) bodyRows'
      return $ B.table mempty (zip alignments (repeat 0.0))
                  headerRow bodyRows) <|> fallback pos
    Nothing -> fallback pos

 where

  parseTableCell ts = do
    st <- getState
    let ts' = Foldable.toList $ unGroffTokens ts
    let tcell = try $ do
          skipMany memptyLine
          plain . trimInlines <$> (parseInlines <* eof)
    res <- if null ts'
              then return $ Right mempty
              else lift $ readWithMTokens tcell st ts'
    case res of
      Left e  -> throwError e
      Right x -> return x

  isHrule :: GroffTokens -> Bool
  isHrule (GroffTokens ss) =
    case Foldable.toList ss of
      [MLine [RoffStr [c]]] -> c `elem` ['_','-','=']
      _                     -> False

  fallback pos = do
    report $ SkippedContent "TABLE" pos
    return $ B.para (B.text "TABLE")

  columnTypeToAlignment :: Char -> Maybe Alignment
  columnTypeToAlignment c =
    case toLower c of
      'a' -> Just AlignLeft
      'c' -> Just AlignCenter
      'l' -> Just AlignLeft
      'n' -> Just AlignRight
      'r' -> Just AlignRight
      _   -> Nothing


parseNewParagraph :: PandocMonad m => ManParser m Blocks
parseNewParagraph = do
  mmacro "P" <|> mmacro "PP" <|> mmacro "LP" <|> memptyLine
  return mempty

--
-- Parser: [GroffToken] -> Pandoc
--

msatisfy :: Monad m => (GroffToken -> Bool) -> ParserT [GroffToken] st m GroffToken
msatisfy predic = tokenPrim show nextPos testTok
  where
    testTok t     = if predic t then Just t else Nothing
    nextPos _pos _x (MMacro _ _ pos':_) = pos'
    nextPos pos _x _xs  = updatePosString
                             (setSourceColumn
                               (setSourceLine pos $ sourceLine pos + 1) 1) ""

mtoken :: PandocMonad m => ManParser m GroffToken
mtoken = msatisfy (const True)

mline :: PandocMonad m => ManParser m GroffToken
mline = msatisfy isMLine where
  isMLine (MLine _) = True
  isMLine _ = False

memptyLine :: PandocMonad m => ManParser m GroffToken
memptyLine = msatisfy isMEmptyLine where
  isMEmptyLine MEmptyLine = True
  isMEmptyLine _ = False

mmacro :: PandocMonad m => MacroKind -> ManParser m GroffToken
mmacro mk = msatisfy isMMacro where
  isMMacro (MMacro mk' _ _) | mk == mk' = True
                            | otherwise = False
  isMMacro _ = False

mmacroAny :: PandocMonad m => ManParser m GroffToken
mmacroAny = msatisfy isMMacro where
  isMMacro (MMacro{}) = True
  isMMacro _ = False

--
-- GroffToken -> Block functions
--

parseTitle :: PandocMonad m => ManParser m Blocks
parseTitle = do
  (MMacro _ args _) <- mmacro "TH"
  let adjustMeta =
       case args of
         (x:y:z:_) -> setMeta "title" (linePartsToInlines x) .
                      setMeta "section" (linePartsToInlines y) .
                      setMeta "date" (linePartsToInlines z)
         [x,y]     -> setMeta "title" (linePartsToInlines x) .
                      setMeta "section" (linePartsToInlines y)
         [x]       -> setMeta "title" (linePartsToInlines x)
         []        -> id
  modifyState $ \st -> st{ metadata = adjustMeta $ metadata st }
  return mempty

linePartsToInlines :: [LinePart] -> Inlines
linePartsToInlines = go False

  where
  go :: Bool -> [LinePart] -> Inlines
  go _ [] = mempty
  go mono (MacroArg _:xs) = go mono xs -- shouldn't happen
  go mono (RoffStr s : RoffStr t : xs) = go mono (RoffStr (s <> t):xs)
  go mono (RoffStr s : xs)
    | mono      = code s <> go mono xs
    | otherwise = text s <> go mono xs
  go mono (Font fs: xs) =
    if litals > 0 && litals >= lbolds && litals >= lmonos
       then emph (go mono (Font fs{ fontItalic = False } :
                   map (adjustFontSpec (\s -> s{ fontItalic = False }))
                   itals)) <>
            go mono italsrest
       else if lbolds > 0 && lbolds >= lmonos
            then strong (go mono (Font fs{ fontBold = False } :
                   map (adjustFontSpec (\s -> s{ fontBold = False }))
                   bolds)) <>
                 go mono boldsrest
            else if lmonos > 0
                 then go True (Font fs{ fontMonospace = False } :
                    map (adjustFontSpec (\s -> s { fontMonospace = False }))
                    monos) <> go mono monosrest
                 else go mono xs
    where
      adjustFontSpec f (Font fspec) = Font (f fspec)
      adjustFontSpec _ x            = x
      withFont f (Font fspec) = f fspec
      withFont _ _            = False
      litals = length itals
      lbolds = length bolds
      lmonos = length monos
      (itals, italsrest) =
        if fontItalic fs
           then break (withFont (not . fontItalic)) xs
           else ([], xs)
      (bolds, boldsrest) =
        if fontBold fs
           then break (withFont (not . fontBold)) xs
           else ([], xs)
      (monos, monosrest) =
        if fontMonospace fs
           then break (withFont (not . fontMonospace)) xs
           else ([], xs)
  go mono (FontSize _fs : xs) = go mono xs

parsePara :: PandocMonad m => ManParser m Blocks
parsePara = para . trimInlines <$> parseInlines

parseInlines :: PandocMonad m => ManParser m Inlines
parseInlines = mconcat . intersperse B.space <$> many1 parseInline

parseInline :: PandocMonad m => ManParser m Inlines
parseInline = try $ do
  tok <- mtoken
  case tok of
    MLine lparts -> return $ linePartsToInlines lparts
    MMacro mname args _pos ->
      case mname of
        "UR" -> parseLink args
        "MT" -> parseEmailLink args
        "B"  -> parseBold args
        "I"  -> parseItalic args
        "br" -> return linebreak
        "BI" -> parseAlternatingFonts [strong, emph] args
        "IB" -> parseAlternatingFonts [emph, strong] args
        "IR" -> parseAlternatingFonts [emph, id] args
        "RI" -> parseAlternatingFonts [id, emph] args
        "BR" -> parseAlternatingFonts [strong, id] args
        "RB" -> parseAlternatingFonts [id, strong] args
        "SY" -> return $ strong $ mconcat $ intersperse B.space
                       $ map linePartsToInlines args
        "YS" -> return mempty
        "OP" -> case args of
                  (x:ys) -> return $ B.space <> str "[" <> B.space <>
                             mconcat (strong (linePartsToInlines x) :
                               map ((B.space <>) . linePartsToInlines) ys)
                             <> B.space <> str "]"
                  []     -> return mempty
        _ -> mzero
    _ -> mzero

parseBold :: PandocMonad m => [Arg] -> ManParser m Inlines
parseBold [] = do
  MLine lparts <- mline
  return $ strong $ linePartsToInlines lparts
parseBold args = return $
  strong $ mconcat $ intersperse B.space $ map linePartsToInlines args

parseItalic :: PandocMonad m => [Arg] -> ManParser m Inlines
parseItalic [] = do
  MLine lparts <- mline
  return $ emph $ linePartsToInlines lparts
parseItalic args = return $
  emph $ mconcat $ intersperse B.space $ map linePartsToInlines args

parseAlternatingFonts :: PandocMonad m
                      => [Inlines -> Inlines]
                      -> [Arg]
                      -> ManParser m Inlines
parseAlternatingFonts constructors args = return $ mconcat $
  zipWith (\f arg -> f (linePartsToInlines arg)) (cycle constructors) args

lineInl :: PandocMonad m => ManParser m Inlines
lineInl = do
  (MLine fragments) <- mline
  return $ linePartsToInlines fragments

bareIP :: PandocMonad m => ManParser m GroffToken
bareIP = msatisfy isBareIP where
  isBareIP (MMacro "IP" [] _) = True
  isBareIP _                  = False

endmacro :: PandocMonad m => String -> ManParser m GroffToken
endmacro name = mmacro name <|> lookAhead newBlockMacro
  where
    newBlockMacro = msatisfy isNewBlockMacro
    isNewBlockMacro (MMacro "SH" _ _) = True
    isNewBlockMacro (MMacro "SS" _ _) = True
    isNewBlockMacro _ = False

parseCodeBlock :: PandocMonad m => ManParser m Blocks
parseCodeBlock = try $ do
  optional bareIP -- some people indent their code
  toks <- (mmacro "nf" *> many (mline <|> memptyLine) <* endmacro "fi")
      <|> (mmacro "EX" *> many (mline <|> memptyLine) <* endmacro "EE")
  return $ codeBlock (intercalate "\n" . catMaybes $
                      extractText <$> toks)

  where

  extractText :: GroffToken -> Maybe String
  extractText (MLine ss)
    | not (null ss)
    , all isFontToken ss = Nothing
    | otherwise          = Just $ linePartsToString ss
    where isFontToken (FontSize{}) = True
          isFontToken (Font{})     = True
          isFontToken _            = False
  extractText MEmptyLine = Just ""
  -- string are intercalated with '\n', this prevents double '\n'
  extractText _ = Nothing

parseHeader :: PandocMonad m => ManParser m Blocks
parseHeader = do
  MMacro name args _ <- mmacro "SH" <|> mmacro "SS"
  contents <- if null args
                 then lineInl
                 else return $ mconcat $ intersperse B.space
                             $ map linePartsToInlines args
  let lvl = if name == "SH" then 1 else 2
  return $ header lvl contents

parseBlockQuote :: PandocMonad m => ManParser m Blocks
parseBlockQuote = blockQuote <$> continuation

data ListType = Ordered ListAttributes
              | Bullet

listTypeMatches :: Maybe ListType -> ListType -> Bool
listTypeMatches Nothing _            = True
listTypeMatches (Just Bullet) Bullet = True
listTypeMatches (Just (Ordered (_,x,y))) (Ordered (_,x',y'))
                                     = x == x' && y == y'
listTypeMatches (Just _) _           = False

listItem :: PandocMonad m => Maybe ListType -> ManParser m (ListType, Blocks)
listItem mbListType = try $ do
  (MMacro _ args _) <- mmacro "IP"
  case args of
    (arg1 : _)  -> do
      let cs = linePartsToString arg1
      let cs' = if not ('.' `elem` cs || ')' `elem` cs) then cs ++ "." else cs
      let lt = case Parsec.runParser anyOrderedListMarker defaultParserState
                     "list marker" cs' of
                  Right (start, listtype, listdelim)
                    | cs == cs' -> Ordered (start, listtype, listdelim)
                    | otherwise -> Ordered (start, listtype, DefaultDelim)
                  Left _        -> Bullet
      guard $ listTypeMatches mbListType lt
      inls <- option mempty parseInlines
      continuations <- mconcat <$> many continuation
      return (lt, para inls <> continuations)
    []          -> mzero

parseList :: PandocMonad m => ManParser m Blocks
parseList = try $ do
  (lt, x) <- listItem Nothing
  xs <- map snd <$> many (listItem (Just lt))
  return $ case lt of
             Bullet        -> bulletList (x:xs)
             Ordered lattr -> orderedListWith lattr (x:xs)

continuation :: PandocMonad m => ManParser m Blocks
continuation =
      mconcat <$> (mmacro "RS" *> manyTill parseBlock (endmacro "RE"))
  <|> mconcat <$> many1 (try (bareIP *> parsePara))

definitionListItem :: PandocMonad m
                   => ManParser m (Inlines, [Blocks])
definitionListItem = try $ do
  mmacro "TP"  -- args specify indent level, can ignore
  term <- parseInline
  moreterms <- many $ try $ do
                 mmacro "TQ"
                 newterm <- parseInline
                 return newterm
  inls <- option mempty parseInlines
  continuations <- mconcat <$> many continuation
  return ( mconcat (intersperse B.linebreak (term:moreterms))
         , [para inls <> continuations])

parseDefinitionList :: PandocMonad m => ManParser m Blocks
parseDefinitionList = definitionList <$> many1 definitionListItem

parseLink :: PandocMonad m => [Arg] -> ManParser m Inlines
parseLink args = do
  contents <- mconcat <$> many lineInl
  MMacro _ endargs _ <- mmacro "UE"
  let url = case args of
              [] -> ""
              (x:_) -> linePartsToString x
  return $ link url "" contents <>
    case endargs of
      []    -> mempty
      (x:_) -> linePartsToInlines x

parseEmailLink :: PandocMonad m => [Arg] -> ManParser m Inlines
parseEmailLink args = do
  contents <- mconcat <$> many lineInl
  MMacro _ endargs _ <- mmacro "ME"
  let url = case args of
              [] -> ""
              (x:_) -> "mailto:" ++ linePartsToString x
  return $ link url "" contents <>
    case endargs of
      []    -> mempty
      (x:_) -> linePartsToInlines x

skipUnkownMacro :: PandocMonad m => ManParser m Blocks
skipUnkownMacro = do
  tok <- mmacroAny
  case tok of
    MMacro mkind _ pos -> do
      report $ SkippedContent ('.':mkind) pos
      return mempty
    _                 -> fail "the impossible happened"
