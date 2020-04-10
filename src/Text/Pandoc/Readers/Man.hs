{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Man
   Copyright   : Copyright (C) 2018-2020 Yan Pashkovsky and John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Yan Pashkovsky <yanp.bugz@gmail.com>
   Stability   : WIP
   Portability : portable

Conversion of man to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Man (readMan) where

import Data.Char (toLower)
import Data.Default (Default)
import Control.Monad (liftM, mzero, guard, void)
import Control.Monad.Trans (lift)
import Control.Monad.Except (throwError)
import Data.Maybe (catMaybes, isJust)
import Data.List (intersperse, intercalate)
import qualified Data.Text as T
import Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad(..), report)
import Text.Pandoc.Error (PandocError (PandocParsecError))
import Text.Pandoc.Logging (LogMessage(..))
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Walk (query)
import Text.Pandoc.Shared (crFilter, mapLeft)
import Text.Pandoc.Readers.Roff  -- TODO explicit imports
import Text.Parsec hiding (tokenPrim)
import qualified Text.Parsec as Parsec
import Text.Parsec.Pos (updatePosString, initialPos)
import qualified Data.Foldable as Foldable

data ManState = ManState { readerOptions   :: ReaderOptions
                         , metadata        :: Meta
                         , tableCellsPlain :: Bool
                         } deriving Show

instance Default ManState where
  def = ManState { readerOptions   = def
                 , metadata        = nullMeta
                 , tableCellsPlain = True }

type ManParser m = ParserT [RoffToken] ManState m


-- | Read man (troff) from an input string and return a Pandoc document.
readMan :: PandocMonad m => ReaderOptions -> T.Text -> m Pandoc
readMan opts txt = do
  tokenz <- lexRoff (initialPos "input") (crFilter txt)
  let state = def {readerOptions = opts} :: ManState
  eitherdoc <- readWithMTokens parseMan state
     (Foldable.toList . unRoffTokens $ tokenz)
  either throwError return eitherdoc

readWithMTokens :: PandocMonad m
        => ParserT [RoffToken] ManState m a  -- ^ parser
        -> ManState                         -- ^ initial state
        -> [RoffToken]                       -- ^ input
        -> m (Either PandocError a)
readWithMTokens parser state input =
  let leftF = PandocParsecError . T.pack . intercalate "\n" $ show <$> input
  in mapLeft leftF `liftM` runParserT parser state "source" input

parseMan :: PandocMonad m => ManParser m Pandoc
parseMan = do
  bs <- many parseBlock <* eof
  meta <- metadata <$> getState
  let (Pandoc _ blocks) = doc $ mconcat bs
  return $ Pandoc meta blocks

parseBlock :: PandocMonad m => ManParser m Blocks
parseBlock = choice [ parseList
                    , parseDefinitionList
                    , parseHeader
                    , parseTable
                    , parseTitle
                    , parseCodeBlock
                    , parseBlockQuote
                    , parseNewParagraph
                    , parsePara
                    , skipUnknownMacro
                    ]

parseTable :: PandocMonad m => ManParser m Blocks
parseTable = do
  modifyState $ \st -> st { tableCellsPlain = True }
  let isTbl Tbl{} = True
      isTbl _     = False
  Tbl _opts rows pos <- msatisfy isTbl
  case rows of
    ((as,_):_) -> try (do
      let as' = map (columnTypeToAlignment . columnType) as
      guard $ all isJust as'
      let alignments = catMaybes as'
      let (headerRow', bodyRows') =
            case rows of
              (h:x:bs)
               | isHrule x -> (h, bs)
              _ -> (([],[]), rows)
      headerRow <- mapM parseTableCell $ snd headerRow'
      bodyRows <- mapM (mapM parseTableCell . snd) bodyRows'
      isPlainTable <- tableCellsPlain <$> getState
      let widths = if isPlainTable
                      then repeat ColWidthDefault
                      else repeat $ ColWidth (1.0 / fromIntegral (length alignments))
      return $ B.table B.emptyCaption (zip alignments widths)
                  (TableHead nullAttr $ toHeaderRow headerRow)
                  [TableBody nullAttr 0 [] $ map toRow bodyRows]
                  (TableFoot nullAttr [])) <|> fallback pos
    [] -> fallback pos

 where

  parseTableCell ts = do
    st <- getState
    let ts' = Foldable.toList $ unRoffTokens ts
    let plaintcell = try $ do
          skipMany memptyLine
          plain . trimInlines <$> (parseInlines <* eof)
    let blockstcell = try $ do
          skipMany memptyLine
          mconcat <$> many parseBlock <* eof
    res <- if null ts'
              then return $ Right mempty
              else lift $ readWithMTokens plaintcell st ts'
    case res of
      Left _  -> do
        res' <- lift $ readWithMTokens blockstcell st ts'
        case res' of
          Left _  -> Prelude.fail "Could not parse table cell"
          Right x -> do
            modifyState $ \s -> s{ tableCellsPlain = False }
            return x
      Right x -> return x

  isHrule :: TableRow -> Bool
  isHrule ([cellfmt], _) = columnType cellfmt `elem` ['_','-','=']
  isHrule (_, [RoffTokens ss]) =
    case Foldable.toList ss of
      [TextLine [RoffStr (T.unpack -> [c])]] -> c `elem` ['_','-','=']
      _                     -> False
  isHrule _ = False

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

  toRow = Row nullAttr . map simpleCell
  toHeaderRow l = if null l then [] else [toRow l]

parseNewParagraph :: PandocMonad m => ManParser m Blocks
parseNewParagraph = do
  mmacro "P" <|> mmacro "PP" <|> mmacro "LP" <|> memptyLine
  return mempty

--
-- Parser: [RoffToken] -> Pandoc
--

msatisfy :: Monad m => (RoffToken -> Bool) -> ParserT [RoffToken] st m RoffToken
msatisfy predic = tokenPrim show nextPos testTok
  where
    testTok t     = if predic t then Just t else Nothing
    nextPos _pos _x (ControlLine _ _ pos':_) = pos'
    nextPos pos _x _xs  = updatePosString
                             (setSourceColumn
                               (setSourceLine pos $ sourceLine pos + 1) 1) ""

mtoken :: PandocMonad m => ManParser m RoffToken
mtoken = msatisfy (const True)

mline :: PandocMonad m => ManParser m RoffToken
mline = msatisfy isTextLine where
  isTextLine (TextLine _) = True
  isTextLine _ = False

memptyLine :: PandocMonad m => ManParser m RoffToken
memptyLine = msatisfy isEmptyLine where
  isEmptyLine EmptyLine = True
  isEmptyLine _ = False

mmacro :: PandocMonad m => T.Text -> ManParser m RoffToken
mmacro mk = msatisfy isControlLine where
  isControlLine (ControlLine mk' _ _) | mk == mk' = True
                            | otherwise = False
  isControlLine _ = False

mmacroAny :: PandocMonad m => ManParser m RoffToken
mmacroAny = msatisfy isControlLine where
  isControlLine ControlLine{} = True
  isControlLine _ = False

--
-- RoffToken -> Block functions
--

parseTitle :: PandocMonad m => ManParser m Blocks
parseTitle = do
  (ControlLine _ args _) <- mmacro "TH"
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
  go mono (Font fs: xs)
    | litals > 0 && litals >= lbolds && litals >= lmonos
       = emph (go mono (Font fs{ fontItalic = False } :
                   map (adjustFontSpec (\s -> s{ fontItalic = False }))
                   itals)) <>
            go mono italsrest
    | lbolds > 0 && lbolds >= lmonos
       = strong (go mono (Font fs{ fontBold = False } :
              map (adjustFontSpec (\s -> s{ fontBold = False }))
              bolds)) <>
            go mono boldsrest
    | lmonos > 0
       = go True (Font fs{ fontMonospace = False } :
          map (adjustFontSpec (\s -> s { fontMonospace = False }))
          monos) <> go mono monosrest
    | otherwise = go mono xs
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

parsePara :: PandocMonad m => ManParser m Blocks
parsePara = para . trimInlines <$> parseInlines

parseInlines :: PandocMonad m => ManParser m Inlines
parseInlines = mconcat . intersperse B.space <$> many1 parseInline

parseInline :: PandocMonad m => ManParser m Inlines
parseInline = try $ do
  tok <- mtoken
  case tok of
    TextLine lparts -> return $ linePartsToInlines lparts
    ControlLine mname args pos -> handleInlineMacro mname args pos
    _ -> mzero

handleInlineMacro :: PandocMonad m
                  => T.Text -> [Arg] -> SourcePos -> ManParser m Inlines
handleInlineMacro mname args _pos =
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

parseBold :: PandocMonad m => [Arg] -> ManParser m Inlines
parseBold [] = do
  TextLine lparts <- mline
  return $ strong $ linePartsToInlines lparts
parseBold args = return $
  strong $ mconcat $ intersperse B.space $ map linePartsToInlines args

parseItalic :: PandocMonad m => [Arg] -> ManParser m Inlines
parseItalic [] = do
  TextLine lparts <- mline
  return $ emph $ linePartsToInlines lparts
parseItalic args = return $
  emph $ mconcat $ intersperse B.space $ map linePartsToInlines args

parseAlternatingFonts :: [Inlines -> Inlines]
                      -> [Arg]
                      -> ManParser m Inlines
parseAlternatingFonts constructors args = return $ mconcat $
  zipWith (\f arg -> f (linePartsToInlines arg)) (cycle constructors) args

lineInl :: PandocMonad m => ManParser m Inlines
lineInl = do
  (TextLine fragments) <- mline
  return $ linePartsToInlines fragments

bareIP :: PandocMonad m => ManParser m RoffToken
bareIP = msatisfy isBareIP where
  isBareIP (ControlLine "IP" [] _) = True
  isBareIP _                  = False

endmacro :: PandocMonad m => T.Text -> ManParser m ()
endmacro name = void (mmacro name)
             <|> lookAhead (void newBlockMacro)
             <|> lookAhead eof
  where
    newBlockMacro = msatisfy isNewBlockMacro
    isNewBlockMacro (ControlLine "SH" _ _) = True
    isNewBlockMacro (ControlLine "SS" _ _) = True
    isNewBlockMacro _ = False

parseCodeBlock :: PandocMonad m => ManParser m Blocks
parseCodeBlock = try $ do
  optional bareIP
  optional (mmacro "in") -- some people indent their code
  toks <- (mmacro "nf" *> manyTill codeline (endmacro "fi"))
      <|> (mmacro "EX" *> manyTill codeline (endmacro "EE"))
  optional (mmacro "in")
  return $ codeBlock (T.intercalate "\n" $ catMaybes toks)

  where

  codeline = do
    tok <- mtoken
    case tok of
      ControlLine "PP" _ _ -> return $ Just "" -- .PP sometimes used for blank line
      ControlLine mname args pos ->
        (Just . query getText <$> handleInlineMacro mname args pos) <|>
          do report $ SkippedContent ("." <> mname) pos
             return Nothing
      Tbl _ _ pos     -> do
        report $ SkippedContent "TABLE" pos
        return $ Just "TABLE"
      EmptyLine -> return $ Just ""
      TextLine ss
        | not (null ss)
        , all isFontToken ss -> return Nothing
        | otherwise -> return $ Just $ linePartsToText ss

  isFontToken Font{}     = True
  isFontToken _            = False

  getText :: Inline -> T.Text
  getText (Str s)    = s
  getText Space      = " "
  getText (Code _ s) = s
  getText SoftBreak  = "\n"
  getText LineBreak  = "\n"
  getText _          = ""

parseHeader :: PandocMonad m => ManParser m Blocks
parseHeader = do
  ControlLine name args _ <- mmacro "SH" <|> mmacro "SS"
  contents <- if null args
                 then option mempty lineInl
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
  (ControlLine _ args _) <- mmacro "IP"
  case args of
    (arg1 : _)  -> do
      let cs = linePartsToText arg1
      let cs' = if not (T.any (== '.') cs || T.any (== ')') cs) then cs <> "." else cs
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
  <|> mconcat <$> many1 (  try (bareIP *> parsePara)
                       <|> try (bareIP *> parseCodeBlock)
                        )

definitionListItem :: PandocMonad m
                   => ManParser m (Inlines, [Blocks])
definitionListItem = try $ do
  mmacro "TP"  -- args specify indent level, can ignore
  term <- parseInline
  moreterms <- many $ try $ do
                 mmacro "TQ"
                 parseInline
  inls <- option mempty parseInlines
  continuations <- mconcat <$> many continuation
  return ( mconcat (intersperse B.linebreak (term:moreterms))
         , [para inls <> continuations])

parseDefinitionList :: PandocMonad m => ManParser m Blocks
parseDefinitionList = definitionList <$> many1 definitionListItem

parseLink :: PandocMonad m => [Arg] -> ManParser m Inlines
parseLink args = do
  contents <- mconcat <$> many lineInl
  ControlLine _ endargs _ <- mmacro "UE"
  let url = case args of
              [] -> ""
              (x:_) -> linePartsToText x
  return $ link url "" contents <>
    case endargs of
      []    -> mempty
      (x:_) -> linePartsToInlines x

parseEmailLink :: PandocMonad m => [Arg] -> ManParser m Inlines
parseEmailLink args = do
  contents <- mconcat <$> many lineInl
  ControlLine _ endargs _ <- mmacro "ME"
  let url = case args of
              [] -> ""
              (x:_) -> "mailto:" <> linePartsToText x
  return $ link url "" contents <>
    case endargs of
      []    -> mempty
      (x:_) -> linePartsToInlines x

skipUnknownMacro :: PandocMonad m => ManParser m Blocks
skipUnknownMacro = do
  tok <- mmacroAny
  case tok of
    ControlLine mkind _ pos -> do
      report $ SkippedContent ("." <> mkind) pos
      return mempty
    _                 -> Prelude.fail "the impossible happened"
