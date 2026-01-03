{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.RTF
   Copyright   : Copyright (C) 2021-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane (<jgm@berkeley.edu>)
   Stability   : alpha
   Portability : portable

Conversion of RTF documents 'Pandoc' document.
We target version 1.5 of the RTF spec.
-}
module Text.Pandoc.Readers.RTF (readRTF) where

import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import Control.Monad
import Control.Monad.Except (throwError)
import Crypto.Hash (hashWith, SHA1(SHA1))
import qualified Data.List as L
import Data.Word (Word8, Word16)
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Pandoc.Builder (Blocks, Inlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad (..), insertMedia, report)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Logging (LogMessage(UnsupportedCodePage))
import Text.Pandoc.Shared (tshow)
import Data.Char (isAlphaNum, chr, isAscii, isLetter, isSpace, ord)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe, fromMaybe)
import Safe (lastMay, initSafe, headDef)
-- import Debug.Trace

-- TODO:
-- [ ] more complex table features
--

-- | Read RTF from an input string and return a Pandoc document.
readRTF  :: (PandocMonad m, ToSources a)
         => ReaderOptions
         -> a
         -> m Pandoc
readRTF opts s = do
  let sources = toSources s
  parsed <- readWithM parseRTF def{ sOptions = opts } sources
  case parsed of
       Left e  -> throwError e
       Right d -> return d

data CharSet = ANSI | Mac | Pc | Pca
  deriving (Show, Eq)

-- first index is the list (or override) id, second is the list level
type ListTable = IntMap.IntMap ListLevelTable
type ListLevelTable = IntMap.IntMap ListType

data RTFState = RTFState  { sOptions     :: ReaderOptions
                          , sCharSet     :: CharSet
                          , sGroupStack  :: [Properties]
                          , sListStack   :: [List]
                          , sCurrentCell :: Blocks
                          , sTableRows   :: [TableRow] -- reverse order
                          , sTextContent :: [(Properties, Text)]
                          , sMetadata    :: [(Text, Inlines)]
                          , sFontTable   :: FontTable
                          , sStylesheet  :: Stylesheet
                          , sListTable   :: ListTable
                          , sListOverrideTable :: ListTable
                          , sEatChars    :: Int
                          } deriving (Show)

instance Default RTFState where
 def = RTFState { sOptions = def
                , sCharSet = ANSI
                , sGroupStack = []
                , sListStack = []
                , sCurrentCell = mempty
                , sTableRows = []
                , sTextContent = []
                , sMetadata = []
                , sFontTable = mempty
                , sStylesheet = mempty
                , sListTable = mempty
                , sListOverrideTable = mempty
                , sEatChars = 0
                }

type FontTable = IntMap.IntMap FontFamily

data FontFamily =
  Roman | Swiss | Modern | Script | Decor | Tech | Bidi
  deriving (Show, Eq)

data StyleType = ParagraphStyle | SectionStyle | CharStyle | TableStyle
  deriving (Show, Eq)

data Style =
  Style { styleNum :: Int
        , styleType :: StyleType
        , styleBasedOn :: Maybe Int
        , styleName :: Text
        , styleFormatting :: [Tok]
        } deriving (Show, Eq)

type Stylesheet = IntMap.IntMap Style

data PictType =
  Emfblip | Pngblip | Jpegblip
  deriving (Show, Eq)

data Pict =
  Pict { picType :: Maybe PictType
       , picWidth :: Maybe Int
       , picHeight :: Maybe Int
       , picWidthGoal :: Maybe Int
       , picHeightGoal :: Maybe Int
       , picBinary :: Bool
       , picData :: Text
       , picName :: Text
       , picBytes :: BL.ByteString
       } deriving (Show, Eq)

instance Default Pict where
 def = Pict { picType = Nothing
            , picWidth = Nothing
            , picHeight = Nothing
            , picWidthGoal = Nothing
            , picHeightGoal = Nothing
            , picBinary = False
            , picData = mempty
            , picName = mempty
            , picBytes = mempty }

data Properties =
  Properties
  { gBold :: Bool
  , gItalic :: Bool
  , gCaps :: Bool
  , gDeleted :: Bool
  , gSub :: Bool
  , gSuper :: Bool
  , gSmallCaps :: Bool
  , gUnderline :: Bool
  , gHyperlink :: Maybe Text
  , gAnchor :: Maybe Text
  , gImage :: Maybe Pict
  , gFontFamily :: Maybe FontFamily
  , gHidden :: Bool
  , gUC :: Int -- number of ansi chars to skip after unicode char
  , gFootnote :: Maybe Blocks
  , gOutlineLevel :: Maybe ListLevel
  , gListOverride :: Maybe Override
  , gListLevel :: Maybe Int
  , gInTable :: Bool
  } deriving (Show, Eq)

instance Default Properties where
   def = Properties { gBold = False
                    , gItalic = False
                    , gCaps = False
                    , gDeleted = False
                    , gSub = False
                    , gSuper = False
                    , gSmallCaps = False
                    , gUnderline = False
                    , gHyperlink = Nothing
                    , gAnchor = Nothing
                    , gImage = Nothing
                    , gFontFamily = Nothing
                    , gHidden = False
                    , gUC = 1
                    , gFootnote = Nothing
                    , gOutlineLevel = Nothing
                    , gListOverride = Nothing
                    , gListLevel = Nothing
                    , gInTable = False
                    }

type RTFParser m = ParsecT Sources RTFState m

data ListType = Bullet | Ordered ListAttributes
  deriving (Show, Eq)

type Override = Int

type ListLevel = Int

data List =
    List Override ListLevel ListType [Blocks]  -- items in reverse order
    deriving (Show, Eq)

newtype TableRow = TableRow [Blocks] -- cells in reverse order
    deriving (Show, Eq)

parseRTF :: PandocMonad m => RTFParser m Pandoc
parseRTF = do
  skipMany nl
  bs <- many tok >>= foldM processTok mempty >>= emitBlocks
  unclosed <- closeContainers
  let doc = B.doc $ bs <> unclosed
  kvs <- sMetadata <$> getState
  pure $ foldr (uncurry B.setMeta) doc kvs

data Tok = Tok !SourcePos !TokContents
  deriving (Show, Eq)

data TokContents =
    ControlWord !Text !(Maybe Int)
  | ControlSymbol !Char
  | UnformattedText !Text
  | BinData !BL.ByteString
  | HexVals [Word8]
  | Grouped [Tok]
  deriving (Show, Eq)

tok :: PandocMonad m => RTFParser m Tok
tok = do
  pos <- getPosition
  Tok pos <$!> ((controlThing <|> unformattedText <|> grouped) <* skipMany nl)
 where
  controlThing = do
    char '\\' *>
      ( controlWord
     <|> (HexVals <$> many1 hexVal)
     <|> (ControlSymbol <$> anyChar) )
  controlWord = do
    name <- letterSequence
    param <- parameter <* optional delimChar
    case name of
      "bin" -> do
        let n = fromMaybe 0 param
        spaces
        -- NOTE: We assume here that if the document contains binary
        -- data, it will not be valid UTF-8 and hence it will have been
        -- read as latin1, so we can recover the data in the following
        -- way.  This is probably not completely reliable, but I don't
        -- know if we can do better without making this reader take
        -- a ByteString input.
        dat <- BL.pack . map (fromIntegral . ord) <$> count n anyChar
        return $! BinData dat
      _ -> return $! ControlWord name param
  parameter = do
    hyph <- option False $ True <$ char '-'
    rest <- many digit
    if null rest
       then return Nothing
       else do
         let pstr = T.pack rest
         case TR.decimal pstr of
           Right (!i,_) ->
                return $! Just $! if hyph
                                     then (-1) * i
                                     else i
           _ -> return Nothing
  hexVal = do
    char '\''
    x <- hexDigit
    y <- hexDigit
    return $ hexToWord (T.pack [x,y])
  letterSequence = T.pack <$> many1 (satisfy (\c -> isAscii c && isLetter c))
  unformattedText = do
    ts <-  filter (\c -> c /= '\r' && c /= '\n') <$>
           ( many1 (satisfy (\c -> not (isSpecial c) || c == '\r' || c == '\n')))
    return $! UnformattedText $! T.pack ts
  grouped = do
    char '{'
    skipMany nl
    ts <- manyTill tok (char '}')
    case ts of
       Tok _ (ControlWord "rtf" (Just 1)) : _ -> do
         setInput mempty -- discard remaining input: content after the \rtf1
                         -- group can be non-RTF
       _ -> return ()
    return $! Grouped ts

nl :: PandocMonad m => RTFParser m ()
nl = void (char '\n' <|> char '\r')

isSpecial :: Char -> Bool
isSpecial '{' = True
isSpecial '}' = True
isSpecial '\\' = True
isSpecial '\n' = True
isSpecial _ = False

delimChar :: PandocMonad m => RTFParser m Char
delimChar = satisfy (\c -> not (isAlphaNum c || isSpecial c))

modifyGroup :: PandocMonad m
            => (Properties -> Properties)
            -> RTFParser m ()
modifyGroup f =
  updateState $ \st ->
    st{ sGroupStack =
          case sGroupStack st of
            [] -> []
            (x:xs) -> f x : xs }

addFormatting :: (Properties, Text) -> Inlines
addFormatting (_, "\n") = B.linebreak
addFormatting (props, _) | gHidden props = mempty
addFormatting (props, _) | Just bs <- gFootnote props = B.note bs
addFormatting (props, txt) =
  (if gBold props then B.strong else id) .
  (if gItalic props then B.emph else id) .
  (if gDeleted props then B.strikeout else id) .
  (if gSub props then B.subscript else id) .
  (if gSuper props then B.superscript else id) .
  (if gSmallCaps props then B.smallcaps else id) .
  (if gUnderline props then B.underline else id) .
  (case gHyperlink props of
     Nothing -> id
     Just linkdest -> B.link linkdest mempty) .
  (case gAnchor props of
     Nothing -> id
     Just ident -> B.spanWith (ident,[],[])) .
  (case gFontFamily props of
     Just Modern -> B.code
     _ -> case gImage props of
            Just pict ->
              let attr = ("",[],
                         (case picWidthGoal pict of
                           Nothing -> []
                           Just w  -> [("width", tshow (fromIntegral w / 1440
                                                         :: Double)
                                          <> "in")]) ++
                         (case picHeightGoal pict of
                            Nothing -> []
                            Just h -> [("height", tshow (fromIntegral h / 1440
                                                         :: Double)
                                          <> "in")]))
              in  B.imageWith attr (picName pict) "" . B.text
            Nothing -> B.text) .
  (if gCaps props then T.toUpper else id)
  $ txt

addText :: PandocMonad m => Text -> RTFParser m ()
addText t = do
  gs <- sGroupStack <$> getState
  let !props = case gs of
                (x:_) -> x
                _ -> def
  updateState (\s -> s{ sTextContent = (props, t) : sTextContent s })

inGroup :: PandocMonad m => RTFParser m a -> RTFParser m a
inGroup p = do
  updateState $ \st ->
    st{ sGroupStack =
        case sGroupStack st of
          [] -> [def]
          (x:xs) -> (x:x:xs) } -- inherit current group's properties
  result <- p
  updateState $ \st ->
    st{ sGroupStack =
        case sGroupStack st of
          [] -> [] -- should not happen
          (_:xs) -> xs }
  return result

getStyleFormatting :: PandocMonad m => Int -> RTFParser m [Tok]
getStyleFormatting stynum = do
  stylesheet <- sStylesheet <$> getState
  case IntMap.lookup stynum stylesheet of
    Nothing -> return []
    Just sty ->
      case styleBasedOn sty of
        Just i -> (<> styleFormatting sty)  <$> getStyleFormatting i
        Nothing -> return $ styleFormatting sty

isMetadataField :: Text -> Bool
isMetadataField "title" = True
isMetadataField "subject" = True
isMetadataField "author" = True
isMetadataField "manager" = True
isMetadataField "company" = True
isMetadataField "operator" = True
isMetadataField "category" = True
isMetadataField "keywords" = True
isMetadataField "comment" = True
isMetadataField "doccomm" = True
isMetadataField "hlinkbase" = True
isMetadataField "generator" = True
isMetadataField _ = False

isHeaderFooter :: Text -> Bool
isHeaderFooter "header" = True
isHeaderFooter "headerl" = True
isHeaderFooter "headerr" = True
isHeaderFooter "headerf" = True
isHeaderFooter "footer" = True
isHeaderFooter "footerl" = True
isHeaderFooter "footerr" = True
isHeaderFooter "footerf" = True
isHeaderFooter _ = False

boolParam :: Maybe Int -> Bool
boolParam (Just 0) = False
boolParam _ = True

isUnderline :: Text -> Bool
isUnderline "ul" = True
isUnderline "uld" = True
isUnderline "uldash" = True
isUnderline "uldashd" = True
isUnderline "uldashdd" = True
isUnderline "uldb" = True
isUnderline "ulth" = True
isUnderline "ulthd" = True
isUnderline "ulthdash" = True
isUnderline "ulw" = True
isUnderline "ulwave" = True
isUnderline _ = False

processTok :: PandocMonad m => Blocks -> Tok -> RTFParser m Blocks
processTok bs (Tok pos tok') = do
  setPosition pos
  case tok' of
    HexVals{} -> return ()
    UnformattedText{} -> return ()
    _ -> updateState $ \s -> s{ sEatChars = 0 }
  case tok' of
    Grouped (Tok _ (ControlSymbol '*') : toks@(firsttok:_)) ->
      case firsttok of
        Tok _ (ControlWord "shppict" _) -> inGroup (foldM processTok bs toks)
        Tok _ (ControlWord "shpinst" _) -> inGroup (foldM processTok bs toks)
        _ -> bs <$ (do oldTextContent <- sTextContent <$> getState
                       processTok mempty (Tok pos (Grouped toks))
                       updateState $ \st -> st{ sTextContent = oldTextContent })
    Grouped (Tok _ (ControlWord "shp" _) : toks) ->
      inGroup (foldM processTok bs toks)
    Grouped [ Tok _ (ControlWord "sp" _)
            , Tok _ (Grouped [Tok _ (ControlWord "sn" _),
                      Tok _ (UnformattedText sn)])
            , Tok _ (Grouped (Tok _ (ControlWord "sv" _) : svtoks))
            ] ->
      case sn of
        "pib" -> inGroup (foldM processTok bs svtoks)
        _ -> pure bs
    Grouped (Tok _ (ControlWord "fonttbl" _) : toks) -> inGroup $ do
      updateState $ \s -> s{ sFontTable = processFontTable toks }
      pure bs
    Grouped (Tok _ (ControlWord "field" _) : toks) ->
      inGroup $ handleField bs toks
    Grouped (Tok _ (ControlWord "pict" _) : toks) ->
      bs <$ inGroup (handlePict toks)
    Grouped (Tok _ (ControlWord "stylesheet" _) : toks) ->
      bs <$ inGroup (handleStylesheet toks)
    Grouped (Tok _ (ControlWord "listtext" _) : _) -> do
      -- eject any previous list items...sometimes TextEdit
      -- doesn't put in a \par
      emitBlocks bs
    Grouped (Tok _ (ControlWord "pgdsc" _) : _) -> pure bs
    Grouped (Tok _ (ControlWord "colortbl" _) : _) -> pure bs
    Grouped (Tok _ (ControlWord "listtable" _) : toks) ->
      bs <$ inGroup (handleListTable toks)
    Grouped (Tok _ (ControlWord "listoverridetable" _) : toks) ->
      bs <$ inGroup (handleListOverrideTable toks)
    Grouped (Tok _ (ControlWord "wgrffmtfilter" _) : _) -> pure bs
    Grouped (Tok _ (ControlWord "themedata" _) : _) -> pure bs
    Grouped (Tok _ (ControlWord "colorschememapping" _) : _) -> pure bs
    Grouped (Tok _ (ControlWord "datastore" _) : _) -> pure bs
    Grouped (Tok _ (ControlWord "latentstyles" _) : _) -> pure bs
    Grouped (Tok _ (ControlWord "pntxta" _) : _) -> pure bs -- TODO
    Grouped (Tok _ (ControlWord "pntxtb" _) : _) -> pure bs -- TODO
    Grouped (Tok _ (ControlWord "xmlnstbl" _) : _) -> pure bs
    Grouped (Tok _ (ControlWord "filetbl" _) : _) -> pure bs
    Grouped (Tok _ (ControlWord "expandedcolortbl" _) : _) -> pure bs
    Grouped (Tok _ (ControlWord "listtables" _) : _) -> pure bs
    Grouped (Tok _ (ControlWord "revtbl" _) : _) -> pure bs
    Grouped (Tok _ (ControlWord "bkmkstart" _)
             : Tok _ (UnformattedText t) : _) -> do
      -- TODO ideally we'd put the span around bkmkstart/end, but this
      -- is good for now:
      modifyGroup (\g -> g{ gAnchor = Just $ T.strip t })
      pure bs
    Grouped (Tok _ (ControlWord "bkmkend" _) : _) -> do
      modifyGroup (\g -> g{ gAnchor = Nothing })
      pure bs
    Grouped (Tok _ (ControlWord f _) : _) | isHeaderFooter f -> pure bs
    Grouped (Tok _ (ControlWord "footnote" _) : toks) -> do
      noteBs <- inGroup $ processDestinationToks toks
      modifyGroup (\g -> g{ gFootnote = Just noteBs })
      addText "*"
      modifyGroup (\g -> g{ gFootnote = Nothing })
      return bs
    Grouped (Tok _ (ControlWord "info" _) : toks) ->
      bs <$ inGroup (processDestinationToks toks)
    Grouped (Tok _ (ControlWord f _) : toks) | isMetadataField f -> inGroup $ do
      foldM_ processTok mempty toks
      annotatedToks <- reverse . sTextContent <$> getState
      updateState $ \s -> s{ sTextContent = [] }
      let ils = B.trimInlines . mconcat $ map addFormatting annotatedToks
      updateState $ \s -> s{ sMetadata = (f, ils) : sMetadata s }
      pure bs
    Grouped toks -> inGroup (foldM processTok bs toks)
    UnformattedText t -> bs <$ do
      -- return $! traceShowId $! (pos, t)
      eatChars <- sEatChars <$> getState
      case eatChars of
        0 -> addText t
        n | n < T.length t -> do
             updateState $ \s -> s{ sEatChars = 0 }
             addText (T.drop n t)
          | otherwise -> do
             updateState $ \s -> s{ sEatChars = n - T.length t }
    HexVals ws -> bs <$ do
      eatChars <- sEatChars <$> getState
      let ws' = drop eatChars ws
      updateState $ \s -> s{ sEatChars = if null ws'
                                            then eatChars - length ws
                                            else 0 }
      charset <- sCharSet <$> getState
      case charset of
        ANSI -> addText $ T.pack $ map defaultAnsiWordToChar ws'
        Mac  -> addText $ T.pack $ map macToChar ws'
        Pc   -> addText $ T.pack $ map pcToChar ws'
        Pca  -> addText $ T.pack $ map pcaToChar ws'
    ControlWord "ansi" _ -> bs <$
      updateState (\s -> s{ sCharSet = ANSI })
    ControlWord "ansicpg" (Just cpg) | cpg /= 1252 -> bs <$
      report (UnsupportedCodePage cpg)
    ControlWord "mac" _ -> bs <$
      updateState (\s -> s{ sCharSet = Mac })
    ControlWord "pc" _ -> bs <$
      updateState (\s -> s{ sCharSet = Pc })
    ControlWord "pca" _ -> bs <$
      updateState (\s -> s{ sCharSet = Pca })
    ControlWord "outlinelevel" mbp -> bs <$
      modifyGroup (\g -> g{ gOutlineLevel = mbp })
    ControlWord "ls" mbp -> bs <$
      modifyGroup (\g -> g{ gListOverride = mbp })
    ControlWord "ilvl" mbp -> bs <$
      modifyGroup (\g -> g{ gListLevel = mbp })
    ControlSymbol '\\' -> bs <$ addText "\\"
    ControlSymbol '{' -> bs <$ addText "{"
    ControlSymbol '}' -> bs <$ addText "}"
    ControlSymbol '~' -> bs <$ addText "\x00a0"
    ControlSymbol '-' -> bs <$ addText "\x00ad"
    ControlSymbol '_' -> bs <$ addText "\x2011"
    ControlWord "trowd" _ -> bs <$ do -- add new row
      updateState $ \s -> s{ sTableRows = TableRow [] : sTableRows s
                           , sCurrentCell = mempty }
    ControlWord "cell" _ -> bs <$ do
      new <- emitBlocks mempty
      curCell <- (<> new) . sCurrentCell <$> getState
      updateState $ \s -> s{ sTableRows =
                                case sTableRows s of
                                  TableRow cs : rs ->
                                    TableRow (curCell : cs) : rs
                                  [] -> [TableRow [curCell]] -- shouldn't happen
                           , sCurrentCell = mempty }
    ControlWord "intbl" _ -> do
      ls <- closeLists 0 -- see #11364
      ((ls <>) <$> emitBlocks bs) <* modifyGroup (\g -> g{ gInTable = True })
    ControlWord "plain" _ -> bs <$ modifyGroup (const def)
    ControlWord "lquote" _ -> bs <$ addText "\x2018"
    ControlWord "rquote" _ -> bs <$ addText "\x2019"
    ControlWord "ldblquote" _ -> bs <$ addText "\x201C"
    ControlWord "rdblquote" _ -> bs <$ addText "\x201D"
    ControlWord "emdash" _ -> bs <$ addText "\x2014"
    ControlWord "emspace" _ -> bs <$ addText "\x2003"
    ControlWord "enspace" _ -> bs <$ addText "\x2002"
    ControlWord "endash" _ -> bs <$ addText "\x2013"
    ControlWord "bullet" _ -> bs <$ addText "\x2022"
    ControlWord "tab" _ -> bs <$ addText "\t"
    ControlWord "line" _ -> bs <$ addText "\n"
    ControlSymbol '\n' -> bs <$ addText "\n"
    ControlSymbol '\r' -> bs <$ addText "\n"
    ControlWord "uc" (Just i) -> bs <$ modifyGroup (\g -> g{ gUC = i })
    ControlWord "cs" (Just n) -> do
      getStyleFormatting n >>= foldM processTok bs
    ControlWord "s" (Just n) -> do
      getStyleFormatting n >>= foldM processTok bs
    ControlWord "ds" (Just n) -> do
      getStyleFormatting n >>= foldM processTok bs
    ControlWord "f" (Just i) -> bs <$ do
      fontTable <- sFontTable <$> getState
      modifyGroup (\g -> g{ gFontFamily = IntMap.lookup i fontTable })
    ControlWord "u" (Just i) -> bs <$ do
      st <- getState
      let curgroup = case sGroupStack st of
                       [] -> def
                       (x:_) -> x
      updateState $ \s -> s{ sEatChars = gUC curgroup }
      -- "RTF control words generally accept signed 16-bit numbers as
      -- arguments. For this reason, Unicode values greater than 32767
      -- must be expressed as negative numbers."
      let codepoint :: Word16
          codepoint = fromIntegral i
      addText (T.singleton (chr $ fromIntegral codepoint))
    ControlWord "caps" mbp -> bs <$
      modifyGroup (\g -> g{ gCaps = boolParam mbp })
    ControlWord "deleted" mbp -> bs <$
      modifyGroup (\g -> g{ gDeleted = boolParam mbp })
    ControlWord "b" mbp -> bs <$
      modifyGroup (\g -> g{ gBold = boolParam mbp })
    ControlWord "i" mbp -> bs <$
      modifyGroup (\g -> g{ gItalic = boolParam mbp })
    ControlWord "sub" mbp -> bs <$
      modifyGroup (\g -> g{ gSub = boolParam mbp })
    ControlWord "super" mbp -> bs <$
      modifyGroup (\g -> g{ gSuper = boolParam mbp })
    ControlWord "nosupersub" mbp -> bs <$
      modifyGroup (\g -> g{ gSuper = not $ boolParam mbp
                          , gSub = not $ boolParam mbp })
    ControlWord "up" mbp -> bs <$
      modifyGroup (\g -> g{ gSuper = boolParam mbp })
    ControlWord "strike" mbp -> bs <$
      modifyGroup (\g -> g{ gDeleted = boolParam mbp })
    ControlWord "strikedl" mbp -> bs <$
      modifyGroup (\g -> g{ gDeleted = boolParam mbp })
    ControlWord "striked" mbp -> bs <$
      modifyGroup (\g -> g{ gDeleted = boolParam mbp })
    ControlWord "scaps" mbp -> bs <$
      modifyGroup (\g -> g{ gSmallCaps = boolParam mbp })
    ControlWord "v" mbp -> bs <$
      modifyGroup (\g -> g{ gHidden = boolParam mbp })
    ControlWord x mbp | isUnderline x -> bs <$
      modifyGroup (\g -> g{ gUnderline = boolParam mbp })
    ControlWord "ulnone" _ -> bs <$
      modifyGroup (\g -> g{ gUnderline = False })
    ControlWord "pard" _ -> do
      newbs <- emitBlocks bs
      modifyGroup (const def)
      getStyleFormatting 0 >>= foldM processTok newbs
    ControlWord "par" _ -> emitBlocks bs
    _ -> pure bs

processDestinationToks :: PandocMonad m => [Tok] -> RTFParser m Blocks
processDestinationToks toks = do
  textContent <- sTextContent <$> getState
  liststack <- sListStack <$> getState
  updateState $ \s -> s{ sTextContent = mempty
                       , sListStack = [] }
  result <- inGroup $
              foldM processTok mempty toks >>= emitBlocks
  unclosed <- closeContainers
  updateState $ \s -> s{ sTextContent = textContent
                       , sListStack = liststack }
  return $ result <> unclosed

-- close lists >= level
closeLists :: PandocMonad m => Int -> RTFParser m Blocks
closeLists lvl = do
  lists <- sListStack <$> getState
  case lists of
    (List _ lvl' lt items : rest) | lvl' >= lvl -> do
      let newlist = (case lt of
                      Bullet -> B.bulletList
                      Ordered listAttr -> B.orderedListWith listAttr)
                    (reverse items)
      updateState $ \s -> s{ sListStack = rest }
      case rest of
        [] -> do
          updateState $ \s -> s{ sListStack = rest }
          pure newlist
        (List lo lvl'' lt' [] : rest') -> do -- should not happen
          updateState $ \s -> s{ sListStack =
               List lo lvl'' lt' [newlist] : rest' }
          closeLists lvl
        (List lo lvl'' lt' (i:is) : rest') -> do
          updateState $ \s -> s{ sListStack =
               List lo lvl'' lt' (i <> newlist : is) : rest' }
          closeLists lvl
    _ -> pure mempty

closeTable :: PandocMonad m => RTFParser m Blocks
closeTable = do
  rawrows <- sTableRows <$> getState
  if null rawrows
     then return mempty
     else do
       let getCells (TableRow cs) = reverse cs
       let rows = map getCells . reverse $ rawrows
       updateState $ \s -> s{ sCurrentCell = mempty
                            , sTableRows = [] }
       return $ B.simpleTable [] rows

closeContainers :: PandocMonad m => RTFParser m Blocks
closeContainers = do
  tbl <- closeTable
  lists <- closeLists 0
  return $ tbl <> lists

trimFinalLineBreak :: Inlines -> Inlines
trimFinalLineBreak ils =
  case Seq.viewr (B.unMany ils) of
    rest Seq.:> LineBreak -> B.Many rest
    _ -> ils

emitBlocks :: PandocMonad m => Blocks -> RTFParser m Blocks
emitBlocks bs = do
  annotatedToks <- reverse . sTextContent <$> getState
  updateState $ \s -> s{ sTextContent = [] }
  let justCode = def{ gFontFamily = Just Modern }
  let prop = case annotatedToks of
               [] -> def
               ((p,_):_) -> p
  tbl <- if gInTable prop || null annotatedToks
            then pure mempty
            else closeTable
  new <-
    case annotatedToks of
      [] -> pure mempty
      _ | Just lst <- gListOverride prop
         -> do
           let level = fromMaybe 0 $ gListLevel prop
           listOverrideTable <- sListOverrideTable <$> getState
           let listType = fromMaybe Bullet $
                 IntMap.lookup lst listOverrideTable >>= IntMap.lookup level
           lists <- sListStack <$> getState
           -- get para contents of list item
           let newbs = B.para . B.trimInlines . trimFinalLineBreak . mconcat $
                        map addFormatting annotatedToks
           case lists of
             (List lo parentlevel _lt items : cs)
               | lo == lst
               , parentlevel == level
               -- add another item to existing list
               -> do updateState $ \s ->
                        s{ sListStack =
                             List lo level listType (newbs:items) : cs }
                     pure mempty
               | lo /= lst || level < parentlevel
               -- close parent list and add new list
               -> do new <- closeLists level  -- close open lists > level
                     updateState $ \s ->
                       s{ sListStack = List lst level listType [newbs] :
                           sListStack s }
                     pure new
             _ -> do -- add new list (level > parentlevel)
                  updateState $ \s ->
                    s{ sListStack = List lst level listType [newbs] :
                         sListStack s }
                  pure mempty
        | Just lvl <- gOutlineLevel prop
         -> do
            lists <- closeLists 0
            pure $ lists <>
                   B.header (lvl + 1)
                   (B.trimInlines . mconcat $ map addFormatting
                                            $ removeCommonFormatting
                                              annotatedToks)
        | all ((== justCode) . fst) annotatedToks
         -> do
            lists <- closeLists 0
            pure $ lists <>
                    B.codeBlock (mconcat $ map snd annotatedToks)
        | all (T.all isSpace . snd) annotatedToks
         -> closeLists 0
        | otherwise -> do
            lists <- closeLists 0
            pure $ lists <>
              B.para (B.trimInlines . trimFinalLineBreak . mconcat
                $ map addFormatting annotatedToks)
  if gInTable prop
     then do
       updateState $ \s -> s{ sCurrentCell = sCurrentCell s <> new }
       pure bs
     else do
       pure $ bs <> tbl <> new

-- Headers often have a style applied. We usually want to remove
-- this, because headers will have their own styling in the target
-- format.
removeCommonFormatting :: [(Properties, Text)] -> [(Properties, Text)]
removeCommonFormatting =
  (\ts ->
    if all (gBold . fst) ts
       then map (\(p,t) -> (p{ gBold = False }, t)) ts
       else ts) .
  (\ts ->
    if all (gItalic . fst) ts
       then map (\(p,t) -> (p{ gItalic = False }, t)) ts
       else ts)


-- {\field{\*\fldinst{HYPERLINK "http://pandoc.org"}}{\fldrslt foo}}
handleField :: PandocMonad m => Blocks -> [Tok] -> RTFParser m Blocks
handleField bs ts = do
  let isFieldMod (Tok _ (ControlWord w _)) =
        w `elem` ["flddirty", "fldedit", "fldlock", "fldpriv"]
      isFieldMod _ = False

  let instructionTokens (Tok _ (Grouped toks)) = Just toks
      instructionTokens unformattedTok@(Tok _ (UnformattedText _)) = Just [unformattedTok]
      instructionTokens _ = Nothing
  case dropWhile isFieldMod ts of
    [Tok _ (Grouped
       (Tok _ (ControlSymbol '*')
       :Tok _ (ControlWord "fldinst" Nothing)
       :instrtoks
       :_)),
     Tok _ (Grouped
       (Tok _ (ControlWord "fldrslt" Nothing)
       :resulttoks))] -> do
         case instructionTokens instrtoks of
           Nothing -> pure bs
           Just instrtoks' ->
             case getHyperlink instrtoks' of
               Just linkdest -> do
                 modifyGroup $ \g -> g{ gHyperlink = Just linkdest }
                 result <- foldM processTok bs resulttoks
                 modifyGroup $ \g -> g{ gHyperlink = Nothing }
                 return result
               Nothing -> foldM processTok bs resulttoks
    _ -> pure bs

getHyperlink :: [Tok] -> Maybe Text
getHyperlink [] = Nothing
getHyperlink (Tok _ (UnformattedText w) : rest)
  | Just w' <- unquote <$> T.stripPrefix "HYPERLINK" (T.strip w)
    = if T.null w'
         then case rest of
                (Tok _ (ControlSymbol '\\') : Tok _ (UnformattedText b) : _)
                  | Just bkmrk <- unquote <$> T.stripPrefix "l " (T.strip b)
                    -> Just $ "#" <> bkmrk
                _ -> Just mempty
         else Just w'
getHyperlink (_:ts) = getHyperlink ts

unquote :: Text -> Text
unquote = T.dropWhile (=='"') . T.dropWhileEnd (=='"') . T.strip

handleListTable :: PandocMonad m => [Tok] -> RTFParser m ()
handleListTable toks = do
  mapM_ handleList toks

handleList :: PandocMonad m => Tok -> RTFParser m ()
handleList (Tok _ (Grouped (Tok _ (ControlWord "list" _) : toks))) = do
  let listid = headDef 0 [n | Tok _ (ControlWord "listid" (Just n)) <- toks]
  let levels = [ts | Tok _ (Grouped (Tok _ (ControlWord "listlevel" _) : ts))
                 <- toks]
  tbl <- foldM handleListLevel mempty (zip [0..] levels)
  updateState $ \s -> s{ sListTable = IntMap.insert listid tbl $ sListTable s }
handleList _ = return ()

handleListLevel :: PandocMonad m
                => ListLevelTable
                -> (Int, [Tok])
                -> RTFParser m ListLevelTable
handleListLevel levelTable (lvl, toks) = do
  let start = headDef 1
                [n | Tok _ (ControlWord "levelstartat" (Just n)) <- toks]
  let mbNumberStyle =
        case [n | Tok _ (ControlWord "levelnfc" (Just n)) <- toks] of
          [] -> Nothing
          (0:_) -> Just Decimal
          (1:_) -> Just UpperRoman
          (2:_) -> Just LowerRoman
          (3:_) -> Just UpperAlpha
          (4:_) -> Just LowerAlpha
          (23:_) -> Nothing
          (255:_) -> Nothing
          _ -> Just DefaultStyle
  let listType = case mbNumberStyle of
                   Nothing -> Bullet
                   Just numStyle -> Ordered (start,numStyle,Period)
  return $ IntMap.insert lvl listType levelTable

handleListOverrideTable :: PandocMonad m => [Tok] -> RTFParser m ()
handleListOverrideTable toks = mapM_ handleListOverride toks

handleListOverride :: PandocMonad m => Tok -> RTFParser m ()
handleListOverride
 (Tok _ (Grouped (Tok _ (ControlWord "listoverride" _) : toks))) = do
  let listid = headDef 0 [n | Tok _ (ControlWord "listid" (Just n)) <- toks]
  let lsn = headDef 0 [n | Tok _ (ControlWord "ls" (Just n)) <- toks]
  -- TODO override stuff, esp. start num -- for now we just handle indirection
  listTable <- sListTable <$> getState
  case IntMap.lookup listid listTable of
    Nothing -> return ()
    Just tbl -> updateState $ \s ->
                   s{ sListOverrideTable = IntMap.insert lsn tbl $
                        sListOverrideTable s }
handleListOverride _ = return ()

handleStylesheet :: PandocMonad m => [Tok] -> RTFParser m ()
handleStylesheet toks = do
  let styles = mapMaybe parseStyle toks
  updateState $ \s -> s{ sStylesheet = IntMap.fromList
                                     $ zip (map styleNum styles) styles }

parseStyle :: Tok -> Maybe Style
parseStyle (Tok _ (Grouped toks)) = do
  let (styType, styNum, rest) =
        case toks of
          Tok _ (ControlWord "s" (Just n)) : ts -> (ParagraphStyle, n, ts)
          Tok _ (ControlWord "ds" (Just n)) : ts -> (SectionStyle, n, ts)
          Tok _ (ControlWord "cs" (Just n)) : ts -> (CharStyle, n, ts)
          Tok _ (ControlWord "ts" (Just n)) : ts -> (TableStyle, n, ts)
          _ -> (ParagraphStyle, 0, toks)
  let styName = case lastMay rest of
                  Just (Tok _ (UnformattedText t)) -> T.dropWhileEnd (==';') t
                  _ -> mempty
  let isBasedOn (Tok _ (ControlWord "sbasedon" (Just _))) = True
      isBasedOn _ = False
  let styBasedOn = case L.find isBasedOn toks of
                     Just (Tok _ (ControlWord "sbasedon" (Just i))) -> Just i
                     _ -> Nothing
  let isStyleControl (Tok _ (ControlWord x _)) =
         x `elem` ["cs", "s", "ds", "additive", "sbasedon", "snext",
                   "sautoupd", "shidden", "keycode", "alt", "shift",
                   "ctrl", "fn"]
      isStyleControl _ = False
  let styFormatting = filter (not . isStyleControl) (initSafe rest)
  return $ Style{ styleNum = styNum
                , styleType = styType
                , styleBasedOn = styBasedOn
                , styleName = styName
                , styleFormatting = styFormatting
                }
parseStyle _ = Nothing

hexToWord  :: Text -> Word8
hexToWord t = case TR.hexadecimal t of
                Left _ -> 0
                Right (x,_) -> x


handlePict :: PandocMonad m => [Tok] -> RTFParser m ()
handlePict toks = do
  let pict = L.foldl' getPictData def toks
  let altText = "image"
  let bytes =
        if picBinary pict
           then picBytes pict
           else BL.pack $ map hexToWord $ T.chunksOf 2 $ picData pict
  let (mimetype, ext) =
        case picType pict of
          Just Emfblip -> (Just "image/x-emf", ".emf")
          Just Pngblip -> (Just "image/png", ".png")
          Just Jpegblip -> (Just "image/jpeg", ".jpg")
          Nothing -> (Nothing, "")
  case mimetype of
    Just mt -> do
      let pictname = show (hashWith SHA1 $ BL.toStrict bytes) <> ext
      insertMedia pictname (Just mt) bytes
      modifyGroup $ \g -> g{ gImage = Just pict{ picName = T.pack pictname,
                                                 picBytes = bytes } }
      addText altText
      modifyGroup $ \g -> g{ gImage = Nothing }
    _ -> return ()
 where
  getPictData :: Pict -> Tok -> Pict
  getPictData pict (Tok _ tok') =
    case tok' of
      ControlWord "emfblip" _-> pict{ picType = Just Emfblip }
      ControlWord "pngblip" _-> pict{ picType = Just Pngblip }
      ControlWord "jpegblip" _-> pict{ picType = Just Jpegblip }
      ControlWord "picw" (Just w) -> pict{ picWidth = Just w }
      ControlWord "pich" (Just h) -> pict{ picHeight = Just h }
      ControlWord "picwgoal" (Just w) -> pict{ picWidthGoal = Just w }
      ControlWord "pichgoal" (Just h) -> pict{ picHeightGoal = Just h }
      BinData d | not (BL.null d)
                  -> pict{ picBinary = True, picBytes = picBytes pict <> d }
      UnformattedText t -> pict{ picData = t }
      _ -> pict


processFontTable :: [Tok] -> FontTable
processFontTable = snd . L.foldl' go (0, mempty)
 where
  go (fontnum, tbl) (Tok _ tok') =
    case tok' of
     (ControlWord "f" (Just i)) -> (i, tbl)
     (ControlWord "fnil" _) -> (fontnum, tbl)
     (ControlWord "froman" _) -> (fontnum, IntMap.insert fontnum Roman tbl)
     (ControlWord "fswiss" _) -> (fontnum, IntMap.insert fontnum Swiss tbl)
     (ControlWord "fmodern" _) -> (fontnum, IntMap.insert fontnum Modern tbl)
     (ControlWord "fscript" _) -> (fontnum, IntMap.insert fontnum Script tbl)
     (ControlWord "fdecor" _) -> (fontnum, IntMap.insert fontnum Decor tbl)
     (ControlWord "ftech" _) -> (fontnum, IntMap.insert fontnum Tech tbl)
     (ControlWord "fbidi" _) -> (fontnum, IntMap.insert fontnum Bidi tbl)
     (Grouped ts) -> L.foldl' go (fontnum, tbl) ts
     _ -> (fontnum, tbl)

defaultAnsiWordToChar :: Word8 -> Char
defaultAnsiWordToChar i =
  case i of
    128 -> '\8364'
    130 -> '\8218'
    131 -> '\402'
    132 -> '\8222'
    133 -> '\8230'
    134 -> '\8224'
    135 -> '\8225'
    136 -> '\710'
    137 -> '\8240'
    138 -> '\352'
    139 -> '\8249'
    140 -> '\338'
    142 -> '\381'
    145 -> '\8216'
    146 -> '\8217'
    147 -> '\8220'
    148 -> '\8221'
    149 -> '\8226'
    150 -> '\8211'
    151 -> '\8212'
    152 -> '\732'
    153 -> '\8482'
    154 -> '\353'
    155 -> '\8250'
    156 -> '\339'
    158 -> '\382'
    159 -> '\376'
    173 -> '\xAD'
    _ -> chr (fromIntegral i)

macToChar :: Word8 -> Char
macToChar i = chr $
  case i of
    0x80 -> 0xC4
    0x81 -> 0xC5
    0x82 -> 0xC7
    0x83 -> 0xC9
    0x84 -> 0xD1
    0x85 -> 0xD6
    0x86 -> 0xDC
    0x87 -> 0xE1
    0x88 -> 0xE0
    0x89 -> 0xE2
    0x8A -> 0xE4
    0x8B -> 0xE3
    0x8C -> 0xE5
    0x8D -> 0xE7
    0x8E -> 0xE9
    0x8F -> 0xE8
    0x90 -> 0xEA
    0x91 -> 0xEB
    0x92 -> 0xED
    0x93 -> 0xEC
    0x94 -> 0xEE
    0x95 -> 0xEF
    0x96 -> 0xF1
    0x97 -> 0xF3
    0x98 -> 0xF2
    0x99 -> 0xF4
    0x9A -> 0xF6
    0x9B -> 0xF5
    0x9C -> 0xFA
    0x9D -> 0xF9
    0x9E -> 0xFB
    0x9F -> 0xFC
    0xA0 -> 0xDD
    0xA1 -> 0xB0
    0xA2 -> 0xA2
    0xA3 -> 0xA3
    0xA4 -> 0xA7
    0xA5 -> 0xD7
    0xA6 -> 0xB6
    0xA7 -> 0xDF
    0xA8 -> 0xAE
    0xA9 -> 0xA9
    0xAA -> 0xB2
    0xAB -> 0xB4
    0xAC -> 0xA8
    0xAD -> 0xB3
    0xAE -> 0xC6
    0xAF -> 0xD8
    0xB0 -> 0xB9
    0xB1 -> 0xB1
    0xB2 -> 0xBC
    0xB3 -> 0xBD
    0xB4 -> 0xA5
    0xB5 -> 0xB5
    0xBA -> 0xBE
    0xBB -> 0xAA
    0xBC -> 0xBA
    0xBE -> 0xE6
    0xBF -> 0xF8
    0xC0 -> 0xBF
    0xC1 -> 0xA1
    0xC2 -> 0xAC
    0xC3 -> 0x0141
    0xC4 -> 0x0192
    0xC5 -> 0x02CB
    0xC7 -> 0xAB
    0xC8 -> 0xBB
    0xC9 -> 0xA6
    0xCA -> 0xA0
    0xCB -> 0xC0
    0xCC -> 0xC3
    0xCD -> 0xD5
    0xCE -> 0x0152
    0xCF -> 0x0153
    0xD0 -> 0xAD
    0xD4 -> 0x0142
    0xD6 -> 0xF7
    0xD8 -> 0xFF
    0xD9 -> 0x0178
    0xDB -> 0xA4
    0xDC -> 0xD0
    0xDD -> 0xF0
    0xDE -> 0xDE
    0xDF -> 0xFE
    0xE0 -> 0xFD
    0xE1 -> 0xB7
    0xE5 -> 0xC2
    0xE6 -> 0xCA
    0xE7 -> 0xC1
    0xE8 -> 0xCB
    0xE9 -> 0xC8
    0xEA -> 0xCD
    0xEB -> 0xCE
    0xEC -> 0xCF
    0xED -> 0xCC
    0xEE -> 0xD3
    0xEF -> 0xD4
    0xF1 -> 0xD2
    0xF2 -> 0xDA
    0xF3 -> 0xDB
    0xF4 -> 0xD9
    0xF5 -> 0x0131
    0xF6 -> 0x02C6
    0xF7 -> 0x02DC
    0xF8 -> 0xAF
    0xF9 -> 0x02D8
    0xFA -> 0x02D9
    0xFB -> 0x02DA
    0xFC -> 0xB8
    0xFD -> 0x02DD
    0xFE -> 0x02DB
    0xFF -> 0x02C7
    _ -> fromIntegral i

pcToChar :: Word8 -> Char
pcToChar i = chr $
  case i of
    0x80 -> 0xc7
    0x81 -> 0xfc
    0x82 -> 0xe9
    0x83 -> 0xe2
    0x84 -> 0xe4
    0x85 -> 0xe0
    0x86 -> 0xe5
    0x87 -> 0xe7
    0x88 -> 0xea
    0x89 -> 0xeb
    0x8a -> 0xe8
    0x8b -> 0xef
    0x8c -> 0xee
    0x8d -> 0xec
    0x8e -> 0xc4
    0x8f -> 0xc5
    0x90 -> 0xc9
    0x91 -> 0xe6
    0x92 -> 0xc6
    0x93 -> 0xf4
    0x94 -> 0xf6
    0x95 -> 0xf2
    0x96 -> 0xfb
    0x97 -> 0xf9
    0x98 -> 0xff
    0x99 -> 0xd6
    0x9a -> 0xdc
    0x9b -> 0xa2
    0x9c -> 0xa3
    0x9d -> 0xa5
    0x9e -> 0x20a7
    0x9f -> 0x0192
    0xa0 -> 0xe1
    0xa1 -> 0xed
    0xa2 -> 0xf3
    0xa3 -> 0xfa
    0xa4 -> 0xf1
    0xa5 -> 0xd1
    0xa6 -> 0xaa
    0xa7 -> 0xba
    0xa8 -> 0xbf
    0xa9 -> 0x2310
    0xaa -> 0xac
    0xab -> 0xbd
    0xac -> 0xbc
    0xad -> 0xa1
    0xae -> 0xab
    0xaf -> 0xbb
    0xb0 -> 0x2591
    0xb1 -> 0x2592
    0xb2 -> 0x2593
    0xb3 -> 0x2502
    0xb4 -> 0x2524
    0xb5 -> 0x2561
    0xb6 -> 0x2562
    0xb7 -> 0x2556
    0xb8 -> 0x2555
    0xb9 -> 0x2563
    0xba -> 0x2551
    0xbb -> 0x2557
    0xbc -> 0x255d
    0xbd -> 0x255c
    0xbe -> 0x255b
    0xbf -> 0x2510
    0xc0 -> 0x2514
    0xc1 -> 0x2534
    0xc2 -> 0x252c
    0xc3 -> 0x251c
    0xc4 -> 0x2500
    0xc5 -> 0x253c
    0xc6 -> 0x255e
    0xc7 -> 0x255f
    0xc8 -> 0x255a
    0xc9 -> 0x2554
    0xca -> 0x2569
    0xcb -> 0x2566
    0xcc -> 0x2560
    0xcd -> 0x2550
    0xce -> 0x256c
    0xcf -> 0x2567
    0xd0 -> 0x2568
    0xd1 -> 0x2564
    0xd2 -> 0x2565
    0xd3 -> 0x2559
    0xd4 -> 0x2558
    0xd5 -> 0x2552
    0xd6 -> 0x2553
    0xd7 -> 0x256b
    0xd8 -> 0x256a
    0xd9 -> 0x2518
    0xda -> 0x250c
    0xdb -> 0x2588
    0xdc -> 0x2584
    0xdd -> 0x258c
    0xde -> 0x2590
    0xdf -> 0x2580
    0xe0 -> 0x03b1
    0xe1 -> 0xdf
    0xe2 -> 0x0393
    0xe3 -> 0x03c0
    0xe4 -> 0x03a3
    0xe5 -> 0x03c3
    0xe6 -> 0xb5
    0xe7 -> 0x03c4
    0xe8 -> 0x03a6
    0xe9 -> 0x0398
    0xea -> 0x03a9
    0xeb -> 0x03b4
    0xec -> 0x221e
    0xed -> 0x03c6
    0xee -> 0x03b5
    0xef -> 0x2229
    0xf0 -> 0x2261
    0xf1 -> 0xb1
    0xf2 -> 0x2265
    0xf3 -> 0x2264
    0xf4 -> 0x2320
    0xf5 -> 0x2321
    0xf6 -> 0xf7
    0xf7 -> 0x2248
    0xf8 -> 0xb0
    0xf9 -> 0x2219
    0xfa -> 0xb7
    0xfb -> 0x221a
    0xfc -> 0x207f
    0xfd -> 0xb2
    0xfe -> 0x25a0
    0xff -> 0xa0
    _    -> fromIntegral i

pcaToChar :: Word8 -> Char
pcaToChar i = chr $
  case i of
    0x80 -> 0x00c7
    0x81 -> 0x00fc
    0x82 -> 0x00e9
    0x83 -> 0x00e2
    0x84 -> 0x00e4
    0x85 -> 0x00e0
    0x86 -> 0x00e5
    0x87 -> 0x00e7
    0x88 -> 0x00ea
    0x89 -> 0x00eb
    0x8a -> 0x00e8
    0x8b -> 0x00ef
    0x8c -> 0x00ee
    0x8d -> 0x00ec
    0x8e -> 0x00c4
    0x8f -> 0x00c5
    0x90 -> 0x00c9
    0x91 -> 0x00e6
    0x92 -> 0x00c6
    0x93 -> 0x00f4
    0x94 -> 0x00f6
    0x95 -> 0x00f2
    0x96 -> 0x00fb
    0x97 -> 0x00f9
    0x98 -> 0x00ff
    0x99 -> 0x00d6
    0x9a -> 0x00dc
    0x9b -> 0x00f8
    0x9c -> 0x00a3
    0x9d -> 0x00d8
    0x9e -> 0x00d7
    0x9f -> 0x0192
    0xa0 -> 0x00e1
    0xa1 -> 0x00ed
    0xa2 -> 0x00f3
    0xa3 -> 0x00fa
    0xa4 -> 0x00f1
    0xa5 -> 0x00d1
    0xa6 -> 0x00aa
    0xa7 -> 0x00ba
    0xa8 -> 0x00bf
    0xa9 -> 0x00ae
    0xaa -> 0x00ac
    0xab -> 0x00bd
    0xac -> 0x00bc
    0xad -> 0x00a1
    0xae -> 0x00ab
    0xaf -> 0x00bb
    0xb0 -> 0x2591
    0xb1 -> 0x2592
    0xb2 -> 0x2593
    0xb3 -> 0x2502
    0xb4 -> 0x2524
    0xb5 -> 0x00c1
    0xb6 -> 0x00c2
    0xb7 -> 0x00c0
    0xb8 -> 0x00a9
    0xb9 -> 0x2563
    0xba -> 0x2551
    0xbb -> 0x2557
    0xbc -> 0x255d
    0xbd -> 0x00a2
    0xbe -> 0x00a5
    0xbf -> 0x2510
    0xc0 -> 0x2514
    0xc1 -> 0x2534
    0xc2 -> 0x252c
    0xc3 -> 0x251c
    0xc4 -> 0x2500
    0xc5 -> 0x253c
    0xc6 -> 0x00e3
    0xc7 -> 0x00c3
    0xc8 -> 0x255a
    0xc9 -> 0x2554
    0xca -> 0x2569
    0xcb -> 0x2566
    0xcc -> 0x2560
    0xcd -> 0x2550
    0xce -> 0x256c
    0xcf -> 0x00a4
    0xd0 -> 0x00f0
    0xd1 -> 0x00d0
    0xd2 -> 0x00ca
    0xd3 -> 0x00cb
    0xd4 -> 0x00c8
    0xd5 -> 0x0131
    0xd6 -> 0x00cd
    0xd7 -> 0x00ce
    0xd8 -> 0x00cf
    0xd9 -> 0x2518
    0xda -> 0x250c
    0xdb -> 0x2588
    0xdc -> 0x2584
    0xdd -> 0x00a6
    0xde -> 0x00cc
    0xdf -> 0x2580
    0xe0 -> 0x00d3
    0xe1 -> 0x00df
    0xe2 -> 0x00d4
    0xe3 -> 0x00d2
    0xe4 -> 0x00f5
    0xe5 -> 0x00d5
    0xe6 -> 0x00b5
    0xe7 -> 0x00fe
    0xe8 -> 0x00de
    0xe9 -> 0x00da
    0xea -> 0x00db
    0xeb -> 0x00d9
    0xec -> 0x00fd
    0xed -> 0x00dd
    0xee -> 0x00af
    0xef -> 0x00b4
    0xf0 -> 0x00ad
    0xf1 -> 0x00b1
    0xf2 -> 0x2017
    0xf3 -> 0x00be
    0xf4 -> 0x00b6
    0xf5 -> 0x00a7
    0xf6 -> 0x00f7
    0xf7 -> 0x00b8
    0xf8 -> 0x00b0
    0xf9 -> 0x00a8
    0xfa -> 0x00b7
    0xfb -> 0x00b9
    0xfc -> 0x00b3
    0xfd -> 0x00b2
    0xfe -> 0x25a0
    0xff -> 0x00a0
    _    -> fromIntegral i
