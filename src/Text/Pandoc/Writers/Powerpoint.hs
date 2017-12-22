{-# LANGUAGE PatternGuards, MultiWayIf, OverloadedStrings #-}

{-
Copyright (C) 2017 Jesse Rosenthal <jrosenthal@jhu.edu>

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
   Module      : Text.Pandoc.Writers.Powerpoint
   Copyright   : Copyright (C) 2017 Jesse Rosenthal
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to powerpoint (pptx).
-}

module Text.Pandoc.Writers.Powerpoint (writePowerpoint) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader
import Control.Monad.State
import Codec.Archive.Zip
import Data.List (intercalate, stripPrefix, isPrefixOf, nub)
-- import Control.Monad (mplus)
import Data.Default
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import System.FilePath.Posix (splitDirectories, splitExtension)
import Text.XML.Light
import Text.Pandoc.Definition
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Error (PandocError(..))
import qualified Text.Pandoc.Class as P
import Text.Pandoc.Options
import Text.Pandoc.MIME
import Text.Pandoc.Logging
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString.Lazy.Char8 as BL8
-- import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Shared (fixDisplayMath)
import Text.Pandoc.Writers.OOXML
import qualified Data.Map as M
import Data.Maybe (mapMaybe, listToMaybe)
import Text.Pandoc.ImageSize
import Control.Applicative ((<|>))

import Text.TeXMath
import Text.Pandoc.Writers.Math (convertMath)


writePowerpoint :: (PandocMonad m)
                => WriterOptions  -- ^ Writer options
                -> Pandoc         -- ^ Document to convert
                -> m BL.ByteString
writePowerpoint opts (Pandoc meta blks) = do
  let blks' = walk fixDisplayMath blks
  distArchive <- (toArchive . BL.fromStrict) <$>
                      P.readDefaultDataFile "reference.pptx"
  refArchive <- case writerReferenceDoc opts of
                     Just f  -> toArchive <$> P.readFileLazy f
                     Nothing -> (toArchive . BL.fromStrict) <$>
                        P.readDataFile "reference.pptx"

  utctime <- P.getCurrentTime

  let env = def { envMetadata = meta
                , envRefArchive = refArchive
                , envDistArchive = distArchive
                , envUTCTime = utctime
                , envOpts = opts
                }
  runP env def $ do pres <- blocksToPresentation blks'
                    archv <- presentationToArchive pres
                    return $ fromArchive archv

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

data WriterEnv = WriterEnv { envMetadata :: Meta
                           , envRunProps :: RunProps
                           , envParaProps :: ParaProps
                           , envSlideLevel :: Int
                           , envRefArchive :: Archive
                           , envDistArchive :: Archive
                           , envUTCTime :: UTCTime
                           , envOpts :: WriterOptions
                           , envPresentationSize :: PresentationSize
                           , envSlideHasHeader :: Bool
                           , envInList :: Bool
                           , envInNoteSlide :: Bool
                           }
                 deriving (Show)

instance Default WriterEnv where
  def = WriterEnv { envMetadata = mempty
                  , envRunProps = def
                  , envParaProps = def
                  , envSlideLevel = 2
                  , envRefArchive = emptyArchive
                  , envDistArchive = emptyArchive
                  , envUTCTime = posixSecondsToUTCTime 0
                  , envOpts = def
                  , envPresentationSize = def
                  , envSlideHasHeader = False
                  , envInList = False
                  , envInNoteSlide = False
                  }

data MediaInfo = MediaInfo { mInfoFilePath :: FilePath
                           , mInfoLocalId  :: Int
                           , mInfoGlobalId :: Int
                           , mInfoMimeType :: Maybe MimeType
                           , mInfoExt      :: Maybe String
                           , mInfoCaption  :: Bool
                           } deriving (Show, Eq)

data WriterState = WriterState { stCurSlideId :: Int
                               -- the difference between the number at
                               -- the end of the slide file name and
                               -- the rId number
                               , stSlideIdOffset :: Int
                               , stLinkIds :: M.Map Int (M.Map Int (URL, String))
                               -- (FP, Local ID, Global ID, Maybe Mime)
                               , stMediaIds :: M.Map Int [MediaInfo]
                               , stMediaGlobalIds :: M.Map FilePath Int
                               , stNoteIds :: M.Map Int [Block]
                               } deriving (Show, Eq)

instance Default WriterState where
  def = WriterState { stCurSlideId = 0
                    , stSlideIdOffset = 1
                    , stLinkIds = mempty
                    , stMediaIds = mempty
                    , stMediaGlobalIds = mempty
                    , stNoteIds = mempty
                    }

type P m = ReaderT WriterEnv (StateT WriterState m)

runP :: Monad m => WriterEnv -> WriterState -> P m a -> m a
runP env st p = evalStateT (runReaderT p env) st

type Pixels = Integer

data Presentation = Presentation PresentationSize [Slide]
  deriving (Show)

data PresentationSize = PresentationSize { presSizeWidth :: Pixels
                                         , presSizeRatio :: PresentationRatio
                                         }
                      deriving (Show, Eq)

data PresentationRatio = Ratio4x3
                       | Ratio16x9
                       | Ratio16x10
                       deriving (Show, Eq)

-- Note that right now we're only using Ratio4x3.
getPageHeight :: PresentationSize -> Pixels
getPageHeight sz = case presSizeRatio sz of
  Ratio4x3 -> floor (((fromInteger (presSizeWidth sz)) / 4) * 3 :: Double)
  Ratio16x9 -> floor (((fromInteger (presSizeWidth sz)) / 16) * 9 :: Double)
  Ratio16x10 -> floor (((fromInteger (presSizeWidth sz)) / 16) * 10 :: Double)

instance Default PresentationSize where
  def = PresentationSize 720 Ratio4x3

data Slide = MetadataSlide { metadataSlideTitle :: [ParaElem]
                            , metadataSlideSubtitle :: [ParaElem]
                            , metadataSlideAuthors :: [[ParaElem]]
                            , metadataSlideDate :: [ParaElem]
                            }
           | TitleSlide { titleSlideHeader :: [ParaElem]}
           | ContentSlide { contentSlideHeader :: [ParaElem]
                          , contentSlideContent :: [Shape]
                          }
           deriving (Show, Eq)

data SlideElement = SlideElement Pixels Pixels Pixels Pixels Shape
  deriving (Show, Eq)

data Shape = Pic FilePath Text.Pandoc.Definition.Attr [ParaElem]
           | GraphicFrame [Graphic] [ParaElem]
           | TextBox [Paragraph]
  deriving (Show, Eq)

type Cell = [Paragraph]

data TableProps = TableProps { tblPrFirstRow :: Bool
                             , tblPrBandRow :: Bool
                             } deriving (Show, Eq)

type ColWidth = Integer

data Graphic = Tbl TableProps [ColWidth] [Cell] [[Cell]]
  deriving (Show, Eq)


data Paragraph = Paragraph { paraProps :: ParaProps
                           , paraElems  :: [ParaElem]
                           } deriving (Show, Eq)

data HeaderType = TitleHeader | SlideHeader | InternalHeader Int
                deriving (Show, Eq)

-- type StartingAt = Int

-- data AutoNumType = ArabicNum
--                  | AlphaUpperNum
--                  | AlphaLowerNum
--                  | RomanUpperNum
--                  | RomanLowerNum
--                  deriving (Show, Eq)

-- data AutoNumDelim = PeriodDelim
--                   | OneParenDelim
--                   | TwoParensDelim
--                   deriving (Show, Eq)

autoNumberingToType :: ListAttributes -> String
autoNumberingToType (_, numStyle, numDelim) =
  typeString ++ delimString
  where
    typeString = case numStyle of
      Decimal -> "arabic"
      UpperAlpha -> "alphaUc"
      LowerAlpha -> "alphaLc"
      UpperRoman -> "romanUc"
      LowerRoman -> "romanLc"
      _          -> "arabic"
    delimString = case numDelim of
      Period -> "Period"
      OneParen -> "ParenR"
      TwoParens -> "ParenBoth"
      _         -> "Period"

data BulletType = Bullet
                | AutoNumbering ListAttributes
  deriving (Show, Eq)

data Algnment = AlgnLeft | AlgnRight | AlgnCenter
  deriving (Show, Eq)

data ParaProps = ParaProps { pPropHeaderType :: Maybe HeaderType
                           , pPropMarginLeft :: Maybe Pixels
                           , pPropMarginRight :: Maybe Pixels
                           , pPropLevel :: Int
                           , pPropBullet :: Maybe BulletType
                           , pPropAlign :: Maybe Algnment
                           } deriving (Show, Eq)

instance Default ParaProps where
  def = ParaProps { pPropHeaderType = Nothing
                  , pPropMarginLeft = Just 0
                  , pPropMarginRight = Just 0
                  , pPropLevel = 0
                  , pPropBullet = Nothing
                  , pPropAlign = Nothing
                  }

newtype TeXString = TeXString {unTeXString :: String}
  deriving (Eq, Show)

data ParaElem = Break
              | Run RunProps String
              -- It would be more elegant to have native TeXMath
              -- Expressions here, but this allows us to use
              -- `convertmath` from T.P.Writers.Math. Will perhaps
              -- revisit in the future.
              | MathElem MathType TeXString
              deriving (Show, Eq)

data Strikethrough = NoStrike | SingleStrike | DoubleStrike
  deriving (Show, Eq)

data Capitals = NoCapitals | SmallCapitals | AllCapitals
  deriving (Show, Eq)

type URL = String

data RunProps = RunProps { rPropBold :: Bool
                         , rPropItalics :: Bool
                         , rStrikethrough :: Maybe Strikethrough
                         , rBaseline :: Maybe Int
                         , rCap :: Maybe Capitals
                         , rLink :: Maybe (URL, String)
                         , rPropCode :: Bool
                         , rPropBlockQuote :: Bool
                         , rPropForceSize :: Maybe Pixels
                         } deriving (Show, Eq)

instance Default RunProps where
  def = RunProps { rPropBold = False
                 , rPropItalics = False
                 , rStrikethrough = Nothing
                 , rBaseline = Nothing
                 , rCap = Nothing
                 , rLink = Nothing
                 , rPropCode = False
                 , rPropBlockQuote = False
                 , rPropForceSize = Nothing
                 }

--------------------------------------------------

inlinesToParElems :: Monad m => [Inline] -> P m [ParaElem]
inlinesToParElems ils = concatMapM inlineToParElems ils

inlineToParElems :: Monad m => Inline -> P m [ParaElem]
inlineToParElems (Str s) = do
  pr <- asks envRunProps
  return [Run pr s]
inlineToParElems (Emph ils) =
  local (\r -> r{envRunProps = (envRunProps r){rPropItalics=True}}) $
  inlinesToParElems ils
inlineToParElems (Strong ils) =
  local (\r -> r{envRunProps = (envRunProps r){rPropBold=True}}) $
  inlinesToParElems ils
inlineToParElems (Strikeout ils) =
  local (\r -> r{envRunProps = (envRunProps r){rStrikethrough=Just SingleStrike}}) $
  inlinesToParElems ils
inlineToParElems (Superscript ils) =
  local (\r -> r{envRunProps = (envRunProps r){rBaseline=Just 30000}}) $
  inlinesToParElems ils
inlineToParElems (Subscript ils) =
  local (\r -> r{envRunProps = (envRunProps r){rBaseline=Just (-25000)}}) $
  inlinesToParElems ils
inlineToParElems (SmallCaps ils) =
  local (\r -> r{envRunProps = (envRunProps r){rCap = Just SmallCapitals}}) $
  inlinesToParElems ils
inlineToParElems Space = inlineToParElems (Str " ")
inlineToParElems SoftBreak = inlineToParElems (Str " ")
inlineToParElems LineBreak = return [Break]
inlineToParElems (Link _ ils (url, title)) = do
  local (\r ->r{envRunProps = (envRunProps r){rLink = Just (url, title)}}) $
    inlinesToParElems ils
inlineToParElems (Code _ str) = do
  local (\r ->r{envRunProps = def{rPropCode = True}}) $
    inlineToParElems $ Str str
inlineToParElems (Math mathtype str) =
  return [MathElem mathtype (TeXString str)]
inlineToParElems (Note blks) = do
  notes <- gets stNoteIds
  let maxNoteId = case M.keys notes of
        [] -> 0
        lst -> maximum lst
      curNoteId = maxNoteId + 1
  modify $ \st -> st { stNoteIds = M.insert curNoteId blks notes }
  inlineToParElems $ Superscript [Str $ show curNoteId]
inlineToParElems (Span _ ils) = concatMapM inlineToParElems ils
inlineToParElems (RawInline _ _) = return []
inlineToParElems _ = return []

isListType :: Block -> Bool
isListType (OrderedList _ _) = True
isListType (BulletList _) = True
isListType (DefinitionList _) = True
isListType _ = False

blockToParagraphs :: PandocMonad m => Block -> P m [Paragraph]
blockToParagraphs (Plain ils) = do
  parElems <- inlinesToParElems ils
  pProps <- asks envParaProps
  return [Paragraph pProps parElems]
blockToParagraphs (Para ils) = do
  parElems <- inlinesToParElems ils
  pProps <- asks envParaProps
  return [Paragraph pProps parElems]
blockToParagraphs (LineBlock ilsList) = do
  parElems <- inlinesToParElems $ intercalate [LineBreak] ilsList
  pProps <- asks envParaProps
  return [Paragraph pProps parElems]
-- TODO: work out the attributes
blockToParagraphs (CodeBlock attr str) =
  local (\r -> r{envParaProps = def{pPropMarginLeft = Just 100}}) $
  blockToParagraphs $ Para [Code attr str]
-- We can't yet do incremental lists, but we should render a
-- (BlockQuote List) as a list to maintain compatibility with other
-- formats.
blockToParagraphs (BlockQuote (blk : blks)) | isListType blk = do
  ps  <- blockToParagraphs blk
  ps' <- blockToParagraphs $ BlockQuote blks
  return $ ps ++ ps'
blockToParagraphs (BlockQuote blks) =
  local (\r -> r{ envParaProps = (envParaProps r){pPropMarginLeft = Just 100}
                , envRunProps = (envRunProps r){rPropForceSize = Just blockQuoteSize}})$
  concatMapM blockToParagraphs blks
-- TODO: work out the format
blockToParagraphs (RawBlock _ _) = return []
  -- parElems <- inlinesToParElems [Str str]
  -- paraProps <- asks envParaProps
  -- return [Paragraph paraProps parElems]
-- TODO: work out the format
blockToParagraphs (Header n _ ils) = do
  slideLevel <- asks envSlideLevel
  parElems <- inlinesToParElems ils
  -- For the time being we're not doing headers inside of bullets, but
  -- we might change that.
  let headerType = case n `compare` slideLevel of
                     LT -> TitleHeader
                     EQ -> SlideHeader
                     GT -> InternalHeader (n - slideLevel)
  return [Paragraph def{pPropHeaderType = Just headerType} parElems]
blockToParagraphs (BulletList blksLst) = do
  pProps <- asks envParaProps
  let lvl = pPropLevel pProps
  local (\env -> env{ envInList = True
                    , envParaProps = pProps{ pPropLevel = lvl + 1
                                           , pPropBullet = Just Bullet
                                           , pPropMarginLeft = Nothing
                                           }}) $
    concatMapM multiParBullet blksLst
blockToParagraphs (OrderedList listAttr blksLst) = do
  pProps <- asks envParaProps
  let lvl = pPropLevel pProps
  local (\env -> env{ envInList = True
                    , envParaProps = pProps{ pPropLevel = lvl + 1
                                           , pPropBullet = Just (AutoNumbering listAttr)
                                           , pPropMarginLeft = Nothing
                                           }}) $
    concatMapM multiParBullet blksLst
blockToParagraphs (DefinitionList entries) = do
  let go :: PandocMonad m => ([Inline], [[Block]]) -> P m [Paragraph]
      go (ils, blksLst) = do
        term <-blockToParagraphs $ Para [Strong ils]
        -- For now, we'll treat each definition term as a
        -- blockquote. We can extend this further later.
        definition <- concatMapM (blockToParagraphs . BlockQuote) blksLst
        return $ term ++ definition
  concatMapM go entries
blockToParagraphs (Div _ blks)  = concatMapM blockToParagraphs blks
-- TODO
blockToParagraphs blk = do
  P.report $ BlockNotRendered blk
  return []

-- Make sure the bullet env gets turned off after the first para.
multiParBullet :: PandocMonad m => [Block] -> P m [Paragraph]
multiParBullet [] = return []
multiParBullet (b:bs) = do
  pProps <- asks envParaProps
  p <- blockToParagraphs b
  ps <- local (\env -> env{envParaProps = pProps{pPropBullet = Nothing}}) $
    concatMapM blockToParagraphs bs
  return $ p ++ ps

cellToParagraphs :: PandocMonad m => Alignment -> TableCell -> P m [Paragraph]
cellToParagraphs algn tblCell = do
  paras <- mapM (blockToParagraphs) tblCell
  let alignment = case algn of
        AlignLeft -> Just AlgnLeft
        AlignRight -> Just AlgnRight
        AlignCenter -> Just AlgnCenter
        AlignDefault -> Nothing
      paras' = map (map (\p -> p{paraProps = (paraProps p){pPropAlign = alignment}})) paras
  return $ concat paras'

rowToParagraphs :: PandocMonad m => [Alignment] -> [TableCell] -> P m [[Paragraph]]
rowToParagraphs algns tblCells = do
  -- We have to make sure we have the right number of alignments
  let pairs = zip (algns ++ repeat AlignDefault) tblCells
  mapM (\(a, tc) -> cellToParagraphs a tc) pairs

blockToShape :: PandocMonad m => Block -> P m Shape
blockToShape (Plain (il:_)) | Image attr ils (url, _) <- il =
      Pic url attr <$> (inlinesToParElems ils)
blockToShape (Para (il:_))  | Image attr ils (url, _) <- il =
      Pic url attr <$> (inlinesToParElems ils)
blockToShape (Table caption algn _ hdrCells rows) = do
  caption' <- inlinesToParElems caption
  pageWidth <- presSizeWidth <$> asks envPresentationSize
  hdrCells' <- rowToParagraphs algn hdrCells
  rows' <- mapM (rowToParagraphs algn) rows
  let tblPr = if null hdrCells
              then TableProps { tblPrFirstRow = False
                              , tblPrBandRow = True
                              }
              else TableProps { tblPrFirstRow = True
                              , tblPrBandRow = True
                              }
      colWidths = if null hdrCells
                 then case rows of
                        r : _ | not (null r) -> replicate (length r) $
                                                (pageWidth - (2 * hardcodedTableMargin))`div` (toInteger $ length r)
                        -- satisfy the compiler. This is the same as
                        -- saying that rows is empty, but the compiler
                        -- won't understand that `[]` exhausts the
                        -- alternatives.
                        _ -> []
                 else replicate (length hdrCells) $
                      (pageWidth - (2 * hardcodedTableMargin)) `div` (toInteger $ length hdrCells)

  return $ GraphicFrame [Tbl tblPr colWidths hdrCells' rows'] caption'
blockToShape blk = TextBox <$> blockToParagraphs blk

blocksToShapes :: PandocMonad m => [Block] -> P m [Shape]
blocksToShapes blks = combineShapes <$> mapM blockToShape blks

splitBlocks' :: Monad m => [Block] -> [[Block]] -> [Block] -> P m [[Block]]
splitBlocks' cur acc [] = return $ acc ++ (if null cur then [] else [cur])
splitBlocks' cur acc (HorizontalRule : blks) =
  splitBlocks' [] (acc ++ (if null cur then [] else [cur])) blks
splitBlocks' cur acc (h@(Header n _ _) : blks) = do
  slideLevel <- asks envSlideLevel
  case compare n slideLevel of
    LT -> splitBlocks' [] (acc ++ (if null cur then [] else [cur]) ++ [[h]]) blks
    EQ -> splitBlocks' [h] (acc ++ (if null cur then [] else [cur])) blks
    GT -> splitBlocks' (cur ++ [h]) acc blks
splitBlocks' cur acc ((Para (img@(Image _ _ _):ils)) : blks) = do
  slideLevel <- asks envSlideLevel
  case cur of
    (Header n _ _) : [] | n == slideLevel ->
                            splitBlocks' []
                            (acc ++ [cur ++ [Para [img]]])
                            (if null ils then blks else (Para ils) : blks)
    _ ->  splitBlocks' []
          (acc ++ (if null cur then [] else [cur]) ++ [[Para [img]]])
          (if null ils then blks else (Para ils) : blks)
splitBlocks' cur acc ((Plain (img@(Image _ _ _):ils)) : blks) = do
  slideLevel <- asks envSlideLevel
  case cur of
    (Header n _ _) : [] | n == slideLevel ->
                            splitBlocks' []
                            (acc ++ [cur ++ [Para [img]]])
                            (if null ils then blks else (Plain ils) : blks)
    _ ->  splitBlocks' []
          (acc ++ (if null cur then [] else [cur]) ++ [[Para [img]]])
          (if null ils then blks else (Plain ils) : blks)
splitBlocks' cur acc (tbl@(Table _ _ _ _ _) : blks) = do
  slideLevel <- asks envSlideLevel
  case cur of
    (Header n _ _) : [] | n == slideLevel ->
                            splitBlocks' [] (acc ++ [cur ++ [tbl]]) blks
    _ ->  splitBlocks' [] (acc ++ (if null cur then [] else [cur]) ++ [[tbl]]) blks
splitBlocks' cur acc (blk : blks) = splitBlocks' (cur ++ [blk]) acc blks

splitBlocks :: Monad m => [Block] -> P m [[Block]]
splitBlocks = splitBlocks' [] []

blocksToSlide' :: PandocMonad m => Int -> [Block] -> P m Slide
blocksToSlide' lvl ((Header n _ ils) : blks)
  | n < lvl = do
      hdr <- inlinesToParElems ils
      return $ TitleSlide {titleSlideHeader = hdr}
  | n == lvl = do
      hdr <- inlinesToParElems ils
      inNoteSlide <- asks envInNoteSlide
      shapes <- if inNoteSlide
                then forceFontSize noteSize $ blocksToShapes blks
                else blocksToShapes blks
      return $ ContentSlide { contentSlideHeader = hdr
                            , contentSlideContent = shapes
                            }
blocksToSlide' _ (blk : blks) = do
      inNoteSlide <- asks envInNoteSlide
      shapes <- if inNoteSlide
                then forceFontSize noteSize $ blocksToShapes (blk : blks)
                else blocksToShapes (blk : blks)
      return $ ContentSlide { contentSlideHeader = []
                            , contentSlideContent = shapes
                            }
blocksToSlide' _ [] = return $ ContentSlide { contentSlideHeader = []
                                            , contentSlideContent = []
                                            }

blocksToSlide :: PandocMonad m => [Block] -> P m Slide
blocksToSlide blks = do
  slideLevel <- asks envSlideLevel
  blocksToSlide' slideLevel blks

makeNoteEntry :: Int -> [Block] -> [Block]
makeNoteEntry n blks =
  let enum = Str (show n ++ ".")
  in
    case blks of
      (Para ils : blks') -> (Para $ enum : Space : ils) : blks'
      _ -> (Para [enum]) : blks

forceFontSize :: PandocMonad m => Pixels -> P m a -> P m a
forceFontSize px x = do
  rpr <- asks envRunProps
  local (\r -> r {envRunProps = rpr{rPropForceSize = Just px}}) x

-- Right now, there's no logic for making more than one slide, but I
-- want to leave the option open to make multiple slides if we figure
-- out how to guess at how much space the text of the notes will take
-- up (or if we allow a way for it to be manually controlled). Plus a
-- list will make it easier to put together in the final
-- `blocksToPresentation` function (since we can just add an empty
-- list without checking the state).
makeNotesSlides :: PandocMonad m => P m [Slide]
makeNotesSlides = local (\env -> env{envInNoteSlide=True}) $ do
  noteIds <- gets stNoteIds
  if M.null noteIds
    then return []
    else do let hdr = Header 2 nullAttr [Str "Notes"]
            blks <- return $
                    concatMap (\(n, bs) -> makeNoteEntry n bs) $
                    M.toList noteIds
            sld <- blocksToSlide $ hdr : blks
            return [sld]

getMetaSlide :: PandocMonad m => P m (Maybe Slide)
getMetaSlide  = do
  meta <- asks envMetadata
  title <- inlinesToParElems $ docTitle meta
  subtitle <- inlinesToParElems $
    case lookupMeta "subtitle" meta of
      Just (MetaString s)           -> [Str s]
      Just (MetaInlines ils)        -> ils
      Just (MetaBlocks [Plain ils]) -> ils
      Just (MetaBlocks [Para ils])  -> ils
      _                             -> []
  authors <- mapM inlinesToParElems $ docAuthors meta
  date <- inlinesToParElems $ docDate meta
  if null title && null subtitle && null authors && null date
    then return Nothing
    else return $ Just $ MetadataSlide { metadataSlideTitle = title
                                       , metadataSlideSubtitle = subtitle
                                       , metadataSlideAuthors = authors
                                       , metadataSlideDate = date
                                       }

blocksToPresentation :: PandocMonad m => [Block] -> P m Presentation
blocksToPresentation blks = do
  blksLst <- splitBlocks blks
  slides <- mapM blocksToSlide blksLst
  noteSlides <- makeNotesSlides
  let slides' = slides ++ noteSlides
  metadataslide <- getMetaSlide
  presSize <- asks envPresentationSize
  return $ case metadataslide of
             Just metadataslide' -> Presentation presSize $ metadataslide' : slides'
             Nothing            -> Presentation presSize slides'

--------------------------------------------------------------------

copyFileToArchive :: PandocMonad m => Archive -> FilePath -> P m Archive
copyFileToArchive arch fp = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  case findEntryByPath fp refArchive `mplus` findEntryByPath fp distArchive of
    Nothing -> fail $ fp ++ " missing in reference file"
    Just e -> return $ addEntryToArchive e arch

getMediaFiles :: PandocMonad m => P m [FilePath]
getMediaFiles = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  let allEntries = nub $ filesInArchive refArchive ++ filesInArchive distArchive
  return $ filter (isPrefixOf "ppt/media") allEntries


copyFileToArchiveIfExists :: PandocMonad m => Archive -> FilePath -> P m Archive
copyFileToArchiveIfExists arch fp = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  case findEntryByPath fp refArchive `mplus` findEntryByPath fp distArchive of
    Nothing -> return $ arch
    Just e -> return $ addEntryToArchive e arch

inheritedFiles :: [FilePath]
inheritedFiles = [ "_rels/.rels"
                 , "docProps/app.xml"
                 , "docProps/core.xml"
                 , "ppt/slideLayouts/slideLayout4.xml"
                 , "ppt/slideLayouts/_rels/slideLayout9.xml.rels"
                 , "ppt/slideLayouts/_rels/slideLayout2.xml.rels"
                 , "ppt/slideLayouts/_rels/slideLayout10.xml.rels"
                 , "ppt/slideLayouts/_rels/slideLayout1.xml.rels"
                 , "ppt/slideLayouts/_rels/slideLayout3.xml.rels"
                 , "ppt/slideLayouts/_rels/slideLayout5.xml.rels"
                 , "ppt/slideLayouts/_rels/slideLayout7.xml.rels"
                 , "ppt/slideLayouts/_rels/slideLayout8.xml.rels"
                 , "ppt/slideLayouts/_rels/slideLayout11.xml.rels"
                 , "ppt/slideLayouts/_rels/slideLayout4.xml.rels"
                 , "ppt/slideLayouts/_rels/slideLayout6.xml.rels"
                 , "ppt/slideLayouts/slideLayout2.xml"
                 , "ppt/slideLayouts/slideLayout8.xml"
                 , "ppt/slideLayouts/slideLayout11.xml"
                 , "ppt/slideLayouts/slideLayout3.xml"
                 , "ppt/slideLayouts/slideLayout6.xml"
                 , "ppt/slideLayouts/slideLayout9.xml"
                 , "ppt/slideLayouts/slideLayout5.xml"
                 , "ppt/slideLayouts/slideLayout7.xml"
                 , "ppt/slideLayouts/slideLayout1.xml"
                 , "ppt/slideLayouts/slideLayout10.xml"
                 -- , "ppt/_rels/presentation.xml.rels"
                 , "ppt/theme/theme1.xml"
                 , "ppt/presProps.xml"
                 -- , "ppt/slides/_rels/slide1.xml.rels"
                 -- , "ppt/slides/_rels/slide2.xml.rels"
                 -- This is the one we're
                 -- going to build
                 -- , "ppt/slides/slide2.xml"
                 -- , "ppt/slides/slide1.xml"
                 , "ppt/viewProps.xml"
                 , "ppt/tableStyles.xml"
                 , "ppt/slideMasters/_rels/slideMaster1.xml.rels"
                 , "ppt/slideMasters/slideMaster1.xml"
                 -- , "ppt/presentation.xml"
                 -- , "[Content_Types].xml"
                 ]

-- Here are some that might not be there. We won't fail if they're not
possibleInheritedFiles :: [FilePath]
possibleInheritedFiles = [ "ppt/theme/_rels/theme1.xml.rels" ]

presentationToArchive :: PandocMonad m => Presentation -> P m Archive
presentationToArchive p@(Presentation _ slides) = do
  newArch <- foldM copyFileToArchive emptyArchive inheritedFiles
  mediaDir <- getMediaFiles
  newArch' <- foldM copyFileToArchiveIfExists newArch $
              possibleInheritedFiles ++ mediaDir
  -- presentation entry and rels. We have to do the rels first to make
  -- sure we know the correct offset for the rIds.
  presEntry <- presentationToPresEntry p
  presRelsEntry <- presentationToRelsEntry p
  slideEntries <- mapM (\(s, n) -> slideToEntry s n) $ zip slides [1..]
  slideRelEntries <- mapM (\(s,n) -> slideToSlideRelEntry s n) $ zip slides [1..]
  -- These have to come after everything, because they need the info
  -- built up in the state.
  mediaEntries <- makeMediaEntries
  contentTypesEntry <- presentationToContentTypes p >>= contentTypesToEntry
  -- fold everything into our inherited archive and return it.
  return $ foldr addEntryToArchive newArch' $
    slideEntries ++
    slideRelEntries ++
    mediaEntries ++
    [contentTypesEntry, presEntry, presRelsEntry]

--------------------------------------------------

combineShapes :: [Shape] -> [Shape]
combineShapes [] = []
combineShapes (s : []) = [s]
combineShapes (pic@(Pic _ _ _) : ss) = pic : combineShapes ss
combineShapes ((TextBox []) : ss) = combineShapes ss
combineShapes (s : TextBox [] : ss) = combineShapes (s : ss)
combineShapes (s@(TextBox (p:ps)) : s'@(TextBox (p':ps')) : ss)
  | pPropHeaderType (paraProps p) == Just TitleHeader ||
    pPropHeaderType (paraProps p) == Just SlideHeader =
      TextBox [p] : (combineShapes $ TextBox ps : s' : ss)
  | pPropHeaderType (paraProps p') == Just TitleHeader ||
    pPropHeaderType (paraProps p') == Just SlideHeader =
      s : TextBox [p'] : (combineShapes $ TextBox ps' : ss)
  | otherwise = combineShapes $ TextBox ((p:ps) ++ (p':ps')) : ss
combineShapes (s:ss) = s : combineShapes ss

--------------------------------------------------

getLayout :: PandocMonad m => Slide -> P m Element
getLayout slide = do
  let layoutpath = case slide of
        (MetadataSlide _ _ _ _) -> "ppt/slideLayouts/slideLayout1.xml"
        (TitleSlide _)          -> "ppt/slideLayouts/slideLayout3.xml"
        (ContentSlide _ _)      -> "ppt/slideLayouts/slideLayout2.xml"
  distArchive <- asks envDistArchive
  root <- case findEntryByPath layoutpath distArchive of
        Just e -> case parseXMLDoc $ UTF8.toStringLazy $ fromEntry e of
                    Just element -> return $ element
                    Nothing      -> throwError $
                                    PandocSomeError $
                                    layoutpath ++ " corrupt in reference file"
        Nothing -> throwError $
                   PandocSomeError $
                   layoutpath ++ " missing in reference file"
  return root
  -- let ns = elemToNameSpaces root
  -- case findChild (elemName ns "p" "cSld") root of
  --   Just element' -> return element'
  --   Nothing       -> throwError $
  --                    PandocSomeError $
  --                    layoutpath ++ " not correctly formed layout file"

shapeHasName :: NameSpaces -> String -> Element -> Bool
shapeHasName ns name element
  | Just nvSpPr <- findChild (elemName ns "p" "nvSpPr") element
  , Just cNvPr <- findChild (elemName ns "p" "cNvPr") nvSpPr
  , Just nm <- findAttr (QName "name" Nothing Nothing) cNvPr =
      nm == name
  | otherwise = False

-- getContentTitleShape :: NameSpaces -> Element -> Maybe Element
-- getContentTitleShape ns spTreeElem
--   | isElem ns "p" "spTree" spTreeElem =
--   filterChild (\e -> (isElem ns "p" "sp" e) && (shapeHasName ns "Title 1" e)) spTreeElem
--   | otherwise = Nothing

-- getSubtitleShape :: NameSpaces -> Element -> Maybe Element
-- getSubtitleShape ns spTreeElem
--   | isElem ns "p" "spTree" spTreeElem =
--   filterChild (\e -> (isElem ns "p" "sp" e) && (shapeHasName ns "Subtitle 2" e)) spTreeElem
--   | otherwise = Nothing

-- getDateShape :: NameSpaces -> Element -> Maybe Element
-- getDateShape ns spTreeElem
--   | isElem ns "p" "spTree" spTreeElem =
--   filterChild (\e -> (isElem ns "p" "sp" e) && (shapeHasName ns "Date Placeholder 3" e)) spTreeElem
--   | otherwise = Nothing

getContentShape :: NameSpaces -> Element -> Maybe Element
getContentShape ns spTreeElem
  | isElem ns "p" "spTree" spTreeElem =
  filterChild (\e -> (isElem ns "p" "sp" e) && (shapeHasName ns "Content Placeholder 2" e)) spTreeElem
  | otherwise = Nothing


-- cursorHasName :: QName -> XMLC.Cursor -> Bool
-- cursorHasName nm cur = case XMLC.current cur of
--   Elem element -> case XMLC.tagName $ XMLC.getTag element of
--                        nm -> True
--                        _ -> False
--   _ -> False

-- fillInTxBody :: NameSpaces -> [Paragraph] -> Element -> Element
-- fillInTxBody ns paras txBodyElem
--   | isElem ns "p" "txBody" txBodyElem =
--       replaceNamedChildren ns "a" "p" (map paragraphToElement paras) txBodyElem
--   | otherwise = txBodyElem

-- fillInShape :: NameSpaces -> Shape -> Element -> Element
-- fillInShape ns shape spElem
--   | TextBox paras <- shape
--   , isElemn ns "p" "sp" spElem =
--       replaceNamedChildren ns "p" "txBody" (fillInTxBody ns paras sp


-- fillInShape :: NameSpaces -> Element -> Shape -> Element
-- fillInShape ns spElem (TextBox paras) = fillInParagraphs ns spElem paras
-- fillInShape _ spElem pic = spElem

contentIsElem :: NameSpaces -> String -> String -> Content -> Bool
contentIsElem ns prefix name (Elem element) = isElem ns prefix name element
contentIsElem _ _ _ _ = False

replaceNamedChildren :: NameSpaces -> String -> String -> [Element] -> Element -> Element
replaceNamedChildren ns prefix name newKids element =
  let content = elContent element
      content' = filter (\c -> not (contentIsElem ns prefix name c)) content
  in
    element{elContent = content' ++ map Elem newKids}


----------------------------------------------------------------

registerLink :: PandocMonad m => (URL, String) -> P m Int
registerLink link = do
  curSlideId <- gets stCurSlideId
  linkReg <- gets stLinkIds
  mediaReg <- gets stMediaIds
  let maxLinkId = case M.lookup curSlideId linkReg of
        Just mp -> case M.keys mp of
          [] -> 1
          ks -> maximum ks
        Nothing -> 1
      maxMediaId = case M.lookup curSlideId mediaReg of
        Just [] -> 1
        Just mInfos -> maximum $ map mInfoLocalId mInfos
        Nothing -> 1
      maxId = max maxLinkId maxMediaId
      slideLinks = case M.lookup curSlideId linkReg of
        Just mp -> M.insert (maxId + 1) link mp
        Nothing -> M.singleton (maxId + 1) link
  modify $ \st -> st{ stLinkIds = M.insert curSlideId slideLinks linkReg}
  return $ maxId + 1

registerMedia :: PandocMonad m => FilePath -> [ParaElem] -> P m MediaInfo
registerMedia fp caption = do
  curSlideId <- gets stCurSlideId
  linkReg <- gets stLinkIds
  mediaReg <- gets stMediaIds
  globalIds <- gets stMediaGlobalIds
  let maxLinkId = case M.lookup curSlideId linkReg of
        Just mp -> case M.keys mp of
          [] -> 1
          ks -> maximum ks
        Nothing -> 1
      maxMediaId = case M.lookup curSlideId mediaReg of
        Just [] -> 1
        Just mInfos -> maximum $ map mInfoLocalId mInfos
        Nothing -> 1
      maxLocalId = max maxLinkId maxMediaId

      maxGlobalId = case M.elems globalIds of
        [] -> 0
        ids -> maximum ids

  (imgBytes, mbMt) <- P.fetchItem fp
  let imgExt = (mbMt >>= extensionFromMimeType >>= (\x -> return $ '.':x))
               <|>
               case imageType imgBytes of
                 Just Png  -> Just ".png"
                 Just Jpeg -> Just ".jpeg"
                 Just Gif  -> Just ".gif"
                 Just Pdf  -> Just ".pdf"
                 Just Eps  -> Just ".eps"
                 Just Svg  -> Just ".svg"
                 Nothing   -> Nothing

  let newGlobalId = case M.lookup fp globalIds of
        Just ident -> ident
        Nothing    -> maxGlobalId + 1

  let newGlobalIds = M.insert fp newGlobalId globalIds

  let mediaInfo = MediaInfo { mInfoFilePath = fp
                            , mInfoLocalId = maxLocalId + 1
                            , mInfoGlobalId = newGlobalId
                            , mInfoMimeType = mbMt
                            , mInfoExt = imgExt
                            , mInfoCaption = (not . null) caption
                            }

  let slideMediaInfos = case M.lookup curSlideId mediaReg of
        Just minfos -> mediaInfo : minfos
        Nothing     -> [mediaInfo]


  modify $ \st -> st{ stMediaIds = M.insert curSlideId slideMediaInfos mediaReg
                    , stMediaGlobalIds = newGlobalIds
                    }
  return mediaInfo

makeMediaEntry :: PandocMonad m => MediaInfo -> P m Entry
makeMediaEntry mInfo = do
  epochtime <- (floor . utcTimeToPOSIXSeconds) <$> asks envUTCTime
  (imgBytes, _) <- P.fetchItem (mInfoFilePath mInfo)
  let ext = case mInfoExt mInfo of
              Just e -> e
              Nothing -> ""
  let fp = "ppt/media/image" ++ (show $ mInfoGlobalId mInfo) ++ ext
  return $ toEntry fp epochtime $ BL.fromStrict imgBytes

makeMediaEntries :: PandocMonad m => P m [Entry]
makeMediaEntries = do
  mediaInfos <- gets stMediaIds
  let allInfos = mconcat $ M.elems mediaInfos
  mapM makeMediaEntry allInfos

-- | Scales the image to fit the page
-- sizes are passed in emu
fitToPage' :: (Double, Double)  -- image size in emu
           -> Integer           -- pageWidth
           -> Integer           -- pageHeight
           -> (Integer, Integer) -- imagesize
fitToPage' (x, y) pageWidth pageHeight
  -- Fixes width to the page width and scales the height
  | x <= fromIntegral pageWidth && y <= fromIntegral pageHeight =
      (floor x, floor y)
  | x / fromIntegral pageWidth > y / fromIntegral pageWidth =
      (pageWidth, floor $ ((fromIntegral pageWidth) / x) * y)
  | otherwise =
      (floor $ ((fromIntegral pageHeight) / y) * x, pageHeight)

positionImage :: (Double, Double) -> Integer -> Integer -> (Integer, Integer)
positionImage (x, y) pageWidth pageHeight =
  let (x', y') = fitToPage' (x, y) pageWidth pageHeight
  in
    ((pageWidth - x') `div` 2, (pageHeight - y') `div`  2)

getMaster :: PandocMonad m => P m Element
getMaster = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  parseXml refArchive distArchive "ppt/slideMasters/slideMaster1.xml"

-- We want to get the header dimensions, so we can make sure that the
-- image goes underneath it. We only use this in a content slide if it
-- has a header.

getHeaderSize :: PandocMonad m => P m ((Integer, Integer), (Integer, Integer))
getHeaderSize = do
  master <- getMaster
  let ns = elemToNameSpaces master
      sps = [master] >>=
            findChildren (elemName ns "p" "cSld") >>=
            findChildren (elemName ns "p" "spTree") >>=
            findChildren (elemName ns "p" "sp")
      mbXfrm =
        listToMaybe (filter (shapeHasName ns "Title Placeholder 1") sps) >>=
        findChild (elemName ns "p" "spPr") >>=
        findChild (elemName ns "a" "xfrm")
      xoff = mbXfrm >>=
             findChild (elemName ns "a" "off") >>=
             findAttr (QName "x" Nothing Nothing) >>=
             (listToMaybe . (\s -> reads s :: [(Integer, String)]))
      yoff = mbXfrm >>=
             findChild (elemName ns "a" "off") >>=
             findAttr (QName "y" Nothing Nothing) >>=
             (listToMaybe . (\s -> reads s :: [(Integer, String)]))
      xext = mbXfrm >>=
             findChild (elemName ns "a" "ext") >>=
             findAttr (QName "cx" Nothing Nothing) >>=
             (listToMaybe . (\s -> reads s :: [(Integer, String)]))
      yext = mbXfrm >>=
             findChild (elemName ns "a" "ext") >>=
             findAttr (QName "cy" Nothing Nothing) >>=
             (listToMaybe . (\s -> reads s :: [(Integer, String)]))
      off = case xoff of
              Just (xoff', _) | Just (yoff',_) <- yoff -> (xoff', yoff')
              _                               -> (1043490, 1027664)
      ext = case xext of
              Just (xext', _) | Just (yext',_) <- yext -> (xext', yext')
              _                               -> (7024744, 1143000)
  return $ (off, ext)


-- Hard-coded for now
captionPosition :: ((Integer, Integer), (Integer, Integer))
captionPosition = ((457200, 6061972), (8229600, 527087))

createCaption :: PandocMonad m => [ParaElem] -> P m Element
createCaption paraElements = do
  let para = Paragraph def{pPropAlign = Just AlgnCenter} paraElements
  elements <- mapM paragraphToElement [para]
  let ((x, y), (cx, cy)) = captionPosition
  let txBody = mknode "p:txBody" [] $
               [mknode "a:bodyPr" [] (), mknode "a:lstStyle" [] ()] ++ elements
  return $
    mknode "p:sp" [] [ mknode "p:nvSpPr" []
                       [ mknode "p:cNvPr" [("id","1"), ("name","TextBox 3")] ()
                       , mknode "p:cNvSpPr" [("txBox", "1")] ()
                       , mknode "p:nvPr" [] ()
                       ]
                     , mknode "p:spPr" []
                       [ mknode "a:xfrm" []
                         [ mknode "a:off" [("x", show x), ("y", show y)] ()
                         , mknode "a:ext" [("cx", show cx), ("cy", show cy)] ()
                         ]
                       , mknode "a:prstGeom" [("prst", "rect")]
                         [ mknode "a:avLst" [] ()
                         ]
                       , mknode "a:noFill" [] ()
                       ]
                     , txBody
                     ]

-- Largely lifted from inlineToOpenXML' in T.P.W.Docx. Can't be easily
-- abstracted because of some different namespaces and monads. TODO.
makePicElement :: PandocMonad m
               => MediaInfo
               -> Text.Pandoc.Definition.Attr
               -> P m Element
makePicElement mInfo attr = do
  opts <- asks envOpts
  pageWidth <- presSizeWidth <$> asks envPresentationSize
  pageHeight <- getPageHeight <$> asks envPresentationSize
  hasHeader <- asks envSlideHasHeader
  let hasCaption = mInfoCaption mInfo
  (imgBytes, _) <- P.fetchItem (mInfoFilePath mInfo)
  -- We're not using x exts
  ((hXoff, hYoff), (_, hYext)) <- if hasHeader
                                  then getHeaderSize
                                  else return ((0, 0), (0, 0))

  let ((capX, capY), (_, _)) = if hasCaption
                               then captionPosition
                               else ((0,0), (0,0))
  let (xpt,ypt) = desiredSizeInPoints opts attr
                  (either (const def) id (imageSize opts imgBytes))
  -- 12700 emu = 1 pt
  let (xemu,yemu) = fitToPage' (xpt * 12700, ypt * 12700)
                    ((pageWidth * 12700) - (2 * hXoff) - (2 * capX))
                    ((if hasCaption then capY else (pageHeight * 12700)) - (hYoff + hYext))
      (xoff, yoff) = positionImage (xpt * 12700, ypt * 12700) (pageWidth * 12700) (pageHeight * 12700)
      xoff' = if hasHeader then xoff + hXoff else xoff
      xoff'' = if hasCaption then xoff' + capX else xoff'
      yoff' = if hasHeader then hYoff + hYext else yoff
      -- let (xemu,yemu)=((floor $ xpt * 12700), (floor $ ypt * 12700))
  let cNvPicPr = mknode "p:cNvPicPr" [] $
                 mknode "a:picLocks" [("noGrp","1")
                                     ,("noChangeAspect","1")] ()
  let nvPicPr  = mknode "p:nvPicPr" []
                 [ mknode "p:cNvPr"
                   [("descr", mInfoFilePath mInfo),("id","0"),("name","Picture 1")] ()
                 , cNvPicPr
                 , mknode "p:nvPr" [] ()]
  let blipFill = mknode "p:blipFill" []
                 [ mknode "a:blip" [("r:embed", "rId" ++ (show $ mInfoLocalId mInfo))] ()
                 , mknode "a:stretch" [] $
                   mknode "a:fillRect" [] () ]
  let xfrm =    mknode "a:xfrm" []
                [ mknode "a:off" [("x",show xoff''), ("y",show yoff')] ()
                , mknode "a:ext" [("cx",show xemu)
                                 ,("cy",show yemu)] () ]
  let prstGeom = mknode "a:prstGeom" [("prst","rect")] $
                 mknode "a:avLst" [] ()
  let ln =      mknode "a:ln" [("w","9525")]
                [ mknode "a:noFill" [] ()
                , mknode "a:headEnd" [] ()
                , mknode "a:tailEnd" [] () ]
  let spPr =    mknode "p:spPr" [("bwMode","auto")]
                [xfrm, prstGeom, mknode "a:noFill" [] (), ln]
  return $
    mknode "p:pic" []
      [ nvPicPr
      , blipFill
      , spPr ]

-- Currently hardcoded, until I figure out how to make it dynamic.
blockQuoteSize :: Pixels
blockQuoteSize = 20

noteSize :: Pixels
noteSize = 18

paraElemToElement :: PandocMonad m => ParaElem -> P m Element
paraElemToElement Break = return $ mknode "a:br" [] ()
paraElemToElement (Run rpr s) = do
  let attrs =
        if rPropCode rpr
        then []
        else (case rPropForceSize rpr of
                Just n -> [("sz", (show $ n * 100))]
                Nothing -> []) ++
             (if rPropBold rpr then [("b", "1")] else []) ++
             (if rPropItalics rpr then [("i", "1")] else []) ++
             (case rStrikethrough rpr of
                Just NoStrike     -> [("strike", "noStrike")]
                Just SingleStrike -> [("strike", "sngStrike")]
                Just DoubleStrike -> [("strike", "dblStrike")]
                Nothing -> []) ++
             (case rBaseline rpr of
                Just n -> [("baseline", show n)]
                Nothing -> []) ++
             (case rCap rpr of
                Just NoCapitals -> [("cap", "none")]
                Just SmallCapitals -> [("cap", "small")]
                Just AllCapitals -> [("cap", "all")]
                Nothing -> []) ++
             []
  linkProps <- case rLink rpr of
                 Just link -> do idNum <- registerLink link
                                 return [mknode "a:hlinkClick"
                                          [("r:id", "rId" ++ show idNum)]
                                          ()
                                        ]
                 Nothing -> return []
  let propContents = if rPropCode rpr
                     then [mknode "a:latin" [("typeface", "Courier")] ()]
                     else linkProps
  return $ mknode "a:r" [] [ mknode "a:rPr" attrs propContents
                           , mknode "a:t" [] s
                           ]
paraElemToElement (MathElem mathType texStr) = do
  res <- convertMath writeOMML mathType (unTeXString texStr)
  case res of
    Right r -> return $ mknode "a14:m" [] $ addMathInfo r
    Left (Str s) -> paraElemToElement (Run def s)
    Left _       -> throwError $ PandocShouldNeverHappenError "non-string math fallback"

-- This is a bit of a kludge -- really requires adding an option to
-- TeXMath, but since that's a different package, we'll do this one
-- step at a time.
addMathInfo :: Element -> Element
addMathInfo element =
  let mathspace = Attr { attrKey = (QName "m" Nothing (Just "xmlns"))
                       , attrVal = "http://schemas.openxmlformats.org/officeDocument/2006/math"
                       }
  in add_attr mathspace element

-- We look through the element to see if it contains an a14:m
-- element. If so, we surround it. This is a bit ugly, but it seems
-- more dependable than looking through shapes for math. Plus this is
-- an xml implementation detail, so it seems to make sense to do it at
-- the xml level.
surroundWithMathAlternate :: Element -> Element
surroundWithMathAlternate element =
  case findElement (QName "m" Nothing (Just "a14")) element of
    Just _ ->
      mknode "mc:AlternateContent"
         [("xmlns:mc", "http://schemas.openxmlformats.org/markup-compatibility/2006")
         ] [ mknode "mc:Choice"
             [ ("xmlns:a14", "http://schemas.microsoft.com/office/drawing/2010/main")
             , ("Requires", "a14")] [ element ]
           ]
    Nothing -> element

paragraphToElement :: PandocMonad m => Paragraph -> P m Element
paragraphToElement par = do
  let
    attrs = [("lvl", show $ pPropLevel $ paraProps par)] ++
            (case pPropMarginLeft (paraProps par) of
               Just px -> [("marL", show $ 12700 * px), ("indent", "0")]
               Nothing -> []
            ) ++
            (case pPropAlign (paraProps par) of
               Just AlgnLeft -> [("algn", "l")]
               Just AlgnRight -> [("algn", "r")]
               Just AlgnCenter -> [("algn", "ctr")]
               Nothing -> []
            )
    props = [] ++
            (case pPropBullet $ paraProps par of
               Just Bullet -> []
               Just (AutoNumbering attrs') ->
                 [mknode "a:buAutoNum" [("type", autoNumberingToType attrs')] ()]
               Nothing -> [mknode "a:buNone" [] ()]
            )
  paras <- mapM paraElemToElement (paraElems par)
  return $ mknode "a:p" [] $ [mknode "a:pPr" attrs props] ++ paras

shapeToElement :: PandocMonad m => Element -> Shape -> P m Element
shapeToElement layout (TextBox paras)
  | ns <- elemToNameSpaces layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld
  , Just sp <- getContentShape ns spTree = do
      elements <- mapM paragraphToElement paras
      let txBody = mknode "p:txBody" [] $
                   [mknode "a:bodyPr" [] (), mknode "a:lstStyle" [] ()] ++ elements
          emptySpPr = mknode "p:spPr" [] ()
      return $
        surroundWithMathAlternate $
        replaceNamedChildren ns "p" "txBody" [txBody] $
        replaceNamedChildren ns "p" "spPr" [emptySpPr] $
        sp
  -- XXX: TODO
  | otherwise = return $ mknode "p:sp" [] ()
-- XXX: TODO
shapeToElement layout (Pic fp attr alt) = do
  mInfo <- registerMedia fp alt
  case mInfoExt mInfo of
    Just _ -> makePicElement mInfo attr
    Nothing -> shapeToElement layout $ TextBox [Paragraph def alt]
shapeToElement _ (GraphicFrame tbls _) = do
  elements <- mapM graphicToElement tbls
  return $ mknode "p:graphicFrame" [] $
    [ mknode "p:nvGraphicFramePr" [] $
      [ mknode "p:cNvPr" [("id", "6"), ("name", "Content Placeholder 5")] ()
      , mknode "p:cNvGraphicFramePr" [] $
        [mknode "a:graphicFrameLocks" [("noGrp", "1")] ()]
      , mknode "p:nvPr" [] $
        [mknode "p:ph" [("idx", "1")] ()]
      ]
    , mknode "p:xfrm" [] $
      [ mknode "a:off" [("x", "457200"), ("y", "1600200")] ()
      , mknode "a:ext" [("cx", "8029388"), ("cy", "3644152")] ()
      ]
    ] ++ elements

shapeToElements :: PandocMonad m => Element -> Shape -> P m [Element]
shapeToElements layout shp = do
  case shp of
    (Pic _ _ alt) | (not . null) alt -> do
      element <- shapeToElement layout shp
      caption <- createCaption alt
      return [element, caption]
    (GraphicFrame _ cptn) | (not . null) cptn -> do
      element <- shapeToElement layout shp
      caption <- createCaption cptn
      return [element, caption]
    _ -> do
      element <- shapeToElement layout shp
      return [element]

shapesToElements :: PandocMonad m => Element -> [Shape] -> P m [Element]
shapesToElements layout shps = do
 concat <$> mapM (shapeToElements layout) shps

hardcodedTableMargin :: Integer
hardcodedTableMargin = 36


graphicToElement :: PandocMonad m => Graphic -> P m Element
graphicToElement (Tbl tblPr colWidths hdrCells rows) = do
  let cellToOpenXML paras = do elements <- mapM paragraphToElement paras
                               return $
                                 [mknode "a:txBody" [] $
                                  ([ mknode "a:bodyPr" [] ()
                                   , mknode "a:lstStyle" [] ()]
                                   ++ elements)]
  headers' <- mapM cellToOpenXML hdrCells
  rows' <- mapM (mapM cellToOpenXML) rows
  let borderProps = mknode "a:tcPr" [] ()
  let emptyCell = [mknode "a:p" [] [mknode "a:pPr" [] ()]]
  let mkcell border contents = mknode "a:tc" []
                            $ (if null contents
                               then emptyCell
                               else contents) ++ [ borderProps | border ]
  let mkrow border cells = mknode "a:tr" [("h", "0")] $ map (mkcell border) cells
  -- let textwidth = 14400  -- 5.5 in in twips, 1/20 pt
  -- let fullrow = 14400 -- 100% specified in pct
  -- let rowwidth = fullrow * sum colWidths

  let mkgridcol w = mknode "a:gridCol"
                       [("w", show ((12700 * w) :: Integer))] ()
  let hasHeader = not (all null hdrCells)
  return $ mknode "a:graphic" [] $
    [mknode "a:graphicData" [("uri", "http://schemas.openxmlformats.org/drawingml/2006/table")] $
     [mknode "a:tbl" [] $
      [ mknode "a:tblPr" [ ("firstRow", if tblPrFirstRow tblPr then "1" else "0")
                         , ("bandRow", if tblPrBandRow tblPr then "1" else "0")
                         ] ()
      , mknode "a:tblGrid" [] (if all (==0) colWidths
                               then []
                               else map mkgridcol colWidths)
      ]
      ++ [ mkrow True headers' | hasHeader ] ++ map (mkrow False) rows'
     ]
    ]

getShapeByName :: NameSpaces -> Element -> String -> Maybe Element
getShapeByName ns spTreeElem name
  | isElem ns "p" "spTree" spTreeElem =
  filterChild (\e -> (isElem ns "p" "sp" e) && (shapeHasName ns name e)) spTreeElem
  | otherwise = Nothing

nonBodyTextToElement :: PandocMonad m => Element -> String -> [ParaElem] -> P m Element
nonBodyTextToElement layout shapeName paraElements
  | ns <- elemToNameSpaces layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld
  , Just sp <- getShapeByName ns spTree shapeName = do
      let hdrPara = Paragraph def paraElements
      element <- paragraphToElement hdrPara
      let txBody = mknode "p:txBody" [] $
                   [mknode "a:bodyPr" [] (), mknode "a:lstStyle" [] ()] ++
                   [element]
      return $ replaceNamedChildren ns "p" "txBody" [txBody] sp
  -- XXX: TODO
  | otherwise = return $ mknode "p:sp" [] ()


-- hdrToElement :: Element -> [ParaElem] -> Element
-- hdrToElement layout paraElems
--   | ns <- elemToNameSpaces layout
--   , Just cSld <- findChild (elemName ns "p" "cSld") layout
--   , Just spTree <- findChild (elemName ns "p" "spTree") cSld
--   , Just sp <- getContentTitleShape ns spTree =
--   let hdrPara = Paragraph def paraElems
--       txBody = mknode "p:txBody" [] $
--                [mknode "a:bodyPr" [] (), mknode "a:lstStyle" [] ()] ++
--                [paragraphToElement hdrPara]
--   in
--     replaceNamedChildren ns "p" "txBody" [txBody] sp
--   -- XXX: TODO
--   | otherwise = mknode "p:sp" [] ()
-- -- XXX: TODO
-- hdrToElement _ _ = mknode "p:sp" [] ()

contentToElement :: PandocMonad m => Element -> [ParaElem] -> [Shape] -> P m Element
contentToElement layout hdrShape shapes
  | ns <- elemToNameSpaces layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld = do
      element <- nonBodyTextToElement layout "Title 1" hdrShape
      let hdrShapeElements = if null hdrShape
                             then []
                             else [element]
      contentElements <- shapesToElements layout shapes
      return $
        replaceNamedChildren ns "p" "sp"
        (hdrShapeElements ++ contentElements)
        spTree
contentToElement _ _ _ = return $ mknode "p:sp" [] ()

titleToElement :: PandocMonad m => Element -> [ParaElem] -> P m Element
titleToElement layout titleElems
  | ns <- elemToNameSpaces layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld = do
      element <- nonBodyTextToElement layout "Title 1" titleElems
      let titleShapeElements = if null titleElems
                               then []
                               else [element]
      return $ replaceNamedChildren ns "p" "sp" titleShapeElements spTree
titleToElement _ _ = return $ mknode "p:sp" [] ()

metadataToElement :: PandocMonad m => Element -> [ParaElem] -> [ParaElem] -> [[ParaElem]] -> [ParaElem] -> P m Element
metadataToElement layout titleElems subtitleElems authorsElems dateElems
  | ns <- elemToNameSpaces layout
  , Just cSld <- findChild (elemName ns "p" "cSld") layout
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld = do
      titleShapeElements <- if null titleElems
                            then return []
                            else sequence [nonBodyTextToElement layout "Title 1" titleElems]
      let combinedAuthorElems = intercalate [Break] authorsElems
          subtitleAndAuthorElems = intercalate [Break, Break] [subtitleElems, combinedAuthorElems]
      subtitleShapeElements <- if null subtitleAndAuthorElems
                               then return []
                               else sequence [nonBodyTextToElement layout "Subtitle 2" subtitleAndAuthorElems]
      dateShapeElements <- if null dateElems
                           then return []
                           else sequence [nonBodyTextToElement layout "Date Placeholder 3" dateElems]
      return $ replaceNamedChildren ns "p" "sp"
        (titleShapeElements ++ subtitleShapeElements ++ dateShapeElements)
        spTree
metadataToElement _ _ _ _ _ = return $ mknode "p:sp" [] ()

slideToElement :: PandocMonad m => Slide -> P m Element
slideToElement s@(ContentSlide hdrElems shapes) = do
  layout <- getLayout s
  spTree <- local (\env -> if null hdrElems
                           then env
                           else env{envSlideHasHeader=True}) $
            contentToElement layout hdrElems shapes
  return $ mknode "p:sld"
    [ ("xmlns:a", "http://schemas.openxmlformats.org/drawingml/2006/main"),
      ("xmlns:r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships"),
      ("xmlns:p", "http://schemas.openxmlformats.org/presentationml/2006/main")
    ] [mknode "p:cSld" [] [spTree]]
slideToElement s@(TitleSlide hdrElems) = do
  layout <- getLayout s
  spTree <- titleToElement layout hdrElems
  return $ mknode "p:sld"
    [ ("xmlns:a", "http://schemas.openxmlformats.org/drawingml/2006/main"),
      ("xmlns:r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships"),
      ("xmlns:p", "http://schemas.openxmlformats.org/presentationml/2006/main")
    ] [mknode "p:cSld" [] [spTree]]
slideToElement s@(MetadataSlide titleElems subtitleElems authorElems dateElems) = do
  layout <- getLayout s
  spTree <- metadataToElement layout titleElems subtitleElems authorElems dateElems
  return $ mknode "p:sld"
    [ ("xmlns:a", "http://schemas.openxmlformats.org/drawingml/2006/main"),
      ("xmlns:r", "http://schemas.openxmlformats.org/officeDocument/2006/relationships"),
      ("xmlns:p", "http://schemas.openxmlformats.org/presentationml/2006/main")
    ] [mknode "p:cSld" [] [spTree]]

-----------------------------------------------------------------------

slideToFilePath :: Slide -> Int -> FilePath
slideToFilePath _ idNum = "slide" ++ (show $ idNum) ++ ".xml"

slideToSlideId :: Monad m => Slide -> Int -> P m String
slideToSlideId _ idNum = do
  n <- gets stSlideIdOffset
  return $ "rId" ++ (show $ idNum + n)


data Relationship = Relationship { relId :: Int
                                 , relType :: MimeType
                                 , relTarget :: FilePath
                                 } deriving (Show, Eq)

elementToRel :: Element -> Maybe Relationship
elementToRel element
  | elName element == QName "Relationship" (Just "http://schemas.openxmlformats.org/package/2006/relationships") Nothing =
      do rId <- findAttr (QName "Id" Nothing Nothing) element
         numStr <- stripPrefix "rId" rId
         num <- case reads numStr :: [(Int, String)] of
           (n, _) : _ -> Just n
           []         -> Nothing
         type' <- findAttr (QName "Type" Nothing Nothing) element
         target <- findAttr (QName "Target" Nothing Nothing) element
         return $ Relationship num type' target
  | otherwise = Nothing

slideToPresRel :: Monad m => Slide -> Int -> P m Relationship
slideToPresRel slide idNum = do
  n <- gets stSlideIdOffset
  let rId = idNum + n
      fp = "slides/" ++ slideToFilePath slide idNum
  return $ Relationship { relId = rId
                        , relType = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slide"
                        , relTarget = fp
                        }

getRels :: PandocMonad m => P m [Relationship]
getRels = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  relsElem <- parseXml refArchive distArchive "ppt/_rels/presentation.xml.rels"
  let globalNS = "http://schemas.openxmlformats.org/package/2006/relationships"
  let relElems = findChildren (QName "Relationship" (Just globalNS) Nothing) relsElem
  return $ mapMaybe elementToRel relElems

presentationToRels :: PandocMonad m => Presentation -> P m [Relationship]
presentationToRels (Presentation _ slides) = do
  mySlideRels <- mapM (\(s, n) -> slideToPresRel s n) $ zip slides [1..]
  rels <- getRels
  let relsWithoutSlides = filter (\r -> relType r /= "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slide") rels
  -- We want to make room for the slides in the id space. The slides
  -- will start at Id2 (since Id1 is for the slide master). There are
  -- two slides in the data file, but that might change in the future,
  -- so we will do this:
  --
  -- 1. We look to see what the minimum relWithoutSlide id (greater than 1) is.
  -- 2. We add the difference between this and the number of slides to
  -- all relWithoutSlide rels (unless they're 1)

  let minRelNotOne = case filter (1<) $ map relId relsWithoutSlides of
        [] -> 0                 -- doesn't matter in this case, since
                                -- there will be nothing to map the
                                -- function over
        l  -> minimum l

      modifyRelNum :: Int -> Int
      modifyRelNum 1 = 1
      modifyRelNum n = n - minRelNotOne + 2 + length slides

      relsWithoutSlides' = map (\r -> r{relId = modifyRelNum $ relId r}) relsWithoutSlides

  return $ mySlideRels ++ relsWithoutSlides'

relToElement :: Relationship -> Element
relToElement rel = mknode "Relationship" [ ("Id", "rId" ++ (show $ relId rel))
                                         , ("Type", relType rel)
                                         , ("Target", relTarget rel) ] ()

relsToElement :: [Relationship] -> Element
relsToElement rels = mknode "Relationships"
                     [("xmlns", "http://schemas.openxmlformats.org/package/2006/relationships")]
                     (map relToElement rels)

presentationToRelsEntry :: PandocMonad m => Presentation -> P m Entry
presentationToRelsEntry pres = do
  rels <- presentationToRels pres
  elemToEntry "ppt/_rels/presentation.xml.rels" $ relsToElement rels

elemToEntry :: PandocMonad m => FilePath -> Element -> P m Entry
elemToEntry fp element = do
  epochtime <- (floor . utcTimeToPOSIXSeconds) <$> asks envUTCTime
  return $ toEntry fp epochtime $ renderXml element

slideToEntry :: PandocMonad m => Slide -> Int -> P m Entry
slideToEntry slide idNum = do
  modify $ \st -> st{stCurSlideId = idNum}
  element <- slideToElement slide
  elemToEntry ("ppt/slides/" ++ slideToFilePath slide idNum) element

slideToSlideRelEntry :: PandocMonad m => Slide -> Int -> P m Entry
slideToSlideRelEntry slide idNum = do
  element <- slideToSlideRelElement slide idNum
  elemToEntry ("ppt/slides/_rels/" ++ slideToFilePath slide idNum ++ ".rels") element

linkRelElement :: Int -> (URL, String) -> Element
linkRelElement idNum (url, _) =
  mknode "Relationship" [ ("Id", "rId" ++ show idNum)
                        , ("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink")
                        , ("Target", url)
                        , ("TargetMode", "External")
                        ] ()

linkRelElements :: M.Map Int (URL, String) -> [Element]
linkRelElements mp = map (\(n, lnk) -> linkRelElement n lnk) (M.toList mp)

mediaRelElement :: MediaInfo -> Element
mediaRelElement mInfo =
  let ext = case mInfoExt mInfo of
              Just e -> e
              Nothing -> ""
  in
    mknode "Relationship" [ ("Id", "rId" ++ (show $ mInfoLocalId mInfo))
                          , ("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image")
                          , ("Target", "../media/image" ++ (show $ mInfoGlobalId mInfo) ++ ext)
                          ] ()

slideToSlideRelElement :: PandocMonad m => Slide -> Int -> P m Element
slideToSlideRelElement slide idNum = do
  let target =  case slide of
        (MetadataSlide _ _ _ _) -> "../slideLayouts/slideLayout1.xml"
        (TitleSlide _)        -> "../slideLayouts/slideLayout3.xml"
        (ContentSlide _ _)    -> "../slideLayouts/slideLayout2.xml"

  linkIds <- gets stLinkIds
  mediaIds <- gets stMediaIds

  let linkRels = case M.lookup idNum linkIds of
                   Just mp -> linkRelElements mp
                   Nothing -> []
      mediaRels = case M.lookup idNum mediaIds of
                   Just mInfos -> map mediaRelElement mInfos
                   Nothing -> []

  return $
    mknode "Relationships"
    [("xmlns", "http://schemas.openxmlformats.org/package/2006/relationships")]
    ([mknode "Relationship" [ ("Id", "rId1")
                           , ("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/slideLayout")
                           , ("Target", target)] ()
    ] ++ linkRels ++ mediaRels)

-- slideToSlideRelEntry :: PandocMonad m => Slide -> Int -> P m Entry
-- slideToSlideRelEntry slide idNum = do
--   let fp = "ppt/slides/_rels/slide" ++ (show idNum) ++ ".xml.rels"
--   elemToEntry fp $ slideToSlideRelElement slide

slideToSldIdElement :: PandocMonad m => Slide -> Int -> P m Element
slideToSldIdElement slide idNum = do
  let id' = show $ idNum + 255
  rId <- slideToSlideId slide idNum
  return $ mknode "p:sldId" [("id", id'), ("r:id", rId)] ()

presentationToSldIdLst :: PandocMonad m => Presentation -> P m Element
presentationToSldIdLst (Presentation _ slides) = do
  ids <- mapM (\(s,n) -> slideToSldIdElement s n) (zip slides [1..])
  return $ mknode "p:sldIdLst" [] ids

presentationToPresentationElement :: PandocMonad m => Presentation -> P m Element
presentationToPresentationElement pres = do
  refArchive <- asks envRefArchive
  distArchive <- asks envDistArchive
  element <- parseXml refArchive distArchive "ppt/presentation.xml"
  sldIdLst <- presentationToSldIdLst pres

  let modifySldIdLst :: Content -> Content
      modifySldIdLst (Elem e) = case elName e of
        (QName "sldIdLst" _ _) -> Elem sldIdLst
        _                      -> Elem e
      modifySldIdLst ct = ct

      newContent = map modifySldIdLst $ elContent element

  return $ element{elContent = newContent}

presentationToPresEntry :: PandocMonad m => Presentation -> P m Entry
presentationToPresEntry pres = presentationToPresentationElement pres >>=
  elemToEntry "ppt/presentation.xml"




defaultContentTypeToElem :: DefaultContentType -> Element
defaultContentTypeToElem dct =
  mknode "Default"
  [("Extension", defContentTypesExt dct),
    ("ContentType", defContentTypesType dct)]
  ()

overrideContentTypeToElem :: OverrideContentType -> Element
overrideContentTypeToElem oct =
  mknode "Override"
  [("PartName", overrideContentTypesPart oct),
    ("ContentType", overrideContentTypesType oct)]
  ()

contentTypesToElement :: ContentTypes -> Element
contentTypesToElement ct =
  let ns = "http://schemas.openxmlformats.org/package/2006/content-types"
  in
    mknode "Types" [("xmlns", ns)] $
    (map defaultContentTypeToElem $ contentTypesDefaults ct) ++
    (map overrideContentTypeToElem $ contentTypesOverrides ct)

data DefaultContentType = DefaultContentType
                           { defContentTypesExt :: String
                           , defContentTypesType:: MimeType
                           }
                         deriving (Show, Eq)

data OverrideContentType = OverrideContentType
                           { overrideContentTypesPart :: FilePath
                           , overrideContentTypesType :: MimeType
                           }
                          deriving (Show, Eq)

data ContentTypes = ContentTypes { contentTypesDefaults :: [DefaultContentType]
                                 , contentTypesOverrides :: [OverrideContentType]
                                 }
                    deriving (Show, Eq)

contentTypesToEntry :: PandocMonad m => ContentTypes -> P m Entry
contentTypesToEntry ct = elemToEntry "[Content_Types].xml" $ contentTypesToElement ct

pathToOverride :: FilePath -> Maybe OverrideContentType
pathToOverride fp = OverrideContentType ("/" ++ fp) <$> (getContentType fp)

mediaContentType :: MediaInfo -> Maybe DefaultContentType
mediaContentType mInfo
  | Just ('.' : ext) <- mInfoExt mInfo =
      Just $ DefaultContentType { defContentTypesExt = ext
                                , defContentTypesType =
                                    case mInfoMimeType mInfo of
                                      Just mt -> mt
                                      Nothing -> "application/octet-stream"
                                }
  | otherwise = Nothing

presentationToContentTypes :: PandocMonad m => Presentation -> P m ContentTypes
presentationToContentTypes (Presentation _ slides) = do
  mediaInfos <- (mconcat . M.elems) <$> gets stMediaIds
  let defaults = [ DefaultContentType "xml" "application/xml"
                 , DefaultContentType "rels" "application/vnd.openxmlformats-package.relationships+xml"
                 ]
      mediaDefaults = nub $ mapMaybe mediaContentType mediaInfos
      inheritedOverrides = mapMaybe pathToOverride inheritedFiles
      presOverride = mapMaybe pathToOverride ["ppt/presentation.xml"]
      slideOverrides =
        mapMaybe
        (\(s, n) ->
           pathToOverride $ "ppt/slides/" ++ slideToFilePath s n)
        (zip slides [1..])
      -- propOverride = mapMaybe pathToOverride ["docProps/core.xml"]
  return $ ContentTypes
    (defaults ++ mediaDefaults)
    (inheritedOverrides ++ presOverride ++ slideOverrides)

-- slideToElement :: Element -> Slide -> Element
-- slideToElement layout (ContentSlide _ shapes) =
--   let sps = map (shapeToElement layout) shapes

presML :: String
presML = "application/vnd.openxmlformats-officedocument.presentationml"

noPresML :: String
noPresML = "application/vnd.openxmlformats-officedocument"

getContentType :: FilePath -> Maybe MimeType
getContentType fp
  | fp == "ppt/presentation.xml" = Just $ presML ++ ".presentation.main+xml"
  | fp == "ppt/presProps.xml" = Just $ presML ++ ".presProps+xml"
  | fp == "ppt/viewProps.xml" = Just $ presML ++ ".viewProps+xml"
  | fp == "ppt/tableStyles.xml" = Just $ presML ++ ".tableStyles+xml"
  | fp == "docProps/core.xml" = Just $ "application/vnd.openxmlformats-package.core-properties+xml"
  | fp == "docProps/app.xml" = Just $ noPresML ++ ".extended-properties+xml"
  | "ppt" : "slideMasters" : f : [] <- splitDirectories fp
  , (_, ".xml") <- splitExtension f =
      Just $ presML ++ ".slideMaster+xml"
  | "ppt" : "slides" : f : [] <- splitDirectories fp
  , (_, ".xml") <- splitExtension f =
      Just $ presML ++ ".slide+xml"
  | "ppt" : "notesMasters"  : f : [] <- splitDirectories fp
  , (_, ".xml") <- splitExtension f =
      Just $ presML ++ ".notesMaster+xml"
  | "ppt" : "notesSlides"  : f : [] <- splitDirectories fp
  , (_, ".xml") <- splitExtension f =
      Just $ presML ++ ".notesSlide+xml"
  | "ppt" : "theme" : f : [] <- splitDirectories fp
  , (_, ".xml") <- splitExtension f =
      Just $ noPresML ++ ".theme+xml"
  | "ppt" : "slideLayouts" : _ : [] <- splitDirectories fp=
      Just $ presML ++ ".slideLayout+xml"
  | otherwise = Nothing
