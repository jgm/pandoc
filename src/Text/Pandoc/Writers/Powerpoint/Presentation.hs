{-# LANGUAGE PatternGuards #-}

{-
Copyright (C) 2017-2018 Jesse Rosenthal <jrosenthal@jhu.edu>

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
   Module      : Text.Pandoc.Writers.Powerpoint.Presentation
   Copyright   : Copyright (C) 2017-2018 Jesse Rosenthal
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Definition of Presentation datatype, modeling a MS Powerpoint (pptx)
document, and functions for converting a Pandoc document to
Presentation.
-}

module Text.Pandoc.Writers.Powerpoint.Presentation ( documentToPresentation
                                                   , Presentation(..)
                                                   , DocProps(..)
                                                   , Slide(..)
                                                   , Layout(..)
                                                   , SpeakerNotes(..)
                                                   , SlideId(..)
                                                   , Shape(..)
                                                   , Graphic(..)
                                                   , BulletType(..)
                                                   , Algnment(..)
                                                   , Paragraph(..)
                                                   , ParaElem(..)
                                                   , ParaProps(..)
                                                   , RunProps(..)
                                                   , TableProps(..)
                                                   , Strikethrough(..)
                                                   , Capitals(..)
                                                   , PicProps(..)
                                                   , URL
                                                   , TeXString(..)
                                                   , LinkTarget(..)
                                                   ) where


import Control.Monad.Reader
import Control.Monad.State
import Data.List (intercalate)
import Data.Default
import Text.Pandoc.Definition
import Text.Pandoc.ImageSize
import Text.Pandoc.Slides (getSlideLevel)
import Text.Pandoc.Options
import Text.Pandoc.Logging
import Text.Pandoc.Walk
import Text.Pandoc.Compat.Time (UTCTime)
import qualified Text.Pandoc.Shared as Shared -- so we don't overlap "Element"
import Text.Pandoc.Writers.Shared (metaValueToInlines)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (maybeToList, fromMaybe)
import Text.Pandoc.Highlighting
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Skylighting

data WriterEnv = WriterEnv { envMetadata :: Meta
                           , envRunProps :: RunProps
                           , envParaProps :: ParaProps
                           , envSlideLevel :: Int
                           , envOpts :: WriterOptions
                           , envSlideHasHeader :: Bool
                           , envInList :: Bool
                           , envInNoteSlide :: Bool
                           , envCurSlideId :: SlideId
                           }
                 deriving (Show)

instance Default WriterEnv where
  def = WriterEnv { envMetadata = mempty
                  , envRunProps = def
                  , envParaProps = def
                  , envSlideLevel = 2
                  , envOpts = def
                  , envSlideHasHeader = False
                  , envInList = False
                  , envInNoteSlide = False
                  , envCurSlideId = SlideId "Default"
                  }


data WriterState = WriterState { stNoteIds :: M.Map Int [Block]
                               -- associate anchors with slide id
                               , stAnchorMap :: M.Map String SlideId
                               , stSlideIdSet :: S.Set SlideId
                               , stLog :: [LogMessage]
                               , stSpeakerNotesMap :: M.Map SlideId [[Paragraph]]
                               } deriving (Show, Eq)

instance Default WriterState where
  def = WriterState { stNoteIds = mempty
                    , stAnchorMap = mempty
                    -- we reserve this s
                    , stSlideIdSet = reservedSlideIds
                    , stLog = []
                    , stSpeakerNotesMap = mempty
                    }

metadataSlideId :: SlideId
metadataSlideId = SlideId "Metadata"

tocSlideId :: SlideId
tocSlideId = SlideId "TOC"

endNotesSlideId :: SlideId
endNotesSlideId = SlideId "EndNotes"

reservedSlideIds :: S.Set SlideId
reservedSlideIds = S.fromList [ metadataSlideId
                              , tocSlideId
                              , endNotesSlideId
                              ]

uniqueSlideId' :: Integer -> S.Set SlideId -> String -> SlideId
uniqueSlideId' n idSet s =
  let s' = if n == 0 then s else s ++ "-" ++ show n
  in if SlideId s' `S.member` idSet
     then uniqueSlideId' (n+1) idSet s
     else SlideId s'

uniqueSlideId :: S.Set SlideId -> String -> SlideId
uniqueSlideId = uniqueSlideId' 0

runUniqueSlideId :: String -> Pres SlideId
runUniqueSlideId s = do
  idSet <- gets stSlideIdSet
  let sldId = uniqueSlideId idSet s
  modify $ \st -> st{stSlideIdSet = S.insert sldId idSet}
  return sldId

addLogMessage :: LogMessage -> Pres ()
addLogMessage msg = modify $ \st -> st{stLog = msg : stLog st}

type Pres = ReaderT WriterEnv (State WriterState)

runPres :: WriterEnv -> WriterState -> Pres a -> (a, [LogMessage])
runPres env st p = (pres, reverse $ stLog finalSt)
  where (pres, finalSt) = runState (runReaderT p env) st

-- GHC 7.8 will still complain about concat <$> mapM unless we specify
-- Functor. We can get rid of this when we stop supporting GHC 7.8.
concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

type Pixels = Integer

data Presentation = Presentation DocProps [Slide]
  deriving (Show)

data DocProps = DocProps { dcTitle :: Maybe String
                         , dcSubject :: Maybe String
                         , dcCreator :: Maybe String
                         , dcKeywords :: Maybe [String]
                         , dcCreated :: Maybe UTCTime
                         } deriving (Show, Eq)


data Slide = Slide { slideId :: SlideId
                   , slideLayout :: Layout
                   , slideSpeakerNotes :: Maybe SpeakerNotes
                   } deriving (Show, Eq)

newtype SlideId = SlideId String
  deriving (Show, Eq, Ord)

-- In theory you could have anything on a notes slide but it seems
-- designed mainly for one textbox, so we'll just put in the contents
-- of that textbox, to avoid other shapes that won't work as well.
newtype SpeakerNotes = SpeakerNotes {fromSpeakerNotes :: [Paragraph]}
  deriving (Show, Eq)

data Layout = MetadataSlide { metadataSlideTitle :: [ParaElem]
                            , metadataSlideSubtitle :: [ParaElem]
                            , metadataSlideAuthors :: [[ParaElem]]
                            , metadataSlideDate :: [ParaElem]
                            }
           | TitleSlide { titleSlideHeader :: [ParaElem]}
           | ContentSlide { contentSlideHeader :: [ParaElem]
                          , contentSlideContent :: [Shape]
                          }
           | TwoColumnSlide { twoColumnSlideHeader :: [ParaElem]
                            , twoColumnSlideLeft   :: [Shape]
                            , twoColumnSlideRight  :: [Shape]
                            }
           deriving (Show, Eq)

data Shape = Pic PicProps FilePath [ParaElem]
           | GraphicFrame [Graphic] [ParaElem]
           | TextBox [Paragraph]
  deriving (Show, Eq)

type Cell = [Paragraph]

data TableProps = TableProps { tblPrFirstRow :: Bool
                             , tblPrBandRow :: Bool
                             } deriving (Show, Eq)

data Graphic = Tbl TableProps [Cell] [[Cell]]
  deriving (Show, Eq)


data Paragraph = Paragraph { paraProps :: ParaProps
                           , paraElems  :: [ParaElem]
                           } deriving (Show, Eq)


data BulletType = Bullet
                | AutoNumbering ListAttributes
  deriving (Show, Eq)

data Algnment = AlgnLeft | AlgnRight | AlgnCenter
  deriving (Show, Eq)

data ParaProps = ParaProps { pPropMarginLeft :: Maybe Pixels
                           , pPropMarginRight :: Maybe Pixels
                           , pPropLevel :: Int
                           , pPropBullet :: Maybe BulletType
                           , pPropAlign :: Maybe Algnment
                           , pPropSpaceBefore :: Maybe Pixels
                           } deriving (Show, Eq)

instance Default ParaProps where
  def = ParaProps { pPropMarginLeft = Just 0
                  , pPropMarginRight = Just 0
                  , pPropLevel = 0
                  , pPropBullet = Nothing
                  , pPropAlign = Nothing
                  , pPropSpaceBefore = Nothing
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

data LinkTarget = ExternalTarget (URL, String)
                | InternalTarget SlideId
                deriving (Show, Eq)

data RunProps = RunProps { rPropBold :: Bool
                         , rPropItalics :: Bool
                         , rStrikethrough :: Maybe Strikethrough
                         , rBaseline :: Maybe Int
                         , rCap :: Maybe Capitals
                         , rLink :: Maybe LinkTarget
                         , rPropCode :: Bool
                         , rPropBlockQuote :: Bool
                         , rPropForceSize :: Maybe Pixels
                         , rSolidFill :: Maybe Color
                         -- TODO: Make a full underline data type with
                         -- the different options.
                         , rPropUnderline :: Bool
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
                 , rSolidFill = Nothing
                 , rPropUnderline = False
                 }

data PicProps = PicProps { picPropLink :: Maybe LinkTarget
                         , picWidth    :: Maybe Dimension
                         , picHeight   :: Maybe Dimension
                         } deriving (Show, Eq)

instance Default PicProps where
  def = PicProps { picPropLink = Nothing
                 , picWidth = Nothing
                 , picHeight = Nothing
                 }

--------------------------------------------------

inlinesToParElems :: [Inline] -> Pres [ParaElem]
inlinesToParElems ils = concatMapM inlineToParElems ils

inlineToParElems :: Inline -> Pres [ParaElem]
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
inlineToParElems (Link _ ils (url, title)) =
  local (\r ->r{envRunProps = (envRunProps r){rLink = Just $ ExternalTarget (url, title)}}) $
  inlinesToParElems ils
inlineToParElems (Code _ str) =
  local (\r ->r{envRunProps = (envRunProps r){rPropCode = True}}) $
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
  local (\env -> env{envRunProps = (envRunProps env){rLink = Just $ InternalTarget endNotesSlideId}}) $
    inlineToParElems $ Superscript [Str $ show curNoteId]
inlineToParElems (Span _ ils) = concatMapM inlineToParElems ils
inlineToParElems (RawInline _ _) = return []
inlineToParElems _ = return []

isListType :: Block -> Bool
isListType (OrderedList _ _) = True
isListType (BulletList _) = True
isListType (DefinitionList _) = True
isListType _ = False

registerAnchorId :: String -> Pres ()
registerAnchorId anchor = do
  anchorMap <- gets stAnchorMap
  sldId <- asks envCurSlideId
  unless (null anchor) $
    modify $ \st -> st {stAnchorMap = M.insert anchor sldId anchorMap}

-- Currently hardcoded, until I figure out how to make it dynamic.
blockQuoteSize :: Pixels
blockQuoteSize = 20

noteSize :: Pixels
noteSize = 18

blockToParagraphs :: Block -> Pres [Paragraph]
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
  local (\r -> r{ envParaProps = def{pPropMarginLeft = Just 100}
                , envRunProps = (envRunProps r){rPropCode = True}}) $ do
  mbSty <- writerHighlightStyle <$> asks envOpts
  synMap <- writerSyntaxMap <$> asks envOpts
  case mbSty of
    Just sty ->
      case highlight synMap (formatSourceLines sty) attr str of
        Right pElems -> do pProps <- asks envParaProps
                           return [Paragraph pProps pElems]
        Left _ -> blockToParagraphs $ Para [Str str]
    Nothing -> blockToParagraphs $ Para [Str str]
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
blockToParagraphs (Header _ (ident, _, _) ils) = do
  -- Note that this function only deals with content blocks, so it
  -- will only touch headers that are above the current slide level --
  -- slides at or below the slidelevel will be taken care of by
  -- `blocksToSlide'`. We have the register anchors in both of them.
  registerAnchorId ident
  -- we set the subeader to bold
  parElems <- local (\e->e{envRunProps = (envRunProps e){rPropBold=True}}) $
              inlinesToParElems ils
  -- and give it a bit of space before it.
  return [Paragraph def{pPropSpaceBefore = Just 30} parElems]
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
  let go :: ([Inline], [[Block]]) -> Pres [Paragraph]
      go (ils, blksLst) = do
        term <-blockToParagraphs $ Para [Strong ils]
        -- For now, we'll treat each definition term as a
        -- blockquote. We can extend this further later.
        definition <- concatMapM (blockToParagraphs . BlockQuote) blksLst
        return $ term ++ definition
  concatMapM go entries
blockToParagraphs (Div (_, "notes" : [], _) blks) = do
  sldId <- asks envCurSlideId
  spkNotesMap <- gets stSpeakerNotesMap
  paras <- concatMapM blockToParagraphs blks
  let spkNotesMap' = case M.lookup sldId spkNotesMap of
        Just lst -> M.insert sldId (paras : lst) spkNotesMap
        Nothing  -> M.insert sldId [paras] spkNotesMap
  modify $ \st -> st{stSpeakerNotesMap = spkNotesMap'}
  return []
blockToParagraphs (Div _ blks)  = concatMapM blockToParagraphs blks
blockToParagraphs blk = do
  addLogMessage $ BlockNotRendered blk
  return []

-- Make sure the bullet env gets turned off after the first para.
multiParBullet :: [Block] -> Pres [Paragraph]
multiParBullet [] = return []
multiParBullet (b:bs) = do
  pProps <- asks envParaProps
  p <- blockToParagraphs b
  ps <- local (\env -> env{envParaProps = pProps{pPropBullet = Nothing}}) $
    concatMapM blockToParagraphs bs
  return $ p ++ ps

cellToParagraphs :: Alignment -> TableCell -> Pres [Paragraph]
cellToParagraphs algn tblCell = do
  paras <- mapM blockToParagraphs tblCell
  let alignment = case algn of
        AlignLeft -> Just AlgnLeft
        AlignRight -> Just AlgnRight
        AlignCenter -> Just AlgnCenter
        AlignDefault -> Nothing
      paras' = map (map (\p -> p{paraProps = (paraProps p){pPropAlign = alignment}})) paras
  return $ concat paras'

rowToParagraphs :: [Alignment] -> [TableCell] -> Pres [[Paragraph]]
rowToParagraphs algns tblCells = do
  -- We have to make sure we have the right number of alignments
  let pairs = zip (algns ++ repeat AlignDefault) tblCells
  mapM (uncurry cellToParagraphs) pairs

withAttr :: Attr -> Shape -> Shape
withAttr attr (Pic picPr url caption) =
  let picPr' = picPr { picWidth = dimension Width attr
                     , picHeight = dimension Height attr
                     }
  in
    Pic picPr' url caption
withAttr _ sp = sp

blockToShape :: Block -> Pres Shape
blockToShape (Plain (il:_)) | Image attr ils (url, _) <- il =
      (withAttr attr . Pic def url) <$> inlinesToParElems ils
blockToShape (Para (il:_))  | Image attr ils (url, _) <- il =
      (withAttr attr . Pic def url) <$> inlinesToParElems ils
blockToShape (Plain (il:_)) | Link _ (il':_) target <- il
                            , Image attr ils (url, _) <- il' =
      (withAttr attr . Pic def {picPropLink = Just $ ExternalTarget target} url) <$>
      inlinesToParElems ils
blockToShape (Para (il:_))  | Link _ (il':_) target <- il
                            , Image attr ils (url, _) <- il' =
      (withAttr attr . Pic def{picPropLink = Just $ ExternalTarget target} url) <$>
      inlinesToParElems ils
blockToShape (Table caption algn _ hdrCells rows) = do
  caption' <- inlinesToParElems caption
  hdrCells' <- rowToParagraphs algn hdrCells
  rows' <- mapM (rowToParagraphs algn) rows
  let tblPr = if null hdrCells
              then TableProps { tblPrFirstRow = False
                              , tblPrBandRow = True
                              }
              else TableProps { tblPrFirstRow = True
                              , tblPrBandRow = True
                              }

  return $ GraphicFrame [Tbl tblPr hdrCells' rows'] caption'
blockToShape blk = do paras <- blockToParagraphs blk
                      let paras' = map (\par -> par{paraElems = combineParaElems $ paraElems par}) paras
                      return $ TextBox paras'

combineShapes :: [Shape] -> [Shape]
combineShapes [] = []
combineShapes[s] = [s]
combineShapes (pic@(Pic{}) : ss) = pic : combineShapes ss
combineShapes (TextBox [] : ss) = combineShapes ss
combineShapes (s : TextBox [] : ss) = combineShapes (s : ss)
combineShapes (TextBox (p:ps) : TextBox (p':ps') : ss) =
  combineShapes $ TextBox ((p:ps) ++ (p':ps')) : ss
combineShapes (s:ss) = s : combineShapes ss

blocksToShapes :: [Block] -> Pres [Shape]
blocksToShapes blks = combineShapes <$> mapM blockToShape blks

isImage :: Inline -> Bool
isImage (Image{}) = True
isImage (Link _ (Image _ _ _ : _) _) = True
isImage _ = False

splitBlocks' :: [Block] -> [[Block]] -> [Block] -> Pres [[Block]]
splitBlocks' cur acc [] = return $ acc ++ (if null cur then [] else [cur])
splitBlocks' cur acc (HorizontalRule : blks) =
  splitBlocks' [] (acc ++ (if null cur then [] else [cur])) blks
splitBlocks' cur acc (h@(Header n _ _) : blks) = do
  slideLevel <- asks envSlideLevel
  case compare n slideLevel of
    LT -> splitBlocks' [] (acc ++ (if null cur then [] else [cur]) ++ [[h]]) blks
    EQ -> splitBlocks' [h] (acc ++ (if null cur then [] else [cur])) blks
    GT -> splitBlocks' (cur ++ [h]) acc blks
-- `blockToParagraphs` treats Plain and Para the same, so we can save
-- some code duplication by treating them the same here.
splitBlocks' cur acc (Plain ils : blks) = splitBlocks' cur acc (Para ils : blks)
splitBlocks' cur acc (Para (il:ils) : blks) | isImage il = do
  slideLevel <- asks envSlideLevel
  case cur of
    [(Header n _ _)] | n == slideLevel ->
                            splitBlocks' []
                            (acc ++ [cur ++ [Para [il]]])
                            (if null ils then blks else Para ils : blks)
    _ -> splitBlocks' []
         (acc ++ (if null cur then [] else [cur]) ++ [[Para [il]]])
         (if null ils then blks else Para ils : blks)
splitBlocks' cur acc (tbl@(Table{}) : blks) = do
  slideLevel <- asks envSlideLevel
  case cur of
    [(Header n _ _)] | n == slideLevel ->
                            splitBlocks' [] (acc ++ [cur ++ [tbl]]) blks
    _ ->  splitBlocks' [] (acc ++ (if null cur then [] else [cur]) ++ [[tbl]]) blks
splitBlocks' cur acc (d@(Div (_, classes, _) _): blks) | "columns" `elem` classes =  do
  slideLevel <- asks envSlideLevel
  case cur of
    [(Header n _ _)] | n == slideLevel ->
                            splitBlocks' [] (acc ++ [cur ++ [d]]) blks
    _ ->  splitBlocks' [] (acc ++ (if null cur then [] else [cur]) ++ [[d]]) blks
splitBlocks' cur acc (blk : blks) = splitBlocks' (cur ++ [blk]) acc blks

splitBlocks :: [Block] -> Pres [[Block]]
splitBlocks = splitBlocks' [] []

getSpeakerNotes :: Pres (Maybe SpeakerNotes)
getSpeakerNotes = do
  sldId <- asks envCurSlideId
  spkNtsMap <- gets stSpeakerNotesMap
  return $ (SpeakerNotes . concat . reverse) <$> (M.lookup sldId spkNtsMap)

blocksToSlide' :: Int -> [Block] -> Pres Slide
blocksToSlide' lvl (Header n (ident, _, _) ils : blks)
  | n < lvl = do
      registerAnchorId ident
      sldId <- asks envCurSlideId
      hdr <- inlinesToParElems ils
      return $ Slide sldId TitleSlide {titleSlideHeader = hdr} Nothing
  | n == lvl = do
      registerAnchorId ident
      hdr <- inlinesToParElems ils
      -- Now get the slide without the header, and then add the header
      -- in.
      slide <- blocksToSlide' lvl blks
      let layout = case slideLayout slide of
            ContentSlide _ cont          -> ContentSlide hdr cont
            TwoColumnSlide _ contL contR -> TwoColumnSlide hdr contL contR
            layout'                     -> layout'
      return $ slide{slideLayout = layout}
blocksToSlide' _ (blk : blks)
  | Div (_, classes, _) divBlks <- blk
  , "columns" `elem` classes
  , Div (_, clsL, _) blksL : Div (_, clsR, _) blksR : remaining <- divBlks
  , "column" `elem` clsL, "column" `elem` clsR = do
      unless (null blks)
        (mapM (addLogMessage . BlockNotRendered) blks >> return ())
      unless (null remaining)
        (mapM (addLogMessage . BlockNotRendered) remaining >> return ())
      mbSplitBlksL <- splitBlocks blksL
      mbSplitBlksR <- splitBlocks blksR
      let blksL' = case mbSplitBlksL of
            bs : _ -> bs
            []     -> []
      let blksR' = case mbSplitBlksR of
            bs : _ -> bs
            []     -> []
      shapesL <- blocksToShapes blksL'
      shapesR <- blocksToShapes blksR'
      sldId <- asks envCurSlideId
      return $ Slide
        sldId
        TwoColumnSlide { twoColumnSlideHeader = []
                       , twoColumnSlideLeft = shapesL
                       , twoColumnSlideRight = shapesR
                       }
        Nothing
blocksToSlide' _ (blk : blks) = do
      inNoteSlide <- asks envInNoteSlide
      shapes <- if inNoteSlide
                then forceFontSize noteSize $ blocksToShapes (blk : blks)
                else blocksToShapes (blk : blks)
      sldId <- asks envCurSlideId
      return $
        Slide
        sldId
        ContentSlide { contentSlideHeader = []
                     , contentSlideContent = shapes
                     }
        Nothing
blocksToSlide' _ [] = do
  sldId <- asks envCurSlideId
  return $
    Slide
    sldId
    ContentSlide { contentSlideHeader = []
                 , contentSlideContent = []
                 }
    Nothing

blocksToSlide :: [Block] -> Pres Slide
blocksToSlide blks = do
  slideLevel <- asks envSlideLevel
  sld <- blocksToSlide' slideLevel blks
  spkNotes <- getSpeakerNotes
  return $ sld{slideSpeakerNotes = spkNotes}

makeNoteEntry :: Int -> [Block] -> [Block]
makeNoteEntry n blks =
  let enum = Str (show n ++ ".")
  in
    case blks of
      (Para ils : blks') -> (Para $ enum : Space : ils) : blks'
      _ -> Para [enum] : blks

forceFontSize :: Pixels -> Pres a -> Pres a
forceFontSize px x = do
  rpr <- asks envRunProps
  local (\r -> r {envRunProps = rpr{rPropForceSize = Just px}}) x

-- We leave these as blocks because we will want to include them in
-- the TOC.
makeEndNotesSlideBlocks :: Pres [Block]
makeEndNotesSlideBlocks = do
  noteIds <- gets stNoteIds
  slideLevel <- asks envSlideLevel
  meta <- asks envMetadata
  -- Get identifiers so we can give the notes section a unique ident.
  anchorSet <- M.keysSet <$> gets stAnchorMap
  if M.null noteIds
    then return []
    else do let title = case lookupMeta "notes-title" meta of
                  Just val -> metaValueToInlines val
                  Nothing  -> [Str "Notes"]
                ident = Shared.uniqueIdent title anchorSet
                hdr = Header slideLevel (ident, [], []) title
            blks <- return $
                    concatMap (\(n, bs) -> makeNoteEntry n bs) $
                    M.toList noteIds
            return $ hdr : blks

getMetaSlide :: Pres (Maybe Slide)
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
    else return $
         Just $
         Slide
         metadataSlideId
         MetadataSlide { metadataSlideTitle = title
                       , metadataSlideSubtitle = subtitle
                       , metadataSlideAuthors = authors
                       , metadataSlideDate = date
                       }
         Nothing

-- adapted from the markdown writer
elementToListItem :: Shared.Element -> Pres [Block]
elementToListItem (Shared.Sec lev _nums (ident,_,_) headerText subsecs) = do
  opts <- asks envOpts
  let headerLink = if null ident
                   then walk Shared.deNote headerText
                   else [Link nullAttr (walk Shared.deNote headerText)
                          ('#':ident, "")]
  listContents <- if null subsecs || lev >= writerTOCDepth opts
                  then return []
                  else mapM elementToListItem subsecs
  return [Plain headerLink, BulletList listContents]
elementToListItem (Shared.Blk _) = return []

makeTOCSlide :: [Block] -> Pres Slide
makeTOCSlide blks = local (\env -> env{envCurSlideId = tocSlideId}) $ do
  contents <- BulletList <$> mapM elementToListItem (Shared.hierarchicalize blks)
  meta <- asks envMetadata
  slideLevel <- asks envSlideLevel
  let tocTitle = case lookupMeta "toc-title" meta of
                   Just val -> metaValueToInlines val
                   Nothing  -> [Str "Table of Contents"]
      hdr = Header slideLevel nullAttr tocTitle
  sld <- blocksToSlide [hdr, contents]
  return sld

combineParaElems' :: Maybe ParaElem -> [ParaElem] -> [ParaElem]
combineParaElems' mbPElem [] = maybeToList mbPElem
combineParaElems' Nothing (pElem : pElems) =
  combineParaElems' (Just pElem) pElems
combineParaElems' (Just pElem') (pElem : pElems)
  | Run rPr' s' <- pElem'
  , Run rPr s <- pElem
  , rPr == rPr' =
    combineParaElems' (Just $ Run rPr' $ s' ++ s) pElems
  | otherwise =
    pElem' : combineParaElems' (Just pElem) pElems

combineParaElems :: [ParaElem] -> [ParaElem]
combineParaElems = combineParaElems' Nothing

applyToParagraph :: Monad m => (ParaElem -> m ParaElem) -> Paragraph -> m Paragraph
applyToParagraph f para = do
  paraElems' <- mapM f $ paraElems para
  return $ para {paraElems = paraElems'}

applyToShape :: Monad m => (ParaElem -> m ParaElem) -> Shape -> m Shape
applyToShape f (Pic pPr fp pes) = do
  pes' <- mapM f pes
  return $ Pic pPr fp pes'
applyToShape f (GraphicFrame gfx pes) = do
  pes' <- mapM f pes
  return $ GraphicFrame gfx pes'
applyToShape f (TextBox paras) = do
  paras' <- mapM (applyToParagraph f) paras
  return $ TextBox paras'

applyToLayout :: Monad m => (ParaElem -> m ParaElem) -> Layout -> m Layout
applyToLayout f (MetadataSlide title subtitle authors date) = do
  title' <- mapM f title
  subtitle' <- mapM f subtitle
  authors' <- mapM (mapM f) authors
  date' <- mapM f date
  return $ MetadataSlide title' subtitle' authors' date'
applyToLayout f (TitleSlide title) = do
  title' <- mapM f title
  return $ TitleSlide title'
applyToLayout f (ContentSlide hdr content) = do
  hdr' <- mapM f hdr
  content' <- mapM (applyToShape f) content
  return $ ContentSlide hdr' content'
applyToLayout f (TwoColumnSlide hdr contentL contentR) = do
  hdr' <- mapM f hdr
  contentL' <- mapM (applyToShape f) contentL
  contentR' <- mapM (applyToShape f) contentR
  return $ TwoColumnSlide hdr' contentL' contentR'

applyToSlide :: Monad m => (ParaElem -> m ParaElem) -> Slide -> m Slide
applyToSlide f slide = do
  layout' <- applyToLayout f $ slideLayout slide
  mbNotes' <- case slideSpeakerNotes slide of
                Just (SpeakerNotes notes) -> (Just . SpeakerNotes) <$>
                                             mapM (applyToParagraph f) notes
                Nothing -> return Nothing
  return slide{slideLayout = layout', slideSpeakerNotes = mbNotes'}

replaceAnchor :: ParaElem -> Pres ParaElem
replaceAnchor (Run rProps s)
  | Just (ExternalTarget ('#':anchor, _)) <- rLink rProps = do
      anchorMap <- gets stAnchorMap
      -- If the anchor is not in the anchormap, we just remove the
      -- link.
      let rProps' = case M.lookup anchor anchorMap of
                      Just n  -> rProps{rLink = Just $ InternalTarget n}
                      Nothing -> rProps{rLink = Nothing}
      return $ Run rProps' s
replaceAnchor pe = return pe

blocksToPresentationSlides :: [Block] -> Pres [Slide]
blocksToPresentationSlides blks = do
  opts <- asks envOpts
  metadataslides <- maybeToList <$> getMetaSlide
  -- As far as I can tell, if we want to have a variable-length toc in
  -- the future, we'll have to make it twice. Once to get the length,
  -- and a second time to include the notes slide. We can't make the
  -- notes slide before the body slides because we need to know if
  -- there are notes, and we can't make either before the toc slide,
  -- because we need to know its length to get slide numbers right.
  --
  -- For now, though, since the TOC slide is only length 1, if it
  -- exists, we'll just get the length, and then come back to make the
  -- slide later
  blksLst <- splitBlocks blks
  bodySlideIds <- mapM
                  (\n -> runUniqueSlideId $ "BodySlide" ++ show n)
                  (take (length blksLst) [1..] :: [Integer])
  bodyslides <- mapM
                (\(bs, ident) ->
                    local (\st -> st{envCurSlideId = ident}) (blocksToSlide bs))
                (zip blksLst bodySlideIds)
  endNotesSlideBlocks <- makeEndNotesSlideBlocks
  -- now we come back and make the real toc...
  tocSlides <- if writerTableOfContents opts
               then do toc <- makeTOCSlide $ blks ++ endNotesSlideBlocks
                       return [toc]
               else return []
  -- ... and the notes slide. We test to see if the blocks are empty,
  -- because we don't want to make an empty slide.
  endNotesSlides <- if null endNotesSlideBlocks
                    then return []
                    else do endNotesSlide <- local
                              (\env -> env { envCurSlideId = endNotesSlideId
                                           , envInNoteSlide = True
                                           })
                              (blocksToSlide endNotesSlideBlocks)
                            return [endNotesSlide]

  let slides = metadataslides ++ tocSlides ++ bodyslides ++ endNotesSlides
  mapM (applyToSlide replaceAnchor) slides

metaToDocProps :: Meta -> DocProps
metaToDocProps meta =
  let keywords = case lookupMeta "keywords" meta of
                   Just (MetaList xs) -> Just $ map Shared.stringify xs
                   _                  -> Nothing

      authors = case map Shared.stringify $ docAuthors meta of
                  [] -> Nothing
                  ss -> Just $ intercalate ";" ss
  in
    DocProps{ dcTitle = Shared.stringify <$> lookupMeta "title" meta
            , dcSubject = Shared.stringify <$> lookupMeta "subject" meta
            , dcCreator = authors
            , dcKeywords = keywords
            , dcCreated = Nothing
            }

documentToPresentation :: WriterOptions
                       -> Pandoc
                       -> (Presentation, [LogMessage])
documentToPresentation opts (Pandoc meta blks) =
  let env = def { envOpts = opts
                , envMetadata = meta
                , envSlideLevel = fromMaybe (getSlideLevel blks) (writerSlideLevel opts)
                }
      (presSlides, msgs) = runPres env def $ blocksToPresentationSlides blks
      docProps = metaToDocProps meta
  in
    (Presentation docProps presSlides, msgs)

-- --------------------------------------------------------------

applyTokStyToRunProps :: TokenStyle -> RunProps -> RunProps
applyTokStyToRunProps tokSty rProps =
  rProps{ rSolidFill     = tokenColor tokSty <|> rSolidFill rProps
        , rPropBold      = tokenBold tokSty || rPropBold rProps
        , rPropItalics   = tokenItalic tokSty || rPropItalics rProps
        , rPropUnderline = tokenUnderline tokSty || rPropUnderline rProps
        }

formatToken :: Style -> Token -> ParaElem
formatToken sty (tokType, txt) =
  let rProps = def{rPropCode = True, rSolidFill = defaultColor sty}
      rProps' = case M.lookup tokType (tokenStyles sty) of
        Just tokSty -> applyTokStyToRunProps tokSty rProps
        Nothing     -> rProps
  in
    Run rProps' $ T.unpack txt

formatSourceLine :: Style -> FormatOptions -> SourceLine -> [ParaElem]
formatSourceLine sty _ srcLn = map (formatToken sty) srcLn

formatSourceLines :: Style -> FormatOptions -> [SourceLine] -> [ParaElem]
formatSourceLines sty opts srcLns = intercalate [Break] $
                                    map (formatSourceLine sty opts) srcLns
