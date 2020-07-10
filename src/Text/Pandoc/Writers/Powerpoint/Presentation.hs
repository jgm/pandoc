{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
   Module      : Text.Pandoc.Writers.Powerpoint.Presentation
   Copyright   : Copyright (C) 2017-2020 Jesse Rosenthal
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
                                                   , Pixels
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
import Data.Time (UTCTime)
import qualified Text.Pandoc.Shared as Shared -- so we don't overlap "Element"
import Text.Pandoc.Shared (tshow)
import Text.Pandoc.Writers.Shared (lookupMetaInlines, lookupMetaBlocks
                                 , lookupMetaString, toTableOfContents
                                 , toLegacyTable)
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
                           , envInSpeakerNotes :: Bool
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
                  , envInSpeakerNotes = False
                  }


data WriterState = WriterState { stNoteIds :: M.Map Int [Block]
                               -- associate anchors with slide id
                               , stAnchorMap :: M.Map T.Text SlideId
                               , stSlideIdSet :: S.Set SlideId
                               , stLog :: [LogMessage]
                               , stSpeakerNotes :: SpeakerNotes
                               } deriving (Show, Eq)

instance Default WriterState where
  def = WriterState { stNoteIds = mempty
                    , stAnchorMap = mempty
                    -- we reserve this s
                    , stSlideIdSet = reservedSlideIds
                    , stLog = []
                    , stSpeakerNotes = mempty
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

uniqueSlideId' :: Integer -> S.Set SlideId -> T.Text -> SlideId
uniqueSlideId' n idSet s =
  let s' = if n == 0 then s else s <> "-" <> tshow n
  in if SlideId s' `S.member` idSet
     then uniqueSlideId' (n+1) idSet s
     else SlideId s'

uniqueSlideId :: S.Set SlideId -> T.Text -> SlideId
uniqueSlideId = uniqueSlideId' 0

runUniqueSlideId :: T.Text -> Pres SlideId
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

data DocProps = DocProps { dcTitle :: Maybe T.Text
                         , dcSubject :: Maybe T.Text
                         , dcCreator :: Maybe T.Text
                         , dcKeywords :: Maybe [T.Text]
                         , dcDescription :: Maybe T.Text
                         , cpCategory :: Maybe T.Text
                         , dcCreated :: Maybe UTCTime
                         , customProperties :: Maybe [(T.Text, T.Text)]
                         } deriving (Show, Eq)


data Slide = Slide { slideId :: SlideId
                   , slideLayout :: Layout
                   , slideSpeakerNotes :: SpeakerNotes
                   } deriving (Show, Eq)

newtype SlideId = SlideId T.Text
  deriving (Show, Eq, Ord)

-- In theory you could have anything on a notes slide but it seems
-- designed mainly for one textbox, so we'll just put in the contents
-- of that textbox, to avoid other shapes that won't work as well.
newtype SpeakerNotes = SpeakerNotes {fromSpeakerNotes :: [Paragraph]}
  deriving (Show, Eq, Monoid, Semigroup)

data Layout = MetadataSlide [ParaElem] [ParaElem] [[ParaElem]] [ParaElem]
            --              title      subtitle   authors      date
            | TitleSlide [ParaElem]
            --           heading
            | ContentSlide [ParaElem] [Shape]
            --             heading    content
            | TwoColumnSlide [ParaElem] [Shape] [Shape]
            --               heading    left    right
            deriving (Show, Eq)

data Shape = Pic PicProps FilePath [ParaElem]
           | GraphicFrame [Graphic] [ParaElem]
           | TextBox [Paragraph]
           | RawOOXMLShape T.Text
  deriving (Show, Eq)

type TableCell = [Paragraph]

-- TODO: remove when better handling of new
-- tables is implemented
type SimpleCell = [Block]

data TableProps = TableProps { tblPrFirstRow :: Bool
                             , tblPrBandRow :: Bool
                             } deriving (Show, Eq)

data Graphic = Tbl TableProps [TableCell] [[TableCell]]
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
                           , pPropIndent :: Maybe Pixels
                           } deriving (Show, Eq)

instance Default ParaProps where
  def = ParaProps { pPropMarginLeft = Just 0
                  , pPropMarginRight = Just 0
                  , pPropLevel = 0
                  , pPropBullet = Nothing
                  , pPropAlign = Nothing
                  , pPropSpaceBefore = Nothing
                  , pPropIndent = Just 0
                  }

newtype TeXString = TeXString {unTeXString :: T.Text}
  deriving (Eq, Show)

data ParaElem = Break
              | Run RunProps T.Text
              -- It would be more elegant to have native TeXMath
              -- Expressions here, but this allows us to use
              -- `convertmath` from T.P.Writers.Math. Will perhaps
              -- revisit in the future.
              | MathElem MathType TeXString
              | RawOOXMLParaElem T.Text
              deriving (Show, Eq)

data Strikethrough = NoStrike | SingleStrike | DoubleStrike
  deriving (Show, Eq)

data Capitals = NoCapitals | SmallCapitals | AllCapitals
  deriving (Show, Eq)

type URL = T.Text

data LinkTarget = ExternalTarget (URL, T.Text)
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
inlineToParElems (Underline ils) =
  local (\r -> r{envRunProps = (envRunProps r){rPropUnderline=True}}) $
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
-- We ignore notes if we're in a speaker notes div. Otherwise this
-- would add an entry to the endnotes slide, which would put speaker
-- notes in the public presentation. In the future, we can entertain a
-- way of adding a speakernotes-specific note that would just add
-- paragraphs to the bottom of the notes page.
inlineToParElems (Note blks) = do
  inSpNotes <- asks envInSpeakerNotes
  if inSpNotes
    then return []
    else do
    notes <- gets stNoteIds
    let maxNoteId = case M.keys notes of
          [] -> 0
          lst -> maximum lst
        curNoteId = maxNoteId + 1
    modify $ \st -> st { stNoteIds = M.insert curNoteId blks notes }
    local (\env -> env{envRunProps = (envRunProps env){rLink = Just $ InternalTarget endNotesSlideId}}) $
      inlineToParElems $ Superscript [Str $ tshow curNoteId]
inlineToParElems (Span _ ils) = inlinesToParElems ils
inlineToParElems (Quoted quoteType ils) =
  inlinesToParElems $ [Str open] ++ ils ++ [Str close]
  where (open, close) = case quoteType of
                          SingleQuote -> ("\x2018", "\x2019")
                          DoubleQuote -> ("\x201C", "\x201D")
inlineToParElems il@(RawInline fmt s) =
  case fmt of
    Format "openxml" -> return [RawOOXMLParaElem s]
    _                -> do addLogMessage $ InlineNotRendered il
                           return []
inlineToParElems (Cite _ ils) = inlinesToParElems ils
-- Note: we shouldn't reach this, because images should be handled at
-- the shape level, but should that change in the future, we render
-- the alt text.
inlineToParElems (Image _ alt _) = inlinesToParElems alt



isListType :: Block -> Bool
isListType (OrderedList _ _) = True
isListType (BulletList _) = True
isListType (DefinitionList _) = True
isListType _ = False

registerAnchorId :: T.Text -> Pres ()
registerAnchorId anchor = do
  anchorMap <- gets stAnchorMap
  sldId <- asks envCurSlideId
  unless (T.null anchor) $
    modify $ \st -> st {stAnchorMap = M.insert anchor sldId anchorMap}

-- Currently hardcoded, until I figure out how to make it dynamic.
blockQuoteSize :: Pixels
blockQuoteSize = 20

noteSize :: Pixels
noteSize = 18

blockToParagraphs :: Block -> Pres [Paragraph]
blockToParagraphs (Plain ils) = blockToParagraphs (Para ils)
blockToParagraphs (Para ils) = do
  parElems <- inlinesToParElems ils
  pProps <- asks envParaProps
  return [Paragraph pProps parElems]
blockToParagraphs (LineBlock ilsList) = do
  parElems <- inlinesToParElems $ intercalate [LineBreak] ilsList
  pProps <- asks envParaProps
  return [Paragraph pProps parElems]
-- TODO: work out the attributes
blockToParagraphs (CodeBlock attr str) = do
  pProps <- asks envParaProps
  local (\r -> r{ envParaProps = def{ pPropMarginLeft = Nothing
                                    , pPropBullet = Nothing
                                    , pPropLevel = pPropLevel pProps
                                    , pPropIndent = Just 0
                                    }
                , envRunProps = (envRunProps r){rPropCode = True}}) $ do
    mbSty <- writerHighlightStyle <$> asks envOpts
    synMap <- writerSyntaxMap <$> asks envOpts
    case mbSty of
      Just sty ->
        case highlight synMap (formatSourceLines sty) attr str of
          Right pElems -> do pPropsNew <- asks envParaProps
                             return [Paragraph pPropsNew pElems]
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
  local (\r -> r{ envParaProps = (envParaProps r){ pPropMarginLeft = Just 100
                                                 , pPropIndent = Just 0
                                                 }
                , envRunProps = (envRunProps r){rPropForceSize = Just blockQuoteSize}})$
  concatMapM blockToParagraphs blks
-- TODO: work out the format
blockToParagraphs blk@(RawBlock _ _) = do addLogMessage $ BlockNotRendered blk
                                          return []
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
                                           , pPropIndent = Nothing
                                           }}) $
    concatMapM multiParBullet blksLst
blockToParagraphs (OrderedList listAttr blksLst) = do
  pProps <- asks envParaProps
  let lvl = pPropLevel pProps
  local (\env -> env{ envInList = True
                    , envParaProps = pProps{ pPropLevel = lvl + 1
                                           , pPropBullet = Just (AutoNumbering listAttr)
                                           , pPropMarginLeft = Nothing
                                           , pPropIndent = Nothing
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

cellToParagraphs :: Alignment -> SimpleCell -> Pres [Paragraph]
cellToParagraphs algn tblCell = do
  paras <- mapM blockToParagraphs tblCell
  let alignment = case algn of
        AlignLeft -> Just AlgnLeft
        AlignRight -> Just AlgnRight
        AlignCenter -> Just AlgnCenter
        AlignDefault -> Nothing
      paras' = map (map (\p -> p{paraProps = (paraProps p){pPropAlign = alignment}})) paras
  return $ concat paras'

rowToParagraphs :: [Alignment] -> [SimpleCell] -> Pres [[Paragraph]]
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
blockToShape (Plain ils) = blockToShape (Para ils)
blockToShape (Para (il:_))  | Image attr ils (url, _) <- il =
      (withAttr attr . Pic def (T.unpack url)) <$> inlinesToParElems ils
blockToShape (Para (il:_))  | Link _ (il':_) target <- il
                            , Image attr ils (url, _) <- il' =
      (withAttr attr . Pic def{picPropLink = Just $ ExternalTarget target} (T.unpack url))
      <$> inlinesToParElems ils
blockToShape (Table _ blkCapt specs thead tbody tfoot) = do
  let (caption, algn, _, hdrCells, rows) = toLegacyTable blkCapt specs thead tbody tfoot
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
-- If the format isn't openxml, we fall through to blockToPargraphs
blockToShape (RawBlock (Format "openxml") str) = return $ RawOOXMLShape str
blockToShape blk = do paras <- blockToParagraphs blk
                      let paras' = map (\par -> par{paraElems = combineParaElems $ paraElems par}) paras
                      return $ TextBox paras'

combineShapes :: [Shape] -> [Shape]
combineShapes [] = []
combineShapes (pic@Pic{} : ss) = pic : combineShapes ss
combineShapes (TextBox [] : ss) = combineShapes ss
combineShapes (s : TextBox [] : ss) = combineShapes (s : ss)
combineShapes (TextBox (p:ps) : TextBox (p':ps') : ss) =
  combineShapes $ TextBox ((p:ps) ++ (p':ps')) : ss
combineShapes (s:ss) = s : combineShapes ss

isNotesDiv :: Block -> Bool
isNotesDiv (Div (_, ["notes"], _) _) = True
isNotesDiv _ = False

blocksToShapes :: [Block] -> Pres [Shape]
blocksToShapes blks = combineShapes <$> mapM blockToShape blks

isImage :: Inline -> Bool
isImage Image{} = True
isImage (Link _ (Image{} : _) _) = True
isImage _ = False

splitBlocks' :: [Block] -> [[Block]] -> [Block] -> Pres [[Block]]
splitBlocks' cur acc [] = return $ acc ++ ([cur | not (null cur)])
splitBlocks' cur acc (HorizontalRule : blks) =
  splitBlocks' [] (acc ++ ([cur | not (null cur)])) blks
splitBlocks' cur acc (h@(Header n _ _) : blks) = do
  slideLevel <- asks envSlideLevel
  let (nts, blks') = span isNotesDiv blks
  case compare n slideLevel of
    LT -> splitBlocks' [] (acc ++ ([cur | not (null cur)]) ++ [h : nts]) blks'
    EQ -> splitBlocks' (h:nts) (acc ++ ([cur | not (null cur)])) blks'
    GT -> splitBlocks' (cur ++ (h:nts)) acc blks'
-- `blockToParagraphs` treats Plain and Para the same, so we can save
-- some code duplication by treating them the same here.
splitBlocks' cur acc (Plain ils : blks) = splitBlocks' cur acc (Para ils : blks)
splitBlocks' cur acc (Para (il:ils) : blks) | isImage il = do
  slideLevel <- asks envSlideLevel
  let (nts, blks') = if null ils
                     then span isNotesDiv blks
                     else ([], blks)
  case cur of
    [Header n _ _] | n == slideLevel ->
                            splitBlocks' []
                            (acc ++ [cur ++ [Para [il]] ++ nts])
                            (if null ils then blks' else Para ils : blks')
    _ -> splitBlocks' []
         (acc ++ ([cur | not (null cur)]) ++ [Para [il] : nts])
         (if null ils then blks' else Para ils : blks')
splitBlocks' cur acc (tbl@Table{} : blks) = do
  slideLevel <- asks envSlideLevel
  let (nts, blks') = span isNotesDiv blks
  case cur of
    [Header n _ _] | n == slideLevel ->
                            splitBlocks' [] (acc ++ [cur ++ [tbl] ++ nts]) blks'
    _ ->  splitBlocks' [] (acc ++ ([cur | not (null cur)]) ++ [tbl : nts]) blks'
splitBlocks' cur acc (d@(Div (_, classes, _) _): blks) | "columns" `elem` classes =  do
  slideLevel <- asks envSlideLevel
  let (nts, blks') = span isNotesDiv blks
  case cur of
    [Header n _ _] | n == slideLevel ->
                            splitBlocks' [] (acc ++ [cur ++ [d] ++ nts]) blks'
    _ ->  splitBlocks' [] (acc ++ ([cur | not (null cur)]) ++ [d : nts]) blks'
splitBlocks' cur acc (blk : blks) = splitBlocks' (cur ++ [blk]) acc blks

splitBlocks :: [Block] -> Pres [[Block]]
splitBlocks = splitBlocks' [] []

blocksToSlide' :: Int -> [Block] -> SpeakerNotes -> Pres Slide
blocksToSlide' lvl (Header n (ident, _, _) ils : blks) spkNotes
  | n < lvl = do
      registerAnchorId ident
      sldId <- asks envCurSlideId
      hdr <- inlinesToParElems ils
      return $ Slide sldId (TitleSlide hdr) spkNotes
  | n == lvl = do
      registerAnchorId ident
      hdr <- inlinesToParElems ils
      -- Now get the slide without the header, and then add the header
      -- in.
      slide <- blocksToSlide' lvl blks spkNotes
      let layout = case slideLayout slide of
            ContentSlide _ cont          -> ContentSlide hdr cont
            TwoColumnSlide _ contL contR -> TwoColumnSlide hdr contL contR
            layout'                     -> layout'
      return $ slide{slideLayout = layout}
blocksToSlide' _ (blk : blks) spkNotes
  | Div (_, classes, _) divBlks <- blk
  , "columns" `elem` classes
  , Div (_, clsL, _) blksL : Div (_, clsR, _) blksR : remaining <- divBlks
  , "column" `elem` clsL, "column" `elem` clsR = do
      mapM_ (addLogMessage . BlockNotRendered) (blks ++ remaining)
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
        (TwoColumnSlide [] shapesL shapesR)
        spkNotes
blocksToSlide' _ (blk : blks) spkNotes = do
      inNoteSlide <- asks envInNoteSlide
      shapes <- if inNoteSlide
                then forceFontSize noteSize $ blocksToShapes (blk : blks)
                else blocksToShapes (blk : blks)
      sldId <- asks envCurSlideId
      return $
        Slide
        sldId
        (ContentSlide [] shapes)
        spkNotes
blocksToSlide' _ [] spkNotes = do
  sldId <- asks envCurSlideId
  return $
    Slide
    sldId
    (ContentSlide [] [])
    spkNotes

blockToSpeakerNotes :: Block -> Pres SpeakerNotes
blockToSpeakerNotes (Div (_, ["notes"], _) blks) =
  local (\env -> env{envInSpeakerNotes=True}) $
  SpeakerNotes <$> concatMapM blockToParagraphs blks
blockToSpeakerNotes _ = return mempty

handleSpeakerNotes :: Block -> Pres ()
handleSpeakerNotes blk = do
  spNotes <- blockToSpeakerNotes blk
  modify $ \st -> st{stSpeakerNotes = stSpeakerNotes st <> spNotes}

handleAndFilterSpeakerNotes' :: [Block] -> Pres [Block]
handleAndFilterSpeakerNotes' blks = do
  mapM_ handleSpeakerNotes blks
  return $ filter (not . isNotesDiv) blks

handleAndFilterSpeakerNotes :: [Block] -> Pres ([Block], SpeakerNotes)
handleAndFilterSpeakerNotes blks = do
  modify $ \st -> st{stSpeakerNotes = mempty}
  blks' <- walkM handleAndFilterSpeakerNotes' blks
  spkNotes <- gets stSpeakerNotes
  return (blks', spkNotes)

blocksToSlide :: [Block] -> Pres Slide
blocksToSlide blks = do
  (blks', spkNotes) <- handleAndFilterSpeakerNotes blks
  slideLevel <- asks envSlideLevel
  blocksToSlide' slideLevel blks' spkNotes

makeNoteEntry :: (Int, [Block]) -> [Block]
makeNoteEntry (n, blks) =
  let enum = Str (tshow n <> ".")
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
  exts <- writerExtensions <$> asks envOpts
  meta <- asks envMetadata
  -- Get identifiers so we can give the notes section a unique ident.
  anchorSet <- M.keysSet <$> gets stAnchorMap
  if M.null noteIds
    then return []
    else let title = case lookupMetaInlines "notes-title" meta of
                       [] -> [Str "Notes"]
                       ls -> ls
             ident = Shared.uniqueIdent exts title anchorSet
             hdr = Header slideLevel (ident, [], []) title
             blks = concatMap makeNoteEntry $
                    M.toList noteIds
         in return $ hdr : blks

getMetaSlide :: Pres (Maybe Slide)
getMetaSlide  = do
  meta <- asks envMetadata
  title <- inlinesToParElems $ docTitle meta
  subtitle <- inlinesToParElems $ lookupMetaInlines "subtitle" meta
  authors <- mapM inlinesToParElems $ docAuthors meta
  date <- inlinesToParElems $ docDate meta
  if null title && null subtitle && null authors && null date
    then return Nothing
    else return $
         Just $
         Slide
         metadataSlideId
         (MetadataSlide title subtitle authors date)
         mempty

addSpeakerNotesToMetaSlide :: Slide -> [Block] -> Pres (Slide, [Block])
addSpeakerNotesToMetaSlide (Slide sldId layout@(MetadataSlide{}) spkNotes) blks =
  do let (ntsBlks, blks') = span isNotesDiv blks
     spkNotes' <- mconcat <$> mapM blockToSpeakerNotes ntsBlks
     return (Slide sldId layout (spkNotes <> spkNotes'), blks')
addSpeakerNotesToMetaSlide sld blks = return (sld, blks)

makeTOCSlide :: [Block] -> Pres Slide
makeTOCSlide blks = local (\env -> env{envCurSlideId = tocSlideId}) $ do
  opts <- asks envOpts
  let contents = toTableOfContents opts blks
  meta <- asks envMetadata
  slideLevel <- asks envSlideLevel
  let tocTitle = case lookupMetaInlines "toc-title" meta of
                   [] -> [Str "Table of Contents"]
                   ls -> ls
      hdr = Header slideLevel nullAttr tocTitle
  blocksToSlide [hdr, contents]

combineParaElems' :: Maybe ParaElem -> [ParaElem] -> [ParaElem]
combineParaElems' mbPElem [] = maybeToList mbPElem
combineParaElems' Nothing (pElem : pElems) =
  combineParaElems' (Just pElem) pElems
combineParaElems' (Just pElem') (pElem : pElems)
  | Run rPr' s' <- pElem'
  , Run rPr s <- pElem
  , rPr == rPr' =
    combineParaElems' (Just $ Run rPr' $ s' <> s) pElems
  | otherwise =
    pElem' : combineParaElems' (Just pElem) pElems

combineParaElems :: [ParaElem] -> [ParaElem]
combineParaElems = combineParaElems' Nothing

applyToParagraph :: Monad m => (ParaElem -> m ParaElem) -> Paragraph -> m Paragraph
applyToParagraph f para = do
  paraElems' <- mapM f $ paraElems para
  return $ para {paraElems = paraElems'}

applyToShape :: Monad m => (ParaElem -> m ParaElem) -> Shape -> m Shape
applyToShape f (Pic pPr fp pes) = Pic pPr fp <$> mapM f pes
applyToShape f (GraphicFrame gfx pes) = GraphicFrame gfx <$> mapM f pes
applyToShape f (TextBox paras) = TextBox <$> mapM (applyToParagraph f) paras
applyToShape _ (RawOOXMLShape str) = return $ RawOOXMLShape str

applyToLayout :: Monad m => (ParaElem -> m ParaElem) -> Layout -> m Layout
applyToLayout f (MetadataSlide title subtitle authors date) = do
  title' <- mapM f title
  subtitle' <- mapM f subtitle
  authors' <- mapM (mapM f) authors
  date' <- mapM f date
  return $ MetadataSlide title' subtitle' authors' date'
applyToLayout f (TitleSlide title) = TitleSlide <$> mapM f title
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
  let paras = fromSpeakerNotes $ slideSpeakerNotes slide
  notes' <- SpeakerNotes <$> mapM (applyToParagraph f) paras
  return slide{slideLayout = layout', slideSpeakerNotes = notes'}

replaceAnchor :: ParaElem -> Pres ParaElem
replaceAnchor (Run rProps s)
  | Just (ExternalTarget (T.uncons -> Just ('#', anchor), _)) <- rLink rProps
  = do
      anchorMap <- gets stAnchorMap
      -- If the anchor is not in the anchormap, we just remove the
      -- link.
      let rProps' = case M.lookup anchor anchorMap of
                      Just n  -> rProps{rLink = Just $ InternalTarget n}
                      Nothing -> rProps{rLink = Nothing}
      return $ Run rProps' s
replaceAnchor pe = return pe

emptyParaElem :: ParaElem -> Bool
emptyParaElem (Run _ s) =
  T.null $ Shared.trim s
emptyParaElem (MathElem _ ts) =
  T.null $ Shared.trim $ unTeXString ts
emptyParaElem _ = False

emptyParagraph :: Paragraph -> Bool
emptyParagraph para = all emptyParaElem $ paraElems para


emptyShape :: Shape -> Bool
emptyShape (TextBox paras) = all emptyParagraph paras
emptyShape _ = False

emptyLayout :: Layout -> Bool
emptyLayout layout = case layout of
  MetadataSlide title subtitle authors date ->
    all emptyParaElem title &&
    all emptyParaElem subtitle &&
    all (all emptyParaElem) authors &&
    all emptyParaElem date
  TitleSlide hdr -> all emptyParaElem hdr
  ContentSlide hdr shapes ->
    all emptyParaElem hdr &&
    all emptyShape shapes
  TwoColumnSlide hdr shapes1 shapes2 ->
    all emptyParaElem hdr &&
    all emptyShape shapes1 &&
    all emptyShape shapes2

emptySlide :: Slide -> Bool
emptySlide (Slide _ layout notes) = (notes == mempty) && emptyLayout layout

blocksToPresentationSlides :: [Block] -> Pres [Slide]
blocksToPresentationSlides blks = do
  opts <- asks envOpts
  mbMetadataSlide <- getMetaSlide
  -- if the metadata slide exists, we try to add any speakerNotes
  -- which immediately follow it. We also convert from maybe to a
  -- list, so that it will be able to add together more easily with
  -- the other lists of slides.
  (metadataslides, blks') <- case mbMetadataSlide of
                                 Just sld ->
                                   do (s, bs) <- addSpeakerNotesToMetaSlide sld blks
                                      return ([s], bs)
                                 Nothing -> return ([], blks)
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
  blksLst <- splitBlocks blks'
  bodySlideIds <- mapM
                  (\n -> runUniqueSlideId $ "BodySlide" <> tshow n)
                  (take (length blksLst) [1..] :: [Integer])
  bodyslides <- mapM
                (\(bs, ident) ->
                    local (\st -> st{envCurSlideId = ident}) (blocksToSlide bs))
                (zip blksLst bodySlideIds)
  endNotesSlideBlocks <- makeEndNotesSlideBlocks
  -- now we come back and make the real toc...
  tocSlides <- if writerTableOfContents opts
               then do toc <- makeTOCSlide $ blks' ++ endNotesSlideBlocks
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
      slides' = filter (not . emptySlide) slides
  mapM (applyToSlide replaceAnchor) slides'

metaToDocProps :: Meta -> DocProps
metaToDocProps meta =
  let keywords = case lookupMeta "keywords" meta of
                   Just (MetaList xs) -> Just $ map Shared.stringify xs
                   _                  -> Nothing

      authors = case map Shared.stringify $ docAuthors meta of
                  [] -> Nothing
                  ss -> Just $ T.intercalate "; " ss

      description = case map Shared.stringify $ lookupMetaBlocks "description" meta of
                  [] -> Nothing
                  ss -> Just $ T.intercalate "_x000d_\n" ss

      customProperties' = case [(k, lookupMetaString k meta) | k <- M.keys (unMeta meta)
                               , k `notElem` ["title", "author", "keywords", "description"
                                             , "subject","lang","category"]] of
                  [] -> Nothing
                  ss -> Just ss
  in
    DocProps{ dcTitle = Shared.stringify <$> lookupMeta "title" meta
            , dcSubject = Shared.stringify <$> lookupMeta "subject" meta
            , dcCreator = authors
            , dcKeywords = keywords
            , dcDescription = description
            , cpCategory = Shared.stringify <$> lookupMeta "category" meta
            , dcCreated = Nothing
            , customProperties = customProperties'
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
    Run rProps' txt

formatSourceLine :: Style -> FormatOptions -> SourceLine -> [ParaElem]
formatSourceLine sty _ srcLn = map (formatToken sty) srcLn

formatSourceLines :: Style -> FormatOptions -> [SourceLine] -> [ParaElem]
formatSourceLines sty opts srcLns = intercalate [Break] $
                                    map (formatSourceLine sty opts) srcLns
