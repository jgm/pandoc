{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ViewPatterns      #-}
{- |
   Module      : Text.Pandoc.Readers.Docx
   Copyright   : Copyright (C) 2014-2020 Jesse Rosenthal
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Conversion of Docx type (defined in Text.Pandoc.Readers.Docx.Parse)
to 'Pandoc' document.  -}

{-
Current state of implementation of Docx entities ([x] means
implemented, [-] means partially implemented):

* Blocks

  - [X] Para
  - [X] CodeBlock (styled with `SourceCode`)
  - [X] BlockQuote (styled with `Quote`, `BlockQuote`, or, optionally,
        indented)
  - [X] OrderedList
  - [X] BulletList
  - [X] DefinitionList (styled with adjacent `DefinitionTerm` and `Definition`)
  - [X] Header (styled with `Heading#`)
  - [ ] HorizontalRule
  - [-] Table (column widths and alignments not yet implemented)

* Inlines

  - [X] Str
  - [X] Emph
  - [X] Strong
  - [X] Strikeout
  - [X] Superscript
  - [X] Subscript
  - [X] SmallCaps
  - [-] Underline (was previously converted to Emph)
  - [ ] Quoted
  - [ ] Cite
  - [X] Code (styled with `VerbatimChar`)
  - [X] Space
  - [X] LineBreak (these are invisible in Word: entered with Shift-Return)
  - [X] Math
  - [X] Link (links to an arbitrary bookmark create a span with the target as
        id and "anchor" class)
  - [X] Image
  - [X] Note (Footnotes and Endnotes are silently combined.)
-}

module Text.Pandoc.Readers.Docx
       ( readDocx
       ) where

import Codec.Archive.Zip
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor (bimap, first)
import qualified Data.ByteString.Lazy as B
import Data.Default (Default)
import Data.List (delete, intersect, foldl')
import Data.Char (isSpace)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe (catMaybes, isJust, fromMaybe)
import Data.Sequence (ViewL (..), viewl)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Text.Pandoc.Builder as Pandoc
import Text.Pandoc.MediaBag (MediaBag)
import Text.Pandoc.Options
import Text.Pandoc.Readers.Docx.Combine
import Text.Pandoc.Readers.Docx.Lists
import Text.Pandoc.Readers.Docx.Parse as Docx
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.TeXMath (writeTeX)
import Control.Monad.Except (throwError)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import qualified Text.Pandoc.Class.PandocMonad as P
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Data.List.NonEmpty (nonEmpty)

readDocx :: PandocMonad m
         => ReaderOptions
         -> B.ByteString
         -> m Pandoc
readDocx opts bytes =
  case toArchiveOrFail bytes of
    Right archive ->
      case archiveToDocxWithWarnings archive of
        Right (docx, parserWarnings) -> do
          mapM_ (P.report . DocxParserWarning) parserWarnings
          (meta, blks) <- docxToOutput opts docx
          return $ Pandoc meta blks
        Left docxerr -> throwError $ PandocSomeError $
                         "couldn't parse docx file: " <> T.pack (show docxerr)
    Left err -> throwError $ PandocSomeError $
                  "couldn't unpack docx container: " <> T.pack err

data DState = DState { docxAnchorMap :: M.Map T.Text T.Text
                     , docxAnchorSet :: Set.Set T.Text
                     , docxImmedPrevAnchor :: Maybe T.Text
                     , docxMediaBag  :: MediaBag
                     , docxDropCap   :: Inlines
                     -- keep track of (numId, lvl) values for
                     -- restarting
                     , docxListState :: M.Map (T.Text, T.Text) Integer
                     , docxPrevPara  :: Inlines
                     , docxTableCaptions :: [Blocks]
                     }

instance Default DState where
  def = DState { docxAnchorMap = M.empty
               , docxAnchorSet = mempty
               , docxImmedPrevAnchor = Nothing
               , docxMediaBag  = mempty
               , docxDropCap   = mempty
               , docxListState = M.empty
               , docxPrevPara  = mempty
               , docxTableCaptions = []
               }

data DEnv = DEnv { docxOptions       :: ReaderOptions
                 , docxInHeaderBlock :: Bool
                 , docxInBidi        :: Bool
                 }

instance Default DEnv where
  def = DEnv def False False

type DocxContext m = ReaderT DEnv (StateT DState m)

evalDocxContext :: PandocMonad m => DocxContext m a -> DEnv -> DState -> m a
evalDocxContext ctx env st = flip evalStateT st $ runReaderT ctx env

-- This is empty, but we put it in for future-proofing.
spansToKeep :: [CharStyleName]
spansToKeep = []

divsToKeep :: [ParaStyleName]
divsToKeep = ["Definition", "Definition Term"]

metaStyles :: M.Map ParaStyleName T.Text
metaStyles = M.fromList [ ("Title", "title")
                        , ("Subtitle", "subtitle")
                        , ("Author", "author")
                        , ("Date", "date")
                        , ("Abstract", "abstract")]

sepBodyParts :: [BodyPart] -> ([BodyPart], [BodyPart])
sepBodyParts = span (\bp -> isMetaPar bp || isEmptyPar bp)

isMetaPar :: BodyPart -> Bool
isMetaPar (Paragraph pPr _) =
  not $ null $ intersect (getStyleNames $ pStyle pPr) (M.keys metaStyles)
isMetaPar _ = False

isEmptyPar :: BodyPart -> Bool
isEmptyPar (Paragraph _ parParts) =
  all isEmptyParPart parParts
  where
    isEmptyParPart (PlainRun (Run _ runElems)) = all isEmptyElem runElems
    isEmptyParPart _                           = False
    isEmptyElem (TextRun s) = trim s == ""
    isEmptyElem _           = True
isEmptyPar _ = False

bodyPartsToMeta' :: PandocMonad m => [BodyPart] -> DocxContext m (M.Map T.Text MetaValue)
bodyPartsToMeta' [] = return M.empty
bodyPartsToMeta' (bp : bps)
  | (Paragraph pPr parParts) <- bp
  , (c : _)<- getStyleNames (pStyle pPr) `intersect` M.keys metaStyles
  , (Just metaField) <- M.lookup c metaStyles = do
    inlines <- smushInlines <$> mapM parPartToInlines parParts
    remaining <- bodyPartsToMeta' bps
    let
      f (MetaInlines ils) (MetaInlines ils') = MetaBlocks [Para ils, Para ils']
      f (MetaInlines ils) (MetaBlocks blks) = MetaBlocks (Para ils : blks)
      f m (MetaList mv) = MetaList (m : mv)
      f m n             = MetaList [m, n]
    return $ M.insertWith f metaField (MetaInlines (toList inlines)) remaining
bodyPartsToMeta' (_ : bps) = bodyPartsToMeta' bps

bodyPartsToMeta :: PandocMonad m => [BodyPart] -> DocxContext m Meta
bodyPartsToMeta bps = do
  mp <- bodyPartsToMeta' bps
  let mp' =
        case M.lookup "author" mp of
          Just mv -> M.insert "author" (fixAuthors mv) mp
          Nothing -> mp
  return $ Meta mp'

fixAuthors :: MetaValue -> MetaValue
fixAuthors (MetaBlocks blks) = MetaList [MetaInlines ils | Para ils <- blks]
fixAuthors mv = mv

isInheritedFromStyles :: (Eq (StyleName s), HasStyleName s, HasParentStyle s) => [StyleName s] -> s -> Bool
isInheritedFromStyles names sty
  | getStyleName sty `elem` names = True
  | Just psty <- getParentStyle sty = isInheritedFromStyles names psty
  | otherwise = False

hasStylesInheritedFrom :: [ParaStyleName] -> ParagraphStyle -> Bool
hasStylesInheritedFrom ns s = any (isInheritedFromStyles ns) $ pStyle s

removeStyleNamed :: ParaStyleName -> ParagraphStyle -> ParagraphStyle
removeStyleNamed sn ps = ps{pStyle = filter (\psd -> getStyleName psd /= sn) $ pStyle ps}

isCodeCharStyle :: CharStyle -> Bool
isCodeCharStyle = isInheritedFromStyles ["Verbatim Char"]

isCodeDiv :: ParagraphStyle -> Bool
isCodeDiv = hasStylesInheritedFrom ["Source Code"]

isBlockQuote :: ParStyle -> Bool
isBlockQuote =
  isInheritedFromStyles [
    "Quote", "Block Text", "Block Quote", "Block Quotation"
    ]

runElemToInlines :: RunElem -> Inlines
runElemToInlines (TextRun s)   = text s
runElemToInlines LnBrk         = linebreak
runElemToInlines Tab           = space
runElemToInlines SoftHyphen    = text "\xad"
runElemToInlines NoBreakHyphen = text "\x2011"

runElemToText :: RunElem -> T.Text
runElemToText (TextRun s)   = s
runElemToText LnBrk         = T.singleton '\n'
runElemToText Tab           = T.singleton '\t'
runElemToText SoftHyphen    = T.singleton '\xad'
runElemToText NoBreakHyphen = T.singleton '\x2011'

runToText :: Run -> T.Text
runToText (Run _ runElems) = T.concat $ map runElemToText runElems
runToText _                = ""

parPartToText :: ParPart -> T.Text
parPartToText (PlainRun run)             = runToText run
parPartToText (InternalHyperLink _ runs) = T.concat $ map runToText runs
parPartToText (ExternalHyperLink _ runs) = T.concat $ map runToText runs
parPartToText _                          = ""

blacklistedCharStyles :: [CharStyleName]
blacklistedCharStyles = ["Hyperlink"]

resolveDependentRunStyle :: PandocMonad m => RunStyle -> DocxContext m RunStyle
resolveDependentRunStyle rPr
  | Just s  <- rParentStyle rPr
  , getStyleName s `notElem` blacklistedCharStyles = do
      opts <- asks docxOptions
      if isEnabled Ext_styles opts
        then return rPr
        else leftBiasedMergeRunStyle rPr <$> resolveDependentRunStyle (cStyleData s)
  | otherwise = return rPr

runStyleToTransform :: PandocMonad m => RunStyle -> DocxContext m (Inlines -> Inlines)
runStyleToTransform rPr' = do
  opts <- asks docxOptions
  inBidi <- asks docxInBidi
  let styles = isEnabled Ext_styles opts
      ctl = (Just True == isRTL rPr') || (Just True == isForceCTL rPr')
      italic rPr | ctl = isItalicCTL rPr
                 | otherwise = isItalic rPr
      bold rPr | ctl = isBoldCTL rPr
               | otherwise = isBold rPr
      go rPr
        | Just sn <- getStyleName <$> rParentStyle rPr
        , sn `elem` spansToKeep =
            spanWith ("", [normalizeToClassName sn], [])
            . go rPr{rParentStyle = Nothing}
        | styles, Just s <- rParentStyle rPr =
             spanWith (extraAttr s) . go rPr{rParentStyle = Nothing}
        | Just True <- italic rPr =
            emph . go rPr{isItalic = Nothing, isItalicCTL = Nothing}
        | Just True <- bold rPr =
            strong . go rPr{isBold = Nothing, isBoldCTL = Nothing}
        | Just True <- isSmallCaps rPr =
            smallcaps . go rPr{isSmallCaps = Nothing}
        | Just True <- isStrike rPr =
            strikeout . go rPr{isStrike = Nothing}
        | Just True <- isRTL rPr =
            spanWith ("",[],[("dir","rtl")]) . go rPr{isRTL = Nothing}
        | inBidi, Just False <- isRTL rPr =
            spanWith ("",[],[("dir","ltr")]) . go rPr{isRTL = Nothing}
        | Just SupScrpt <- rVertAlign rPr =
            superscript . go rPr{rVertAlign = Nothing}
        | Just SubScrpt <- rVertAlign rPr =
            subscript . go rPr{rVertAlign = Nothing}
        | Just "single" <- rUnderline rPr =
            Pandoc.underline . go rPr{rUnderline = Nothing}
        | otherwise = id
  return $ go rPr'


runToInlines :: PandocMonad m => Run -> DocxContext m Inlines
runToInlines (Run rs runElems)
  | maybe False isCodeCharStyle $ rParentStyle rs = do
      rPr <- resolveDependentRunStyle rs
      let codeString = code $ T.concat $ map runElemToText runElems
      return $ case rVertAlign rPr of
        Just SupScrpt -> superscript codeString
        Just SubScrpt -> subscript codeString
        _             -> codeString
  | otherwise = do
      rPr <- resolveDependentRunStyle rs
      let ils = smushInlines (map runElemToInlines runElems)
      transform <- runStyleToTransform rPr
      return $ transform ils
runToInlines (Footnote bps) = note . smushBlocks <$> mapM bodyPartToBlocks bps
runToInlines (Endnote bps) = note . smushBlocks <$> mapM bodyPartToBlocks bps
runToInlines (InlineDrawing fp title alt bs ext) = do
  (lift . lift) $ P.insertMedia fp Nothing bs
  return $ imageWith (extentToAttr ext) (T.pack fp) title $ text alt
runToInlines InlineChart = return $ spanWith ("", ["chart"], []) $ text "[CHART]"

extentToAttr :: Extent -> Attr
extentToAttr (Just (w, h)) =
  ("", [], [("width", showDim w), ("height", showDim h)] )
  where
    showDim d = tshow (d / 914400) <> "in"
extentToAttr _ = nullAttr

blocksToInlinesWarn :: PandocMonad m => T.Text -> Blocks -> DocxContext m Inlines
blocksToInlinesWarn cmtId blks = do
  let paraOrPlain :: Block -> Bool
      paraOrPlain (Para _)  = True
      paraOrPlain (Plain _) = True
      paraOrPlain _         = False
  unless (all paraOrPlain blks) $
    lift $ P.report $ DocxParserWarning $
      "Docx comment " <> cmtId <> " will not retain formatting"
  return $ blocksToInlines' (toList blks)

-- The majority of work in this function is done in the primed
-- subfunction `partPartToInlines'`. We make this wrapper so that we
-- don't have to modify `docxImmedPrevAnchor` state after every function.
parPartToInlines :: PandocMonad m => ParPart -> DocxContext m Inlines
parPartToInlines parPart =
  case parPart of
    (BookMark _ anchor) | anchor `notElem` dummyAnchors -> do
      inHdrBool <- asks docxInHeaderBlock
      ils <- parPartToInlines' parPart
      immedPrevAnchor <- gets docxImmedPrevAnchor
      unless (isJust immedPrevAnchor || inHdrBool)
        (modify $ \s -> s{ docxImmedPrevAnchor = Just anchor})
      return ils
    _ -> do
      ils <- parPartToInlines' parPart
      modify $ \s -> s{ docxImmedPrevAnchor = Nothing}
      return ils

parPartToInlines' :: PandocMonad m => ParPart -> DocxContext m Inlines
parPartToInlines' (PlainRun r) = runToInlines r
parPartToInlines' (ChangedRuns (TrackedChange Insertion (ChangeInfo _ author date)) runs) = do
  opts <- asks docxOptions
  case readerTrackChanges opts of
    AcceptChanges -> smushInlines <$> mapM runToInlines runs
    RejectChanges -> return mempty
    AllChanges    -> do
      ils <- smushInlines <$> mapM runToInlines runs
      let attr = ("", ["insertion"], addAuthorAndDate author date)
      return $ spanWith attr ils
parPartToInlines' (ChangedRuns (TrackedChange Deletion (ChangeInfo _ author date)) runs) = do
  opts <- asks docxOptions
  case readerTrackChanges opts of
    AcceptChanges -> return mempty
    RejectChanges -> smushInlines <$> mapM runToInlines runs
    AllChanges    -> do
      ils <- smushInlines <$> mapM runToInlines runs
      let attr = ("", ["deletion"], addAuthorAndDate author date)
      return $ spanWith attr ils
parPartToInlines' (CommentStart cmtId author date bodyParts) = do
  opts <- asks docxOptions
  case readerTrackChanges opts of
    AllChanges -> do
      blks <- smushBlocks <$> mapM bodyPartToBlocks bodyParts
      ils <- blocksToInlinesWarn cmtId blks
      let attr = ("", ["comment-start"], ("id", cmtId) : addAuthorAndDate author date)
      return $ spanWith attr ils
    _ -> return mempty
parPartToInlines' (CommentEnd cmtId) = do
  opts <- asks docxOptions
  case readerTrackChanges opts of
    AllChanges -> do
      let attr = ("", ["comment-end"], [("id", cmtId)])
      return $ spanWith attr mempty
    _ -> return mempty
parPartToInlines' (BookMark _ anchor) | anchor `elem` dummyAnchors =
  return mempty
parPartToInlines' (BookMark _ anchor) =
  -- We record these, so we can make sure not to overwrite
  -- user-defined anchor links with header auto ids.
  do
    -- get whether we're in a header.
    inHdrBool <- asks docxInHeaderBlock
    -- Get the anchor map.
    anchorMap <- gets docxAnchorMap
    -- We don't want to rewrite if we're in a header, since we'll take
    -- care of that later, when we make the header anchor. If the
    -- bookmark were already in uniqueIdent form, this would lead to a
    -- duplication. Otherwise, we check to see if the id is already in
    -- there. Rewrite if necessary. This will have the possible effect
    -- of rewriting user-defined anchor links. However, since these
    -- are not defined in pandoc, it seems like a necessary evil to
    -- avoid an extra pass.
    immedPrevAnchor <- gets docxImmedPrevAnchor
    case immedPrevAnchor of
      Just prevAnchor -> do
        unless inHdrBool
          (modify $ \s -> s { docxAnchorMap = M.insert anchor prevAnchor anchorMap})
        return mempty
      Nothing -> do
        exts <- asks (readerExtensions . docxOptions)
        let newAnchor =
              if not inHdrBool && anchor `elem` M.elems anchorMap
              then uniqueIdent exts [Str anchor]
                     (Set.fromList $ M.elems anchorMap)
              else anchor
        unless inHdrBool
          (modify $ \s -> s { docxAnchorMap = M.insert anchor newAnchor anchorMap})
        return $ spanWith (newAnchor, ["anchor"], []) mempty
parPartToInlines' (Drawing fp title alt bs ext) = do
  (lift . lift) $ P.insertMedia fp Nothing bs
  return $ imageWith (extentToAttr ext) (T.pack fp) title $ text alt
parPartToInlines' Chart =
  return $ spanWith ("", ["chart"], []) $ text "[CHART]"
parPartToInlines' (InternalHyperLink anchor runs) = do
  ils <- smushInlines <$> mapM runToInlines runs
  return $ link ("#" <> anchor) "" ils
parPartToInlines' (ExternalHyperLink target runs) = do
  ils <- smushInlines <$> mapM runToInlines runs
  return $ link target "" ils
parPartToInlines' (PlainOMath exps) =
  return $ math $ writeTeX exps
parPartToInlines' (Field info runs) =
  case info of
    HyperlinkField url -> parPartToInlines' $ ExternalHyperLink url runs
    UnknownField -> smushInlines <$> mapM runToInlines runs
parPartToInlines' NullParPart = return mempty

isAnchorSpan :: Inline -> Bool
isAnchorSpan (Span (_, ["anchor"], []) _) = True
isAnchorSpan _ = False

dummyAnchors :: [T.Text]
dummyAnchors = ["_GoBack"]

makeHeaderAnchor :: PandocMonad m => Blocks -> DocxContext m Blocks
makeHeaderAnchor bs = traverse makeHeaderAnchor' bs

makeHeaderAnchor' :: PandocMonad m => Block -> DocxContext m Block
-- If there is an anchor already there (an anchor span in the header,
-- to be exact), we rename and associate the new id with the old one.
makeHeaderAnchor' (Header n (ident, classes, kvs) ils)
  | (c:_) <- filter isAnchorSpan ils
  , (Span (anchIdent, ["anchor"], _) cIls) <- c = do
    hdrIDMap <- gets docxAnchorMap
    exts <- asks (readerExtensions . docxOptions)
    let newIdent = if T.null ident
                   then uniqueIdent exts ils (Set.fromList $ M.elems hdrIDMap)
                   else ident
        newIls = concatMap f ils where f il | il == c   = cIls
                                            | otherwise = [il]
    modify $ \s -> s {docxAnchorMap = M.insert anchIdent newIdent hdrIDMap}
    makeHeaderAnchor' $ Header n (newIdent, classes, kvs) newIls
-- Otherwise we just give it a name, and register that name (associate
-- it with itself.)
makeHeaderAnchor' (Header n (ident, classes, kvs) ils) =
  do
    hdrIDMap <- gets docxAnchorMap
    exts <- asks (readerExtensions . docxOptions)
    let newIdent = if T.null ident
                   then uniqueIdent exts ils (Set.fromList $ M.elems hdrIDMap)
                   else ident
    modify $ \s -> s {docxAnchorMap = M.insert newIdent newIdent hdrIDMap}
    return $ Header n (newIdent, classes, kvs) ils
makeHeaderAnchor' blk = return blk

-- Rewrite a standalone paragraph block as a plain
singleParaToPlain :: Blocks -> Blocks
singleParaToPlain blks
  | (Para ils :< seeq) <- viewl $ unMany blks
  , Seq.null seeq =
      singleton $ Plain ils
singleParaToPlain blks = blks

cellToCell :: PandocMonad m => RowSpan -> Docx.Cell -> DocxContext m Pandoc.Cell
cellToCell rowSpan (Docx.Cell gridSpan _ bps) = do
  blks <- smushBlocks <$> mapM bodyPartToBlocks bps
  let blks' = singleParaToPlain $ fromList $ blocksToDefinitions $ blocksToBullets $ toList blks
  return (cell AlignDefault rowSpan (ColSpan (fromIntegral gridSpan)) blks')

rowsToRows :: PandocMonad m => [Docx.Row] -> DocxContext m [Pandoc.Row]
rowsToRows rows = do
  let rowspans = (fmap . fmap) (first RowSpan) (Docx.rowsToRowspans rows)
  cells <- traverse (traverse (uncurry cellToCell)) rowspans
  return (fmap (Pandoc.Row nullAttr) cells)

splitHeaderRows :: Bool -> [Docx.Row] -> ([Docx.Row], [Docx.Row])
splitHeaderRows hasFirstRowFormatting rs = bimap reverse reverse $ fst
  $ if hasFirstRowFormatting
    then foldl' f ((take 1 rs, []), True) (drop 1 rs)
    else foldl' f (([], []), False) rs
  where
    f ((headerRows, bodyRows), previousRowWasHeader) r@(Docx.Row h cs)
      | h == HasTblHeader || (previousRowWasHeader && any isContinuationCell cs)
        = ((r : headerRows, bodyRows), True)
      | otherwise
        = ((headerRows, r : bodyRows), False)

    isContinuationCell (Docx.Cell _ vm _) = vm == Docx.Continue


-- like trimInlines, but also take out linebreaks
trimSps :: Inlines -> Inlines
trimSps (Many ils) = Many $ Seq.dropWhileL isSp $Seq.dropWhileR isSp ils
  where isSp Space     = True
        isSp SoftBreak = True
        isSp LineBreak = True
        isSp _         = False

extraAttr :: (Eq (StyleName a), HasStyleName a) => a -> Attr
extraAttr s = ("", [], [("custom-style", fromStyleName $ getStyleName s)])

parStyleToTransform :: PandocMonad m => ParagraphStyle -> DocxContext m (Blocks -> Blocks)
parStyleToTransform pPr = case pStyle pPr of
  c@(getStyleName -> styleName):cs
    | styleName `elem` divsToKeep -> do
        let pPr' = pPr { pStyle = cs }
        transform <- parStyleToTransform pPr'
        return $ divWith ("", [normalizeToClassName styleName], []) . transform
    | styleName `elem` listParagraphStyles -> do
        let pPr' = pPr { pStyle = cs, indentation = Nothing}
        transform <- parStyleToTransform pPr'
        return $ divWith ("", [normalizeToClassName styleName], []) . transform
    | otherwise -> do
        let pPr' = pPr { pStyle = cs }
        transform <- parStyleToTransform pPr'
        styles <- asks (isEnabled Ext_styles . docxOptions)
        return $
          (if styles then divWith (extraAttr c) else id)
          . (if isBlockQuote c then blockQuote else id)
          . transform
  []
    | Just left <- indentation pPr >>= leftParIndent -> do
        let pPr' = pPr { indentation = Nothing }
            hang = fromMaybe 0 $ indentation pPr >>= hangingParIndent
        transform <- parStyleToTransform pPr'
        return $ if (left - hang) > 0
                 then blockQuote . transform
                 else transform
    | otherwise -> return id

normalizeToClassName :: (FromStyleName a) => a -> T.Text
normalizeToClassName = T.map go . fromStyleName
  where go c | isSpace c = '-'
             | otherwise = c

bodyPartToTableCaption :: PandocMonad m => BodyPart -> DocxContext m (Maybe Blocks)
bodyPartToTableCaption (TblCaption pPr parparts) =
  Just <$> bodyPartToBlocks (Paragraph pPr parparts)
bodyPartToTableCaption _ = pure Nothing

bodyPartToBlocks :: PandocMonad m => BodyPart -> DocxContext m Blocks
bodyPartToBlocks (Paragraph pPr parparts)
  | Just True <- pBidi pPr = do
      let pPr' = pPr { pBidi = Nothing }
      local (\s -> s{ docxInBidi = True })
        (bodyPartToBlocks (Paragraph pPr' parparts))
  | isCodeDiv pPr = do
      transform <- parStyleToTransform pPr
      return $
        transform $
        codeBlock $
        T.concat $
        map parPartToText parparts
  | Just (style, n) <- pHeading pPr = do
    ils <-local (\s-> s{docxInHeaderBlock=True})
           (smushInlines <$> mapM parPartToInlines parparts)
    makeHeaderAnchor $
      headerWith ("", map normalizeToClassName . delete style $ getStyleNames (pStyle pPr), []) n ils
  | otherwise = do
    ils <- trimSps . smushInlines <$> mapM parPartToInlines parparts
    prevParaIls <- gets docxPrevPara
    dropIls <- gets docxDropCap
    let ils' = dropIls <> ils
    let (paraOrPlain, pPr')
          | hasStylesInheritedFrom ["Compact"] pPr = (plain, removeStyleNamed "Compact" pPr)
          | otherwise = (para, pPr)
    if dropCap pPr'
      then do modify $ \s -> s { docxDropCap = ils' }
              return mempty
      else do modify $ \s -> s { docxDropCap = mempty }
              let ils'' = (if null prevParaIls then mempty
                          else prevParaIls <> space) <> ils'
                  handleInsertion = do
                    modify $ \s -> s {docxPrevPara = mempty}
                    transform <- parStyleToTransform pPr'
                    return $ transform $ paraOrPlain ils''
              opts <- asks docxOptions
              case (pChange pPr', readerTrackChanges opts) of
                  _ | null ils'', not (isEnabled Ext_empty_paragraphs opts) ->
                    return mempty
                  (Just (TrackedChange Insertion _), AcceptChanges) ->
                      handleInsertion
                  (Just (TrackedChange Insertion _), RejectChanges) -> do
                      modify $ \s -> s {docxPrevPara = ils''}
                      return mempty
                  (Just (TrackedChange Insertion (ChangeInfo _ cAuthor cDate))
                   , AllChanges) -> do
                      let attr = ("", ["paragraph-insertion"], addAuthorAndDate cAuthor cDate)
                          insertMark = spanWith attr mempty
                      transform <- parStyleToTransform pPr'
                      return $ transform $
                        paraOrPlain $ ils'' <> insertMark
                  (Just (TrackedChange Deletion _), AcceptChanges) -> do
                      modify $ \s -> s {docxPrevPara = ils''}
                      return mempty
                  (Just (TrackedChange Deletion _), RejectChanges) ->
                      handleInsertion
                  (Just (TrackedChange Deletion (ChangeInfo _ cAuthor cDate))
                   , AllChanges) -> do
                      let attr = ("", ["paragraph-deletion"], addAuthorAndDate cAuthor cDate)
                          insertMark = spanWith attr mempty
                      transform <- parStyleToTransform pPr'
                      return $ transform $
                        paraOrPlain $ ils'' <> insertMark
                  _ -> handleInsertion
bodyPartToBlocks (ListItem pPr numId lvl (Just levelInfo) parparts) = do
  -- We check whether this current numId has previously been used,
  -- since Docx expects us to pick up where we left off.
  listState <- gets docxListState
  let startFromState = M.lookup (numId, lvl) listState
      Level _ fmt txt startFromLevelInfo = levelInfo
      start = case startFromState of
        Just n -> n + 1
        Nothing -> fromMaybe 1 startFromLevelInfo
      kvs = [ ("level", lvl)
            , ("num-id", numId)
            , ("format", fmt)
            , ("text", txt)
            , ("start", tshow start)
            ]
  modify $ \st -> st{ docxListState =
    -- expire all the continuation data for lists of level > this one:
    -- a new level 1 list item resets continuation for level 2+
    let notExpired (_, lvl') _ = lvl' <= lvl
    in M.insert (numId, lvl) start (M.filterWithKey notExpired listState) }
  blks <- bodyPartToBlocks (Paragraph pPr parparts)
  return $ divWith ("", ["list-item"], kvs) blks
bodyPartToBlocks (ListItem pPr _ _ _ parparts) =
  let pPr' = pPr {pStyle = constructBogusParStyleData "list-paragraph": pStyle pPr}
  in
    bodyPartToBlocks $ Paragraph pPr' parparts
bodyPartToBlocks (TblCaption _ _) =
  return $ para mempty -- collected separately
bodyPartToBlocks (Tbl _ _ _ []) =
  return $ para mempty
bodyPartToBlocks (Tbl cap grid look parts) = do
  captions <- gets docxTableCaptions
  fullCaption <- case captions of
    c : cs -> do
      modify (\s -> s { docxTableCaptions = cs })
      return c
    [] -> return $ if T.null cap then mempty else plain (text cap)
  let shortCaption = if T.null cap then Nothing else Just (toList (text cap))
      cap' = caption shortCaption fullCaption
      (hdr, rows) = splitHeaderRows (firstRowFormatting look) parts

  let width = maybe 0 maximum $ nonEmpty $ map rowLength parts
      rowLength :: Docx.Row -> Int
      rowLength (Docx.Row _ c) = sum (fmap (\(Docx.Cell gridSpan _ _) -> fromIntegral gridSpan) c)

  headerCells <- rowsToRows hdr
  bodyCells <- rowsToRows rows

      -- Horizontal column alignment goes to the default at the moment. Getting
      -- it might be difficult, since there doesn't seem to be a column entity
      -- in docx.
  let alignments = replicate width AlignDefault
      totalWidth = sum grid
      widths = (\w -> ColWidth (fromInteger w / fromInteger totalWidth)) <$> grid

  return $ table cap'
                 (zip alignments widths)
                 (TableHead nullAttr headerCells)
                 [TableBody nullAttr 0 [] bodyCells]
                 (TableFoot nullAttr [])
bodyPartToBlocks (OMathPara e) =
  return $ para $ displayMath (writeTeX e)

-- replace targets with generated anchors.
rewriteLink' :: PandocMonad m => Inline -> DocxContext m Inline
rewriteLink' l@(Link attr ils (T.uncons -> Just ('#',target), title)) = do
  anchorMap <- gets docxAnchorMap
  case M.lookup target anchorMap of
    Just newTarget -> do
      modify $ \s -> s{docxAnchorSet = Set.insert newTarget (docxAnchorSet s)}
      return $ Link attr ils ("#" <> newTarget, title)
    Nothing        -> do
      modify $ \s -> s{docxAnchorSet = Set.insert target (docxAnchorSet s)}
      return l
rewriteLink' il = return il

rewriteLinks :: PandocMonad m => [Block] -> DocxContext m [Block]
rewriteLinks = mapM (walkM rewriteLink')

removeOrphanAnchors'' :: PandocMonad m => Inline -> DocxContext m [Inline]
removeOrphanAnchors'' s@(Span (ident, classes, _) ils)
  | "anchor" `elem` classes = do
      anchorSet <- gets docxAnchorSet
      return $ if ident `Set.member` anchorSet
               then [s]
               else ils
removeOrphanAnchors'' il = return [il]

removeOrphanAnchors' :: PandocMonad m => [Inline] -> DocxContext m [Inline]
removeOrphanAnchors' ils = liftM concat $ mapM removeOrphanAnchors'' ils

removeOrphanAnchors :: PandocMonad m => [Block] -> DocxContext m [Block]
removeOrphanAnchors = mapM (walkM removeOrphanAnchors')

bodyToOutput :: PandocMonad m => Body -> DocxContext m (Meta, [Block])
bodyToOutput (Body bps) = do
  let (metabps, blkbps) = sepBodyParts bps
  meta <- bodyPartsToMeta metabps
  captions <- catMaybes <$> mapM bodyPartToTableCaption blkbps
  modify (\s -> s { docxTableCaptions = captions })
  blks <- smushBlocks <$> mapM bodyPartToBlocks blkbps
  blks' <- rewriteLinks $ blocksToDefinitions $ blocksToBullets $ toList blks
  blks'' <- removeOrphanAnchors blks'
  return (meta, blks'')

docxToOutput :: PandocMonad m
             => ReaderOptions
             -> Docx
             -> m (Meta, [Block])
docxToOutput opts (Docx (Document _ body)) =
  let dEnv   = def { docxOptions  = opts} in
   evalDocxContext (bodyToOutput body) dEnv def

addAuthorAndDate :: T.Text -> Maybe T.Text -> [(T.Text, T.Text)]
addAuthorAndDate author mdate =
  ("author", author) : maybe [] (\date -> [("date", date)]) mdate
