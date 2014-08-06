{-# LANGUAGE PatternGuards #-}

{-
Copyright (C) 2014 Jesse Rosenthal <jrosenthal@jhu.edu>

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
   Module      : Text.Pandoc.Readers.Docx
   Copyright   : Copyright (C) 2014 Jesse Rosenthal
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
  - [X] Emph (From italics. `underline` currently read as span. In
        future, it might optionally be emph as well)
  - [X] Strong
  - [X] Strikeout
  - [X] Superscript
  - [X] Subscript
  - [X] SmallCaps
  - [ ] Quoted
  - [ ] Cite
  - [X] Code (styled with `VerbatimChar`)
  - [X] Space
  - [X] LineBreak (these are invisible in Word: entered with Shift-Return)
  - [ ] Math
  - [X] Link (links to an arbitrary bookmark create a span with the target as
        id and "anchor" class)
  - [-] Image (Links to path in archive. Future option for
        data-encoded URI likely.)
  - [X] Note (Footnotes and Endnotes are silently combined.)
-}

module Text.Pandoc.Readers.Docx
       ( readDocx
       ) where

import Codec.Archive.Zip
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Builder (text, toList)
import Text.Pandoc.Walk
import Text.Pandoc.Readers.Docx.Parse
import Text.Pandoc.Readers.Docx.Lists
import Text.Pandoc.Readers.Docx.Reducible
import Text.Pandoc.Shared
import Text.Pandoc.MediaBag (insertMedia, MediaBag)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (delete, stripPrefix, (\\), intersperse, intersect)
import Data.Monoid
import Text.TeXMath (writeTeX)
import qualified Text.TeXMath.Types as TM
import Data.Default (Default)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative ((<$>))

readDocx :: ReaderOptions
         -> B.ByteString
         -> (Pandoc, MediaBag)
readDocx opts bytes =
  case archiveToDocx (toArchive bytes) of
    Right docx -> (Pandoc meta blks, mediaBag) where
      (meta, blks, mediaBag) = (docxToOutput opts docx)
    Left _   -> error $ "couldn't parse docx file"

data DState = DState { docxAnchorMap :: M.Map String String
                     , docxMediaBag      :: MediaBag }

instance Default DState where
  def = DState { docxAnchorMap = M.empty
               , docxMediaBag  = mempty }

data DEnv = DEnv { docxOptions  :: ReaderOptions
                 , docxInHeaderBlock :: Bool }

instance Default DEnv where
  def = DEnv def False

type DocxContext = ReaderT DEnv (State DState)

evalDocxContext :: DocxContext a -> DEnv -> DState -> a
evalDocxContext ctx env st = evalState (runReaderT ctx env) st

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

-- This is empty, but we put it in for future-proofing.
spansToKeep :: [String]
spansToKeep = []

divsToKeep :: [String]
divsToKeep = ["list-item", "Definition", "DefinitionTerm"]

metaStyles :: M.Map String String
metaStyles = M.fromList [ ("Title", "title")
                        , ("Subtitle", "subtitle")
                        , ("Author", "author")
                        , ("Date", "date")
                        , ("Abstract", "abstract")]

sepBodyParts :: [BodyPart] -> ([BodyPart], [BodyPart])
sepBodyParts = span (\bp -> (isMetaPar bp || isEmptyPar bp))

isMetaPar :: BodyPart -> Bool
isMetaPar (Paragraph pPr _) =
  not $ null $ intersect (pStyle pPr) (M.keys metaStyles)
isMetaPar _ = False

isEmptyPar :: BodyPart -> Bool
isEmptyPar (Paragraph _ parParts) =
  all isEmptyParPart parParts
  where
    isEmptyParPart (PlainRun (Run _ runElems)) = all isEmptyElem runElems
    isEmptyParPart _ = False
    isEmptyElem (TextRun s) = trim s == ""
    isEmptyElem _           = True
isEmptyPar _ = False

bodyPartsToMeta' :: [BodyPart] -> DocxContext (M.Map String MetaValue)
bodyPartsToMeta' [] = return M.empty
bodyPartsToMeta' (bp : bps)
  | (Paragraph pPr parParts) <- bp
  , (c : _)<- intersect (pStyle pPr) (M.keys metaStyles)
  , (Just metaField) <- M.lookup c metaStyles = do
    inlines <- parPartsToInlines parParts
    remaining <- bodyPartsToMeta' bps
    let
      f (MetaInlines ils) (MetaInlines ils') = MetaBlocks [Para ils, Para ils']
      f (MetaInlines ils) (MetaBlocks blks) = MetaBlocks ((Para ils) : blks)
      f m (MetaList mv) = MetaList (m : mv)
      f m n             = MetaList [m, n]
    return $ M.insertWith f metaField (MetaInlines inlines) remaining
bodyPartsToMeta' (_ : bps) = bodyPartsToMeta' bps

bodyPartsToMeta :: [BodyPart] -> DocxContext Meta
bodyPartsToMeta bps = do
  mp <- bodyPartsToMeta' bps
  let mp' =
        case M.lookup "author" mp of
          Just mv -> M.insert "author" (fixAuthors mv) mp
          Nothing -> mp
  return $ Meta mp'

fixAuthors :: MetaValue -> MetaValue
fixAuthors (MetaBlocks blks) =
  MetaList $ map g $ filter f blks
    where f (Para _) = True
          f _          = False
          g (Para ils) = MetaInlines ils
          g _          = MetaInlines []
fixAuthors mv = mv

runStyleToContainers :: RunStyle -> [Container Inline]
runStyleToContainers rPr =
  let spanClassToContainers :: String -> [Container Inline]
      spanClassToContainers s | s `elem` codeSpans =
        [Container $ (\ils -> Code ("", [], []) (concatMap ilToCode ils))]
      spanClassToContainers s | s `elem` spansToKeep =
        [Container $ Span ("", [s], [])]
      spanClassToContainers _ = []

      classContainers = case rStyle rPr of
        Nothing -> []
        Just s  -> spanClassToContainers s

      formatters = map Container $ mapMaybe id
                   [ if isBold rPr then (Just Strong) else Nothing
                   , if isItalic rPr then (Just Emph) else Nothing
                   , if isSmallCaps rPr then (Just SmallCaps) else Nothing
                   , if isStrike rPr then (Just Strikeout) else Nothing
                   , if isSuperScript rPr then (Just Superscript) else Nothing
                   , if isSubScript rPr then (Just Subscript) else Nothing
                   , rUnderline rPr >>=
                     (\f -> if f == "single" then (Just Emph) else Nothing)
                 ]
  in
   classContainers ++ formatters

parStyleToContainers :: ParagraphStyle -> [Container Block]
parStyleToContainers pPr | (c:cs) <- pStyle pPr, Just n <- isHeaderClass c =
  [Container $ \_ -> Header n ("", delete ("Heading" ++ show n) cs, []) []]
parStyleToContainers pPr | (c:cs) <- pStyle pPr, c `elem` divsToKeep =
  let pPr' = pPr { pStyle = cs }
  in
   (Container $ Div ("", [c], [])) : (parStyleToContainers pPr')
parStyleToContainers pPr | (c:cs) <- pStyle pPr, c `elem` codeDivs =
  -- This is a bit of a cludge. We make the codeblock from the raw
  -- parparts in bodyPartToBlocks. But we need something to match against.
  let pPr' = pPr { pStyle = cs }
  in
   (Container $ \_ -> CodeBlock ("", [], []) "") : (parStyleToContainers pPr')
parStyleToContainers pPr | (c:cs) <- pStyle pPr,  c `elem` listParagraphDivs =
  let pPr' = pPr { pStyle = cs, indentation = Nothing}
  in
   (Container $ Div ("", [c], [])) : (parStyleToContainers pPr')

parStyleToContainers pPr | (c:cs) <- pStyle pPr, c `elem` blockQuoteDivs =
  let pPr' = pPr { pStyle = cs \\ blockQuoteDivs }
  in
   (Container BlockQuote) : (parStyleToContainers pPr')
parStyleToContainers pPr | (_:cs) <- pStyle pPr =
  let pPr' = pPr { pStyle = cs}
  in
    parStyleToContainers pPr'
parStyleToContainers pPr | null (pStyle pPr),
                          Just left <- indentation pPr >>= leftParIndent,
                          Just hang <- indentation pPr >>= hangingParIndent =
  let pPr' = pPr { indentation = Nothing }
  in
   case (left - hang) > 0 of
     True -> (Container BlockQuote) : (parStyleToContainers pPr')
     False -> parStyleToContainers pPr'
parStyleToContainers pPr | null (pStyle pPr),
                          Just left <- indentation pPr >>= leftParIndent =
  let pPr' = pPr { indentation = Nothing }
  in
   case left > 0 of
     True -> (Container BlockQuote) : (parStyleToContainers pPr')
     False -> parStyleToContainers pPr'
parStyleToContainers _ = []


strToInlines :: String -> [Inline]
strToInlines = toList . text

codeSpans :: [String]
codeSpans = ["VerbatimChar"]

blockQuoteDivs :: [String]
blockQuoteDivs = ["Quote", "BlockQuote"]

codeDivs :: [String]
codeDivs = ["SourceCode"]

runElemToInlines :: RunElem -> [Inline]
runElemToInlines (TextRun s) = strToInlines s
runElemToInlines (LnBrk) = [LineBreak]
runElemToInlines (Tab) = [Space]

runElemToString :: RunElem -> String
runElemToString (TextRun s) = s
runElemToString (LnBrk) = ['\n']
runElemToString (Tab) = ['\t']

runElemsToString :: [RunElem] -> String
runElemsToString = concatMap runElemToString

runToString :: Run -> String
runToString (Run _ runElems) = runElemsToString runElems
runToString _ = ""

parPartToString :: ParPart -> String
parPartToString (PlainRun run) = runToString run
parPartToString (InternalHyperLink _ runs) = concatMap runToString runs
parPartToString (ExternalHyperLink _ runs) = concatMap runToString runs
parPartToString _ = ""


inlineCodeContainer :: Container Inline -> Bool
inlineCodeContainer (Container f) = case f [] of
  Code _ "" -> True
  _         -> False
inlineCodeContainer _ = False


runToInlines :: Run -> DocxContext [Inline]
runToInlines (Run rs runElems)
  | any inlineCodeContainer (runStyleToContainers rs) =
      return $
      rebuild (runStyleToContainers rs) $ [Str $ runElemsToString runElems]
  | otherwise =
      return $
      rebuild (runStyleToContainers rs) (concatMap runElemToInlines runElems)
runToInlines (Footnote bps) =
  concatMapM bodyPartToBlocks bps >>= (\blks -> return [Note blks])
runToInlines (Endnote bps) =
  concatMapM bodyPartToBlocks bps >>= (\blks -> return [Note blks])

parPartToInlines :: ParPart -> DocxContext [Inline]
parPartToInlines (PlainRun r) = runToInlines r
parPartToInlines (Insertion _ author date runs) = do
  opts <- asks docxOptions
  case readerTrackChanges opts of
    AcceptChanges -> concatMapM runToInlines runs >>= return
    RejectChanges -> return []
    AllChanges    -> do
      ils <- (concatMapM runToInlines runs)
      return [Span
              ("", ["insertion"], [("author", author), ("date", date)])
              ils]
parPartToInlines (Deletion _ author date runs) = do
  opts <- asks docxOptions
  case readerTrackChanges opts of
    AcceptChanges -> return []
    RejectChanges -> concatMapM runToInlines runs >>= return
    AllChanges    -> do
      ils <- concatMapM runToInlines runs
      return [Span
              ("", ["deletion"], [("author", author), ("date", date)])
              ils]
parPartToInlines (BookMark _ anchor) | anchor `elem` dummyAnchors = return []
parPartToInlines (BookMark _ anchor) =
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
    let newAnchor =
          if not inHdrBool && anchor `elem` (M.elems anchorMap)
          then uniqueIdent [Str anchor] (M.elems anchorMap)
          else anchor
    unless inHdrBool
      (modify $ \s -> s { docxAnchorMap = M.insert anchor newAnchor anchorMap})
    return [Span (newAnchor, ["anchor"], []) []]
parPartToInlines (Drawing fp bs) = do
  mediaBag <- gets docxMediaBag
  modify $ \s -> s { docxMediaBag = insertMedia fp Nothing bs mediaBag }
  return [Image [] (fp, "")]
parPartToInlines (InternalHyperLink anchor runs) = do
  ils <- concatMapM runToInlines runs
  return [Link ils ('#' : anchor, "")]
parPartToInlines (ExternalHyperLink target runs) = do
  ils <- concatMapM runToInlines runs
  return [Link ils (target, "")]
parPartToInlines (PlainOMath omath) = do
  e <- oMathToExps omath
  return [Math InlineMath (writeTeX e)]

oMathToExps :: OMath -> DocxContext [TM.Exp]
oMathToExps (OMath oMathElems) = concatMapM oMathElemToExps oMathElems

oMathElemToExps :: OMathElem -> DocxContext [TM.Exp]
oMathElemToExps (Accent style base) = do
  baseExp <- baseToExp base
  let chr = case accentChar style of
        Just c -> c
        Nothing -> '\180'       -- default to acute.
  return [TM.EOver False baseExp (TM.ESymbol TM.Accent [chr])]
oMathElemToExps(Bar style base) = do
  baseExp <- baseToExp base
  return $ case barPos style of
    Top    -> [TM.EOver False baseExp (TM.ESymbol TM.Accent "\175")]
    Bottom -> [TM.EUnder False baseExp (TM.ESymbol TM.Accent "\818")]
oMathElemToExps (Box base) =
  (\e -> return [e]) =<< baseToExp base
oMathElemToExps (BorderBox base) =
  -- TODO: This should be "\\boxed" somehow
  (\e -> return [e]) =<< baseToExp base
oMathElemToExps (Delimiter dPr bases) = do
  baseExps <- mapM baseToExp bases
  let inDelimExps = map Right baseExps
      beg = fromMaybe '(' (delimBegChar dPr)
      end = fromMaybe ')' (delimEndChar dPr)
      sep = fromMaybe '|' (delimSepChar dPr)
      exps = intersperse (Left [sep]) inDelimExps
  return [TM.EDelimited [beg] [end] exps]
oMathElemToExps (EquationArray bases) = do
  let f b = do bs <- baseToExp' b
               return [bs]
  baseExps <- mapM f bases
  return [TM.EArray [] baseExps]
oMathElemToExps (Fraction num denom) = do
  numExp  <- concatMapM oMathElemToExps num >>= (return . TM.EGrouped)
  denExp  <- concatMapM oMathElemToExps denom >>= (return . TM.EGrouped)
  return [TM.EFraction TM.NormalFrac numExp denExp]
oMathElemToExps (Function fname base) = do
  -- We need a string for the fname, but omml gives it to us as a
  -- series of oMath elems. We're going to filter out the oMathRuns,
  -- which should work for us most of the time.
  let f :: OMathElem -> String
      f (OMathRun _ run) = runToString run
      f _                  = ""
      fnameString = concatMap f fname
  baseExp  <- baseToExp base
  return [TM.EMathOperator fnameString, baseExp]
oMathElemToExps (Group style base)
  | Just Top <- groupPos style = do
    baseExp <- baseToExp base
    let chr = case groupChr style of
          Just c -> c
          Nothing -> '\65079'   -- default to overbrace
    return [TM.EOver False baseExp (TM.ESymbol TM.Accent [chr])]
  | otherwise = do
    baseExp <- baseToExp base
    let chr = case groupChr style of
          Just c -> c
          Nothing -> '\65080'   -- default to underbrace
    return [TM.EUnder False baseExp (TM.ESymbol TM.Accent [chr])]
oMathElemToExps (LowerLimit base limElems) = do
  baseExp <- baseToExp base
  lim <- concatMapM oMathElemToExps limElems >>= (return . TM.EGrouped)
  return [TM.EUnder True lim baseExp]
oMathElemToExps (UpperLimit base limElems) = do
  baseExp <- baseToExp base
  lim <- concatMapM oMathElemToExps limElems >>= (return . TM.EGrouped)
  return [TM.EOver True lim baseExp]
oMathElemToExps (Matrix bases) = do
  rows <- mapM (mapM (\b -> baseToExp' b)) bases
  return [TM.EArray [TM.AlignCenter] rows]
oMathElemToExps (NAry style sub sup base) = do
  subExps  <- concatMapM oMathElemToExps sub
  supExps  <- concatMapM oMathElemToExps sup
  baseExp <-  baseToExp base
  let opChar = case nAryChar style of
        Just c -> c
        -- default to integral
        Nothing -> '\8747'
  return [ TM.ESubsup
           (TM.ESymbol TM.Op [opChar])
           (TM.EGrouped subExps)
           (TM.EGrouped supExps)
         , baseExp]
oMathElemToExps (Phantom base) =
  (\e -> return [TM.EPhantom e]) =<< baseToExp base
oMathElemToExps (Radical degree base) = do
  degExps  <- concatMapM oMathElemToExps degree
  baseExp <- baseToExp base
  return $ case degExps of
    [] -> [TM.ESqrt baseExp]
    ds -> [TM.ERoot (TM.EGrouped ds) baseExp]
oMathElemToExps (PreSubSuper sub sup base) = do
  subExps  <- concatMapM oMathElemToExps sub
  supExps  <- concatMapM oMathElemToExps sup
  baseExp <- baseToExp base
  return [ TM.ESubsup
          (TM.EIdentifier "") (TM.EGrouped subExps) (TM.EGrouped supExps)
         , baseExp]
oMathElemToExps (Sub base sub) = do
  baseExp <- baseToExp base
  subExps  <- concatMapM oMathElemToExps sub
  return [TM.ESub baseExp (TM.EGrouped subExps)]
oMathElemToExps (SubSuper base sub sup) = do
  baseExp <- baseToExp base
  subExps  <- concatMapM oMathElemToExps sub
  supExps  <- concatMapM oMathElemToExps sup
  return [TM.ESubsup baseExp (TM.EGrouped subExps) (TM.EGrouped supExps)]
oMathElemToExps (Super base sup) = do
  baseExp <- baseToExp base
  supExps  <- concatMapM oMathElemToExps sup
  return [TM.ESuper baseExp (TM.EGrouped supExps)]
oMathElemToExps (OMathRun sty run@(Run _ _))
  | NoStyle <- oMathRunTextStyle sty =
    return $ [TM.EIdentifier $ runToString run]
  | Nothing <- oMathRunStyleToTextType sty =
    return $ [TM.EIdentifier $ runToString run]
  | Just textType <-  oMathRunStyleToTextType sty =
      return $ if oMathLit sty
               then [TM.EText textType  (runToString run)]
               else [TM.EStyled textType [TM.EIdentifier $ runToString run]]
oMathElemToExps (OMathRun _ _) = return []

oMathRunStyleToTextType :: OMathRunStyle -> Maybe TM.TextType
oMathRunStyleToTextType mrPr
  | Normal <- oMathRunTextStyle mrPr =
    Just $ TM.TextNormal
  | Styled scr sty  <- oMathRunTextStyle mrPr
    ,Just OBold <- sty
  , Just OSansSerif <- scr =
    Just $ TM.TextSansSerifBold
  | Styled scr sty  <- oMathRunTextStyle mrPr
  , Just OBoldItalic <- sty
  , Just OSansSerif <- scr =
    Just $ TM.TextSansSerifBoldItalic
  | Styled scr sty  <- oMathRunTextStyle mrPr
  , Just OBold <- sty
  , Just OScript <- scr =
    Just $ TM.TextBoldScript
  | Styled scr sty  <- oMathRunTextStyle mrPr
  , Just OBold <- sty
  , Just OFraktur <- scr =
    Just $ TM.TextBoldFraktur
  | Styled scr sty  <- oMathRunTextStyle mrPr
  , Just OItalic <- sty
  , Just OSansSerif <- scr =
    Just $ TM.TextSansSerifItalic
  | Styled _ sty  <- oMathRunTextStyle mrPr
  , Just OBold <- sty =
    Just $ TM.TextBold
  | Styled _ sty  <- oMathRunTextStyle mrPr
  , Just OItalic <- sty =
    Just $ TM.TextItalic
  | Styled scr _  <- oMathRunTextStyle mrPr
  , Just OMonospace <- scr =
    Just $ TM.TextMonospace
  | Styled scr _  <- oMathRunTextStyle mrPr
  , Just OSansSerif <- scr =
    Just $ TM.TextSansSerif
  | Styled scr _  <- oMathRunTextStyle mrPr
  , Just ODoubleStruck <- scr =
    Just $ TM.TextDoubleStruck
  | Styled scr _  <- oMathRunTextStyle mrPr
  , Just OScript <- scr =
    Just $ TM.TextDoubleStruck
  | Styled scr _  <- oMathRunTextStyle mrPr
  , Just OFraktur <- scr =
    Just $ TM.TextFraktur
  | Styled _ sty  <- oMathRunTextStyle mrPr
  , Just OBoldItalic <- sty =
    Just $ TM.TextBoldItalic
  | otherwise = Nothing



baseToExp :: Base -> DocxContext TM.Exp
baseToExp (Base mathElems) =
  concatMapM oMathElemToExps mathElems >>= (return . TM.EGrouped)

-- an ungrouped version of baseToExp
baseToExp' :: Base -> DocxContext [TM.Exp]
baseToExp' (Base mathElems) =
  concatMapM oMathElemToExps mathElems


isAnchorSpan :: Inline -> Bool
isAnchorSpan (Span (ident, classes, kvs) ils) =
  (not . null) ident &&
  classes == ["anchor"] &&
  null kvs &&
  null ils
isAnchorSpan _ = False

dummyAnchors :: [String]
dummyAnchors = ["_GoBack"]

makeHeaderAnchor :: Block -> DocxContext Block
-- If there is an anchor already there (an anchor span in the header,
-- to be exact), we rename and associate the new id with the old one.
makeHeaderAnchor (Header n (_, classes, kvs) ils)
  | (x : xs) <- filter isAnchorSpan ils
  , (Span (ident, _, _) _) <- x
  , notElem ident dummyAnchors =
    do
      hdrIDMap <- gets docxAnchorMap
      let newIdent = uniqueIdent ils (M.elems hdrIDMap)
      modify $ \s -> s {docxAnchorMap = M.insert ident newIdent hdrIDMap}
      return $ Header n (newIdent, classes, kvs) (ils \\ (x:xs))
-- Otherwise we just give it a name, and register that name (associate
-- it with itself.)
makeHeaderAnchor (Header n (_, classes, kvs) ils) =
  do
    hdrIDMap <- gets docxAnchorMap
    let newIdent = uniqueIdent ils (M.elems hdrIDMap)
    modify $ \s -> s {docxAnchorMap = M.insert newIdent newIdent hdrIDMap}
    return $ Header n (newIdent, classes, kvs) ils
makeHeaderAnchor blk = return blk


parPartsToInlines :: [ParPart] -> DocxContext [Inline]
parPartsToInlines parparts = do
  ils <- concatMapM parPartToInlines parparts
  return $ reduceList $ ils

cellToBlocks :: Cell -> DocxContext [Block]
cellToBlocks (Cell bps) = concatMapM bodyPartToBlocks bps

rowToBlocksList :: Row -> DocxContext [[Block]]
rowToBlocksList (Row cells) = mapM cellToBlocks cells

isBlockCodeContainer :: Container Block -> Bool
isBlockCodeContainer (Container f) | CodeBlock _ _ <- f [] = True
isBlockCodeContainer _ = False

isHeaderContainer :: Container Block -> Bool
isHeaderContainer (Container f) | Header _ _ _ <- f [] = True
isHeaderContainer _ = False

bodyPartToBlocks :: BodyPart -> DocxContext [Block]
bodyPartToBlocks (Paragraph pPr parparts)
  | any isBlockCodeContainer (parStyleToContainers pPr) =
    let
      otherConts = filter (not . isBlockCodeContainer) (parStyleToContainers pPr)
    in
     return $
     rebuild
     otherConts
     [CodeBlock ("", [], []) (concatMap parPartToString parparts)]
bodyPartToBlocks (Paragraph pPr parparts)
  | any isHeaderContainer (parStyleToContainers pPr) = do
    ils <- normalizeSpaces <$> local (\s -> s{docxInHeaderBlock = True})
                                (parPartsToInlines parparts)
    let (Container hdrFun) = head $ filter isHeaderContainer (parStyleToContainers pPr)
        Header n attr _ = hdrFun []
    hdr <- makeHeaderAnchor $ Header n attr ils
    return [hdr]
bodyPartToBlocks (Paragraph pPr parparts) = do
  ils <- parPartsToInlines parparts >>= (return . normalizeSpaces)
  case ils of
    [] -> return []
    _ -> do
      return $
       rebuild
       (parStyleToContainers pPr)
       [Para ils]
bodyPartToBlocks (ListItem pPr numId lvl levelInfo parparts) = do
  let
    kvs = case levelInfo of
      (_, fmt, txt, Just start) -> [ ("level", lvl)
                                   , ("num-id", numId)
                                   , ("format", fmt)
                                   , ("text", txt)
                                   , ("start", (show start))
                                   ]

      (_, fmt, txt, Nothing)    -> [ ("level", lvl)
                                   , ("num-id", numId)
                                   , ("format", fmt)
                                   , ("text", txt)
                                   ]
  blks <- bodyPartToBlocks (Paragraph pPr parparts)
  return $ [Div ("", ["list-item"], kvs) blks]
bodyPartToBlocks (Tbl _ _ _ []) =
  return [Para []]
bodyPartToBlocks (Tbl cap _ look (r:rs)) = do
  let caption = strToInlines cap
      (hdr, rows) = case firstRowFormatting look of
        True -> (Just r, rs)
        False -> (Nothing, r:rs)
  hdrCells <- case hdr of
    Just r' -> rowToBlocksList r'
    Nothing -> return []

  cells <- mapM rowToBlocksList rows

  let size = case null hdrCells of
        True -> length $ head cells
        False -> length $ hdrCells
      --
      -- The two following variables (horizontal column alignment and
      -- relative column widths) go to the default at the
      -- moment. Width information is in the TblGrid field of the Tbl,
      -- so should be possible. Alignment might be more difficult,
      -- since there doesn't seem to be a column entity in docx.
      alignments = replicate size AlignDefault
      widths = replicate size 0 :: [Double]

  return [Table caption alignments widths hdrCells cells]
bodyPartToBlocks (OMathPara _ maths) = do
  omaths <- mapM oMathToExps maths
  return [Para $
          map (\m -> Math DisplayMath (writeTeX m))
          omaths]

-- replace targets with generated anchors.
rewriteLink :: Inline -> DocxContext Inline
rewriteLink l@(Link ils ('#':target, title)) = do
  anchorMap <- gets docxAnchorMap
  return $ case M.lookup target anchorMap of
    Just newTarget -> (Link ils ('#':newTarget, title))
    Nothing        -> l
rewriteLink il = return il

bodyToOutput :: Body -> DocxContext (Meta, [Block], MediaBag)
bodyToOutput (Body bps) = do
  let (metabps, blkbps) = sepBodyParts bps
  meta <- bodyPartsToMeta metabps
  blks <- concatMapM bodyPartToBlocks blkbps >>=
          walkM rewriteLink
  mediaBag <- gets docxMediaBag
  return $ (meta,
            blocksToDefinitions $ blocksToBullets $ blks,
            mediaBag)

docxToOutput :: ReaderOptions -> Docx -> (Meta, [Block], MediaBag)
docxToOutput opts (Docx (Document _ body)) =
  let dEnv   = def { docxOptions  = opts} in
   evalDocxContext (bodyToOutput body) dEnv def


ilToCode :: Inline -> String
ilToCode (Str s) = s
ilToCode Space = " "
ilToCode _     = ""

isHeaderClass :: String -> Maybe Int
isHeaderClass s | Just s' <- stripPrefix "Heading" s =
  case reads s' :: [(Int, String)] of
    [] -> Nothing
    ((n, "") : []) -> Just n
    _       -> Nothing
isHeaderClass _ = Nothing
