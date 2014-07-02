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
import Text.Pandoc.MIME (getMimeType)
import Text.Pandoc.UTF8 (toString)
import Text.Pandoc.Walk
import Text.Pandoc.Readers.Docx.Parse
import Text.Pandoc.Readers.Docx.Lists
import Text.Pandoc.Readers.Docx.Reducible
import Text.Pandoc.Readers.Docx.TexChar
import Text.Pandoc.Shared
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (delete, isPrefixOf, (\\), intercalate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Base64 (encode)
import System.FilePath (combine)
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Text.Printf (printf)

readDocx :: ReaderOptions
         -> B.ByteString
         -> Pandoc
readDocx opts bytes =
  case archiveToDocx (toArchive bytes) of
    Just docx -> Pandoc nullMeta (docxToBlocks opts docx)
    Nothing   -> error $ "couldn't parse docx file"

data DState = DState { docxAnchorMap :: M.Map String String
                     , docxInTexSubscript :: Bool }

data DEnv = DEnv { docxOptions  :: ReaderOptions
                 , docxDocument :: Docx}

type DocxContext = ReaderT DEnv (State DState)

updateDState :: (DState -> DState) -> DocxContext ()
updateDState f = do
  st <- get
  put $ f st

withDState :: DState -> DocxContext a -> DocxContext a
withDState ds dctx = do
  ds' <- get
  updateDState (\_ -> ds)
  dctx' <- dctx
  put ds'
  return dctx'

evalDocxContext :: DocxContext a -> DEnv -> DState -> a
evalDocxContext ctx env st = evalState (runReaderT ctx env) st

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

-- This is empty, but we put it in for future-proofing.
spansToKeep :: [String]
spansToKeep = []

divsToKeep :: [String]
divsToKeep = ["list-item", "Definition", "DefinitionTerm"]

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
                   , underline rPr >>= (\f -> Just $ Span ("", [], [("underline", f)]))
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
runToInlines (Footnote fnId) = do
  (Docx _ notes _ _ _ ) <- asks docxDocument
  case (getFootNote fnId notes) of
    Just bodyParts -> do
      blks <- concatMapM bodyPartToBlocks bodyParts
      return $ [Note blks]
    Nothing        -> return [Note []]
runToInlines (Endnote fnId) = do
  (Docx _ notes _ _ _ ) <- asks docxDocument
  case (getEndNote fnId notes) of
    Just bodyParts -> do
      blks <- concatMapM bodyPartToBlocks bodyParts
      return $ [Note blks]
    Nothing        -> return [Note []]

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
    -- Get the anchor map.
    anchorMap <- gets docxAnchorMap
    -- Check to see if the id is already in there. Rewrite if
    -- necessary. This will have the possible effect of rewriting
    -- user-defined anchor links. However, since these are not defined
    -- in pandoc, it seems like a necessary evil to avoid an extra
    -- pass.
    let newAnchor = case anchor `elem` (M.elems anchorMap) of
          True -> uniqueIdent [Str anchor] (M.elems anchorMap)
          False -> anchor
    updateDState $ \s -> s { docxAnchorMap = M.insert anchor newAnchor anchorMap}
    return [Span (anchor, ["anchor"], []) []]
parPartToInlines (Drawing relid) = do
  (Docx _ _ _ rels _) <- asks docxDocument
  return $ case lookupRelationship relid rels of
    Just target -> [Image [] (combine "word" target, "")]
    Nothing     -> [Image [] ("", "")]
parPartToInlines (InternalHyperLink anchor runs) = do
  ils <- concatMapM runToInlines runs
  return [Link ils ('#' : anchor, "")]
parPartToInlines (ExternalHyperLink relid runs) = do
  (Docx _ _ _ rels _) <- asks docxDocument
  rs <- concatMapM runToInlines runs
  return $ case lookupRelationship relid rels of
    Just target ->
      [Link rs (target, "")]
    Nothing ->
      [Link rs ("", "")]
parPartToInlines (PlainOMath omath) = do
  s <- oMathToTexString omath
  return [Math InlineMath s]

oMathToTexString :: OMath -> DocxContext String
oMathToTexString (OMath omathElems) = do
  ss <- mapM oMathElemToTexString omathElems
  return $ intercalate " " ss
oMathElemToTexString :: OMathElem -> DocxContext String
oMathElemToTexString (Accent style base) | Just c <- accentChar style = do
  baseString <- baseToTexString base
  return $ case lookupTexChar c of
    s@('\\' : _) -> printf "%s{%s}" s baseString
    _            -> printf "\\acute{%s}" baseString -- we default.
oMathElemToTexString (Accent _ base) =
    baseToTexString base >>= (\s -> return $ printf "\\acute{%s}" s)
oMathElemToTexString (Bar style base) = do
  baseString <- baseToTexString base
  return $ case barPos style of
    Top    -> printf "\\overline{%s}" baseString
    Bottom -> printf "\\underline{%s}" baseString
oMathElemToTexString (Box base) = baseToTexString base
oMathElemToTexString (BorderBox base) = 
  baseToTexString base >>= (\s -> return $ printf "\\boxed{%s}" s)
oMathElemToTexString (Delimiter dPr bases) = do
  let beg = fromMaybe '(' (delimBegChar dPr)
      end = fromMaybe ')' (delimEndChar dPr)
      sep = fromMaybe '|' (delimSepChar dPr)
      left = "\\left" ++ lookupTexChar beg
      right = "\\right" ++ lookupTexChar end
      mid  = "\\middle" ++ lookupTexChar sep
  baseStrings <- mapM baseToTexString bases
  return $ printf "%s %s %s"
    left
    (intercalate (" " ++ mid ++ " ") baseStrings)
    right
oMathElemToTexString (EquationArray bases) = do
  baseStrings <- mapM baseToTexString bases
  inSub <- gets docxInTexSubscript
  return $
    if inSub
    then
      printf "\\substack{%s}" (intercalate "\\\\ " baseStrings)
    else
      printf
      "\\begin{aligned}\n%s\n\\end{aligned}"
      (intercalate "\\\\\n" baseStrings)
oMathElemToTexString (Fraction num denom) = do
  numString  <- concatMapM oMathElemToTexString num
  denString  <- concatMapM oMathElemToTexString denom
  return $ printf "\\frac{%s}{%s}" numString denString
oMathElemToTexString (Function fname base) = do
  fnameString <- concatMapM oMathElemToTexString fname
  baseString  <- baseToTexString base
  return $ printf "%s %s" fnameString baseString
oMathElemToTexString (Group style base)
  | Just c <- groupChr style
  , grouper <- lookupTexChar c
  , notElem grouper ["\\overbrace", "\\underbrace"]
    = do
      baseString <- baseToTexString base
      return $ case groupPos style of
        Just Top -> printf "\\overset{%s}{%s}" grouper baseString
        _        -> printf "\\underset{%s}{%s}" grouper baseString
oMathElemToTexString (Group style base) = do
  baseString <- baseToTexString base
  return $ case groupPos style of
    Just Top -> printf "\\overbrace{%s}" baseString
    _        -> printf "\\underbrace{%s}" baseString
oMathElemToTexString (LowerLimit base limElems) = do
  baseString <- baseToTexString base
  lim <- concatMapM oMathElemToTexString limElems
    --  we want to make sure to replace the `\rightarrow` with `\to`
  let arrowToTo :: String -> String
      arrowToTo "" = ""
      arrowToTo s | "\\rightarrow" `isPrefixOf` s =
        "\\to" ++ (arrowToTo $ drop (length "\\rightarrow") s)
      arrowToTo (c:cs) = c : arrowToTo cs
      lim' = arrowToTo lim
  return $ case baseString of
    "lim" -> printf "\\lim_{%s}" lim'
    "max" -> printf "\\max_{%s}" lim'
    "min" -> printf "\\min_{%s}" lim'
    _     -> printf "\\operatorname*{%s}_{%s}" baseString lim'
oMathElemToTexString (UpperLimit base limElems) = do
  baseString <- baseToTexString base
  lim <- concatMapM oMathElemToTexString limElems
    --  we want to make sure to replace the `\rightarrow` with `\to`
  let arrowToTo :: String -> String
      arrowToTo "" = ""
      arrowToTo s | "\\rightarrow" `isPrefixOf` s =
        "\\to" ++ (arrowToTo $ drop (length "\\rightarrow") s)
      arrowToTo (c:cs) = c : arrowToTo cs
      lim' = arrowToTo lim
  return $ case baseString of
    "lim" -> printf "\\lim^{%s}" lim'
    "max" -> printf "\\max^{%s}" lim'
    "min" -> printf "\\min^{%s}" lim'
    _     -> printf "\\operatorname*{%s}^{%s}" baseString lim'
oMathElemToTexString (Matrix bases) = do
  let rowString :: [Base] -> DocxContext String
      rowString bs = liftM (intercalate " & ") (mapM baseToTexString bs)

  s <- liftM (intercalate " \\\\\n")(mapM rowString bases)
  return $ printf "\\begin{matrix}\n%s\n\\end{matrix}" s
oMathElemToTexString (NAry style sub sup base) | Just c <- nAryChar style = do
  ds <- gets (\s -> s{docxInTexSubscript = True})
  subString  <- withDState ds $ concatMapM oMathElemToTexString sub
  supString  <- concatMapM oMathElemToTexString sup
  baseString <- baseToTexString base
  return $ case M.lookup c uniconvMap of
    Just s@('\\':_) -> printf "%s_{%s}^{%s}{%s}"
                       s subString supString baseString
    _               -> printf "\\operatorname*{%s}_{%s}^{%s}{%s}"
                       [c] subString supString baseString
oMathElemToTexString (NAry _ sub sup base) = do
  subString  <- concatMapM oMathElemToTexString sub
  supString  <- concatMapM oMathElemToTexString sup
  baseString <- baseToTexString base
  return $ printf "\\int_{%s}^{%s}{%s}"
    subString supString baseString
oMathElemToTexString (Radical degree base) = do
  degString  <- concatMapM oMathElemToTexString degree
  baseString <- baseToTexString base
  return $ case trim degString of
    "" -> printf "\\sqrt{%s}" baseString
    _  -> printf "\\sqrt[%s]{%s}" degString baseString
oMathElemToTexString (PreSubSuper sub sup base) = do
  subString  <- concatMapM oMathElemToTexString sub
  supString  <- concatMapM oMathElemToTexString sup
  baseString <- baseToTexString base
  return $ printf "_{%s}^{%s}%s" subString supString baseString
oMathElemToTexString (Sub base sub) = do
  baseString <- baseToTexString base
  subString  <- concatMapM oMathElemToTexString sub
  return $ printf "%s_{%s}" baseString subString
oMathElemToTexString (SubSuper base sub sup) = do
  baseString <- baseToTexString base
  subString  <- concatMapM oMathElemToTexString sub
  supString  <- concatMapM oMathElemToTexString sup
  return $ printf "%s_{%s}^{%s}" baseString subString supString
oMathElemToTexString (Super base sup) = do
  baseString <- baseToTexString base
  supString  <- concatMapM oMathElemToTexString sup
  return $ printf "%s^{%s}" baseString supString
oMathElemToTexString (OMathRun _ run) = return $ stringToTex $ runToString run
oMathElemToTexString _ = return "[NOT IMPLEMENTED]"

baseToTexString :: Base -> DocxContext String
baseToTexString (Base mathElems) =
  concatMapM oMathElemToTexString mathElems
  

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
      updateDState $ \s -> s {docxAnchorMap = M.insert ident newIdent hdrIDMap}
      return $ Header n (newIdent, classes, kvs) (ils \\ (x:xs))
-- Otherwise we just give it a name, and register that name (associate
-- it with itself.)
makeHeaderAnchor (Header n (_, classes, kvs) ils) =
  do
    hdrIDMap <- gets docxAnchorMap
    let newIdent = uniqueIdent ils (M.elems hdrIDMap)
    updateDState $ \s -> s {docxAnchorMap = M.insert newIdent newIdent hdrIDMap}
    return $ Header n (newIdent, classes, kvs) ils
makeHeaderAnchor blk = return blk


parPartsToInlines :: [ParPart] -> DocxContext [Inline]
parPartsToInlines parparts = do
  ils <- concatMapM parPartToInlines parparts >>=
         --  TODO: Option for self-containted images
         (if False then (walkM makeImagesSelfContained) else return)
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
    ils <- parPartsToInlines parparts >>= (return . normalizeSpaces)
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
bodyPartToBlocks (ListItem pPr numId lvl parparts) = do
  (Docx _ _ numbering _ _) <- asks docxDocument
  let
    kvs = case lookupLevel numId lvl numbering of
      Just (_, fmt, txt, Just start) -> [ ("level", lvl)
                                        , ("num-id", numId)
                                        , ("format", fmt)
                                        , ("text", txt)
                                        , ("start", (show start))
                                        ]

      Just (_, fmt, txt, Nothing)    -> [ ("level", lvl)
                                        , ("num-id", numId)
                                        , ("format", fmt)
                                        , ("text", txt)
                                        ]
      Nothing                        -> []
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
  omaths <- mapM oMathToTexString maths
  return [Para $ map (\s -> Math DisplayMath s) omaths]


-- replace targets with generated anchors.
rewriteLink :: Inline -> DocxContext Inline
rewriteLink l@(Link ils ('#':target, title)) = do
  anchorMap <- gets docxAnchorMap
  return $ case M.lookup target anchorMap of
    Just newTarget -> (Link ils ('#':newTarget, title))
    Nothing        -> l
rewriteLink il = return il

makeImagesSelfContained :: Inline -> DocxContext Inline
makeImagesSelfContained i@(Image alt (uri, title)) = do
  (Docx _ _ _ _ media) <- asks docxDocument
  return $ case lookup uri media of
    Just bs ->
      case getMimeType uri of
        Just mime ->
          let data_uri = "data:" ++ mime ++ ";base64," ++
                         toString (encode $ BS.concat $ B.toChunks bs)
          in
           Image alt (data_uri, title)
        Nothing  -> i
    Nothing -> i
makeImagesSelfContained inline = return inline

bodyToBlocks :: Body -> DocxContext [Block]
bodyToBlocks (Body bps) = do
  blks <- concatMapM bodyPartToBlocks bps >>=
          walkM rewriteLink
  return $
    blocksToDefinitions $
    blocksToBullets $ blks

docxToBlocks :: ReaderOptions -> Docx -> [Block]
docxToBlocks opts d@(Docx (Document _ body) _ _ _ _) =
  let dState = DState { docxAnchorMap = M.empty
                      , docxInTexSubscript = False}
      dEnv   = DEnv { docxOptions  = opts
                    , docxDocument = d}
  in
   evalDocxContext (bodyToBlocks body) dEnv dState

ilToCode :: Inline -> String
ilToCode (Str s) = s
ilToCode Space = " "
ilToCode _     = ""

isHeaderClass :: String -> Maybe Int
isHeaderClass s | "Heading" `isPrefixOf` s =
  case reads (drop (length "Heading") s) :: [(Int, String)] of
    [] -> Nothing
    ((n, "") : []) -> Just n
    _       -> Nothing
isHeaderClass _ = Nothing
