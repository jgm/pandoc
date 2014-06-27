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
import Data.Maybe (mapMaybe, isJust, fromJust)
import Data.List (delete, isPrefixOf, (\\))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Base64 (encode)
import System.FilePath (combine)
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State

readDocx :: ReaderOptions
         -> B.ByteString
         -> Pandoc
readDocx opts bytes =
  case archiveToDocx (toArchive bytes) of
    Just docx -> Pandoc nullMeta (docxToBlocks opts docx)
    Nothing   -> error $ "couldn't parse docx file"


data DState = DState { docxHdrLinks :: M.Map String String }

data DEnv = DEnv { docxOptions  :: ReaderOptions
                 , docxDocument :: Docx}

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


divAttrToContainers :: [String] -> [(String, String)] -> [Container Block]
divAttrToContainers (c:cs) _ | isJust (isHeaderClass c) =
  let n = fromJust (isHeaderClass c)
  in
   [(Container $ \blks ->
      makeHeaderAnchor $
      Header n ("", delete ("Heading" ++ show n) cs, []) (blksToInlines blks))]
divAttrToContainers (c:cs) kvs | c `elem` divsToKeep =
  (Container $ Div ("", [c], [])) : (divAttrToContainers cs kvs)
divAttrToContainers (c:cs) kvs | c `elem` codeDivs =
  -- This is a bit of a cludge. We make the codeblock from the raw
  -- parparts in bodyPartToBlocks. But we need something to match against.
  (Container $ \_ -> CodeBlock ("", [], []) "") : (divAttrToContainers cs kvs)
divAttrToContainers (c:cs) kvs | c `elem` listParagraphDivs =
  let kvs' = filter (\(k,_) -> k /= "indent") kvs
  in
   (Container $ Div ("", [c], [])) : (divAttrToContainers cs kvs')
divAttrToContainers (c:cs) kvs | c `elem` blockQuoteDivs =
  (Container BlockQuote) : (divAttrToContainers (cs \\ blockQuoteDivs) kvs)
divAttrToContainers (_:cs) kvs = divAttrToContainers cs kvs
divAttrToContainers [] kvs | isJust (lookup "indent" kvs) =
  let kvs' = filter (\(k,_) -> k /= "indent") kvs
  in
   case fromJust (lookup "indent" kvs) of
     "0"       -> divAttrToContainers [] kvs'
     ('-' : _) -> divAttrToContainers [] kvs'
     _         -> (Container BlockQuote) : divAttrToContainers [] kvs'
divAttrToContainers _ _ = []


parStyleToContainers :: ParagraphStyle -> [Container Block]
parStyleToContainers pPr =
  let classes = pStyle pPr
      kvs = case indent pPr of
        Just n -> [("indent", show n)]
        Nothing -> []
  in
   divAttrToContainers classes kvs
  

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
parPartToInlines (BookMark _ anchor) = return [Span (anchor, ["anchor"], []) []]
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

isAnchorSpan :: Inline -> Bool
isAnchorSpan (Span (ident, classes, kvs) ils) =
  (not . null) ident &&
  classes == ["anchor"] &&
  null kvs &&
  null ils
isAnchorSpan _ = False

dummyAnchors :: [String]
dummyAnchors = ["_GoBack"]

makeHeaderAnchor :: Block -> Block
makeHeaderAnchor h@(Header n (_, classes, kvs) ils) =
  case filter isAnchorSpan ils of
    []   -> h
    (x@(Span (ident, _, _) _) : xs) ->
      case ident `elem` dummyAnchors of
        True -> h
        False -> Header n (ident, classes, kvs) (ils \\ (x:xs))
    _ -> h
makeHeaderAnchor blk = blk

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

blockCodeContainer :: Container Block -> Bool
blockCodeContainer (Container f) = case f [] of
  CodeBlock _ _ -> True
  _             -> False
blockCodeContainer _ = False

bodyPartToBlocks :: BodyPart -> DocxContext [Block]
bodyPartToBlocks (Paragraph pPr parparts)
  | any blockCodeContainer (parStyleToContainers pPr) =
    let
      otherConts = filter (not . blockCodeContainer) (parStyleToContainers pPr)
    in
     return $ 
     rebuild
     otherConts
     [CodeBlock ("", [], []) (concatMap parPartToString parparts)]
bodyPartToBlocks (Paragraph pPr parparts) = do
  ils <- parPartsToInlines parparts
  case ils of
    [] -> return []
    _ -> do
      parContents <- parPartsToInlines parparts
      let trimmedContents = reverse $
                            dropWhile (Space ==) $
                            reverse $
                            dropWhile (Space ==) parContents
      return $
       rebuild
       (parStyleToContainers pPr)
       [Para trimmedContents]
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
  blks <- concatMapM bodyPartToBlocks bps
  return $
    blocksToDefinitions $
    blocksToBullets $ blks

docxToBlocks :: ReaderOptions -> Docx -> [Block]
docxToBlocks opts d@(Docx (Document _ body) _ _ _ _) =
  let dState = DState { docxHdrLinks = M.empty }
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

blksToInlines :: [Block] -> [Inline]
blksToInlines (Para ils : _) = ils
blksToInlines (Plain ils : _) = ils
blksToInlines _ = []
