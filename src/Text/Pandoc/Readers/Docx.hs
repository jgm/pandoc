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
import Text.Pandoc.Generic (bottomUp)
import Text.Pandoc.MIME (getMimeType)
import Text.Pandoc.UTF8 (toString)
import Text.Pandoc.Walk
import Text.Pandoc.Readers.Docx.Parse
import Text.Pandoc.Readers.Docx.Lists
import Text.Pandoc.Readers.Docx.Reducible
import Data.Maybe (mapMaybe, isJust, fromJust)
import Data.List (delete, isPrefixOf, (\\), intersect)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Base64 (encode)
import System.FilePath (combine)

readDocx :: ReaderOptions
         -> B.ByteString
         -> Pandoc
readDocx opts bytes =
  case archiveToDocx (toArchive bytes) of
    Just docx -> Pandoc nullMeta (docxToBlocks opts docx)
    Nothing   -> error $ "couldn't parse docx file"

spansToKeep :: [String]
spansToKeep = ["list-item", "Definition", "DefinitionTerm"] ++ codeSpans


-- This is empty, but we put it in for future-proofing.
divsToKeep :: [String]
divsToKeep = []

runStyleToContainers :: RunStyle -> [Container Inline]
runStyleToContainers rPr =
  let formatters = mapMaybe id
                 [ if isBold rPr then (Just Strong) else Nothing
                 , if isItalic rPr then (Just Emph) else Nothing
                 , if isSmallCaps rPr then (Just SmallCaps) else Nothing
                 , if isStrike rPr then (Just Strikeout) else Nothing
                 , if isSuperScript rPr then (Just Superscript) else Nothing
                 , if isSubScript rPr then (Just Subscript) else Nothing
                 , rStyle rPr >>=
                   (\s -> if s `elem` spansToKeep then Just s else Nothing) >>=
                   (\s -> Just $ Span ("", [s], []))
                 , underline rPr >>= (\f -> Just $ Span ("", [], [("underline", f)]))
                 ]
  in
   map Container formatters


divAttrToContainers :: [String] -> [(String, String)] -> [Container Block]
divAttrToContainers [] [] = []
divAttrToContainers (c:cs) _ | isJust (isHeaderClass c) =
  let n = fromJust (isHeaderClass c)
  in
   [(Container $ \blks ->
      Header n ("", delete ("Heading" ++ show n) cs, []) (blksToInlines blks))]
divAttrToContainers (c:_) _ | c `elem` codeDivs =
  [Container $ \blks -> CodeBlock ("", [], []) (concatMap blkToCode blks)]
divAttrToContainers (c:cs) kvs | c `elem` listParagraphDivs =
  let kvs' = filter (\(k,_) -> k /= "indent") kvs
  in
   (Container $ Div ("", [c], [])) : (divAttrToContainers cs kvs')
divAttrToContainers (c:cs) kvs | c `elem` blockQuoteDivs =
  (Container BlockQuote) : (divAttrToContainers (cs \\ blockQuoteDivs) kvs)
divAttrToContainers (c:cs) kvs | c `elem` divsToKeep =
  (Container $ Div ("", [c], [])) : (divAttrToContainers cs kvs)
divAttrToContainers (_:cs) kvs = divAttrToContainers cs kvs
divAttrToContainers [] (kv:kvs) | fst kv == "indent" =
  (Container BlockQuote) : divAttrToContainers [] kvs
divAttrToContainers [] (_:kvs) =
  divAttrToContainers [] kvs


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


inlineCodeContainer :: Container Inline -> Bool
inlineCodeContainer (Container f) = case f [] of
  Span (_, classes, _) _ -> (not . null) (classes `intersect` codeSpans)
  _         -> False
inlineCodeContainer _ = False

-- blockCodeContainer :: Container Block -> Bool
-- blockCodeContainer (Container f) = case f [] of
--   Div (ident, classes, kvs) _ -> (not . null) (classes `intersect` codeDivs)
--   _         -> False
-- blockCodeContainer _ = False

runToInlines :: ReaderOptions -> Docx -> Run -> [Inline]
runToInlines _ _ (Run rs runElems)
  | any inlineCodeContainer (runStyleToContainers rs) =
      rebuild (runStyleToContainers rs) $ [Str $ runElemsToString runElems]
  | otherwise =
      rebuild (runStyleToContainers rs) (concatMap runElemToInlines runElems)
runToInlines opts docx@(Docx _ notes _ _ _ ) (Footnote fnId) =
  case (getFootNote fnId notes) of
    Just bodyParts ->
      [Note (concatMap (bodyPartToBlocks opts docx) bodyParts)]
    Nothing        ->
      [Note []]
runToInlines opts docx@(Docx _ notes _ _ _) (Endnote fnId) =
  case (getEndNote fnId notes) of
    Just bodyParts ->
      [Note (concatMap (bodyPartToBlocks opts docx) bodyParts)]
    Nothing        ->
      [Note []]

parPartToInlines :: ReaderOptions -> Docx -> ParPart -> [Inline]
parPartToInlines opts docx (PlainRun r) = runToInlines opts docx r
parPartToInlines _ _ (BookMark _ anchor) | anchor `elem` dummyAnchors = []
parPartToInlines _ _ (BookMark _ anchor) = [Span (anchor, ["anchor"], []) []]
parPartToInlines _ (Docx _ _ _ rels _) (Drawing relid) =
  case lookupRelationship relid rels of
    Just target -> [Image [] (combine "word" target, "")]
    Nothing     -> [Image [] ("", "")]
parPartToInlines opts docx (InternalHyperLink anchor runs) =
  [Link (concatMap (runToInlines opts docx) runs) ('#' : anchor, "")]
parPartToInlines opts docx@(Docx _ _ _ rels _) (ExternalHyperLink relid runs) =
  case lookupRelationship relid rels of
    Just target ->
      [Link (concatMap (runToInlines opts docx) runs) (target, "")]
    Nothing ->
      [Link (concatMap (runToInlines opts docx) runs) ("", "")]

isAnchorSpan :: Inline -> Bool
isAnchorSpan (Span (ident, classes, kvs) ils) =
  (not . null) ident &&
  classes == ["anchor"] &&
  null kvs &&
  null ils
isAnchorSpan _ = False

dummyAnchors :: [String]
dummyAnchors = ["_GoBack"]

makeHeaderAnchors :: Block -> Block
makeHeaderAnchors h@(Header n (_, classes, kvs) ils) =
  case filter isAnchorSpan ils of
    []   -> h
    (x@(Span (ident, _, _) _) : xs) ->
      case ident `elem` dummyAnchors of
        True -> h
        False -> Header n (ident, classes, kvs) (ils \\ (x:xs))
    _ -> h
makeHeaderAnchors blk = blk

parPartsToInlines :: ReaderOptions -> Docx -> [ParPart] -> [Inline]
parPartsToInlines opts docx parparts =
  --
  -- We're going to skip data-uri's for now. It should be an option,
  -- not mandatory.
  --
  (if False -- TODO depend on option
      then walk (makeImagesSelfContained docx)
      else id) $
  -- bottomUp spanTrim $
  -- bottomUp spanCorrect $
  -- bottomUp spanReduce $
  reduceList $ concatMap (parPartToInlines opts docx) parparts

cellToBlocks :: ReaderOptions -> Docx -> Cell -> [Block]
cellToBlocks opts docx (Cell bps) = concatMap (bodyPartToBlocks opts docx) bps

rowToBlocksList :: ReaderOptions -> Docx -> Row -> [[Block]]
rowToBlocksList opts docx (Row cells) = map (cellToBlocks opts docx) cells

bodyPartToBlocks :: ReaderOptions -> Docx -> BodyPart -> [Block]
bodyPartToBlocks opts docx (Paragraph pPr parparts) =
  case parPartsToInlines opts docx parparts of
    [] ->
      []
    _ ->
      let parContents = parPartsToInlines opts docx parparts
          trimmedContents = reverse $ dropWhile (Space ==) $ reverse $ dropWhile (Space ==) parContents
      in
       rebuild
       (parStyleToContainers pPr)
       [Para trimmedContents]
bodyPartToBlocks opts docx@(Docx _ _ numbering _ _) (ListItem pPr numId lvl parparts) =
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
  in
   [Div
    ("", ["list-item"], kvs)
    (bodyPartToBlocks opts docx (Paragraph pPr parparts))]
bodyPartToBlocks _ _ (Tbl _ _ _ []) =
  [Para []]
bodyPartToBlocks opts docx (Tbl cap _ look (r:rs)) =
  let caption = strToInlines cap
      (hdr, rows) = case firstRowFormatting look of
        True -> (Just r, rs)
        False -> (Nothing, r:rs)
      hdrCells = case hdr of
        Just r' -> rowToBlocksList opts docx r'
        Nothing -> []
      cells = map (rowToBlocksList opts docx) rows

      size = case null hdrCells of
        True -> length $ head cells
        False -> length $ hdrCells
      --
      -- The two following variables (horizontal column alignment and
      -- relative column widths) go to the default at the
      -- moment. Width information is in the TblGrid field of the Tbl,
      -- so should be possible. Alignment might be more difficult,
      -- since there doesn't seem to be a column entity in docx.
      alignments = take size (repeat AlignDefault)
      widths = take size (repeat 0) :: [Double]
  in
   [Table caption alignments widths hdrCells cells]


makeImagesSelfContained :: Docx -> Inline -> Inline
makeImagesSelfContained (Docx _ _ _ _ media) i@(Image alt (uri, title)) =
  case lookup uri media of
    Just bs -> case getMimeType uri of
      Just mime ->  let data_uri =
                          "data:" ++ mime ++ ";base64," ++ toString (encode $ BS.concat $ B.toChunks bs)
                    in
                     Image alt (data_uri, title)
      Nothing  -> i
    Nothing -> i
makeImagesSelfContained _ inline = inline

bodyToBlocks :: ReaderOptions -> Docx -> Body -> [Block]
bodyToBlocks opts docx (Body bps) =
  map (makeHeaderAnchors) $
  bottomUp blocksToDefinitions $
  blocksToBullets $
  concatMap (bodyPartToBlocks opts docx) bps

docxToBlocks :: ReaderOptions -> Docx -> [Block]
docxToBlocks opts d@(Docx (Document _ body) _ _ _ _) = bodyToBlocks opts d body


ilToCode :: Inline -> String
ilToCode (Str s) = s
ilToCode _ = ""


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


blkToCode :: Block -> String
blkToCode (Para []) = ""
blkToCode (Para ((Code _ s):ils)) = s ++ (blkToCode (Para ils))
blkToCode (Para ((Span (_, classes, _) ils'): ils))
  | (not . null) (codeSpans `intersect` classes) =
    (init $ unlines $ map ilToCode ils') ++ (blkToCode (Para ils))
blkToCode _ = ""

