{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
Module      : Text.Pandoc.Writers.Docx.Table
Copyright   : Copyright (C) 2012-2021 John MacFarlane
License     : GNU GPL, version 2 or above
Maintainer  : John MacFarlane <jgm@berkeley.edu>

Conversion of table blocks to docx.
-}
module Text.Pandoc.Writers.Docx.Table
  ( tableToOpenXML
  ) where

import Control.Monad.State.Strict
import Data.Text (Text)
import Text.Pandoc.Definition
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Writers.Docx.Types
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Printf (printf)
import Text.Pandoc.Writers.OOXML
import Text.Pandoc.XML.Light as XML
import qualified Data.Text as T

tableToOpenXML :: PandocMonad m
               => ([Block] -> WS m [Content])
               -> Caption
               -> [ColSpec]
               -> TableHead
               -> [TableBody]
               -> TableFoot
               -> WS m [Content]
tableToOpenXML blocksToOpenXML blkCapt specs thead tbody tfoot = do
  let (caption, aligns, widths, headers, rows) =
        toLegacyTable blkCapt specs thead tbody tfoot
  setFirstPara
  modify $ \s -> s { stInTable = True }
  let captionStr = stringify caption
  caption' <- if null caption
                 then return []
                 else withParaPropM (pStyleM "Table Caption")
                      $ blocksToOpenXML [Para caption]
  let alignmentFor al = mknode "w:jc" [("w:val",alignmentToString al)] ()
  -- Table cells require a <w:p> element, even an empty one!
  -- Not in the spec but in Word 2007, 2010. See #4953. And
  -- apparently the last element must be a <w:p>, see #6983.
  let cellToOpenXML (al, cell) = do
        es <- withParaProp (alignmentFor al) $ blocksToOpenXML cell
        return $
          case reverse (onlyElems es) of
            b:e:_ | qName (elName b) == "bookmarkEnd"
                  , qName (elName e) == "p" -> es
            e:_   | qName (elName e) == "p" -> es
            _ -> es ++ [Elem $ mknode "w:p" [] ()]
  headers' <- mapM cellToOpenXML $ zip aligns headers
  rows' <- mapM (mapM cellToOpenXML . zip aligns) rows
  compactStyle <- pStyleM "Compact"
  let emptyCell' = [Elem $ mknode "w:p" [] [mknode "w:pPr" [] [compactStyle]]]
  let mkcell contents = mknode "w:tc" []
                            $ if null contents
                                 then emptyCell'
                                 else contents
  let mkrow cells =
         mknode "w:tr" [] $
           map mkcell cells
  let textwidth = 7920  -- 5.5 in in twips, 1/20 pt
  let fullrow = 5000 -- 100% specified in pct
  let (rowwidth :: Int) = round $ fullrow * sum widths
  let mkgridcol w = mknode "w:gridCol"
                       [("w:w", tshow (floor (textwidth * w) :: Integer))] ()
  let hasHeader = not $ all null headers
  modify $ \s -> s { stInTable = False }
  -- for compatibility with Word <= 2007, we include a val with a bitmask
  -- 0×0020  Apply first row conditional formatting
  -- 0×0040  Apply last row conditional formatting
  -- 0×0080  Apply first column conditional formatting
  -- 0×0100  Apply last column conditional formatting
  -- 0×0200  Do not apply row banding conditional formatting
  -- 0×0400  Do not apply column banding conditional formattin
  let tblLookVal :: Int
      tblLookVal = if hasHeader then 0x20 else 0
  return $
    caption' ++
    [Elem $
     mknode "w:tbl" []
      ( mknode "w:tblPr" []
        (   mknode "w:tblStyle" [("w:val","Table")] () :
            mknode "w:tblW" (if all (== 0) widths
                              then [("w:type", "auto"), ("w:w", "0")]
                              else [("w:type", "pct"), ("w:w", tshow rowwidth)])
                            () :
            mknode "w:tblLook" [("w:firstRow",if hasHeader then "1" else "0")
                               ,("w:lastRow","0")
                               ,("w:firstColumn","0")
                               ,("w:lastColumn","0")
                               ,("w:noHBand","0")
                               ,("w:noVBand","0")
                               ,("w:val", T.pack $ printf "%04x" tblLookVal)
                               ] () :
          [ mknode "w:tblCaption" [("w:val", captionStr)] ()
          | not (null caption) ] )
      : mknode "w:tblGrid" []
        (if all (==0) widths
            then []
            else map mkgridcol widths)
      : [ mkrow headers' | hasHeader ] ++
      map mkrow rows'
      )]

alignmentToString :: Alignment -> Text
alignmentToString = \case
  AlignLeft    -> "left"
  AlignRight   -> "right"
  AlignCenter  -> "center"
  AlignDefault -> "left"
