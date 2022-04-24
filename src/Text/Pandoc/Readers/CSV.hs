{-# LANGUAGE FlexibleContexts    #-}

{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Readers.CSV
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion from CSV or TSV to a 'Pandoc' table.
-}
module Text.Pandoc.Readers.CSV (
  readCSV,
  readTSV
) where
import qualified Data.Text as T
import Text.Pandoc.CSV (parseCSV, defaultCSVOptions, CSVOptions(..))
import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Error
import Text.Pandoc.Sources (ToSources(..), sourcesToText)
import Text.Pandoc.Options (ReaderOptions)
import Control.Monad.Except (throwError)
import Data.Text (Text)

readCSV :: (PandocMonad m, ToSources a)
        => ReaderOptions -- ^ Reader options
        -> a
        -> m Pandoc
readCSV _opts s = do
  readCSVWith defaultCSVOptions $ sourcesToText $ toSources s

readTSV :: (PandocMonad m, ToSources a)
        => ReaderOptions -- ^ Reader options
        -> a
        -> m Pandoc
readTSV _opts s = do
  readCSVWith tsvOpts $ sourcesToText $ toSources s
 where
  tsvOpts = CSVOptions{
    csvDelim = '\t',
    csvQuote = Nothing,
    csvKeepSpace = False,
    csvEscape = Nothing }

readCSVWith :: PandocMonad m
            => CSVOptions
            -> Text
            -> m Pandoc
readCSVWith csvopts txt = do
  case parseCSV csvopts txt of
    Right (r:rs) -> return $ B.doc $ B.table capt
                                             (zip aligns widths)
                                             (TableHead nullAttr hdrs)
                                             [TableBody nullAttr 0 [] rows]
                                             (TableFoot nullAttr [])
       where capt = B.emptyCaption
             numcols = length r
             toplain = B.simpleCell . B.plain . B.text . T.strip
             toRow = Row nullAttr . map toplain
             toHeaderRow l = [toRow l | not (null l)]
             hdrs = toHeaderRow r
             rows = map toRow rs
             aligns = replicate numcols AlignDefault
             widths = replicate numcols ColWidthDefault
    Right []     -> return $ B.doc mempty
    Left e       -> throwError $ PandocParsecError (toSources [("",txt)]) e
