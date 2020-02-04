{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Readers.RST
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion from CSV to a 'Pandoc' table.
-}
module Text.Pandoc.Readers.CSV ( readCSV ) where
import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.CSV (parseCSV, defaultCSVOptions)
import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Shared (crFilter)
import Text.Pandoc.Error
import Text.Pandoc.Options (ReaderOptions)
import Control.Monad.Except (throwError)

readCSV :: PandocMonad m
        => ReaderOptions -- ^ Reader options
        -> Text          -- ^ Text to parse (assuming @'\n'@ line endings)
        -> m Pandoc
readCSV _opts s =
  case parseCSV defaultCSVOptions (crFilter s) of
    Right (r:rs) -> return $ B.doc $ B.table capt (zip aligns widths) hdrs rows
       where capt = mempty
             numcols = length r
             toplain = B.plain . B.text . T.strip
             hdrs = map toplain r
             rows = map (map toplain) rs
             aligns = replicate numcols AlignDefault
             widths = replicate numcols 0
    Right []     -> return $ B.doc mempty
    Left e       -> throwError $ PandocParsecError s e

