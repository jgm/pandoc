{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright (C) 2006-2018 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.IO.Output
   Copyright   : Copyright (C) 2006-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Write pandoc document to its output destination.
-}
module Text.Pandoc.IO.Output
  ( OutputSettings (..)
  , writeOutput
  ) where

import Prelude
import qualified Control.Exception as E
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified System.IO as IO (Newline (..), nativeNewline)
import Text.Pandoc.Class (PandocIO)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Error (PandocError (PandocPDFError))
import Text.Pandoc.IO.Options (LineEnding (..))
import Text.Pandoc.Options (WriterOptions)
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.SelfContained (makeSelfContained)
import Text.Pandoc.Writers (Writer (ByteStringWriter, TextWriter))
import qualified Text.Pandoc.UTF8 as UTF8

-- | Settings specifying how document output should be produced.
data OutputSettings = OutputSettings
  { outputFile :: Maybe FilePath
  , outputFormat :: String
  , outputLineEnding :: LineEnding
  , outputSelfContained :: Bool
  , outputWriter :: Writer PandocIO
  , outputWriterName :: String
  , outputWriterOptions :: WriterOptions
  , outputPdfProgram :: Maybe String
  , outputPdfEngineArgs :: [String]
  }

-- | Write a pandoc document to its target destination.
writeOutput :: Bool -> OutputSettings -> Pandoc -> PandocIO ()
writeOutput standalone outputSettings doc = do
  let outputFileName = fromMaybe "-" $ outputFile outputSettings
  let format = outputFormat outputSettings
  let writer = outputWriter outputSettings
  let writerOptions = outputWriterOptions outputSettings

  let eol = case outputLineEnding outputSettings of
                 CRLF   -> IO.CRLF
                 LF     -> IO.LF
                 Native -> IO.nativeNewline

  case writer of
    ByteStringWriter f -> f writerOptions doc >>= writeFnBinary outputFileName
    TextWriter f -> case outputPdfProgram outputSettings of
      Just pdfProg -> do
              res <- makePDF pdfProg (outputPdfEngineArgs outputSettings) f
                      writerOptions doc
              case res of
                   Right pdf -> writeFnBinary outputFileName pdf
                   Left err' -> liftIO $
                     E.throwIO $ PandocPDFError $
                                   TL.unpack (TE.decodeUtf8With TE.lenientDecode err')

      Nothing -> do
              let htmlFormat = format `elem`
                    ["html","html4","html5","s5","slidy",
                     "slideous","dzslides","revealjs"]
                  addNl = if standalone
                             then id
                             else (<> T.singleton '\n')
              output <- addNl <$> f writerOptions doc
              writerFn eol outputFileName =<<
                if outputSelfContained outputSettings && htmlFormat
                   -- TODO not maximally efficient; change type
                   -- of makeSelfContained so it works w/ Text
                   then T.pack <$> makeSelfContained (T.unpack output)
                   else return output

writeFnBinary :: MonadIO m => FilePath -> B.ByteString -> m ()
writeFnBinary "-" = liftIO . B.putStr
writeFnBinary f   = liftIO . B.writeFile (UTF8.encodePath f)

writerFn :: MonadIO m => IO.Newline -> FilePath -> Text -> m ()
-- TODO this implementation isn't maximally efficient:
writerFn eol "-" = liftIO . UTF8.putStrWith eol . T.unpack
writerFn eol f   = liftIO . UTF8.writeFileWith eol f . T.unpack
