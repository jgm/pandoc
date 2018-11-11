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
   Module      : Text.Pandoc.App.InputSettings
   Copyright   : Copyright (C) 2006-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Does a pandoc conversion based on command-line options.
-}
module Text.Pandoc.App.InputSettings
  ( InputSettings (..)
  , optToInputSettings
  ) where
import Prelude
import Control.Monad.Except (throwError)
import Text.Pandoc.App.FormatHeuristics (formatFromFilePaths)
import Text.Pandoc.App.Opt (Opt (..))
import Text.Pandoc.Class (PandocIO, readDataFile, readFileStrict)
import Text.Pandoc.Error (PandocError (PandocAppError))
import Text.Pandoc.IO.Input (InputSettings (..))
import Text.Pandoc.Options (ReaderOptions (..), def)
import Text.Pandoc.Readers (Reader (..), getReader)
import Text.Pandoc.Shared (isURI)

import qualified Data.Set as Set
import qualified Text.Pandoc.UTF8 as UTF8

optToInputSettings :: Opt -> Bool -> PandocIO InputSettings
optToInputSettings opts standalone = do
  let sources = case optInputFiles opts of
                     []  -> ["-"]
                     xs | optIgnoreArgs opts -> ["-"]
                        | otherwise  -> xs

  -- assign reader and writer based on options and filenames
  let readerName = case optReader opts of
                     Just f  -> f
                     Nothing -> formatFromFilePaths fallback sources
                       where fallback = if any isURI sources
                                           then "html"
                                           else "markdown"

  (reader, readerExts) <-
           case getReader readerName of
                Right (r, es) -> return (r :: Reader PandocIO, es)
                Left e   -> throwError $ PandocAppError e'
                  where e' = case readerName of
                               "pdf" -> e ++
                                 "\nPandoc can convert to PDF, but not from PDF."
                               "doc" -> e ++
                                 "\nPandoc can convert from DOCX, but not " ++
                                 "from DOC.\nTry using Word to save your " ++
                                 "DOC file as DOCX, and convert that with pandoc."
                               _ -> e

  abbrevs <- Set.fromList . filter (not . null) . lines <$>
             case optAbbreviations opts of
                  Nothing -> UTF8.toString <$> readDataFile "abbreviations"
                  Just f  -> UTF8.toString <$> readFileStrict f

  let readerOpts = def
        { readerStandalone = standalone
        , readerColumns = optColumns opts
        , readerTabStop = optTabStop opts
        , readerIndentedCodeClasses = optIndentedCodeClasses opts
        , readerDefaultImageExtension =
           optDefaultImageExtension opts
        , readerTrackChanges = optTrackChanges opts
        , readerAbbreviations = abbrevs
        , readerExtensions = readerExts
        , readerStripComments = optStripComments opts
        }

  let spacesPerTab = if optPreserveTabs opts ||
                        readerName == "t2t" ||
                        readerName == "man"
                     then Nothing
                     else Just (optTabStop opts)

  return $ InputSettings
    { inputSources = sources
    , inputReader = reader
    , inputReaderName = readerName
    , inputReaderOptions = readerOpts
    , inputSpacesPerTab = spacesPerTab
    , inputFileScope = optFileScope opts
    }
