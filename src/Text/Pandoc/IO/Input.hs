{-# LANGUAGE NoImplicitPrelude #-}
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
   Module      : Text.Pandoc.IO.Input
   Copyright   : Copyright (C) 2006-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Read from the file system into a pandoc document.
-}
module Text.Pandoc.IO.Input
  ( InputSettings (..)
  , readInput
  ) where

import Prelude
import Control.Monad ((>=>))
import Control.Monad.Trans (MonadIO (liftIO))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (URI (..), parseURI)
import Text.Pandoc.Class (PandocIO, openURL)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options (ReaderOptions (..))
import Text.Pandoc.Readers (Reader (..))
import Text.Pandoc.Shared (tabFilter, uriPathToPath)
import qualified Text.Pandoc.UTF8 as UTF8

-- | Settings specifying how and which input should be processed.
data InputSettings = InputSettings
  { inputReader :: Reader PandocIO
  , inputReaderName :: String
  , inputReaderOptions :: ReaderOptions
  , inputSources :: [FilePath]
  , inputSpacesPerTab :: Maybe Int
  , inputFileScope :: Bool
  }

-- | Read all input into a pandoc document.
readInput :: InputSettings -> PandocIO Pandoc
readInput inputSettings = do
  let sources = inputSources inputSettings
  let readerName = inputReaderName inputSettings
  let readerOpts = inputReaderOptions inputSettings

  let convertTabs :: Text -> Text
      convertTabs = tabFilter (fromMaybe 0 $ inputSpacesPerTab inputSettings)

      readSources :: [FilePath] -> PandocIO Text
      readSources srcs = convertTabs . T.intercalate (T.pack "\n") <$>
                              mapM readSource srcs

  case inputReader inputSettings of
    TextReader r
      | inputFileScope inputSettings || readerName == "json" ->
                          mconcat <$> mapM (readSource >=> r readerOpts) sources
      | otherwise      -> readSources sources >>= r readerOpts
    ByteStringReader r -> mconcat <$> mapM (readFile' >=> r readerOpts) sources


-- | Read text from a resource.
readSource :: FilePath -> PandocIO Text
readSource "-" = liftIO (UTF8.toText <$> BS.getContents)
readSource src = case parseURI src of
                      Just u | uriScheme u `elem` ["http:","https:"] ->
                                 readURI src
                             | uriScheme u == "file:" ->
                                 liftIO $ UTF8.toText <$>
                                    BS.readFile (uriPathToPath $ uriPath u)
                      _       -> liftIO $ UTF8.toText <$>
                                    BS.readFile src

readURI :: FilePath -> PandocIO Text
readURI src = UTF8.toText . fst <$> openURL src

readFile' :: MonadIO m => FilePath -> m B.ByteString
readFile' "-" = liftIO B.getContents
readFile' f   = liftIO $ B.readFile f
