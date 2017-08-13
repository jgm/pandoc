{-
Copyright © 2017 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

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
   Module      : Text.Pandoc.Lua.PandocModule
   Copyright   : Copyright © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc module for lua.
-}
module Text.Pandoc.Lua.PandocModule ( pushPandocModule ) where

import Control.Monad (unless)
import Data.ByteString.Char8 (unpack)
import Data.Default (Default (..))
import Data.Text (pack)
import Foreign.Lua (Lua, Status (OK), NumResults, call, loadstring, liftIO,
                    push, pushHaskellFunction, rawset)
import Text.Pandoc.Class (readDataFile, runIO, runIOorExplode, setUserDataDir)
import Text.Pandoc.Options (ReaderOptions(readerExtensions))
import Text.Pandoc.Lua.StackInstances ()
import Text.Pandoc.Readers (Reader (..), getReader)

-- | Push the "pandoc" on the lua stack.
pushPandocModule :: Maybe FilePath -> Lua ()
pushPandocModule datadir = do
  script <- liftIO (pandocModuleScript datadir)
  status <- loadstring script
  unless (status /= OK) $ call 0 1
  push "__read"
  pushHaskellFunction readDoc
  rawset (-3)

-- | Get the string representation of the pandoc module
pandocModuleScript :: Maybe FilePath -> IO String
pandocModuleScript datadir = unpack <$>
  runIOorExplode (setUserDataDir datadir >> readDataFile "pandoc.lua")

readDoc :: String -> String -> Lua NumResults
readDoc formatSpec content = do
  case getReader formatSpec of
    Left  s      -> push s -- Unknown reader
    Right (reader, es) ->
      case reader of
        TextReader r -> do
          res <- liftIO $ runIO $ r def{ readerExtensions = es } (pack content)
          case res of
            Left s   -> push $ show s -- error while reading
            Right pd -> push pd       -- success, push Pandoc
        _  -> push "Only string formats are supported at the moment."
  return 1

