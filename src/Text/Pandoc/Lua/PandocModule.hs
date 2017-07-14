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
import Scripting.Lua (LuaState, call, push, pushhsfunction, rawset)
import Text.Pandoc.Class hiding (readDataFile)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options (ReaderOptions(readerExtensions))
import Text.Pandoc.Lua.Compat (loadstring)
import Text.Pandoc.Lua.StackInstances ()
import Text.Pandoc.Readers (Reader (..), getReader)
import Text.Pandoc.Shared (readDataFile)

-- | Push the "pandoc" on the lua stack.
pushPandocModule :: Maybe FilePath -> LuaState -> IO ()
pushPandocModule datadir lua = do
  script <- pandocModuleScript datadir
  status <- loadstring lua script "pandoc.lua"
  unless (status /= 0) $ call lua 0 1
  push lua "__read"
  pushhsfunction lua read_doc
  rawset lua (-3)

-- | Get the string representation of the pandoc module
pandocModuleScript :: Maybe FilePath -> IO String
pandocModuleScript datadir = unpack <$> readDataFile datadir "pandoc.lua"

read_doc :: String -> String -> IO (Either String Pandoc)
read_doc formatSpec content = do
  case getReader formatSpec of
    Left  s      -> return $ Left s
    Right (reader, es) ->
      case reader of
        TextReader r -> do
          res <- runIO $ r def{ readerExtensions = es } (pack content)
          case res of
            Left s   -> return . Left $ show s
            Right pd -> return $ Right pd
        _  -> return $ Left "Only string formats are supported at the moment."

