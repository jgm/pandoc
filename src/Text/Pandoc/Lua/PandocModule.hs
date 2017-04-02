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
{-# LANGUAGE FlexibleInstances #-}
{- |
   Module      : Text.Pandoc.Lua.PandocModule
   Copyright   : Copyright © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc module for lua.
-}
module Text.Pandoc.Lua.PandocModule ( pushPandocModule ) where

import Data.ByteString.Char8 ( unpack )
import Data.Default ( Default(..) )
import Scripting.Lua ( LuaState, call, newtable, push, pushhsfunction, rawset)
import Text.Pandoc.Class hiding ( readDataFile )
import Text.Pandoc.Definition ( Pandoc(..), Block(..) )
import Text.Pandoc.Lua.Compat ( loadstring )
import Text.Pandoc.Lua.StackInstances ()
import Text.Pandoc.Readers.DocBook ( readDocBook )
import Text.Pandoc.Readers.HTML ( readHtml )
import Text.Pandoc.Readers.LaTeX ( readLaTeX )
import Text.Pandoc.Readers.Native ( readNative )
import Text.Pandoc.Readers.Markdown ( readMarkdown )
import Text.Pandoc.Readers.MediaWiki ( readMediaWiki )
import Text.Pandoc.Readers.Org ( readOrg )
import Text.Pandoc.Readers.RST ( readRST )
import Text.Pandoc.Readers.Textile ( readTextile )
import Text.Pandoc.Readers.TWiki ( readTWiki )
import Text.Pandoc.Readers.Txt2Tags ( readTxt2Tags )
import Text.Pandoc.Shared ( readDataFile )

-- | Push the "pandoc" on the lua stack.
pushPandocModule :: LuaState -> IO ()
pushPandocModule lua = do
  script <- pandocModuleScript
  status <- loadstring lua script "pandoc.lua"
  if (status /= 0)
    then return ()
    else do
      call lua 0 1
  push lua "reader"
  pushReadersModule lua readers
  rawset lua (-3)

readers :: [(String, String -> PandocIO Pandoc)]
readers =
  [ ("docbook", readDocBook def)
  , ("html", readHtml def)
  , ("latex", readLaTeX def)
  , ("native", readNative def)
  , ("markdown", readMarkdown def)
  , ("mediawiki", readMediaWiki def)
  , ("org", readOrg def)
  , ("rst", readRST def)
  , ("textile", readTextile def)
  , ("twiki", readTWiki def)
  , ("txt2tags", readTxt2Tags def)
  ]

-- | Get the string representation of the pandoc module
pandocModuleScript :: IO String
pandocModuleScript = unpack <$> readDataFile Nothing "pandoc.lua"

-- | Push a lua table containing readers of the given formats.
pushReadersModule :: LuaState
                 -> [(String, String -> PandocIO Pandoc)]
                 -> IO ()
pushReadersModule lua readerFns = do
  newtable lua
  mapM_ (uncurry $ addReaderTable) readerFns
 where
   addReaderTable :: String
                  -> (String -> PandocIO Pandoc)
                  -> IO ()
   addReaderTable formatName readerFn = do
     let readDoc :: String -> IO Pandoc
         readDoc s = do
           res <- runIO $ readerFn s
           case res of
             (Left x) -> error (show x)
             (Right x) -> return x
     let readBlock :: String -> IO Block
         readBlock s = do
           Pandoc _ blks <- readDoc s
           return $ case blks of
             x:_ -> x
             _   -> Null
     -- Push table containing all functions for this format
     push lua formatName
     newtable lua
     -- set document-reading function
     push lua "read_doc"
     pushhsfunction lua readDoc
     rawset lua (-3)
     -- set block-reading function
     push lua "read_block"
     pushhsfunction lua readBlock
     rawset lua (-3)
     -- store table in readers module
     rawset lua (-3)
