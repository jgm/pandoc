{-
Copyright (C) 2008 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Pluigns
   Copyright   : Copyright (C) 2008 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Support for plugins.
-}

module Text.Pandoc.Plugins (getPlugin)
where

import Language.Haskell.Interpreter
import Text.Pandoc
import Control.Monad (unless, liftM)
import Control.Monad.Error (throwError)
import Data.List (isInfixOf)

-- | Returns the function named @transform@ in the specified
-- module. The module may be identified either by module name
-- or by path name.  The @transform@ function should have type
-- @a -> a@ or @a -> IO a@, where @a@ is an instance of 'Data':
-- for example, @Pandoc -> Pandoc@, @Inline -> IO Inline@,
-- @Block -> Block@, or @[Inline] -> IO [Inline]@.
getPlugin :: String -> IO (Pandoc -> IO Pandoc)
getPlugin modsrc = do
  res <- runInterpreter (evaluatePlugin modsrc)
  case res of
       Right func             -> return func
       Left (WontCompile xs)  -> error $ "WontCompile error for plugin '" ++ modsrc ++ "'\n" ++ unlines (map errMsg xs)
       Left (NotAllowed x)    -> error $ "NotAllowed error for plugin '" ++ modsrc ++ "'\n" ++ x
       Left (UnknownError x)  -> error $ "UnknownError for plugin '" ++ modsrc ++ "'\n" ++ x
       Left (GhcException x)  -> error $ "GhcException for plugin '" ++ modsrc ++ "'\n" ++ x

evaluatePlugin :: String -> Interpreter (Pandoc -> IO Pandoc)
evaluatePlugin modsrc = do
  set [installedModulesInScope := False]
  loadModules [modsrc]
  modnames <- getLoadedModules
  setTopLevelModules modnames
  setImports ["Prelude", "Text.Pandoc", "Text.Pandoc.Definition"]
  exports <- liftM concat $ mapM getModuleExports modnames
  unless ((Fun "transform") `elem` exports) $
    throwError $ UnknownError $ "The plugin module must define a function 'transform'."
  transformType <- typeOf "transform"
  if "-> IO" `isInfixOf` transformType
     then interpret "processInM transform" (as :: Pandoc -> IO Pandoc)
     else interpret "return . (processIn transform)" (as :: Pandoc -> IO Pandoc)
