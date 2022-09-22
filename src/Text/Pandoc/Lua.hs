{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{- |
   Module      : Text.Pandoc.Lua
   Copyright   : Copyright © 2017-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Running pandoc Lua filters.
-}
module Text.Pandoc.Lua
  ( -- * High-level functions
    applyFilter
  , readCustom
  , writeCustom
  -- * Run scripts as program
  , runScript
  -- * Low-level functions
  , Global(..)
  , setGlobals
  , runLua
  ) where

import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Text (unpack)
import Foreign.Ptr (nullPtr)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Pandoc.Class (runIOorExplode)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Lua.Filter (applyFilter)
import Text.Pandoc.Lua.Global (Global (..), setGlobals)
import Text.Pandoc.Lua.Init (runLua)
import Text.Pandoc.Lua.Reader (readCustom)
import Text.Pandoc.Lua.Writer (writeCustom)
import Text.Pandoc.Lua.Orphans ()
import Text.Pandoc.Shared (pandocVersion)
import qualified Lua.Auxiliary as Lua
import qualified Lua.Constants as Lua
import qualified HsLua as Lua
import qualified HsLua.Core.Types as Lua
import qualified Text.Pandoc.UTF8 as UTF8

-- | Uses the first command line argument as the name of a script file
-- and tries to run that script in Lua. Falls back to stdin if no file
-- is given. Any remaining args are passed to Lua via the global table
-- @arg@.
runScript :: IO ()
runScript = do
  rawArgs <- getArgs
  let (actions, args, errs) = getOpt RequireOrder luaOptions rawArgs
  when (not $ null errs) . ioError . userError $
    concat errs ++
    usageInfo "Usage: pandoc-lua [options] [script [args]]" luaOptions

  let (script, arg) = splitAt 1 args
  opts <- foldl' (>>=) (return defaultLuaOpts) actions
  luaResult <- runIOorExplode . runLua $ do
    Lua.pushList Lua.pushString arg
    Lua.setglobal "arg"

    forM_ (reverse $ optExecute opts) $ \case
      ExecuteCode stat -> do
        status <- Lua.dostringTrace stat
        when (status /= Lua.OK)
          Lua.throwErrorAsException
      RequireModule g mod' -> do
        Lua.getglobal "require"
        Lua.pushName mod'
        status <- Lua.pcallTrace 1 1
        if status == Lua.OK
          then Lua.setglobal g
          else Lua.throwErrorAsException

    result <- case script of
      [fp] -> Lua.dofileTrace fp
      _    -> do
        -- load script from stdin
        l <- Lua.state
        Lua.liftIO (Lua.luaL_loadfile l nullPtr) >>= \case
          Lua.LUA_OK -> Lua.pcallTrace 0 Lua.multret
          s          -> pure $ Lua.toStatus s

    when (result /= Lua.OK)
      Lua.throwErrorAsException
  handleError luaResult

-- | Code to execute on startup.
data LuaCode = ExecuteCode ByteString | RequireModule Lua.Name Lua.Name

-- | Lua runner command line options.
data LuaOpt = LuaOpt
  { optNoEnv       :: Bool -- ^ Ignore environment variables
  , optInteractive :: Bool -- ^ Interactive
  , optWarnings    :: Bool -- ^ Whether warnings are enabled
  , optExecute     :: [LuaCode] -- ^ code to execute
  }

defaultLuaOpts :: LuaOpt
defaultLuaOpts = LuaOpt
  { optNoEnv = False
  , optInteractive = False
  , optWarnings = False
  , optExecute = mempty
  }

-- | Lua command line options.
luaOptions :: [OptDescr (LuaOpt -> IO LuaOpt)]
luaOptions =
  [ Option "e" []
    (flip ReqArg "stat" $ \stat opt -> return $
        let code = ExecuteCode $ UTF8.fromString stat
        in opt{ optExecute = code:(optExecute opt) })
    "execute string 'stat'"

  , Option "i" []
    (NoArg $ \opt -> do
        hPutStrLn stderr "[WARNING] Flag `-i` is not supported yet."
        return opt { optInteractive = True })
    "interactive mode -- currently not supported"

  , Option "l" []
    (flip ReqArg "mod" $ \mod' opt -> return $
      let toName = Lua.Name . UTF8.fromString
          code = case break (== '=') mod' of
            (glb, ('=':m))  -> RequireModule (toName glb) (toName m)
            (glb, _       ) -> RequireModule (toName glb) (toName glb)
        in opt{ optExecute = code:(optExecute opt) })
    (unlines
     [ "require library 'mod' into global 'mod';"
     , "if 'mod' has the pattern 'g=module', then"
     , "require library 'module' into global 'g'"
     ])

  , Option "v" []
    (NoArg $ \_opt -> do
        Lua.run @Lua.Exception $ do
          Lua.openlibs
          Lua.dostring "print(_VERSION)"
        putStrLn $ "Embedded in pandoc " ++ unpack pandocVersion
        exitSuccess)
    "show version information"

  , Option "E" []
    (NoArg $ \opt -> do
        hPutStrLn stderr "[WARNING] Flag `-E` is not supported yet."
        return opt { optNoEnv = True })
    "ignore environment variables -- currently not supported"

  , Option "W" []
    (NoArg $ \opt -> do
        hPutStrLn stderr "[WARNING] Flag `-W` is not supported yet."
        return opt { optWarnings = True })
    "turn warnings on -- currently not supported"
  ]
