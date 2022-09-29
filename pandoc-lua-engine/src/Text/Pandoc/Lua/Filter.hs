{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{- |
Module      : Text.Pandoc.Lua.Filter
Copyright   : © 2012-2022 John MacFarlane,
              © 2017-2022 Albert Krewinkel
License     : GNU GPL, version 2 or above
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
Stability   : alpha

Types and functions for running Lua filters.
-}
module Text.Pandoc.Lua.Filter
  ( applyFilter
  ) where
import Control.Monad ((>=>), (<$!>))
import HsLua as Lua
import Text.Pandoc.Definition
import Text.Pandoc.Filter (Environment (..))
import Text.Pandoc.Lua.ErrorConversion ()
import Text.Pandoc.Lua.Marshal.AST
import Text.Pandoc.Lua.Marshal.Filter
import Text.Pandoc.Lua.Global (Global (..), setGlobals)
import Text.Pandoc.Lua.Init (runLua)
import Control.Exception (throw)
import qualified Data.Text as T
import Text.Pandoc.Class (PandocMonad)
import Control.Monad.Trans (MonadIO)
import Text.Pandoc.Error (PandocError (PandocFilterError, PandocLuaError))

-- | Transform document using the filter defined in the given file.
runFilterFile :: FilePath -> Pandoc -> LuaE PandocError Pandoc
runFilterFile filterPath doc = do
  oldtop <- gettop
  stat <- dofileTrace filterPath
  if stat /= Lua.OK
    then throwErrorAsException
    else do
      newtop <- gettop
      -- Use the returned filters, or the implicitly defined global
      -- filter if nothing was returned.
      luaFilters <- forcePeek $
        if newtop - oldtop >= 1
        then peekList peekFilter top
        else (:[]) <$!> (liftLua pushglobaltable *> peekFilter top)
      settop oldtop
      runAll luaFilters doc

runAll :: [Filter] -> Pandoc -> LuaE PandocError Pandoc
runAll = foldr ((>=>) . applyFully) return

-- | Run the Lua filter in @filterPath@ for a transformation to the
-- target format (first element in args). Pandoc uses Lua init files to
-- setup the Lua interpreter.
applyFilter :: (PandocMonad m, MonadIO m)
            => Environment
            -> [String]
            -> FilePath
            -> Pandoc
            -> m Pandoc
applyFilter fenv args fp doc = do
  let globals = [ FORMAT $ case args of
                    x:_ -> T.pack x
                    _   -> ""
                , PANDOC_READER_OPTIONS (envReaderOptions fenv)
                , PANDOC_WRITER_OPTIONS (envWriterOptions fenv)
                , PANDOC_SCRIPT_FILE fp
                ]
  runLua >=> forceResult fp $ do
    setGlobals globals
    runFilterFile fp doc

forceResult :: (PandocMonad m, MonadIO m)
            => FilePath -> Either PandocError Pandoc -> m Pandoc
forceResult fp eitherResult = case eitherResult of
  Right x  -> return x
  Left err -> throw . PandocFilterError (T.pack fp) $ case err of
    PandocLuaError msg -> msg
    _                  -> T.pack $ show err
