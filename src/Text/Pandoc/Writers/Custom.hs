{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{- |
   Module      : Text.Pandoc.Writers.Custom
   Copyright   : 2012-2022 John MacFarlane,
   License     : GNU GPL, version 2 or above
   Maintainer  : John MacFarlane <jgm@berkeley.edu>

Conversion of 'Pandoc' documents to custom markup using
a Lua writer.
-}
module Text.Pandoc.Writers.Custom ( writeCustom ) where
import Control.Exception
import Control.Monad ((<=<))
import Data.Text (Text)
import HsLua
import Control.Monad.IO.Class (MonadIO)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition (Pandoc (..))
import Text.Pandoc.Lua (Global (..), runLua, setGlobals)
import Text.Pandoc.Options (WriterOptions)

import qualified Text.Pandoc.Lua.Writer.Classic as Classic

-- | Convert Pandoc to custom markup.
writeCustom :: (PandocMonad m, MonadIO m)
            => FilePath -> WriterOptions -> Pandoc -> m Text
writeCustom luaFile opts doc = either throw pure <=< runLua $ do
  setGlobals [ PANDOC_DOCUMENT doc
             , PANDOC_SCRIPT_FILE luaFile
             , PANDOC_WRITER_OPTIONS opts
             ]
  dofileTrace luaFile >>= \case
    OK -> pure ()
    _  -> throwErrorAsException
  -- Most classic writers contain code that throws an error if a global
  -- is not present. This would break our check for the existence of a
  -- "Writer" function. We resort to raw access for that reason, but
  -- could also catch the error instead.
  let rawgetglobal x = do
        pushglobaltable
        pushName x
        rawget (nth 2) <* remove (nth 2) -- remove global table

  rawgetglobal "Writer" >>= \case
    TypeNil -> do
      pop 1  -- remove nil
      Classic.runCustom opts doc
    _       -> do
      -- Writer on top of the stack. Call it with document and writer
      -- options as arguments.
      push doc
      push opts
      callTrace 2 1
      forcePeek $ peekText top
