{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ImpredicativeTypes   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{- |
Module      : Text.Pandoc.Scripting
Copyright   : © 2022 Albert Krewinkel
License     : GPL-2.0-or-later
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Central data structure for scripting engines.
-}
module Text.Pandoc.Scripting
  ( ScriptingEngine (..)
  , noEngine
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Error (PandocError (PandocNoScriptingEngine))
import Text.Pandoc.Filter.Environment (Environment)
import Text.Pandoc.Format (ExtensionsDiff)
import Text.Pandoc.Templates (Template)
import Text.Pandoc.Readers (Reader)
import Text.Pandoc.Writers (Writer)

-- | Structure to define a scripting engine.
data ScriptingEngine = ScriptingEngine
  { engineName :: Text   -- ^ Name of the engine.

  , engineApplyFilter :: forall m. (PandocMonad m, MonadIO m)
                      => Environment -> [String] -> FilePath
                      -> Pandoc -> m Pandoc
    -- ^ Use the scripting engine to run a filter.

  , engineReadCustom :: forall m. (PandocMonad m, MonadIO m)
                     => FilePath -> ExtensionsDiff -> m (Reader m)
    -- ^ Use the scripting engine to generate a custom reader
    -- based on the script in the 'FilePath'. Pass 'ExtensionsDiff'
    -- to the reader to process.

  , engineWriteCustom :: forall m. (PandocMonad m, MonadIO m)
                      => FilePath -> ExtensionsDiff
                      -> m (Writer m, m (Template Text))
    -- ^ Use the scripting engine to generate a custom writer
    -- based on the script in the 'FilePath'. Pass 'ExtensionsDiff'
    -- to the writer to process.
    -- The return value includes both a writer and an action to retrieve
    -- a default template (defined in the writer).
  }

noEngine :: ScriptingEngine
noEngine = ScriptingEngine
  { engineName = "none"
  , engineApplyFilter = \_env _args _fp _doc ->
      throwError PandocNoScriptingEngine
  , engineReadCustom = \_ ->
      throwError PandocNoScriptingEngine
  , engineWriteCustom = \_ ->
      throwError PandocNoScriptingEngine
  }
