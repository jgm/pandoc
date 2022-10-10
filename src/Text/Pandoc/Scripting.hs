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
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Error (PandocError (PandocNoScriptingEngine))
import Text.Pandoc.Filter.Environment (Environment)
import Text.Pandoc.Readers (Reader)
import Text.Pandoc.Sources (Sources)
import Text.Pandoc.Writers (Writer)

-- | Structure to define a scripting engine.
data ScriptingEngine = ScriptingEngine
  { engineName :: Text   -- ^ Name of the engine.

  , engineApplyFilter :: forall m. (PandocMonad m, MonadIO m)
                      => Environment -> [String] -> FilePath
                      -> Pandoc -> m Pandoc
    -- ^ Use the scripting engine to run a filter.

  , engineReadCustom :: forall m. (PandocMonad m, MonadIO m)
                     => FilePath -> m (Reader m)
    -- ^ Function to parse input into a 'Pandoc' document.

  , engineWriteCustom :: forall m. (PandocMonad m, MonadIO m)
                      => FilePath -> m (Writer m)
    -- ^ Invoke the given script file to convert to any custom format.
  }

noEngine :: ScriptingEngine
noEngine = ScriptingEngine
  { engineName = "none"
  , engineApplyFilter = \_env _args _fp _doc ->
      throwError PandocNoScriptingEngine
  , engineReadCustom = \_fp ->
      throwError PandocNoScriptingEngine
  , engineWriteCustom = \_fp ->
      throwError PandocNoScriptingEngine
  }
