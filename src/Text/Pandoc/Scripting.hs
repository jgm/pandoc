{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ImpredicativeTypes   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{- |
Module      : Text.Pandoc.Scripting
Copyright   : © 2022-2023 Albert Krewinkel
License     : GPL-2.0-or-later
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Central data structure for scripting engines.
-}
module Text.Pandoc.Scripting
  ( ScriptingEngine (..)
  , CustomComponents(..)
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
import Text.Pandoc.Format (ExtensionsConfig)
import Text.Pandoc.Readers (Reader)
import Text.Pandoc.Writers (Writer)

-- | A component of a custom reader/writer: a custom reader,
-- a custom writer, a template for a custom writer, or a specification
-- of the extensions used by a script and their default values.
-- Note that a single script can contain all of these.
data CustomComponents m =
  CustomComponents
  { customReader :: Maybe (Reader m)
  , customWriter :: Maybe (Writer m)
  , customTemplate :: Maybe Text
  , customExtensions :: Maybe ExtensionsConfig
  }

-- | Structure to define a scripting engine.
data ScriptingEngine = ScriptingEngine
  { engineName :: Text   -- ^ Name of the engine.

  , engineApplyFilter :: forall m. (PandocMonad m, MonadIO m)
                      => Environment -> [String] -> FilePath
                      -> Pandoc -> m Pandoc
    -- ^ Use the scripting engine to run a filter.

  , engineLoadCustom :: forall m. (PandocMonad m, MonadIO m)
                     => FilePath -> m (CustomComponents m)
    -- ^ Function to load a custom reader/writer from a script.
  }

noEngine :: ScriptingEngine
noEngine = ScriptingEngine
  { engineName = "none"
  , engineApplyFilter = \_env _args _fp _doc ->
      throwError PandocNoScriptingEngine
  , engineLoadCustom = \_fp ->
      throwError PandocNoScriptingEngine
  }
