module Text.Pandoc.Parsing.References (
  LogUnused(..),
  logReferenceIssues ) where

import Control.Monad (forM_, unless, when)
import Data.Function (on)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty ((:|)), groupBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Logging (RefType(..), LogMessage(..))
import Text.Pandoc.Parsing.Capabilities (HasLogMessages, logMessage)
import Text.Pandoc.Parsing.State (ParserState(..), Located(..), Key(..))
import Text.Parsec (ParsecT, Stream, getState)

import qualified Data.Map as M
import qualified Data.Set as Set

data LogUnused = LogUnused | DoNotLogUnused deriving Eq

-- | Based on usages recorded in ParserState, reports all unused link,
-- footnote, or substitution definitions, as well as all broken
-- references to these.
--
-- For the old list representation of footnotes, also reports
-- duplicate notes.
logReferenceIssues :: (Stream s m a, PandocMonad m)
                   => LogUnused -> ParsecT s ParserState m ()
logReferenceIssues logUnused = do
  st <- getState

  -- log broken link references and unused definitions
  let allKeys = Set.union (M.keysSet (stateKeys st))
                          (M.keysSet (stateHeaderKeys st))
  logBrokenReferences LinkRef allKeys (stateKeyUsages st)
  when (logUnused == LogUnused) $
    logUnusedReferences LinkRef (stateKeys st) (stateKeyUsages st)

  -- log broken footnote references and unused definitions
  let allNotes = Set.union (Set.fromList (map fst (stateNotes st)))
                           (M.keysSet (stateNotes' st))
  logBrokenReferences FootnoteRef allNotes (stateNoteUsages st)
  when (logUnused == LogUnused) $ do
    logUnusedReferences FootnoteRef (M.fromList (stateNotes st)) (stateNoteUsages st)
    logUnusedReferences FootnoteRef (stateNotes' st) (stateNoteUsages st)

  -- log duplicate footnote references
  let notesListGroupedByKey =
        groupBy ((==) `on` fst) $ sortBy (comparing fst) $ stateNotes st
  forM_ notesListGroupedByKey $ \sameKeyNotes ->
    case sameKeyNotes of
      _ :| [] -> return ()
      duplicates ->
        forM_ duplicates $ \(n, v) ->
          logMessage $ DuplicateReferenceDefinition FootnoteRef n (location v)

logBrokenReferences :: (Ord k, KeyToText k,
                        Stream s m a, HasLogMessages st, PandocMonad m)
                    => RefType
                    -> Set.Set k
                    -> [Located k]
                    -> ParsecT s st m ()
logBrokenReferences refType definedNames usages =
  forM_ usages $ \(Located loc name) ->
    unless (name `Set.member` definedNames ) $
      logMessage $ ReferenceNotFound refType (keyToText name) loc

logUnusedReferences :: (Ord k, KeyToText k,
                        Stream s m a, HasLogMessages st, PandocMonad m)
                    => RefType
                    -> M.Map k (Located v)
                    -> [Located k]
                    -> ParsecT s st m ()
logUnusedReferences refType definitions usages = do
  let usageSet = Set.fromList $ map unLocated usages
  forM_ (M.toList definitions) $ \(name, Located loc _) ->
    unless (name `Set.member` usageSet) $
      logMessage $ UnusedReferenceDefinition refType (keyToText name) loc

class KeyToText a where
  keyToText :: a -> Text

instance KeyToText Key where
  keyToText (Key x) = x

instance KeyToText Text where
  keyToText x = x
