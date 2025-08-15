{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Readers.Typst.Parsing
  ( P,
    PState(..),
    defaultPState,
    pTok,
    pWithContents,
    ignored,
    getField,
    chunks,
  )
where
import Control.Monad (MonadPlus)
import Control.Monad.Reader (lift)
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Text (Text)
import Text.Parsec
    ( ParsecT, getInput, setInput, tokenPrim )
import Typst.Types
    ( Identifier, Content(Elt), FromVal(..), Val(VNone) )
import Text.Pandoc.Class.PandocMonad ( PandocMonad, report )
import Text.Pandoc.Logging (LogMessage(..))

newtype PState = PState
        { sLabels :: [Text]}
        deriving (Show)

defaultPState :: PState
defaultPState =
  PState
  { sLabels = [] }

type P m a = ParsecT [Content] PState m a
-- state tracks a list of labels in the document

pTok :: PandocMonad m => (Content -> Bool) -> P m Content
pTok f = tokenPrim show showPos match
  where
    showPos _oldpos (Elt _ (Just pos) _) _ = pos
    showPos oldpos _ _ = oldpos
    match x | f x = Just x
    match _ = Nothing

ignored :: PandocMonad m => Text -> P m ()
ignored msg = lift $ report $ IgnoredElement msg

pWithContents :: PandocMonad m => P m a -> Seq Content -> P m a
pWithContents pa cs = do
  inp <- getInput
  setInput $ F.toList cs
  res <- pa
  setInput inp
  pure res

-- | Get field value from element, defaulting to VNone.
getField ::
  (MonadFail m, MonadPlus m, FromVal a) =>
  Identifier ->
  M.Map Identifier Val ->
  m a
getField name fields = fromVal $ fromMaybe VNone $ M.lookup name fields

-- | Split a list into chunks of a given size. The last chunk may be smaller.
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)
