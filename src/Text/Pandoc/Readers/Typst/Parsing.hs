{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Readers.Typst.Parsing
  ( P,
    pTok,
    pWithContents,
    warn,
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
    ( ParsecT, getInput, getState, setInput, tokenPrim )
import Typst.Types
    ( Identifier, Content(Elt), FromVal(..), Val(VNone) )


type P m a = ParsecT [Content] (Text -> m ()) m a

pTok :: Monad m => (Content -> Bool) -> P m Content
pTok f = tokenPrim show showPos match
  where
    showPos _oldpos (Elt _ (Just pos) _) _ = pos
    showPos oldpos _ _ = oldpos
    match x | f x = Just x
    match _ = Nothing

warn :: Monad m => Text -> P m ()
warn msg = do
  warn' <- getState
  lift $ warn' msg

pWithContents :: Monad m => P m a -> Seq Content -> P m a
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
