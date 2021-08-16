{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Citeproc.CslJson
  ( cslJsonToReferences )
where

import Citeproc.CslJson
import Citeproc.Types
import Control.Monad.Identity (runIdentity)
import Data.Aeson (eitherDecodeStrict')
import Data.ByteString (ByteString)
import Text.Pandoc.Builder as B
import Data.Text (Text)

fromCslJson :: CslJson Text -> Inlines
fromCslJson (CslText t) = B.text t
fromCslJson CslEmpty = mempty
fromCslJson (CslConcat x y) = fromCslJson x <> fromCslJson y
fromCslJson (CslQuoted x) = B.doubleQuoted (fromCslJson x)
fromCslJson (CslItalic x) = B.emph (fromCslJson x)
fromCslJson (CslNormal x) = fromCslJson x  -- TODO?
fromCslJson (CslBold x) = B.strong (fromCslJson x)
fromCslJson (CslUnderline x) = B.underline (fromCslJson x)
fromCslJson (CslNoDecoration x) =
  B.spanWith ("",["nodecoration"],[]) (fromCslJson x)
fromCslJson (CslSmallCaps x) = B.smallcaps (fromCslJson x)
fromCslJson (CslBaseline x) = fromCslJson x
fromCslJson (CslSub x) = B.subscript (fromCslJson x)
fromCslJson (CslSup x) = B.superscript (fromCslJson x)
fromCslJson (CslNoCase x) = B.spanWith ("",["nocase"],[]) (fromCslJson x)
fromCslJson (CslDiv t x) = B.spanWith ("",["csl-" <> t],[]) (fromCslJson x)
fromCslJson (CslLink u x) = B.link u "" (fromCslJson x)

cslJsonToReferences :: ByteString -> Either String [Reference Inlines]
cslJsonToReferences raw =
  case eitherDecodeStrict' raw of
    Left e        -> Left e
    Right cslrefs -> Right $
      map (runIdentity . traverse (return . fromCslJson)) cslrefs
