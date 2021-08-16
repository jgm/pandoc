{-# LANGUAGE OverloadedStrings   #-}
{- |
Module      : Text.Pandoc.Writers.Docx
Copyright   : Copyright (C) 2012-2021 John MacFarlane
License     : GNU GPL, version 2 or above
Maintainer  : John MacFarlane <jgm@berkeley.edu>

Conversion of table blocks to docx.
-}
module Text.Pandoc.Writers.Docx.Types
  ( EnvProps (..)
  , WriterEnv (..)
  , defaultWriterEnv
  , WriterState (..)
  , defaultWriterState
  , WS
  , ListMarker (..)
  , listMarkerToId
  , pStyleM
  , isStyle
  , setFirstPara
  , withParaProp
  , withParaPropM
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Text (Text)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.Writers.Docx.StyleMap
import Text.Pandoc.Writers.OOXML
import Text.Pandoc.XML.Light as XML
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T

data ListMarker = NoMarker
                | BulletMarker
                | NumberMarker ListNumberStyle ListNumberDelim Int
                deriving (Show, Read, Eq, Ord)

listMarkerToId :: ListMarker -> Text
listMarkerToId NoMarker = "990"
listMarkerToId BulletMarker = "991"
listMarkerToId (NumberMarker sty delim n) = T.pack $
  '9' : '9' : styNum : delimNum : show n
  where styNum = case sty of
                      DefaultStyle -> '2'
                      Example      -> '3'
                      Decimal      -> '4'
                      LowerRoman   -> '5'
                      UpperRoman   -> '6'
                      LowerAlpha   -> '7'
                      UpperAlpha   -> '8'
        delimNum = case delim of
                      DefaultDelim -> '0'
                      Period       -> '1'
                      OneParen     -> '2'
                      TwoParens    -> '3'


data EnvProps = EnvProps{ styleElement  :: Maybe Element
                        , otherElements :: [Element]
                        }

instance Semigroup EnvProps where
  EnvProps s es <> EnvProps s' es' = EnvProps (s <|> s') (es ++ es')

instance Monoid EnvProps where
  mempty = EnvProps Nothing []
  mappend = (<>)

data WriterEnv = WriterEnv
  { envTextProperties :: EnvProps
  , envParaProperties :: EnvProps
  , envRTL            :: Bool
  , envListLevel      :: Int
  , envListNumId      :: Int
  , envInDel          :: Bool
  , envChangesAuthor  :: Text
  , envChangesDate    :: Text
  , envPrintWidth     :: Integer
  }

defaultWriterEnv :: WriterEnv
defaultWriterEnv = WriterEnv
  { envTextProperties = mempty
  , envParaProperties = mempty
  , envRTL = False
  , envListLevel = -1
  , envListNumId = 1
  , envInDel = False
  , envChangesAuthor  = "unknown"
  , envChangesDate    = "1969-12-31T19:00:00Z"
  , envPrintWidth     = 1
  }


data WriterState = WriterState{
         stFootnotes      :: [Element]
       , stComments       :: [([(Text, Text)], [Inline])]
       , stSectionIds     :: Set.Set Text
       , stExternalLinks  :: M.Map Text Text
       , stImages         :: M.Map FilePath (String, String, Maybe MimeType, B.ByteString)
       , stLists          :: [ListMarker]
       , stInsId          :: Int
       , stDelId          :: Int
       , stStyleMaps      :: StyleMaps
       , stFirstPara      :: Bool
       , stInTable        :: Bool
       , stInList         :: Bool
       , stTocTitle       :: [Inline]
       , stDynamicParaProps :: Set.Set ParaStyleName
       , stDynamicTextProps :: Set.Set CharStyleName
       , stCurId          :: Int
       , stNextFigureNum  :: Int
       , stNextTableNum   :: Int
       }

defaultWriterState :: WriterState
defaultWriterState = WriterState{
        stFootnotes      = defaultFootnotes
      , stComments       = []
      , stSectionIds     = Set.empty
      , stExternalLinks  = M.empty
      , stImages         = M.empty
      , stLists          = [NoMarker]
      , stInsId          = 1
      , stDelId          = 1
      , stStyleMaps      = StyleMaps M.empty M.empty
      , stFirstPara      = False
      , stInTable        = False
      , stInList         = False
      , stTocTitle       = [Str "Table of Contents"]
      , stDynamicParaProps = Set.empty
      , stDynamicTextProps = Set.empty
      , stCurId          = 20
      , stNextFigureNum  = 1
      , stNextTableNum   = 1
      }

setFirstPara :: PandocMonad m => WS m ()
setFirstPara =  modify $ \s -> s { stFirstPara = True }

type WS m = ReaderT WriterEnv (StateT WriterState m)

-- Word will insert these footnotes into the settings.xml file
-- (whether or not they're visible in the document). If they're in the
-- file, but not in the footnotes.xml file, it will produce
-- problems. So we want to make sure we insert them into our document.
defaultFootnotes :: [Element]
defaultFootnotes = [ mknode "w:footnote"
                     [("w:type", "separator"), ("w:id", "-1")]
                     [ mknode "w:p" []
                       [mknode "w:r" []
                        [ mknode "w:separator" [] ()]]]
                   , mknode "w:footnote"
                     [("w:type", "continuationSeparator"), ("w:id", "0")]
                     [ mknode "w:p" []
                       [ mknode "w:r" []
                         [ mknode "w:continuationSeparator" [] ()]]]]

pStyleM :: (PandocMonad m) => ParaStyleName -> WS m XML.Element
pStyleM styleName = do
  pStyleMap <- gets (smParaStyle . stStyleMaps)
  let sty' = getStyleIdFromName styleName pStyleMap
  return $ mknode "w:pStyle" [("w:val", fromStyleId sty')] ()

withParaProp :: PandocMonad m => Element -> WS m a -> WS m a
withParaProp d p =
  local (\env -> env {envParaProperties = ep <> envParaProperties env}) p
  where ep = if isStyle d then EnvProps (Just d) [] else EnvProps Nothing [d]

withParaPropM :: PandocMonad m => WS m Element -> WS m a -> WS m a
withParaPropM md p = do
  d <- md
  withParaProp d p

isStyle :: Element -> Bool
isStyle e = isElem [] "w" "rStyle" e ||
            isElem [] "w" "pStyle" e
