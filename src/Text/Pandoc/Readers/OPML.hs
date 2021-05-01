{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.OPML
   Copyright   : Copyright (C) 2013-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of OPML to 'Pandoc' document.
-}

module Text.Pandoc.Readers.OPML ( readOPML ) where
import Control.Monad.State.Strict
import Data.Default
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Pandoc.Builder
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Options
import Text.Pandoc.Error (PandocError(..))
import Text.Pandoc.Readers.HTML (readHtml)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Shared (blocksToInlines')
import Text.Pandoc.Sources (ToSources(..), sourcesToText)
import Text.Pandoc.XML.Light
import Control.Monad.Except (throwError)

type OPML m = StateT OPMLState m

data OPMLState = OPMLState{
                        opmlSectionLevel :: Int
                      , opmlDocTitle     :: Inlines
                      , opmlDocAuthors   :: [Inlines]
                      , opmlDocDate      :: Inlines
                      , opmlOptions      :: ReaderOptions
                      } deriving Show

instance Default OPMLState where
  def = OPMLState{ opmlSectionLevel = 0
                 , opmlDocTitle = mempty
                 , opmlDocAuthors = []
                 , opmlDocDate = mempty
                 , opmlOptions = def
                 }

readOPML :: (PandocMonad m, ToSources a)
         => ReaderOptions
         -> a
         -> m Pandoc
readOPML opts inp  = do
  let sources = toSources inp
  (bs, st') <-
    runStateT (case parseXMLContents (TL.fromStrict . sourcesToText $ sources) of
                     Left msg -> throwError $ PandocXMLError "" msg
                     Right ns -> mapM parseBlock ns)
                 def{ opmlOptions = opts }
  return $
    setTitle (opmlDocTitle st') $
    setAuthors (opmlDocAuthors st') $
    setDate (opmlDocDate st') $
    doc $ mconcat bs

-- convenience function to get an attribute value, defaulting to ""
attrValue :: Text -> Element -> Text
attrValue attr elt =
  fromMaybe "" (lookupAttrBy (\x -> qName x == attr) (elAttribs elt))

-- exceptT :: PandocMonad m => Either PandocError a -> OPML m a
-- exceptT = either throwError return

asHtml :: PandocMonad m => Text -> OPML m Inlines
asHtml s = do
  opts <- gets opmlOptions
  Pandoc _ bs <- readHtml def{ readerExtensions = readerExtensions opts } s
  return $ blocksToInlines' bs

asMarkdown :: PandocMonad m => Text -> OPML m Blocks
asMarkdown s = do
  opts <- gets opmlOptions
  Pandoc _ bs <- readMarkdown def{ readerExtensions = readerExtensions opts } s
  return $ fromList bs

getBlocks :: PandocMonad m => Element -> OPML m Blocks
getBlocks e =  mconcat <$> mapM parseBlock (elContent e)

parseBlock :: PandocMonad m => Content -> OPML m Blocks
parseBlock (Elem e) =
  case qName (elName e) of
        "ownerName"    -> mempty <$ modify (\st ->
                              st{opmlDocAuthors = [text $ strContent e]})
        "dateModified" -> mempty <$ modify (\st ->
                              st{opmlDocDate = text $ strContent e})
        "title"        -> mempty <$ modify (\st ->
                              st{opmlDocTitle = text $ strContent e})
        "outline" -> gets opmlSectionLevel >>= sect . (+1)
        "?xml"  -> return mempty
        _       -> getBlocks e
   where sect n = do headerText <- asHtml $ attrValue "text" e
                     noteBlocks <- asMarkdown $ attrValue "_note" e
                     modify $ \st -> st{ opmlSectionLevel = n }
                     bs <- getBlocks e
                     modify $ \st -> st{ opmlSectionLevel = n - 1 }
                     let headerText' = case T.toUpper (attrValue "type" e) of
                                             "LINK"  -> link
                                               (attrValue "url" e) "" headerText
                                             _ -> headerText
                     return $ header n headerText' <> noteBlocks <> bs
parseBlock _ = return mempty
