{-# LANGUAGE NoImplicitPrelude #-}
{-
Copyright (C) 2013-2019 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Readers.OPML
   Copyright   : Copyright (C) 2013-2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of OPML to 'Pandoc' document.
-}

module Text.Pandoc.Readers.OPML ( readOPML ) where
import Prelude
import Control.Monad.State.Strict
import Data.Char (toUpper)
import Data.Default
import Data.Generics
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Text.HTML.TagSoup.Entity (lookupEntity)
import Text.Pandoc.Builder
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Options
import Text.Pandoc.Readers.HTML (readHtml)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Shared (crFilter, blocksToInlines')
import Text.XML.Light

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

readOPML :: PandocMonad m => ReaderOptions -> Text -> m Pandoc
readOPML opts inp  = do
  (bs, st') <- runStateT
                 (mapM parseBlock $ normalizeTree $
                    parseXML (unpack (crFilter inp))) def{ opmlOptions = opts }
  return $
    setTitle (opmlDocTitle st') $
    setAuthors (opmlDocAuthors st') $
    setDate (opmlDocDate st') $
    doc $ mconcat bs

-- normalize input, consolidating adjacent Text and CRef elements
normalizeTree :: [Content] -> [Content]
normalizeTree = everywhere (mkT go)
  where go :: [Content] -> [Content]
        go (Text (CData CDataRaw _ _):xs) = xs
        go (Text (CData CDataText s1 z):Text (CData CDataText s2 _):xs) =
           Text (CData CDataText (s1 ++ s2) z):xs
        go (Text (CData CDataText s1 z):CRef r:xs) =
           Text (CData CDataText (s1 ++ convertEntity r) z):xs
        go (CRef r:Text (CData CDataText s1 z):xs) =
             Text (CData CDataText (convertEntity r ++ s1) z):xs
        go (CRef r1:CRef r2:xs) =
             Text (CData CDataText (convertEntity r1 ++ convertEntity r2) Nothing):xs
        go xs = xs

convertEntity :: String -> String
convertEntity e = Data.Maybe.fromMaybe (map toUpper e) (lookupEntity e)

-- convenience function to get an attribute value, defaulting to ""
attrValue :: String -> Element -> String
attrValue attr elt =
  fromMaybe "" (lookupAttrBy (\x -> qName x == attr) (elAttribs elt))

-- exceptT :: PandocMonad m => Either PandocError a -> OPML m a
-- exceptT = either throwError return

asHtml :: PandocMonad m => String -> OPML m Inlines
asHtml s = do
  opts <- gets opmlOptions
  Pandoc _ bs <- readHtml def{ readerExtensions = readerExtensions opts } (pack s)
  return $ blocksToInlines' bs

asMarkdown :: PandocMonad m => String -> OPML m Blocks
asMarkdown s = do
  opts <- gets opmlOptions
  Pandoc _ bs <- readMarkdown def{ readerExtensions = readerExtensions opts }  (pack s)
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
                     let headerText' = case map toUpper (attrValue "type" e) of
                                             "LINK"  -> link
                                               (attrValue "url" e) "" headerText
                                             _ -> headerText
                     return $ header n headerText' <> noteBlocks <> bs
parseBlock _ = return mempty
