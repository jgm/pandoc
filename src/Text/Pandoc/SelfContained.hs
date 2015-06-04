{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2011-2015 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.SelfContained
   Copyright   : Copyright (C) 2011-2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions for converting an HTML file into one that can be viewed
offline, by incorporating linked images, CSS, and scripts into
the HTML using data URIs.
-}
module Text.Pandoc.SelfContained ( makeSelfContained ) where
import Text.HTML.TagSoup
import Network.URI (isURI, escapeURIString, URI(..), parseURI)
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import System.FilePath (takeExtension, takeDirectory, (</>))
import Data.Char (toLower, isAscii, isAlphaNum)
import Codec.Compression.GZip as Gzip
import qualified Data.ByteString.Lazy as L
import Text.Pandoc.Shared (renderTags', err, fetchItem')
import Text.Pandoc.MediaBag (MediaBag)
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.UTF8 (toString)
import Text.Pandoc.Options (WriterOptions(..))
import Data.List (isPrefixOf)
import Control.Applicative
import Text.CSS.Parse (parseNestedBlocks, NestedBlock(..))
import Text.CSS.Render (renderNestedBlocks)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)

isOk :: Char -> Bool
isOk c = isAscii c && isAlphaNum c

makeDataURI :: String -> ByteString -> String
makeDataURI mime raw =
  if textual
     then "data:" ++ mime' ++ "," ++ escapeURIString isOk (toString raw)
     else "data:" ++ mime' ++ ";base64," ++ toString (encode raw)
  where textual = "text/" `Data.List.isPrefixOf` mime
        mime' = if textual && ';' `notElem` mime
                   then mime ++ ";charset=utf-8"
                   else mime  -- mime type already has charset

convertTag :: MediaBag -> Maybe String -> Tag String -> IO (Tag String)
convertTag media sourceURL t@(TagOpen tagname as)
  | tagname `elem`
     ["img", "embed", "video", "input", "audio", "source", "track"] = do
       as' <- mapM processAttribute as
       return $ TagOpen tagname as'
  where processAttribute (x,y) =
           if x == "src" || x == "href" || x == "poster"
              then do
                enc <- getDataURI media sourceURL (fromAttrib "type" t) y
                return (x, enc)
              else return (x,y)
convertTag media sourceURL t@(TagOpen "script" as) =
  case fromAttrib "src" t of
       []     -> return t
       src    -> do
           enc <- getDataURI media sourceURL (fromAttrib "type" t) src
           return $ TagOpen "script" (("src",enc) : [(x,y) | (x,y) <- as, x /= "src"])
convertTag media sourceURL t@(TagOpen "link" as) =
  case fromAttrib "href" t of
       []  -> return t
       src -> do
           enc <- getDataURI media sourceURL (fromAttrib "type" t) src
           return $ TagOpen "link" (("href",enc) : [(x,y) | (x,y) <- as, x /= "href"])
convertTag _ _ t = return t

-- NOTE: This is really crude, it doesn't respect CSS comments.
cssURLs :: MediaBag -> Maybe String -> FilePath -> ByteString
        -> IO ByteString
cssURLs media sourceURL d orig = do
  case parseNestedBlocks (decodeUtf8 orig) of
       Left _err  -> return orig
       Right bs   -> (encodeUtf8 . toStrict . toLazyText . renderNestedBlocks)
                   <$> mapM (handleCSSUrls media sourceURL d) bs

handleCSSUrls :: MediaBag -> Maybe String -> FilePath -> NestedBlock
              -> IO NestedBlock
handleCSSUrls media sourceURL d (NestedBlock t bs) =
  NestedBlock t <$> mapM (handleCSSUrls media sourceURL d) bs
handleCSSUrls media sourceURL d (LeafBlock (selector, attrs)) = do
  attrs' <- mapM (handleCSSAttr media sourceURL d) attrs
  return (LeafBlock (selector, attrs'))

handleCSSAttr :: MediaBag -> Maybe String -> FilePath -> (Text, Text)
              -> IO (Text, Text)
handleCSSAttr media sourceURL d (key, val) =
  if "url(" `T.isPrefixOf` val
     then do
        let url = T.unpack $ dropParens $ T.drop 3 val
        case url of
             '#':_ -> return (key, val)
             'd':'a':'t':'a':':':_ -> return (key, val)
             _ -> do
               let url' = if isURI url then url else d </> url
               enc <- getDataURI media sourceURL "" url'
               return (key, T.pack enc)
      else return (key, val)

dropParens :: Text -> Text
dropParens = T.dropAround (`elem` ['(',')','"','\'',' ','\t','\n','\r'])

getDataURI :: MediaBag -> Maybe String -> MimeType -> String
       -> IO String
getDataURI _ _ _ src@('d':'a':'t':'a':':':_) = return src  -- already data: uri
getDataURI media sourceURL mimetype src = do
  let ext = map toLower $ takeExtension src
  fetchResult <- fetchItem' media sourceURL src
  (raw, respMime) <- case fetchResult of
                          Left msg -> err 67 $ "Could not fetch " ++ src ++
                                               "\n" ++ show msg
                          Right x  -> return x
  let raw' = if ext == ".gz"
                then B.concat $ L.toChunks $ Gzip.decompress $ L.fromChunks
                      $ [raw]
                else raw
  let mime = case (mimetype, respMime) of
                  ("",Nothing) -> error
                         $ "Could not determine mime type for `" ++ src ++ "'"
                  (x, Nothing) -> x
                  (_, Just x ) -> x
  let cssSourceURL = case parseURI src of
                          Just u
                            | uriScheme u `elem` ["http:","https:"] ->
                                Just $ show u{ uriPath = "",
                                               uriQuery = "",
                                               uriFragment = "" }
                          _ -> Nothing
  result <- if mime == "text/css"
               then cssURLs media cssSourceURL (takeDirectory src) raw'
               else return raw'
  return $ makeDataURI mime result

-- | Convert HTML into self-contained HTML, incorporating images,
-- scripts, and CSS using data: URIs.
makeSelfContained :: WriterOptions -> String -> IO String
makeSelfContained opts inp = do
  let tags = parseTags inp
  out' <- mapM (convertTag (writerMediaBag opts) (writerSourceURL opts)) tags
  return $ renderTags' out'
