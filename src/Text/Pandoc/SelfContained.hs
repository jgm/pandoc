{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2011-2014 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2011-2014 John MacFarlane
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
import Text.Pandoc.UTF8 (toString,  fromString)
import Text.Pandoc.Options (WriterOptions(..))

isOk :: Char -> Bool
isOk c = isAscii c && isAlphaNum c

convertTag :: MediaBag -> Maybe String -> Tag String -> IO (Tag String)
convertTag media sourceURL t@(TagOpen tagname as)
  | tagname `elem`
     ["img", "embed", "video", "input", "audio", "source", "track"] = do
       as' <- mapM processAttribute as
       return $ TagOpen tagname as'
  where processAttribute (x,y) =
           if x == "src" || x == "href" || x == "poster"
              then do
                (raw, mime) <- getRaw media sourceURL (fromAttrib "type" t) y
                let enc = "data:" ++ mime ++ ";base64," ++ toString (encode raw)
                return (x, enc)
              else return (x,y)
convertTag media sourceURL t@(TagOpen "script" as) =
  case fromAttrib "src" t of
       []     -> return t
       src    -> do
           (raw, mime) <- getRaw media sourceURL (fromAttrib "type" t) src
           let enc = "data:" ++ mime ++ "," ++ escapeURIString isOk (toString raw)
           return $ TagOpen "script" (("src",enc) : [(x,y) | (x,y) <- as, x /= "src"])
convertTag media sourceURL t@(TagOpen "link" as) =
  case fromAttrib "href" t of
       []  -> return t
       src -> do
           (raw, mime) <- getRaw media sourceURL (fromAttrib "type" t) src
           let enc = "data:" ++ mime ++ "," ++ escapeURIString isOk (toString raw)
           return $ TagOpen "link" (("href",enc) : [(x,y) | (x,y) <- as, x /= "href"])
convertTag _ _ t = return t

-- NOTE: This is really crude, it doesn't respect CSS comments.
cssURLs :: MediaBag -> Maybe String -> FilePath -> ByteString
        -> IO ByteString
cssURLs media sourceURL d orig =
  case B.breakSubstring "url(" orig of
       (x,y) | B.null y  -> return orig
             | otherwise -> do
                  let (u,v) = B.breakSubstring ")" $ B.drop 4 y
                  let url = toString
                          $ case B.take 1 u of
                                 "\"" -> B.takeWhile (/='"') $ B.drop 1 u
                                 "'"  -> B.takeWhile (/='\'') $ B.drop 1 u
                                 _    -> u
                  let url' = if isURI url
                                then url
                                else d </> url
                  (raw, mime) <- getRaw media sourceURL "" url'
                  rest <- cssURLs media sourceURL d v
                  let enc = "data:" `B.append` fromString mime `B.append`
                               ";base64," `B.append` (encode raw)
                  return $ x `B.append` "url(" `B.append` enc `B.append` rest

getRaw :: MediaBag -> Maybe String -> MimeType -> String
       -> IO (ByteString, MimeType)
getRaw media sourceURL mimetype src = do
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
  return (result, mime)

-- | Convert HTML into self-contained HTML, incorporating images,
-- scripts, and CSS using data: URIs.
makeSelfContained :: WriterOptions -> String -> IO String
makeSelfContained opts inp = do
  let tags = parseTags inp
  out' <- mapM (convertTag (writerMediaBag opts) (writerSourceURL opts)) tags
  return $ renderTags' out'
