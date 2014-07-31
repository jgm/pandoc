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
import Network.URI (isURI, escapeURIString)
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import System.FilePath (takeExtension, dropExtension, takeDirectory, (</>))
import Data.Char (toLower, isAscii, isAlphaNum)
import Codec.Compression.GZip as Gzip
import qualified Data.ByteString.Lazy as L
import Text.Pandoc.Shared (renderTags', openURL, readDataFile, err)
import Text.Pandoc.MediaBag (MediaBag, lookupMedia)
import Text.Pandoc.UTF8 (toString,  fromString)
import Text.Pandoc.MIME (getMimeType)
import System.Directory (doesFileExist)

isOk :: Char -> Bool
isOk c = isAscii c && isAlphaNum c

convertTag :: MediaBag -> Maybe FilePath -> Tag String -> IO (Tag String)
convertTag media userdata t@(TagOpen tagname as)
  | tagname `elem` ["img", "embed", "video", "input", "audio", "source"] = do
       as' <- mapM processAttribute as
       return $ TagOpen tagname as'
  where processAttribute (x,y) =
           if x == "src" || x == "href" || x == "poster"
              then do
                (raw, mime) <- getRaw media userdata (fromAttrib "type" t) y
                let enc = "data:" ++ mime ++ ";base64," ++ toString (encode raw)
                return (x, enc)
              else return (x,y)
convertTag media userdata t@(TagOpen "script" as) =
  case fromAttrib "src" t of
       []     -> return t
       src    -> do
           (raw, mime) <- getRaw media userdata (fromAttrib "type" t) src
           let enc = "data:" ++ mime ++ "," ++ escapeURIString isOk (toString raw)
           return $ TagOpen "script" (("src",enc) : [(x,y) | (x,y) <- as, x /= "src"])
convertTag media userdata t@(TagOpen "link" as) =
  case fromAttrib "href" t of
       []  -> return t
       src -> do
           (raw, mime) <- getRaw media userdata (fromAttrib "type" t) src
           let enc = "data:" ++ mime ++ "," ++ escapeURIString isOk (toString raw)
           return $ TagOpen "link" (("href",enc) : [(x,y) | (x,y) <- as, x /= "href"])
convertTag _ _ t = return t

-- NOTE: This is really crude, it doesn't respect CSS comments.
cssURLs :: MediaBag -> Maybe FilePath -> FilePath -> ByteString
        -> IO ByteString
cssURLs media userdata d orig =
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
                  (raw, mime) <- getRaw media userdata "" url'
                  rest <- cssURLs media userdata d v
                  let enc = "data:" `B.append` fromString mime `B.append`
                               ";base64," `B.append` (encode raw)
                  return $ x `B.append` "url(" `B.append` enc `B.append` rest

getItem :: MediaBag -> Maybe FilePath -> String
        -> IO (ByteString, Maybe String)
getItem media userdata f =
  if isURI f
     then openURL f >>= either handleErr return
     else do
       -- strip off trailing query or fragment part, if relative URL.
       -- this is needed for things like cmunrm.eot?#iefix,
       -- which is used to get old versions of IE to work with web fonts.
       let f' = takeWhile (\c -> c /= '?' && c /= '#') f
       let mbMime = case takeExtension f' of
                         ".gz" -> getMimeType $ dropExtension f'
                         x     -> getMimeType x
       exists <- doesFileExist f'
       if exists
          then do
            cont <- B.readFile f'
            return (cont, mbMime)
          else case lookupMedia f media of
                    Just (mime,bs) -> return (BS.concat $ L.toChunks bs,
                                              Just mime)
                    Nothing        -> do
                      cont <- readDataFile userdata f'
                      return (cont, mbMime)
  where handleErr e = err 61 $ "Failed to retrieve " ++ f ++ "\n" ++ show e

getRaw :: MediaBag -> Maybe FilePath -> String -> String
       -> IO (ByteString, String)
getRaw media userdata mimetype src = do
  let ext = map toLower $ takeExtension src
  (raw, respMime) <- getItem media userdata src
  let raw' = if ext == ".gz"
                then B.concat $ L.toChunks $ Gzip.decompress $ L.fromChunks
                      $ [raw]
                else raw
  let mime = case (mimetype, respMime) of
                  ("",Nothing) -> error
                         $ "Could not determine mime type for `" ++ src ++ "'"
                  (x, Nothing) -> x
                  (_, Just x ) -> x
  result <- if mime == "text/css"
               then cssURLs media userdata (takeDirectory src) raw'
               else return raw'
  return (result, mime)

-- | Convert HTML into self-contained HTML, incorporating images,
-- scripts, and CSS using data: URIs.  Items specified using absolute
-- URLs will be downloaded; those specified using relative URLs will
-- be sought first relative to the working directory, then in the
-- media bag, then relative
-- to the user data directory (if the first parameter is 'Just'
-- a directory), and finally relative to pandoc's default data
-- directory.
makeSelfContained :: MediaBag -> Maybe FilePath -> String -> IO String
makeSelfContained media userdata inp = do
  let tags = parseTags inp
  out' <- mapM (convertTag media userdata) tags
  return $ renderTags' out'
