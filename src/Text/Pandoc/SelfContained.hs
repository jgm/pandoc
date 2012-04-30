{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2011 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2011 John MacFarlane
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
import Network.URI (isAbsoluteURI, parseURI, escapeURIString)
import Network.HTTP
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString, fromString)
import System.FilePath (takeExtension, dropExtension, takeDirectory, (</>))
import Data.Char (toLower, isAscii, isAlphaNum)
import Codec.Compression.GZip as Gzip
import qualified Data.ByteString.Lazy as L
import Text.Pandoc.Shared (findDataFile)
import Text.Pandoc.MIME (getMimeType)
import System.Directory (doesFileExist)

getItem :: Maybe FilePath -> String -> IO (ByteString, Maybe String)
getItem userdata f =
  if isAbsoluteURI f
     then openURL f
     else do
       let mime = case takeExtension f of
                      ".gz" -> getMimeType $ dropExtension f
                      x     -> getMimeType x
       exists <- doesFileExist f
       if exists
          then do
            cont <- B.readFile f
            return (cont, mime)
          else do
            res <- findDataFile userdata f
            exists' <- doesFileExist res
            if exists'
               then do
                 cont <- B.readFile res
                 return (cont, mime)
               else error $ "Could not find `" ++ f ++ "'"

-- TODO - have this return mime type too - then it can work for google
-- chart API, e.g.
openURL :: String -> IO (ByteString, Maybe String)
openURL u = getBodyAndMimeType =<< simpleHTTP (getReq u)
  where getReq v = case parseURI v of
                     Nothing  -> error $ "Could not parse URI: " ++ v
                     Just u'  -> mkRequest GET u'
        getBodyAndMimeType (Left err) = fail (show err)
        getBodyAndMimeType (Right r)  = return (rspBody r, findHeader HdrContentType r)

isOk :: Char -> Bool
isOk c = isAscii c && isAlphaNum c

convertTag :: Maybe FilePath -> Tag String -> IO (Tag String)
convertTag userdata t@(TagOpen "img" as) =
       case fromAttrib "src" t of
         []   -> return t
         src  -> do
           (raw, mime) <- getRaw userdata (fromAttrib "type" t) src
           let enc = "data:" ++ mime ++ ";base64," ++ toString (encode raw)
           return $ TagOpen "img" (("src",enc) : [(x,y) | (x,y) <- as, x /= "src"])
convertTag userdata t@(TagOpen "video" as) =
       case fromAttrib "src" t of
         []   -> return t
         src  -> do
           (raw, mime) <- getRaw userdata (fromAttrib "type" t) src
           let enc = "data:" ++ mime ++ ";base64," ++ toString (encode raw)
           return $ TagOpen "video" (("src",enc) : [(x,y) | (x,y) <- as, x /= "src"])
convertTag userdata t@(TagOpen "script" as) =
  case fromAttrib "src" t of
       []     -> return t
       src    -> do
           (raw, mime) <- getRaw userdata (fromAttrib "type" t) src
           let enc = "data:" ++ mime ++ "," ++ escapeURIString isOk (toString raw)
           return $ TagOpen "script" (("src",enc) : [(x,y) | (x,y) <- as, x /= "src"]) 
convertTag userdata t@(TagOpen "link" as) =
  case fromAttrib "href" t of
       []  -> return t
       src -> do
           (raw, mime) <- getRaw userdata (fromAttrib "type" t) src
           let enc = "data:" ++ mime ++ "," ++ escapeURIString isOk (toString raw)
           return $ TagOpen "link" (("href",enc) : [(x,y) | (x,y) <- as, x /= "href"]) 
convertTag _ t = return t

cssURLs :: Maybe FilePath -> FilePath -> ByteString -> IO ByteString
cssURLs userdata d orig =
  case B.breakSubstring "url(" orig of
       (x,y) | B.null y  -> return orig
             | otherwise -> do
                  let (u,v) = B.breakSubstring ")" $ B.drop 4 y
                  let url = toString
                          $ case B.take 1 u of
                                 "\"" -> B.takeWhile (/='"') $ B.drop 1 u
                                 _    -> u
                  (raw, mime) <- getRaw userdata "" (d </> url)
                  rest <- cssURLs userdata d v
                  let enc = "data:" `B.append` fromString mime `B.append`
                               ";base64," `B.append` (encode raw)
                  return $ x `B.append` "url(" `B.append` enc `B.append` rest

getRaw :: Maybe FilePath -> String -> String -> IO (ByteString, String)
getRaw userdata mimetype src = do
  let ext = map toLower $ takeExtension src
  (raw, respMime) <- getItem userdata src
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
               then cssURLs userdata (takeDirectory src) raw'
               else return raw'
  return (result, mime)

-- | Convert HTML into self-contained HTML, incorporating images,
-- scripts, and CSS using data: URIs.  Items specified using absolute
-- URLs will be downloaded; those specified using relative URLs will
-- be sought first relative to the working directory, then relative
-- to the user data directory (if the first parameter is 'Just'
-- a directory), and finally relative to pandoc's default data
-- directory.
makeSelfContained :: Maybe FilePath -> String -> IO String
makeSelfContained userdata inp = do
  let tags = parseTags inp
  out' <- mapM (convertTag userdata) tags
  return $ renderTags' out'

-- repeated from HTML reader:
renderTags' :: [Tag String] -> String
renderTags' = renderTagsOptions
               renderOptions{ optMinimize = \x ->
                                    let y = map toLower x
                                    in  y == "hr" || y == "br" ||
                                        y == "img" || y == "meta" ||
                                        y == "link"
                            , optRawTag = \x ->
                                    let y = map toLower x
                                    in  y == "script" || y == "style" }
