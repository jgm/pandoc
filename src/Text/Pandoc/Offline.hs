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
   Module      : Text.Pandoc.Offline
   Copyright   : Copyright (C) 2011 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions for converting an HTML file into one that can be viewed
offline, by incorporating linked images, CSS, and scripts into
the HTML using data URIs.
-}
module Text.Pandoc.Offline ( offline ) where
import Text.HTML.TagSoup
import Network.URI (isAbsoluteURI, parseURI, escapeURIString)
import Network.HTTP
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import System.FilePath (takeExtension, dropExtension, takeDirectory, (</>))
import Data.Char (toLower, isAscii, isAlphaNum)
import Codec.Compression.GZip as Gzip
import qualified Data.ByteString.Lazy as L
import Text.Pandoc.Shared (findDataFile)
import System.Directory (doesFileExist)

getItem :: String -> IO ByteString
getItem f =
  if isAbsoluteURI f
     then openURL f
     else do
       let userDataDir = "." -- TODO writeUserDataDir
       exists <- doesFileExist f
       if exists
          then B.readFile f
          else do
            res <- findDataFile (Just userDataDir) f
            exists' <- doesFileExist res
            if exists'
               then B.readFile res
               else B.readFile f -- will throw error

openURL :: String -> IO ByteString
openURL u = getResponseBody =<< simpleHTTP (getReq u)
  where getReq v = case parseURI v of
                     Nothing  -> error $ "Could not parse URI: " ++ v
                     Just u'  -> mkRequest GET u'

mimeTypeFor :: String -> String
mimeTypeFor s = case lookup s mimetypes of
                     Nothing -> error $ "Could not find mime type for " ++ s
                     Just x  -> x
  where mimetypes = [ -- taken from MissingH
                (".a", "application/octet-stream"),
                (".ai", "application/postscript"),
                (".aif", "audio/x-aiff"),
                (".aifc", "audio/x-aiff"),
                (".aiff", "audio/x-aiff"),
                (".au", "audio/basic"),
                (".avi", "video/x-msvideo"),
                (".bat", "text/plain"),
                (".bcpio", "application/x-bcpio"),
                (".bin", "application/octet-stream"),
                (".bmp", "image/x-ms-bmp"),
                (".c", "text/plain"),
                (".cdf", "application/x-netcdf"),
                (".cpio", "application/x-cpio"),
                (".csh", "application/x-csh"),
                (".css", "text/css"),
                (".dll", "application/octet-stream"),
                (".doc", "application/msword"),
                (".dot", "application/msword"),
                (".dvi", "application/x-dvi"),
                (".eml", "message/rfc822"),
                (".eps", "application/postscript"),
                (".etx", "text/x-setext"),
                (".exe", "application/octet-stream"),
                (".gif", "image/gif"),
                (".gtar", "application/x-gtar"),
                (".h", "text/plain"),
                (".hdf", "application/x-hdf"),
                (".htm", "text/html"),
                (".html", "text/html"),
                (".ief", "image/ief"),
                (".jpe", "image/jpeg"),
                (".jpeg", "image/jpeg"),
                (".jpg", "image/jpeg"),
                (".js", "application/x-javascript"),
                (".ksh", "text/plain"),
                (".latex", "application/x-latex"),
                (".m1v", "video/mpeg"),
                (".man", "application/x-troff-man"),
                (".me", "application/x-troff-me"),
                (".mht", "message/rfc822"),
                (".mhtml", "message/rfc822"),
                (".mif", "application/x-mif"),
                (".mov", "video/quicktime"),
                (".movie", "video/x-sgi-movie"),
                (".mp2", "audio/mpeg"),
                (".mp3", "audio/mpeg"),
                (".mpa", "video/mpeg"),
                (".mpe", "video/mpeg"),
                (".mpeg", "video/mpeg"),
                (".mpg", "video/mpeg"),
                (".ms", "application/x-troff-ms"),
                (".nc", "application/x-netcdf"),
                (".nws", "message/rfc822"),
                (".o", "application/octet-stream"),
                (".obj", "application/octet-stream"),
                (".oda", "application/oda"),
                (".p12", "application/x-pkcs12"),
                (".p7c", "application/pkcs7-mime"),
                (".pbm", "image/x-portable-bitmap"),
                (".pdf", "application/pdf"),
                (".pfx", "application/x-pkcs12"),
                (".pgm", "image/x-portable-graymap"),
                (".pl", "text/plain"),
                (".png", "image/png"),
                (".pnm", "image/x-portable-anymap"),
                (".pot", "application/vnd.ms-powerpoint"),
                (".ppa", "application/vnd.ms-powerpoint"),
                (".ppm", "image/x-portable-pixmap"),
                (".pps", "application/vnd.ms-powerpoint"),
                (".ppt", "application/vnd.ms-powerpoint"),
                (".ps", "application/postscript"),
                (".pwz", "application/vnd.ms-powerpoint"),
                (".py", "text/x-python"),
                (".pyc", "application/x-python-code"),
                (".pyo", "application/x-python-code"),
                (".qt", "video/quicktime"),
                (".ra", "audio/x-pn-realaudio"),
                (".ram", "application/x-pn-realaudio"),
                (".ras", "image/x-cmu-raster"),
                (".rdf", "application/xml"),
                (".rgb", "image/x-rgb"),
                (".roff", "application/x-troff"),
                (".rtx", "text/richtext"),
                (".sgm", "text/x-sgml"),
                (".sgml", "text/x-sgml"),
                (".sh", "application/x-sh"),
                (".shar", "application/x-shar"),
                (".snd", "audio/basic"),
                (".so", "application/octet-stream"),
                (".src", "application/x-wais-source"),
                (".sv4cpio", "application/x-sv4cpio"),
                (".sv4crc", "application/x-sv4crc"),
                (".swf", "application/x-shockwave-flash"),
                (".t", "application/x-troff"),
                (".tar", "application/x-tar"),
                (".tcl", "application/x-tcl"),
                (".tex", "application/x-tex"),
                (".texi", "application/x-texinfo"),
                (".texinfo", "application/x-texinfo"),
                (".tif", "image/tiff"),
                (".tiff", "image/tiff"),
                (".tr", "application/x-troff"),
                (".tsv", "text/tab-separated-values"),
                (".txt", "text/plain"),
                (".ustar", "application/x-ustar"),
                (".vcf", "text/x-vcard"),
                (".wav", "audio/x-wav"),
                (".wiz", "application/msword"),
                (".xbm", "image/x-xbitmap"),
                (".xlb", "application/vnd.ms-excel"),
                (".xls", "application/vnd.ms-excel"),
                (".xml", "text/xml"),
                (".xpm", "image/x-xpixmap"),
                (".xsl", "application/xml"),
                (".xwd", "image/x-xwindowdump"),
                (".zip", "application/zip"),
                (".jpg", "image/jpg"),
                (".mid", "audio/midi"),
                (".midi", "audio/midi"),
                (".pct", "image/pict"),
                (".pic", "image/pict"),
                (".pict", "image/pict"),
                (".rtf", "application/rtf"),
                (".xul", "text/xul")
                ]

isOk :: Char -> Bool
isOk c = isAscii c && isAlphaNum c

convertTag :: Tag String -> IO (Tag String)
convertTag t@(TagOpen "img" as) =
       case fromAttrib "src" t of
         []   -> return t
         src  -> do
           (raw, mime) <- getRaw t src
           let enc = "data:" ++ mime ++ ";base64," ++ toString (encode raw)
           return $ TagOpen "img" (("src",enc) : [(x,y) | (x,y) <- as, x /= "src"])
convertTag t@(TagOpen "script" as) =
  case fromAttrib "src" t of
       []     -> return t
       src    -> do
           (raw, mime) <- getRaw t src
           let enc = "data:" ++ mime ++ "," ++ escapeURIString isOk (toString raw)
           return $ TagOpen "script" (("src",enc) : [(x,y) | (x,y) <- as, x /= "src"]) 
convertTag t@(TagOpen "link" as) =
  case fromAttrib "href" t of
       []  -> return t
       src -> do
           (raw, mime) <- getRaw t src
           let enc = "data:" ++ mime ++ "," ++ escapeURIString isOk (toString raw)
           return $ TagOpen "link" (("href",enc) : [(x,y) | (x,y) <- as, x /= "href"]) 
convertTag t = return t

cssImports :: FilePath -> ByteString -> IO ByteString
cssImports d orig =
  case B.breakSubstring "@import" orig of
       (x,y) | B.null y  -> return orig
             | otherwise -> do
                  rest <- handleImport d (B.drop 7 y) >>= cssImports d
                  return $ x `B.append` rest

-- @import url("blah");
-- @import url(blah);
-- @import "blah";
handleImport :: FilePath -> ByteString -> IO ByteString
handleImport d x = fmap (`B.append` rest) (getItem $ d </> url)
  where lparenOrQuote c = c == '(' || c == '"'
        rparenOrQuote c = c == ')' || c == '"'
        url = toString
              $ B.takeWhile (not . rparenOrQuote)
              $ B.dropWhile lparenOrQuote
              $ B.dropWhile (not . lparenOrQuote) x
        rest = B.drop 1 $ B.dropWhile (/= ';') x

getRaw :: Tag String -> String -> IO (ByteString, String)
getRaw t src = do
  let ext = map toLower $ takeExtension src
  let (ext',decomp) = if ext == ".gz"
                         then (takeExtension $ dropExtension src, B.concat . L.toChunks . Gzip.decompress . L.fromChunks . (:[]))
                         else (ext, id)
  let mime = case fromAttrib "type" t of
                  []  -> mimeTypeFor ext'
                  x   -> x
  raw <- getItem src
  result <- if mime == "text/css"
               then cssImports (takeDirectory src) $ decomp raw
               else return $ decomp raw
  return (result, mime)

offline :: String -> IO String
offline inp = do
  let tags = parseTags inp
  out' <- mapM convertTag tags
  return $ renderTagsOptions renderOptions{ optMinimize = (\t -> t == "br"
                 || t == "img" || t == "meta" || t == "link" ) } out'

