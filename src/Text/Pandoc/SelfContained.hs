{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2011-2016 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2011-2016 John MacFarlane
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
import Text.Pandoc.Shared (renderTags', err, fetchItem', warn, trim)
import Text.Pandoc.MediaBag (MediaBag)
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.UTF8 (toString)
import Text.Pandoc.Options (WriterOptions(..))
import Data.List (isPrefixOf)
import Control.Applicative ((<|>))
import Text.Parsec (runParserT, ParsecT)
import qualified Text.Parsec as P
import Control.Monad.Trans (lift)

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

cssURLs :: MediaBag -> Maybe String -> FilePath -> ByteString
        -> IO ByteString
cssURLs media sourceURL d orig = do
  res <- runParserT (parseCSSUrls media sourceURL d) () "css" orig
  case res of
       Left e    -> warn ("Could not parse CSS: " ++ show e) >> return orig
       Right bs  -> return bs

parseCSSUrls :: MediaBag -> Maybe String -> FilePath
             -> ParsecT ByteString () IO ByteString
parseCSSUrls media sourceURL d = B.concat <$> P.many
    (pCSSWhite <|> pCSSComment <|> pCSSUrl media sourceURL d <|> pCSSOther)

-- Note: some whitespace in CSS is significant, so we can't collapse it!
pCSSWhite :: ParsecT ByteString () IO ByteString
pCSSWhite = B.singleton <$> P.space <* P.spaces

pCSSComment :: ParsecT ByteString () IO ByteString
pCSSComment = P.try $ do
  P.string "/*"
  P.manyTill P.anyChar (P.try (P.string "*/"))
  return B.empty

pCSSOther :: ParsecT ByteString () IO ByteString
pCSSOther = do
  (B.pack <$> P.many1 (P.noneOf "u/ \n\r\t")) <|>
    (B.singleton <$> P.char 'u') <|>
    (B.singleton <$> P.char '/')

pCSSUrl :: MediaBag -> Maybe String -> FilePath
        -> ParsecT ByteString () IO ByteString
pCSSUrl media sourceURL d = P.try $ do
  P.string "url("
  P.spaces
  quote <- P.option Nothing (Just <$> P.oneOf "\"'")
  url <- P.manyTill P.anyChar (maybe (P.lookAhead (P.char ')')) P.char quote)
  P.spaces
  P.char ')'
  let fallback = B.pack ("url(" ++ maybe "" (:[]) quote ++ trim url ++
                            maybe "" (:[]) quote ++ ")")
  case trim url of
      '#':_ -> return fallback
      'd':'a':'t':'a':':':_ -> return fallback
      u ->  do let url' = if isURI u then u else d </> u
               enc <- lift $ getDataURI media sourceURL "" url'
               return (B.pack $ "url(" ++ enc ++ ")")


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
