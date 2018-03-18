{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2011-2018 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2011-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions for converting an HTML file into one that can be viewed
offline, by incorporating linked images, CSS, and scripts into
the HTML using data URIs.
-}
module Text.Pandoc.SelfContained ( makeDataURI, makeSelfContained ) where
import Prelude
import Codec.Compression.GZip as Gzip
import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.Char (isAlphaNum, isAscii, toLower)
import Data.List (isPrefixOf)
import Network.URI (escapeURIString)
import System.FilePath (takeDirectory, takeExtension, (</>))
import Text.HTML.TagSoup
import Text.Pandoc.Class (PandocMonad (..), fetchItem, getInputFiles, report,
                          setInputFiles)
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.Shared (isURI, renderTags', trim)
import Text.Pandoc.UTF8 (toString)
import Text.Parsec (ParsecT, runParserT)
import qualified Text.Parsec as P

isOk :: Char -> Bool
isOk c = isAscii c && isAlphaNum c

makeDataURI :: (MimeType, ByteString) -> String
makeDataURI (mime, raw) =
  if textual
     then "data:" ++ mime' ++ "," ++ escapeURIString isOk (toString raw)
     else "data:" ++ mime' ++ ";base64," ++ toString (encode raw)
  where textual = "text/" `Data.List.isPrefixOf` mime
        mime' = if textual && ';' `notElem` mime
                   then mime ++ ";charset=utf-8"
                   else mime  -- mime type already has charset

convertTags :: PandocMonad m => [Tag String] -> m [Tag String]
convertTags [] = return []
convertTags (t@TagOpen{}:ts)
  | fromAttrib "data-external" t == "1" = (t:) <$> convertTags ts
convertTags (t@(TagOpen tagname as):ts)
  | tagname `elem`
     ["img", "embed", "video", "input", "audio", "source", "track",
      "section"] = do
       as' <- mapM processAttribute as
       rest <- convertTags ts
       return $ TagOpen tagname as' : rest
  where processAttribute (x,y) =
           if x `elem` ["src", "data-src", "href", "poster", "data-background-image"]
              then do
                enc <- getDataURI (fromAttrib "type" t) y
                return (x, enc)
              else return (x,y)
convertTags (t@(TagOpen "script" as):TagClose "script":ts) =
  case fromAttrib "src" t of
       []  -> (t:) <$> convertTags ts
       src    -> do
           let typeAttr = fromAttrib "type" t
           res <- getData typeAttr src
           rest <- convertTags ts
           case res of
                Left dataUri -> return $ TagOpen "script"
                     (("src",dataUri) : [(x,y) | (x,y) <- as, x /= "src"]) :
                     TagClose "script" : rest
                Right (mime, bs)
                  | ("text/javascript" `isPrefixOf` mime ||
                     "application/javascript" `isPrefixOf` mime ||
                     "application/x-javascript" `isPrefixOf` mime) &&
                     not ("</script" `B.isInfixOf` bs) ->
                     return $
                       TagOpen "script" [("type", typeAttr)|not (null typeAttr)]
                       : TagText (toString bs)
                       : TagClose "script"
                       : rest
                  | otherwise ->
                       return  $ TagOpen "script"
                         (("src",makeDataURI (mime, bs)) :
                          [(x,y) | (x,y) <- as, x /= "src"]) :
                        TagClose "script" : rest
convertTags (t@(TagOpen "link" as):ts) =
  case fromAttrib "href" t of
       []  -> (t:) <$> convertTags ts
       src -> do
           res <- getData (fromAttrib "type" t) src
           case res of
                Left dataUri -> do
                  rest <- convertTags ts
                  return $ TagOpen "link"
                     (("href",dataUri) : [(x,y) | (x,y) <- as, x /= "href"]) :
                     rest
                Right (mime, bs)
                  | "text/css" `isPrefixOf` mime
                    && null (fromAttrib "media" t)
                    && not ("</" `B.isInfixOf` bs) -> do
                      rest <- convertTags $
                                 dropWhile (==TagClose "link") ts
                      return $
                       TagOpen "style" [("type", mime)]
                       : TagText (toString bs)
                       : TagClose "style"
                       : rest
                  | otherwise -> do
                      rest <- convertTags ts
                      return $ TagOpen "link"
                       (("href",makeDataURI (mime, bs)) :
                         [(x,y) | (x,y) <- as, x /= "href"]) : rest
convertTags (t:ts) = (t:) <$> convertTags ts

cssURLs :: PandocMonad m
        => FilePath -> ByteString -> m ByteString
cssURLs d orig = do
  res <- runParserT (parseCSSUrls d) () "css" orig
  case res of
       Left e    -> do
         report $ CouldNotParseCSS (show e)
         return orig
       Right bs  -> return bs

parseCSSUrls :: PandocMonad m
             => FilePath -> ParsecT ByteString () m ByteString
parseCSSUrls d = B.concat <$> P.many
  (pCSSWhite <|> pCSSComment <|> pCSSImport d <|> pCSSUrl d <|> pCSSOther)

pCSSImport :: PandocMonad m
           => FilePath -> ParsecT ByteString () m ByteString
pCSSImport d = P.try $ do
  P.string "@import"
  P.spaces
  res <- (pQuoted <|> pUrl) >>= handleCSSUrl d
  P.spaces
  P.char ';'
  P.spaces
  case res of
       Left b       -> return $ B.pack "@import " <> b
       Right (_, b) -> return b

-- Note: some whitespace in CSS is significant, so we can't collapse it!
pCSSWhite :: PandocMonad m => ParsecT ByteString () m ByteString
pCSSWhite = B.singleton <$> P.space <* P.spaces

pCSSComment :: PandocMonad m => ParsecT ByteString () m ByteString
pCSSComment = P.try $ do
  P.string "/*"
  P.manyTill P.anyChar (P.try (P.string "*/"))
  return B.empty

pCSSOther :: PandocMonad m => ParsecT ByteString () m ByteString
pCSSOther =
  (B.pack <$> P.many1 (P.noneOf "u/ \n\r\t")) <|>
  (B.singleton <$> P.char 'u') <|>
  (B.singleton <$> P.char '/')

pCSSUrl :: PandocMonad m
        => FilePath -> ParsecT ByteString () m ByteString
pCSSUrl d = P.try $ do
  res <- pUrl >>= handleCSSUrl d
  case res of
       Left b -> return b
       Right (mt,b) -> do
         let enc = makeDataURI (mt, b)
         return (B.pack $ "url(" ++ enc ++ ")")

pQuoted :: PandocMonad m
        => ParsecT ByteString () m (String, ByteString)
pQuoted = P.try $ do
  quote <- P.oneOf "\"'"
  url <- P.manyTill P.anyChar (P.char quote)
  let fallback = B.pack ([quote] ++ trim url ++ [quote])
  return (url, fallback)

pUrl :: PandocMonad m
     => ParsecT ByteString () m (String, ByteString)
pUrl = P.try $ do
  P.string "url("
  P.spaces
  quote <- P.option Nothing (Just <$> P.oneOf "\"'")
  url <- P.manyTill P.anyChar (maybe (P.lookAhead (P.char ')')) P.char quote)
  P.spaces
  P.char ')'
  let fallback = B.pack ("url(" ++ maybe "" (:[]) quote ++ trim url ++
                            maybe "" (:[]) quote ++ ")")
  return (url, fallback)

handleCSSUrl :: PandocMonad m
             => FilePath -> (String, ByteString)
             -> ParsecT ByteString () m
                  (Either ByteString (MimeType, ByteString))
handleCSSUrl d (url, fallback) =
  case escapeURIString (/='|') (trim url) of
      '#':_ -> return $ Left fallback
      'd':'a':'t':'a':':':_ -> return $ Left fallback
      u ->  do let url' = if isURI u then u else d </> u
               res <- lift $ getData "" url'
               case res of
                    Left uri -> return $ Left (B.pack $ "url(" ++ uri ++ ")")
                    Right (mt, raw) -> do
                      -- note that the downloaded CSS may
                      -- itself contain url(...).
                      b <- if "text/css" `isPrefixOf` mt
                              then cssURLs d raw
                              else return raw
                      return $ Right (mt, b)

getDataURI :: PandocMonad m => MimeType -> String -> m String
getDataURI mimetype src = do
  res <- getData mimetype src
  case res of
       Left uri -> return uri
       Right x  -> return $ makeDataURI x

getData :: PandocMonad m
        => MimeType -> String
        -> m (Either String (MimeType, ByteString))
getData _ src@('d':'a':'t':'a':':':_) = return $ Left src-- already data: uri
getData mimetype src = do
  let ext = map toLower $ takeExtension src
  (raw, respMime) <- fetchItem src
  let raw' = if ext == ".gz"
                then B.concat $ L.toChunks $ Gzip.decompress $ L.fromChunks [raw]
                else raw
  mime <- case (mimetype, respMime) of
                  ("",Nothing) -> throwError $ PandocSomeError
                         $ "Could not determine mime type for `" ++ src ++ "'"
                  (x, Nothing) -> return x
                  (_, Just x ) -> return x
  result <- if "text/css" `isPrefixOf` mime
               then do
                 oldInputs <- getInputFiles
                 setInputFiles [src]
                 res <- cssURLs (takeDirectory src) raw'
                 setInputFiles oldInputs
                 return res
               else return raw'
  return $ Right (mime, result)



-- | Convert HTML into self-contained HTML, incorporating images,
-- scripts, and CSS using data: URIs.
makeSelfContained :: PandocMonad m => String -> m String
makeSelfContained inp = do
  let tags = parseTags inp
  out' <- convertTags tags
  return $ renderTags' out'
