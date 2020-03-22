{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{- |
   Module      : Text.Pandoc.SelfContained
   Copyright   : Copyright (C) 2011-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions for converting an HTML file into one that can be viewed
offline, by incorporating linked images, CSS, and scripts into
the HTML using data URIs.
-}
module Text.Pandoc.SelfContained ( makeDataURI, makeSelfContained ) where
import Codec.Compression.GZip as Gzip
import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Char (isAlphaNum, isAscii)
import Network.URI (escapeURIString)
import System.FilePath (takeDirectory, takeExtension, (</>))
import Text.HTML.TagSoup
import Text.Pandoc.Class.PandocMonad (PandocMonad (..), fetchItem,
                                      getInputFiles, report, setInputFiles)
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.Shared (isURI, renderTags', trim)
import Text.Pandoc.UTF8 (toString, toText, fromText)
import Text.Parsec (ParsecT, runParserT)
import qualified Text.Parsec as P

isOk :: Char -> Bool
isOk c = isAscii c && isAlphaNum c

makeDataURI :: (MimeType, ByteString) -> T.Text
makeDataURI (mime, raw) =
  if textual
     then "data:" <> mime' <> "," <> T.pack (escapeURIString isOk (toString raw))
     else "data:" <> mime' <> ";base64," <> toText (encode raw)
  where textual = "text/" `T.isPrefixOf` mime
        mime' = if textual && T.any (== ';') mime
                   then mime <> ";charset=utf-8"
                   else mime  -- mime type already has charset

convertTags :: PandocMonad m => [Tag T.Text] -> m [Tag T.Text]
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
       ""  -> (t:) <$> convertTags ts
       src -> do
           let typeAttr = fromAttrib "type" t
           res <- getData typeAttr src
           rest <- convertTags ts
           case res of
                Left dataUri -> return $ TagOpen "script"
                     (("src",dataUri) : [(x,y) | (x,y) <- as, x /= "src"]) :
                     TagClose "script" : rest
                Right (mime, bs)
                  | ("text/javascript" `T.isPrefixOf` mime ||
                     "application/javascript" `T.isPrefixOf` mime ||
                     "application/x-javascript" `T.isPrefixOf` mime) &&
                     not ("</script" `B.isInfixOf` bs) ->
                     return $
                       TagOpen "script" [("type", typeAttr)|not (T.null typeAttr)]
                       : TagText (toText bs)
                       : TagClose "script"
                       : rest
                  | otherwise ->
                       return  $ TagOpen "script"
                         (("src",makeDataURI (mime, bs)) :
                          [(x,y) | (x,y) <- as, x /= "src"]) :
                        TagClose "script" : rest
convertTags (t@(TagOpen "link" as):ts) =
  case fromAttrib "href" t of
       ""  -> (t:) <$> convertTags ts
       src -> do
           res <- getData (fromAttrib "type" t) src
           case res of
                Left dataUri -> do
                  rest <- convertTags ts
                  return $ TagOpen "link"
                     (("href",dataUri) : [(x,y) | (x,y) <- as, x /= "href"]) :
                     rest
                Right (mime, bs)
                  | "text/css" `T.isPrefixOf` mime
                    && T.null (fromAttrib "media" t)
                    && not ("</" `B.isInfixOf` bs) -> do
                      rest <- convertTags $
                                 dropWhile (==TagClose "link") ts
                      return $
                       TagOpen "style" [("type", "text/css")] -- see #5725
                       : TagText (toText bs)
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
         report $ CouldNotParseCSS $ T.pack $ show e
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
         return $ fromText $ "url(" <> enc <> ")"

pQuoted :: PandocMonad m
        => ParsecT ByteString () m (T.Text, ByteString)
pQuoted = P.try $ do
  quote <- P.oneOf "\"'"
  url <- T.pack <$> P.manyTill P.anyChar (P.char quote)
  let fallback = fromText $ T.singleton quote <> trim url <> T.singleton quote
  return (url, fallback)

pUrl :: PandocMonad m
     => ParsecT ByteString () m (T.Text, ByteString)
pUrl = P.try $ do
  P.string "url("
  P.spaces
  quote <- P.option Nothing (Just <$> P.oneOf "\"'")
  url <- T.pack <$> P.manyTill P.anyChar (maybe (P.lookAhead (P.char ')')) P.char quote)
  P.spaces
  P.char ')'
  let fallback = fromText ("url(" <> maybe "" T.singleton quote <> trim url <>
                            maybe "" T.singleton quote <> ")")
  return (url, fallback)

handleCSSUrl :: PandocMonad m
             => FilePath -> (T.Text, ByteString)
             -> ParsecT ByteString () m
                  (Either ByteString (MimeType, ByteString))
handleCSSUrl d (url, fallback) =
  case escapeURIString (/='|') (T.unpack $ trim url) of
      '#':_ -> return $ Left fallback
      'd':'a':'t':'a':':':_ -> return $ Left fallback
      u ->  do let url' = if isURI (T.pack u) then T.pack u else T.pack (d </> u)
               res <- lift $ getData "" url'
               case res of
                    Left uri -> return $ Left (fromText $ "url(" <> uri <> ")")
                    Right (mt', raw) -> do
                      -- note that the downloaded CSS may
                      -- itself contain url(...).
                      (mt, b) <- if "text/css" `T.isPrefixOf` mt'
                                    -- see #5725: in HTML5, content type
                                    -- isn't allowed on style type attribute
                                    then ("text/css",) <$> cssURLs d raw
                                    else return (mt', raw)
                      return $ Right (mt, b)

getDataURI :: PandocMonad m => MimeType -> T.Text -> m T.Text
getDataURI mimetype src = do
  res <- getData mimetype src
  case res of
       Left uri -> return uri
       Right x  -> return $ makeDataURI x

getData :: PandocMonad m
        => MimeType -> T.Text
        -> m (Either T.Text (MimeType, ByteString))
getData mimetype src
  | "data:" `T.isPrefixOf` src = return $ Left src -- already data: uri
  | otherwise = do
      let ext = T.toLower $ T.pack $ takeExtension $ T.unpack src
      (raw, respMime) <- fetchItem src
      let raw' = if ext `elem` [".gz", ".svgz"]
                 then B.concat $ L.toChunks $ Gzip.decompress $ L.fromChunks [raw]
                 else raw
      mime <- case (mimetype, respMime) of
                ("",Nothing) -> throwError $ PandocSomeError
                  $ "Could not determine mime type for `" <> src <> "'"
                (x, Nothing) -> return x
                (_, Just x ) -> return x
      result <- if "text/css" `T.isPrefixOf` mime
                then do
                  oldInputs <- getInputFiles
                  setInputFiles [T.unpack src]
                  res <- cssURLs (takeDirectory $ T.unpack src) raw'
                  setInputFiles oldInputs
                  return res
               else return raw'
      return $ Right (mime, result)



-- | Convert HTML into self-contained HTML, incorporating images,
-- scripts, and CSS using data: URIs.
makeSelfContained :: PandocMonad m => T.Text -> m T.Text
makeSelfContained inp = do
  let tags = parseTags inp
  out' <- convertTags tags
  return $ renderTags' out'
