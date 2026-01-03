{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}
{- |
   Module      : Text.Pandoc.SelfContained
   Copyright   : Copyright (C) 2011-2024 John MacFarlane
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
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Char (isAlphaNum, isAscii)
import Crypto.Hash (hashWith, SHA1(SHA1))
import Network.URI (escapeURIString)
import System.FilePath (takeDirectory, takeExtension, (</>))
import Text.HTML.TagSoup
import Text.Pandoc.Class.PandocMonad (PandocMonad (..), fetchItem,
                                      getInputFiles, report, setInputFiles)
import Text.Pandoc.Logging
import Text.Pandoc.Error (PandocError(..))
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.Shared (renderTags', trim, tshow, safeRead)
import Text.Pandoc.URI (isURI)
import Text.Pandoc.UTF8 (toString, toText, fromText)
import Text.Pandoc.Parsing (ParsecT, runParserT)
import qualified Text.Pandoc.Parsing as P
import Control.Monad.Except (throwError, catchError)
import Data.Either (lefts, rights)
import Data.Maybe (isNothing)
import qualified Data.Map as M
import Control.Monad.State

isOk :: Char -> Bool
isOk c = isAscii c && isAlphaNum c

makeDataURI :: (MimeType, ByteString) -> T.Text
makeDataURI (mime, raw) =
  if textual
     then "data:" <> mime' <> "," <> T.pack (escapeURIString isOk (toString raw))
     else "data:" <> mime' <> ";base64," <> toText (encode raw')
  where textual = "text/" `T.isPrefixOf` mime
        raw' = if "+xml" `T.isSuffixOf` mime
                  then B.filter (/= '\r') raw  -- strip off CRs
                  else raw
        mime' = if textual && T.any (== ';') mime
                   then mime <> ";charset=utf-8"
                   else mime  -- mime type already has charset

isSourceAttribute :: T.Text -> (T.Text, T.Text) -> Bool
isSourceAttribute tagname (x,_) =
  x == "src" ||
  x == "data-src" ||
  (x == "href" && tagname == "link") ||
  x == "poster" ||
  x == "data-background-image"

data ConvertState =
  ConvertState
  { isHtml5 :: Bool
  , svgMap  :: M.Map T.Text (T.Text, [Attribute T.Text])
    -- map from hash to (id, svg attributes)
  } deriving (Show)

convertTags :: PandocMonad m =>
               [Tag T.Text] -> StateT ConvertState m [Tag T.Text]
convertTags [] = return []
convertTags (t@TagOpen{}:ts)
  | fromAttrib "data-external" t == "1" = (t:) <$> convertTags ts
convertTags (t@(TagOpen "style" _):ts) =
  case span isTagText ts of
    (xs,rest) -> do
      xs' <- mapM (\case
                    TagText s -> TagText . toText <$> cssURLs "" (fromText s)
                    tag -> return tag) xs
      ((t:xs') ++) <$> convertTags rest
convertTags (t@(TagOpen "script" as):tc@(TagClose "script"):ts) =
  case fromAttrib "src" t of
       ""  -> ([t, tc] ++) <$> convertTags ts
       src -> do
           let typeAttr = fromAttrib "type" t
           res <- getData typeAttr src
           rest <- convertTags ts
           case res of
                AlreadyDataURI dataUri -> return $ TagOpen "script"
                     (("src",dataUri) : [(x,y) | (x,y) <- as, x /= "src"]) :
                     TagClose "script" : rest
                Fetched (mime, bs)
                  | ("text/javascript" `T.isPrefixOf` mime ||
                     "application/javascript" `T.isPrefixOf` mime ||
                     "application/x-javascript" `T.isPrefixOf` mime) &&
                     not ("</script" `B.isInfixOf` bs) ->
                     return $
                       TagOpen "script" [(k,v) | (k,v) <- as
                                               , k == "type" ||
                                                 "data-" `T.isPrefixOf` k]
                       : TagText (toText bs)
                       : TagClose "script"
                       : rest
                  | otherwise ->
                       return  $ TagOpen "script"
                         (("src",makeDataURI (mime, bs)) :
                          [(x,y) | (x,y) <- as, x /= "src"]) :
                        TagClose "script" : rest
                CouldNotFetch _ -> return $ t:tc:rest
convertTags (t@(TagOpen "link" as):ts) =
  case fromAttrib "href" t of
       ""  -> (t:) <$> convertTags ts
       src -> do
           res <- getData (fromAttrib "type" t) src
           case res of
                AlreadyDataURI dataUri -> do
                  rest <- convertTags ts
                  return $ TagOpen "link"
                     (("href",dataUri) : [(x,y) | (x,y) <- as, x /= "href"]) :
                     rest
                Fetched (mime, bs)
                  | ("text/css" `T.isPrefixOf` mime ||
                      fromAttrib "rel" t == "stylesheet")
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
                CouldNotFetch _ -> do
                      rest <- convertTags ts
                      return $ t:rest
convertTags (t@(TagOpen tagname as):ts)
  | any (isSourceAttribute tagname) as
     = do
       let inlineSvgs = tagname == "img" &&
                        case T.words <$> lookup "class" as of
                          Nothing -> False
                          Just cs -> "inline-svg" `elem` cs
       as' <- mapM (processAttribute inlineSvgs) as
       let attrs = addRole "img" $ addAriaLabel $ rights as'
       let svgContents = lefts as'
       rest <- convertTags ts
       case svgContents of
         [] -> return $ TagOpen tagname attrs : rest
         ((hash, tags) : _) -> do
             -- drop "</img>" if present
             let rest' = case rest of
                           TagClose tn : xs | tn == tagname ->  xs
                           _ -> rest
             svgmap <- gets svgMap
             case M.lookup hash svgmap of
               Just (svgid, svgattrs) -> do
                 let attrs' = [(k,v) | (k,v) <- combineSvgAttrs svgattrs attrs
                                     , k /= "id"]
                 return $ TagOpen "svg" attrs' :
                          TagOpen "use" [("href", "#" <> svgid),
                                         ("width", "100%"),
                                         ("height", "100%")] :
                          TagClose "use" :
                          TagClose "svg" :
                          rest'
               Nothing ->
                  case dropWhile (not . isTagOpenName "svg") tags of
                    TagOpen "svg" svgattrs : tags' -> do
                      let attrs' = combineSvgAttrs svgattrs attrs
                      let svgid = case lookup "id" attrs' of
                                     Just id' -> id'
                                     Nothing -> "svg_" <> hash
                      let attrs'' = ("id", svgid) :
                                    [(k,v) | (k,v) <- attrs', k /= "id"]
                      modify $ \st ->
                        st{ svgMap = M.insert hash (svgid, attrs'') (svgMap st) }
                      let fixUrl x =
                            case T.breakOn "url(#" x of
                              (_,"") -> x
                              (before, after) -> before <>
                                  "url(#" <> svgid <> "_" <> T.drop 5 after
                      let addIdPrefix ("id", x) = ("id", svgid <> "_" <> x)
                          addIdPrefix (k, x)
                           | k == "xlink:href" || k == "href" =
                            case T.uncons x of
                              Just ('#', x') -> (k, "#" <> svgid <> "_" <> x')
                              _ -> (k, x)
                          -- this clause handles things like
                          -- style="fill:url(#radialGradient46);stroke:none",
                          -- adding the svg id prefix to the anchor:
                          addIdPrefix (k, x) = (k, fixUrl x)
                      let ensureUniqueId (TagOpen tname ats) =
                            TagOpen tname (map addIdPrefix ats)
                          ensureUniqueId x = x
                      return $ TagOpen "svg" attrs'' :
                                 map ensureUniqueId tags' ++ rest'
                    _ -> return $ TagOpen tagname attrs : rest
  where processAttribute inlineSvgs (x,y) =
           if isSourceAttribute tagname (x,y)
              then do
                res <- getData (fromAttrib "type" t) y
                case res of
                  AlreadyDataURI enc -> return $ Right (x, enc)
                  Fetched ("image/svg+xml", bs) | inlineSvgs -> do
                    -- we filter CR in the hash to ensure that Windows
                    -- and non-Windows tests agree:
                    let hash = T.pack $ take 20 $ show $ hashWith SHA1
                                             $ B.filter (/='\r') bs
                    return $ Left (hash, getSvgTags (toText bs))
                  Fetched (mt,bs) -> return $ Right (x, makeDataURI (mt,bs))
                  CouldNotFetch _ -> return $ Right (x, y)
              else return $ Right (x,y)

convertTags (t:ts) = (t:) <$> convertTags ts

addRole :: T.Text -> [(T.Text, T.Text)] -> [(T.Text, T.Text)]
addRole role attrs =
  case lookup "role" attrs of
    Nothing -> ("role", role) : attrs
    Just _ -> attrs

addAriaLabel :: [(T.Text, T.Text)] -> [(T.Text, T.Text)]
addAriaLabel attrs =
  case lookup "aria-label" attrs of
    Just _ -> attrs
    Nothing -> case lookup "alt" attrs of
                 Just alt -> ("aria-label", alt) : attrs
                 Nothing -> attrs

-- we want to drop spaces, <?xml>, and comments before <svg>
-- and anything after </svg>:
getSvgTags :: T.Text -> [Tag T.Text]
getSvgTags t =
  case takeWhile (not . isTagCloseName "svg") .
       dropWhile (not . isTagOpenName "svg") $ parseTags t of
    [] -> []
    xs -> xs ++ [TagClose "svg"]

combineSvgAttrs :: [(T.Text, T.Text)] -> [(T.Text, T.Text)] -> [(T.Text, T.Text)]
combineSvgAttrs svgAttrs imgAttrs =
  case (mbViewBox, mbHeight, mbWidth) of
    (Nothing, Just h, Just w) -> -- calculate viewBox
      combinedAttrs ++ [("viewBox", T.unwords ["0", "0", tshow w, tshow h])]
    (Just (_minx,_miny,w,h), Nothing, Nothing) ->
        combinedAttrs ++
        [ ("width", dropPointZero (tshow w)) |
            isNothing (lookup "width" combinedAttrs) ] ++
        [ ("height", dropPointZero (tshow h)) |
            isNothing (lookup "height" combinedAttrs) ]
    _ -> combinedAttrs
 where
  dropPointZero t = case T.stripSuffix ".0" t of
                       Nothing -> t
                       Just t' -> t'
  combinedAttrs =
    [(k, v) | (k, v) <- imgAttrs
            , k /= "class"] ++
    [(k, v) | (k, v) <- svgAttrs
            , isNothing (lookup k imgAttrs)
            , k `notElem` ["xmlns", "xmlns:xlink", "version", "class"]] ++
    mergedClasses
  parseViewBox t =
    case map (safeRead . addZero) $ T.words t of
      [Just llx, Just lly, Just urx, Just ury] -> Just (llx, lly, urx, ury)
      _ -> Nothing
  addZero t =
    if "-." `T.isPrefixOf` t
       then "-0." <> T.drop 2 t -- safeRead fails on -.33, needs -0.33
       else t
  (mbViewBox :: Maybe (Double, Double, Double, Double)) =
        lookup "viewBox" svgAttrs >>= parseViewBox
  (mbHeight :: Maybe Int) = lookup "height" combinedAttrs >>= safeRead
  (mbWidth :: Maybe Int) = lookup "width" combinedAttrs >>= safeRead
  mergedClasses = case (lookup "class" imgAttrs, lookup "class" svgAttrs) of
                    (Just c1, Just c2) -> [("class", c1 <> " " <> c2)]
                    _ -> []

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
                    AlreadyDataURI uri -> return $ Left (fromText $ "url(" <> uri <> ")")
                    Fetched (mt', raw) -> do
                      -- note that the downloaded CSS may
                      -- itself contain url(...).
                      (mt, b) <- if "text/css" `T.isPrefixOf` mt'
                                    -- see #5725: in HTML5, content type
                                    -- isn't allowed on style type attribute
                                    then ("text/css",) <$> cssURLs d raw
                                    else return (mt', raw)
                      return $ Right (mt, b)
                    CouldNotFetch _ -> return $ Left fallback

data GetDataResult =
    AlreadyDataURI T.Text
  | CouldNotFetch PandocError
  | Fetched (MimeType, ByteString)
  deriving (Show)

getData :: PandocMonad m
        => MimeType -> T.Text
        -> m GetDataResult
getData mimetype src
  | "data:" `T.isPrefixOf` src = return $ AlreadyDataURI src -- already data: uri
  | otherwise = catchError fetcher handler
 where
   fetcher = do
      let ext = T.toLower $ T.pack $ takeExtension $ T.unpack src
      (raw, respMime) <- fetchItem src
      let raw' = if ext `elem` [".gz", ".svgz"]
                 then B.concat $ L.toChunks $ Gzip.decompress $ L.fromChunks [raw]
                 else raw
      let mime = case (mimetype, respMime) of
                  ("",Nothing) -> "application/octet-stream"
                  (x, Nothing) -> x
                  (_, Just x ) -> x
      result <- if "text/css" `T.isPrefixOf` mime
                then do
                  oldInputs <- getInputFiles
                  setInputFiles [T.unpack src]
                  res <- cssURLs (takeDirectory $ T.unpack src) raw'
                  setInputFiles oldInputs
                  return res
               else return raw'
      return $ Fetched (mime, result)
   handler e
      -- If fetch failed and we have a fragment and/or query,
      -- try the fetch again without these, since the resource
      -- may be local (see #1477, #11021)
     | T.any (\c -> c == '?' || c == '#') src && not (isURI src)
       = getData mimetype (removeQueryAndFragment src)
     | otherwise
       = case e of
          PandocResourceNotFound r -> do
            report $ CouldNotFetchResource r ""
            return $ CouldNotFetch e
          PandocHttpError u er -> do
            report $ CouldNotFetchResource u er
            return $ CouldNotFetch e
          _ -> throwError e
   removeQueryAndFragment = T.takeWhile (\c -> c /= '#' && c /= '?')

-- | Convert HTML into self-contained HTML, incorporating images,
-- scripts, and CSS using data: URIs.
makeSelfContained :: PandocMonad m => T.Text -> m T.Text
makeSelfContained inp = do
  let tags = parseTags inp
  let html5 = case tags of
                  (TagOpen "!DOCTYPE" [("html","")]:_) -> True
                  _ -> False
  let convertState = ConvertState { isHtml5 = html5,
                                    svgMap = mempty }
  out' <- evalStateT (convertTags tags) convertState
  return $ renderTags' out'
