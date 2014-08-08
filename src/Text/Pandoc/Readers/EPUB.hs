{-# LANGUAGE
   ViewPatterns
 , StandaloneDeriving
 , TupleSections
 , FlexibleContexts  #-}

module Text.Pandoc.Readers.EPUB
  (readEPUB)
  where

import Text.XML.Light
import Text.Pandoc.Definition hiding (Attr)
import Text.Pandoc.Walk (walk, query)
import Text.Pandoc.Generic(bottomUp)
import Text.Pandoc.Readers.HTML (readHtml)
import Text.Pandoc.Options ( ReaderOptions(..)
                           , Extension (Ext_epub_html_exts)
                           , readerTrace)
import Text.Pandoc.Shared (escapeURI)
import Text.Pandoc.MediaBag (MediaBag, insertMedia)
import Text.Pandoc.Compat.Except (MonadError, throwError, runExcept, Except)
import qualified Text.Pandoc.Builder as B
import Codec.Archive.Zip ( Archive (..), toArchive, fromEntry
                         , findEntryByPath, Entry)
import qualified Data.ByteString.Lazy as BL (ByteString)
import System.FilePath ( takeFileName, (</>), dropFileName, normalise
                       , joinPath, dropFileName, splitDirectories
                       , splitFileName )
import qualified Text.Pandoc.UTF8 as UTF8 (toStringLazy)
import Control.Applicative ((<$>))
import Control.Monad (guard, liftM, when)
import Data.Monoid (mempty, (<>))
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Set as S
import qualified Data.Map as M (Map, lookup, fromList, elems)
import Control.DeepSeq.Generics (deepseq, NFData)

import Debug.Trace (trace)

type MIME = String

type Items = M.Map String (FilePath, MIME)

readEPUB :: ReaderOptions -> BL.ByteString -> (Pandoc, MediaBag)
readEPUB opts bytes = runEPUB (archiveToEPUB opts $ toArchive bytes)

runEPUB :: Except String a -> a
runEPUB = either error id . runExcept

-- Note that internal reference are aggresively normalised so that all ids
-- are of the form "filename#id"
--
archiveToEPUB :: (MonadError String m) => ReaderOptions -> Archive -> m (Pandoc, MediaBag)
archiveToEPUB os archive = do
  (root, content) <- getManifest archive
  meta  <- parseMeta content
  (cover, items) <- parseManifest content
  let coverDoc = fromMaybe mempty (imageToPandoc <$> cover)
  spine <- parseSpine items content
  let escapedSpine = map (escapeURI . takeFileName . fst) spine
  Pandoc _ bs <-
      foldM' (\a b -> ((a <>) . bottomUp (prependHash escapedSpine))
        `liftM` parseSpineElem root b) mempty spine
  let ast = coverDoc <> (Pandoc meta bs)
  let mediaBag = fetchImages (M.elems items) root archive ast
  return $ (ast, mediaBag)
  where
    rs = readerExtensions os
    os' = os {readerExtensions = foldr S.insert rs [Ext_epub_html_exts]}
    parseSpineElem :: MonadError String m => FilePath -> (FilePath, MIME) -> m Pandoc
    parseSpineElem (normalise -> r) (normalise -> path, mime) = do
      when (readerTrace os) (traceM path)
      doc <- mimeToReader mime r path
      let docSpan = B.doc $ B.para $ B.spanWith (takeFileName path, [], []) mempty
      return $ docSpan <> doc
    mimeToReader :: MonadError String m => MIME -> FilePath -> FilePath ->  m Pandoc
    mimeToReader "application/xhtml+xml" r path = do
      fname <- findEntryByPathE (r </> path) archive
      return $ fixInternalReferences (r </> path) .
                readHtml os' .
                  UTF8.toStringLazy $
                    fromEntry fname
    mimeToReader s _ path
      | s `elem` imageMimes = return $ imageToPandoc path
      | otherwise = return $ mempty

fetchImages :: [(FilePath, MIME)]
            -> FilePath
            -> Archive
            -> Pandoc
            -> MediaBag
fetchImages mimes root arc (query iq -> links) =
    foldr (uncurry3 insertMedia) mempty
      (mapMaybe getEntry links)
  where
    getEntry (normalise -> l) =
      let mediaPos = normalise (root </> l) in
        (l , lookup mediaPos mimes, ) . fromEntry
          <$> findEntryByPath mediaPos arc

iq :: Inline -> [FilePath]
iq (Image _ (url, _)) = [url]
iq _ = []

-- Remove relative paths
renameImages :: FilePath -> Inline -> Inline
renameImages root (Image a (url, b)) = Image a (collapse (root </> url), b)
renameImages _ x = x

collapse :: FilePath -> FilePath
collapse = joinPath . reverse . foldl go [] . splitDirectories
  where
    go rs "." = rs
    go r@(p:rs) ".." = case p of
                            ".." -> ("..":r)
                            "/" -> ("..":r)
                            _ -> rs
    go _ "/" = ["/"]
    go rs x = x:rs

imageToPandoc :: FilePath -> Pandoc
imageToPandoc s = B.doc . B.para $ B.image s "" mempty

imageMimes :: [String]
imageMimes = ["image/gif", "image/jpeg", "image/png"]

type CoverImage = FilePath

parseManifest :: (MonadError String m) => Element -> m (Maybe CoverImage, Items)
parseManifest content = do
  manifest <- findElementE (dfName "manifest") content
  let items = findChildren (dfName "item") manifest
  r <- mapM parseItem items
  let cover = findAttr (emptyName "href") =<< filterChild findCover manifest
  return (cover, (M.fromList r))
  where
    findCover e = maybe False (isInfixOf "cover-image")
                  (findAttr (emptyName "properties") e)
    parseItem e = do
      uid <- findAttrE (emptyName "id") e
      href <- findAttrE (emptyName "href") e
      mime <- findAttrE (emptyName "media-type") e
      return (uid, (href, mime))

parseSpine :: MonadError String m => Items -> Element -> m [(FilePath, MIME)]
parseSpine is e = do
  spine <- findElementE (dfName "spine") e
  let itemRefs = findChildren (dfName "itemref") spine
  mapM (mkE "parseSpine" . (flip M.lookup is)) $ mapMaybe parseItemRef itemRefs
  where
    parseItemRef ref = do
      let linear = maybe True (== "yes") (findAttr (emptyName "linear") ref)
      guard linear
      findAttr (emptyName "idref") ref

parseMeta :: MonadError String m => Element -> m Meta
parseMeta content = do
  meta <- findElementE (dfName "metadata") content
  let dcspace (QName _ (Just "http://purl.org/dc/elements/1.1/") (Just "dc")) = True
      dcspace _ = False
  let dcs = filterChildrenName dcspace meta
  let r = foldr parseMetaItem nullMeta dcs
  return r

-- http://www.idpf.org/epub/30/spec/epub30-publications.html#sec-metadata-elem
parseMetaItem :: Element -> Meta -> Meta
parseMetaItem e@(stripNamespace . elName -> field) meta =
  B.setMeta (renameMeta field) (B.str $ strContent e) meta

renameMeta :: String -> String
renameMeta "creator" = "author"
renameMeta s = s

getManifest :: MonadError String m => Archive -> m (String, Element)
getManifest archive = do
  metaEntry <- findEntryByPathE ("META-INF" </> "container.xml") archive
  docElem <- (parseXMLDocE . UTF8.toStringLazy . fromEntry) metaEntry
  let namespaces = mapMaybe attrToNSPair (elAttribs docElem)
  ns <- mkE "xmlns not in namespaces" (lookup "xmlns" namespaces)
  as <- liftM ((map attrToPair) . elAttribs)
    (findElementE (QName "rootfile" (Just ns) Nothing) docElem)
  root <- mkE "Root not found" (lookup "full-path" as)
  let rootdir = dropFileName root
  --mime <- lookup "media-type" as
  manifest <- findEntryByPathE root archive
  liftM ((,) rootdir) (parseXMLDocE . UTF8.toStringLazy . fromEntry $ manifest)

-- Fixup

fixInternalReferences :: FilePath -> Pandoc -> Pandoc
fixInternalReferences pathToFile =
  (walk $ renameImages root)
  . (walk normalisePath)
  . (walk $ fixBlockIRs filename)
  . (walk $ fixInlineIRs filename)
  where
    (root, escapeURI -> filename) = splitFileName pathToFile

fixInlineIRs :: String -> Inline -> Inline
fixInlineIRs s (Span as v) =
  Span (fixAttrs s as) v
fixInlineIRs s (Code as code) =
  Code (fixAttrs s as) code
fixInlineIRs s (Link t ('#':url, tit)) =
  Link t (addHash s url, tit)
fixInlineIRs _ v = v

normalisePath :: Inline -> Inline
normalisePath (Link t (url, tit)) =
  let (path, uid) = span (/= '#') url in
  Link t (takeFileName path ++ uid, tit)
normalisePath s = s

prependHash :: [String] -> Inline -> Inline
prependHash ps l@(Link is (url, tit))
  | or [s `isPrefixOf` url | s <- ps] =
    Link is ('#':url, tit)
  | otherwise = l
prependHash _ i = i

fixBlockIRs :: String -> Block -> Block
fixBlockIRs s (Div as b) =
  Div (fixAttrs s as) b
fixBlockIRs s (Header i as b) =
  Header i (fixAttrs s as) b
fixBlockIRs s (CodeBlock as code) =
  CodeBlock (fixAttrs s as) code
fixBlockIRs _ b = b

fixAttrs :: FilePath -> B.Attr -> B.Attr
fixAttrs s (ident, cs, kvs) = (addHash s ident, filter (not . null) cs, removeEPUBAttrs kvs)

addHash :: String -> String -> String
addHash _ "" = ""
addHash s ident = s ++ "#" ++ ident

removeEPUBAttrs :: [(String, String)] -> [(String, String)]
removeEPUBAttrs kvs = filter (not . isEPUBAttr) kvs

isEPUBAttr :: (String, String) -> Bool
isEPUBAttr (k, _) = "epub:" `isPrefixOf` k

-- Library

-- Strict version of foldM
foldM' :: (Monad m, NFData a) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `deepseq` foldM' f z' xs

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

traceM :: Monad m => String -> m ()
traceM = flip trace (return ())

-- Utility

stripNamespace :: QName -> String
stripNamespace (QName v _ _) = v

attrToNSPair :: Attr -> Maybe (String, String)
attrToNSPair (Attr (QName "xmlns" _ _) val) = Just ("xmlns", val)
attrToNSPair _ = Nothing

attrToPair :: Attr -> (String, String)
attrToPair (Attr (QName name _ _) val) = (name, val)

defaultNameSpace :: Maybe String
defaultNameSpace = Just "http://www.idpf.org/2007/opf"

dfName :: String -> QName
dfName s = QName s defaultNameSpace Nothing

emptyName :: String -> QName
emptyName s = QName s Nothing Nothing

-- Convert Maybe interface to Either

findAttrE :: MonadError String m => QName -> Element -> m String
findAttrE q e = mkE "findAttr" $ findAttr q e

findEntryByPathE :: MonadError String m => FilePath -> Archive -> m Entry
findEntryByPathE path a = mkE ("No entry on path: " ++ path) $ findEntryByPath path a

parseXMLDocE :: MonadError String m => String -> m Element
parseXMLDocE doc = mkE "Unable to parse XML doc" $ parseXMLDoc doc

findElementE :: MonadError String m => QName -> Element -> m Element
findElementE e x = mkE ("Unable to find element: " ++ show e) $ findElement e x

mkE :: MonadError String m => String -> Maybe a -> m a
mkE s = maybe (throwError s) return
