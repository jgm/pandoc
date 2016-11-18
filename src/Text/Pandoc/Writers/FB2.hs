{-# LANGUAGE PatternGuards #-}

{-
Copyright (c) 2011-2012, Sergey Astanin
All rights reserved.

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

{- | Conversion of 'Pandoc' documents to FB2 (FictionBook2) format.

FictionBook is an XML-based e-book format. For more information see:
<http://www.fictionbook.org/index.php/Eng:XML_Schema_Fictionbook_2.1>

-}
module Text.Pandoc.Writers.FB2 (writeFB2)  where

import Control.Monad.State (StateT, evalStateT, get, modify)
import Control.Monad.State (liftM, liftM2, liftIO)
import Data.ByteString.Base64 (encode)
import Data.Char (toLower, isSpace, isAscii, isControl)
import Data.List (intersperse, intercalate, isPrefixOf, stripPrefix)
import Data.Either (lefts, rights)
import Network.Browser (browse, request, setAllowRedirects, setOutHandler)
import Network.HTTP (catchIO_, getRequest, getHeaders, getResponseBody)
import Network.HTTP (lookupHeader, HeaderName(..), urlEncode)
import Network.URI (isURI, unEscapeString)
import System.FilePath (takeExtension)
import Text.XML.Light
import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Text.XML.Light as X
import qualified Text.XML.Light.Cursor as XC

import Text.Pandoc.Definition
import Text.Pandoc.Options (WriterOptions(..), HTMLMathMethod(..), def)
import Text.Pandoc.Shared (orderedListMarkers, isHeaderBlock, capitalize,
                           linesToPara)

-- | Data to be written at the end of the document:
-- (foot)notes, URLs, references, images.
data FbRenderState = FbRenderState
    { footnotes :: [ (Int, String, [Content]) ]  -- ^ #, ID, text
    , imagesToFetch :: [ (String, String) ]  -- ^ filename, URL or path
    , parentListMarker :: String  -- ^ list marker of the parent ordered list
    , parentBulletLevel :: Int  -- ^ nesting level of the unordered list
    , writerOptions :: WriterOptions
    } deriving (Show)

-- | FictionBook building monad.
type FBM = StateT FbRenderState IO

newFB :: FbRenderState
newFB = FbRenderState { footnotes = [], imagesToFetch = []
                      , parentListMarker = "", parentBulletLevel = 0
                      , writerOptions = def }

data ImageMode = NormalImage | InlineImage deriving (Eq)
instance Show ImageMode where
    show NormalImage = "imageType"
    show InlineImage = "inlineImageType"

-- | Produce an FB2 document from a 'Pandoc' document.
writeFB2 :: WriterOptions    -- ^ conversion options
         -> Pandoc           -- ^ document to convert
         -> IO String        -- ^ FictionBook2 document (not encoded yet)
writeFB2 opts (Pandoc meta blocks) = flip evalStateT newFB $ do
     modify (\s -> s { writerOptions = opts { writerStandalone = True } })
     desc <- description meta
     fp <- frontpage meta
     secs <- renderSections 1 blocks
     let body = el "body" $ fp ++ secs
     notes <- renderFootnotes
     (imgs,missing) <- liftM imagesToFetch get >>= \s -> liftIO (fetchImages s)
     let body' = replaceImagesWithAlt missing body
     let fb2_xml = el "FictionBook" (fb2_attrs, [desc, body'] ++ notes ++ imgs)
     return $ xml_head ++ (showContent fb2_xml) ++ "\n"
  where
  xml_head = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  fb2_attrs =
      let xmlns = "http://www.gribuser.ru/xml/fictionbook/2.0"
          xlink = "http://www.w3.org/1999/xlink"
      in  [ uattr "xmlns" xmlns
          , attr ("xmlns", "l") xlink ]


frontpage :: Meta -> FBM [Content]
frontpage meta' = do
  t <- cMapM toXml . docTitle $ meta'
  return $
    [ el "title" (el "p" t)
    , el "annotation" (map (el "p" . cMap plain)
                       (docAuthors meta' ++ [docDate meta']))
    ]

description :: Meta -> FBM Content
description meta' = do
  bt <- booktitle meta'
  let as = authors meta'
  dd <- docdate meta'
  return $ el "description"
    [ el "title-info" (bt ++ as ++ dd)
    , el "document-info" [ el "program-used" "pandoc" ] -- FIXME: +version
    ]

booktitle :: Meta -> FBM [Content]
booktitle meta' = do
  t <- cMapM toXml . docTitle $ meta'
  return $ if null t
           then []
           else [ el "book-title" t ]

authors :: Meta -> [Content]
authors meta' = cMap author (docAuthors meta')

author :: [Inline] -> [Content]
author ss =
  let ws = words . cMap plain $ ss
      email = (el "email") `fmap` (take 1 $ filter ('@' `elem`) ws)
      ws' = filter ('@' `notElem`) ws
      names = case ws' of
                (nickname:[]) -> [ el "nickname" nickname ]
                (fname:lname:[]) -> [ el "first-name" fname
                                    , el "last-name" lname ]
                (fname:rest) -> [ el "first-name" fname
                                , el "middle-name" (concat . init $ rest)
                                , el "last-name" (last rest) ]
                ([]) -> []
  in  list $ el "author" (names ++ email)

docdate :: Meta -> FBM [Content]
docdate meta' = do
  let ss = docDate meta'
  d <- cMapM toXml ss
  return $ if null d
           then []
           else [el "date" d]

-- | Divide the stream of blocks into sections and convert to XML
-- representation.
renderSections :: Int -> [Block] -> FBM [Content]
renderSections level blocks = do
    let secs = splitSections level blocks
    mapM (renderSection level) secs

renderSection :: Int -> ([Inline], [Block]) -> FBM Content
renderSection level (ttl, body) = do
    title <- if null ttl
            then return []
            else return . list . el "title" . formatTitle $ ttl
    content <- if (hasSubsections body)
               then renderSections (level + 1) body
               else cMapM blockToXml body
    return $ el "section" (title ++ content)
  where
    hasSubsections = any isHeaderBlock

-- | Only <p> and <empty-line> are allowed within <title> in FB2.
formatTitle :: [Inline] -> [Content]
formatTitle inlines =
  let lns = split isLineBreak inlines
      lns' = map (el "p" . cMap plain) lns
  in  intersperse (el "empty-line" ()) lns'

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split cond xs = let (b,a) = break cond xs
                in  (b:split cond (drop 1 a))

isLineBreak :: Inline -> Bool
isLineBreak LineBreak = True
isLineBreak _ = False

-- | Divide the stream of block elements into sections: [(title, blocks)].
splitSections :: Int -> [Block] -> [([Inline], [Block])]
splitSections level blocks = reverse $ revSplit (reverse blocks)
  where
  revSplit [] = []
  revSplit rblocks =
    let (lastsec, before) = break sameLevel rblocks
        (header, prevblocks) =
            case before of
              ((Header n _ title):prevblocks') ->
                  if n == level
                     then (title, prevblocks')
                     else ([], before)
              _ -> ([], before)
    in (header, reverse lastsec) : revSplit prevblocks
  sameLevel (Header n _ _) = n == level
  sameLevel _ = False

-- | Make another FictionBook body with footnotes.
renderFootnotes :: FBM [Content]
renderFootnotes = do
  fns <- footnotes `liftM` get
  if null fns
    then return []  -- no footnotes
    else return . list $
         el "body" ([uattr "name" "notes"], map renderFN (reverse fns))
  where
    renderFN (n, idstr, cs) =
        let fn_texts = (el "title" (el "p" (show n))) : cs
        in  el "section" ([uattr "id" idstr], fn_texts)

-- | Fetch images and encode them for the FictionBook XML.
-- Return image data and a list of hrefs of the missing images.
fetchImages :: [(String,String)] -> IO ([Content],[String])
fetchImages links = do
    imgs <- mapM (uncurry fetchImage) links
    return $ (rights imgs, lefts imgs)

-- | Fetch image data from disk or from network and make a <binary> XML section.
-- Return either (Left hrefOfMissingImage) or (Right xmlContent).
fetchImage :: String -> String -> IO (Either String Content)
fetchImage href link = do
  mbimg <-
      case (isURI link, readDataURI link) of
       (True, Just (mime,_,True,base64)) ->
           let mime' = map toLower mime
           in if mime' == "image/png" || mime' == "image/jpeg"
              then return (Just (mime',base64))
              else return Nothing
       (True, Just _) -> return Nothing  -- not base64-encoded
       (True, Nothing) -> fetchURL link
       (False, _) -> do
        d <- nothingOnError $ B.readFile (unEscapeString link)
        let t = case map toLower (takeExtension link) of
                  ".png" -> Just "image/png"
                  ".jpg" -> Just "image/jpeg"
                  ".jpeg" -> Just "image/jpeg"
                  ".jpe" -> Just "image/jpeg"
                  _ -> Nothing  -- only PNG and JPEG are supported in FB2
        return $ liftM2 (,) t (liftM (toStr . encode) d)
  case mbimg of
    Just (imgtype, imgdata) -> do
        return . Right $ el "binary"
                   ( [uattr "id" href
                     , uattr "content-type" imgtype]
                   , txt imgdata )
    _ -> return (Left ('#':href))
  where
   nothingOnError :: (IO B.ByteString) -> (IO (Maybe B.ByteString))
   nothingOnError action = liftM Just action `E.catch` omnihandler
   omnihandler :: E.SomeException -> IO (Maybe B.ByteString)
   omnihandler _ = return Nothing

-- | Extract mime type and encoded data from the Data URI.
readDataURI :: String -- ^ URI
            -> Maybe (String,String,Bool,String)
               -- ^ Maybe (mime,charset,isBase64,data)
readDataURI uri =
  case stripPrefix "data:" uri of
    Nothing   -> Nothing
    Just rest ->
      let meta = takeWhile (/= ',') rest  -- without trailing ','
          uridata = drop (length meta + 1) rest
          parts = split (== ';') meta
          (mime,cs,enc)=foldr upd ("text/plain","US-ASCII",False) parts
      in  Just (mime,cs,enc,uridata)

 where
   upd str m@(mime,cs,enc)
       | isMimeType str                          = (str,cs,enc)
       | Just str' <- stripPrefix "charset=" str = (mime,str',enc)
       | str ==  "base64"                        = (mime,cs,True)
       | otherwise                               = m

-- Without parameters like ;charset=...; see RFC 2045, 5.1
isMimeType :: String -> Bool
isMimeType s =
    case split (=='/') s of
      [mtype,msubtype] ->
          ((map toLower mtype) `elem` types
           || "x-" `isPrefixOf` (map toLower mtype))
          && all valid mtype
          && all valid msubtype
      _ -> False
 where
   types =  ["text","image","audio","video","application","message","multipart"]
   valid c = isAscii c && not (isControl c) && not (isSpace c) &&
             c `notElem` "()<>@,;:\\\"/[]?="

-- | Fetch URL, return its Content-Type and binary data on success.
fetchURL :: String -> IO (Maybe (String, String))
fetchURL url = do
  flip catchIO_ (return Nothing) $ do
     r <- browse $ do
           setOutHandler (const (return ()))
           setAllowRedirects True
           liftM snd . request . getRequest $ url
     let content_type = lookupHeader HdrContentType (getHeaders r)
     content <- liftM (Just . toStr . encode . toBS) . getResponseBody $ Right r
     return $ liftM2 (,) content_type content

toBS :: String -> B.ByteString
toBS = B.pack . map (toEnum . fromEnum)

toStr :: B.ByteString -> String
toStr = map (toEnum . fromEnum) . B.unpack

footnoteID :: Int -> String
footnoteID i = "n" ++ (show i)

linkID :: Int -> String
linkID i = "l" ++ (show i)

-- | Convert a block-level Pandoc's element to FictionBook XML representation.
blockToXml :: Block -> FBM [Content]
blockToXml (Plain ss) = cMapM toXml ss  -- FIXME: can lead to malformed FB2
blockToXml (Para [Math DisplayMath formula]) = insertMath NormalImage formula
-- title beginning with fig: indicates that the image is a figure
blockToXml (Para [Image atr alt (src,'f':'i':'g':':':tit)]) =
  insertImage NormalImage (Image atr alt (src,tit))
blockToXml (Para ss) = liftM (list . el "p") $ cMapM toXml ss
blockToXml (CodeBlock _ s) = return . spaceBeforeAfter .
                             map (el "p" . el "code") . lines $ s
blockToXml (RawBlock _ s) = return . spaceBeforeAfter .
                            map (el "p" . el "code") . lines $ s
blockToXml (Div _ bs) = cMapM blockToXml bs
blockToXml (BlockQuote bs) = liftM (list . el "cite") $ cMapM blockToXml bs
blockToXml (LineBlock lns) = blockToXml $ linesToPara lns
blockToXml (OrderedList a bss) = do
    state <- get
    let pmrk = parentListMarker state
    let markers = map ((pmrk ++ " ") ++) $ orderedListMarkers a
    let mkitem mrk bs = do
          modify (\s -> s { parentListMarker = mrk })
          itemtext <- cMapM blockToXml . paraToPlain $ bs
          modify (\s -> s { parentListMarker = pmrk }) -- old parent marker
          return . el "p" $ [ txt mrk, txt " " ] ++ itemtext
    mapM (uncurry mkitem) (zip markers bss)
blockToXml (BulletList bss) = do
    state <- get
    let level = parentBulletLevel state
    let pmrk = parentListMarker state
    let prefix = replicate (length pmrk) ' '
    let bullets = ["\x2022", "\x25e6", "*", "\x2043", "\x2023"]
    let mrk = prefix ++ bullets !! (level `mod` (length bullets))
    let mkitem bs = do
          modify (\s -> s { parentBulletLevel = (level+1) })
          itemtext <- cMapM blockToXml . paraToPlain $ bs
          modify (\s -> s { parentBulletLevel = level }) -- restore bullet level
          return $ el "p" $ [ txt (mrk ++ " ") ] ++ itemtext
    mapM mkitem bss
blockToXml (DefinitionList defs) =
    cMapM mkdef defs
    where
      mkdef (term, bss) = do
          def' <- cMapM (cMapM blockToXml . sep . paraToPlain . map indent) bss
          t <- wrap "strong" term
          return [ el "p" t, el "p" def' ]
      sep blocks =
          if all needsBreak blocks then
              blocks ++ [Plain [LineBreak]]
          else
              blocks
      needsBreak (Para _) = False
      needsBreak (Plain ins) = LineBreak `notElem` ins
      needsBreak _ = True
blockToXml (Header _ _ _) = -- should never happen, see renderSections
                          error "unexpected header in section text"
blockToXml HorizontalRule = return
                            [ el "empty-line" ()
                            , el "p" (txt (replicate 10 '—'))
                            , el "empty-line" () ]
blockToXml (Table caption aligns _ headers rows) = do
    hd <- mkrow "th" headers aligns
    bd <- mapM (\r -> mkrow "td" r aligns) rows
    c <- return . el "emphasis" =<< cMapM toXml caption
    return [el "table" (hd : bd), el "p" c]
    where
      mkrow :: String -> [TableCell] -> [Alignment] -> FBM Content
      mkrow tag cells aligns' =
        (el "tr") `liftM` (mapM (mkcell tag) (zip cells aligns'))
      --
      mkcell :: String -> (TableCell, Alignment) -> FBM Content
      mkcell tag (cell, align) = do
        cblocks <- cMapM blockToXml cell
        return $ el tag ([align_attr align], cblocks)
      --
      align_attr a = Attr (QName "align" Nothing Nothing) (align_str a)
      align_str AlignLeft = "left"
      align_str AlignCenter = "center"
      align_str AlignRight = "right"
      align_str AlignDefault = "left"
blockToXml Null = return []

-- Replace paragraphs with plain text and line break.
-- Necessary to simulate multi-paragraph lists in FB2.
paraToPlain :: [Block] -> [Block]
paraToPlain [] = []
paraToPlain (Para inlines : rest) =
    let p = (Plain (inlines ++ [LineBreak]))
    in  p : paraToPlain rest
paraToPlain (p:rest) = p : paraToPlain rest

-- Simulate increased indentation level. Will not really work
-- for multi-line paragraphs.
indent :: Block -> Block
indent = indentBlock
  where
  -- indentation space
  spacer :: String
  spacer = replicate 4 ' '
  --
  indentBlock (Plain ins) = Plain ((Str spacer):ins)
  indentBlock (Para ins) = Para ((Str spacer):ins)
  indentBlock (CodeBlock a s) =
    let s' = unlines . map (spacer++) . lines $ s
    in  CodeBlock a s'
  indentBlock (BlockQuote bs) = BlockQuote (map indent bs)
  indentBlock (Header l attr' ins) = Header l attr' (indentLines ins)
  indentBlock everythingElse = everythingElse
  -- indent every (explicit) line
  indentLines :: [Inline] -> [Inline]
  indentLines ins = let lns = split isLineBreak ins :: [[Inline]]
                    in  intercalate [LineBreak] $ map ((Str spacer):) lns

-- | Convert a Pandoc's Inline element to FictionBook XML representation.
toXml :: Inline -> FBM [Content]
toXml (Str s) = return [txt s]
toXml (Span _ ils) = cMapM toXml ils
toXml (Emph ss) = list `liftM` wrap "emphasis" ss
toXml (Strong ss) = list `liftM` wrap "strong" ss
toXml (Strikeout ss) = list `liftM` wrap "strikethrough" ss
toXml (Superscript ss) = list `liftM` wrap "sup" ss
toXml (Subscript ss) = list `liftM` wrap "sub" ss
toXml (SmallCaps ss) = cMapM toXml $ capitalize ss
toXml (Quoted SingleQuote ss) = do  -- FIXME: should be language-specific
  inner <- cMapM toXml ss
  return $ [txt "‘"] ++ inner ++ [txt "’"]
toXml (Quoted DoubleQuote ss) = do
  inner <- cMapM toXml ss
  return $ [txt "“"] ++ inner ++ [txt "”"]
toXml (Cite _ ss) = cMapM toXml ss  -- FIXME: support citation styles
toXml (Code _ s) = return [el "code" s]
toXml Space = return [txt " "]
toXml SoftBreak = return [txt " "]
toXml LineBreak = return [el "empty-line" ()]
toXml (Math _ formula) = insertMath InlineImage formula
toXml (RawInline _ _) = return []  -- raw TeX and raw HTML are suppressed
toXml (Link _ text (url,ttl)) = do
  fns <- footnotes `liftM` get
  let n = 1 + length fns
  let ln_id = linkID n
  let ln_ref = list . el "sup" . txt $ "[" ++ show n ++ "]"
  ln_text <- cMapM toXml text
  let ln_desc =
          let ttl' = dropWhile isSpace ttl
          in if null ttl'
             then list . el "p" $ el "code" url
             else list . el "p" $ [ txt (ttl' ++ ": "), el "code" url ]
  modify (\s -> s { footnotes = (n, ln_id, ln_desc) : fns })
  return $ ln_text ++
         [ el "a"
                  ( [ attr ("l","href") ('#':ln_id)
                    , uattr "type" "note" ]
                  , ln_ref) ]
toXml img@(Image _ _ _) = insertImage InlineImage img
toXml (Note bs) = do
  fns <- footnotes `liftM` get
  let n = 1 + length fns
  let fn_id = footnoteID n
  fn_desc <- cMapM blockToXml bs
  modify (\s -> s { footnotes = (n, fn_id, fn_desc) : fns })
  let fn_ref = el "sup" . txt $ "[" ++ show n ++ "]"
  return . list $ el "a" ( [ attr ("l","href") ('#':fn_id)
                           , uattr "type" "note" ]
                         , fn_ref )

insertMath :: ImageMode -> String -> FBM [Content]
insertMath immode formula = do
  htmlMath <- return . writerHTMLMathMethod . writerOptions =<< get
  case htmlMath of
    WebTeX url -> do
       let alt = [Code nullAttr formula]
       let imgurl = url ++ urlEncode formula
       let img = Image nullAttr alt (imgurl, "")
       insertImage immode img
    _ -> return [el "code" formula]

insertImage :: ImageMode -> Inline -> FBM [Content]
insertImage immode (Image _ alt (url,ttl)) = do
  images <- imagesToFetch `liftM` get
  let n = 1 + length images
  let fname = "image" ++ show n
  modify (\s -> s { imagesToFetch = (fname, url) : images })
  let ttlattr = case (immode, null ttl) of
                  (NormalImage, False) -> [ uattr "title" ttl ]
                  _ -> []
  return . list $
         el "image" $
            [ attr ("l","href") ('#':fname)
            , attr ("l","type") (show immode)
            , uattr "alt" (cMap plain alt) ]
            ++ ttlattr
insertImage _ _ = error "unexpected inline instead of image"

replaceImagesWithAlt :: [String] -> Content -> Content
replaceImagesWithAlt missingHrefs body =
  let cur = XC.fromContent body
      cur' = replaceAll cur
  in  XC.toTree . XC.root $ cur'
  where
  --
    replaceAll :: XC.Cursor -> XC.Cursor
    replaceAll c =
        let n = XC.current c
            c' = if isImage n && isMissing n
                 then XC.modifyContent replaceNode c
                 else c
        in  case XC.nextDF c' of
              (Just cnext) -> replaceAll cnext
              Nothing -> c'  -- end of document
  --
    isImage :: Content -> Bool
    isImage (Elem e) = (elName e) == (uname "image")
    isImage _ = False
  --
    isMissing (Elem img@(Element _ _ _ _)) =
        let imgAttrs = elAttribs img
            badAttrs = map (attr ("l","href")) missingHrefs
        in  any (`elem` imgAttrs) badAttrs
    isMissing _ = False
  --
    replaceNode :: Content -> Content
    replaceNode n@(Elem img@(Element _ _ _ _)) =
        let attrs = elAttribs img
            alt = getAttrVal attrs (uname "alt")
            imtype = getAttrVal attrs (qname "l" "type")
        in case (alt, imtype) of
             (Just alt', Just imtype') ->
                 if imtype' == show NormalImage
                 then el "p" alt'
                 else txt alt'
             (Just alt', Nothing) -> txt alt'  -- no type attribute
             _ -> n   -- don't replace if alt text is not found
    replaceNode n = n
  --
    getAttrVal :: [X.Attr] -> QName -> Maybe String
    getAttrVal attrs name =
        case filter ((name ==) . attrKey) attrs of
           (a:_) -> Just (attrVal a)
           _     -> Nothing


-- | Wrap all inlines with an XML tag (given its unqualified name).
wrap :: String -> [Inline] -> FBM Content
wrap tagname inlines = el tagname `liftM` cMapM toXml inlines

-- " Create a singleton list.
list :: a -> [a]
list = (:[])

-- | Convert an 'Inline' to plaintext.
plain :: Inline -> String
plain (Str s) = s
plain (Emph ss) = concat (map plain ss)
plain (Span _ ss) = concat (map plain ss)
plain (Strong ss) = concat (map plain ss)
plain (Strikeout ss) = concat (map plain ss)
plain (Superscript ss) = concat (map plain ss)
plain (Subscript ss) = concat (map plain ss)
plain (SmallCaps ss) = concat (map plain ss)
plain (Quoted _ ss) = concat (map plain ss)
plain (Cite _ ss) = concat (map plain ss)  -- FIXME
plain (Code _ s) = s
plain Space = " "
plain SoftBreak = " "
plain LineBreak = "\n"
plain (Math _ s) = s
plain (RawInline _ s) = s
plain (Link _ text (url,_)) = concat (map plain text ++ [" <", url, ">"])
plain (Image _ alt _) = concat (map plain alt)
plain (Note _) = ""  -- FIXME

-- | Create an XML element.
el :: (Node t)
   => String   -- ^ unqualified element name
   -> t        -- ^ node contents
   -> Content  -- ^ XML content
el name cs = Elem $ unode name cs

-- | Put empty lines around content
spaceBeforeAfter :: [Content] -> [Content]
spaceBeforeAfter cs =
    let emptyline = el "empty-line" ()
    in  [emptyline] ++ cs ++ [emptyline]

-- | Create a plain-text XML content.
txt :: String -> Content
txt s = Text $ CData CDataText s Nothing

-- | Create an XML attribute with an unqualified name.
uattr :: String -> String -> Text.XML.Light.Attr
uattr name val = Attr (uname name) val

-- | Create an XML attribute with a qualified name from given namespace.
attr :: (String, String) -> String -> Text.XML.Light.Attr
attr (ns, name) val = Attr (qname ns name) val

-- | Unqualified name
uname :: String -> QName
uname name = QName name Nothing Nothing

-- | Qualified name
qname :: String -> String -> QName
qname ns name = QName name Nothing (Just ns)

-- | Abbreviation for 'concatMap'.
cMap :: (a -> [b]) -> [a] -> [b]
cMap = concatMap

-- | Monadic equivalent of 'concatMap'.
cMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
cMapM f xs = concat `liftM` mapM f xs
