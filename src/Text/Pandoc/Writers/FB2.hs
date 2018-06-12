{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}

{-
Copyright (c) 2011-2012 Sergey Astanin
              2012-2018 John MacFarlane

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
Module      : Text.Pandoc.Writers.FB2
Copyright   : Copyright (C) 2011-2012 Sergey Astanin
                            2012-2018 John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : John MacFarlane
Stability   : alpha
Portability : portable

Conversion of 'Pandoc' documents to FB2 (FictionBook2) format.

FictionBook is an XML-based e-book format. For more information see:
<http://www.fictionbook.org/index.php/Eng:XML_Schema_Fictionbook_2.1>

-}
module Text.Pandoc.Writers.FB2 (writeFB2)  where

import Prelude
import Control.Monad (zipWithM)
import Control.Monad.Except (catchError)
import Control.Monad.State.Strict (StateT, evalStateT, get, lift, liftM, modify)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as B8
import Data.Char (isAscii, isControl, isSpace, toLower)
import Data.Either (lefts, rights)
import Data.List (intercalate, isPrefixOf, stripPrefix)
import Data.Text (Text, pack)
import Network.HTTP (urlEncode)
import Text.XML.Light
import qualified Text.XML.Light as X
import qualified Text.XML.Light.Cursor as XC

import Text.Pandoc.Class (PandocMonad, report)
import qualified Text.Pandoc.Class as P
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options (HTMLMathMethod (..), WriterOptions (..), def)
import Text.Pandoc.Shared (capitalize, isHeaderBlock, isURI, orderedListMarkers)

-- | Data to be written at the end of the document:
-- (foot)notes, URLs, references, images.
data FbRenderState = FbRenderState
    { footnotes         :: [ (Int, String, [Content]) ]  -- ^ #, ID, text
    , imagesToFetch     :: [ (String, String) ]  -- ^ filename, URL or path
    , parentListMarker  :: String  -- ^ list marker of the parent ordered list
    , writerOptions     :: WriterOptions
    } deriving (Show)

-- | FictionBook building monad.
type FBM m = StateT FbRenderState m

newFB :: FbRenderState
newFB = FbRenderState { footnotes = [], imagesToFetch = []
                      , parentListMarker = ""
                      , writerOptions = def }

data ImageMode = NormalImage | InlineImage deriving (Eq)
instance Show ImageMode where
    show NormalImage = "imageType"
    show InlineImage = "inlineImageType"

-- | Produce an FB2 document from a 'Pandoc' document.
writeFB2 :: PandocMonad m
         => WriterOptions    -- ^ conversion options
         -> Pandoc           -- ^ document to convert
         -> m Text           -- ^ FictionBook2 document (not encoded yet)
writeFB2 opts doc = flip evalStateT newFB $ pandocToFB2 opts doc

pandocToFB2 :: PandocMonad m
            => WriterOptions
            -> Pandoc
            -> FBM m Text
pandocToFB2 opts (Pandoc meta blocks) = do
     modify (\s -> s { writerOptions = opts })
     desc <- description meta
     title <- cMapM toXml . docTitle $ meta
     secs <- renderSections 1 blocks
     let body = el "body" $ el "title" (el "p" title) : secs
     notes <- renderFootnotes
     (imgs,missing) <- fmap imagesToFetch get >>= \s -> lift (fetchImages s)
     let body' = replaceImagesWithAlt missing body
     let fb2_xml = el "FictionBook" (fb2_attrs, [desc, body'] ++ notes ++ imgs)
     return $ pack $ xml_head ++ showContent fb2_xml ++ "\n"
  where
  xml_head = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  fb2_attrs =
      let xmlns = "http://www.gribuser.ru/xml/fictionbook/2.0"
          xlink = "http://www.w3.org/1999/xlink"
      in  [ uattr "xmlns" xmlns
          , attr ("xmlns", "l") xlink ]

description :: PandocMonad m => Meta -> FBM m Content
description meta' = do
  let genre = el "genre" "unrecognised"
  bt <- booktitle meta'
  let as = authors meta'
  dd <- docdate meta'
  annotation <- case lookupMeta "abstract" meta' of
                  Just (MetaBlocks bs) -> (list . el "annotation") <$> (cMapM blockToXml $ map unPlain bs)
                  _ -> pure mempty
  let lang = case lookupMeta "lang" meta' of
               Just (MetaInlines [Str s]) -> [el "lang" $ iso639 s]
               Just (MetaString s)        -> [el "lang" $ iso639 s]
               _                          -> []
             where iso639 = takeWhile (/= '-') -- Convert BCP 47 to ISO 639
  let coverimage url = do
        let img = Image nullAttr mempty (url, "")
        im <- insertImage InlineImage img
        return [el "coverpage" im]
  coverpage <- case lookupMeta "cover-image" meta' of
                    Just (MetaInlines [Str s]) -> coverimage s
                    Just (MetaString s) -> coverimage s
                    _       -> return []
  return $ el "description"
    [ el "title-info" (genre : (as ++ bt ++ annotation ++ dd ++ lang))
    , el "document-info" (el "program-used" "pandoc" : coverpage)
    ]

booktitle :: PandocMonad m => Meta -> FBM m [Content]
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
      email = el "email" <$> take 1 (filter ('@' `elem`) ws)
      ws' = filter ('@' `notElem`) ws
      names = case ws' of
                [nickname] -> [ el "nickname" nickname ]
                [fname, lname] -> [ el "first-name" fname
                                    , el "last-name" lname ]
                (fname:rest) -> [ el "first-name" fname
                                , el "middle-name" (concat . init $ rest)
                                , el "last-name" (last rest) ]
                [] -> []
  in  list $ el "author" (names ++ email)

docdate :: PandocMonad m => Meta -> FBM m [Content]
docdate meta' = do
  let ss = docDate meta'
  d <- cMapM toXml ss
  return $ if null d
           then []
           else [el "date" d]

-- | Divide the stream of blocks into sections and convert to XML
-- representation.
renderSections :: PandocMonad m => Int -> [Block] -> FBM m [Content]
renderSections level blocks = do
    let secs = splitSections level blocks
    mapM (renderSection level) secs

renderSection :: PandocMonad m => Int -> ([Inline], [Block]) -> FBM m Content
renderSection level (ttl, body) = do
    title <- if null ttl
            then return []
            else list . el "title" <$> formatTitle ttl
    content <- if hasSubsections body
               then renderSections (level + 1) body
               else cMapM blockToXml body
    return $ el "section" (title ++ content)
  where
    hasSubsections = any isHeaderBlock

-- | Only <p> and <empty-line> are allowed within <title> in FB2.
formatTitle :: PandocMonad m => [Inline] -> FBM m [Content]
formatTitle inlines =
  cMapM (blockToXml . Para) $ split (== LineBreak) inlines

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split cond xs = let (b,a) = break cond xs
                in  (b:split cond (drop 1 a))

isLineBreak :: Inline -> Bool
isLineBreak LineBreak = True
isLineBreak _         = False

-- | Divide the stream of block elements into sections: [(title, blocks)].
splitSections :: Int -> [Block] -> [([Inline], [Block])]
splitSections level blocks = reverse $ revSplit (reverse blocks)
  where
  revSplit [] = []
  revSplit rblocks =
    let (lastsec, before) = break sameLevel rblocks
        (header, prevblocks) =
            case before of
              (Header n _ title:prevblocks') ->
                  if n == level
                     then (title, prevblocks')
                     else ([], before)
              _ -> ([], before)
    in (header, reverse lastsec) : revSplit prevblocks
  sameLevel (Header n _ _) = n == level
  sameLevel _              = False

-- | Make another FictionBook body with footnotes.
renderFootnotes :: PandocMonad m => FBM m [Content]
renderFootnotes = do
  fns <- footnotes `liftM` get
  if null fns
    then return []  -- no footnotes
    else return . list $
         el "body" ([uattr "name" "notes"], map renderFN (reverse fns))
  where
    renderFN (n, idstr, cs) =
        let fn_texts = el "title" (el "p" (show n)) : cs
        in  el "section" ([uattr "id" idstr], fn_texts)

-- | Fetch images and encode them for the FictionBook XML.
-- Return image data and a list of hrefs of the missing images.
fetchImages :: PandocMonad m => [(String,String)] -> m ([Content],[String])
fetchImages links = do
    imgs <- mapM (uncurry fetchImage) links
    return (rights imgs, lefts imgs)

-- | Fetch image data from disk or from network and make a <binary> XML section.
-- Return either (Left hrefOfMissingImage) or (Right xmlContent).
fetchImage :: PandocMonad m => String -> String -> m (Either String Content)
fetchImage href link = do
  mbimg <-
      case (isURI link, readDataURI link) of
       (True, Just (mime,_,True,base64)) ->
           let mime' = map toLower mime
           in if mime' == "image/png" || mime' == "image/jpeg"
              then return (Just (mime',base64))
              else return Nothing
       (True, Just _) -> return Nothing  -- not base64-encoded
       _               ->
         catchError (do (bs, mbmime) <- P.fetchItem link
                        case mbmime of
                             Nothing -> do
                               report $ CouldNotDetermineMimeType link
                               return Nothing
                             Just mime -> return $ Just (mime,
                                                      B8.unpack $ encode bs))
                    (\e ->
                       do report $ CouldNotFetchResource link (show e)
                          return Nothing)
  case mbimg of
    Just (imgtype, imgdata) ->
        return . Right $ el "binary"
                   ( [uattr "id" href
                     , uattr "content-type" imgtype]
                   , txt imgdata )
    _ -> return (Left ('#':href))


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
          (map toLower mtype `elem` types
           || "x-" `isPrefixOf` map toLower mtype)
          && all valid mtype
          && all valid msubtype
      _ -> False
 where
   types =  ["text","image","audio","video","application","message","multipart"]
   valid c = isAscii c && not (isControl c) && not (isSpace c) &&
             c `notElem` "()<>@,;:\\\"/[]?="

footnoteID :: Int -> String
footnoteID i = "n" ++ show i

-- | Convert a block-level Pandoc's element to FictionBook XML representation.
blockToXml :: PandocMonad m => Block -> FBM m [Content]
blockToXml (Plain ss) = cMapM toXml ss  -- FIXME: can lead to malformed FB2
blockToXml (Para [Math DisplayMath formula]) = insertMath NormalImage formula
-- title beginning with fig: indicates that the image is a figure
blockToXml (Para [Image atr alt (src,'f':'i':'g':':':tit)]) =
  insertImage NormalImage (Image atr alt (src,tit))
blockToXml (Para ss) = (list . el "p") <$> cMapM toXml ss
blockToXml (CodeBlock _ s) = return . spaceBeforeAfter .
                             map (el "p" . el "code") . lines $ s
blockToXml b@(RawBlock _ _) = do
  report $ BlockNotRendered b
  return []
blockToXml (Div _ bs) = cMapM blockToXml bs
blockToXml (BlockQuote bs) = (list . el "cite") <$> cMapM blockToXml bs
blockToXml (LineBlock lns) =
  (list . el "poem") <$> mapM stanza (split null lns)
  where
    v xs = el "v" <$> cMapM toXml xs
    stanza xs = el "stanza" <$> mapM v xs
blockToXml (OrderedList a bss) = do
    state <- get
    let pmrk = parentListMarker state
    let markers = (pmrk ++) <$> orderedListMarkers a
    let mkitem mrk bs = do
          modify (\s -> s { parentListMarker = mrk ++ " "})
          item <- cMapM blockToXml $ plainToPara $ indentBlocks (mrk ++ " ") bs
          modify (\s -> s { parentListMarker = pmrk }) -- old parent marker
          return item
    concat <$> zipWithM mkitem markers bss
blockToXml (BulletList bss) = do
    state <- get
    let pmrk = parentListMarker state
    let mrk = pmrk ++ "•"
    let mkitem bs = do
          modify (\s -> s { parentListMarker = mrk ++ " "})
          item <- cMapM blockToXml $ plainToPara $ indentBlocks (mrk ++ " ") bs
          modify (\s -> s { parentListMarker = pmrk }) -- old parent marker
          return item
    cMapM mkitem bss
blockToXml (DefinitionList defs) =
    cMapM mkdef defs
    where
      mkdef (term, bss) = do
          items <- cMapM (cMapM blockToXml . plainToPara . indentBlocks (replicate 4 ' ')) bss
          t <- wrap "strong" term
          return (el "p" t : items)
blockToXml h@Header{} = do
  -- should not occur after hierarchicalize, except inside lists/blockquotes
  report $ BlockNotRendered h
  return []
blockToXml HorizontalRule = return [ el "empty-line" () ]
blockToXml (Table caption aligns _ headers rows) = do
    hd <- mkrow "th" headers aligns
    bd <- mapM (\r -> mkrow "td" r aligns) rows
    c <- el "emphasis" <$> cMapM toXml caption
    return [el "table" (hd : bd), el "p" c]
    where
      mkrow :: PandocMonad m => String -> [TableCell] -> [Alignment] -> FBM m Content
      mkrow tag cells aligns' =
        el "tr" <$> mapM (mkcell tag) (zip cells aligns')
      --
      mkcell :: PandocMonad m => String -> (TableCell, Alignment) -> FBM m Content
      mkcell tag (cell, align) = do
        cblocks <- cMapM blockToXml cell
        return $ el tag ([align_attr align], cblocks)
      --
      align_attr a = Attr (QName "align" Nothing Nothing) (align_str a)
      align_str AlignLeft    = "left"
      align_str AlignCenter  = "center"
      align_str AlignRight   = "right"
      align_str AlignDefault = "left"
blockToXml Null = return []

-- Replace plain text with paragraphs and add line break after paragraphs.
-- It is used to convert plain text from tight list items to paragraphs.
plainToPara :: [Block] -> [Block]
plainToPara [] = []
plainToPara (Plain inlines : rest) =
    Para inlines : plainToPara rest
plainToPara (Para inlines : rest) =
    Para inlines : HorizontalRule : plainToPara rest -- HorizontalRule will be converted to <empty-line />
plainToPara (p:rest) = p : plainToPara rest

-- Replace plain text with paragraphs
unPlain :: Block -> Block
unPlain (Plain inlines) = Para inlines
unPlain x = x

-- Simulate increased indentation level. Will not really work
-- for multi-line paragraphs.
indentPrefix :: String -> Block -> Block
indentPrefix spacer = indentBlock
  where
  indentBlock (Plain ins) = Plain (Str spacer:ins)
  indentBlock (Para ins) = Para (Str spacer:ins)
  indentBlock (CodeBlock a s) =
    let s' = unlines . map (spacer++) . lines $ s
    in  CodeBlock a s'
  indentBlock (BlockQuote bs) = BlockQuote (map indent bs)
  indentBlock (Header l attr' ins) = Header l attr' (indentLines ins)
  indentBlock everythingElse = everythingElse
  -- indent every (explicit) line
  indentLines :: [Inline] -> [Inline]
  indentLines ins = let lns = split isLineBreak ins :: [[Inline]]
                    in  intercalate [LineBreak] $ map (Str spacer:) lns

indent :: Block -> Block
indent = indentPrefix spacer
  where
  -- indentation space
  spacer :: String
  spacer = replicate 4 ' '

indentBlocks :: String -> [Block] -> [Block]
indentBlocks _ [] = []
indentBlocks prefix (x:xs) = indentPrefix prefix x : map (indentPrefix $ replicate (length prefix) ' ') xs

-- | Convert a Pandoc's Inline element to FictionBook XML representation.
toXml :: PandocMonad m => Inline -> FBM m [Content]
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
toXml SoftBreak = return [txt "\n"]
toXml LineBreak = return [txt "\n"]
toXml (Math _ formula) = insertMath InlineImage formula
toXml il@(RawInline _ _) = do
  report $ InlineNotRendered il
  return []  -- raw TeX and raw HTML are suppressed
toXml (Link _ text (url,_)) = do
  ln_text <- cMapM toXml text
  return [ el "a" ( [ attr ("l","href") url ], ln_text) ]
toXml img@Image{} = insertImage InlineImage img
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

insertMath :: PandocMonad m => ImageMode -> String -> FBM m [Content]
insertMath immode formula = do
  htmlMath <- fmap (writerHTMLMathMethod . writerOptions) get
  case htmlMath of
    WebTeX url -> do
       let alt = [Code nullAttr formula]
       let imgurl = url ++ urlEncode formula
       let img = Image nullAttr alt (imgurl, "")
       insertImage immode img
    _ -> return [el "code" formula]

insertImage :: PandocMonad m => ImageMode -> Inline -> FBM m [Content]
insertImage immode (Image _ alt (url,ttl)) = do
  images <- imagesToFetch `liftM` get
  let n = 1 + length images
  let fname = "image" ++ show n
  modify (\s -> s { imagesToFetch = (fname, url) : images })
  let ttlattr = case (immode, null ttl) of
                  (NormalImage, False) -> [ uattr "title" ttl ]
                  _                    -> []
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
              Nothing      -> c'  -- end of document
  --
    isImage :: Content -> Bool
    isImage (Elem e) = elName e == uname "image"
    isImage _        = False
  --
    isMissing (Elem img@Element{}) =
        let imgAttrs = elAttribs img
            badAttrs = map (attr ("l","href")) missingHrefs
        in  any (`elem` imgAttrs) badAttrs
    isMissing _ = False
  --
    replaceNode :: Content -> Content
    replaceNode n@(Elem img@Element{}) =
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
wrap :: PandocMonad m => String -> [Inline] -> FBM m Content
wrap tagname inlines = el tagname `liftM` cMapM toXml inlines

-- " Create a singleton list.
list :: a -> [a]
list = (:[])

-- | Convert an 'Inline' to plaintext.
plain :: Inline -> String
plain (Str s)               = s
plain (Emph ss)             = cMap plain ss
plain (Span _ ss)           = cMap plain ss
plain (Strong ss)           = cMap plain ss
plain (Strikeout ss)        = cMap plain ss
plain (Superscript ss)      = cMap plain ss
plain (Subscript ss)        = cMap plain ss
plain (SmallCaps ss)        = cMap plain ss
plain (Quoted _ ss)         = cMap plain ss
plain (Cite _ ss)           = cMap plain ss  -- FIXME
plain (Code _ s)            = s
plain Space                 = " "
plain SoftBreak             = " "
plain LineBreak             = "\n"
plain (Math _ s)            = s
plain (RawInline _ _)       = ""
plain (Link _ text (url,_)) = concat (map plain text ++ [" <", url, ">"])
plain (Image _ alt _)       = cMap plain alt
plain (Note _)              = ""  -- FIXME

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
uattr name = Attr (uname name)

-- | Create an XML attribute with a qualified name from given namespace.
attr :: (String, String) -> String -> Text.XML.Light.Attr
attr (ns, name) = Attr (qname ns name)

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
