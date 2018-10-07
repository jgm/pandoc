{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-
Copyright (C) 2017-2018 Hamish Mackenzie

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
   Module      : Text.Pandoc.Readers.JATS
   Copyright   : Copyright (C) 2017-2018 Hamish Mackenzie
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of JATS XML to 'Pandoc' document.
-}

module Text.Pandoc.Readers.JATS ( readJATS ) where
import Prelude
import Control.Monad.State.Strict
import Data.Char (isDigit, isSpace, toUpper)
import Data.Default
import Data.Generics
import Data.List (intersperse)
import qualified Data.Map as Map
import Data.Maybe (maybeToList, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.HTML.TagSoup.Entity (lookupEntity)
import Text.Pandoc.Builder
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Options
import Text.Pandoc.Shared (underlineSpan, crFilter, safeRead)
import Text.TeXMath (readMathML, writeTeX)
import Text.XML.Light
import qualified Data.Set as S (fromList, member)
import Data.Set ((\\))

type JATS m = StateT JATSState m

data JATSState = JATSState{ jatsSectionLevel :: Int
                          , jatsQuoteType    :: QuoteType
                          , jatsMeta         :: Meta
                          , jatsBook         :: Bool
                          , jatsFigureTitle  :: Inlines
                          , jatsContent      :: [Content]
                          } deriving Show

instance Default JATSState where
  def = JATSState{ jatsSectionLevel = 0
                 , jatsQuoteType = DoubleQuote
                 , jatsMeta = mempty
                 , jatsBook = False
                 , jatsFigureTitle = mempty
                 , jatsContent = [] }


readJATS :: PandocMonad m => ReaderOptions -> Text -> m Pandoc
readJATS _ inp = do
  let tree = normalizeTree . parseXML
               $ T.unpack $ crFilter inp
  (bs, st') <- flip runStateT (def{ jatsContent = tree }) $ mapM parseBlock tree
  return $ Pandoc (jatsMeta st') (toList . mconcat $ bs)

-- normalize input, consolidating adjacent Text and CRef elements
normalizeTree :: [Content] -> [Content]
normalizeTree = everywhere (mkT go)
  where go :: [Content] -> [Content]
        go (Text (CData CDataRaw _ _):xs) = xs
        go (Text (CData CDataText s1 z):Text (CData CDataText s2 _):xs) =
           Text (CData CDataText (s1 ++ s2) z):xs
        go (Text (CData CDataText s1 z):CRef r:xs) =
           Text (CData CDataText (s1 ++ convertEntity r) z):xs
        go (CRef r:Text (CData CDataText s1 z):xs) =
             Text (CData CDataText (convertEntity r ++ s1) z):xs
        go (CRef r1:CRef r2:xs) =
             Text (CData CDataText (convertEntity r1 ++ convertEntity r2) Nothing):xs
        go xs = xs

convertEntity :: String -> String
convertEntity e = Data.Maybe.fromMaybe (map toUpper e) (lookupEntity e)

-- convenience function to get an attribute value, defaulting to ""
attrValue :: String -> Element -> String
attrValue attr =
  fromMaybe "" . maybeAttrValue attr

maybeAttrValue :: String -> Element -> Maybe String
maybeAttrValue attr elt =
  lookupAttrBy (\x -> qName x == attr) (elAttribs elt)

-- convenience function
named :: String -> Element -> Bool
named s e = qName (elName e) == s

--

addMeta :: PandocMonad m => ToMetaValue a => String -> a -> JATS m ()
addMeta field val = modify (setMeta field val)

instance HasMeta JATSState where
  setMeta field v s =  s {jatsMeta = setMeta field v (jatsMeta s)}
  deleteMeta field s = s {jatsMeta = deleteMeta field (jatsMeta s)}

isBlockElement :: Content -> Bool
isBlockElement (Elem e) = qName (elName e) `S.member` blocktags
  where blocktags = S.fromList (paragraphLevel ++ lists ++ mathML ++ other) \\ S.fromList inlinetags
        paragraphLevel = ["address", "array", "boxed-text", "chem-struct-wrap",
            "code", "fig", "fig-group", "graphic", "media", "preformat",
            "supplementary-material", "table-wrap", "table-wrap-group",
            "alternatives", "disp-formula", "disp-formula-group"]
        lists = ["def-list", "list"]
        mathML = ["tex-math", "mml:math"]
        other = ["p", "related-article", "related-object", "ack", "disp-quote",
            "speech", "statement", "verse-group", "x"]
        inlinetags = ["email", "ext-link", "uri", "inline-supplementary-material",
            "related-article", "related-object", "hr", "bold", "fixed-case",
            "italic", "monospace", "overline", "overline-start", "overline-end",
            "roman", "sans-serif", "sc", "strike", "underline", "underline-start",
            "underline-end", "ruby", "alternatives", "inline-graphic", "private-char",
            "chem-struct", "inline-formula", "tex-math", "mml:math", "abbrev",
            "milestone-end", "milestone-start", "named-content", "styled-content",
            "fn", "target", "xref", "sub", "sup", "x", "address", "array",
            "boxed-text", "chem-struct-wrap", "code", "fig", "fig-group", "graphic",
            "media", "preformat", "supplementary-material", "table-wrap",
            "table-wrap-group", "disp-formula", "disp-formula-group",
            "citation-alternatives", "element-citation", "mixed-citation",
            "nlm-citation", "award-id", "funding-source", "open-access",
            "def-list", "list", "ack", "disp-quote", "speech", "statement",
            "verse-group"]
isBlockElement _ = False

-- Trim leading and trailing newline characters
trimNl :: String -> String
trimNl = reverse . go . reverse . go
  where go ('\n':xs) = xs
        go xs        = xs

-- function that is used by both graphic (in parseBlock)
-- and inline-graphic (in parseInline)
getGraphic :: PandocMonad m => Element -> JATS m Inlines
getGraphic e = do
  let atVal a = attrValue a e
      attr = (atVal "id", words $ atVal "role", [])
      imageUrl = atVal "href"
      captionOrLabel = case filterChild (\x -> named "caption" x
                                            || named "label" x) e of
                        Nothing -> return mempty
                        Just z  -> mconcat <$>
                                         mapM parseInline (elContent z)
  figTitle <- gets jatsFigureTitle
  let (caption, title) = if isNull figTitle
                            then (captionOrLabel, atVal "title")
                            else (return figTitle, "fig:")
  fmap (imageWith attr imageUrl title) caption

getBlocks :: PandocMonad m => Element -> JATS m Blocks
getBlocks e =  mconcat <$>
                 mapM parseBlock (elContent e)


parseBlock :: PandocMonad m => Content -> JATS m Blocks
parseBlock (Text (CData CDataRaw _ _)) = return mempty -- DOCTYPE
parseBlock (Text (CData _ s _)) = if all isSpace s
                                     then return mempty
                                     else return $ plain $ trimInlines $ text s
parseBlock (CRef x) = return $ plain $ str $ map toUpper x
parseBlock (Elem e) =
  case qName (elName e) of
        "p" -> parseMixed para (elContent e)
        "code" -> codeBlockWithLang
        "preformat" -> codeBlockWithLang
        "disp-quote" -> parseBlockquote
        "list" -> case attrValue "list-type" e of
                    "bullet" -> bulletList <$> listitems
                    listType -> do
                      let start = fromMaybe 1 $
                                  (strContent <$> (filterElement (named "list-item") e
                                               >>= filterElement (named "label")))
                                   >>= safeRead
                      orderedListWith (start, parseListStyleType listType, DefaultDelim)
                        <$> listitems
        "def-list" -> definitionList <$> deflistitems
        "sec" -> gets jatsSectionLevel >>= sect . (+1)
        "graphic" -> para <$> getGraphic e
        "journal-meta" -> parseMetadata e
        "article-meta" -> parseMetadata e
        "custom-meta" -> parseMetadata e
        "title" -> return mempty -- processed by header
        "table" -> parseTable
        "fig" -> divWith (attrValue "id" e, ["fig"], []) <$> getBlocks e
        "table-wrap" -> divWith (attrValue "id" e, ["table-wrap"], []) <$> getBlocks e
        "caption" -> divWith (attrValue "id" e, ["caption"], []) <$> sect 6
        "ref-list" -> parseRefList e
        "?xml"  -> return mempty
        _       -> getBlocks e
   where parseMixed container conts = do
           let (ils,rest) = break isBlockElement conts
           ils' <- (trimInlines . mconcat) <$> mapM parseInline ils
           let p = if ils' == mempty then mempty else container ils'
           case rest of
                 []     -> return p
                 (r:rs) -> do
                    b <- parseBlock r
                    x <- parseMixed container rs
                    return $ p <> b <> x
         codeBlockWithLang = do
           let classes' = case attrValue "language" e of
                                "" -> []
                                x  -> [x]
           return $ codeBlockWith (attrValue "id" e, classes', [])
                  $ trimNl $ strContentRecursive e
         parseBlockquote = do
            attrib <- case filterChild (named "attribution") e of
                             Nothing  -> return mempty
                             Just z   -> (para . (str "— " <>) . mconcat)
                                         <$>
                                              mapM parseInline (elContent z)
            contents <- getBlocks e
            return $ blockQuote (contents <> attrib)
         parseListStyleType "roman-lower" = LowerRoman
         parseListStyleType "roman-upper" = UpperRoman
         parseListStyleType "alpha-lower" = LowerAlpha
         parseListStyleType "alpha-upper" = UpperAlpha
         parseListStyleType _             = DefaultStyle
         listitems = mapM getBlocks $ filterChildren (named "list-item") e
         deflistitems = mapM parseVarListEntry $ filterChildren
                     (named "def-item") e
         parseVarListEntry e' = do
                     let terms = filterChildren (named "term") e'
                     let items = filterChildren (named "def") e'
                     terms' <- mapM getInlines terms
                     items' <- mapM getBlocks items
                     return (mconcat $ intersperse (str "; ") terms', items')
         parseTable = do
                      let isCaption x = named "title" x || named "caption" x
                      caption <- case filterChild isCaption e of
                                       Just t  -> getInlines t
                                       Nothing -> return mempty
                      let e' = fromMaybe e $ filterChild (named "tgroup") e
                      let isColspec x = named "colspec" x || named "col" x
                      let colspecs = case filterChild (named "colgroup") e' of
                                           Just c -> filterChildren isColspec c
                                           _      -> filterChildren isColspec e'
                      let isRow x = named "row" x || named "tr" x
                      headrows <- case filterChild (named "thead") e' of
                                       Just h  -> case filterChild isRow h of
                                                       Just x  -> parseRow x
                                                       Nothing -> return []
                                       Nothing -> return []
                      bodyrows <- case filterChild (named "tbody") e' of
                                       Just b  -> mapM parseRow
                                                  $ filterChildren isRow b
                                       Nothing -> mapM parseRow
                                                  $ filterChildren isRow e'
                      let toAlignment c = case findAttr (unqual "align") c of
                                                Just "left"   -> AlignLeft
                                                Just "right"  -> AlignRight
                                                Just "center" -> AlignCenter
                                                _             -> AlignDefault
                      let toWidth c = case findAttr (unqual "colwidth") c of
                                                Just w -> fromMaybe 0
                                                   $ safeRead $ '0': filter (\x ->
                                                     isDigit x || x == '.') w
                                                Nothing -> 0 :: Double
                      let numrows = case bodyrows of
                                         [] -> 0
                                         xs -> maximum $ map length xs
                      let aligns = case colspecs of
                                     [] -> replicate numrows AlignDefault
                                     cs -> map toAlignment cs
                      let widths = case colspecs of
                                     []  -> replicate numrows 0
                                     cs  -> let ws = map toWidth cs
                                                tot = sum ws
                                            in  if all (> 0) ws
                                                   then map (/ tot) ws
                                                   else replicate numrows 0
                      let headrows' = if null headrows
                                         then replicate numrows mempty
                                         else headrows
                      return $ table caption (zip aligns widths)
                                 headrows' bodyrows
         isEntry x  = named "entry" x || named "td" x || named "th" x
         parseRow = mapM (parseMixed plain . elContent) . filterChildren isEntry
         sect n = do isbook <- gets jatsBook
                     let n' = if isbook || n == 0 then n + 1 else n
                     headerText <- case filterChild (named "title") e `mplus`
                                        (filterChild (named "info") e >>=
                                            filterChild (named "title")) of
                                      Just t  -> getInlines t
                                      Nothing -> return mempty
                     oldN <- gets jatsSectionLevel
                     modify $ \st -> st{ jatsSectionLevel = n }
                     b <- getBlocks e
                     let ident = attrValue "id" e
                     modify $ \st -> st{ jatsSectionLevel = oldN }
                     return $ headerWith (ident,[],[]) n' headerText <> b

getInlines :: PandocMonad m => Element -> JATS m Inlines
getInlines e' = (trimInlines . mconcat) <$>
                 mapM parseInline (elContent e')

parseMetadata :: PandocMonad m => Element -> JATS m Blocks
parseMetadata e = do
  getTitle e
  getAuthors e
  getAffiliations e
  return mempty

getTitle :: PandocMonad m => Element -> JATS m ()
getTitle e = do
  tit <-  case filterElement (named "article-title") e of
               Just s  -> getInlines s
               Nothing -> return mempty
  subtit <-  case filterElement (named "subtitle") e of
               Just s  -> (text ": " <>) <$>
                           getInlines s
               Nothing -> return mempty
  when (tit /= mempty) $ addMeta "title" tit
  when (subtit /= mempty) $ addMeta "subtitle" subtit

getAuthors :: PandocMonad m => Element -> JATS m ()
getAuthors e = do
  authors <- mapM getContrib $ filterElements
              (\x -> named "contrib" x &&
                     attrValue "contrib-type" x == "author") e
  authorNotes <- mapM getInlines $ filterElements (named "author-notes") e
  let authors' = case (reverse authors, authorNotes) of
                   ([], _)    -> []
                   (_, [])    -> authors
                   (a:as, ns) -> reverse as ++ [a <> mconcat ns]
  unless (null authors) $ addMeta "author" authors'

getAffiliations :: PandocMonad m => Element -> JATS m ()
getAffiliations x = do
  affs <- mapM getInlines $ filterChildren (named "aff") x
  unless (null affs) $ addMeta "institute" affs

getContrib :: PandocMonad m => Element -> JATS m Inlines
getContrib x = do
  given <- maybe (return mempty) getInlines
            $ filterElement (named "given-names") x
  family <- maybe (return mempty) getInlines
            $ filterElement (named "surname") x
  if given == mempty && family == mempty
     then return mempty
     else if given == mempty || family == mempty
          then return $ given <> family
          else return $ given <> space <> family

parseRefList :: PandocMonad m => Element -> JATS m Blocks
parseRefList e = do
  refs <- mapM parseRef $ filterChildren (named "ref") e
  addMeta "references" refs
  return mempty

parseRef :: PandocMonad m
         => Element -> JATS m (Map.Map String MetaValue)
parseRef e = do
  let refId = text $ attrValue "id" e
  let getInlineText n = maybe (return mempty) getInlines . filterChild (named n)
  case filterChild (named "element-citation") e of
       Just c  -> do
         let refType = text $
               case attrValue "publication-type" c of
                  "journal" -> "article-journal"
                  x -> x
         (refTitle, refContainerTitle) <- do
           t <- getInlineText "article-title" c
           ct <- getInlineText "source" c
           if t == mempty
              then return (ct, mempty)
              else return (t, ct)
         refLabel <- getInlineText "label" c
         refYear <- getInlineText "year" c
         refVolume <- getInlineText "volume" c
         refFirstPage <- getInlineText "fpage" c
         refLastPage <- getInlineText "lpage" c
         refPublisher <- getInlineText "publisher-name" c
         refPublisherPlace <- getInlineText "publisher-loc" c
         let refPages = refFirstPage <> (if refLastPage == mempty
                                            then mempty
                                            else text "\x2013" <> refLastPage)
         let personGroups' = filterChildren (named "person-group") c
         let getName nm = do
               given <- maybe (return mempty) getInlines
                         $ filterChild (named "given-names") nm
               family <- maybe (return mempty) getInlines
                         $ filterChild (named "surname") nm
               return $ toMetaValue $ Map.fromList [
                   ("given", given)
                 , ("family", family)
                 ]
         personGroups <- mapM (\pg ->
                                do names <- mapM getName
                                            (filterChildren (named "name") pg)
                                   return (attrValue "person-group-type" pg,
                                           toMetaValue names))
                         personGroups'
         return $ Map.fromList $
           [ ("id", toMetaValue refId)
           , ("type", toMetaValue refType)
           , ("title", toMetaValue refTitle)
           , ("container-title", toMetaValue refContainerTitle)
           , ("publisher", toMetaValue refPublisher)
           , ("publisher-place", toMetaValue refPublisherPlace)
           , ("title", toMetaValue refTitle)
           , ("issued", toMetaValue
                        $ Map.fromList [
                            ("year", refYear)
                          ])
           , ("volume", toMetaValue refVolume)
           , ("page", toMetaValue refPages)
           , ("citation-label", toMetaValue refLabel)
           ] ++ personGroups
       Nothing -> return $ Map.insert "id" (toMetaValue refId) mempty
       -- TODO handle mixed-citation

strContentRecursive :: Element -> String
strContentRecursive = strContent .
  (\e' -> e'{ elContent = map elementToStr $ elContent e' })

elementToStr :: Content -> Content
elementToStr (Elem e') = Text $ CData CDataText (strContentRecursive e') Nothing
elementToStr x = x

parseInline :: PandocMonad m => Content -> JATS m Inlines
parseInline (Text (CData _ s _)) = return $ text s
parseInline (CRef ref) =
  return $ maybe (text $ map toUpper ref) text $ lookupEntity ref
parseInline (Elem e) =
  case qName (elName e) of
        "italic" -> emph <$> innerInlines
        "bold" -> strong <$> innerInlines
        "strike" -> strikeout <$> innerInlines
        "sub" -> subscript <$> innerInlines
        "sup" -> superscript <$> innerInlines
        "underline" -> underlineSpan <$> innerInlines
        "break" -> return linebreak
        "sc" -> smallcaps <$> innerInlines

        "code" -> codeWithLang
        "monospace" -> codeWithLang

        "inline-graphic" -> getGraphic e
        "disp-quote" -> do
            qt <- gets jatsQuoteType
            let qt' = if qt == SingleQuote then DoubleQuote else SingleQuote
            modify $ \st -> st{ jatsQuoteType = qt' }
            contents <- innerInlines
            modify $ \st -> st{ jatsQuoteType = qt }
            return $ if qt == SingleQuote
                        then singleQuoted contents
                        else doubleQuoted contents

        "xref" -> do
            ils <- innerInlines
            let rid = attrValue "rid" e
            let refType = ("ref-type",) <$> maybeAttrValue "ref-type" e
            let attr = (attrValue "id" e, [], maybeToList refType)
            return $ if refType == Just ("ref-type","bibr")
                        then cite [Citation{
                                         citationId = rid
                                       , citationPrefix = []
                                       , citationSuffix = []
                                       , citationMode = NormalCitation
                                       , citationNoteNum = 0
                                       , citationHash = 0}] ils
                        else linkWith attr ('#' : rid) "" ils
        "ext-link" -> do
             ils <- innerInlines
             let title = fromMaybe "" $ findAttr (QName "title" (Just "http://www.w3.org/1999/xlink") Nothing) e
             let href = case findAttr (QName "href" (Just "http://www.w3.org/1999/xlink") Nothing) e of
                               Just h -> h
                               _      -> '#' : attrValue "rid" e
             let ils' = if ils == mempty then str href else ils
             let attr = (attrValue "id" e, [], [])
             return $ linkWith attr href title ils'

        "disp-formula" -> formula displayMath
        "inline-formula" -> formula math
        "math" | qPrefix (elName e) == Just "mml" -> return . math $ mathML e
        "tex-math" -> return . math $ strContent e

        "email" -> return $ link ("mailto:" ++ strContent e) ""
                          $ str $ strContent e
        "uri" -> return $ link (strContent e) "" $ str $ strContent e
        "fn" -> (note . mconcat) <$>
                         mapM parseBlock (elContent e)
        _          -> innerInlines
   where innerInlines = (trimInlines . mconcat) <$>
                          mapM parseInline (elContent e)
         mathML x =
            case readMathML . showElement $ everywhere (mkT removePrefix) x of
                Left _ -> mempty
                Right m -> writeTeX m
         formula constructor = do
            let whereToLook = fromMaybe e $ filterElement (named "alternatives") e
                texMaths = map strContent $
                            filterChildren (named  "tex-math") whereToLook
                mathMLs = map mathML $
                            filterChildren isMathML whereToLook
            return . mconcat . take 1 . map constructor $ texMaths ++ mathMLs

         isMathML x = qName   (elName x) == "math" &&
                      qPrefix (elName x) == Just "mml"
         removePrefix elname = elname { qPrefix = Nothing }
         codeWithLang = do
           let classes' = case attrValue "language" e of
                               "" -> []
                               l  -> [l]
           return $ codeWith (attrValue "id" e,classes',[]) $ strContentRecursive e
