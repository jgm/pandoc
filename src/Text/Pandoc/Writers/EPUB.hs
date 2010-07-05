{-
Copyright (C) 2010 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.EPUB
   Copyright   : Copyright (C) 2010 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to EPUB.
-}
module Text.Pandoc.Writers.EPUB ( writeEPUB ) where
import Data.IORef
import Data.Maybe ( fromMaybe, isNothing )
import Data.List ( findIndices, isPrefixOf )
import System.Environment ( getEnv )
import System.FilePath ( (</>), takeBaseName, takeExtension )
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 ( fromString )
import Codec.Archive.Zip
import System.Time
import Text.Pandoc.Shared hiding ( Element )
import Text.Pandoc.Definition
import Control.Monad (liftM)
import Text.XML.Light hiding (ppTopElement)
import Text.Pandoc.UUID
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Writers.Markdown ( writePlain )
import Data.Char ( toLower )

-- | Produce an EPUB file from a Pandoc document.
writeEPUB :: FilePath       -- ^ Relative directory of source file
          -> String         -- ^ EPUB stylesheet
          -> WriterOptions  -- ^ Writer options
          -> Pandoc         -- ^ Document to convert
          -> IO B.ByteString
writeEPUB sourceDir stylesheet opts doc = do
  (TOD epochtime _) <- getClockTime
  let opts' = opts{ writerEmailObfuscation = NoObfuscation
                  , writerStandalone = True
                  , writerWrapText = False }
  -- mimetype
  let mimetypeEntry = toEntry "mimetype" epochtime $ fromString "application/epub+zip"
  -- container.xml
  let containerData = fromString $ ppTopElement $
       unode "container" ! [("version","1.0")
              ,("xmlns","urn:oasis:names:tc:opendocument:xmlns:container")] $
         unode "rootfiles" $
           unode "rootfile" ! [("full-path","content.opf")
               ,("media-type","application/oebps-package+xml")] $ ()
  let containerEntry = toEntry "META-INF/container.xml" epochtime containerData
  -- stylesheet
  let stylesheetEntry = toEntry "stylesheet.css" epochtime $
                             fromString stylesheet
  -- title page
  let vars = writerVariables opts'
  let tpContent = fromString $
          writeHtmlString opts'{writerTemplate = pageTemplate
                               ,writerVariables = ("titlepage","yes"):vars} doc
  let tpEntry = toEntry "title_page.xhtml" epochtime tpContent
  -- handle pictures
  picEntriesRef <- newIORef ([] :: [Entry])
  Pandoc meta blocks <- liftM (processWith transformBlock) $
     processWithM (transformInline (writerHTMLMathMethod opts)
                     sourceDir picEntriesRef) doc
  picEntries <- readIORef picEntriesRef
  -- body pages
  let isH1 (Header 1 _) = True
      isH1 _            = False
  let chunks = splitByIndices (dropWhile (==0) $ findIndices isH1 blocks) blocks
  let titleize (Header 1 xs : ys) = Pandoc meta{docTitle = xs} ys
      titleize xs                 = Pandoc meta xs
  let chapToHtml = writeHtmlString opts'{ writerTemplate = pageTemplate
                                        , writerHTMLMathMethod = PlainMath}
  let chapters = map titleize chunks
  let chapterToEntry :: Int -> Pandoc -> Entry
      chapterToEntry num chap = toEntry ("ch" ++ show num ++ ".xhtml")
                                   epochtime $ fromString $ chapToHtml chap
  let chapterEntries = zipWith chapterToEntry [1..] chapters
  -- contents.opf
  uuid <- getRandomUUID
  let chapterNode ent = unode "item" !
                           [("id", takeBaseName $ eRelativePath ent),
                            ("href", eRelativePath ent),
                            ("media-type", "application/xhtml+xml")] $ ()
  let chapterRefNode ent = unode "itemref" !
                             [("idref", takeBaseName $ eRelativePath ent)] $ ()
  let pictureNode ent = unode "item" !
                           [("id", takeBaseName $ eRelativePath ent),
                            ("href", eRelativePath ent),
                            ("media-type", fromMaybe "application/octet-stream"
                               $ imageTypeOf $ eRelativePath ent)] $ ()
  let plainify t = removeTrailingSpace $
                   writePlain opts'{ writerStandalone = False } $
                    Pandoc meta [Plain t]
  let plainTitle = plainify $ docTitle meta
  let plainAuthors = map plainify $ docAuthors meta
  lang <- catch (liftM (takeWhile (/='.')) $ getEnv "lang")
                (\_ -> return "en-US")
  let contentsData = fromString $ ppTopElement $
        unode "package" ! [("version","2.0")
                          ,("xmlns","http://www.idpf.org/2007/opf")
                          ,("unique-identifier","BookId")] $
          [ metadataElement (writerEPUBMetadata opts')
              uuid lang plainTitle plainAuthors
          , unode "manifest" $
             [ unode "item" ! [("id","ncx"), ("href","toc.ncx")
                              ,("media-type","application/x-dtbncx+xml")] $ ()
             , unode "item" ! [("id","style"), ("href","stylesheet.css")
                              ,("media-type","text/css")] $ ()
             ] ++
             map chapterNode (tpEntry : chapterEntries) ++
             map pictureNode picEntries
          , unode "spine" ! [("toc","ncx")] $
              map chapterRefNode (tpEntry : chapterEntries)
          ]
  let contentsEntry = toEntry "content.opf" epochtime contentsData
  -- toc.ncx
  let navPointNode ent n tit = unode "navPoint" !
                                [("id", "navPoint-" ++ show n)
                                ,("playOrder", show n)] $
                                   [ unode "navLabel" $ unode "text" tit
                                   , unode "content" ! [("src",
                                        eRelativePath ent)] $ ()
                                   ]
  let tocData = fromString $ ppTopElement $
        unode "ncx" ! [("version","2005-1")
                       ,("xmlns","http://www.daisy.org/z3986/2005/ncx/")] $
          [ unode "head"
             [ unode "meta" ! [("name","dtb:uid")
                              ,("content", show uuid)] $ ()
             , unode "meta" ! [("name","dtb:depth")
                              ,("content", "1")] $ ()
             , unode "meta" ! [("name","dtb:totalPageCount")
                              ,("content", "0")] $ ()
             , unode "meta" ! [("name","dtb:maxPageNumber")
                              ,("content", "0")] $ ()
             ] 
          , unode "docTitle" $ unode "text" $ plainTitle
          , unode "navMap" $ zipWith3 navPointNode (tpEntry : chapterEntries)
                                [1..(length chapterEntries + 1)]
                                ("Title Page" : map (\(Pandoc m _) ->
                                   plainify $ docTitle m) chapters)
          ]
  let tocEntry = toEntry "toc.ncx" epochtime tocData
  -- construct archive
  let archive = foldr addEntryToArchive emptyArchive
                 (mimetypeEntry : containerEntry : stylesheetEntry : tpEntry :
                  contentsEntry : tocEntry : (picEntries ++ chapterEntries) )
  return $ fromArchive archive

metadataElement :: String -> UUID -> String -> String -> [String] -> Element
metadataElement metadataXML uuid lang title authors =
  let userNodes = parseXML metadataXML
      elt = unode "metadata" ! [("xmlns:dc","http://purl.org/dc/elements/1.1/")
                               ,("xmlns:opf","http://www.idpf.org/2007/opf")] $
            filter isDublinCoreElement $ onlyElems userNodes
      dublinElements = ["contributor","coverage","creator","date",
            "description","format","identifier","language","publisher",
            "relation","rights","source","subject","title","type"]
      isDublinCoreElement e = qPrefix (elName e) == Just "dc" &&
                              qName (elName e) `elem` dublinElements
      contains e n = not (null (findElements (QName n Nothing (Just "dc")) e))
      newNodes = [ unode "dc:title" title | not (elt `contains` "title") ] ++
           [ unode "dc:language" lang | not (elt `contains` "language") ] ++
           [ unode "dc:identifier" ! [("id","BookId")] $ show uuid |
               not (elt `contains` "identifier") ] ++
           [ unode "dc:creator" ! [("opf:role","aut")] $ a | a <- authors ]
  in  elt{ elContent = elContent elt ++ map Elem newNodes }

transformInline :: HTMLMathMethod
                -> FilePath
                -> IORef [Entry]
                -> Inline
                -> IO Inline
transformInline _ _ _ (Image lab (src,_)) | isNothing (imageTypeOf src) =
  return (Emph lab) 
transformInline _ sourceDir picsRef (Image lab (src,tit)) = do
  entries <- readIORef picsRef
  let newsrc = "images/img" ++ show (length entries) ++ takeExtension src
  catch (readEntry [] (sourceDir </> src) >>= \entry ->
           modifyIORef picsRef (entry{ eRelativePath = newsrc } :) >>
           return (Image lab (newsrc, tit)))
        (\_ -> return (Emph lab))
transformInline (MathML _) _ _ x@(Math _ _) = do
  let writeHtmlInline opts z = removeTrailingSpace $
         writeHtmlString opts $ Pandoc (Meta [] [] []) [Plain [z]]
      mathml = writeHtmlInline defaultWriterOptions{
                 writerHTMLMathMethod = MathML Nothing } x
      fallback = writeHtmlInline defaultWriterOptions{
                 writerHTMLMathMethod = PlainMath } x
      inOps = "<ops:switch xmlns:ops=\"http://www.idpf.org/2007/ops\">" ++
       "<ops:case required-namespace=\"http://www.w3.org/1998/Math/MathML\">" ++
       mathml ++ "</ops:case><ops:default>" ++ fallback ++ "</ops:default>" ++
       "</ops:switch>"
  return $ HtmlInline $ if "<math" `isPrefixOf` mathml then inOps else mathml
transformInline _ _ _ (HtmlInline _) = return $ Str ""
transformInline _ _ _ x = return x

transformBlock :: Block -> Block
transformBlock (RawHtml _) = Null
transformBlock x = x

(!) :: Node t => (t -> Element) -> [(String, String)] -> t -> Element
(!) f attrs n = add_attrs (map (\(k,v) -> Attr (unqual k) v) attrs) (f n)

-- | Version of 'ppTopElement' that specifies UTF-8 encoding.
ppTopElement :: Element -> String
ppTopElement = ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++) . ppElement

imageTypeOf :: FilePath -> Maybe String
imageTypeOf x = case drop 1 (map toLower (takeExtension x)) of
                     "jpg"       -> Just "image/jpeg"
                     "jpeg"      -> Just "image/jpeg"
                     "jfif"      -> Just "image/jpeg"
                     "png"       -> Just "image/png"
                     "gif"       -> Just "image/gif"
                     "svg"       -> Just "image/svg+xml"
                     _           -> Nothing

pageTemplate :: String
pageTemplate = unlines
 [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
 , "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
 , "<html xmlns=\"http://www.w3.org/1999/xhtml\">"
 , "<head>"
 , "<title>$title$</title>"
 , "<link href=\"stylesheet.css\" type=\"text/css\" rel=\"stylesheet\" />"
 , "</head>"
 , "<body>"
 , "$if(titlepage)$"
 , "<h1 class=\"title\">$title$</h1>"
 , "$for(author)$"
 , "<h2 class=\"author\">$author$</h2>"
 , "$endfor$"
 , "$else$"
 , "<h1>$title$</h1>"
 , "$body$"
 , "$endif$"
 , "</body>"
 , "</html>"
 ]

