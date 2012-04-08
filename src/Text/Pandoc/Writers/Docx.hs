{-
Copyright (C) 2012 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.Docx
   Copyright   : Copyright (C) 2012 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to docx.
-}
module Text.Pandoc.Writers.Docx ( writeDocx ) where
import Data.List ( intercalate )
import System.FilePath ( (</>) )
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.ByteString.Lazy.UTF8 ( fromString, toString )
import Text.Pandoc.UTF8 as UTF8
import System.IO ( stderr )
import Codec.Archive.Zip
import Data.Time.Clock.POSIX
import Paths_pandoc ( getDataFileName )
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import System.Directory
import Text.Pandoc.ImageSize
import Text.Pandoc.Shared hiding (Element)
import Text.Pandoc.Readers.TeXMath
import Text.Pandoc.Highlighting ( highlight )
import Text.Highlighting.Kate.Types ()
import Text.XML.Light
import Text.TeXMath
import Control.Monad.State
import Text.Highlighting.Kate

data WriterState = WriterState{
         stTextProperties :: [Element]
       , stParaProperties :: [Element]
       , stFootnotes      :: [Element]
       , stSectionIds     :: [String]
       , stExternalLinks  :: M.Map String String
       , stImages         :: M.Map FilePath (String, B.ByteString)
       , stListLevel      :: Int
       , stListMarker     :: ListMarker
       , stNumStyles      :: M.Map ListMarker Int
       , stLists          :: [ListMarker]
       }

data ListMarker = NoMarker
                | BulletMarker
                | NumberMarker ListNumberStyle ListNumberDelim Int
                deriving (Show, Read, Eq, Ord)

defaultWriterState :: WriterState
defaultWriterState = WriterState{
        stTextProperties = []
      , stParaProperties = []
      , stFootnotes      = []
      , stSectionIds     = []
      , stExternalLinks  = M.empty
      , stImages         = M.empty
      , stListLevel      = -1
      , stListMarker     = NoMarker
      , stNumStyles      = M.fromList [(NoMarker, 0)]
      , stLists          = [NoMarker]
      }

type WS a = StateT WriterState IO a

showTopElement' :: Element -> String
showTopElement' x = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++ showElement x

mknode :: Node t => String -> [(String,String)] -> t -> Element
mknode s attrs =
  add_attrs (map (\(k,v) -> Attr (unqual k) v) attrs) . node (unqual s)

-- | Produce an Docx file from a Pandoc document.
writeDocx :: Maybe FilePath -- ^ Path specified by --reference-docx
          -> WriterOptions  -- ^ Writer options
          -> Pandoc         -- ^ Document to convert
          -> IO B.ByteString
writeDocx mbRefDocx opts doc@(Pandoc (Meta tit auths date) _) = do
  let datadir = writerUserDataDir opts
  refArchive <- liftM toArchive $
       case mbRefDocx of
             Just f -> B.readFile f
             Nothing -> do
               let defaultDocx = getDataFileName "reference.docx" >>= B.readFile
               case datadir of
                     Nothing  -> defaultDocx
                     Just d   -> do
                        exists <- doesFileExist (d </> "reference.docx")
                        if exists
                           then B.readFile (d </> "reference.docx")
                           else defaultDocx

  (newContents, st) <- runStateT (writeOpenXML opts{writerWrapText = False} doc)
                       defaultWriterState
  epochtime <- floor `fmap` getPOSIXTime
  let imgs = M.elems $ stImages st
  let imgPath ident img = "media/" ++ ident ++
                            case imageType img of
                                  Just Png  -> ".png"
                                  Just Jpeg -> ".jpeg"
                                  Just Gif  -> ".gif"
                                  Nothing   -> ""
  let toImgRel (ident,img) =  mknode "Relationship" [("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"),("Id",ident),("Target",imgPath ident img)] ()
  let newrels = map toImgRel imgs
  let relpath = "word/_rels/document.xml.rels"
  let reldoc = case findEntryByPath relpath refArchive >>=
                    parseXMLDoc . toString . fromEntry of
                      Just d  -> d
                      Nothing -> error $ relpath ++ "missing in reference docx"
  let reldoc' = reldoc{ elContent = elContent reldoc ++ map Elem newrels }
  -- create entries for images
  let toImageEntry (ident,img) = toEntry ("word/" ++ imgPath ident img)
         epochtime img
  let imageEntries = map toImageEntry imgs
  -- NOW get list of external links and images from this, and do what's needed
  let toLinkRel (src,ident) =  mknode "Relationship" [("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink"),("Id",ident),("Target",src),("TargetMode","External") ] ()
  let newrels' = map toLinkRel $ M.toList $ stExternalLinks st
  let reldoc'' = reldoc' { elContent = elContent reldoc' ++ map Elem newrels' }
  let relEntry = toEntry relpath epochtime $ fromString $ showTopElement' reldoc''
  let contentEntry = toEntry "word/document.xml" epochtime $ fromString $ showTopElement' newContents
  -- styles
  let newstyles = styleToOpenXml $ writerHighlightStyle opts
  let stylepath = "word/styles.xml"
  let styledoc = case findEntryByPath stylepath refArchive >>=
                      parseXMLDoc . toString . fromEntry of
                        Just d  -> d
                        Nothing -> error $ stylepath ++ "missing in reference docx"
  let styledoc' = styledoc{ elContent = elContent styledoc ++ map Elem newstyles }
  let styleEntry = toEntry stylepath epochtime $ fromString $ showTopElement' styledoc'
  -- construct word/numbering.xml
  let numpath = "word/numbering.xml"
  let numEntry = toEntry numpath epochtime $ fromString $ showTopElement'
                 $ mkNumbering (stNumStyles st) (stLists st)
  let docPropsPath = "docProps/core.xml"
  let docProps = mknode "cp:coreProperties"
          [("xmlns:cp","http://schemas.openxmlformats.org/package/2006/metadata/core-properties")
          ,("xmlns:dc","http://purl.org/dc/elements/1.1/")
          ,("xmlns:dcterms","http://purl.org/dc/terms/")
          ,("xmlns:dcmitype","http://purl.org/dc/dcmitype/")
          ,("xmlns:xsi","http://www.w3.org/2001/XMLSchema-instance")]
          $ mknode "dc:title" [] (stringify tit)
          : mknode "dcterms:created" [("xsi:type","dcterms:W3CDTF")]
            (maybe "" id $ normalizeDate $ stringify date)
          : mknode "dcterms:modified" [("xsi:type","dcterms:W3CDTF")] () -- put current time here
          : map (mknode "dc:creator" [] . stringify) auths
  let docPropsEntry = toEntry docPropsPath epochtime $ fromString $ showTopElement' docProps
  let relsPath = "_rels/.rels"
  rels <- case findEntryByPath relsPath refArchive of
                   Just e  -> return $ toString $ fromEntry e
                   Nothing -> err 57 "could not find .rels/_rels in reference docx"
  -- fix .rels/_rels, which can get screwed up when reference.docx is edited by Word
  let rels' = substitute "http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties"
                  "http://schemas.openxmlformats.org/officedocument/2006/relationships/metadata/core-properties"
                  rels
  let relsEntry = toEntry relsPath epochtime $ fromString rels'
  let archive = foldr addEntryToArchive refArchive $
                  relsEntry : contentEntry : relEntry : numEntry : styleEntry : docPropsEntry : imageEntries
  return $ fromArchive archive

styleToOpenXml :: Style -> [Element]
styleToOpenXml style = parStyle : map toStyle alltoktypes
  where alltoktypes = enumFromTo KeywordTok NormalTok
        toStyle toktype = mknode "w:style" [("w:type","character"),
                           ("w:customStyle","1"),("w:styleId",show toktype)]
                             [ mknode "w:name" [("w:val",show toktype)] ()
                             , mknode "w:basedOn" [("w:val","VerbatimChar")] ()
                             , mknode "w:rPr" [] $
                               [ mknode "w:color" [("w:val",tokCol toktype)] ()
                                 | tokCol toktype /= "auto" ] ++
                               [ mknode "w:shd" [("w:val","clear"),("w:fill",tokBg toktype)] ()
                                 | tokBg toktype /= "auto" ] ++
                               [ mknode "w:b" [] () | tokFeature tokenBold toktype ] ++
                               [ mknode "w:i" [] () | tokFeature tokenItalic toktype ] ++
                               [ mknode "w:u" [] () | tokFeature tokenUnderline toktype ]
                             ]
        tokStyles = tokenStyles style
        tokFeature f toktype = maybe False f $ lookup toktype tokStyles
        tokCol toktype = maybe "auto" (drop 1 . fromColor)
                         $ (tokenColor =<< lookup toktype tokStyles)
                           `mplus` defaultColor style
        tokBg toktype = maybe "auto" (drop 1 . fromColor)
                         $ (tokenBackground =<< lookup toktype tokStyles)
                           `mplus` backgroundColor style
        parStyle = mknode "w:style" [("w:type","paragraph"),
                           ("w:customStyle","1"),("w:styleId","SourceCode")]
                             [ mknode "w:name" [("w:val","Source Code")] ()
                             , mknode "w:basedOn" [("w:val","Normal")] ()
                             , mknode "w:link" [("w:val","VerbatimChar")] ()
                             , mknode "w:pPr" []
                               $ mknode "w:wordWrap" [("w:val","off")] ()
                               : ( maybe [] (\col -> [mknode "w:shd" [("w:val","clear"),("w:fill",drop 1 $ fromColor col)] ()])
                                 $ backgroundColor style )
                             ]

mkNumbering :: M.Map ListMarker Int -> [ListMarker] -> Element
mkNumbering markers lists =
  mknode "w:numbering" [("xmlns:w","http://schemas.openxmlformats.org/wordprocessingml/2006/main")]
   $  map mkAbstractNum (M.toList markers)
   ++ zipWith (mkNum markers) lists [1..(length lists)]

mkNum :: M.Map ListMarker Int -> ListMarker -> Int -> Element
mkNum markers marker numid =
  mknode "w:num" [("w:numId",show numid)]
   $ mknode "w:abstractNumId" [("w:val",show absnumid)] ()
   : case marker of
       NoMarker     -> []
       BulletMarker -> []
       NumberMarker _ _ start ->
          map (\lvl -> mknode "w:lvlOverride" [("w:ilvl",show (lvl :: Int))]
              $ mknode "w:startOverride" [("w:val",show start)] ()) [0..6]
   where absnumid = maybe 0 id $ M.lookup marker markers

mkAbstractNum :: (ListMarker,Int) -> Element
mkAbstractNum (marker,numid) =
  mknode "w:abstractNum" [("w:abstractNumId",show numid)]
    $ mknode "w:multiLevelType" [("w:val","multilevel")] ()
    : map (mkLvl marker) [0..6]

mkLvl :: ListMarker -> Int -> Element
mkLvl marker lvl =
  mknode "w:lvl" [("w:ilvl",show lvl)] $
    [ mknode "w:start" [("w:val",start)] ()
      | marker /= NoMarker && marker /= BulletMarker ] ++
    [ mknode "w:numFmt" [("w:val",fmt)] ()
    , mknode "w:lvlText" [("w:val",lvltxt)] ()
    , mknode "w:lvlJc" [("w:val","left")] ()
    , mknode "w:pPr" []
      [ mknode "w:tabs" []
        $ mknode "w:tab" [("w:val","num"),("w:pos",show $ lvl * step)] ()
      , mknode "w:ind" [("w:left",show $ lvl * step + hang),("w:hanging",show hang)] ()
      ]
    ]
    where (fmt, lvltxt, start) =
            case marker of
                 NoMarker             -> ("bullet"," ","1")
                 BulletMarker         -> ("bullet",bulletFor lvl,"1")
                 NumberMarker st de n -> (styleFor st lvl
                                         ,patternFor de ("%" ++ show (lvl + 1))
                                         ,show n)
          step = 720
          hang = 480
          bulletFor 0 = "\8226"
          bulletFor 1 = "\9702"
          bulletFor 2 = "\8227"
          bulletFor 3 = "\8259"
          bulletFor 4 = "\8226"
          bulletFor 5 = "\9702"
          bulletFor _ = "\8227"
          styleFor UpperAlpha _ = "upperLetter"
          styleFor LowerAlpha _ = "lowerLetter"
          styleFor UpperRoman _ = "upperRoman"
          styleFor LowerRoman _ = "lowerRoman"
          styleFor Decimal _ = "decimal"
          styleFor DefaultStyle 1 = "decimal"
          styleFor DefaultStyle 2 = "lowerLetter"
          styleFor DefaultStyle 3 = "lowerRoman"
          styleFor DefaultStyle 4 = "decimal"
          styleFor DefaultStyle 5 = "lowerLetter"
          styleFor DefaultStyle 6 = "lowerRoman"
          styleFor _ _ = "decimal"
          patternFor OneParen s = s ++ ")"
          patternFor TwoParens s = "(" ++ s ++ ")"
          patternFor _ s = s ++ "."

-- | Convert Pandoc document to string in OpenXML format.
writeOpenXML :: WriterOptions -> Pandoc -> WS Element
writeOpenXML opts (Pandoc (Meta tit auths dat) blocks) = do
  title <- withParaProp (pStyle "Title") $ blocksToOpenXML opts [Para tit | not (null tit)]
  authors <- withParaProp (pStyle "Authors") $ blocksToOpenXML opts
                 [Para (intercalate [LineBreak] auths) | not (null auths)]
  date <- withParaProp (pStyle "Date") $ blocksToOpenXML opts [Para dat | not (null dat)]
  let convertSpace (Str x : Space : Str y : xs) = Str (x ++ " " ++ y) : xs
      convertSpace (Str x : Str y : xs) = Str (x ++ y) : xs
      convertSpace xs = xs
  let blocks' = bottomUp convertSpace $ blocks
  doc <- blocksToOpenXML opts blocks'
  notes' <- reverse `fmap` gets stFootnotes
  let notes = case notes' of
                   [] -> []
                   ns -> [mknode "w:footnotes" [] ns]
  let meta = title ++ authors ++ date
  return $ mknode "w:document"
            [("xmlns:w","http://schemas.openxmlformats.org/wordprocessingml/2006/main")
            ,("xmlns:m","http://schemas.openxmlformats.org/officeDocument/2006/math")
            ,("xmlns:r","http://schemas.openxmlformats.org/officeDocument/2006/relationships")
            ,("xmlns:o","urn:schemas-microsoft-com:office:office")
            ,("xmlns:v","urn:schemas-microsoft-com:vml")
            ,("xmlns:w10","urn:schemas-microsoft-com:office:word")
            ,("xmlns:a","http://schemas.openxmlformats.org/drawingml/2006/main")
            ,("xmlns:pic","http://schemas.openxmlformats.org/drawingml/2006/picture")
            ,("xmlns:wp","http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing")]
         $ mknode "w:body" [] (meta ++ doc ++ notes)

-- | Convert a list of Pandoc blocks to OpenXML.
blocksToOpenXML :: WriterOptions -> [Block] -> WS [Element]
blocksToOpenXML opts bls = concat `fmap` mapM (blockToOpenXML opts) bls

pStyle :: String -> Element
pStyle sty = mknode "w:pStyle" [("w:val",sty)] ()

rStyle :: String -> Element
rStyle sty = mknode "w:rStyle" [("w:val",sty)] ()

-- | Convert a Pandoc block element to OpenXML.
blockToOpenXML :: WriterOptions -> Block -> WS [Element]
blockToOpenXML _ Null = return []
blockToOpenXML opts (Header lev lst) = do
  contents <- withParaProp (pStyle $ "Heading" ++ show lev) $
               blockToOpenXML opts (Para lst)
  usedIdents <- gets stSectionIds
  let ident = uniqueIdent lst usedIdents
  modify $ \s -> s{ stSectionIds = ident : stSectionIds s }
  let bookmarkStart = mknode "w:bookmarkStart" [("w:id",ident)
                                               ,("w:name",ident)] ()
  let bookmarkEnd = mknode "w:bookmarkEnd" [("w:id",ident)] ()
  return $ [bookmarkStart] ++ contents ++ [bookmarkEnd]
blockToOpenXML opts (Plain lst) = blockToOpenXML opts (Para lst)
blockToOpenXML opts (Para x@[Image alt _]) = do
  paraProps <- getParaProps
  contents <- inlinesToOpenXML opts x
  captionNode <- withParaProp (pStyle "ImageCaption")
                 $ blockToOpenXML opts (Para alt)
  return $ mknode "w:p" [] (paraProps ++ contents) : captionNode
blockToOpenXML opts (Para lst) = do
  paraProps <- getParaProps
  contents <- inlinesToOpenXML opts lst
  return [mknode "w:p" [] (paraProps ++ contents)]
blockToOpenXML _ (RawBlock format str)
  | format == "openxml" = return [ x | Elem x <- parseXML str ]
  | otherwise           = return []
blockToOpenXML opts (BlockQuote blocks) =
  withParaProp (pStyle "BlockQuote") $ blocksToOpenXML opts blocks
blockToOpenXML opts (CodeBlock attrs str) =
  withParaProp (pStyle "SourceCode") $ blockToOpenXML opts $ Para [Code attrs str]
blockToOpenXML _ HorizontalRule = return [
  mknode "w:p" [] $ mknode "w:r" [] $ mknode "w:pict" []
    $ mknode "v:rect" [("style","width:0;height:1.5pt"),
                       ("o:hralign","center"),
                       ("o:hrstd","t"),("o:hr","t")] () ]
blockToOpenXML opts (Table caption aligns widths headers rows) = do
  let captionStr = stringify caption
  caption' <- if null caption
                 then return []
                 else withParaProp (pStyle "TableCaption")
                      $ blockToOpenXML opts (Para caption)
  let alignmentFor al = mknode "w:jc" [("w:val",alignmentToString al)] ()
  let cellToOpenXML (al, cell) = withParaProp (alignmentFor al)
                                    $ blocksToOpenXML opts cell
  headers' <- mapM cellToOpenXML $ zip aligns headers
  rows' <- mapM (\cells -> mapM cellToOpenXML $ zip aligns cells)
           $ rows
  let borderProps = mknode "w:tcPr" []
                    [ mknode "w:tcBorders" []
                      $ mknode "w:bottom" [("w:val","single")] ()
                    , mknode "w:vAlign" [("w:val","bottom")] () ]
  let mkcell border contents = mknode "w:tc" []
                            $ [ borderProps | border ] ++
                            if null contents
                               then [mknode "w:p" [] ()]
                               else contents
  let mkrow border cells = mknode "w:tr" [] $ map (mkcell border) cells
  let textwidth = 7920  -- 5.5 in in twips, 1/20 pt
  let mkgridcol w = mknode "w:gridCol"
                       [("w:w", show $ (floor (textwidth * w) :: Integer))] ()
  return $
    [ mknode "w:tbl" []
      ( mknode "w:tblPr" []
        ( [ mknode "w:tblStyle" [("w:val","TableNormal")] () ] ++
          [ mknode "w:tblCaption" [("w:val", captionStr)] ()
          | not (null caption) ] )
      : mknode "w:tblGrid" []
        (if all (==0) widths
            then []
            else map mkgridcol widths)
      : [ mkrow True headers' | not (all null headers) ] ++
      map (mkrow False) rows'
      )
    ] ++ caption'
blockToOpenXML opts (BulletList lst) = do
  let marker = BulletMarker
  addList marker
  asList $ concat `fmap` mapM (listItemToOpenXML opts marker) lst
blockToOpenXML opts (OrderedList (start, numstyle, numdelim) lst) = do
  let marker = NumberMarker numstyle numdelim start
  addList marker
  asList $ concat `fmap` mapM (listItemToOpenXML opts marker) lst
blockToOpenXML opts (DefinitionList items) =
  concat `fmap` mapM (definitionListItemToOpenXML opts) items

definitionListItemToOpenXML  :: WriterOptions -> ([Inline],[[Block]]) -> WS [Element]
definitionListItemToOpenXML opts (term,defs) = do
  term' <- withParaProp (pStyle "DefinitionTerm")
           $ blockToOpenXML opts (Para term)
  defs' <- withParaProp (pStyle "Definition")
           $ concat `fmap` mapM (blocksToOpenXML opts) defs
  return $ term' ++ defs'

getNumId :: WS Int
getNumId = length `fmap` gets stLists

addList :: ListMarker -> WS ()
addList marker = do
  lists <- gets stLists
  modify $ \st -> st{ stLists = lists ++ [marker] }
  numStyles <- gets stNumStyles
  case M.lookup marker numStyles of
           Just _  -> return ()
           Nothing -> modify $ \st ->
                 st{ stNumStyles = M.insert marker (M.size numStyles + 1) numStyles }

listItemToOpenXML :: WriterOptions -> ListMarker -> [Block] -> WS [Element]
listItemToOpenXML _ _ []                   = return []
listItemToOpenXML opts marker (first:rest) = do
  first' <- withMarker marker $ blockToOpenXML opts first
  rest'  <- withMarker NoMarker $ blocksToOpenXML opts rest
  return $ first' ++ rest'

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft -> "left"
                                 AlignRight -> "right"
                                 AlignCenter -> "center"
                                 AlignDefault -> "left"

-- | Convert a list of inline elements to OpenXML.
inlinesToOpenXML :: WriterOptions -> [Inline] -> WS [Element]
inlinesToOpenXML opts lst = concat `fmap` mapM (inlineToOpenXML opts) lst

withMarker :: ListMarker -> WS a -> WS a
withMarker m p = do
  origMarker <- gets stListMarker
  modify $ \st -> st{ stListMarker = m }
  result <- p
  modify $ \st -> st{ stListMarker = origMarker }
  return result

asList :: WS a -> WS a
asList p = do
  origListLevel <- gets stListLevel
  modify $ \st -> st{ stListLevel = stListLevel st + 1 }
  result <- p
  modify $ \st -> st{ stListLevel = origListLevel }
  return result

getTextProps :: WS [Element]
getTextProps = do
  props <- gets stTextProperties
  return $ if null props
              then []
              else [mknode "w:rPr" [] $ props]

pushTextProp :: Element -> WS ()
pushTextProp d = modify $ \s -> s{ stTextProperties = d : stTextProperties s }

popTextProp :: WS ()
popTextProp = modify $ \s -> s{ stTextProperties = drop 1 $ stTextProperties s }

withTextProp :: Element -> WS a -> WS a
withTextProp d p = do
  pushTextProp d
  res <- p
  popTextProp
  return res

getParaProps :: WS [Element]
getParaProps = do
  props <- gets stParaProperties
  listLevel <- gets stListLevel
  listMarker <- gets stListMarker
  numid <- case listMarker of
                 NoMarker -> return 1
                 _        -> getNumId
  let listPr = if listLevel >= 0
                  then [ mknode "w:numPr" []
                         [ mknode "w:numId" [("w:val",show numid)] ()
                         , mknode "w:ilvl" [("w:val",show listLevel)] () ]
                       ]
                  else []
  return $ case props ++ listPr of
                [] -> []
                ps -> [mknode "w:pPr" [] ps]

pushParaProp :: Element -> WS ()
pushParaProp d = modify $ \s -> s{ stParaProperties = d : stParaProperties s }

popParaProp :: WS ()
popParaProp = modify $ \s -> s{ stParaProperties = drop 1 $ stParaProperties s }

withParaProp :: Element -> WS a -> WS a
withParaProp d p = do
  pushParaProp d
  res <- p
  popParaProp
  return res

formattedString :: String -> WS [Element]
formattedString str = do
  props <- getTextProps
  return [ mknode "w:r" [] $
             props ++
             [ mknode "w:t" [("xml:space","preserve")] str ] ]

-- | Convert an inline element to OpenXML.
inlineToOpenXML :: WriterOptions -> Inline -> WS [Element]
inlineToOpenXML _ (Str str) = formattedString str
inlineToOpenXML opts Space = inlineToOpenXML opts (Str " ")
inlineToOpenXML opts (Strong lst) =
  withTextProp (mknode "w:b" [] ()) $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Emph lst) =
  withTextProp (mknode "w:i" [] ()) $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Subscript lst) =
  withTextProp (mknode "w:vertAlign" [("w:val","subscript")] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Superscript lst) =
  withTextProp (mknode "w:vertAlign" [("w:val","superscript")] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML opts (SmallCaps lst) =
  withTextProp (mknode "w:smallCaps" [] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Strikeout lst) =
  withTextProp (mknode "w:strike" [] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML _ LineBreak = return [ mknode "w:br" [] () ]
inlineToOpenXML _ (RawInline f str)
  | f == "openxml" = return [ x | Elem x <- parseXML str ]
  | otherwise      = return []
inlineToOpenXML opts (Quoted quoteType lst) =
  inlinesToOpenXML opts $ [Str open] ++ lst ++ [Str close]
    where (open, close) = case quoteType of
                            SingleQuote -> ("\x2018", "\x2019")
                            DoubleQuote -> ("\x201C", "\x201D")
inlineToOpenXML opts (Math InlineMath str) =
  case texMathToOMML DisplayInline str of
        Right r -> return [r]
        Left  _ -> inlinesToOpenXML opts (readTeXMath str)
inlineToOpenXML opts (Math DisplayMath str) =
  case texMathToOMML DisplayBlock str of
        Right r -> return [br, r, br]
        Left  _ -> do
            fallback <- inlinesToOpenXML opts (readTeXMath str)
            return $ [br] ++ fallback ++ [br]
    where br = mknode "w:br" [] ()
inlineToOpenXML opts (Cite _ lst) = inlinesToOpenXML opts lst
inlineToOpenXML _ (Code attrs str) =
  withTextProp (rStyle "VerbatimChar")
  $ case highlight formatOpenXML attrs str of
         Nothing  -> intercalate [mknode "w:br" [] ()]
                     `fmap` (mapM formattedString $ lines str)
         Just h   -> return h
     where formatOpenXML _fmtOpts = intercalate [mknode "w:br" [] ()] .
                                    map (map toHlTok)
           toHlTok (toktype,tok) = mknode "w:r" []
                                     [ mknode "w:rPr" []
                                       [ rStyle $ show toktype ]
                                     , mknode "w:t" [("xml:space","preserve")] tok ]
inlineToOpenXML opts (Note bs) = do
  notes <- gets stFootnotes
  let notenum = length notes + 1
  let notemarker = mknode "w:r" []
                   [ mknode "w:rPr" [] (rStyle "FootnoteReference")
                   , mknode "w:footnoteRef" [] () ]
  let notemarkerXml = RawInline "openxml" $ ppElement notemarker
  let insertNoteRef (Plain ils : xs) = Plain (notemarkerXml : ils) : xs
      insertNoteRef (Para ils  : xs) = Para  (notemarkerXml : ils) : xs
      insertNoteRef xs               = Para [notemarkerXml] : xs
  oldListLevel <- gets stListLevel
  oldParaProperties <- gets stParaProperties
  oldTextProperties <- gets stTextProperties
  modify $ \st -> st{ stListLevel = -1, stParaProperties = [], stTextProperties = [] }
  contents <- withParaProp (pStyle "FootnoteText") $ blocksToOpenXML opts
                $ insertNoteRef bs
  modify $ \st -> st{ stListLevel = oldListLevel, stParaProperties = oldParaProperties,
                      stTextProperties = oldTextProperties }
  let newnote = mknode "w:footnote" [("w:id",show notenum)] $ contents
  modify $ \s -> s{ stFootnotes = newnote : notes }
  return [ mknode "w:r" []
           [ mknode "w:rPr" [] (rStyle "FootnoteReference")
           , mknode "w:footnoteReference" [("w:id", show notenum)] () ] ]
-- internal link:
inlineToOpenXML opts (Link txt ('#':xs,_)) = do
  contents <- withTextProp (rStyle "Hyperlink") $ inlinesToOpenXML opts txt
  return [ mknode "w:hyperlink" [("w:anchor",xs)] contents ]
-- external link:
inlineToOpenXML opts (Link txt (src,_)) = do
  contents <- withTextProp (rStyle "Hyperlink") $ inlinesToOpenXML opts txt
  extlinks <- gets stExternalLinks
  ind <- case M.lookup src extlinks of
            Just i   -> return i
            Nothing  -> do
              let i = "link" ++ show (M.size extlinks)
              modify $ \st -> st{ stExternalLinks =
                        M.insert src i extlinks }
              return i
  return [ mknode "w:hyperlink" [("r:id",ind)] contents ]
inlineToOpenXML opts (Image alt (src, tit)) = do
  exists <- liftIO $ doesFileExist src
  if exists
     then do
       imgs <- gets stImages
       (ident,size) <- case M.lookup src imgs of
                            Just (i,img) -> return (i, imageSize img)
                            Nothing -> do
                              img <- liftIO $ B.readFile src
                              let ident' = "image" ++ show (M.size imgs + 1)
                              let size'  = imageSize img
                              modify $ \st -> st{
                                 stImages = M.insert src (ident',img) $ stImages st }
                              return (ident',size')
       let (xpt,ypt) = maybe (120,120) sizeInPoints size
       -- 12700 emu = 1 pt
       let (xemu,yemu) = (xpt * 12700, ypt * 12700)
       let cNvPicPr = mknode "pic:cNvPicPr" [] $
                        mknode "a:picLocks" [("noChangeArrowheads","1"),("noChangeAspect","1")] ()
       let nvPicPr  = mknode "pic:nvPicPr" []
                       [ mknode "pic:cNvPr"
                           [("descr",src),("id","0"),("name","Picture")] ()
                       , cNvPicPr ]
       let blipFill = mknode "pic:blipFill" []
                        [ mknode "a:blip" [("r:embed",ident)] ()
                        , mknode "a:stretch" [] $ mknode "a:fillRect" [] () ]
       let xfrm =    mknode "a:xfrm" []
                       [ mknode "a:off" [("x","0"),("y","0")] ()
                       , mknode "a:ext" [("cx",show xemu),("cy",show yemu)] () ]
       let prstGeom = mknode "a:prstGeom" [("prst","rect")] $
                        mknode "a:avLst" [] ()
       let ln =      mknode "a:ln" [("w","9525")]
                       [ mknode "a:noFill" [] ()
                       , mknode "a:headEnd" [] ()
                       , mknode "a:tailEnd" [] () ]
       let spPr =    mknode "pic:spPr" [("bwMode","auto")]
                       [xfrm, prstGeom, mknode "a:noFill" [] (), ln]
       let graphic = mknode "a:graphic" [] $
                       mknode "a:graphicData" [("uri","http://schemas.openxmlformats.org/drawingml/2006/picture")]
                         [ mknode "pic:pic" []
                           [ nvPicPr
                           , blipFill
                           , spPr ] ]
       return [ mknode "w:r" [] $
           mknode "w:drawing" [] $
             mknode "wp:inline" []
               [ mknode "wp:extent" [("cx",show xemu),("cy",show yemu)] ()
               , mknode "wp:effectExtent" [("b","0"),("l","0"),("r","0"),("t","0")] ()
               , mknode "wp:docPr" [("descr",tit),("id","1"),("name","Picture")] ()
               , graphic ] ]
     else do
       liftIO $ UTF8.hPutStrLn stderr $
          "Could not find image `" ++ src ++ "', skipping..."
       inlinesToOpenXML opts alt
