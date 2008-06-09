{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE PatternGuards #-}
{-
Copyright (C) 2008 Andrea Rossato <andrea.rossato@ing.unitn.it>

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
   Module      : Text.Pandoc.Writers.OpenDocument
   Copyright   : Copyright (C) 2008 Andrea Rossato
   License     : GNU GPL, version 2 or above

   Maintainer  : Andrea Rossato <andrea.rossato@ing.unitn.it>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to OpenDocument XML.
-}
module Text.Pandoc.Writers.OpenDocument ( writeOpenDocument ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.XML
import Text.Pandoc.Readers.TeXMath
import Text.PrettyPrint.HughesPJ hiding ( Str )

import Control.Applicative ((<$>))
import Control.Monad.State hiding ( when )
import Data.Char (chr)

-- | Auxiliary function to convert Plain block to Para.
plainToPara :: Block -> Block
plainToPara (Plain x) = Para x
plainToPara x = x

--
-- OpenDocument writer
--

data WriterState =
    WriterState { stNotes       :: [Doc]
                , stTableStyles :: [Doc]
                , stParaStyles  :: [Doc]
                , stListStyles  :: [(Int, [Doc])]
                , indentPara    :: Int
                , inDefinition  :: Bool
                }

defaultWriterState :: WriterState
defaultWriterState =
    WriterState { stNotes       = []
                , stTableStyles = []
                , stParaStyles  = []
                , stListStyles  = []
                , indentPara    = 0
                , inDefinition  = False
                }

addTableStyle :: Doc -> State WriterState ()
addTableStyle i = modify $ \s -> s { stTableStyles = i : stTableStyles s }

addNote :: Doc -> State WriterState ()
addNote i = modify $ \s -> s { stNotes = i : stNotes s }

addParaStyle :: Doc -> State WriterState ()
addParaStyle i = modify $ \s -> s { stParaStyles = i : stParaStyles s }

increaseIndent :: State WriterState ()
increaseIndent = modify $ \s -> s { indentPara = 1 + indentPara s }

resetIndent :: State WriterState ()
resetIndent = modify $ \s -> s { indentPara = 0 }

setInDefinitionList :: Bool -> State WriterState ()
setInDefinitionList b = modify $  \s -> s { inDefinition = b }

inParagraphTags :: Doc -> Doc
inParagraphTags = inTags False "text:p" [("text:style-name", "Text_20_body")]

inParagraphTagsWithStyle :: String -> Doc -> Doc
inParagraphTagsWithStyle sty = inTags False "text:p" [("text:style-name", sty)]

inSpanTags :: String -> Doc -> Doc
inSpanTags s = inTags False "text:span" [("text:style-name",s)]

inHeaderTags :: Int -> Doc -> Doc
inHeaderTags i = inTags False "text:h" [ ("text:style-name", "Heading_20_" ++ show i)
                                       , ("text:outline-level", show i)]

inQuotes :: QuoteType -> Doc -> Doc
inQuotes SingleQuote s = text "&#8216;" <> s <> text "&#8217;"
inQuotes DoubleQuote s = text "&#8220;" <> s <> text "&#8221;"

-- | Convert list of authors to a docbook <author> section
authorToOpenDocument :: [Char] -> Doc
authorToOpenDocument name =
  if ',' `elem` name
    then -- last name first
         let (lastname, rest) = break (==',') name
             firstname = removeLeadingSpace rest in
         inParagraphTags $ (text $ escapeStringForXML firstname) <+>
                           (text $ escapeStringForXML lastname)
    else -- last name last
         let namewords = words name
             lengthname = length namewords
             (firstname, lastname) = case lengthname of
               0  -> ("","")
               1  -> ("", name)
               n  -> (joinWithSep " " (take (n-1) namewords), last namewords)
          in inParagraphTags $ (text $ escapeStringForXML firstname) <+>
                               (text $ escapeStringForXML lastname)

-- | Convert Pandoc document to string in OpenDocument format.
writeOpenDocument :: WriterOptions -> Pandoc -> String
writeOpenDocument opts (Pandoc (Meta title authors date) blocks) =
  let when p a = if p then a else empty
      root     = inTags True "office:document-content" openDocumentNameSpaces
      header   = when (writerStandalone opts) $ text (writerHeader opts)
      title'   = case runState (wrap opts title) defaultWriterState of
                   (t,_) -> if isEmpty t then empty else inHeaderTags 1 t
      authors' = when (authors /= []) $ vcat (map authorToOpenDocument authors)
      date'    = when (date    /= []) $ inParagraphTags (text $ escapeStringForXML date)
      meta     = when (writerStandalone opts) $ title' $$ authors' $$ date'
      before   = writerIncludeBefore opts
      after    = writerIncludeAfter opts
      (doc, s) = runState (blocksToOpenDocument opts blocks) defaultWriterState

      body     = (if null before then empty else text before) $$
                 doc $$
                 (if null after then empty else text after)
      body'    = if writerStandalone opts
                   then inTagsIndented "office:body" $
                        inTagsIndented "office:text" (meta $$ body)
                   else body
      listStyle (n,l) = inTags True "text:list-style" [("style:name", "L" ++ show n)] (vcat l)
      listStyles  = map listStyle (stListStyles s)
  in  render $ header $$ root (generateStyles (stTableStyles s ++ stParaStyles s ++ listStyles) $$ body' $$ text "")

withParagraphStyle :: WriterOptions -> String -> [Block] -> State WriterState Doc
withParagraphStyle  o s (b:bs)
    | Para l <- b = go =<< inParagraphTagsWithStyle s <$> inlinesToOpenDocument o l
    | otherwise   = go =<< blockToOpenDocument o b
    where go i = ($$) i <$>  withParagraphStyle o s bs
withParagraphStyle _ _ [] = return empty

inPreformattedTags :: String -> State WriterState Doc
inPreformattedTags s = do
  n <- paraStyle "Preformatted_20_Text" []
  return . inParagraphTagsWithStyle ("P" ++ show n) . text $ s

orderedListToOpenDocument :: WriterOptions -> Int -> [[Block]] -> State WriterState Doc
orderedListToOpenDocument o pn bs =
    vcat . map (inTagsIndented "text:list-item") <$>
    mapM (orderedItemToOpenDocument o pn . map plainToPara) bs

orderedItemToOpenDocument :: WriterOptions -> Int -> [Block] -> State WriterState Doc
orderedItemToOpenDocument  o n (b:bs)
    | OrderedList a l <- b = newLevel a l
    | Para          l <- b = go =<< inParagraphTagsWithStyle ("P" ++ show n) <$> inlinesToOpenDocument o l
    | otherwise            = go =<< blockToOpenDocument o b
    where
      go i = ($$) i <$> orderedItemToOpenDocument o n bs
      newLevel a l = do
        nn <- length <$> gets stParaStyles
        ls <- head   <$> gets stListStyles
        modify $ \s -> s { stListStyles = orderedListLevelStyle a ls : tail (stListStyles s) }
        inTagsIndented "text:list" <$> orderedListToOpenDocument o nn l
orderedItemToOpenDocument  _ _ [] = return empty

newOrderedListStyle :: ListAttributes -> State WriterState (Int,Int)
newOrderedListStyle a = do
  ln <- (+) 1 . length  <$> gets stListStyles
  pn <- paraListStyle ln
  let nbs = orderedListLevelStyle a (ln, [])
  modify $ \s -> s { stListStyles = nbs : stListStyles s }
  return (ln,pn)

bulletListToOpenDocument :: WriterOptions -> [[Block]] -> State WriterState Doc
bulletListToOpenDocument o b = do
  ln <- (+) 1 . length <$> gets stListStyles
  (pn,ns) <- bulletListStyle ln
  modify $ \s -> s { stListStyles = ns : stListStyles s }
  is <- listItemsToOpenDocument ("P" ++ show pn) o b
  return $ inTags True "text:list" [("text:style-name", "L" ++ show ln)] is

listItemsToOpenDocument :: String -> WriterOptions -> [[Block]] -> State WriterState Doc
listItemsToOpenDocument s o is =
    vcat . map (inTagsIndented "text:list-item") <$> mapM (withParagraphStyle o s . map plainToPara) is

deflistItemToOpenDocument :: WriterOptions -> ([Inline],[Block]) -> State WriterState Doc
deflistItemToOpenDocument o (t,d) = do
  t' <- withParagraphStyle o "Definition_20_Term"       [Para t]
  d' <- withParagraphStyle o "Definition_20_Definition" (map plainToPara d)
  return $ t' $$ d'

inBlockQuote :: WriterOptions -> Int -> [Block] -> State WriterState Doc
inBlockQuote  o i (b:bs)
    | BlockQuote l <- b = do increaseIndent
                             ni <- paraStyle "Quotations" []
                             go ni =<< inBlockQuote o ni l
    | Para       l <- b = do go i  =<< inParagraphTagsWithStyle ("P" ++ show  i) <$> inlinesToOpenDocument o l
    | otherwise         = do go i  =<< blockToOpenDocument o b
    where go  ni block  = ($$) block <$>  inBlockQuote o ni bs
inBlockQuote     _ _ [] = resetIndent >> return empty

-- | Convert a list of Pandoc blocks to OpenDocument.
blocksToOpenDocument :: WriterOptions -> [Block] -> State WriterState Doc
blocksToOpenDocument o b = vcat <$> mapM (blockToOpenDocument o) b

-- | Convert a Pandoc block element to OpenDocument.
blockToOpenDocument :: WriterOptions -> Block -> State WriterState Doc
blockToOpenDocument o bs
    | Plain          b <- bs = wrap o b
    | Para           b <- bs = inParagraphTags <$> wrap o b
    | Header       i b <- bs = inHeaderTags  i <$> wrap o b
    | BlockQuote     b <- bs = mkBlockQuote b
    | CodeBlock    _ s <- bs = preformatted s
    | RawHtml        _ <- bs = return empty
    | DefinitionList b <- bs = defList b
    | BulletList     b <- bs = bulletListToOpenDocument o b
    | OrderedList  a b <- bs = orderedList a b
    | Table  c a w h r <- bs = table c a w h r
    | Null             <- bs = return empty
    | HorizontalRule   <- bs = return empty
    | otherwise              = return empty
    where
      defList       b = do setInDefinitionList True
                           r <- vcat  <$> mapM (deflistItemToOpenDocument o) b
                           setInDefinitionList False
                           return r
      preformatted  s = vcat <$> mapM (inPreformattedTags . escapeStringForXML) (lines s)
      mkBlockQuote  b = do increaseIndent
                           i <- paraStyle "Quotations" []
                           inBlockQuote o i (map plainToPara b)
      orderedList a b = do (ln,pn) <- newOrderedListStyle a
                           inTags True "text:list" [ ("text:style-name", "L" ++ show ln)]
                                      <$> orderedListToOpenDocument o pn b
      table c a w h r = do
        tn <- length <$> gets stTableStyles
        pn <- length <$> gets stParaStyles
        let  genIds      = map chr [65..]
             name        = "Table" ++ show (tn + 1)
             columnIds   = zip genIds w
             mkColumn  n = selfClosingTag "table:table-column" [("table:style-name", name ++ "." ++ [fst n])]
             columns     = map mkColumn columnIds
             paraHStyles = paraTableStyles "Heading"  pn a
             paraStyles  = paraTableStyles "Contents" (pn + length (newPara paraHStyles)) a
             newPara     = map snd . filter (not . isEmpty . snd)
        addTableStyle $ tableStyle tn columnIds
        mapM_ addParaStyle . newPara $ paraHStyles ++ paraStyles
        captionDoc <- if null c
                      then return empty
                      else withParagraphStyle o "Caption" [Para c]
        th <- colHeadsToOpenDocument       o name (map fst paraHStyles) h
        tr <- mapM (tableRowToOpenDocument o name (map fst paraStyles)) r
        return $ inTags True "table:table" [ ("table:name"      , name)
                                           , ("table:style-name", name)
                                           ] (vcat columns $$ th $$ vcat tr) $$ captionDoc

colHeadsToOpenDocument :: WriterOptions -> String -> [String] -> [[Block]] -> State WriterState Doc
colHeadsToOpenDocument o tn ns hs =
    inTagsIndented "table:table-header-rows" . inTagsIndented "table:table-row" . vcat <$>
    mapM (tableItemToOpenDocument o tn) (zip ns hs)

tableRowToOpenDocument :: WriterOptions -> String -> [String] -> [[Block]] -> State WriterState Doc
tableRowToOpenDocument o tn ns cs =
    inTagsIndented "table:table-row" . vcat <$>
    mapM (tableItemToOpenDocument o tn) (zip ns cs)

tableItemToOpenDocument :: WriterOptions -> String -> (String,[Block])-> State WriterState Doc
tableItemToOpenDocument o tn (n,i) =
  let a = [ ("table:style-name" , tn ++ ".A1" )
          , ("office:value-type", "string"     )
          ]
  in  inTags True "table:table-cell" a <$>
      withParagraphStyle o n (map plainToPara i)

-- | Take list of inline elements and return wrapped doc.
wrap :: WriterOptions -> [Inline] -> State WriterState Doc
wrap o l = if writerWrapText o
                then fsep <$> mapM (inlinesToOpenDocument o) (splitBy Space l)
                else inlinesToOpenDocument o l

-- | Convert a list of inline elements to OpenDocument.
inlinesToOpenDocument :: WriterOptions -> [Inline] -> State WriterState Doc
inlinesToOpenDocument o l = hcat <$> mapM (inlineToOpenDocument o) l

-- | Convert an inline element to OpenDocument.
inlineToOpenDocument :: WriterOptions -> Inline -> State WriterState Doc
inlineToOpenDocument o ils
    | Ellipses      <- ils = return $ text "&#8230;"
    | EmDash        <- ils = return $ text "&#8212;"
    | EnDash        <- ils = return $ text "&#8211;"
    | Apostrophe    <- ils = return $ text "&#8217;"
    | Space         <- ils = return $ char ' '
    | LineBreak     <- ils = return $ selfClosingTag "text:line-break" []
    | Str         s <- ils = return $ text $ escapeStringForXML s
    | Emph        l <- ils = inSpanTags "Emphasis"           <$> inlinesToOpenDocument o l
    | Strong      l <- ils = inSpanTags "Strong_20_Emphasis" <$> inlinesToOpenDocument o l
    | Strikeout   l <- ils = inSpanTags "Strikeout"          <$> inlinesToOpenDocument o l
    | Superscript l <- ils = inSpanTags "Superscript"        <$> inlinesToOpenDocument o l
    | Subscript   l <- ils = inSpanTags "Subscript"          <$> inlinesToOpenDocument o l
    | Quoted    t l <- ils = inQuotes t <$> inlinesToOpenDocument o l
    | Code        s <- ils = preformatted s
    | Math        s <- ils = inlinesToOpenDocument o (readTeXMath s)
    | TeX         s <- ils = preformatted s
    | HtmlInline  s <- ils = preformatted s
    | Link  l (s,t) <- ils = mkLink s t <$> inlinesToOpenDocument o l
    | Image l (s,t) <- ils = mkLink s t <$> inlinesToOpenDocument o l
    | Note        l <- ils = mkNote l
    | otherwise            = return empty
    where
      preformatted = return . inSpanTags "Teletype" . text . escapeStringForXML
      mkLink   s t = inTags False "text:a" [ ("xlink:type" , "simple")
                                           , ("xlink:href" , s       )
                                           , ("office:name", t       )
                                           ] . inSpanTags "Definition"
      mkNote     l = do
        n <- length <$> gets stNotes
        let footNote t = inTags False "text:note"
                         [ ("text:id"        , "ftn" ++ show n)
                         , ("text:note-class", "footnote"     )] $
                         inTagsSimple "text:note-citation" (text . show $ n + 1) $$
                         inTagsSimple "text:note-body" t
        nn <- footNote <$> withParagraphStyle o "Footnote" l
        addNote nn
        return nn

generateStyles :: [Doc] -> Doc
generateStyles acc =
    let scripts = selfClosingTag "office:scripts" []
        fonts   = inTagsIndented "office:font-face-decls"
                  (vcat $ map font ["Lucida Sans Unicode", "Tahoma", "Times New Roman"])
        font fn = selfClosingTag "style:font-face"
                  [ ("style:name"     , "&apos;" ++ fn ++ "&apos;")
                  , ("svg:font-family", fn                        )]
    in  scripts $$ fonts $$ inTagsIndented "office:automatic-styles" (vcat $ reverse acc)

bulletListStyle :: Int -> State WriterState (Int,(Int,[Doc]))
bulletListStyle l =
    let doStyles  i = inTags True "text:list-level-style-bullet"
                      [ ("text:level"      , show (i + 1)       )
                      , ("text:style-name" , "Bullet_20_Symbols")
                      , ("style:num-suffix", "."                )
                      , ("text:bullet-char", [bulletList !! i]  )
                      ] (listLevelStyle i)
        bulletList  = map chr $ cycle [8226,8227,8259]
        listElStyle = map doStyles [0..9]
    in  do pn <- paraListStyle l
           return (pn, (l, listElStyle))

orderedListLevelStyle :: ListAttributes -> (Int, [Doc]) -> (Int,[Doc])
orderedListLevelStyle (s,n, d) (l,ls) =
    let suffix    = case d of
                      OneParen  -> [("style:num-suffix", ")")]
                      TwoParens -> [("style:num-prefix", "(")
                                   ,("style:num-suffix", ")")]
                      _         -> [("style:num-suffix", ".")]
        format    = case n of
                      UpperAlpha   -> "A"
                      LowerAlpha   -> "a"
                      UpperRoman   -> "I"
                      LowerRoman   -> "i"
                      _            -> "1"
        listStyle = inTags True "text:list-level-style-number"
                    ([ ("text:level"      , show $ 1 + length ls  )
                     , ("text:style-name" , "Numbering_20_Symbols")
                     , ("style:num-format", format                )
                     , ("text:start-value", show s                )
                     ] ++ suffix) (listLevelStyle (1 + length ls))
    in  (l, ls ++ [listStyle])

listLevelStyle :: Int -> Doc
listLevelStyle i =
    let indent = show (0.25 * fromIntegral i :: Double) in
    selfClosingTag "style:list-level-properties"
                       [ ("text:space-before"   , indent ++ "in")
                       , ("text:min-label-width",       "0.25in")]

tableStyle :: Int -> [(Char,Float)] -> Doc
tableStyle num wcs =
    let tableId        = "Table" ++ show (num + 1)
        table          = inTags True "style:style"
                         [("style:name", tableId)] $
                         selfClosingTag "style:table-properties"
                         [ ("style:rel-width", "100%"  )
                         , ("table:align"    , "center")]
        colStyle (c,w) = inTags True "style:style"
                         [ ("style:name"  , tableId ++ "." ++ [c])
                         , ("style:family", "table-column"       )] $
                         selfClosingTag "style:table-column-properties"
                         [("style:column-width", show (7 * w) ++ "in")]
        cellStyle      = inTags True "style:style"
                         [ ("style:name"  , tableId ++ ".A1")
                         , ("style:family", "table-cell"    )] $
                         selfClosingTag "style:table-cell-properties"
                         [ ("fo:border", "none")]
        columnStyles   = map colStyle wcs
    in  table $$ vcat columnStyles $$ cellStyle

paraStyle :: String -> [(String,String)] -> State WriterState Int
paraStyle parent attrs = do
  pn <- (+)   1 . length       <$> gets stParaStyles
  i  <- (*) 0.5 . fromIntegral <$> gets indentPara :: State WriterState Double
  b  <- gets inDefinition
  let styleAttr = [ ("style:name"             , "P" ++ show pn)
                  , ("style:family"           , "paragraph"   )
                  , ("style:parent-style-name", parent        )]
      indentVal = flip (++) "in" . show $ if b then (max 0.5 i) else i
      indent    = if i == 0 && not b
                  then empty
                  else selfClosingTag "style:paragraph-properties"
                           [ ("fo:margin-left"         , indentVal)
                           , ("fo:margin-right"        , "0in"    )
                           , ("fo:text-indent"         , "0in"    )
                           , ("style:auto-text-indent" , "false"  )]
  addParaStyle $ inTags True "style:style" (styleAttr ++ attrs) indent
  return pn

paraListStyle :: Int -> State WriterState Int
paraListStyle l = paraStyle "Text_20_body" [("style:list-style-name", "L" ++ show l )]

paraTableStyles :: String -> Int -> [Alignment] -> [(String, Doc)]
paraTableStyles _ _ [] = []
paraTableStyles t s (a:xs)
    | AlignRight  <- a = (         pName s, res s "end"   ) : paraTableStyles t (s + 1) xs
    | AlignCenter <- a = (         pName s, res s "center") : paraTableStyles t (s + 1) xs
    | otherwise        = ("Table_20_" ++ t, empty         ) : paraTableStyles t  s      xs
    where pName sn = "P" ++ show (sn + 1)
          res sn x = inTags True "style:style"
                     [ ("style:name"             , pName sn        )
                     , ("style:family"           , "paragraph"     )
                     , ("style:parent-style-name", "Table_20_" ++ t)] $
                     selfClosingTag "style:paragraph-properties"
                     [ ("fo:text-align", x)
                     , ("style:justify-single-word", "false")]

openDocumentNameSpaces :: [(String, String)]
openDocumentNameSpaces =
    [ ("xmlns:office"  , "urn:oasis:names:tc:opendocument:xmlns:office:1.0"           )
    , ("xmlns:style"   , "urn:oasis:names:tc:opendocument:xmlns:style:1.0"            )
    , ("xmlns:text"    , "urn:oasis:names:tc:opendocument:xmlns:text:1.0"             )
    , ("xmlns:table"   , "urn:oasis:names:tc:opendocument:xmlns:table:1.0"            )
    , ("xmlns:draw"    , "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0"          )
    , ("xmlns:fo"      , "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0")
    , ("xmlns:xlink"   , "http://www.w3.org/1999/xlink"                               )
    , ("xmlns:dc"      , "http://purl.org/dc/elements/1.1/"                           )
    , ("xmlns:meta"    , "urn:oasis:names:tc:opendocument:xmlns:meta:1.0"             )
    , ("xmlns:number"  , "urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0"        )
    , ("xmlns:svg"     , "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0"   )
    , ("xmlns:chart"   , "urn:oasis:names:tc:opendocument:xmlns:chart:1.0"            )
    , ("xmlns:dr3d"    , "urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0"             )
    , ("xmlns:math"    , "http://www.w3.org/1998/Math/MathML"                         )
    , ("xmlns:form"    , "urn:oasis:names:tc:opendocument:xmlns:form:1.0"             )
    , ("xmlns:script"  , "urn:oasis:names:tc:opendocument:xmlns:script:1.0"           )
    , ("xmlns:ooo"     , "http://openoffice.org/2004/office"                          )
    , ("xmlns:ooow"    , "http://openoffice.org/2004/writer"                          )
    , ("xmlns:oooc"    , "http://openoffice.org/2004/calc"                            )
    , ("xmlns:dom"     , "http://www.w3.org/2001/xml-events"                          )
    , ("xmlns:xforms"  , "http://www.w3.org/2002/xforms"                              )
    , ("xmlns:xsd"     , "http://www.w3.org/2001/XMLSchema"                           )
    , ("xmlns:xsi"     , "http://www.w3.org/2001/XMLSchema-instance"                  )
    , ("office:version", "1.0"                                                        )
    ]
