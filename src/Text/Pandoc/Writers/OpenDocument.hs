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
import Text.Printf ( printf )
import Control.Applicative ( (<$>) )
import Control.Arrow ( (***), (>>>) )
import Control.Monad.State hiding ( when )
import Data.Char (chr)
import Data.List (intercalate)

-- | Auxiliary function to convert Plain block to Para.
plainToPara :: Block -> Block
plainToPara (Plain x) = Para x
plainToPara x = x

--
-- OpenDocument writer
--

data WriterState =
    WriterState { stNotes         :: [Doc]
                , stTableStyles   :: [Doc]
                , stParaStyles    :: [Doc]
                , stListStyles    :: [(Int, [Doc])]
                , stTextStyles    :: [Doc]
                , stTextStyleAttr :: [(TextStyle,[(String,String)])]
                , stIndentPara    :: Int
                , stInDefinition  :: Bool
                , stTight         :: Bool
                }

defaultWriterState :: WriterState
defaultWriterState =
    WriterState { stNotes         = []
                , stTableStyles   = []
                , stParaStyles    = []
                , stListStyles    = []
                , stTextStyles    = []
                , stTextStyleAttr = []
                , stIndentPara    = 0
                , stInDefinition  = False
                , stTight         = False
                }

when :: Bool -> Doc -> Doc
when p a = if p then a else empty

addTableStyle :: Doc -> State WriterState ()
addTableStyle i = modify $ \s -> s { stTableStyles = i : stTableStyles s }

addNote :: Doc -> State WriterState ()
addNote i = modify $ \s -> s { stNotes = i : stNotes s }

addParaStyle :: Doc -> State WriterState ()
addParaStyle i = modify $ \s -> s { stParaStyles = i : stParaStyles s }

addTextStyle :: Doc -> State WriterState ()
addTextStyle i = modify $ \s -> s { stTextStyles = i : stTextStyles s }

addTextStyleAttr :: (TextStyle, [(String,String)]) -> State WriterState ()
addTextStyleAttr i = modify $ \s -> s { stTextStyleAttr = i : stTextStyleAttr s }

rmTextStyleAttr :: State WriterState ()
rmTextStyleAttr = modify $ \s -> s { stTextStyleAttr = rmHead (stTextStyleAttr s) }
    where rmHead l = if l /= [] then tail l else []

increaseIndent :: State WriterState ()
increaseIndent = modify $ \s -> s { stIndentPara = 1 + stIndentPara s }

resetIndent :: State WriterState ()
resetIndent = modify $ \s -> s { stIndentPara = (stIndentPara s) - 1 }

inTightList :: State WriterState a -> State WriterState a
inTightList  f = modify (\s -> s { stTight = True  }) >> f >>= \r ->
                 modify (\s -> s { stTight = False }) >> return r

setInDefinitionList :: Bool -> State WriterState ()
setInDefinitionList b = modify $  \s -> s { stInDefinition = b }

inParagraphTags :: Doc -> Doc
inParagraphTags = inTags False "text:p" [("text:style-name", "Text_20_body")]

inParagraphTagsWithStyle :: String -> Doc -> Doc
inParagraphTagsWithStyle sty = inTags False "text:p" [("text:style-name", sty)]

inSpanTags :: String -> Doc -> Doc
inSpanTags s = inTags False "text:span" [("text:style-name",s)]

withTextStyle :: TextStyle -> State WriterState a -> State WriterState a
withTextStyle s f = addTextStyleAttr (s,textStyleAttr s) >>
                    f >>= \r -> rmTextStyleAttr >> return r

inTextStyle :: Doc -> State WriterState Doc
inTextStyle d = do
  at <- gets stTextStyleAttr
  if at == []
     then return d
     else do
       tn <- (+) 1 . length  <$> gets stTextStyles
       addTextStyle $ inTags False "style:style" [("style:name"  , "T" ++ show tn)
                                                 ,("style:family", "text"        )]
                    $ selfClosingTag "style:text-properties" (concatMap snd at)
       return $ inTags False "text:span" [("text:style-name","T" ++ show tn)] d

inHeaderTags :: Int -> Doc -> Doc
inHeaderTags i = inTags False "text:h" [ ("text:style-name", "Heading_20_" ++ show i)
                                       , ("text:outline-level", show i)]

inQuotes :: QuoteType -> Doc -> Doc
inQuotes SingleQuote s = text "&#8216;" <> s <> text "&#8217;"
inQuotes DoubleQuote s = text "&#8220;" <> s <> text "&#8221;"

handleSpaces :: String -> Doc
handleSpaces s
    | ( ' ':_) <- s = genTag s
    | ('\t':x) <- s = selfClosingTag "text:tab" [] <> rm x
    | otherwise     = rm s
    where
        genTag       = span (==' ') >>> tag . length *** rm >>> uncurry (<>)
        tag n        = when (n /= 0) $ selfClosingTag "text:s" [("text:c", show n)]
        rm ( ' ':xs) = char ' ' <> genTag xs
        rm ('\t':xs) = selfClosingTag "text:tab" [] <> genTag xs
        rm (   x:xs) = char  x  <> rm xs
        rm        [] = empty

-- | Convert list of authors to a docbook <author> section
authorToOpenDocument :: [Char] -> Doc
authorToOpenDocument name =
  if ',' `elem` name
    then -- last name first
         let (lastname, rest) = break (==',') name
             firstname = removeLeadingSpace rest
         in  inParagraphTagsWithStyle "Author" $
                 (text $ escapeStringForXML firstname) <+>
                 (text $ escapeStringForXML lastname)
    else -- last name last
         let namewords = words name
             lengthname = length namewords
             (firstname, lastname) = case lengthname of
               0  -> ("","")
               1  -> ("", name)
               n  -> (intercalate " " (take (n-1) namewords), last namewords)
          in inParagraphTagsWithStyle "Author" $
                 (text $ escapeStringForXML firstname) <+>
                 (text $ escapeStringForXML lastname)

-- | Convert Pandoc document to string in OpenDocument format.
writeOpenDocument :: WriterOptions -> Pandoc -> String
writeOpenDocument opts (Pandoc (Meta title authors date) blocks) =
  let root     = inTags True "office:document-content" openDocumentNameSpaces
      header   = when (writerStandalone opts) $ text (writerHeader opts)
      title'   = case runState (wrap opts title) defaultWriterState of
                   (t,_) -> if isEmpty t then empty else inHeaderTags 1 t
      authors' = when (authors /= []) $ vcat (map authorToOpenDocument authors)
      date'    = when (date    /= []) $
                 inParagraphTagsWithStyle "Date" (text $ escapeStringForXML date)
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
      styles   = stTableStyles s ++ stParaStyles s ++ stTextStyles s
      listStyle (n,l) = inTags True "text:list-style" [("style:name", "L" ++ show n)] (vcat l)
      listStyles  = map listStyle (stListStyles s)
  in  render $ header $$ root (generateStyles (styles ++ listStyles) $$ body' $$ text "")

withParagraphStyle :: WriterOptions -> String -> [Block] -> State WriterState Doc
withParagraphStyle  o s (b:bs)
    | Para l <- b = go =<< inParagraphTagsWithStyle s <$> inlinesToOpenDocument o l
    | otherwise   = go =<< blockToOpenDocument o b
    where go i = ($$) i <$>  withParagraphStyle o s bs
withParagraphStyle _ _ [] = return empty

inPreformattedTags :: String -> State WriterState Doc
inPreformattedTags s = do
  n <- paraStyle "Preformatted_20_Text" []
  return . inParagraphTagsWithStyle ("P" ++ show n) . handleSpaces $ s

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

isTightList :: [[Block]] -> Bool
isTightList []          = False
isTightList (b:_)
    | Plain {} : _ <- b = True
    | otherwise         = False

newOrderedListStyle :: Bool -> ListAttributes -> State WriterState (Int,Int)
newOrderedListStyle b a = do
  ln <- (+) 1 . length  <$> gets stListStyles
  let nbs = orderedListLevelStyle a (ln, [])
  pn <- if b then inTightList (paraListStyle ln) else paraListStyle ln
  modify $ \s -> s { stListStyles = nbs : stListStyles s }
  return (ln,pn)

bulletListToOpenDocument :: WriterOptions -> [[Block]] -> State WriterState Doc
bulletListToOpenDocument o b = do
  ln <- (+) 1 . length <$> gets stListStyles
  (pn,ns) <- if isTightList b then inTightList (bulletListStyle ln) else bulletListStyle ln
  modify $ \s -> s { stListStyles = ns : stListStyles s }
  is <- listItemsToOpenDocument ("P" ++ show pn) o b
  return $ inTags True "text:list" [("text:style-name", "L" ++ show ln)] is

listItemsToOpenDocument :: String -> WriterOptions -> [[Block]] -> State WriterState Doc
listItemsToOpenDocument s o is =
    vcat . map (inTagsIndented "text:list-item") <$> mapM (withParagraphStyle o s . map plainToPara) is

deflistItemToOpenDocument :: WriterOptions -> ([Inline],[Block]) -> State WriterState Doc
deflistItemToOpenDocument o (t,d) = do
  let ts = if isTightList [d]
           then "Definition_20_Term_20_Tight"       else "Definition_20_Term"
      ds = if isTightList [d]
           then "Definition_20_Definition_20_Tight" else "Definition_20_Definition"
  t' <- withParagraphStyle o ts [Para t]
  d' <- withParagraphStyle o ds (map plainToPara d)
  return $ t' $$ d'

inBlockQuote :: WriterOptions -> Int -> [Block] -> State WriterState Doc
inBlockQuote  o i (b:bs)
    | BlockQuote l <- b = do increaseIndent
                             ni <- paraStyle "Quotations" []
                             go =<< inBlockQuote o ni (map plainToPara l)
    | Para       l <- b = do go =<< inParagraphTagsWithStyle ("P" ++ show  i) <$> inlinesToOpenDocument o l
    | otherwise         = do go =<< blockToOpenDocument o b
    where go  block  = ($$) block <$> inBlockQuote o i bs
inBlockQuote     _ _ [] =  resetIndent >> return empty

-- | Convert a list of Pandoc blocks to OpenDocument.
blocksToOpenDocument :: WriterOptions -> [Block] -> State WriterState Doc
blocksToOpenDocument o b = vcat <$> mapM (blockToOpenDocument o) b

-- | Convert a Pandoc block element to OpenDocument.
blockToOpenDocument :: WriterOptions -> Block -> State WriterState Doc
blockToOpenDocument o bs
    | Plain          b <- bs = inParagraphTags <$> wrap o b
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
    | HorizontalRule   <- bs = return $ selfClosingTag "text:p" [ ("text:style-name", "Horizontal_20_Line") ]
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
      orderedList a b = do (ln,pn) <- newOrderedListStyle (isTightList b) a
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
    | Ellipses      <- ils = inTextStyle $ text "&#8230;"
    | EmDash        <- ils = inTextStyle $ text "&#8212;"
    | EnDash        <- ils = inTextStyle $ text "&#8211;"
    | Apostrophe    <- ils = inTextStyle $ text "&#8217;"
    | Space         <- ils = inTextStyle $ char ' '
    | LineBreak     <- ils = return $ selfClosingTag "text:line-break" []
    | Str         s <- ils = inTextStyle $ handleSpaces $ escapeStringForXML s
    | Emph        l <- ils = withTextStyle Italic $ inlinesToOpenDocument o l
    | Strong      l <- ils = withTextStyle Bold   $ inlinesToOpenDocument o l
    | Strikeout   l <- ils = withTextStyle Strike $ inlinesToOpenDocument o l
    | Superscript l <- ils = withTextStyle Sup    $ inlinesToOpenDocument o l
    | Subscript   l <- ils = withTextStyle Sub    $ inlinesToOpenDocument o l
    | SmallCaps   l <- ils = withTextStyle SmallC $ inlinesToOpenDocument o l
    | Quoted    t l <- ils = inQuotes t <$> inlinesToOpenDocument o l
    | Code        s <- ils = preformatted s
    | Math      _ s <- ils = inlinesToOpenDocument o (readTeXMath s)
    | Cite      _ l <- ils = inlinesToOpenDocument o l
    | TeX         s <- ils = preformatted s
    | HtmlInline  s <- ils = preformatted s
    | Link  l (s,t) <- ils = mkLink s t <$> inlinesToOpenDocument o l
    | Image _ (s,_) <- ils = return $ mkImg  s
    | Note        l <- ils = mkNote l
    | otherwise            = return empty
    where
      preformatted = return . inSpanTags "Teletype" . handleSpaces . escapeStringForXML
      mkLink   s t = inTags False "text:a" [ ("xlink:type" , "simple")
                                           , ("xlink:href" , s       )
                                           , ("office:name", t       )
                                           ] . inSpanTags "Definition"
      mkImg  s     = inTags False "draw:frame" [] $
                     selfClosingTag "draw:image" [ ("xlink:href"   , s       )
                                                 , ("xlink:type"   , "simple")
                                                 , (" xlink:show"  , "embed" )
                                                 , ("xlink:actuate", "onLoad")]
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
                      ] (listLevelStyle (1 + i))
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

tableStyle :: Int -> [(Char,Double)] -> Doc
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
                         [("style:column-width", printf "%.2f" (7 * w) ++ "in")]
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
  i  <- (*) 0.5 . fromIntegral <$> gets stIndentPara :: State WriterState Double
  b  <- gets stInDefinition
  t  <- gets stTight
  let styleAttr = [ ("style:name"             , "P" ++ show pn)
                  , ("style:family"           , "paragraph"   )
                  , ("style:parent-style-name", parent        )]
      indentVal = flip (++) "in" . show $ if b then (max 0.5 i) else i
      tight     = if t then [ ("fo:margin-top"          , "0in"    )
                            , ("fo:margin-bottom"       , "0in"    )]
                       else []
      indent    = when (i /= 0 || b || t) $
                  selfClosingTag "style:paragraph-properties" $
                           [ ("fo:margin-left"         , indentVal)
                           , ("fo:margin-right"        , "0in"    )
                           , ("fo:text-indent"         , "0in"    )
                           , ("style:auto-text-indent" , "false"  )]
                         ++ tight
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

data TextStyle = Italic | Bold | Strike | Sub | Sup | SmallC deriving ( Eq )

textStyleAttr :: TextStyle -> [(String,String)]
textStyleAttr s
    | Italic <- s = [("fo:font-style"                ,"italic"    )
                    ,("style:font-style-asian"       ,"italic"    )
                    ,("style:font-style-complex"     ,"italic"    )]
    | Bold   <- s = [("fo:font-weight"               ,"bold"      )
                    ,("style:font-weight-asian"      ,"bold"      )
                    ,("style:font-weight-complex"    ,"bold"      )]
    | Strike <- s = [("style:text-line-through-style", "solid"    )]
    | Sub    <- s = [("style:text-position"          ,"sub 58%"   )]
    | Sup    <- s = [("style:text-position"          ,"super 58%" )]
    | SmallC <- s = [("fo:font-variant"              ,"small-caps")]
    | otherwise   = []

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
