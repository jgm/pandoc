{-
Copyright (C) 2006-2014 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.RTF
   Copyright   : Copyright (C) 2006-2014 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to RTF (rich text format).
-}
module Text.Pandoc.Writers.RTF ( writeRTF, writeRTFWithEmbeddedImages ) where
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Readers.TeXMath
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Walk
import Data.List ( isSuffixOf, intercalate )
import Data.Char ( ord, chr, isDigit )
import qualified Data.ByteString as B
import qualified Data.Map as M
import Text.Printf ( printf )
import Text.Pandoc.ImageSize

-- | Convert Image inlines into a raw RTF embedded image, read from a file,
-- or a MediaBag, or the internet.
-- If file not found or filetype not jpeg or png, leave the inline unchanged.
rtfEmbedImage :: WriterOptions -> Inline -> IO Inline
rtfEmbedImage opts x@(Image _ (src,_)) = do
  result <- fetchItem' (writerMediaBag opts) (writerSourceURL opts) src
  case result of
       Right (imgdata, Just mime)
         | mime == "image/jpeg" || mime == "image/png" -> do
         let bytes = map (printf "%02x") $ B.unpack imgdata
         let filetype = case mime of
                             "image/jpeg" -> "\\jpegblip"
                             "image/png"  -> "\\pngblip"
                             _            -> error "Unknown file type"
         let sizeSpec = case imageSize imgdata of
                             Nothing -> ""
                             Just sz -> "\\picw" ++ show xpx ++
                                        "\\pich" ++ show ypx ++
                                        "\\picwgoal" ++ show (xpt * 20)
                                        ++ "\\pichgoal" ++ show (ypt * 20)
                                -- twip = 1/1440in = 1/20pt
                                where (xpx, ypx) = sizeInPixels sz
                                      (xpt, ypt) = sizeInPoints sz
         let raw = "{\\pict" ++ filetype ++ sizeSpec ++ " " ++
                    concat bytes ++ "}"
         return $ if B.null imgdata
                     then x
                     else RawInline (Format "rtf") raw
       _ -> return x
rtfEmbedImage _ x = return x

-- | Convert Pandoc to a string in rich text format, with
-- images embedded as encoded binary data.
writeRTFWithEmbeddedImages :: WriterOptions -> Pandoc -> IO String
writeRTFWithEmbeddedImages options doc =
  writeRTF options `fmap` walkM (rtfEmbedImage options) doc

-- | Convert Pandoc to a string in rich text format.
writeRTF :: WriterOptions -> Pandoc -> String
writeRTF options (Pandoc meta@(Meta metamap) blocks) =
  let spacer = not $ all null $ docTitle meta : docDate meta : docAuthors meta
      toPlain (MetaBlocks [Para ils]) = MetaInlines ils
      toPlain x = x
      -- adjust title, author, date so we don't get para inside para
      meta'  = Meta $ M.adjust toPlain "title"
                    . M.adjust toPlain "author"
                    . M.adjust toPlain "date"
                    $ metamap
      Just metadata = metaToJSON options
              (Just . concatMap (blockToRTF 0 AlignDefault))
              (Just . inlineListToRTF)
              meta'
      body = concatMap (blockToRTF 0 AlignDefault) blocks
      isTOCHeader (Header lev _ _) = lev <= writerTOCDepth options
      isTOCHeader _ = False
      context = defField "body" body
              $ defField "spacer" spacer
              $ (if writerTableOfContents options
                    then defField "toc"
                          (tableOfContents $ filter isTOCHeader blocks)
                    else id)
              $ metadata
  in  if writerStandalone options
         then renderTemplate' (writerTemplate options) context
         else body

-- | Construct table of contents from list of header blocks.
tableOfContents :: [Block] -> String
tableOfContents headers =
  let contentsTree = hierarchicalize headers
  in  concatMap (blockToRTF 0 AlignDefault) $
      [Header 1 nullAttr [Str "Contents"],
       BulletList (map elementToListItem contentsTree)]

elementToListItem :: Element -> [Block]
elementToListItem (Blk _) = []
elementToListItem (Sec _ _ _ sectext subsecs) = [Plain sectext] ++
  if null subsecs
     then []
     else [BulletList (map elementToListItem subsecs)]

-- | Convert unicode characters (> 127) into rich text format representation.
handleUnicode :: String -> String
handleUnicode [] = []
handleUnicode (c:cs) =
  if ord c > 127
     then if surrogate c
          then let x = ord c - 0x10000
                   (q, r) = x `divMod` 0x400
                   upper = q + 0xd800
                   lower = r + 0xDC00
               in enc (chr upper) ++ enc (chr lower) ++ handleUnicode cs
          else enc c ++ handleUnicode cs
     else c:(handleUnicode cs)
  where
    surrogate x = not (   (0x0000 <= ord x && ord x <= 0xd7ff)
                       || (0xe000 <= ord x && ord x <= 0xffff) )
    enc x = '\\':'u':(show (ord x)) ++ "?"

-- | Escape special characters.
escapeSpecial :: String -> String
escapeSpecial = escapeStringUsing $
  [ ('\t',"\\tab ")
  , ('\8216',"\\u8216'")
  , ('\8217',"\\u8217'")
  , ('\8220',"\\u8220\"")
  , ('\8221',"\\u8221\"")
  , ('\8211',"\\u8211-")
  , ('\8212',"\\u8212-")
  ] ++ backslashEscapes "{\\}"

-- | Escape strings as needed for rich text format.
stringToRTF :: String -> String
stringToRTF = handleUnicode . escapeSpecial

-- | Escape things as needed for code block in RTF.
codeStringToRTF :: String -> String
codeStringToRTF str = intercalate "\\line\n" $ lines (stringToRTF str)

-- | Make a paragraph with first-line indent, block indent, and space after.
rtfParSpaced :: Int       -- ^ space after (in twips)
             -> Int       -- ^ block indent (in twips)
             -> Int       -- ^ first line indent (relative to block) (in twips)
             -> Alignment -- ^ alignment
             -> String    -- ^ string with content
             -> String
rtfParSpaced spaceAfter indent firstLineIndent alignment content =
  let alignString = case alignment of
                           AlignLeft -> "\\ql "
                           AlignRight -> "\\qr "
                           AlignCenter -> "\\qc "
                           AlignDefault -> "\\ql "
  in  "{\\pard " ++ alignString ++
      "\\f0 \\sa" ++ (show spaceAfter) ++ " \\li" ++ (show indent) ++
      " \\fi" ++ (show firstLineIndent) ++ " " ++ content ++ "\\par}\n"

-- | Default paragraph.
rtfPar :: Int       -- ^ block indent (in twips)
       -> Int       -- ^ first line indent (relative to block) (in twips)
       -> Alignment -- ^ alignment
       -> String    -- ^ string with content
       -> String
rtfPar = rtfParSpaced 180

-- | Compact paragraph (e.g. for compact list items).
rtfCompact ::  Int       -- ^ block indent (in twips)
           ->  Int       -- ^ first line indent (relative to block) (in twips)
           ->  Alignment -- ^ alignment
           ->  String    -- ^ string with content
           ->  String
rtfCompact = rtfParSpaced 0

-- number of twips to indent
indentIncrement :: Int
indentIncrement = 720

listIncrement :: Int
listIncrement = 360

-- | Returns appropriate bullet list marker for indent level.
bulletMarker :: Int -> String
bulletMarker indent = case indent `mod` 720 of
                             0 -> "\\bullet "
                             _ -> "\\endash "

-- | Returns appropriate (list of) ordered list markers for indent level.
orderedMarkers :: Int -> ListAttributes -> [String]
orderedMarkers indent (start, style, delim) =
  if style == DefaultStyle && delim == DefaultDelim
     then case indent `mod` 720 of
              0 -> orderedListMarkers (start, Decimal, Period)
              _ -> orderedListMarkers (start, LowerAlpha, Period)
     else orderedListMarkers (start, style, delim)

-- | Convert Pandoc block element to RTF.
blockToRTF :: Int       -- ^ indent level
           -> Alignment -- ^ alignment
           -> Block     -- ^ block to convert
           -> String
blockToRTF _ _ Null = ""
blockToRTF indent alignment (Div _ bs) =
  concatMap (blockToRTF indent alignment) bs
blockToRTF indent alignment (Plain lst) =
  rtfCompact indent 0 alignment $ inlineListToRTF lst
blockToRTF indent alignment (Para lst) =
  rtfPar indent 0 alignment $ inlineListToRTF lst
blockToRTF indent alignment (BlockQuote lst) =
  concatMap (blockToRTF (indent + indentIncrement) alignment) lst
blockToRTF indent _ (CodeBlock _ str) =
  rtfPar indent 0 AlignLeft ("\\f1 " ++ (codeStringToRTF str))
blockToRTF _ _ (RawBlock f str)
  | f == Format "rtf" = str
  | otherwise         = ""
blockToRTF indent alignment (BulletList lst) = spaceAtEnd $
  concatMap (listItemToRTF alignment indent (bulletMarker indent)) lst
blockToRTF indent alignment (OrderedList attribs lst) = spaceAtEnd $ concat $
  zipWith (listItemToRTF alignment indent) (orderedMarkers indent attribs) lst
blockToRTF indent alignment (DefinitionList lst) = spaceAtEnd $
  concatMap (definitionListItemToRTF alignment indent) lst
blockToRTF indent _ HorizontalRule =
  rtfPar indent 0 AlignCenter "\\emdash\\emdash\\emdash\\emdash\\emdash"
blockToRTF indent alignment (Header level _ lst) = rtfPar indent 0 alignment $
  "\\b \\fs" ++ (show (40 - (level * 4))) ++ " " ++ inlineListToRTF lst
blockToRTF indent alignment (Table caption aligns sizes headers rows) =
  (if all null headers
      then ""
      else tableRowToRTF True indent aligns sizes headers) ++
  concatMap (tableRowToRTF False indent aligns sizes) rows ++
  rtfPar indent 0 alignment (inlineListToRTF caption)

tableRowToRTF :: Bool -> Int -> [Alignment] -> [Double] -> [[Block]] -> String
tableRowToRTF header indent aligns sizes' cols =
  let totalTwips = 6 * 1440 -- 6 inches
      sizes = if all (== 0) sizes'
                 then take (length cols) $ repeat (1.0 / fromIntegral (length cols))
                 else sizes'
      columns = concat $ zipWith (tableItemToRTF indent) aligns cols
      rightEdges = tail $ scanl (\sofar new -> sofar + floor (new * totalTwips))
                                (0 :: Integer) sizes
      cellDefs = map (\edge -> (if header
                                   then "\\clbrdrb\\brdrs"
                                   else "") ++ "\\cellx" ++ show edge)
                     rightEdges
      start = "{\n\\trowd \\trgaph120\n" ++ concat cellDefs ++ "\n" ++
              "\\trkeep\\intbl\n{\n"
      end = "}\n\\intbl\\row}\n"
  in  start ++ columns ++ end

tableItemToRTF :: Int -> Alignment -> [Block] -> String
tableItemToRTF indent alignment item =
  let contents = concatMap (blockToRTF indent alignment) item
  in  "{" ++ substitute "\\pard" "\\pard\\intbl" contents ++ "\\cell}\n"

-- | Ensure that there's the same amount of space after compact
-- lists as after regular lists.
spaceAtEnd :: String -> String
spaceAtEnd str =
  if isSuffixOf "\\par}\n" str
     then (take ((length str) - 6) str) ++ "\\sa180\\par}\n"
     else str

-- | Convert list item (list of blocks) to RTF.
listItemToRTF :: Alignment  -- ^ alignment
              -> Int        -- ^ indent level
              -> String     -- ^ list start marker
              -> [Block]    -- ^ list item (list of blocks)
              -> [Char]
listItemToRTF alignment indent marker [] =
  rtfCompact (indent + listIncrement) (0 - listIncrement) alignment
             (marker ++ "\\tx" ++ (show listIncrement) ++ "\\tab ")
listItemToRTF alignment indent marker list =
  let (first:rest) = map (blockToRTF (indent + listIncrement) alignment) list
      listMarker = "\\fi" ++ show (0 - listIncrement) ++ " " ++ marker ++ "\\tx" ++
                      show listIncrement ++ "\\tab"
      insertListMarker ('\\':'f':'i':'-':d:xs) | isDigit d =
        listMarker ++ dropWhile isDigit xs
      insertListMarker ('\\':'f':'i':d:xs) | isDigit d =
        listMarker ++ dropWhile isDigit xs
      insertListMarker (x:xs) =
        x : insertListMarker xs
      insertListMarker [] = []
      -- insert the list marker into the (processed) first block
  in  insertListMarker first ++ concat rest

-- | Convert definition list item (label, list of blocks) to RTF.
definitionListItemToRTF :: Alignment          -- ^ alignment
                        -> Int                -- ^ indent level
                        -> ([Inline],[[Block]]) -- ^ list item (list of blocks)
                        -> [Char]
definitionListItemToRTF alignment indent (label, defs) =
  let labelText = blockToRTF indent alignment (Plain label)
      itemsText = concatMap (blockToRTF (indent + listIncrement) alignment) $
                    concat defs
  in  labelText ++ itemsText

-- | Convert list of inline items to RTF.
inlineListToRTF :: [Inline]   -- ^ list of inlines to convert
                -> String
inlineListToRTF lst = concatMap inlineToRTF lst

-- | Convert inline item to RTF.
inlineToRTF :: Inline         -- ^ inline to convert
            -> String
inlineToRTF (Span _ lst) = inlineListToRTF lst
inlineToRTF (Emph lst) = "{\\i " ++ (inlineListToRTF lst) ++ "}"
inlineToRTF (Strong lst) = "{\\b " ++ (inlineListToRTF lst) ++ "}"
inlineToRTF (Strikeout lst) = "{\\strike " ++ (inlineListToRTF lst) ++ "}"
inlineToRTF (Superscript lst) = "{\\super " ++ (inlineListToRTF lst) ++ "}"
inlineToRTF (Subscript lst) = "{\\sub " ++ (inlineListToRTF lst) ++ "}"
inlineToRTF (SmallCaps lst) = "{\\scaps " ++ (inlineListToRTF lst) ++ "}"
inlineToRTF (Quoted SingleQuote lst) =
  "\\u8216'" ++ (inlineListToRTF lst) ++ "\\u8217'"
inlineToRTF (Quoted DoubleQuote lst) =
  "\\u8220\"" ++ (inlineListToRTF lst) ++ "\\u8221\""
inlineToRTF (Code _ str) = "{\\f1 " ++ (codeStringToRTF str) ++ "}"
inlineToRTF (Str str) = stringToRTF str
inlineToRTF (Math t str) = inlineListToRTF $ texMathToInlines t str
inlineToRTF (Cite _ lst) = inlineListToRTF lst
inlineToRTF (RawInline f str)
  | f == Format "rtf" = str
  | otherwise         = ""
inlineToRTF (LineBreak) = "\\line "
inlineToRTF Space = " "
inlineToRTF (Link text (src, _)) =
  "{\\field{\\*\\fldinst{HYPERLINK \"" ++ (codeStringToRTF src) ++
  "\"}}{\\fldrslt{\\ul\n" ++ (inlineListToRTF text) ++ "\n}}}\n"
inlineToRTF (Image _ (source, _)) =
  "{\\cf1 [image: " ++ source ++ "]\\cf0}"
inlineToRTF (Note contents) =
  "{\\super\\chftn}{\\*\\footnote\\chftn\\~\\plain\\pard " ++
  (concatMap (blockToRTF 0 AlignDefault) contents) ++ "}"
