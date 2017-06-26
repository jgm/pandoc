{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright (C) 2006-2017 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to RTF (rich text format).
-}
module Text.Pandoc.Writers.RTF ( writeRTF
                               ) where
import Control.Monad.Except (catchError, throwError)
import qualified Data.ByteString as B
import Data.Char (chr, isDigit, ord)
import Data.List (intercalate, isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Text.Pandoc.Class (PandocMonad, report)
import qualified Text.Pandoc.Class as P
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import Text.Printf (printf)

-- | Convert Image inlines into a raw RTF embedded image, read from a file,
-- or a MediaBag, or the internet.
-- If file not found or filetype not jpeg or png, leave the inline unchanged.
rtfEmbedImage :: PandocMonad m => WriterOptions -> Inline -> m Inline
rtfEmbedImage opts x@(Image attr _ (src,_)) = catchError
  (do result <- P.fetchItem (writerSourceURL opts) src
      case result of
           (imgdata, Just mime)
             | mime == "image/jpeg" || mime == "image/png" -> do
             let bytes = map (printf "%02x") $ B.unpack imgdata
             filetype <-
                case mime of
                     "image/jpeg" -> return "\\jpegblip"
                     "image/png"  -> return "\\pngblip"
                     _            -> throwError $
                                         PandocShouldNeverHappenError $
                                         "Unknown file type " ++ mime
             sizeSpec <-
                case imageSize opts imgdata of
                     Left msg -> do
                       report $ CouldNotDetermineImageSize src msg
                       return ""
                     Right sz -> return $ "\\picw" ++ show xpx ++
                                "\\pich" ++ show ypx ++
                                "\\picwgoal" ++ show (floor (xpt * 20) :: Integer)
                                ++ "\\pichgoal" ++ show (floor (ypt * 20) :: Integer)
                        -- twip = 1/1440in = 1/20pt
                        where (xpx, ypx) = sizeInPixels sz
                              (xpt, ypt) = desiredSizeInPoints opts attr sz
             let raw = "{\\pict" ++ filetype ++ sizeSpec ++ "\\bin " ++
                        concat bytes ++ "}"
             if B.null imgdata
                then do
                  report $ CouldNotFetchResource src "image contained no data"
                  return x
                else return $ RawInline (Format "rtf") raw
             | otherwise -> do
               report $ CouldNotFetchResource src "image is not a jpeg or png"
               return x
           (_, Nothing) -> do
             report $ CouldNotDetermineMimeType src
             return x)
  (\e -> do
     report $ CouldNotFetchResource src (show e)
     return x)
rtfEmbedImage _ x = return x

-- | Convert Pandoc to a string in rich text format.
writeRTF :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeRTF options doc = do
  -- handle images
  Pandoc meta@(Meta metamap) blocks <- walkM (rtfEmbedImage options) doc
  let spacer = not $ all null $ docTitle meta : docDate meta : docAuthors meta
  let toPlain (MetaBlocks [Para ils]) = MetaInlines ils
      toPlain x                       = x
  -- adjust title, author, date so we don't get para inside para
  let meta'  = Meta $ M.adjust toPlain "title"
                    . M.adjust toPlain "author"
                    . M.adjust toPlain "date"
                    $ metamap
  metadata <- metaToJSON options
              (fmap concat . mapM (blockToRTF 0 AlignDefault))
              (inlinesToRTF)
              meta'
  body <- blocksToRTF 0 AlignDefault blocks
  let isTOCHeader (Header lev _ _) = lev <= writerTOCDepth options
      isTOCHeader _                = False
  toc <- tableOfContents $ filter isTOCHeader blocks
  let context = defField "body" body
              $ defField "spacer" spacer
              $ (if writerTableOfContents options
                    then defField "table-of-contents" toc
                         -- for backwards compatibility,
                         -- we populate toc with the contents
                         -- of the toc rather than a boolean:
                         . defField "toc" toc
                    else id)
              $ metadata
  T.pack <$>
      case writerTemplate options of
           Just tpl -> renderTemplate' tpl context
           Nothing  -> return $
                       case reverse body of
                            ('\n':_) -> body
                            _        -> body ++ "\n"

-- | Construct table of contents from list of header blocks.
tableOfContents :: PandocMonad m => [Block] -> m String
tableOfContents headers = do
  let contents = map elementToListItem $ hierarchicalize headers
  blocksToRTF 0 AlignDefault $
      [Header 1 nullAttr [Str "Contents"], BulletList contents]

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
                           AlignLeft    -> "\\ql "
                           AlignRight   -> "\\qr "
                           AlignCenter  -> "\\qc "
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

blocksToRTF :: PandocMonad m
            => Int
            -> Alignment
            -> [Block]
            -> m String
blocksToRTF indent align = fmap concat . mapM (blockToRTF indent align)

-- | Convert Pandoc block element to RTF.
blockToRTF :: PandocMonad m
           => Int       -- ^ indent level
           -> Alignment -- ^ alignment
           -> Block     -- ^ block to convert
           -> m String
blockToRTF _ _ Null = return ""
blockToRTF indent alignment (Div _ bs) =
  blocksToRTF indent alignment bs
blockToRTF indent alignment (Plain lst) =
  rtfCompact indent 0 alignment <$> inlinesToRTF lst
blockToRTF indent alignment (Para lst) =
  rtfPar indent 0 alignment <$> inlinesToRTF lst
blockToRTF indent alignment (LineBlock lns) =
  blockToRTF indent alignment $ linesToPara lns
blockToRTF indent alignment (BlockQuote lst) =
  blocksToRTF (indent + indentIncrement) alignment lst
blockToRTF indent _ (CodeBlock _ str) =
  return $ rtfPar indent 0 AlignLeft ("\\f1 " ++ (codeStringToRTF str))
blockToRTF _ _ b@(RawBlock f str)
  | f == Format "rtf" = return str
  | otherwise         = do
      report $ BlockNotRendered b
      return ""
blockToRTF indent alignment (BulletList lst) = (spaceAtEnd . concat) <$>
  mapM (listItemToRTF alignment indent (bulletMarker indent)) lst
blockToRTF indent alignment (OrderedList attribs lst) =
  (spaceAtEnd . concat) <$>
   mapM (\(x,y) -> listItemToRTF alignment indent x y)
   (zip (orderedMarkers indent attribs) lst)
blockToRTF indent alignment (DefinitionList lst) = (spaceAtEnd . concat) <$>
  mapM (definitionListItemToRTF alignment indent) lst
blockToRTF indent _ HorizontalRule = return $
  rtfPar indent 0 AlignCenter "\\emdash\\emdash\\emdash\\emdash\\emdash"
blockToRTF indent alignment (Header level _ lst) = do
  contents <- inlinesToRTF lst
  return $ rtfPar indent 0 alignment $
             "\\b \\fs" ++ (show (40 - (level * 4))) ++ " " ++ contents
blockToRTF indent alignment (Table caption aligns sizes headers rows) = do
  caption' <- inlinesToRTF caption
  header' <- if all null headers
                then return ""
                else tableRowToRTF True indent aligns sizes headers
  rows' <- concat <$> mapM (tableRowToRTF False indent aligns sizes) rows
  return $ header' ++ rows' ++ rtfPar indent 0 alignment caption'

tableRowToRTF :: PandocMonad m
              => Bool -> Int -> [Alignment] -> [Double] -> [[Block]] -> m String
tableRowToRTF header indent aligns sizes' cols = do
  let totalTwips = 6 * 1440 -- 6 inches
  let sizes = if all (== 0) sizes'
                 then take (length cols) $ repeat (1.0 / fromIntegral (length cols))
                 else sizes'
  columns <- concat <$> mapM (\(x,y) -> tableItemToRTF indent x y)
                         (zip aligns cols)
  let rightEdges = tail $ scanl (\sofar new -> sofar + floor (new * totalTwips))
                                (0 :: Integer) sizes
  let cellDefs = map (\edge -> (if header
                                   then "\\clbrdrb\\brdrs"
                                   else "") ++ "\\cellx" ++ show edge)
                     rightEdges
  let start = "{\n\\trowd \\trgaph120\n" ++ concat cellDefs ++ "\n" ++
              "\\trkeep\\intbl\n{\n"
  let end = "}\n\\intbl\\row}\n"
  return $ start ++ columns ++ end

tableItemToRTF :: PandocMonad m => Int -> Alignment -> [Block] -> m String
tableItemToRTF indent alignment item = do
  contents <- blocksToRTF indent alignment item
  return $ "{" ++ substitute "\\pard" "\\pard\\intbl" contents ++ "\\cell}\n"

-- | Ensure that there's the same amount of space after compact
-- lists as after regular lists.
spaceAtEnd :: String -> String
spaceAtEnd str =
  if isSuffixOf "\\par}\n" str
     then (take ((length str) - 6) str) ++ "\\sa180\\par}\n"
     else str

-- | Convert list item (list of blocks) to RTF.
listItemToRTF :: PandocMonad m
              => Alignment  -- ^ alignment
              -> Int        -- ^ indent level
              -> String     -- ^ list start marker
              -> [Block]    -- ^ list item (list of blocks)
              -> m String
listItemToRTF alignment indent marker [] = return $
  rtfCompact (indent + listIncrement) (0 - listIncrement) alignment
             (marker ++ "\\tx" ++ (show listIncrement) ++ "\\tab ")
listItemToRTF alignment indent marker list = do
  (first:rest) <- mapM (blockToRTF (indent + listIncrement) alignment) list
  let listMarker = "\\fi" ++ show (0 - listIncrement) ++ " " ++ marker ++
                   "\\tx" ++ show listIncrement ++ "\\tab"
  let insertListMarker ('\\':'f':'i':'-':d:xs) | isDigit d =
        listMarker ++ dropWhile isDigit xs
      insertListMarker ('\\':'f':'i':d:xs) | isDigit d =
        listMarker ++ dropWhile isDigit xs
      insertListMarker (x:xs) =
        x : insertListMarker xs
      insertListMarker [] = []
   -- insert the list marker into the (processed) first block
  return $ insertListMarker first ++ concat rest

-- | Convert definition list item (label, list of blocks) to RTF.
definitionListItemToRTF :: PandocMonad m
                        => Alignment          -- ^ alignment
                        -> Int                -- ^ indent level
                        -> ([Inline],[[Block]]) -- ^ list item (list of blocks)
                        -> m String
definitionListItemToRTF alignment indent (label, defs) = do
  labelText <- blockToRTF indent alignment (Plain label)
  itemsText <- blocksToRTF (indent + listIncrement) alignment (concat defs)
  return $ labelText ++ itemsText

-- | Convert list of inline items to RTF.
inlinesToRTF :: PandocMonad m
             => [Inline]   -- ^ list of inlines to convert
             -> m String
inlinesToRTF lst = concat <$> mapM inlineToRTF lst

-- | Convert inline item to RTF.
inlineToRTF :: PandocMonad m
            => Inline         -- ^ inline to convert
            -> m String
inlineToRTF (Span _ lst) = inlinesToRTF lst
inlineToRTF (Emph lst) = do
  contents <- inlinesToRTF lst
  return $ "{\\i " ++ contents ++ "}"
inlineToRTF (Strong lst) = do
  contents <- inlinesToRTF lst
  return $ "{\\b " ++ contents ++ "}"
inlineToRTF (Strikeout lst) = do
  contents <- inlinesToRTF lst
  return $ "{\\strike " ++ contents ++ "}"
inlineToRTF (Superscript lst) = do
  contents <- inlinesToRTF lst
  return $ "{\\super " ++ contents ++ "}"
inlineToRTF (Subscript lst) = do
  contents <- inlinesToRTF lst
  return $ "{\\sub " ++ contents ++ "}"
inlineToRTF (SmallCaps lst) = do
  contents <- inlinesToRTF lst
  return $ "{\\scaps " ++ contents ++ "}"
inlineToRTF (Quoted SingleQuote lst) = do
  contents <- inlinesToRTF lst
  return $ "\\u8216'" ++ contents ++ "\\u8217'"
inlineToRTF (Quoted DoubleQuote lst) = do
  contents <- inlinesToRTF lst
  return $ "\\u8220\"" ++ contents ++ "\\u8221\""
inlineToRTF (Code _ str) = return $ "{\\f1 " ++ (codeStringToRTF str) ++ "}"
inlineToRTF (Str str) = return $ stringToRTF str
inlineToRTF (Math t str) = texMathToInlines t str >>= inlinesToRTF
inlineToRTF (Cite _ lst) = inlinesToRTF lst
inlineToRTF il@(RawInline f str)
  | f == Format "rtf" = return str
  | otherwise         = do
      return $ InlineNotRendered il
      return ""
inlineToRTF (LineBreak) = return "\\line "
inlineToRTF SoftBreak = return " "
inlineToRTF Space = return " "
inlineToRTF (Link _ text (src, _)) = do
  contents <- inlinesToRTF text
  return $ "{\\field{\\*\\fldinst{HYPERLINK \"" ++ (codeStringToRTF src) ++
    "\"}}{\\fldrslt{\\ul\n" ++ contents ++ "\n}}}\n"
inlineToRTF (Image _ _ (source, _)) =
  return $ "{\\cf1 [image: " ++ source ++ "]\\cf0}"
inlineToRTF (Note contents) = do
  body <- concat <$> mapM (blockToRTF 0 AlignDefault) contents
  return $ "{\\super\\chftn}{\\*\\footnote\\chftn\\~\\plain\\pard " ++
    body ++ "}"
