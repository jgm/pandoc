{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Writers.RTF
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to RTF (rich text format).
-}
module Text.Pandoc.Writers.RTF ( writeRTF
                               ) where
import Prelude
import Control.Monad.Except (catchError, throwError)
import Control.Monad
import qualified Data.ByteString as B
import Data.Char (chr, isDigit, ord)
import Data.List (intercalate, isSuffixOf)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class (PandocMonad, report)
import qualified Text.Pandoc.Class as P
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.DocLayout (render, literal)
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import Text.Printf (printf)

-- | Convert Image inlines into a raw RTF embedded image, read from a file,
-- or a MediaBag, or the internet.
-- If file not found or filetype not jpeg or png, leave the inline unchanged.
rtfEmbedImage :: PandocMonad m => WriterOptions -> Inline -> m Inline
rtfEmbedImage opts x@(Image attr _ (src,_)) = catchError
  (do result <- P.fetchItem src
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
  metadata <- metaToContext options
              (fmap (literal . T.pack . concat) .
                mapM (blockToRTF 0 AlignDefault))
              (fmap (literal . T.pack) . inlinesToRTF)
              meta'
  body <- T.pack <$> blocksToRTF 0 AlignDefault blocks
  toc <- T.pack <$> blocksToRTF 0 AlignDefault
          [toTableOfContents options $ filter isHeaderBlock blocks]
  let context = defField "body" body
              $ defField "spacer" spacer
              $(if writerTableOfContents options
                   then defField "table-of-contents" toc
                        -- for backwards compatibility,
                        -- we populate toc with the contents
                        -- of the toc rather than a boolean:
                        . defField "toc" toc
                   else id) metadata
  return $
    case writerTemplate options of
       Just tpl -> render Nothing $ renderTemplate tpl context
       Nothing  -> case T.unsnoc body of
                        Just (_,'\n') -> body
                        _             -> body <> T.singleton '\n'

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
     else c:handleUnicode cs
  where
    surrogate x = not (   (0x0000 <= ord x && ord x <= 0xd7ff)
                       || (0xe000 <= ord x && ord x <= 0xffff) )
    enc x = '\\':'u':show (ord x) ++ "?"

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
      "\\f0 \\sa" ++ show spaceAfter ++ " \\li" ++ show indent ++
      " \\fi" ++ show firstLineIndent ++ " " ++ content ++ "\\par}\n"

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
  return $ rtfPar indent 0 AlignLeft ("\\f1 " ++ codeStringToRTF str)
blockToRTF _ _ b@(RawBlock f str)
  | f == Format "rtf" = return str
  | otherwise         = do
      report $ BlockNotRendered b
      return ""
blockToRTF indent alignment (BulletList lst) = (spaceAtEnd . concat) <$>
  mapM (listItemToRTF alignment indent (bulletMarker indent)) lst
blockToRTF indent alignment (OrderedList attribs lst) =
  (spaceAtEnd . concat) <$>
   zipWithM (listItemToRTF alignment indent) (orderedMarkers indent attribs) lst
blockToRTF indent alignment (DefinitionList lst) = (spaceAtEnd . concat) <$>
  mapM (definitionListItemToRTF alignment indent) lst
blockToRTF indent _ HorizontalRule = return $
  rtfPar indent 0 AlignCenter "\\emdash\\emdash\\emdash\\emdash\\emdash"
blockToRTF indent alignment (Header level _ lst) = do
  contents <- inlinesToRTF lst
  return $ rtfPar indent 0 alignment $
             "\\b \\fs" ++ show (40 - (level * 4)) ++ " " ++ contents
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
                 then replicate (length cols) (1.0 / fromIntegral (length cols))
                 else sizes'
  columns <- concat <$>
     zipWithM (tableItemToRTF indent) aligns cols
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
  if "\\par}\n" `isSuffixOf` str
     then take (length str - 6) str ++ "\\sa180\\par}\n"
     else str

-- | Convert list item (list of blocks) to RTF.
listItemToRTF :: PandocMonad m
              => Alignment  -- ^ alignment
              -> Int        -- ^ indent level
              -> String     -- ^ list start marker
              -> [Block]    -- ^ list item (list of blocks)
              -> m String
listItemToRTF alignment indent marker [] = return $
  rtfCompact (indent + listIncrement) (negate listIncrement) alignment
             (marker ++ "\\tx" ++ show listIncrement ++ "\\tab ")
listItemToRTF alignment indent marker (listFirst:listRest) = do
  let f = blockToRTF (indent + listIncrement) alignment
  first <- f listFirst
  rest <- mapM f listRest
  let listMarker = "\\fi" ++ show (negate listIncrement) ++ " " ++ marker ++
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
inlineToRTF (Code _ str) = return $ "{\\f1 " ++ codeStringToRTF str ++ "}"
inlineToRTF (Str str) = return $ stringToRTF str
inlineToRTF (Math t str) = texMathToInlines t str >>= inlinesToRTF
inlineToRTF (Cite _ lst) = inlinesToRTF lst
inlineToRTF il@(RawInline f str)
  | f == Format "rtf" = return str
  | otherwise         = do
      report $ InlineNotRendered il
      return ""
inlineToRTF LineBreak = return "\\line "
inlineToRTF SoftBreak = return " "
inlineToRTF Space = return " "
inlineToRTF (Link _ text (src, _)) = do
  contents <- inlinesToRTF text
  return $ "{\\field{\\*\\fldinst{HYPERLINK \"" ++ codeStringToRTF src ++
    "\"}}{\\fldrslt{\\ul\n" ++ contents ++ "\n}}}\n"
inlineToRTF (Image _ _ (source, _)) =
  return $ "{\\cf1 [image: " ++ source ++ "]\\cf0}"
inlineToRTF (Note contents) = do
  body <- concat <$> mapM (blockToRTF 0 AlignDefault) contents
  return $ "{\\super\\chftn}{\\*\\footnote\\chftn\\~\\plain\\pard " ++
    body ++ "}"
