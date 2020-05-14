{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{- |
   Module      : Text.Pandoc.Writers.RTF
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to RTF (rich text format).
-}
module Text.Pandoc.Writers.RTF ( writeRTF
                               ) where
import Control.Monad.Except (catchError, throwError)
import Control.Monad
import qualified Data.ByteString as B
import Data.Char (chr, isDigit, ord)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import qualified Text.Pandoc.Class.PandocMonad as P
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
             let bytes = map (T.pack . printf "%02x") $ B.unpack imgdata
             filetype <-
                case mime of
                     "image/jpeg" -> return "\\jpegblip"
                     "image/png"  -> return "\\pngblip"
                     _            -> throwError $
                                         PandocShouldNeverHappenError $
                                         "Unknown file type " <> mime
             sizeSpec <-
                case imageSize opts imgdata of
                     Left msg -> do
                       report $ CouldNotDetermineImageSize src msg
                       return ""
                     Right sz -> return $ "\\picw" <> tshow xpx <>
                                "\\pich" <> tshow ypx <>
                                "\\picwgoal" <> tshow (floor (xpt * 20) :: Integer)
                                <> "\\pichgoal" <> tshow (floor (ypt * 20) :: Integer)
                        -- twip = 1/1440in = 1/20pt
                        where (xpx, ypx) = sizeInPixels sz
                              (xpt, ypt) = desiredSizeInPoints opts attr sz
             let raw = "{\\pict" <> filetype <> sizeSpec <> "\\bin " <>
                        T.concat bytes <> "}"
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
     report $ CouldNotFetchResource src $ tshow e
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
              (fmap (literal . T.concat) .
                mapM (blockToRTF 0 AlignDefault))
              (fmap literal . inlinesToRTF)
              meta'
  body <- blocksToRTF 0 AlignDefault blocks
  toc <- blocksToRTF 0 AlignDefault [toTableOfContents options blocks]
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
handleUnicode :: Text -> Text
handleUnicode = T.concatMap $ \c ->
  if ord c > 127
     then if surrogate c
          then let x = ord c - 0x10000
                   (q, r) = x `divMod` 0x400
                   upper = q + 0xd800
                   lower = r + 0xDC00
               in enc (chr upper) <> enc (chr lower)
          else enc c
     else T.singleton c
  where
    surrogate x = not (   (0x0000 <= ord x && ord x <= 0xd7ff)
                       || (0xe000 <= ord x && ord x <= 0xffff) )
    enc x = "\\u" <> tshow (ord x) <> "?"

-- | Escape special characters.
escapeSpecial :: Text -> Text
escapeSpecial = escapeStringUsing $
  [ ('\t',"\\tab ")
  , ('\8216',"\\u8216'")
  , ('\8217',"\\u8217'")
  , ('\8220',"\\u8220\"")
  , ('\8221',"\\u8221\"")
  , ('\8211',"\\u8211-")
  , ('\8212',"\\u8212-")
  ] <> backslashEscapes "{\\}"

-- | Escape strings as needed for rich text format.
stringToRTF :: Text -> Text
stringToRTF = handleUnicode . escapeSpecial

-- | Escape things as needed for code block in RTF.
codeStringToRTF :: Text -> Text
codeStringToRTF str = T.intercalate "\\line\n" $ T.lines (stringToRTF str)

-- | Make a paragraph with first-line indent, block indent, and space after.
rtfParSpaced :: Int       -- ^ space after (in twips)
             -> Int       -- ^ block indent (in twips)
             -> Int       -- ^ first line indent (relative to block) (in twips)
             -> Alignment -- ^ alignment
             -> Text    -- ^ string with content
             -> Text
rtfParSpaced spaceAfter indent firstLineIndent alignment content =
  let alignString = case alignment of
                           AlignLeft    -> "\\ql "
                           AlignRight   -> "\\qr "
                           AlignCenter  -> "\\qc "
                           AlignDefault -> "\\ql "
  in  "{\\pard " <> alignString <>
      "\\f0 \\sa" <> tshow spaceAfter <> " \\li" <> T.pack (show indent) <>
      " \\fi" <> tshow firstLineIndent <> " " <> content <> "\\par}\n"

-- | Default paragraph.
rtfPar :: Int       -- ^ block indent (in twips)
       -> Int       -- ^ first line indent (relative to block) (in twips)
       -> Alignment -- ^ alignment
       -> Text    -- ^ string with content
       -> Text
rtfPar = rtfParSpaced 180

-- | Compact paragraph (e.g. for compact list items).
rtfCompact ::  Int       -- ^ block indent (in twips)
           ->  Int       -- ^ first line indent (relative to block) (in twips)
           ->  Alignment -- ^ alignment
           ->  Text    -- ^ string with content
           ->  Text
rtfCompact = rtfParSpaced 0

-- number of twips to indent
indentIncrement :: Int
indentIncrement = 720

listIncrement :: Int
listIncrement = 360

-- | Returns appropriate bullet list marker for indent level.
bulletMarker :: Int -> Text
bulletMarker indent = case indent `mod` 720 of
                             0 -> "\\bullet "
                             _ -> "\\endash "

-- | Returns appropriate (list of) ordered list markers for indent level.
orderedMarkers :: Int -> ListAttributes -> [Text]
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
            -> m Text
blocksToRTF indent align = fmap T.concat . mapM (blockToRTF indent align)

-- | Convert Pandoc block element to RTF.
blockToRTF :: PandocMonad m
           => Int       -- ^ indent level
           -> Alignment -- ^ alignment
           -> Block     -- ^ block to convert
           -> m Text
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
  return $ rtfPar indent 0 AlignLeft ("\\f1 " <> codeStringToRTF str)
blockToRTF _ _ b@(RawBlock f str)
  | f == Format "rtf" = return str
  | otherwise         = do
      report $ BlockNotRendered b
      return ""
blockToRTF indent alignment (BulletList lst) = (spaceAtEnd . T.concat) <$>
  mapM (listItemToRTF alignment indent (bulletMarker indent)) lst
blockToRTF indent alignment (OrderedList attribs lst) =
  (spaceAtEnd . T.concat) <$>
   zipWithM (listItemToRTF alignment indent) (orderedMarkers indent attribs) lst
blockToRTF indent alignment (DefinitionList lst) = (spaceAtEnd . T.concat) <$>
  mapM (definitionListItemToRTF alignment indent) lst
blockToRTF indent _ HorizontalRule = return $
  rtfPar indent 0 AlignCenter "\\emdash\\emdash\\emdash\\emdash\\emdash"
blockToRTF indent alignment (Header level _ lst) = do
  contents <- inlinesToRTF lst
  return $ rtfPar indent 0 alignment $
             "\\b \\fs" <> tshow (40 - (level * 4)) <> " " <> contents
blockToRTF indent alignment (Table _ blkCapt specs thead tbody tfoot) = do
  let (caption, aligns, sizes, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
  caption' <- inlinesToRTF caption
  header' <- if all null headers
                then return ""
                else tableRowToRTF True indent aligns sizes headers
  rows' <- T.concat <$> mapM (tableRowToRTF False indent aligns sizes) rows
  return $ header' <> rows' <> rtfPar indent 0 alignment caption'

tableRowToRTF :: PandocMonad m
              => Bool -> Int -> [Alignment] -> [Double] -> [[Block]] -> m Text
tableRowToRTF header indent aligns sizes' cols = do
  let totalTwips = 6 * 1440 -- 6 inches
  let sizes = if all (== 0) sizes'
                 then replicate (length cols) (1.0 / fromIntegral (length cols))
                 else sizes'
  columns <- T.concat <$>
     zipWithM (tableItemToRTF indent) aligns cols
  let rightEdges = tail $ scanl (\sofar new -> sofar + floor (new * totalTwips))
                                (0 :: Integer) sizes
  let cellDefs = map (\edge -> (if header
                                   then "\\clbrdrb\\brdrs"
                                   else "") <> "\\cellx" <> tshow edge)
                     rightEdges
  let start = "{\n\\trowd \\trgaph120\n" <> T.concat cellDefs <> "\n" <>
              "\\trkeep\\intbl\n{\n"
  let end = "}\n\\intbl\\row}\n"
  return $ start <> columns <> end

tableItemToRTF :: PandocMonad m => Int -> Alignment -> [Block] -> m Text
tableItemToRTF indent alignment item = do
  contents <- blocksToRTF indent alignment item
  return $ "{" <> T.replace "\\pard" "\\pard\\intbl" contents <> "\\cell}\n"

-- | Ensure that there's the same amount of space after compact
-- lists as after regular lists.
spaceAtEnd :: Text -> Text
spaceAtEnd str = maybe str (<> "\\sa180\\par}\n") $ T.stripSuffix "\\par}\n" str

-- | Convert list item (list of blocks) to RTF.
listItemToRTF :: PandocMonad m
              => Alignment  -- ^ alignment
              -> Int        -- ^ indent level
              -> Text     -- ^ list start marker
              -> [Block]    -- ^ list item (list of blocks)
              -> m Text
listItemToRTF alignment indent marker [] = return $
  rtfCompact (indent + listIncrement) (negate listIncrement) alignment
             (marker <> "\\tx" <> tshow listIncrement <> "\\tab ")
listItemToRTF alignment indent marker (listFirst:listRest) = do
  let f = blockToRTF (indent + listIncrement) alignment
  first <- f listFirst
  rest <- mapM f listRest
  let listMarker = "\\fi" <> tshow (negate listIncrement) <> " " <> marker <>
                   "\\tx" <> tshow listIncrement <> "\\tab"
  -- Find the first occurrence of \\fi or \\fi-, then replace it and the following
  -- digits with the list marker.
  let insertListMarker t = case popDigit $ optionDash $ T.drop 3 suff of
        Just suff' -> pref <> listMarker <> T.dropWhile isDigit suff'
        Nothing    -> t
        where
          (pref, suff) = T.breakOn "\\fi" t
          optionDash x = case T.uncons x of
            Just ('-', xs) -> xs
            _              -> x
          popDigit x
            | Just (d, xs) <- T.uncons x
            , isDigit d = Just xs
            | otherwise = Nothing
   -- insert the list marker into the (processed) first block
  return $ insertListMarker first <> T.concat rest

-- | Convert definition list item (label, list of blocks) to RTF.
definitionListItemToRTF :: PandocMonad m
                        => Alignment          -- ^ alignment
                        -> Int                -- ^ indent level
                        -> ([Inline],[[Block]]) -- ^ list item (list of blocks)
                        -> m Text
definitionListItemToRTF alignment indent (label, defs) = do
  labelText <- blockToRTF indent alignment (Plain label)
  itemsText <- blocksToRTF (indent + listIncrement) alignment (concat defs)
  return $ labelText <> itemsText

-- | Convert list of inline items to RTF.
inlinesToRTF :: PandocMonad m
             => [Inline]   -- ^ list of inlines to convert
             -> m Text
inlinesToRTF lst = T.concat <$> mapM inlineToRTF lst

-- | Convert inline item to RTF.
inlineToRTF :: PandocMonad m
            => Inline         -- ^ inline to convert
            -> m Text
inlineToRTF (Span _ lst) = inlinesToRTF lst
inlineToRTF (Emph lst) = do
  contents <- inlinesToRTF lst
  return $ "{\\i " <> contents <> "}"
inlineToRTF (Underline lst) = do
  contents <- inlinesToRTF lst
  return $ "{\\ul " <> contents <> "}"
inlineToRTF (Strong lst) = do
  contents <- inlinesToRTF lst
  return $ "{\\b " <> contents <> "}"
inlineToRTF (Strikeout lst) = do
  contents <- inlinesToRTF lst
  return $ "{\\strike " <> contents <> "}"
inlineToRTF (Superscript lst) = do
  contents <- inlinesToRTF lst
  return $ "{\\super " <> contents <> "}"
inlineToRTF (Subscript lst) = do
  contents <- inlinesToRTF lst
  return $ "{\\sub " <> contents <> "}"
inlineToRTF (SmallCaps lst) = do
  contents <- inlinesToRTF lst
  return $ "{\\scaps " <> contents <> "}"
inlineToRTF (Quoted SingleQuote lst) = do
  contents <- inlinesToRTF lst
  return $ "\\u8216'" <> contents <> "\\u8217'"
inlineToRTF (Quoted DoubleQuote lst) = do
  contents <- inlinesToRTF lst
  return $ "\\u8220\"" <> contents <> "\\u8221\""
inlineToRTF (Code _ str) = return $ "{\\f1 " <> codeStringToRTF str <> "}"
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
  return $ "{\\field{\\*\\fldinst{HYPERLINK \"" <> codeStringToRTF src <>
    "\"}}{\\fldrslt{\\ul\n" <> contents <> "\n}}}\n"
inlineToRTF (Image _ _ (source, _)) =
  return $ "{\\cf1 [image: " <> source <> "]\\cf0}"
inlineToRTF (Note contents) = do
  body <- T.concat <$> mapM (blockToRTF 0 AlignDefault) contents
  return $ "{\\super\\chftn}{\\*\\footnote\\chftn\\~\\plain\\pard " <>
    body <> "}"
