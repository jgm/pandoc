{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{- |
   Module      : Text.Pandoc.Writers.Textile
   Copyright   : Copyright (C) 2010-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Textile markup.

Textile:  <http://thresholdstate.com/articles/4312/the-textile-reference-manual>
-}
module Text.Pandoc.Writers.Textile ( writeTextile ) where
import Control.Monad.State.Strict
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.DocLayout (render, literal)
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Shared
import Text.Pandoc.XML (escapeStringForXML)

data WriterState = WriterState {
    stNotes     :: [Text]        -- Footnotes
  , stListLevel :: [Char]        -- String at beginning of list items, e.g. "**"
  , stStartNum  :: Maybe Int     -- Start number if first list item
  , stUseTags   :: Bool          -- True if we should use HTML tags because we're in a complex list
  }

type TW = StateT WriterState

-- | Convert Pandoc to Textile.
writeTextile :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeTextile opts document =
  evalStateT (pandocToTextile opts document)
            WriterState { stNotes = [],
                          stListLevel = [],
                          stStartNum = Nothing,
                          stUseTags = False }

-- | Return Textile representation of document.
pandocToTextile :: PandocMonad m
                => WriterOptions -> Pandoc -> TW m Text
pandocToTextile opts (Pandoc meta blocks) = do
  metadata <- metaToContext opts
                 (fmap literal . blockListToTextile opts)
                 (fmap literal . inlineListToTextile opts) meta
  body <- blockListToTextile opts blocks
  notes <- gets $ T.unlines . reverse . stNotes
  let main = body <> if T.null notes then "" else "\n\n" <> notes
  let context = defField "body" main metadata
  return $
    case writerTemplate opts of
         Nothing  -> main
         Just tpl -> render Nothing $ renderTemplate tpl context

withUseTags :: PandocMonad m => TW m a -> TW m a
withUseTags action = do
  oldUseTags <- gets stUseTags
  modify $ \s -> s { stUseTags = True }
  result <- action
  modify $ \s -> s { stUseTags = oldUseTags }
  return result

-- | Escape one character as needed for Textile.
escapeCharForTextile :: Char -> Text
escapeCharForTextile x = case x of
                         '&'      -> "&amp;"
                         '<'      -> "&lt;"
                         '>'      -> "&gt;"
                         '"'      -> "&quot;"
                         '*'      -> "&#42;"
                         '_'      -> "&#95;"
                         '@'      -> "&#64;"
                         '+'      -> "&#43;"
                         '-'      -> "&#45;"
                         '|'      -> "&#124;"
                         '\x2014' -> " -- "
                         '\x2013' -> " - "
                         '\x2019' -> "'"
                         '\x2026' -> "..."
                         c        -> T.singleton c

-- | Escape string as needed for Textile.
escapeTextForTextile :: Text -> Text
escapeTextForTextile = T.concatMap escapeCharForTextile

-- | Convert Pandoc block element to Textile.
blockToTextile :: PandocMonad m
               => WriterOptions -- ^ Options
               -> Block         -- ^ Block element
               -> TW m Text

blockToTextile _ Null = return ""

blockToTextile opts (Div attr bs) = do
  let startTag = render Nothing $ tagWithAttrs "div" attr
  let endTag = "</div>"
  contents <- blockListToTextile opts bs
  return $ startTag <> "\n\n" <> contents <> "\n\n" <> endTag <> "\n"

blockToTextile opts (Plain inlines) =
  inlineListToTextile opts inlines

-- title beginning with fig: indicates that the image is a figure
blockToTextile opts (Para [Image attr txt (src,T.stripPrefix "fig:" -> Just tit)]) = do
  capt <- blockToTextile opts (Para txt)
  im <- inlineToTextile opts (Image attr txt (src,tit))
  return $ im <> "\n" <> capt

blockToTextile opts (Para inlines) = do
  useTags <- gets stUseTags
  listLevel <- gets stListLevel
  contents <- inlineListToTextile opts inlines
  return $ if useTags
              then "<p>" <> contents <> "</p>"
              else contents <> if null listLevel then "\n" else ""

blockToTextile opts (LineBlock lns) =
  blockToTextile opts $ linesToPara lns

blockToTextile _ b@(RawBlock f str)
  | f == Format "html" || f == Format "textile" = return str
  | otherwise                                   = do
      report $ BlockNotRendered b
      return ""

blockToTextile _ HorizontalRule = return "<hr />\n"

blockToTextile opts (Header level (ident,classes,keyvals) inlines) = do
  contents <- inlineListToTextile opts inlines
  let identAttr = if T.null ident then "" else "#" <> ident
  let attribs = if T.null identAttr && null classes
                   then ""
                   else "(" <> T.unwords classes <> identAttr <> ")"
  let lang = maybe "" (\x -> "[" <> x <> "]") $ lookup "lang" keyvals
  let styles = maybe "" (\x -> "{" <> x <> "}") $ lookup "style" keyvals
  let prefix = "h" <> tshow level <> attribs <> styles <> lang <> ". "
  return $ prefix <> contents <> "\n"

blockToTextile _ (CodeBlock (_,classes,_) str) | any (T.all isSpace) (T.lines str) =
  return $ "<pre"  <> classes' <> ">\n" <> escapeStringForXML str <>
           "\n</pre>\n"
    where classes' = if null classes
                        then ""
                        else " class=\"" <> T.unwords classes <> "\""

blockToTextile _ (CodeBlock (_,classes,_) str) =
  return $ "bc" <> classes' <> ". " <> str <> "\n\n"
    where classes' = if null classes
                        then ""
                        else "(" <> T.unwords classes <> ")"

blockToTextile opts (BlockQuote bs@[Para _]) = do
  contents <- blockListToTextile opts bs
  return $ "bq. " <> contents <> "\n\n"

blockToTextile opts (BlockQuote blocks) = do
  contents <- blockListToTextile opts blocks
  return $ "<blockquote>\n\n" <> contents <> "\n</blockquote>\n"

blockToTextile opts (Table _ blkCapt specs thead tbody tfoot)
  = case toLegacyTable blkCapt specs thead tbody tfoot of
      ([], aligns, widths, headers, rows') | all (==0) widths -> do
        hs <- mapM (liftM (("_. " <>) . stripTrailingNewlines) . blockListToTextile opts) headers
        let cellsToRow cells = "|" <> T.intercalate "|" cells <> "|"
        let header = if all null headers then "" else cellsToRow hs <> "\n"
        let blocksToCell (align, bs) = do
              contents <- stripTrailingNewlines <$> blockListToTextile opts bs
              let alignMarker = case align of
                                     AlignLeft    -> "<. "
                                     AlignRight   -> ">. "
                                     AlignCenter  -> "=. "
                                     AlignDefault -> ""
              return $ alignMarker <> contents
        let rowToCells = mapM blocksToCell . zip aligns
        bs <- mapM rowToCells rows'
        let body = T.unlines $ map cellsToRow bs
        return $ header <> body
      (capt, aligns, widths, headers, rows') -> do
        let alignStrings = map alignmentToText aligns
        captionDoc <- if null capt
                         then return ""
                         else do
                            c <- inlineListToTextile opts capt
                            return $ "<caption>" <> c <> "</caption>\n"
        let percent w = tshow (truncate (100*w) :: Integer) <> "%"
        let coltags = if all (== 0.0) widths
                         then ""
                         else T.unlines $ map
                               (\w -> "<col width=\"" <> percent w <> "\" />") widths
        head' <- if all null headers
                    then return ""
                    else do
                       hs <- tableRowToTextile opts alignStrings 0 headers
                       return $ "<thead>\n" <> hs <> "\n</thead>\n"
        body' <- zipWithM (tableRowToTextile opts alignStrings) [1..] rows'
        return $ "<table>\n" <> captionDoc <> coltags <> head' <>
                  "<tbody>\n" <> T.unlines body' <> "</tbody>\n</table>\n"

blockToTextile opts x@(BulletList items) = do
  oldUseTags <- gets stUseTags
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        contents <- withUseTags $ mapM (listItemToTextile opts) items
        return $ "<ul>\n" <> vcat contents <> "\n</ul>\n"
     else do
        modify $ \s -> s { stListLevel = stListLevel s <> "*" }
        level <- gets $ length . stListLevel
        contents <- mapM (listItemToTextile opts) items
        modify $ \s -> s { stListLevel = init (stListLevel s) }
        return $ vcat contents <> (if level > 1 then "" else "\n")

blockToTextile opts x@(OrderedList attribs@(start, _, _) items) = do
  oldUseTags <- gets stUseTags
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        contents <- withUseTags $ mapM (listItemToTextile opts) items
        return $ "<ol" <> listAttribsToString attribs <> ">\n" <> vcat contents <>
                   "\n</ol>\n"
     else do
        modify $ \s -> s { stListLevel = stListLevel s <> "#"
                         , stStartNum = if start > 1
                                           then Just start
                                           else Nothing }
        level <- gets $ length . stListLevel
        contents <- mapM (listItemToTextile opts) items
        modify $ \s -> s { stListLevel = init (stListLevel s),
                           stStartNum = Nothing }
        return $ vcat contents <> (if level > 1 then "" else "\n")

blockToTextile opts (DefinitionList items) = do
  contents <- withUseTags $ mapM (definitionListItemToTextile opts) items
  return $ "<dl>\n" <> vcat contents <> "\n</dl>\n"

-- Auxiliary functions for lists:

-- | Convert ordered list attributes to HTML attribute string
listAttribsToString :: ListAttributes -> Text
listAttribsToString (startnum, numstyle, _) =
  let numstyle' = camelCaseToHyphenated $ tshow numstyle
  in  (if startnum /= 1
          then " start=\"" <> tshow startnum <> "\""
          else "") <>
      (if numstyle /= DefaultStyle
          then " style=\"list-style-type: " <> numstyle' <> ";\""
          else "")

-- | Convert bullet or ordered list item (list of blocks) to Textile.
listItemToTextile :: PandocMonad m
                  => WriterOptions -> [Block] -> TW m Text
listItemToTextile opts items = do
  contents <- blockListToTextile opts items
  useTags <- gets stUseTags
  if useTags
     then return $ "<li>" <> contents <> "</li>"
     else do
       marker <- gets stListLevel
       mbstart <- gets stStartNum
       case mbstart of
            Just n -> do
              modify $ \s -> s{ stStartNum = Nothing }
              return $ T.pack marker <> tshow n <> " " <> contents
            Nothing -> return $ T.pack marker <> " " <> contents

-- | Convert definition list item (label, list of blocks) to Textile.
definitionListItemToTextile :: PandocMonad m
                            => WriterOptions
                             -> ([Inline],[[Block]])
                             -> TW m Text
definitionListItemToTextile opts (label, items) = do
  labelText <- inlineListToTextile opts label
  contents <- mapM (blockListToTextile opts) items
  return $ "<dt>" <> labelText <> "</dt>\n" <>
          T.intercalate "\n" (map (\d -> "<dd>" <> d <> "</dd>") contents)

-- | True if the list can be handled by simple wiki markup, False if HTML tags will be needed.
isSimpleList :: Block -> Bool
isSimpleList x =
  case x of
       BulletList items                 -> all isSimpleListItem items
       OrderedList (_, sty, _) items    -> all isSimpleListItem items &&
                                            sty `elem` [DefaultStyle, Decimal]
       _                                -> False

-- | True if list item can be handled with the simple wiki syntax.  False if
--   HTML tags will be needed.
isSimpleListItem :: [Block] -> Bool
isSimpleListItem []  = True
isSimpleListItem [x] =
  case x of
       Plain _         -> True
       Para  _         -> True
       BulletList _    -> isSimpleList x
       OrderedList _ _ -> isSimpleList x
       _               -> False
isSimpleListItem [x, y] | isPlainOrPara x =
  case y of
       BulletList _    -> isSimpleList y
       OrderedList _ _ -> isSimpleList y
       _               -> False
isSimpleListItem _ = False

isPlainOrPara :: Block -> Bool
isPlainOrPara (Plain _) = True
isPlainOrPara (Para  _) = True
isPlainOrPara _         = False

-- | Concatenates strings with line breaks between them.
vcat :: [Text] -> Text
vcat = T.intercalate "\n"

-- Auxiliary functions for tables. (TODO: these are common to HTML, MediaWiki,
-- and Textile writers, and should be abstracted out.)

tableRowToTextile :: PandocMonad m
                  => WriterOptions
                  -> [Text]
                  -> Int
                  -> [[Block]]
                  -> TW m Text
tableRowToTextile opts alignStrings rownum cols' = do
  let celltype = if rownum == 0 then "th" else "td"
  let rowclass = case rownum of
                      0 -> "header"
                      x | x `rem` 2 == 1 -> "odd"
                      _ -> "even"
  cols'' <- zipWithM
            (\alignment item -> tableItemToTextile opts celltype alignment item)
            alignStrings cols'
  return $ "<tr class=\"" <> rowclass <> "\">\n" <> T.unlines cols'' <> "</tr>"

alignmentToText :: Alignment -> Text
alignmentToText alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

tableItemToTextile :: PandocMonad m
                   => WriterOptions
                   -> Text
                   -> Text
                   -> [Block]
                   -> TW m Text
tableItemToTextile opts celltype align' item = do
  let mkcell x = "<" <> celltype <> " align=\"" <> align' <> "\">" <>
                    x <> "</" <> celltype <> ">"
  contents <- blockListToTextile opts item
  return $ mkcell contents

-- | Convert list of Pandoc block elements to Textile.
blockListToTextile :: PandocMonad m
                   => WriterOptions -- ^ Options
                   -> [Block]       -- ^ List of block elements
                   -> TW m Text
blockListToTextile opts blocks =
  vcat <$> mapM (blockToTextile opts) blocks

-- | Convert list of Pandoc inline elements to Textile.
inlineListToTextile :: PandocMonad m
                    => WriterOptions -> [Inline] -> TW m Text
inlineListToTextile opts lst =
  T.concat <$> mapM (inlineToTextile opts) lst

-- | Convert Pandoc inline element to Textile.
inlineToTextile :: PandocMonad m => WriterOptions -> Inline -> TW m Text

inlineToTextile opts (Span _ lst) =
  inlineListToTextile opts lst

inlineToTextile opts (Emph lst) = do
  contents <- inlineListToTextile opts lst
  return $ if '_' `elemText` contents
              then "<em>" <> contents <> "</em>"
              else "_" <> contents <> "_"

inlineToTextile opts (Underline lst) = do
  contents <- inlineListToTextile opts lst
  return $ if '+' `elemText` contents
              then "<u>" <> contents <> "</u>"
              else "+" <> contents <> "+"

inlineToTextile opts (Strong lst) = do
  contents <- inlineListToTextile opts lst
  return $ if '*' `elemText` contents
              then "<strong>" <> contents <> "</strong>"
              else "*" <> contents <> "*"

inlineToTextile opts (Strikeout lst) = do
  contents <- inlineListToTextile opts lst
  return $ if '-' `elemText` contents
              then "<del>" <> contents <> "</del>"
              else "-" <> contents <> "-"

inlineToTextile opts (Superscript lst) = do
  contents <- inlineListToTextile opts lst
  return $ if '^' `elemText` contents
              then "<sup>" <> contents <> "</sup>"
              else "[^" <> contents <> "^]"

inlineToTextile opts (Subscript lst) = do
  contents <- inlineListToTextile opts lst
  return $ if '~' `elemText` contents
              then "<sub>" <> contents <> "</sub>"
              else "[~" <> contents <> "~]"

inlineToTextile opts (SmallCaps lst) = inlineListToTextile opts lst

inlineToTextile opts (Quoted SingleQuote lst) = do
  contents <- inlineListToTextile opts lst
  return $ "'" <> contents <> "'"

inlineToTextile opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToTextile opts lst
  return $ "\"" <> contents <> "\""

inlineToTextile opts (Cite _  lst) = inlineListToTextile opts lst

inlineToTextile _ (Code _ str) =
  return $ if '@' `elemText` str
           then "<tt>" <> escapeStringForXML str <> "</tt>"
           else "@" <> str <> "@"

inlineToTextile _ (Str str) = return $ escapeTextForTextile str

inlineToTextile _ (Math _ str) =
  return $ "<span class=\"math\">" <> escapeStringForXML str <> "</span>"

inlineToTextile opts il@(RawInline f str)
  | f == Format "html" || f == Format "textile" = return str
  | (f == Format "latex" || f == Format "tex") &&
     isEnabled Ext_raw_tex opts                 = return str
  | otherwise                                   = do
      report $ InlineNotRendered il
      return ""

inlineToTextile _ LineBreak = return "\n"

inlineToTextile _ SoftBreak = return " "

inlineToTextile _ Space = return " "

inlineToTextile opts (Link (_, cls, _) txt (src, _)) = do
  label <- case txt of
                [Code _ s]
                 | s == src -> return "$"
                [Str s]
                 | s == src -> return "$"
                _           -> inlineListToTextile opts txt
  let classes = if null cls || cls == ["uri"] && label == "$"
                   then ""
                   else "(" <> T.unwords cls <> ")"
  return $ "\"" <> classes <> label <> "\":" <> src

inlineToTextile opts (Image attr@(_, cls, _) alt (source, tit)) = do
  alt' <- inlineListToTextile opts alt
  let txt = if T.null tit
               then if T.null alt'
                       then ""
                       else "(" <> alt' <> ")"
               else "(" <> tit <> ")"
      classes = if null cls
                   then ""
                   else "(" <> T.unwords cls <> ")"
      showDim dir = let toCss str = Just $ tshow dir <> ":" <> str <> ";"
                    in case dimension dir attr of
                         Just (Percent a) -> toCss $ tshow (Percent a)
                         Just dim         -> toCss $ showInPixel opts dim <> "px"
                         Nothing          -> Nothing
      styles = case (showDim Width, showDim Height) of
                 (Just w, Just h)   -> "{" <> w <> h <> "}"
                 (Just w, Nothing)  -> "{" <> w <> "height:auto;}"
                 (Nothing, Just h)  -> "{" <> "width:auto;" <> h <> "}"
                 (Nothing, Nothing) -> ""
  return $ "!" <> classes <> styles <> source <> txt <> "!"

inlineToTextile opts (Note contents) = do
  curNotes <- gets stNotes
  let newnum = length curNotes + 1
  contents' <- blockListToTextile opts contents
  let thisnote = "fn" <> tshow newnum <> ". " <> contents' <> "\n"
  modify $ \s -> s { stNotes = thisnote : curNotes }
  return $ "[" <> tshow newnum <> "]"
  -- note - may not work for notes with multiple blocks
