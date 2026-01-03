{-# LANGUAGE CPP  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.AsciiDoc
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to asciidoc.

Note that some information may be lost in conversion, due to
expressive limitations of asciidoc.  Footnotes and table cells with
paragraphs (or other block items) are not possible in asciidoc.
If pandoc encounters one of these, it will insert a message indicating
that it has omitted the construct.

AsciiDoc:  <http://www.methods.co.nz/asciidoc/>
-}
module Text.Pandoc.Writers.AsciiDoc (
  writeAsciiDoc,
  writeAsciiDocLegacy,
  writeAsciiDoctor
  ) where
import Control.Monad (foldM)
import Control.Monad.State.Strict
    ( StateT, MonadState(get), gets, modify, evalStateT )
import Data.Char (isPunctuation, isSpace)
#if MIN_VERSION_base(4,19,0)
import Data.List (delete, intercalate, intersperse, mapAccumL, uncons, sortOn, unsnoc)
#else
import Data.List (delete, intercalate, intersperse, mapAccumL, uncons, sortOn)
#endif
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, isJust, catMaybes)
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Network.URI (parseURI, URI(uriScheme))
import System.FilePath (dropExtension)
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (blankline, space)
import Text.DocLayout
import Text.Pandoc.Builder (emptyCell)
import Text.Pandoc.Shared
import Text.Pandoc.URI
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Walk (walk)

#if !MIN_VERSION_base(4,19,0)
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
#endif

data WriterState = WriterState { defListMarker       :: Text
                               , orderedListLevel    :: Int
                               , bulletListLevel     :: Int
                               , intraword           :: Bool
                               , autoIds             :: Set.Set Text
                               , legacy              :: Bool
                               , inList              :: Bool
                               , hasMath             :: Bool
                               -- |0 is no table
                               -- 1 is top level table
                               -- 2 is a table in a table
                               , tableNestingLevel   :: Int
                               }

defaultWriterState :: WriterState
defaultWriterState = WriterState { defListMarker      = "::"
                                 , orderedListLevel   = 0
                                 , bulletListLevel    = 0
                                 , intraword          = False
                                 , autoIds            = Set.empty
                                 , legacy             = False
                                 , inList             = False
                                 , hasMath            = False
                                 , tableNestingLevel  = 0
                                 }

-- | Convert Pandoc to AsciiDoc.
writeAsciiDoc :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeAsciiDoc opts document =
  evalStateT (pandocToAsciiDoc opts document) defaultWriterState

{-# DEPRECATED writeAsciiDoctor "Use writeAsciiDoc instead" #-}
-- | Deprecated synonym of 'writeAsciiDoc'.
writeAsciiDoctor :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeAsciiDoctor = writeAsciiDoc

-- | Convert Pandoc to legacy AsciiDoc.
writeAsciiDocLegacy :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeAsciiDocLegacy opts document =
  evalStateT (pandocToAsciiDoc opts document)
    defaultWriterState{ legacy = True }

type ADW = StateT WriterState

-- | Return asciidoc representation of document.
pandocToAsciiDoc :: PandocMonad m => WriterOptions -> Pandoc -> ADW m Text
pandocToAsciiDoc opts (Pandoc meta blocks) = do
  let titleblock = not $ null (docTitle meta) && null (docAuthors meta) &&
                         null (docDate meta)
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  metadata <- metaToContext opts
              (blockListToAsciiDoc opts)
              (fmap chomp . inlineListToAsciiDoc opts)
              meta
  main <- blockListToAsciiDoc opts $ makeSections False Nothing blocks
  st <- get
  let context  = defField "body" main
               $ defField "toc"
                  (writerTableOfContents opts &&
                   isJust (writerTemplate opts))
               $ defField "math" (hasMath st && not (legacy st))
               $ defField "titleblock" titleblock metadata
  return $ render colwidth $
    case writerTemplate opts of
       Nothing  -> main
       Just tpl -> renderTemplate tpl context

data EscContext = Normal | InTable
  deriving (Show, Eq)

-- | Escape special characters for AsciiDoc.
escapeString :: EscContext -> Text -> Doc Text
escapeString context t
  | T.any needsEscape t
  = literal $
      case T.foldl' go (False, mempty) t of
        (True, x) -> x <> "++" -- close passthrough context
        (False, x) -> x
  | otherwise = literal t
 where
  -- Bool is True when we are in a ++ passthrough context
  go :: (Bool, Text) -> Char -> (Bool, Text)
  go (True, x) '+' = (False, x <> "++" <> "{plus}") -- close context
  go (False, x) '+' = (False, x <> "{plus}")
  go (True, x) '|'
    | context == InTable = (False, x <> "++" <> "{vbar}") -- close context
  go (False, x) '|'
    | context == InTable = (False, x <> "{vbar}")
  go (True, x) c
    | needsEscape c = (True, T.snoc x c)
    | otherwise = (False, T.snoc (x <> "++") c)
  go (False, x) c
    | needsEscape c = (True, x <> "++" <> T.singleton c)
    | otherwise = (False, T.snoc x c)

  needsEscape '{' = True
  needsEscape '+' = True
  needsEscape '`' = True
  needsEscape '*' = True
  needsEscape '#' = True
  needsEscape '_' = True
  needsEscape '<' = True
  needsEscape '>' = True
  needsEscape '[' = True
  needsEscape ']' = True
  needsEscape '\\' = True
  needsEscape '|' = True
  needsEscape _ = False

-- | Ordered list start parser for use in Para below.
olMarker :: Parsec Text ParserState Char
olMarker = do (start, style', delim) <- anyOrderedListMarker
              if delim == Period &&
                          (style' == UpperAlpha || (style' == UpperRoman &&
                          start `elem` [1, 5, 10, 50, 100, 500, 1000]))
                          then spaceChar >> spaceChar
                          else spaceChar

-- | True if string begins with an ordered list marker
-- or would be interpreted as an AsciiDoc option command
needsEscaping :: Text -> Bool
needsEscaping s = beginsWithOrderedListMarker s || isBracketed s
  where
    beginsWithOrderedListMarker str =
      case runParser olMarker defaultParserState "para start" (T.take 10 str) of
             Left  _ -> False
             Right _ -> True
    isBracketed t
      | Just ('[', t') <- T.uncons t
      , Just (_, ']')  <- T.unsnoc t'
      = True
      | otherwise = False

-- | Convert Pandoc block element to asciidoc.
blockToAsciiDoc :: PandocMonad m
                => WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> ADW m (Doc Text)
blockToAsciiDoc opts (Div (id',"section":_,_)
                       (Header level (_,cls,kvs) ils : xs)) = do
  hdr <- blockToAsciiDoc opts (Header level (id',cls,kvs) ils)
  rest <- blockListToAsciiDoc opts xs
  return $ hdr $$ rest
blockToAsciiDoc opts (Plain inlines) = do
  contents <- inlineListToAsciiDoc opts inlines
  return $ contents <> blankline
blockToAsciiDoc opts (Para inlines) = do
  contents <- inlineListToAsciiDoc opts inlines
  -- escape if para starts with ordered list marker
  let esc = if needsEscaping (render Nothing contents)
               then text "{empty}"
               else empty
  return $ esc <> contents <> blankline
blockToAsciiDoc opts (LineBlock lns) = do
  let docify line = if null line
                    then return blankline
                    else inlineListToAsciiDoc opts line
  let joinWithLinefeeds = nowrap . mconcat . intersperse cr
  contents <- joinWithLinefeeds <$> mapM docify lns
  return $ "[verse]" $$ text "--" $$ contents $$ text "--" $$ blankline
blockToAsciiDoc _ b@(RawBlock f s)
  | f == "asciidoc" = return $ literal s
  | otherwise         = do
      report $ BlockNotRendered b
      return empty
blockToAsciiDoc _ HorizontalRule =
  return $ blankline <> text "'''''" <> blankline
blockToAsciiDoc opts (Header level (ident,_,_) inlines) = do
  contents <- inlineListToAsciiDoc opts inlines
  ids <- gets autoIds
  let autoId = uniqueIdent (writerExtensions opts) inlines ids
  modify $ \st -> st{ autoIds = Set.insert autoId ids }
  let identifier = if T.null ident ||
                      (isEnabled Ext_auto_identifiers opts && ident == autoId)
                      then empty
                      else "[[" <> literal ident <> "]]"
  return $ identifier $$
           nowrap (text (replicate (level + 1) '=') <> space <> contents) <>
           blankline
blockToAsciiDoc opts (Figure attr (Caption _ longcapt) body) = do
  -- Images in figures all get rendered as individual block-level images
  -- with the given caption. Non-image elements are rendered unchanged.
  capt <- if null longcapt
             then pure mempty
             else ("." <>) . nowrap <$>
                   inlineListToAsciiDoc opts (blocksToInlines longcapt)
  let renderFigElement = \case
        Plain [Image imgAttr alternate (src, tit)] -> do
          args <- imageArguments opts imgAttr alternate src tit
          let figAttributes = case attr of
                ("", _, _)    -> empty
                (ident, _, _) -> literal $ "[#" <> ident <> "]"
          -- .Figure caption
          -- image::images/logo.png[Company logo, title="blah"]
          return $
            capt $$
            figAttributes $$
            "image::" <> args <> blankline
        blk -> blockToAsciiDoc opts blk
  vcat <$> mapM renderFigElement body
blockToAsciiDoc _ (CodeBlock (_,classes,_) str) = return $ flush (
  if null classes
     then "...." $$ literal str $$ "...."
     else attrs $$ "----" $$ literal str $$ "----")
  <> blankline
    where attrs = "[" <> literal (T.intercalate "," classes') <> "]"
          classes' = if "numberLines" `elem` classes
                        then "source%linesnum" : delete "numberLines" classes
                        else "source" : classes
blockToAsciiDoc opts (BlockQuote blocks) = do
  contents <- blockListToAsciiDoc opts blocks
  let isBlock (BlockQuote _) = True
      isBlock _              = False
  -- if there are nested block quotes, put in an open block
  let contents' = if any isBlock blocks
                     then "--" $$ contents $$ "--"
                     else contents
  let bar = text "____"
  return $ bar $$ chomp contents' $$ bar <> blankline
blockToAsciiDoc opts block@(Table _ blkCapt specs thead@(TableHead _ originalHeaders) originalTbody tfoot@(TableFoot _ originalFooters)) = do
  let (caption, aligns, widths, _, _) =
        toLegacyTable blkCapt specs thead originalTbody tfoot
  let headers = adjustEmptyRows originalHeaders
  let rows = adjustEmptyRows $ tableBodiesToRows originalTbody
  let footers = adjustEmptyRows originalFooters
  caption' <- inlineListToAsciiDoc opts caption
  let caption'' = if null caption
                     then empty
                     else "." <> caption' <> cr
  let isSimple = all (== 0) widths
  let relativePercentWidths = if isSimple
                                 then widths
                                 else map (/ sum widths) widths
  let widths'' :: [Integer]
      widths'' = map (floor . (* 100)) relativePercentWidths
  -- ensure that the widths sum to 100
  let widths' = case widths'' of
                     _ | isSimple -> widths''
                     (w:ws) | sum (w:ws) < 100
                               -> (100 - sum ws) : ws
                     ws        -> ws
  let totalwidth :: Integer
      totalwidth = floor $ sum widths * 100
  let alignmentOperator AlignLeft     = "<"
      alignmentOperator AlignCenter   = "^"
      alignmentOperator AlignRight    = ">"
      alignmentOperator AlignDefault  = ""
  let colspec al wi = (alignmentOperator al) ++
                      if wi == 0 then "" else show wi ++ "%"
  let optionSpecForRows rowList spec = if allRowsEmpty rowList then Nothing else Just spec
  let headerspec = optionSpecForRows headers "header"
  let footerspec = optionSpecForRows footers "footer"
  let optionsList = catMaybes [headerspec, footerspec]
  let optionsspec = if null optionsList
                      then empty
                      else text "options=\"" <> text (intercalate "," optionsList) <> text "\","
  let widthspec = if totalwidth == 0
                     then empty
                     else text "width="
                          <> doubleQuotes (text $ show totalwidth ++ "%")
                          <> text ","
  let tablespec = text "["
         <> widthspec
         <> text "cols="
         <> doubleQuotes (text $ intercalate ","
             $ zipWith colspec aligns widths')
         <> text ","
         <> optionsspec <> text "]"

  -- construct cells and recurse in case of nested tables
  parentTableLevel <- gets tableNestingLevel
  let currentNestingLevel = parentTableLevel + 1

  modify $ \st -> st{ tableNestingLevel = currentNestingLevel }

  let separator = text (if parentTableLevel == 0
                          then "|"  -- top level separator
                          else "!") -- nested separator

  let makeCell [Plain x] = do d <- blockListToAsciiDoc opts [Plain x]
                              return $ separator <> chomp d
      makeCell [Para x]  = makeCell [Plain x]
      makeCell []        = return separator
      makeCell bs        = if currentNestingLevel == 2
                             then do
                               --asciidoc only supports nesting once
                               report $ BlockNotRendered block
                               return separator
                             else do
                               d <- blockListToAsciiDoc opts bs
                               return $ (text "a" <> separator) $$ d

  let colSpanFactor (ColSpan colSpan) = if colSpan > 1
                                          then text $ show colSpan
                                          else empty
  let rowSpanFactor (RowSpan rowSpan) = if rowSpan > 1
                                          then text $ "." ++ show rowSpan
                                          else empty

  let makeCellWithSpansAndAlignment (Cell _ alignment rowSpan colSpan blocks) = do
        let spanFactor = colSpanFactor colSpan <> rowSpanFactor rowSpan
        cell <- makeCell blocks
        let alignedCell = alignmentOperator alignment <> cell

        return $ if null spanFactor
          then alignedCell
          else spanFactor <> text "+" <> alignedCell

  let makeRow (Row attr []) = makeRow $ Row attr $ replicate (length widths') emptyCell
      makeRow (Row _ cells) = hsep `fmap` mapM makeCellWithSpansAndAlignment cells

  -- AsciiDoc only supports 1 header row and 1 footer row.
  let headerRow = Data.List.uncons $ adjustHeaders headers
  let footerRow = unsnoc $ adjustFooters footers
  let tailHeaderRows = if allRowsEmpty headers then [] else maybe [] snd headerRow
  let initFooterRows = if allRowsEmpty footers then [] else maybe [] fst footerRow
  rows' <- mapM makeRow $ tailHeaderRows ++ rows ++ initFooterRows
  head' <- case headerRow of
            Nothing -> return empty
            Just (headerRow', _) -> makeRow headerRow'
  foot <- case footerRow of
            Nothing -> return empty
            Just (_, footerRow') -> makeRow footerRow'
  modify $ \st -> st{ tableNestingLevel = parentTableLevel }
  let head'' = if allRowsEmpty headers then empty else head'
  let foot' = if allRowsEmpty footers then empty else foot
  let colwidth = if writerWrapText opts == WrapAuto
                    then writerColumns opts
                    else 100000
  let maxwidth = maximum $ fmap offset (foot <| (head' :| rows'))
  let body = if maxwidth > colwidth then vsep rows' else vcat rows'
  let border = separator <> text "==="
  return $
    caption'' $$ tablespec $$ border $$ head'' $$ body $$ foot' $$ border $$ blankline
blockToAsciiDoc opts (BulletList items) = do
  inlist <- gets inList
  modify $ \st -> st{ inList = True }
  contents <- mapM (bulletListItemToAsciiDoc opts) items
  modify $ \st -> st{ inList = inlist }
  return $ mconcat contents <> blankline
blockToAsciiDoc opts (OrderedList (start, sty, _delim) items) = do
  let listStyle = case sty of
                       DefaultStyle -> []
                       Decimal      -> ["arabic"]
                       Example      -> []
                       _            -> [T.toLower (tshow sty)]
  let listStart = ["start=" <> tshow start | start /= 1]
  let listoptions = case T.intercalate ", " (listStyle ++ listStart) of
                          "" -> empty
                          x  -> brackets (literal x)
  inlist <- gets inList
  modify $ \st -> st{ inList = True }
  contents <- mapM (orderedListItemToAsciiDoc opts) items
  modify $ \st -> st{ inList = inlist }
  return $ listoptions $$ mconcat contents <> blankline
blockToAsciiDoc opts (DefinitionList items) = do
  inlist <- gets inList
  modify $ \st -> st{ inList = True }
  contents <- mapM (definitionListItemToAsciiDoc opts) items
  modify $ \st -> st{ inList = inlist }
  return $ mconcat contents <> blankline

-- convert admonition and sidebar divs to asicidoc
blockToAsciiDoc opts (Div (ident,classes,_) bs) = do
  let identifier = if T.null ident then empty else "[[" <> literal ident <> "]]"
  let admonition_classes = ["attention","caution","danger","error","hint",
                     "important","note","tip","warning"]
  let sidebar_class = "sidebar"

  contents <-
       case classes of
         (l:_) | l `elem` admonition_classes || T.toLower l == sidebar_class -> do
             let (titleBs, bodyBs) =
                     case bs of
                       (Div (_,["title"],_) ts : rest) -> (ts, rest)
                       _ -> ([], bs)
             let fence = if l == "sidebar" then "****" else "===="
             elemTitle <- if null titleBs ||
                                   -- If title matches class, omit
                                   (T.toLower (T.strip (stringify titleBs))) == l
                                   then return mempty
                                   else ("." <>) <$>
                                         blockListToAsciiDoc opts titleBs
             elemBody <- blockListToAsciiDoc opts bodyBs
             return $ "[" <> literal (T.toUpper l) <> "]" $$
                      chomp elemTitle $$
                      fence $$
                      chomp elemBody $$
                      fence
         _ -> blockListToAsciiDoc opts bs
  return $ identifier $$ contents $$ blankline

-- | Convert bullet list item (list of blocks) to asciidoc.
bulletListItemToAsciiDoc :: PandocMonad m
                         => WriterOptions -> [Block] -> ADW m (Doc Text)
bulletListItemToAsciiDoc opts blocks = do
  lev <- gets bulletListLevel
  modify $ \s -> s{ bulletListLevel = lev + 1 }
  isLegacy <- gets legacy
  let blocksWithTasks = if isLegacy
                          then blocks
                          else (taskListItemToAsciiDoc blocks)
  contents <- snd <$> foldM (addBlock opts) (False, empty) blocksWithTasks
  modify $ \s -> s{ bulletListLevel = lev }
  let marker = text (replicate (lev + 1) '*')
  return $ marker <> text " " <> listBegin blocksWithTasks <>
    contents <> cr

-- | Convert a list item containing text starting with @U+2610 BALLOT BOX@
-- or @U+2612 BALLOT BOX WITH X@ to asciidoctor checkbox syntax (e.g. @[x]@).
taskListItemToAsciiDoc :: [Block] -> [Block]
taskListItemToAsciiDoc = handleTaskListItem toAd listExt
  where
    toAd (Str "☐" : Space : is) = RawInline (Format "asciidoc") "[ ]" : Space : is
    toAd (Str "☒" : Space : is) = RawInline (Format "asciidoc") "[x]" : Space : is
    toAd is = is
    listExt = extensionsFromList [Ext_task_lists]

addBlock :: PandocMonad m
         => WriterOptions -> (Bool, Doc Text) -> Block -> ADW m (Bool, Doc Text)
addBlock opts (containsContinuation, d) b = do
  x <- chomp <$> blockToAsciiDoc opts b
  return $
    case b of
        BulletList{}
          | containsContinuation -> (False, d <> blankline <> x)  -- see #11006
          | otherwise -> (False, d <> cr <> x)
        OrderedList (start, sty, _) _
          | containsContinuation
          , start == 1
          , sty == DefaultStyle -> (False, d <> blankline <> x)  -- see #11006
          | otherwise -> (False, d <> cr <> x)
        Para (Math DisplayMath _:_) -> (containsContinuation, d <> cr <> x)
        Plain (Math DisplayMath _:_) -> (containsContinuation, d <> cr <> x)
        Para{} | isEmpty d -> (containsContinuation, x)
        Plain{} | isEmpty d -> (containsContinuation, x)
        _ -> (True, d <> cr <> text "+" <> cr <> x)

listBegin :: [Block] -> Doc Text
listBegin blocks =
        case blocks of
          Para (Math DisplayMath _:_) : _  -> "{blank}"
          Plain (Math DisplayMath _:_) : _ -> "{blank}"
          Para _ : _                       -> empty
          Plain _ : _                      -> empty
          _ : _                            -> "{blank}"
          []                               -> "{blank}"

-- | Convert ordered list item (a list of blocks) to asciidoc.
orderedListItemToAsciiDoc :: PandocMonad m
                          => WriterOptions -- ^ options
                          -> [Block]       -- ^ list item (list of blocks)
                          -> ADW m (Doc Text)
orderedListItemToAsciiDoc opts blocks = do
  lev <- gets orderedListLevel
  modify $ \s -> s{ orderedListLevel = lev + 1 }
  contents <- snd <$> foldM (addBlock opts) (False, empty) blocks
  modify $ \s -> s{ orderedListLevel = lev }
  let marker = text (replicate (lev + 1) '.')
  return $ marker <> text " " <> listBegin blocks <> contents <> cr

-- | Convert definition list item (label, list of blocks) to asciidoc.
definitionListItemToAsciiDoc :: PandocMonad m
                             => WriterOptions
                             -> ([Inline],[[Block]])
                             -> ADW m (Doc Text)
definitionListItemToAsciiDoc opts (label, defs) = do
  labelText <- inlineListToAsciiDoc opts label
  marker <- gets defListMarker
  if marker == "::"
     then modify (\st -> st{ defListMarker = ";;"})
     else modify (\st -> st{ defListMarker = "::"})
  let divider = cr <> text "+" <> cr
  let defsToAsciiDoc :: PandocMonad m => [Block] -> ADW m (Doc Text)
      defsToAsciiDoc ds = (vcat . intersperse divider . map chomp)
           `fmap` mapM (blockToAsciiDoc opts) ds
  defs' <- mapM defsToAsciiDoc defs
  modify (\st -> st{ defListMarker = marker })
  let contents = nest 2 $ vcat $ intersperse divider $ map chomp defs'
  return $ labelText <> literal marker <> cr <> contents <> cr

-- | Convert list of Pandoc block elements to asciidoc.
blockListToAsciiDoc :: PandocMonad m
                    => WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> ADW m (Doc Text)
blockListToAsciiDoc opts blocks =
  mconcat `fmap` mapM (blockToAsciiDoc opts) blocks

data SpacyLocation = End | Start

-- | Convert list of Pandoc inline elements to asciidoc.
inlineListToAsciiDoc :: PandocMonad m =>
                        WriterOptions ->
                        [Inline] ->
                        ADW m (Doc Text)
inlineListToAsciiDoc opts lst = do
  oldIntraword <- gets intraword
  setIntraword False
  result <- go lst
  setIntraword oldIntraword
  return result
 where go [] = return empty
       go (y:x:xs)
         | not (isSpacy End y) = do
           y' <- if isSpacy Start x
                    then inlineToAsciiDoc opts y
                    else withIntraword $ inlineToAsciiDoc opts y
           x' <- withIntraword $ inlineToAsciiDoc opts x
           xs' <- go xs
           return (y' <> x' <> xs')
         | not (isSpacy Start x) = do
           y' <- withIntraword $ inlineToAsciiDoc opts y
           xs' <- go (x:xs)
           return (y' <> xs')
       go (x:xs) = do
           x' <- inlineToAsciiDoc opts x
           xs' <- go xs
           return (x' <> xs')
       isSpacy :: SpacyLocation -> Inline -> Bool
       isSpacy _ Space = True
       isSpacy _ LineBreak = True
       isSpacy _ SoftBreak = True
       -- Note that \W characters count as spacy in AsciiDoc
       -- for purposes of determining interword:
       isSpacy End (Str xs) = case T.unsnoc xs of
                                   Just (_, c) -> isPunctuation c || isSpace c
                                   _           -> False
       isSpacy Start (Str xs)
         | Just (c, _) <- T.uncons xs = isPunctuation c || isSpace c
       isSpacy End (Link{}) = True
       isSpacy End (Image{}) = True
       isSpacy _ _ = False

setIntraword :: PandocMonad m => Bool -> ADW m ()
setIntraword b = modify $ \st -> st{ intraword = b }

withIntraword :: PandocMonad m => ADW m a -> ADW m a
withIntraword p = setIntraword True *> p <* setIntraword False

-- | Convert Pandoc inline element to asciidoc.
inlineToAsciiDoc :: PandocMonad m => WriterOptions -> Inline -> ADW m (Doc Text)
inlineToAsciiDoc opts (Emph [Strong xs]) =
  inlineToAsciiDoc opts (Strong [Emph xs])  -- see #5565
inlineToAsciiDoc opts (Emph lst) = do
  contents <- inlineListToAsciiDoc opts lst
  isIntraword <- gets intraword
  let marker = if isIntraword then "__" else "_"
  return $ marker <> contents <> marker
inlineToAsciiDoc opts (Underline lst) = do
  contents <- inlineListToAsciiDoc opts lst
  return $ "[.underline]#" <> contents <> "#"
inlineToAsciiDoc opts (Strong lst) = do
  contents <- inlineListToAsciiDoc opts lst
  isIntraword <- gets intraword
  let marker = if isIntraword then "**" else "*"
  return $ marker <> contents <> marker
inlineToAsciiDoc opts (Strikeout lst) = do
  contents <- inlineListToAsciiDoc opts lst
  return $ "[line-through]#" <> contents <> "#"
inlineToAsciiDoc opts (Superscript lst) = do
  contents <- inlineListToAsciiDoc opts lst
  return $ "^" <> contents <> "^"
inlineToAsciiDoc opts (Subscript lst) = do
  contents <- inlineListToAsciiDoc opts lst
  return $ "~" <> contents <> "~"
inlineToAsciiDoc opts (SmallCaps lst) = do
  contents <- inlineListToAsciiDoc opts lst
  return $ "[smallcaps]#" <> contents <> "#"
inlineToAsciiDoc opts (Quoted qt lst) = do
  isLegacy <- gets legacy
  contents <- inlineListToAsciiDoc opts lst
  pure $ case qt of
    SingleQuote
      | isLegacy     -> "`" <> contents <> "'"
      | otherwise    -> "'`" <> contents <> "`'"
    DoubleQuote
      | isLegacy     -> "``" <> contents <> "''"
      | otherwise    -> "\"`" <> contents <> "`\""
inlineToAsciiDoc _ (Code _ str) = do
  isLegacy <- gets legacy
  let escChar '`' = "\\'"
      escChar c   = T.singleton c
  parentTableLevel <- gets tableNestingLevel
  let content
       | isLegacy = literal (T.concatMap escChar str)
       | otherwise = escapeString
                       (if parentTableLevel > 0 then InTable else Normal) str
  return $ text "`" <> content <> "`"
inlineToAsciiDoc _ (Str str) = do
  parentTableLevel <- gets tableNestingLevel
  pure $ escapeString (if parentTableLevel > 0 then InTable else Normal) str
inlineToAsciiDoc _ (Math InlineMath str) = do
  isLegacy <- gets legacy
  modify $ \st -> st{ hasMath = True }
  let content = if isLegacy
                then "$" <> literal str <> "$"
                else literal str
  return $ "latexmath:[" <> content <> "]"
inlineToAsciiDoc _ (Math DisplayMath str) = do
  isLegacy <- gets legacy
  modify $ \st -> st{ hasMath = True }
  let content = if isLegacy
                   then "\\[" <> literal str <> "\\]"
                   else literal str
  inlist <- gets inList
  let sepline = if inlist
                   then text "+"
                   else blankline
  return $
      (cr <> sepline) $$ "[latexmath]" $$ "++++" $$
      content $$ "++++" <> cr
inlineToAsciiDoc _ il@(RawInline f s)
  | f == "asciidoc" = return $ literal s
  | otherwise         = do
      report $ InlineNotRendered il
      return empty
inlineToAsciiDoc _ LineBreak = return $ " +" <> cr
inlineToAsciiDoc _ Space = return space
inlineToAsciiDoc opts SoftBreak =
  case writerWrapText opts of
       WrapAuto     -> return space
       WrapPreserve -> return cr
       WrapNone     -> return space
inlineToAsciiDoc opts (Cite _ lst) = inlineListToAsciiDoc opts lst
inlineToAsciiDoc opts (Link _ txt (src, _tit)) = do
-- relative:  link:downloads/foo.zip[download foo.zip]
-- abs:  http://google.cod[Google]
-- or my@email.com[email john]
  let fixCommas (Str t) =
        intersperse (RawInline (Format "asciidoc") "&#44;")
          $ map Str $ T.splitOn "," t -- see #8070
      fixCommas x = [x]

  linktext <- inlineListToAsciiDoc opts $ walk (concatMap fixCommas) txt
  let needsLinkPrefix = case parseURI (T.unpack src) of
                          Just u -> uriScheme u `notElem` ["http:","https:",
                                                           "ftp:", "irc:",
                                                            "mailto:"]
                          _ -> True
  let needsPassthrough = "--" `T.isInfixOf` src
  let prefix = if needsLinkPrefix
                  then text "link:"
                  else empty
  let srcSuffix = fromMaybe src (T.stripPrefix "mailto:" src)
  let useAuto = case txt of
                      [Str s] | escapeURI s == srcSuffix -> True
                      _       -> False
  return $
    if needsPassthrough
       then
         if useAuto
            then "link:++" <> literal srcSuffix <> "++[]"
            else "link:++" <> literal src <> "++[" <> linktext <> "]"
       else
         if useAuto
            then literal srcSuffix
            else prefix <> literal src <> "[" <> linktext <> "]"
inlineToAsciiDoc opts (Image attr alternate (src, tit)) =
  ("image:" <>) <$> imageArguments opts attr alternate src tit
inlineToAsciiDoc opts (Note [Para inlines]) =
  inlineToAsciiDoc opts (Note [Plain inlines])
inlineToAsciiDoc opts (Note [Plain inlines]) = do
  contents  <- inlineListToAsciiDoc opts inlines
  return $ text "footnote:[" <> contents <> "]"
-- asciidoc can't handle blank lines in notes
inlineToAsciiDoc _ (Note _) = return "[multiblock footnote omitted]"
inlineToAsciiDoc opts (Span (ident,classes,_) ils) = do
  contents <- inlineListToAsciiDoc opts ils
  isIntraword <- gets intraword
  let marker = if isIntraword then "##" else "#"
  case classes of
    [] | T.null ident -> return contents
    ["mark"] | T.null ident -> return $ marker <> contents <> marker
    _ -> do
       let modifier = brackets $ literal $ T.unwords $
            [ "#" <> ident | not (T.null ident)] ++ map ("." <>) classes
       return $ modifier <> marker <> contents <> marker

-- | Provides the arguments for both `image:` and `image::`
-- e.g.: sunset.jpg[Sunset,300,200]
imageArguments :: PandocMonad m => WriterOptions ->
  Attr -> [Inline] -> Text -> Text ->
  ADW m (Doc Text)
imageArguments opts attr altText src title = do
  let txt = if null altText || (altText == [Str ""])
               then [Str . T.pack . dropExtension $ T.unpack src]
               else altText
  linktext <- inlineListToAsciiDoc opts txt
  let linktitle = if T.null title
                     then empty
                     else ",title=\"" <> literal title <> "\""
      showDim dir = case dimension dir attr of
                      Just (Percent a) ->
                        ["scaledwidth=" <> text (show (Percent a))]
                      Just dim         ->
                        [text (show dir) <> "=" <>
                          literal (showInPixel opts dim)]
                      Nothing          ->
                        []
      dimList = showDim Width ++ showDim Height
      dims = if null dimList
                then empty
                else "," <> mconcat (intersperse "," dimList)
  return $ literal src <> "[" <> linktext <> linktitle <> dims <> "]"

-- | Adjust header rows for the fact that AsciiDoc only supports a single header row.
--
-- The first header row will become the single header row in AsciiDoc with the
-- other rows becoming the top body rows.
-- All cells of the first header row with a RowSpan > 1 will be mapped to
-- RowSpan 1 and the remaining RowSpans of those cells wll be added as empty
-- columns into the second row beneath them to preserve the original layout.
adjustHeaders :: [Row] -> [Row]
adjustHeaders [] = []
adjustHeaders [row] = [row]
adjustHeaders (Row attr firstHeaderCells:secondRow:remainingRows) =
  let ((_, emptyHeaderCells), headerRow) = mapAccumL adjustHeaderRowCell (0, []) firstHeaderCells
      secondRow' = applyEmptyCells secondRow emptyHeaderCells
  in  Row attr headerRow:secondRow':remainingRows
 where
  adjustHeaderRowCell (columnPosition, emptyCells) cell@(Cell cellAttr alignment (RowSpan rowSpan) (ColSpan colSpan) blocks) =
    let nextColumnPosition = columnPosition + colSpan
        adjustedHeaderCell = Cell cellAttr alignment (RowSpan 1) (ColSpan colSpan) blocks
        emptyHeaderRowCell = Cell nullAttr AlignDefault (RowSpan rowSpan - 1) (ColSpan colSpan) []
        emptyCellPosition = (columnPosition, emptyHeaderRowCell)
    in  if rowSpan > 1
          then ((nextColumnPosition, emptyCellPosition:emptyCells), adjustedHeaderCell)
          else ((nextColumnPosition, emptyCells), cell)

-- | Adjust footer rows for the fact that AsciiDoc only supports a single footer row.
--
-- The last footer row will become the single footer row in AsciiDoc with the
-- previous footer rows becoming the bottom body rows.
-- All column indices of cells whose RowSpans would reach that last footer row
-- are collected and subtracted by 1. Those collected column indices will then
-- be applied as empty columns into the last footer row to preserve the original
-- layout.
adjustFooters :: [Row] -> [Row]
adjustFooters [] = []
adjustFooters [row] = [row]
adjustFooters rows = adjustFooters' [] (0, length rows) M.empty rows
 where
  adjustFooters' _ _ _ [] = []
  adjustFooters' columnIndices _ _ [row] = [applyEmptyCells row columnIndices]
  adjustFooters' columnIndices rowInfo@(rowIndex, footerLength) previousRowSpans (row:rest) =
    -- Need to keep track of RowSpans from previous rows and how they occupy
    -- space in rows beneath them to be able to apply the correct column
    -- position of RowSpans that would reach the last footer row.
    let (previousRowSpans', row', columnIndices') = adjustFooterRow rowInfo previousRowSpans row
        rows' = adjustFooters' (columnIndices ++ columnIndices') (rowIndex + 1, footerLength) previousRowSpans' rest
    in  row':rows'

  adjustFooterRow rowInfo previousRowSpans (Row attr cells) =
    let ((nextColumnPosition, previousRowSpans'), cells') = mapAccumL (adjustFooterCell rowInfo) (0, previousRowSpans) cells
        (cells'', columnIndices) = unzip cells'

        -- Apply row spans from a previous row that are next to the end of the
        -- current row's cells to keep track of the correct column position.
        previousRowSpans'' = decrementTrailingRowSpans nextColumnPosition previousRowSpans'
    in  (previousRowSpans'', Row attr cells'', catMaybes columnIndices)

-- | Adjust footer cell for the fact that AsciiDoc only supports a single footer row.
--
-- Collects cells whose RowSpan would reach to the last footer row and applies
-- them as empty cells to that last footer row.
adjustFooterCell :: (Int, Int) -> (Int, M.Map Int (RowSpan, ColSpan)) -> Cell -> ((Int, M.Map Int (RowSpan, ColSpan)), (Cell, Maybe (Int, Cell)))
adjustFooterCell rowInfo@(rowIndex, footerLength) (columnPosition, previousSpans) cell@(Cell _ _ (RowSpan rowSpan) (ColSpan colSpan) _)
  | Just (ColSpan previousColSpan, previousSpans') <- takePreviousSpansAtColumn columnPosition previousSpans =
      -- Apply row span from a previous row that occupies this column to keep
      -- track of the correct column position.
      adjustFooterCell rowInfo (columnPosition + previousColSpan, previousSpans') cell
  | rowSpan > 1 && rowIndex + rowSpan >= footerLength =
      -- Adjust row span that would reach all the way to the last footer row and
      -- keep track of that to apply it to the last footer row.
      ((nextColumnPosition, previousRowSpans'), (decrementRowSpanInCell cell, Just (columnPosition, emptyCellWithColSpan)))
  | otherwise = ((nextColumnPosition, previousRowSpans'), (cell, Nothing))
 where
  -- Keep track of this cell's RowSpan for the rows following it.
  previousRowSpans' = insertCurrentSpansAtColumn columnPosition previousSpans (RowSpan rowSpan) (ColSpan colSpan)
  nextColumnPosition = columnPosition + colSpan
  emptyCellWithColSpan = Cell nullAttr AlignDefault (RowSpan 1) (ColSpan colSpan) []

-- | Adjust empty rows for AsciiDoc.
--
-- An empty row without any cells decrements RowSpans that cover it and is
-- removed by them to adjust for being unable to express empty rows with no
-- cells in AsciiDoc.
adjustEmptyRows :: [Row] -> [Row]
adjustEmptyRows = adjustEmptyRows' . map applyInitialRowsLeft
 where
  adjustEmptyRows' [] = []
  adjustEmptyRows' (row:rest)
    | maxRowSpan' <- maxRowSpan row
    , maxRowSpan' > 1 =
        -- Consume empty rows within the row's span.
        let followingRows = take (maxRowSpan' - 1) rest
            rows = consumeEmptyRows (row :| []) followingRows
            rest' = drop (length followingRows) rest
        in  rowFromCellsWithRowsLeft (NonEmpty.head rows) : adjustEmptyRows' (NonEmpty.tail rows ++ rest')
    | otherwise = rowFromCellsWithRowsLeft row : adjustEmptyRows' rest

  rowFromCellsWithRowsLeft (attr, cellsWithRowsLeft) = Row attr $ map fst cellsWithRowsLeft
  cellRowSpan (Cell _ _ (RowSpan rowSpan) _ _) = rowSpan

  consumeEmptyRows rows [] = NonEmpty.reverse rows
  consumeEmptyRows rows (followingRow:restRows) =
    if null (snd followingRow) && any rowHasRowSpanAndRowsLeft rows
      then consumeEmptyRows (fmap (subtractRowsLeft decrementRowSpanInCell) rows) restRows -- Consume empty row for RowSpan and remove it
      else consumeEmptyRows (followingRow <| fmap (subtractRowsLeft id) rows) restRows

  rowHasRowSpanAndRowsLeft (_, cells) = any cellHasRowSpanAndRowsLeft cells
  cellHasRowSpanAndRowsLeft (cell, rowsLeft) = cellRowSpan cell > 1 && rowsLeft >= 1

  subtractRowsLeft changeCell (attr, cells) = (attr, map (subtractRowsLeftCell changeCell) cells)

  subtractRowsLeftCell changeCell cellPair@(cell, rowsLeft)
    | rowsLeft >= 1 = (changeCell cell, rowsLeft - 1)
    | otherwise = cellPair

  applyInitialRowsLeft (Row attr cells) = (attr, map applyInitialRowsLeftCell cells)

  applyInitialRowsLeftCell cell
    | rowSpan <- cellRowSpan cell, rowSpan > 1 = (cell, rowSpan - 1) -- Minus its own row
    | otherwise = (cell, 0)

  maxRowSpan (_, []) = 0
  maxRowSpan (_, cells) = maximum $ map (cellRowSpan . fst) cells

-- | Decrement the RowSpan of a Cell if that RowSpan > 1.
decrementRowSpanInCell :: Cell -> Cell
decrementRowSpanInCell cell@(Cell attr alignment (RowSpan rowSpan) colSpan blocks) =
  if rowSpan > 1
    then Cell attr alignment (RowSpan rowSpan - 1) colSpan blocks
    else cell

-- | Apply empty table cells at the given positions inside a Row.
applyEmptyCells :: Row -> [(Int, Cell)] -> Row
applyEmptyCells (Row attr cells) = Row attr . applyEmptyCells' 0 cells . sortOn fst
 where
  applyEmptyCells' _  cells' [] = cells'
  applyEmptyCells' currentPosition cells' ((columnPosition, columnEmptyCell@(Cell _ _ _ (ColSpan colSpan) _)):rest)
    | columnPosition == currentPosition = columnEmptyCell : applyEmptyCells' (currentPosition + colSpan) cells' rest
  applyEmptyCells' _ [] _ = []
  applyEmptyCells' currentPosition (cell@(Cell _ _ _ (ColSpan currentCellColSpan) _):restCells) emptyCellList =
    cell : applyEmptyCells' (currentPosition + currentCellColSpan) restCells emptyCellList
