{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Text.Pandoc.Writers.Vimdoc (writeVimdoc) where

import Control.Applicative (optional, (<|>))
import Control.Monad (forM)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State (MonadState (..), StateT, evalStateT, gets, modify)
import Data.Default (Default (..))
import Data.List (intercalate, intersperse, transpose)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.DocLayout hiding (char, link, text)
import Text.Pandoc.Class.PandocMonad ( report, PandocMonad )
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Logging (LogMessage (..))
import Text.Pandoc.Options (WrapOption (..), WriterOptions (..))
import Text.Pandoc.Parsing.General (many1Till, many1TillChar, readWith)
import Text.Pandoc.Shared (capitalize, onlySimpleTableCells, orderedListMarkers, isTightList, makeSections, removeFormatting, tshow)
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.URI (escapeURI, isURI)
import Text.Pandoc.Writers.Shared (defField, metaToContext, toLegacyTable)
import Text.Parsec (anyChar, char, eof, string, try)
import Text.Read (readMaybe)
import Text.Pandoc.Chunks (toTOCTree, SecInfo (..))
import Data.Tree (Tree(..))
import Data.Functor ((<&>))
import Data.Sequence (Seq, (|>), (<|))
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

data WriterState = WriterState
  { indentLevel :: Int -- How much to indent the block. Inlines shouldn't
                       -- be concerned with indent level (I guess?)
  , shiftWidth :: Int -- spaces per indentation level
  , writerOptions :: WriterOptions
  , vimdocPrefix :: Maybe Text
  }

instance Default WriterState where
  def =
    WriterState
      { indentLevel = 0
      , shiftWidth = 4
      , writerOptions = def
      , vimdocPrefix = Nothing
      }

indent :: (Monad m) => Int -> (VW m a) -> (VW m a)
indent n = local (\s -> s{indentLevel = indentLevel s + n})

type VW m = StateT (Seq (Doc Text)) (ReaderT WriterState m)

runRR :: (Monad m) => Seq (Doc Text) -> WriterState -> VW m a -> m a
runRR footnotes opts action = runReaderT (evalStateT action footnotes) opts

docShiftWidth :: Meta -> Maybe Int
docShiftWidth meta = case lookupMeta "shiftwidth" meta of
  Just (MetaInlines [Str sw]) -> readMaybe (T.unpack sw)
  Just (MetaString sw) -> readMaybe (T.unpack sw)
  _ -> Nothing

docVimdocPrefix :: Meta -> Maybe Text
docVimdocPrefix meta = case lookupMeta "vimdoc-prefix" meta of
  Just (MetaInlines [Str pref]) -> Just pref
  Just (MetaString pref) -> Just pref
  _ -> Nothing

{- | Build a vim modeline
>>> makeModeLine def
"vim:tw=72:sw=4:ts=4:ft=help:norl:et:"
-}
makeModeLine :: WriterState -> Text
makeModeLine ws =
  T.pack . intercalate ":" $
    [ "vim"
    , "tw=" <> show tw
    , "sw=" <> show sw
    , "ts=" <> show sw
    , "ft=help"
    , "norl" -- left-to-right text
    , "et:" -- expandtab and finishing ":"
    ]
 where
  tw = writerColumns . writerOptions $ ws
  sw = shiftWidth ws

-- | Build a single formatted TOC line
tocEntryToLine :: (PandocMonad m) => SecInfo -> VW m Text
tocEntryToLine secinfo = do
  rightRef <- mkVimdocRef (secId secinfo)
  let numberStr = case secNumber secinfo of
        Nothing -> ""
        Just x | '.' `T.elem` x -> x <> " "
        Just x -> x <> ". "
  title <- inlineListToVimdoc $ removeFormatting (secTitle secinfo)
  let titlePlain = render Nothing (title <> " ")

  -- length sub 2 because vertical bars are concealed
  let rightRefLen = max 0 (T.length rightRef - 2)
  let numberLen = T.length numberStr
  let leftLen = numberLen + T.length titlePlain
  let padForRight = 1
  textWidth <- asks (writerColumns . writerOptions)
  il <- asks indentLevel

  -- positive when we lack space (i.e. content is too long)
  let lack = (il + leftLen + padForRight + rightRefLen) - textWidth

  -- when lacking, truncate title reserving 3+ chars for ellipsis
  let finalTitle =
        if lack >= 0
          then
            let trunc = T.dropEnd (lack + 3) titlePlain
                stripped = T.stripEnd trunc
                ellipsis =
                  T.replicate (3 + T.length trunc - T.length stripped) "."
             in stripped <> ellipsis
          else titlePlain

  -- Negative lack means we have an excess of space, so we fill it with dots
  let dots = T.replicate (negate lack) "."

  pure . T.concat $ [numberStr, finalTitle, dots, " ", rightRef]

vimdocTOC :: (PandocMonad m) => WriterState -> [Block] -> VW m (Doc Text)
vimdocTOC (WriterState{writerOptions = opts}) blocks = do
  let (Node _ subtrees) =
        toTOCTree $ makeSections (writerNumberSections opts) Nothing blocks
  let tocDepth = writerTOCDepth opts
  let isBelowTocDepth (Node sec _) = secLevel sec <= tocDepth

  let makeItem :: (PandocMonad m) => Tree SecInfo -> VW m (Doc Text)
      makeItem (Node secinfo xs) = do
        line <- tocEntryToLine secinfo
        -- When unnumbered, indent constantly by two,
        -- otherwise indent by (length of marker + 1)
        let markerLen = 1 + maybe 1 T.length (secNumber secinfo)
        childItems <-
          indent markerLen $
            traverse makeItem (filter isBelowTocDepth xs)
        pure (literal line $$ nest markerLen (vcat childItems))

  items <- traverse makeItem (filter isBelowTocDepth subtrees)
  pure $ vcat items

writeVimdoc :: (PandocMonad m) => WriterOptions -> Pandoc -> m Text
writeVimdoc opts document@(Pandoc meta _) =
  let
    sw = fromMaybe (shiftWidth def) $ docShiftWidth meta
    vp = docVimdocPrefix meta
    footnotes = Seq.empty
    initialEnv = def{shiftWidth = sw, writerOptions = opts, vimdocPrefix = vp}
   in
    runRR footnotes initialEnv $ pandocToVimdoc document

pandocToVimdoc :: (PandocMonad m) => Pandoc -> VW m Text
pandocToVimdoc (Pandoc meta body) = do
  st <- ask
  let opts = writerOptions st

  metadata <- metaToContext opts blockListToVimdoc inlineListToVimdoc meta
  main <- do
    body' <- blockListToVimdoc body
    footnotes <- get
    rule <- blockToVimdoc HorizontalRule
    let footnotes' = if Seq.null footnotes
          then Empty
          else vsep (toList $ rule <| footnotes)
    pure $ body' <> blankline <> footnotes'

  title <- inlineListToVimdoc $ docTitle meta
  authors <- traverse inlineListToVimdoc $ docAuthors meta
  let authors' = mconcat $ intersperse ("," <> space) (fmap nowrap authors)
  let tw = writerColumns . writerOptions $ st

  let combinedTitle =
        render (Just tw) . cblock tw $
            (title <> space)
              <> (if null authors' then "" else "by" <> space <> authors')

  -- This is placed here because I couldn't find a way to right-align text
  -- inside template to the width specified by a variable
  let toc_reminder =
        render Nothing . rblock tw $
          ("Type |gO| to see the table of contents." :: Doc Text)

  toc <- render (Just tw) <$> vimdocTOC st body

  let modeline = makeModeLine st
  let context =
        defField "body" main
          . defField "toc" (if writerTableOfContents opts then toc else "")
          . defField "modeline" modeline
          . defField "combined-title" combinedTitle
          . defField "toc-reminder" toc_reminder
          $ metadata

  pure $
    case writerTemplate opts of
      Just tpl -> render (Just tw) $ renderTemplate tpl context
      Nothing -> render (Just tw) main

blockListToVimdoc :: (PandocMonad m) => [Block] -> VW m (Doc Text)
blockListToVimdoc blocks = vcat <$> mapM blockToVimdoc blocks

blockToVimdoc :: (PandocMonad m) => Block -> VW m (Doc Text)

blockToVimdoc (Plain inlines) = inlineListToVimdoc inlines

blockToVimdoc (Para inlines) = do
  contents <- inlineListToVimdoc inlines
  pure $ contents <> blankline

blockToVimdoc (LineBlock inliness) = vcat <$> mapM inlineListToVimdoc inliness

blockToVimdoc (CodeBlock (_, cls, _) code) = do
  sw <- asks shiftWidth
  let lang = case cls of
        (lang' : _) -> lang'
        _ -> ""
  -- NOTE: No blankline after the codeblock because closing `<` is concealed
  pure . vcat $
    [ ">" <> literal lang
    , nest sw (literal code)
    , flush "<"
    ]

blockToVimdoc block@(RawBlock format raw) = case format of
  "vimdoc" -> pure $ literal raw
  _ -> "" <$ report (BlockNotRendered block)

blockToVimdoc (BlockQuote blocks) = do
  content <- blockListToVimdoc blocks
  pure $ nest 2 content <> blankline

blockToVimdoc (OrderedList listAttr items) = do
  let itemSpacer = if isTightList items then empty else blankline
  let itemsWithMarkers = zip (orderedListMarkers listAttr) items
  items' <- forM itemsWithMarkers $ \(marker, blocks) -> do
    let markerLen = T.length marker

    item' <- indent (markerLen + 1) $ blockListToVimdoc blocks
    pure $ literal marker <> space <> nest (markerLen + 1) item' <> itemSpacer
  pure $ vcat items' <> blankline

blockToVimdoc (BulletList items) = do
  let itemSpacer = if isTightList items then empty else blankline
  items' <- forM items $ \blocks -> do
    let marker = "-"
    item <- indent 2 $ blockListToVimdoc blocks
    pure $ marker <> " " <> nest 2 item <> itemSpacer
  pure $ vcat items' <> blankline

blockToVimdoc (DefinitionList items) = do
  sw <- asks shiftWidth
  let sepAll = if all (isTightList . snd) items then vcat else vsep
  items' <- forM items $ \(term, definitions) -> do
    let sepCur = if isTightList definitions then vcat else vsep
    labeledTerm <- mkVimdocDefinitionTerm term
    definitions' <- indent sw $ traverse blockListToVimdoc definitions
    pure $ labeledTerm <> cr <> nest sw (sepCur definitions')
  pure $ sepAll items' <> blankline

blockToVimdoc (Header level (ref, _, _) inlines) = do
  tw <- asks (writerColumns . writerOptions)
  let rule = case level of
        1 -> T.replicate tw "="
        2 -> T.replicate tw "-"
        _ -> ""
  title <- fmap (render Nothing) . inlineListToVimdoc $ case level of
    3 -> capitalize inlines
    _ -> inlines

  label <- mkVimdocTag ref
  -- One manual space that ensures that even if spaceLeft is 0, title and ref
  -- don't touch each other
  let label' = " " <> label
  -- (+ 2) due to stars concealment
  let spaceLeft = tw - T.length title + 2

  pure $ vcat
      [ blankline
      , literal rule
      , literal $ title <> T.justifyRight spaceLeft ' ' label'
      , blankline
      ]

blockToVimdoc HorizontalRule = do
  tw <- asks (writerColumns . writerOptions)
  pure $ literal (T.replicate (tw `div` 2) " *") <> blankline

-- Based on blockToMarkdown' from Text.Pandoc.Writers.Markdown
blockToVimdoc t@(Table (_, _, _) blkCapt specs thead tbody tfoot) = do
  let isColRowSpans (Cell _ _ rs cs _) = rs > 1 || cs > 1
  let rowHasColRowSpans (Row _ cs) = any isColRowSpans cs
  let tbodyHasColRowSpans (TableBody _ _ rhs rs) =
        any rowHasColRowSpans rhs || any rowHasColRowSpans rs
  let theadHasColRowSpans (TableHead _ rs) = any rowHasColRowSpans rs
  let tfootHasColRowSpans (TableFoot _ rs) = any rowHasColRowSpans rs
  let hasColRowSpans =
        theadHasColRowSpans thead
          || any tbodyHasColRowSpans tbody
          || tfootHasColRowSpans tfoot
  let (caption, aligns, widths, headers, rows) =
        toLegacyTable blkCapt specs thead tbody tfoot
  let numcols =
        maximum $
          length aligns :| length widths : map length (headers : rows)
  caption' <- inlineListToVimdoc caption
  let caption''
        | null caption = blankline
        | otherwise = blankline $$ caption' $$ blankline
  let hasSimpleCells = onlySimpleTableCells $ headers : rows
  let isSimple = hasSimpleCells && all (== 0) widths && not hasColRowSpans
  let isPlainBlock (Plain _) = True
      isPlainBlock _ = False
  let hasBlocks = not (all (all (all isPlainBlock)) $ headers : rows)
  let padRow r = r ++ replicate x empty
       where
        x = numcols - length r
  let aligns' = aligns ++ replicate x AlignDefault
       where
        x = numcols - length aligns
  let widths' = widths ++ replicate x 0.0
       where
        x = numcols - length widths
  sw <- asks shiftWidth
  rawHeaders <- padRow <$> mapM blockListToVimdoc headers
  rawRows <- mapM (fmap padRow . mapM blockListToVimdoc) rows
  let hasHeader = all null headers
  if
    | isSimple -> do
        -- Simple table
        tbl <-
          indent sw $
            vimdocTable False hasHeader aligns' widths' rawHeaders rawRows
        pure $ nest sw (tbl $$ caption'') $$ blankline
    | not (hasBlocks || hasColRowSpans) -> do
        -- Multiline table
        tbl <-
          indent sw $
            vimdocTable True hasHeader aligns' widths' rawHeaders rawRows
        pure $ nest sw (tbl $$ caption'') $$ blankline
    | otherwise -> ("[TABLE]" $$ caption'') <$ report (BlockNotRendered t)

blockToVimdoc (Figure _ _ blocks) = blockListToVimdoc blocks

blockToVimdoc (Div _ blocks) = blockListToVimdoc blocks

{- | Create a vimdoc tag. Tag is prefixed with "$vimdocPrefix-" if vimdocPrefix
is a Just value.
>>> runReader (mkVimdocTag "abc") def
"*abc*"
>>> runReader (mkVimdocTag "abc") (def{vimdocPrefix = Just "myCoolProject"})
"*myCoolProject-abc*"
-}
mkVimdocTag :: (Monad m) => Text -> VW m Text
mkVimdocTag tag = do
  asks vimdocPrefix <&> \case
    _ | T.null tag -> ""
    Nothing -> "*" <> tag <> "*"
    Just pref' -> "*" <> pref' <> "-" <> tag <> "*"

{- | Create a hotlink for a tag, ie. a followable vimdoc link. Tag is prefixed
 - with "$vimdocPrefix-" if vimdocPrefix is a Just value
>>> runReader (mkVimdocRef "abc") def
"|abc|"
>>> runReader (mkVimdocRef "abc") (def{vimdocPrefix = Just "myCoolProject"})
"|myCoolProject-abc|"
-}
mkVimdocRef :: (Monad m) => Text -> VW m Text
mkVimdocRef ref =
  asks vimdocPrefix <&> \case
    _ | T.null ref -> ""
    Nothing -> "|" <> ref <> "|"
    Just pref' -> "|" <> pref' <> "-" <> ref <> "|"

mkVimdocDefinitionTerm ::
  (PandocMonad m) =>
  [Inline] ->
  VW m (Doc Text)
mkVimdocDefinitionTerm inlines = do
  il <- asks indentLevel
  tw <- asks (writerColumns . writerOptions)
  label <- case inlines of
    -- NOTE: commands in vim are unique, so they get no prefix
    [Code (ref, _, _) code]
      | T.isPrefixOf ":" code ->
          pure . Just $ "*" <> ref <> "*"
    [Code (ref, _, _) _] | not (T.null ref) -> Just <$> mkVimdocTag ref
    [Span (ref, _, _) _] | not (T.null ref) -> Just <$> mkVimdocTag ref
    _ -> pure Nothing

  term <- case inlines of
    [Code _ code] | T.isPrefixOf ":" code -> pure $ literal code
    _ -> inlineListToVimdoc inlines
  let termLen = offset term
  let labelLen = maybe 0 T.length label

  if il + termLen + labelLen > tw
    then
      pure . mconcat $
        [ case label of
            Nothing -> empty
            -- (+2) due to stars concealment
            Just l -> flush (rblock (tw + 2) $ literal l) <> cr
        , term
        ]
    else
      pure . mconcat $
        [ -- Since we calculated that label fits on the same line as
          -- term and since label actually must exceed textwidth to align
          -- properly, we disable wrapping.
          -- vvvvvvvv
          nowrap term
        , case label of
            Nothing -> empty
            -- (+2) due to stars concealment
            Just l -> rblock (tw - termLen - il + 2) (literal l)
        ]

-- | Write a vimdoc table
vimdocTable ::
  (Monad m) =>
  -- | whether this is a multiline table
  Bool ->
  -- | whether the table has a header
  Bool ->
  -- | column alignments
  [Alignment] ->
  -- | column widths
  [Double] ->
  -- | table header cells
  [Doc Text] ->
  -- | table body rows
  [[Doc Text]] ->
  VW m (Doc Text)
vimdocTable multiline headless aligns widths rawHeaders rawRows = do
  let isSimple = all (== 0) widths
  let alignHeader alignment = case alignment of
        AlignLeft -> lblock
        AlignCenter -> cblock
        AlignRight -> rblock
        AlignDefault -> lblock
  -- Number of characters per column necessary to output every cell
  -- without requiring a line break.
  -- The @+2@ is needed for specifying the alignment.
  let numChars = (+ 2) . maybe 0 maximum . nonEmpty . map offset
  -- Number of characters per column necessary to output every cell
  -- without requiring a line break *inside a word*.
  -- The @+2@ is needed for specifying the alignment.
  let minNumChars = (+ 2) . maybe 0 maximum . nonEmpty . map minOffset
  let columns = transpose (rawHeaders : rawRows)

  il <- asks indentLevel

  -- x = (2 * length columns)         -- spaces for specifying the alignment
  -- y = (length columns - 1)         -- spaces between the columns
  -- x + y = (3 * length columns - 1) -- total needed correction
  tw <- asks (writerColumns . writerOptions)
  let tw' = tw - il - 3 * length columns + 1
  wrap <- asks (writerWrapText . writerOptions)

  -- minimal column width without wrapping a single word
  let relWidth w col =
        max
          (floor $ fromIntegral (tw' - 1) * w)
          ( if wrap == WrapAuto
              then minNumChars col
              else numChars col
          )
  let widthsInChars
        | isSimple = map numChars columns
        | otherwise = zipWith relWidth widths columns
  let makeRow =
        hcat
          . intersperse (lblock 1 (literal " "))
          . zipWith3 alignHeader aligns widthsInChars
  let rows' = map makeRow rawRows
  -- TODO: reduce tw in case head is not empty
  let head' = makeRow rawHeaders <> " ~"
  let head'' =
        if headless
          then empty
          else head'
  let body =
        if multiline
          then
            vsep rows'
              $$ if length rows' < 2
                then blankline
                else empty
          else vcat rows'
  return $
    blankline
      $$ head''
      $$ (if multiline then blankline else empty)
      $$ body

-- | Replace Unicode characters with their ASCII representation
replaceSpecialStrings :: Text -> Text
replaceSpecialStrings =
  let expand c = case c of
        '\x00ad' -> ""
        '\x2013' -> "--"
        '\x2014' -> "---"
        '\x2019' -> "'"
        '\x2026' -> "..."
        _        -> T.singleton c
  in T.concatMap expand

inlineListToVimdoc :: (PandocMonad m) => [Inline] -> VW m (Doc Text)
inlineListToVimdoc inlines = hcat <$> mapM inlineToVimdoc inlines

inlineToVimdoc :: (PandocMonad m) => Inline -> VW m (Doc Text)

inlineToVimdoc (Str str) = pure . literal $ replaceSpecialStrings str

-- Neither `:h help-writing`, nor neovim's grammar.js for vimdoc and
-- highlights.scm say anything about styling text, so we strip all the
-- formatting
inlineToVimdoc (Emph inlines) = inlineListToVimdoc inlines
inlineToVimdoc (Underline inlines) = inlineListToVimdoc inlines
inlineToVimdoc (Strong inlines) = inlineListToVimdoc inlines
inlineToVimdoc (Strikeout inlines) = inlineListToVimdoc inlines
inlineToVimdoc (Superscript inlines) = inlineListToVimdoc inlines
inlineToVimdoc (Subscript inlines) = inlineListToVimdoc inlines
inlineToVimdoc (SmallCaps inlines) = inlineListToVimdoc inlines

inlineToVimdoc (Quoted typ inlines) =
  let quote = case typ of SingleQuote -> "'"; DoubleQuote -> "\""
   in inlineListToVimdoc inlines >>= \text -> pure (quote <> text <> quote)

inlineToVimdoc (Cite _citations inlines) = inlineListToVimdoc inlines

inlineToVimdoc (Code (_, cls, _) code) = do
  let hasNoLang = null cls
  pure . literal $ case T.words code of
    [":help", ref] | hasNoLang -> "|" <> ref <> "|"
    [":h", ref]    | hasNoLang -> "|" <> ref <> "|"
    _                          -> "`" <> code <> "`"

inlineToVimdoc Space = pure space
inlineToVimdoc SoftBreak =
  asks (writerWrapText . writerOptions) >>= \case
    WrapAuto -> pure space
    WrapNone -> pure " "
    WrapPreserve -> pure "\n"

inlineToVimdoc LineBreak = pure "\n"

inlineToVimdoc (Math _ math) = pure . literal $ "`$" <> math <> "$`"

inlineToVimdoc inline@(RawInline (Format format) text) = case format of
  "vimdoc" -> pure $ literal text
  _ -> "" <$ report (InlineNotRendered inline)

inlineToVimdoc (Link _ txt (src, _)) = do
  let srcSuffix = fromMaybe src (T.stripPrefix "mailto:" src)
  linkText <- render Nothing <$> inlineListToVimdoc txt

  let isAutolink = case txt of
        [Str x] | escapeURI x `elem` [src, srcSuffix] -> True
        _ -> False

  pure $ case refdocLinkToLink src of
    Right link | isAutolink -> "|" <> literal link <> "|"
    Right link ->
      literal (T.stripEnd linkText) <> space <> "|" <> literal link <> "|"
    Left _ | isURI src, isAutolink -> literal srcSuffix
    Left _ -> literal (T.stripEnd linkText) <> space <> literal srcSuffix

inlineToVimdoc (Image {}) = pure ""

inlineToVimdoc (Note blocks) = do
  newN <- gets (succ . Seq.length)
  contents <- blockListToVimdoc blocks
  tag <- mkVimdocTag ("footnote" <> tshow newN)
  tw <- asks (writerColumns . writerOptions)

  -- (+2) due to concealment of stars
  --                     vvvvvvvv
  let taggedContents = rblock (tw + 2) (literal tag) $$ contents
  modify (|> taggedContents)

  ref <- mkVimdocRef ("footnote" <> tshow newN)
  pure $ space <> literal ref

inlineToVimdoc (Span _ inlines) = inlineListToVimdoc inlines


refdocLinkToLink :: Text -> Either PandocError Text
refdocLinkToLink x = (\parser -> readWith parser Nothing x) $ do
  string "http" >> optional (char 's') >> string "://"

  let vimhelpP = do
        try (string "vimhelp.org/") <|> string "neo.vimhelp.org/"

        try (many1Till anyChar (char '#') >> many1TillChar anyChar eof)
          <|> many1TillChar anyChar (try $ string ".html" >> eof)

  let neovimP = do
        string "neovim.io/doc/user/"
        try (many1Till anyChar (char '#') >> many1TillChar anyChar eof)
          <|> do base <- many1TillChar anyChar (try $ string ".html" >> eof)
                 pure $ base <> ".txt"

  try vimhelpP <|> neovimP
