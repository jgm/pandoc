{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{- |
   Module      : Text.Pandoc.Writers.BBCode
   Copyright   : © 2025 Aleksey Myshko <git@crii.xyz>
   License     : GNU GPL, version 2 or above

   Maintainer  : Aleksey Myshko <git@crii.xyz>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to various BBCode flavors.
-}

module Text.Pandoc.Writers.BBCode (
  -- * Predefined writers
  -- Writers for different flavors of BBCode. 'writeBBCode' is a synonym for
  -- 'writeBBCode_official'
  writeBBCode,
  writeBBCodeOfficial,
  writeBBCodeSteam,
  writeBBCodePhpBB,
  writeBBCodeFluxBB,
  writeBBCodeHubzilla,
  writeBBCodeXenforo,

  -- * Extending the writer
  -- $extending
  FlavorSpec (..),
  WriterState (..),
  RR,
  writeBBCodeCustom,
  inlineToBBCode,
  inlineListToBBCode,
  blockToBBCode,
  blockListToBBCode,

  -- ** Handling attributes
  -- $wrapping_spans_divs
  attrToMap,

  -- * Predefined flavor specifications
  officialSpec,
  steamSpec,
  phpbbSpec,
  fluxbbSpec,
  hubzillaSpec,
  xenforoSpec,
) where

import Control.Applicative (some)
import Control.Monad (forM)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State (MonadState (..), StateT, evalStateT, gets, modify)
import Data.Default (Default (..))
import Data.Either (isRight)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Text.DocLayout hiding (char, link, text)
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging (LogMessage (..))
import Text.Pandoc.Options (WriterOptions (..))
import Text.Pandoc.Parsing (char, digit, eof, readWith)
import Text.Pandoc.Shared (inquotes, onlySimpleTableCells, removeFormatting, trim, tshow)
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.URI (escapeURI)
import Text.Pandoc.Writers.Shared (defField, metaToContext, toLegacyTable, unsmartify)
import Text.Read (readMaybe)

-- Type synonym to prevent haddock-generated HTML from overflowing
type PandocTable =
  (Attr, Caption, [ColSpec], TableHead, [TableBody], TableFoot)

-- $extending
-- If you want to support more Pandoc elements (or render some of them
-- differently) you can do so by creating your own 'FlavorSpec'
--
-- The module exports the @'FlavorSpec'@s underlying @writeBBCode_*@ functions,
-- namely 'officialSpec', 'steamSpec', 'phpbbSpec', 'fluxbbSpec',
-- 'hubzillaSpec'.
--
-- You can create and use your own renderers, for instance here we define a
-- renderer for 'CodeBlock' and use it to create a derivative format:
--
-- > renderCodeBlockCustom :: (PandocMonad m) => Attr -> Text -> RR m (Doc Text)
-- > renderCodeBlockCustom (_, cls, _) code = do
-- >   let opening = case cls of
-- >         (lang : _) -> "[code=" <> lang <> "]"
-- >         ("c++" : _) -> "[code=cpp]"
-- >         _ -> "[code]"
-- >   pure $ mconcat [literal opening, literal code, cr, "[/code]"]
-- >
-- > specCustom = officialSpec{renderCodeBlock = renderCodeBlockCustom}
--
-- Then we can use it to render 'Pandoc' document via 'writeBBCode_custom'

{- | Data type that is a collection of renderers for most elements in a Pandoc
AST (see 'Block' and 'Inline')

The intention here is to allow inheritance between formats, for instance if
format A and format @B@ differ only in rendering tables, @B@ can be implemented
as @A{'renderTable' = renderTableB}@
-}
data FlavorSpec = FlavorSpec
  { renderBlockQuote ::
      forall m.
      (PandocMonad m) =>
      [Block] ->
      RR m (Doc Text)
  -- ^ Render 'BlockQuote'
  , renderBulletList ::
      forall m.
      (PandocMonad m) =>
      [[Block]] ->
      RR m (Doc Text)
  -- ^ Render 'BulletList'
  , renderCodeBlock ::
      forall m.
      (PandocMonad m) =>
      Attr ->
      Text ->
      RR m (Doc Text)
  -- ^ Render 'CodeBlock'
  , renderDefinitionList ::
      forall m.
      (PandocMonad m) =>
      [([Inline], [[Block]])] ->
      RR m (Doc Text)
  -- ^ Render 'DefinitionList'
  , renderHeader ::
      forall m.
      (PandocMonad m) =>
      Int ->
      Attr ->
      [Inline] ->
      RR m (Doc Text)
  -- ^ Render 'Header'
  , renderInlineCode ::
      forall m.
      (PandocMonad m) =>
      Attr ->
      Text ->
      RR m (Doc Text)
  -- ^ Render 'Code'
  , renderLink ::
      forall m.
      (PandocMonad m) =>
      Attr ->
      [Inline] ->
      Target ->
      RR m (Doc Text)
  -- ^ Render 'Link'
  , renderOrderedList ::
      forall m.
      (PandocMonad m) =>
      ListAttributes ->
      [[Block]] ->
      RR m (Doc Text)
  -- ^ Render 'OrderedList'
  , renderStrikeout ::
      forall m.
      (PandocMonad m) =>
      [Inline] ->
      RR m (Doc Text)
  -- ^ Render 'Strikeout'
  , renderTable :: forall m. (PandocMonad m) => PandocTable -> RR m (Doc Text)
  -- ^ Render 'Table'
  , renderHorizontalRule ::
      forall m.
      (PandocMonad m) =>
      RR m (Doc Text)
  -- ^ Render 'HorizontalRule'
  , renderLineBlock ::
      forall m.
      (PandocMonad m) =>
      [[Inline]] ->
      RR m (Doc Text)
  -- ^ Render 'LineBlock'
  , renderPara ::
      forall m.
      (PandocMonad m) =>
      [Inline] ->
      RR m (Doc Text)
  -- ^ Render 'Para'
  , renderSuperscript ::
      forall m.
      (PandocMonad m) =>
      [Inline] ->
      RR m (Doc Text)
  -- ^ Render 'Superscript'
  , renderSubscript :: forall m. (PandocMonad m) => [Inline] -> RR m (Doc Text)
  -- ^ Render 'Subscript'
  , renderSmallCaps :: forall m. (PandocMonad m) => [Inline] -> RR m (Doc Text)
  -- ^ Render 'SmallCaps'
  , renderCite ::
      forall m.
      (PandocMonad m) =>
      [Citation] ->
      [Inline] ->
      RR m (Doc Text)
  -- ^ Render 'Cite'
  , renderNote :: forall m. (PandocMonad m) => [Block] -> RR m (Doc Text)
  -- ^ Render 'Note'
  , renderFigure ::
      forall m.
      (PandocMonad m) =>
      Attr ->
      Caption ->
      [Block] ->
      RR m (Doc Text)
  -- ^ Render 'Figure'
  , renderQuoted ::
      forall m.
      (PandocMonad m) =>
      QuoteType ->
      [Inline] ->
      RR m (Doc Text)
  -- ^ Render 'Quoted'
  , renderMath ::
      forall m.
      (PandocMonad m) =>
      MathType ->
      Text ->
      RR m (Doc Text)
  -- ^ Render 'Math'
  , renderImage ::
      forall m.
      (PandocMonad m) =>
      Attr ->
      [Inline] ->
      Target ->
      RR m (Doc Text)
  -- ^ Render 'Image'
  , wrapSpanDiv :: Bool -> Map Text (Maybe Text) -> Doc Text -> Doc Text
  -- ^ Wrap document in bbcode tags based on attributes/classes. Boolean flag
  -- indicates whether passed argument is a Div or a Span (True means Div)
  }

data WriterState = WriterState
  { writerOptions :: WriterOptions
  , flavorSpec :: FlavorSpec
  , inList :: Bool
  }

instance Default WriterState where
  def =
    WriterState
      { writerOptions = def
      , flavorSpec = officialSpec
      , inList = False
      }

-- | The base of a renderer monad.
type RR m a = StateT (Seq (Doc Text)) (ReaderT WriterState m) a

pandocToBBCode :: (PandocMonad m) => Pandoc -> RR m Text
pandocToBBCode (Pandoc meta body) = do
  opts <- asks writerOptions
  -- Run the rendering that mutates the state by producing footnotes
  bodyContents <- blockListToBBCode body
  -- Get the footnotes
  footnotes <- get
  -- Separate footnotes (if any) with a horizontal rule
  footnotesSep <-
    if null footnotes
      then pure empty
      else
        (\hr -> blankline <> hr <> blankline)
          <$> blockToBBCode HorizontalRule
  -- Put footnotes after the main text
  let docText = bodyContents <> footnotesSep <> vsep (toList footnotes)
  metadata <- metaToContext opts blockListToBBCode inlineListToBBCode meta
  let context = defField "body" docText metadata
  case writerTemplate opts of
    Just tpl -> pure $ render Nothing (renderTemplate tpl context)
    Nothing -> pure $ render Nothing docText

writeBBCode
  , writeBBCodeOfficial
  , writeBBCodeSteam
  , writeBBCodePhpBB
  , writeBBCodeFluxBB
  , writeBBCodeHubzilla
  , writeBBCodeXenforo ::
    (PandocMonad m) => WriterOptions -> Pandoc -> m Text
writeBBCode = writeBBCodeOfficial
writeBBCodeOfficial = writeBBCodeCustom officialSpec
writeBBCodeSteam = writeBBCodeCustom steamSpec
writeBBCodePhpBB = writeBBCodeCustom phpbbSpec
writeBBCodeFluxBB = writeBBCodeCustom fluxbbSpec
writeBBCodeHubzilla = writeBBCodeCustom hubzillaSpec
writeBBCodeXenforo = writeBBCodeCustom xenforoSpec

{- | Convert a 'Pandoc' document to BBCode using the given 'FlavorSpec' and
'WriterOptions'.
-}
writeBBCodeCustom ::
  (PandocMonad m) => FlavorSpec -> WriterOptions -> Pandoc -> m Text
writeBBCodeCustom spec opts document =
  runRR mempty def{writerOptions = opts, flavorSpec = spec} $
    pandocToBBCode document
 where
  runRR :: (Monad m) => Seq (Doc Text) -> WriterState -> RR m a -> m a
  runRR footnotes writerState action =
    runReaderT (evalStateT action footnotes) writerState

blockListToBBCode :: (PandocMonad m) => [Block] -> RR m (Doc Text)
blockListToBBCode blocks =
  chomp . vsep . filter (not . null)
    <$> mapM blockToBBCode blocks

blockToBBCode :: (PandocMonad m) => Block -> RR m (Doc Text)
blockToBBCode block = do
  spec <- asks flavorSpec
  case block of
    Plain inlines -> inlineListToBBCode inlines
    Para inlines -> renderPara spec inlines
    LineBlock inliness -> renderLineBlock spec inliness
    CodeBlock attr code -> renderCodeBlock spec attr code
    RawBlock format raw -> case format of
      "bbcode" -> pure $ literal raw
      _ -> "" <$ report (BlockNotRendered block)
    BlockQuote blocks -> renderBlockQuote spec blocks
    OrderedList attr items -> renderOrderedList spec attr items
    BulletList items -> renderBulletList spec items
    DefinitionList items -> renderDefinitionList spec items
    Header level attr inlines -> renderHeader spec level attr inlines
    HorizontalRule -> renderHorizontalRule spec
    Table attr blkCapt specs thead tbody tfoot ->
      renderTable spec (attr, blkCapt, specs, thead, tbody, tfoot)
    Figure attr caption blocks -> renderFigure spec attr caption blocks
    Div attr blocks -> do
      contents <- blockListToBBCode blocks
      let kvcMap = attrToMap attr
      -- whether passed contents is a Div (Block) element
      --                      vvvv
      pure $ wrapSpanDiv spec True kvcMap contents

inlineToBBCode :: (PandocMonad m) => Inline -> RR m (Doc Text)
inlineToBBCode inline = do
  spec <- asks flavorSpec
  case inline of
    Str str -> do
      opts <- asks writerOptions
      pure . literal $ unsmartify opts str
    Emph inlines -> do
      contents <- inlineListToBBCode inlines
      pure $ mconcat ["[i]", contents, "[/i]"]
    Underline inlines -> do
      contents <- inlineListToBBCode inlines
      pure $ mconcat ["[u]", contents, "[/u]"]
    Strong inlines -> do
      contents <- inlineListToBBCode inlines
      pure $ mconcat ["[b]", contents, "[/b]"]
    Strikeout inlines -> renderStrikeout spec inlines
    Superscript inlines -> renderSuperscript spec inlines
    Subscript inlines -> renderSubscript spec inlines
    SmallCaps inlines -> renderSmallCaps spec inlines
    Quoted typ inlines -> renderQuoted spec typ inlines
    Cite cits inlines -> renderCite spec cits inlines
    Code attr code -> renderInlineCode spec attr code
    Space -> pure space
    SoftBreak -> pure space
    LineBreak -> pure cr
    Math typ math -> renderMath spec typ math
    RawInline (Format format) text -> case format of
      "bbcode" -> pure $ literal text
      _ -> "" <$ report (InlineNotRendered inline)
    Link attr txt target -> renderLink spec attr txt target
    Image attr alt target -> renderImage spec attr alt target
    Note blocks -> renderNote spec blocks
    Span attr inlines -> do
      contents <- inlineListToBBCode inlines
      let kvcMap = attrToMap attr
      -- whether passed contents is a Div (Block element)
      --                      vvvvv
      pure $ wrapSpanDiv spec False kvcMap contents

renderImageDefault ::
  (PandocMonad m) => Attr -> [Inline] -> Target -> RR m (Doc Text)
renderImageDefault (_, _, kvList) alt (source, title) = do
  altText <-
    trim . render Nothing
      <$> inlineListToBBCode (removeFormatting alt)
  let kvMap = Map.fromList kvList
  -- No BBCode flavor supported by the Writer has local images support, but we
  -- still allow source to be plain path or anything else
  pure . literal $
    mconcat
      [ "[img"
      , if T.null altText
          then ""
          else " alt=" <> inquotes altText
      , if T.null title
          then ""
          else " title=" <> inquotes title
      , case Map.lookup "width" kvMap of
          Just w
            | isJust (readMaybe @Int $ T.unpack w) ->
                " width=" <> inquotes w
          _ -> ""
      , case Map.lookup "height" kvMap of
          Just h
            | isJust (readMaybe @Int $ T.unpack h) ->
                " height=" <> inquotes h
          _ -> ""
      , "]"
      , source
      , "[/img]"
      ]

renderImageOmit ::
  (PandocMonad m) => Attr -> [Inline] -> Target -> RR m (Doc Text)
renderImageOmit _ _ _ = pure ""

{- | Basic phpBB doesn't support any attributes, although
@[img src=https://example.com]whatever[/img]@ is supported, but text in tag has
no effect
-}
renderImagePhpBB ::
  (PandocMonad m) => Attr -> [Inline] -> Target -> RR m (Doc Text)
renderImagePhpBB _ _ (source, _) =
  pure . literal $ mconcat ["[img]", source, "[/img]"]

renderImageXenforo ::
  (PandocMonad m) => Attr -> [Inline] -> Target -> RR m (Doc Text)
renderImageXenforo (_, _, kvList) alt (source, title) = do
  altText <-
    trim . render Nothing
      <$> inlineListToBBCode (removeFormatting alt)
  let kvMap = Map.fromList kvList
  -- No BBCode flavor supported by the Writer has local images support, but we
  -- still allow source to be plain path or anything else
  pure . literal $
    mconcat
      [ "[img"
      , if T.null altText
          then ""
          else " alt=" <> inquotes altText
      , if T.null title
          then ""
          else " title=" <> inquotes title
      , case Map.lookup "width" kvMap of
          Just w
            | isRight (readWith sizeP Nothing w) ->
                " width=" <> w
          _ -> ""
      , "]"
      , source
      , "[/img]"
      ]
 where
  sizeP = some digit >> char '%' >> eof

{- | Check whether character is a bracket

>>> T.filter notBracket "[a]b[[ó]qü]]n®"
"ab\243q\252n\174"
-}
notBracket :: Char -> Bool
notBracket = \case
  '[' -> False
  ']' -> False
  _ -> True

-- FluxBB uses [img=alt text] instead of [img alt="alt text"]
renderImageFluxBB ::
  (PandocMonad m) => Attr -> [Inline] -> Target -> RR m (Doc Text)
renderImageFluxBB _ alt (source, _) = do
  alt' <- T.filter notBracket . render Nothing <$> inlineListToBBCode alt
  pure . literal $
    mconcat
      [ "[img"
      , if T.null alt'
          then ""
          else "=" <> alt'
      , "]"
      , source
      , "[/img]"
      ]

inlineListToBBCode :: (PandocMonad m) => [Inline] -> RR m (Doc Text)
inlineListToBBCode inlines = mconcat <$> mapM inlineToBBCode inlines

-- Taken from Data.Ord
clamp :: (Ord a) => (a, a) -> a -> a
clamp (low, high) a = min high (max a low)

renderHeaderDefault ::
  (PandocMonad m) => Int -> Attr -> [Inline] -> RR m (Doc Text)
renderHeaderDefault level _attr inlines =
  case clamp (1, 4) level of
    1 -> inlineToBBCode $ Underline [Strong inlines]
    2 -> inlineToBBCode $ Strong inlines
    3 -> inlineToBBCode $ Underline inlines
    _ -> inlineListToBBCode inlines

-- Adapted from Text.Pandoc.Writers.Org
renderLinkDefault ::
  (PandocMonad m) => Attr -> [Inline] -> Target -> RR m (Doc Text)
renderLinkDefault _ txt (src, _) =
  case txt of
    [Str x]
      | escapeURI x == src ->
          pure $ "[url]" <> literal x <> "[/url]"
    _ -> do
      contents <- inlineListToBBCode txt
      let suffix = if T.null src then "" else "=" <> src
      pure $ "[url" <> literal suffix <> "]" <> contents <> "[/url]"

renderCodeBlockDefault :: (PandocMonad m) => Attr -> Text -> RR m (Doc Text)
renderCodeBlockDefault (_, cls, _) code = do
  let opening = case cls of
        (lang : _) -> "[code=" <> lang <> "]"
        _ -> "[code]"
  pure $ mconcat [literal opening, literal code, cr, "[/code]"]

renderCodeBlockSimple :: (PandocMonad m) => Attr -> Text -> RR m (Doc Text)
renderCodeBlockSimple _ code = do
  pure $ mconcat [literal "[code]", literal code, cr, "[/code]"]

renderInlineCodeLiteral :: (PandocMonad m) => Attr -> Text -> RR m (Doc Text)
renderInlineCodeLiteral _ code = pure $ literal code

renderInlineCodeNoParse :: (PandocMonad m) => Attr -> Text -> RR m (Doc Text)
renderInlineCodeNoParse _ code =
  pure $ mconcat [literal "[noparse]", literal code, "[/noparse]"]

renderInlineCodeHubzilla :: (PandocMonad m) => Attr -> Text -> RR m (Doc Text)
renderInlineCodeHubzilla _ code =
  pure $ mconcat [literal "[code]", literal code, "[/code]"]

renderInlineCodeXenforo :: (PandocMonad m) => Attr -> Text -> RR m (Doc Text)
renderInlineCodeXenforo _ code =
  pure $ mconcat [literal "[icode]", literal code, "[/icode]"]

renderStrikeoutDefault :: (PandocMonad m) => [Inline] -> RR m (Doc Text)
renderStrikeoutDefault inlines = do
  contents <- inlineListToBBCode inlines
  pure $ mconcat ["[s]", contents, "[/s]"]

renderStrikeoutSteam :: (PandocMonad m) => [Inline] -> RR m (Doc Text)
renderStrikeoutSteam inlines = do
  contents <- inlineListToBBCode inlines
  pure $ mconcat ["[strike]", contents, "[/strike]"]

renderDefinitionListDefault ::
  (PandocMonad m) => [([Inline], [[Block]])] -> RR m (Doc Text)
renderDefinitionListDefault items = do
  items' <- forM items $ \(term, definitions) -> do
    term' <- inlineListToBBCode term
    definitions' <- blockToBBCode (BulletList definitions)
    pure $ term' $$ definitions'
  pure $ vcat items'

renderDefinitionListHubzilla ::
  (PandocMonad m) => [([Inline], [[Block]])] -> RR m (Doc Text)
renderDefinitionListHubzilla items = do
  items' <- forM items $ \(term, definitions) -> do
    term' <- inlineListToBBCode term
    let term'' = "[*= " <> term' <> "]"
    definitions' <- forM definitions blockListToBBCode
    pure $ vcat (term'' : definitions')
  pure $ vcat (literal "[dl terms=\"b\"]" : items' ++ [literal "[/dl]"])

listWithTags ::
  (PandocMonad m) =>
  Text ->
  Text ->
  ([[Block]] -> RR m [Doc Text]) ->
  [[Block]] ->
  RR m (Doc Text)
listWithTags open close renderItems items = do
  contents <- local (\s -> s{inList = True}) (renderItems items)
  pure $ vcat $ literal open : contents ++ [literal close]

starListItems :: (PandocMonad m) => [[Block]] -> RR m [Doc Text]
starListItems items = forM items $ \item -> do
  item' <- blockListToBBCode item
  pure $ literal "[*]" <> item'

listStyleCode :: ListNumberStyle -> Maybe Text
listStyleCode = \case
  Decimal -> Just "1"
  DefaultStyle -> Just "1"
  LowerAlpha -> Just "a"
  UpperAlpha -> Just "A"
  LowerRoman -> Just "i"
  UpperRoman -> Just "I"
  Example -> Nothing

renderBulletListOfficial :: (PandocMonad m) => [[Block]] -> RR m (Doc Text)
renderBulletListOfficial = listWithTags "[list]" "[/list]" starListItems

renderBulletListHubzilla :: (PandocMonad m) => [[Block]] -> RR m (Doc Text)
renderBulletListHubzilla = listWithTags "[ul]" "[/ul]" starListItems

renderOrderedListHubzilla ::
  (PandocMonad m) => ListAttributes -> [[Block]] -> RR m (Doc Text)
renderOrderedListHubzilla (_, style, _) = case style of
  DefaultStyle -> listWithTags "[ol]" "[/ol]" starListItems
  Example -> listWithTags "[ol]" "[/ol]" starListItems
  _ -> listWithTags ("[list=" <> suffix <> "]") "[/list]" starListItems
 where
  suffix = fromMaybe "1" $ listStyleCode style

renderOrderedListOfficial ::
  (PandocMonad m) => ListAttributes -> [[Block]] -> RR m (Doc Text)
renderOrderedListOfficial (_, style, _) = do
  let suffix = maybe "" ("=" <>) (listStyleCode style)
  listWithTags ("[list" <> suffix <> "]") "[/list]" starListItems

renderOrderedListSteam ::
  (PandocMonad m) => ListAttributes -> [[Block]] -> RR m (Doc Text)
renderOrderedListSteam _ =
  listWithTags "[olist]" "[/olist]" starListItems

renderHeaderSteam ::
  (PandocMonad m) => Int -> Attr -> [Inline] -> RR m (Doc Text)
renderHeaderSteam level _ inlines = do
  body <- inlineListToBBCode inlines
  let capped = clamp (1, 3) level
      open = "[h" <> tshow capped <> "]"
      close = "[/h" <> tshow capped <> "]"
  pure $ literal open <> body <> literal close

renderHeaderHubzilla ::
  (PandocMonad m) => Int -> Attr -> [Inline] -> RR m (Doc Text)
renderHeaderHubzilla level _ inlines = do
  body <- inlineListToBBCode inlines
  let capped = clamp (1, 6) level
      open = "[h" <> tshow capped <> "]"
      close = "[/h" <> tshow capped <> "]"
  pure $ literal open <> body <> literal close

-- xenForo supports levels 1--3, but levels other than 1--3 become div with
-- .bbHeading class which can be linked to.
renderHeaderXenforo ::
  (PandocMonad m) => Int -> Attr -> [Inline] -> RR m (Doc Text)
renderHeaderXenforo level _ inlines = do
  body <- inlineListToBBCode inlines
  let capped = max 1 level
      open = "[heading=" <> tshow capped <> "]"
      close = "[/heading]"
  pure $ literal open <> body <> literal close

renderTableGeneric ::
  (PandocMonad m) =>
  Text ->
  Text ->
  Text ->
  (Attr, Caption, [ColSpec], TableHead, [TableBody], TableFoot) ->
  RR m (Doc Text)
renderTableGeneric tableTag headerCellTag bodyCellTag table = do
  caption' <- inlineListToBBCode caption
  table' <-
    if not simpleCells
      then "" <$ report (BlockNotRendered tableBlock)
      else do
        headerDocs <-
          if null headers
            then pure []
            else pure <$> renderTableRow headerCellTag headers
        rowDocs <- mapM (renderTableRow bodyCellTag) rows
        pure $ renderTable' headerDocs rowDocs
  pure $ caption' $$ table'
 where
  (attr, blkCapt, specs, thead, tbody, tfoot) = table
  (caption, _, _, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
  tableBlock = Table attr blkCapt specs thead tbody tfoot
  simpleCells = onlySimpleTableCells (headers : rows)
  renderTable' headerDocs rowDocs =
    vcat
      [ literal ("[" <> tableTag <> "]")
      , vcat headerDocs
      , vcat rowDocs
      , literal ("[/" <> tableTag <> "]")
      ]
  renderCell cellTag cellDoc =
    mconcat
      [ literal ("[" <> cellTag <> "]")
      , cellDoc
      , literal ("[/" <> cellTag <> "]")
      ]
  renderTableRow cellTag cells = do
    renderedCells <- mapM blockListToBBCode cells
    let cellsDoc = mconcat $ map (renderCell cellTag) renderedCells
    pure $ literal "[tr]" <> cellsDoc <> literal "[/tr]"

renderTableDefault ::
  (PandocMonad m) =>
  ( Attr
  , Caption
  , [ColSpec]
  , TableHead
  , [TableBody]
  , TableFoot
  ) ->
  RR m (Doc Text)
renderTableDefault = renderTableGeneric "table" "th" "td"

renderTableOmit ::
  (PandocMonad m) =>
  ( Attr
  , Caption
  , [ColSpec]
  , TableHead
  , [TableBody]
  , TableFoot
  ) ->
  RR m (Doc Text)
renderTableOmit (_, blkCapt, specs, thead, tbody, tfoot) = do
  let (caption, _, _, _, _) = toLegacyTable blkCapt specs thead tbody tfoot
  caption' <- inlineListToBBCode caption
  pure $ caption' $$ "(TABLE)"

-- $wrapping_spans_divs
-- Consider attribute a key-value pair with a Just value, and respectively
-- class is key-value pair with Nothing value.
-- For instance, given @("", ["cl1"], [("k", "v")]) :: 'Attr'@, respective Map
-- should look like @'Map.fromList' [("cl1", 'Nothing'), ("k", 'Just' "v")]@
--
-- This transformation is handled by 'attrToMap'
--
-- Example definition of a wrapSpanDiv:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Data.Map (Map)
-- > import qualified Data.Map as Map
-- > import Text.DocLayout
-- > import Data.Text (Text)
-- > import qualified Data.Text as T
-- >
-- > wrapSpanDivSteam :: Bool -> Map Text (Maybe Text) -> Doc Text -> Doc Text
-- > wrapSpanDivSteam isDiv kvc doc = Map.foldrWithKey wrap doc kvc
-- >  where
-- >   wrap "spoiler" (Just _) acc | isDiv = "[spoiler]" <> acc <> "[/spoiler]"
-- >   wrap "spoiler" Nothing acc | isDiv = "[spoiler]" <> acc <> "[/spoiler]"
-- >   wrap _ _ acc = acc
--
-- To verify it works, wrap some text in unnamed spoiler
--
-- >>> render Nothing $ wrapSpanDivSteam True (attrToMap ("", ["spoiler"], [])) "I am text"
-- "[spoiler]I am text[/spoiler]"

{- | The goal of the transformation is to treat classes and key-value pairs
uniformly.

Class list becomes Map where all values are Nothing, and list of key-value
pairs is converted to Map via 'Map.toList'. Both Maps are then merged.
-}
attrToMap :: Attr -> Map Text (Maybe Text)
attrToMap (_, classes, kvList) =
  Map.fromList kvList' `Map.union` Map.fromList classes'
 where
  kvList' = map (\(k, v) -> (k, Just v)) kvList
  classes' = map (\k -> (k, Nothing)) classes

wrapSpanDivOfficial :: Bool -> Map Text (Maybe Text) -> Doc Text -> Doc Text
wrapSpanDivOfficial isDiv kvc doc = Map.foldrWithKey wrap doc kvc
 where
  wrap "left" Nothing acc | isDiv = "[left]" <> acc <> "[/left]"
  wrap "center" Nothing acc | isDiv = "[center]" <> acc <> "[/center]"
  wrap "right" Nothing acc | isDiv = "[right]" <> acc <> "[/right]"
  wrap "spoiler" Nothing acc | isDiv = "[spoiler]" <> acc <> "[/spoiler]"
  wrap "spoiler" (Just v) acc
    | isDiv =
        literal ("[spoiler=" <> T.filter notBracket v <> "]")
          <> acc
          <> "[/spoiler]"
  wrap "size" (Just v) acc
    | Just v' <- readMaybe @Int (T.unpack v)
    , v' > 0 =
        literal ("[size=" <> v <> "]") <> acc <> "[/size]"
  wrap "color" (Just v) acc =
    literal ("[color=" <> v <> "]") <> acc <> "[/color]"
  wrap _ _ acc = acc

wrapSpanDivSteam :: Bool -> Map Text (Maybe Text) -> Doc Text -> Doc Text
wrapSpanDivSteam isDiv kvc doc = Map.foldrWithKey wrap doc kvc
 where
  wrap "spoiler" (Just _) acc | isDiv = "[spoiler]" <> acc <> "[/spoiler]"
  wrap "spoiler" Nothing acc | isDiv = "[spoiler]" <> acc <> "[/spoiler]"
  wrap _ _ acc = acc

wrapSpanDivPhpBB :: Bool -> Map Text (Maybe Text) -> Doc Text -> Doc Text
wrapSpanDivPhpBB _ kvc doc = Map.foldrWithKey wrap doc kvc
 where
  wrap "color" (Just v) acc =
    literal ("[color=" <> v <> "]") <> acc <> "[/color]"
  wrap _ _ acc = acc

wrapSpanDivFluxBB :: Bool -> Map Text (Maybe Text) -> Doc Text -> Doc Text
wrapSpanDivFluxBB _ kvc doc = Map.foldrWithKey wrap doc kvc
 where
  wrap "color" (Just v) acc =
    literal ("[color=" <> v <> "]") <> acc <> "[/color]"
  wrap _ _ acc = acc

wrapSpanDivHubzilla :: Bool -> Map Text (Maybe Text) -> Doc Text -> Doc Text
wrapSpanDivHubzilla isDiv kvc doc = Map.foldrWithKey wrap doc kvc
 where
  wrap "center" Nothing acc | isDiv = "[center]" <> acc <> "[/center]"
  wrap "spoiler" Nothing acc | isDiv = "[spoiler]" <> acc <> "[/spoiler]"
  wrap "spoiler" (Just v) acc
    | isDiv =
        literal ("[spoiler=" <> T.filter notBracket v <> "]")
          <> acc
          <> "[/spoiler]"
  wrap "size" (Just v) acc
    | Just v' <- readMaybe @Int (T.unpack v)
    , v' > 0 =
        literal ("[size=" <> v <> "]") <> acc <> "[/size]"
  wrap "color" (Just v) acc =
    literal ("[color=" <> v <> "]") <> acc <> "[/color]"
  wrap "font" (Just v) acc = literal ("[font=" <> v <> "]") <> acc <> "[/font]"
  wrap _ _ acc = acc

wrapSpanDivXenforo :: Bool -> Map Text (Maybe Text) -> Doc Text -> Doc Text
wrapSpanDivXenforo isDiv kvc doc = Map.foldrWithKey wrap doc kvc
 where
  wrap "left" Nothing acc | isDiv = "[left]" <> acc <> "[/left]"
  wrap "center" Nothing acc | isDiv = "[center]" <> acc <> "[/center]"
  wrap "right" Nothing acc | isDiv = "[right]" <> acc <> "[/right]"
  wrap "spoiler" _ acc | not isDiv = "[ispoiler]" <> acc <> "[/ispoiler]"
  wrap "spoiler" Nothing acc | isDiv = "[spoiler]" <> acc <> "[/spoiler]"
  wrap "spoiler" (Just v) acc
    | isDiv =
        literal ("[spoiler=" <> T.filter notBracket v <> "]")
          <> acc
          <> "[/spoiler]"
  wrap "size" (Just v) acc
    | Just v' <- readMaybe @Int (T.unpack v)
    , v' > 0 =
        literal ("[size=" <> v <> "]") <> acc <> "[/size]"
  wrap "color" (Just v) acc =
    literal ("[color=" <> v <> "]") <> acc <> "[/color]"
  wrap "font" (Just v) acc = literal ("[font=" <> v <> "]") <> acc <> "[/font]"
  wrap _ _ acc = acc

renderOrderedListFluxbb ::
  (PandocMonad m) =>
  ListAttributes ->
  [[Block]] ->
  RR m (Doc Text)
renderOrderedListFluxbb (_, style, _) =
  let suffix = case style of
        LowerAlpha -> "=a"
        UpperAlpha -> "=a"
        _ -> "=1"
   in listWithTags ("[list" <> suffix <> "]") "[/list]" starListItems

renderOrderedListXenforo ::
  (PandocMonad m) =>
  ListAttributes ->
  [[Block]] ->
  RR m (Doc Text)
renderOrderedListXenforo _ =
  listWithTags "[list=1]" "[/list]" starListItems

renderLinkEmailAware ::
  (PandocMonad m) =>
  Attr ->
  [Inline] ->
  Target ->
  RR m (Doc Text)
renderLinkEmailAware attr txt target@(src, _) = do
  case T.stripPrefix "mailto:" src of
    Just address -> do
      linkText <- inlineListToBBCode txt
      let isAutoEmail = case txt of
            [Str x] -> x == address
            _ -> False
      pure $
        if isAutoEmail
          then literal "[email]" <> literal address <> "[/email]"
          else literal ("[email=" <> address <> "]") <> linkText <> "[/email]"
    Nothing -> renderLinkDefault attr txt target

renderBlockQuoteDefault :: (PandocMonad m) => [Block] -> RR m (Doc Text)
renderBlockQuoteDefault blocks = do
  contents <- blockListToBBCode blocks
  pure $ vcat ["[quote]", contents, "[/quote]"]

renderBlockQuoteFluxBB :: (PandocMonad m) => [Block] -> RR m (Doc Text)
renderBlockQuoteFluxBB blocks = do
  contents <- blockListToBBCode blocks
  isInList <- asks inList
  if isInList
    then "" <$ report (BlockNotRendered $ BlockQuote blocks)
    else pure $ vcat ["[quote]", contents, "[/quote]"]

renderHorizontalRuleDefault :: (PandocMonad m) => RR m (Doc Text)
renderHorizontalRuleDefault = pure "* * *"

renderHorizontalRuleHR :: (PandocMonad m) => RR m (Doc Text)
renderHorizontalRuleHR = pure "[hr]"

renderLineBlockDefault :: (PandocMonad m) => [[Inline]] -> RR m (Doc Text)
renderLineBlockDefault inliness = vcat <$> mapM inlineListToBBCode inliness

renderParaDefault :: (PandocMonad m) => [Inline] -> RR m (Doc Text)
renderParaDefault inlines = inlineListToBBCode inlines

renderSuperscriptDefault :: (PandocMonad m) => [Inline] -> RR m (Doc Text)
renderSuperscriptDefault = inlineListToBBCode

renderSubscriptDefault :: (PandocMonad m) => [Inline] -> RR m (Doc Text)
renderSubscriptDefault = inlineListToBBCode

renderSmallCapsDefault :: (PandocMonad m) => [Inline] -> RR m (Doc Text)
renderSmallCapsDefault = inlineListToBBCode

renderCiteDefault ::
  (PandocMonad m) => [Citation] -> [Inline] -> RR m (Doc Text)
renderCiteDefault _ = inlineListToBBCode

renderNoteDefault :: (PandocMonad m) => [Block] -> RR m (Doc Text)
renderNoteDefault blocks = do
  -- NOTE: no BBCode flavor has native syntax for footnotes.
  newN <- gets (succ . Seq.length)
  contents <- blockListToBBCode blocks
  let pointer = "(" <> tshow newN <> ")"
  let contents' = literal pointer <> space <> contents
  modify (|> contents')
  pure $ literal pointer

renderFigureDefault ::
  (PandocMonad m) => Attr -> Caption -> [Block] -> RR m (Doc Text)
renderFigureDefault _ (Caption _ caption) blocks = do
  caption' <- blockListToBBCode caption
  contents <- blockListToBBCode blocks
  pure $ contents $$ caption'

renderQuotedDefault ::
  (PandocMonad m) => QuoteType -> [Inline] -> RR m (Doc Text)
renderQuotedDefault typ inlines = do
  let quote = case typ of SingleQuote -> "'"; DoubleQuote -> "\""
  contents <- inlineListToBBCode inlines
  pure $ mconcat [quote, contents, quote]

renderMathDefault :: (PandocMonad m) => MathType -> Text -> RR m (Doc Text)
renderMathDefault typ math = case typ of
  InlineMath ->
    inlineToBBCode $
      Code ("", ["latex"], []) ("$" <> math <> "$")
  DisplayMath ->
    blockToBBCode $
      CodeBlock ("", ["latex"], []) ("$$" <> math <> "$$")

{- | Format documentation: <https://www.bbcode.org/reference.php>

There is no such thing as «Official» bbcode format, nonetheless this spec
implements what is described on bbcode.org, which is a reasonable base that can
be extended/contracted as needed.
-}
officialSpec :: FlavorSpec
officialSpec =
  FlavorSpec
    { renderOrderedList = renderOrderedListOfficial
    , renderBulletList = renderBulletListOfficial
    , renderDefinitionList = renderDefinitionListDefault
    , renderHeader = renderHeaderDefault
    , renderTable = renderTableDefault
    , renderLink = renderLinkEmailAware
    , renderCodeBlock = renderCodeBlockDefault
    , renderInlineCode = renderInlineCodeLiteral
    , renderStrikeout = renderStrikeoutDefault
    , renderBlockQuote = renderBlockQuoteDefault
    , renderHorizontalRule = renderHorizontalRuleDefault
    , renderLineBlock = renderLineBlockDefault
    , renderPara = renderParaDefault
    , renderSuperscript = renderSuperscriptDefault
    , renderSubscript = renderSubscriptDefault
    , renderSmallCaps = renderSmallCapsDefault
    , renderCite = renderCiteDefault
    , renderNote = renderNoteDefault
    , renderFigure = renderFigureDefault
    , renderMath = renderMathDefault
    , renderQuoted = renderQuotedDefault
    , renderImage = renderImageDefault
    , wrapSpanDiv = wrapSpanDivOfficial
    }

{- | Format documentation: <https://steamcommunity.com/comment/ForumTopic/formattinghelp>

Used at: <https://steamcommunity.com/discussions/forum>

Quirks:

- There seems to be no way to show external images on steam.
  https://steamcommunity.com/sharedfiles/filedetails/?id=2807121939 shows [img]
  and [previewimg] can (could?) be used to show images, although it is likely
  reserved for steam urls only.
-}
steamSpec :: FlavorSpec
steamSpec =
  officialSpec
    { renderOrderedList = renderOrderedListSteam
    , renderHeader = renderHeaderSteam
    , renderLink = renderLinkDefault
    , renderInlineCode = renderInlineCodeNoParse
    , renderStrikeout = renderStrikeoutSteam
    , renderImage = renderImageOmit
    , wrapSpanDiv = wrapSpanDivSteam
    , renderHorizontalRule = renderHorizontalRuleHR
    }

{- | Format documentation: <https://www.phpbb.com/community/help/bbcode>

Used at: <https://www.phpbb.com/community>

Quirks:

- PhpBB docs don't mention strikeout support, but their
  [support forum](https://www.phpbb.com/community) does support it.
- Same for named code blocks.
- @[email=example\@example.com]the email[/url]@ is a valid use of [email]
  tag on the phpBB community forum despite not being in the docs.
-}
phpbbSpec :: FlavorSpec
phpbbSpec =
  officialSpec
    { renderTable = renderTableOmit
    , renderImage = renderImagePhpBB
    , wrapSpanDiv = wrapSpanDivPhpBB
    }

{- | Format documentation: <https://web.archive.org/web/20210623155046/https://fluxbb.org/forums/help.php#bbcode>

Used at: https://bbs.archlinux.org
-}
fluxbbSpec :: FlavorSpec
fluxbbSpec =
  officialSpec
    { renderOrderedList = renderOrderedListFluxbb
    , renderCodeBlock = renderCodeBlockSimple
    , renderTable = renderTableOmit
    , renderBlockQuote = renderBlockQuoteFluxBB
    , renderImage = renderImageFluxBB
    , wrapSpanDiv = wrapSpanDivFluxBB
    }

{- | Format documentation: <https://hubzilla.org/help/member/bbcode>

Used at: <https://hub.netzgemeinde.eu> (see [other hubs](https://hubzilla.org/pubsites))

Quirks:

- If link target is not a URI, it simply points to https://$BASEURL/ when
  rendered by a hub.
-}
hubzillaSpec :: FlavorSpec
hubzillaSpec =
  officialSpec
    { renderOrderedList = renderOrderedListHubzilla
    , renderBulletList = renderBulletListHubzilla
    , renderDefinitionList = renderDefinitionListHubzilla
    , renderHeader = renderHeaderHubzilla
    , renderInlineCode = renderInlineCodeHubzilla
    , renderLink = renderLinkDefault
    , wrapSpanDiv = wrapSpanDivHubzilla
    , renderHorizontalRule = renderHorizontalRuleHR
    }

{- | Format documentation: <https://www.xenfocus.com/community/help/bb-codes/>

Used at: see <https://xenforo.com/>
-}
xenforoSpec :: FlavorSpec
xenforoSpec =
  officialSpec
    { wrapSpanDiv = wrapSpanDivXenforo
    , renderHeader = renderHeaderXenforo
    , renderInlineCode = renderInlineCodeXenforo
    , renderHorizontalRule = renderHorizontalRuleHR
    , renderOrderedList = renderOrderedListXenforo
    , renderImage = renderImageXenforo
    }
