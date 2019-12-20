{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{- |
   Module      : Text.Pandoc.Writers.Jira
   Copyright   : © 2010-2019 Albert Krewinkel, John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Jira markup.

JIRA:
<https://jira.atlassian.com/secure/WikiRendererHelpAction.jspa?section=all>
-}
module Text.Pandoc.Writers.Jira ( writeJira ) where
import Prelude
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify)
import Data.Foldable (find)
import Data.Text (Text)
import Text.Jira.Parser (plainText)
import Text.Jira.Printer (prettyBlocks, prettyInlines)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options (WriterOptions (writerTemplate, writerWrapText),
                            WrapOption (..))
import Text.Pandoc.Shared (linesToPara)
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Math (texMathToInlines)
import Text.Pandoc.Writers.Shared (defField, metaToContext)
import Text.DocLayout (literal, render)
import qualified Data.Text as T
import qualified Text.Jira.Markup as Jira

-- | Convert Pandoc to Jira.
writeJira :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeJira opts = runDefaultConverter (writerWrapText opts) (pandocToJira opts)

-- | State to keep track of footnotes.
newtype ConverterState = ConverterState { stNotes :: [Text] }

-- | Initial converter state.
startState :: ConverterState
startState = ConverterState { stNotes = [] }

-- | Converter monad
type JiraConverter m = ReaderT WrapOption (StateT ConverterState m)

-- | Run a converter using the default state
runDefaultConverter :: PandocMonad m
                    => WrapOption
                    -> (a -> JiraConverter m Text)
                    -> a
                    -> m Text
runDefaultConverter wrap c x = evalStateT (runReaderT (c x) wrap) startState

-- | Return Jira representation of document.
pandocToJira :: PandocMonad m
             => WriterOptions -> Pandoc -> JiraConverter m Text
pandocToJira opts (Pandoc meta blocks) = do
  wrap <- ask
  metadata <- metaToContext opts
                 (fmap literal . runDefaultConverter wrap blockListToJira)
                 (fmap literal . runDefaultConverter wrap inlineListToJira) meta
  body <- blockListToJira blocks
  notes <- gets $ T.intercalate "\n" . reverse . stNotes
  let main = body <> if T.null notes then mempty else "\n\n" <> notes
  let context = defField "body" main metadata
  return $
    case writerTemplate opts of
      Nothing  -> main
      Just tpl -> render Nothing $ renderTemplate tpl context

blockListToJira :: PandocMonad m => [Block] -> JiraConverter m Text
blockListToJira = fmap prettyBlocks . toJiraBlocks

inlineListToJira :: PandocMonad m => [Inline] -> JiraConverter m Text
inlineListToJira = fmap prettyInlines . toJiraInlines

toJiraBlocks :: PandocMonad m => [Block] -> JiraConverter m [Jira.Block]
toJiraBlocks blocks = do
  let convert = \case
        BlockQuote bs        -> singleton . Jira.BlockQuote
                                <$> toJiraBlocks bs -- FIXME!
        BulletList items     -> singleton . Jira.List Jira.CircleBullets
                                <$> toJiraItems items
        CodeBlock attr cs    -> toJiraCode attr cs
        DefinitionList items -> toJiraDefinitionList items
        Div attr bs          -> toJiraPanel attr bs
        Header lvl attr xs   -> toJiraHeader lvl attr xs
        HorizontalRule       -> return . singleton $ Jira.HorizontalRule
        LineBlock xs         -> toJiraBlocks [linesToPara xs]
        OrderedList _ items  -> singleton . Jira.List Jira.Enumeration
                                <$> toJiraItems items
        Para xs              -> singleton . Jira.Para <$> toJiraInlines xs
        Plain xs             -> singleton . Jira.Para <$> toJiraInlines xs
        RawBlock fmt cs      -> rawBlockToJira fmt cs
        Null                 -> return mempty
        Table _ _ _ hd body  -> singleton <$> do
          headerRow <- if null hd
                       then Just <$> toRow Jira.HeaderCell hd
                       else pure Nothing
          bodyRows <- mapM (toRow Jira.BodyCell) body
          let rows = case headerRow of
                       Just header -> header : bodyRows
                       Nothing     -> bodyRows
          return $ Jira.Table rows
  jiraBlocks <- mapM convert blocks
  return $ mconcat jiraBlocks

toRow :: PandocMonad m
      => ([Jira.Block] -> Jira.Cell)
      -> [TableCell]
      -> JiraConverter m Jira.Row
toRow mkCell cells = Jira.Row <$>
  mapM (fmap mkCell . toJiraBlocks) cells

toJiraItems :: PandocMonad m => [[Block]] -> JiraConverter m [[Jira.Block]]
toJiraItems = mapM toJiraBlocks

toJiraCode :: PandocMonad m
           => Attr
           -> Text
           -> JiraConverter m [Jira.Block]
toJiraCode (ident, classes, _attribs) code = do
  let lang = case find (\c -> T.toLower c `elem` knownLanguages) classes of
               Nothing -> Jira.Language "java"
               Just l  -> Jira.Language l
  let addAnchor b = if T.null ident
                    then b
                    else [Jira.Para (singleton (Jira.Anchor ident))] <> b
  return . addAnchor . singleton $ Jira.Code lang mempty code

-- | Creates a Jira definition list
toJiraDefinitionList :: PandocMonad m
                     => [([Inline], [[Block]])]
                     -> JiraConverter m [Jira.Block]
toJiraDefinitionList defItems = do
  let convertDefItem (term, defs) = do
        jiraTerm <- Jira.Para <$> styled Jira.Strong term
        jiraDefs <- mconcat <$> mapM toJiraBlocks defs
        return $ jiraTerm : jiraDefs
  singleton . Jira.List Jira.CircleBullets <$> mapM convertDefItem defItems

-- | Creates a Jira panel
toJiraPanel :: PandocMonad m
            => Attr -> [Block]
            -> JiraConverter m [Jira.Block]
toJiraPanel attr blocks = do
  jiraBlocks <- toJiraBlocks blocks
  return $ if attr == nullAttr
           then jiraBlocks
           else singleton (Jira.Panel [] jiraBlocks)

-- | Creates a Jira header
toJiraHeader :: PandocMonad m
             => Int -> Attr -> [Inline]
             -> JiraConverter m [Jira.Block]
toJiraHeader lvl (ident, _, _) inlines =
  let anchor = Jira.Anchor ident
  in singleton . Jira.Header lvl . (anchor :) <$> toJiraInlines inlines

-- | Handles raw block. Jira is included verbatim, everything else is
-- discarded.
rawBlockToJira :: PandocMonad m
               => Format -> Text
               -> JiraConverter m [Jira.Block]
rawBlockToJira fmt cs = do
  rawInlines <- toJiraRaw fmt cs
  return $
    if null rawInlines
    then mempty
    else singleton (Jira.Para rawInlines)

toJiraRaw :: PandocMonad m
          => Format -> Text -> JiraConverter m [Jira.Inline]
toJiraRaw fmt cs = case fmt of
  Format "jira" -> return . singleton $ Jira.Str cs
  _             -> return mempty


--
-- Inlines
--

toJiraInlines :: PandocMonad m => [Inline] -> JiraConverter m [Jira.Inline]
toJiraInlines inlines = do
  let convert = \case
        Cite _ xs          -> toJiraInlines xs
        Code _ cs          -> return . singleton $
                              Jira.Monospaced (escapeSpecialChars cs)
        Emph xs            -> styled Jira.Emphasis xs
        Image _ _ (src, _) -> pure . singleton $ Jira.Image [] (Jira.URL src)
        LineBreak          -> pure . singleton $ Jira.Linebreak
        Link _ xs (tgt, _) -> singleton . flip Jira.Link (Jira.URL tgt)
                              <$> toJiraInlines xs
        Math mtype cs      -> mathToJira mtype cs
        Note bs            -> registerNotes bs
        Quoted qt xs       -> quotedToJira qt xs
        RawInline fmt cs   -> toJiraRaw fmt cs
        SmallCaps xs       -> styled Jira.Strong xs
        SoftBreak          -> do
                                preserveBreak <- asks (== WrapPreserve)
                                pure . singleton $ if preserveBreak
                                  then Jira.Linebreak
                                  else Jira.Space
        Space              -> pure . singleton $ Jira.Space
        Span _attr xs      -> toJiraInlines xs
        Str s              -> pure $ escapeSpecialChars s
        Strikeout xs       -> styled Jira.Strikeout xs
        Strong xs          -> styled Jira.Strong xs
        Subscript xs       -> styled Jira.Subscript xs
        Superscript xs     -> styled Jira.Superscript xs
  jiraInlines <- mapM convert inlines
  return $ mconcat jiraInlines

singleton :: a -> [a]
singleton = (:[])

styled :: PandocMonad m
       => Jira.InlineStyle -> [Inline]
       -> JiraConverter m [Jira.Inline]
styled s = fmap (singleton . Jira.Styled s) . toJiraInlines

-- | Converts a plain text value to Jira inlines, ensuring that all
-- special characters will be handled appropriately.
escapeSpecialChars :: Text -> [Jira.Inline]
escapeSpecialChars t = case plainText t of
  Right xs -> xs
  Left _  -> singleton $ Jira.Str t

mathToJira :: PandocMonad m
           => MathType
           -> Text
           -> JiraConverter m [Jira.Inline]
mathToJira mtype cs = do
  mathInlines <- toJiraInlines =<< texMathToInlines mtype cs
  return $ case mtype of
    InlineMath  -> mathInlines
    DisplayMath -> Jira.Linebreak : mathInlines ++ [Jira.Linebreak]

quotedToJira :: PandocMonad m
             => QuoteType
             -> [Inline]
             -> JiraConverter m [Jira.Inline]
quotedToJira qtype xs = do
  let quoteChar = case qtype of
                    DoubleQuote -> "\""
                    SingleQuote -> "'"
  let surroundWithQuotes = (Jira.Str quoteChar :) . (++ [Jira.Str quoteChar])
  surroundWithQuotes <$> toJiraInlines xs

registerNotes :: PandocMonad m => [Block] -> JiraConverter m [Jira.Inline]
registerNotes contents = do
  curNotes <- gets stNotes
  let newnum = length curNotes + 1
  contents' <- blockListToJira contents
  let thisnote = "\\[" <> T.pack (show newnum) <> "] " <> contents' <> "\n"
  modify $ \s -> s { stNotes = thisnote : curNotes }
  return . singleton . Jira.Str $
    "[" <> T.pack (show newnum) <> "]"

-- | Language codes recognized by jira
knownLanguages :: [Text]
knownLanguages =
  [ "actionscript", "ada", "applescript", "bash", "c", "c#", "c++"
  , "css", "erlang", "go", "groovy", "haskell", "html", "javascript"
  , "json", "lua", "nyan", "objc", "perl", "php", "python", "r", "ruby"
  , "scala", "sql", "swift", "visualbasic", "xml", "yaml"
  ]
