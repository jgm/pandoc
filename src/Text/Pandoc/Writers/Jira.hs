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
import Control.Monad.State.Strict
import Data.Char (toLower)
import Data.Foldable (find)
import Data.Text (Text, pack)
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging (LogMessage (BlockNotRendered, InlineNotRendered))
import Text.Pandoc.Options (WriterOptions (writerTemplate))
import Text.Pandoc.Shared (blocksToInlines, linesToPara)
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Math (texMathToInlines)
import Text.Pandoc.Writers.Shared (metaToContext, defField)
import qualified Data.Text as T
import Text.DocLayout (literal, render)

data WriterState = WriterState
  { stNotes     :: [Text]      -- Footnotes
  , stListLevel :: Text        -- String at beginning of list items, e.g. "**"
  }

-- | Initial writer state
startState :: WriterState
startState = WriterState
  { stNotes = []
  , stListLevel = ""
  }

type JiraWriter = StateT WriterState

-- | Convert Pandoc to Jira.
writeJira :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeJira opts document =
  evalStateT (pandocToJira opts document) startState

-- | Return Jira representation of document.
pandocToJira :: PandocMonad m
             => WriterOptions -> Pandoc -> JiraWriter m Text
pandocToJira opts (Pandoc meta blocks) = do
  metadata <- metaToContext opts
                 (fmap literal . blockListToJira opts)
                 (fmap literal . inlineListToJira opts) meta
  body <- blockListToJira opts blocks
  notes <- gets $ T.intercalate "\n" . reverse . stNotes
  let main = body <> if T.null notes
                        then mempty
                        else T.pack "\n\n" <> notes
  let context = defField "body" main metadata
  return $
    case writerTemplate opts of
      Nothing  -> main
      Just tpl -> render Nothing $ renderTemplate tpl context

-- | Escape one character as needed for Jira.
escapeCharForJira :: Char -> Text
escapeCharForJira c = case c of
  '&'      -> "&amp;"
  '<'      -> "&lt;"
  '>'      -> "&gt;"
  '"'      -> "&quot;"
  '*'      -> "&ast;"
  '_'      -> "&lowbar;"
  '@'      -> "&commat;"
  '+'      -> "&plus;"
  '-'      -> "&hyphen;"
  '|'      -> "&vert;"
  '{'      -> "\\{"
  '\x2014' -> " -- "
  '\x2013' -> " - "
  '\x2019' -> "'"
  '\x2026' -> "..."
  _        -> T.singleton c

-- | Escape string as needed for Jira.
escapeStringForJira :: Text -> Text
escapeStringForJira = T.concatMap escapeCharForJira

-- | Create an anchor macro from the given element attributes.
anchor :: Attr -> Text
anchor (ident,_,_) =
  if ident == ""
  then ""
  else "{anchor:" <> pack ident <> "}"

-- | Append a newline character unless we are in a list.
appendNewlineUnlessInList :: PandocMonad m
                          => Text
                          -> JiraWriter m Text
appendNewlineUnlessInList t = do
  listLevel <- gets stListLevel
  return (if T.null listLevel then t <> "\n" else t)

-- | Convert Pandoc block element to Jira.
blockToJira :: PandocMonad m
            => WriterOptions -- ^ Options
            -> Block         -- ^ Block element
            -> JiraWriter m Text

blockToJira _ Null = return ""

blockToJira opts (Div attr bs) =
  (anchor attr <>) <$> blockListToJira opts bs

blockToJira opts (Plain inlines) =
  inlineListToJira opts inlines

blockToJira opts (Para inlines) = do
  contents <- inlineListToJira opts inlines
  appendNewlineUnlessInList contents

blockToJira opts (LineBlock lns) =
  blockToJira opts $ linesToPara lns

blockToJira _ b@(RawBlock f str) =
  if f == Format "jira"
  then return (pack str)
  else "" <$ report (BlockNotRendered b)

blockToJira _ HorizontalRule = return "----\n"

blockToJira opts (Header level attr inlines) = do
  contents <- inlineListToJira opts inlines
  let prefix = "h" <> pack (show level) <> ". "
  return $ prefix <> anchor attr <> contents <> "\n"

blockToJira _ (CodeBlock attr@(_,classes,_) str) = do
  let lang = find (\c -> map toLower c `elem` knownLanguages) classes
  let start = case lang of
                Nothing -> "{code}"
                Just l  -> "{code:" <> pack l <> "}"
  let anchorMacro = anchor attr
  appendNewlineUnlessInList . T.intercalate "\n" $
    (if anchorMacro == "" then id else (anchorMacro :))
    [start, pack str, "{code}"]

blockToJira opts (BlockQuote [p@(Para _)]) = do
  contents <- blockToJira opts p
  appendNewlineUnlessInList ("bq. " <> contents)

blockToJira opts (BlockQuote blocks) = do
  contents <- blockListToJira opts blocks
  appendNewlineUnlessInList . T.intercalate "\n" $
    [ "{quote}", contents, "{quote}"]

blockToJira opts (Table _caption _aligns _widths headers rows) = do
  headerCells <- mapM blocksToCell headers
  bodyRows    <- mapM (mapM blocksToCell) rows
  let tblHead = headerCellsToRow headerCells
  let tblBody = map cellsToRow bodyRows
  return $ if all null headers
           then T.unlines tblBody
           else T.unlines (tblHead : tblBody)
 where
  blocksToCell :: PandocMonad m => [Block] -> JiraWriter m Text
  blocksToCell = inlineListToJira opts . blocksToInlines

  cellsToRow :: [Text] -> Text
  cellsToRow cells = "|" <> T.intercalate "|" cells <> "|"

  headerCellsToRow :: [Text] -> Text
  headerCellsToRow cells = "||" <> T.intercalate "||" cells <> "||"

blockToJira opts (BulletList items) =
  listWithMarker opts items '*'

blockToJira opts (OrderedList _listAttr items) =
  listWithMarker opts items '#'

blockToJira opts (DefinitionList items) =
  blockToJira opts (BulletList (map defToBulletItem items))
 where
  defToBulletItem :: ([Inline], [[Block]]) -> [Block]
  defToBulletItem (inlns, defs) =
    let term = Plain [Strong inlns]
        blks = mconcat defs
    in term : blks

-- Auxiliary functions for lists:

-- | Create a list using the given character as bullet item marker.
listWithMarker :: PandocMonad m
               => WriterOptions
               -> [[Block]]
               -> Char
               -> JiraWriter m Text
listWithMarker opts items marker = do
  modify $ \s -> s { stListLevel = stListLevel s `T.snoc` marker }
  contents <- mapM (listItemToJira opts) items
  modify $ \s -> s { stListLevel = T.init (stListLevel s) }
  appendNewlineUnlessInList $ T.intercalate "\n" contents

-- | Convert bullet or ordered list item (list of blocks) to Jira.
listItemToJira :: PandocMonad m
               => WriterOptions
               -> [Block]
               -> JiraWriter m Text
listItemToJira opts items = do
  contents <- blockListToJira opts items
  marker <- gets stListLevel
  return $ marker <> " " <> contents

-- | Convert list of Pandoc block elements to Jira.
blockListToJira :: PandocMonad m
                => WriterOptions -- ^ Options
                -> [Block]       -- ^ List of block elements
                -> JiraWriter m Text
blockListToJira opts blocks =
  T.intercalate "\n" <$> mapM (blockToJira opts) blocks

-- | Convert list of Pandoc inline elements to Jira.
inlineListToJira :: PandocMonad m
                 => WriterOptions
                 -> [Inline]
                 -> JiraWriter m Text
inlineListToJira opts lst =
  T.concat <$> mapM (inlineToJira opts) lst

-- | Convert Pandoc inline element to Jira.
inlineToJira :: PandocMonad m
             => WriterOptions
             -> Inline
             -> JiraWriter m Text

inlineToJira opts (Span attr lst) =
  (anchor attr <>) <$> inlineListToJira opts lst

inlineToJira opts (Emph lst) = do
  contents <- inlineListToJira opts lst
  return $ "_" <> contents <> "_"

inlineToJira opts (Strong lst) = do
  contents <- inlineListToJira opts lst
  return $ "*" <> contents <> "*"

inlineToJira opts (Strikeout lst) = do
  contents <- inlineListToJira opts lst
  return $ "-" <> contents <> "-"

inlineToJira opts (Superscript lst) = do
  contents <- inlineListToJira opts lst
  return $ "{^" <> contents <> "^}"

inlineToJira opts (Subscript lst) = do
  contents <- inlineListToJira opts lst
  return $ "{~" <> contents <> "~}"

inlineToJira opts (SmallCaps lst) = inlineListToJira opts lst

inlineToJira opts (Quoted SingleQuote lst) = do
  contents <- inlineListToJira opts lst
  return $ "'" <> contents <> "'"

inlineToJira opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToJira opts lst
  return $ "\"" <> contents <> "\""

inlineToJira opts (Cite _  lst) = inlineListToJira opts lst

inlineToJira _ (Code attr str) =
  return (anchor attr <> "{{" <> pack str <> "}}")

inlineToJira _ (Str str) = return $ escapeStringForJira (pack str)

inlineToJira opts (Math InlineMath str) =
  lift (texMathToInlines InlineMath str) >>= inlineListToJira opts

inlineToJira opts (Math DisplayMath str) = do
  mathInlines <- lift (texMathToInlines DisplayMath str)
  contents    <- inlineListToJira opts mathInlines
  return $ "\\\\" <> contents <> "\\\\"

inlineToJira _opts il@(RawInline f str) =
  if f == Format "jira"
  then return (pack str)
  else "" <$ report (InlineNotRendered il)

inlineToJira _ LineBreak = return "\n"

inlineToJira _ SoftBreak = return " "

inlineToJira _ Space = return " "

inlineToJira opts (Link _attr txt (src, _title)) = do
  linkText <- inlineListToJira opts txt
  return $ T.concat
    [ "["
    , if null txt then "" else linkText <> "|"
    , pack src
    , "]"
    ]

inlineToJira _opts (Image attr _alt (src, _title)) =
  return . T.concat $ [anchor attr, "!", pack src, "!"]

inlineToJira opts (Note contents) = do
  curNotes <- gets stNotes
  let newnum = length curNotes + 1
  contents' <- blockListToJira opts contents
  let thisnote = "[" <> pack (show newnum) <> "] " <> contents' <> "\n"
  modify $ \s -> s { stNotes = thisnote : curNotes }
  return $ "[" <> pack (show newnum) <> "]"

-- | Language codes recognized by jira
knownLanguages :: [String]
knownLanguages =
  [ "actionscript", "ada", "applescript", "bash", "c", "c#", "c++"
  , "css", "erlang", "go", "groovy", "haskell", "html", "javascript"
  , "json", "lua", "nyan", "objc", "perl", "php", "python", "r", "ruby"
  , "scala", "sql", "swift", "visualbasic", "xml", "yaml"
  ]
