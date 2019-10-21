{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{- |
   Module      : Text.Pandoc.Writers.Markdown
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to markdown-formatted plain text.

Markdown:  <http://daringfireball.net/projects/markdown/>
-}
module Text.Pandoc.Writers.Markdown (writeMarkdown, writePlain) where
import Prelude
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Char (isSpace, isAlphaNum)
import Data.Default
import Data.List (find, group, intersperse, sortBy, stripPrefix, transpose,
                  isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, catMaybes)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP (urlEncode)
import Text.HTML.TagSoup (Tag (..), isTagText, parseTags)
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (blankline, blanklines, char, space)
import Text.DocLayout
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.DocTemplates (Val(..), Context(..), FromContext(..))
import Text.Pandoc.Walk
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Writers.Math (texMathToInlines)
import Text.Pandoc.XML (toHtml5Entities)

type Notes = [[Block]]
type Ref   = (String, Target, Attr)
type Refs  = [Ref]

type MD m = ReaderT WriterEnv (StateT WriterState m)

evalMD :: PandocMonad m => MD m a -> WriterEnv -> WriterState -> m a
evalMD md env st = evalStateT (runReaderT md env) st

data WriterEnv = WriterEnv { envInList          :: Bool
                           , envPlain           :: Bool
                           , envRefShortcutable :: Bool
                           , envBlockLevel      :: Int
                           , envEscapeSpaces    :: Bool
                           }

instance Default WriterEnv
  where def = WriterEnv { envInList         = False
                        , envPlain          = False
                        , envRefShortcutable = True
                        , envBlockLevel      = 0
                        , envEscapeSpaces    = False
                        }

data WriterState = WriterState { stNotes   :: Notes
                               , stPrevRefs :: Refs
                               , stRefs    :: Refs
                               , stKeys    :: M.Map Key
                                                (M.Map (Target, Attr) Int)
                               , stLastIdx  :: Int
                               , stIds     :: Set.Set String
                               , stNoteNum :: Int
                               }

instance Default WriterState
  where def = WriterState{ stNotes = []
                         , stPrevRefs = []
                         , stRefs = []
                         , stKeys = M.empty
                         , stLastIdx = 0
                         , stIds = Set.empty
                         , stNoteNum = 1
                         }

-- | Convert Pandoc to Markdown.
writeMarkdown :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeMarkdown opts document =
  evalMD (pandocToMarkdown opts{
             writerWrapText = if isEnabled Ext_hard_line_breaks opts
                              then WrapNone
                              else writerWrapText opts }
             document) def def

-- | Convert Pandoc to plain text (like markdown, but without links,
-- pictures, or inline formatting).
writePlain :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writePlain opts document =
  evalMD (pandocToMarkdown opts document) def{ envPlain = True } def

pandocTitleBlock :: Doc Text -> [Doc Text] -> Doc Text -> Doc Text
pandocTitleBlock tit auths dat =
  hang 2 (text "% ") tit <> cr <>
  hang 2 (text "% ") (vcat $ map nowrap auths) <> cr <>
  hang 2 (text "% ") dat <> cr

mmdTitleBlock :: Context Text -> Doc Text
mmdTitleBlock (Context hashmap) =
  vcat $ map go $ sortBy (comparing fst) $ M.toList hashmap
  where go (k,v) =
          case (text (T.unpack k), v) of
               (k', ListVal xs)
                 | null xs        -> empty
                 | otherwise      -> k' <> ":" <> space <>
                                      hcat (intersperse "; " $
                                          catMaybes $ map fromVal xs)
               (k', SimpleVal x)
                      | isEmpty x -> empty
                      | otherwise -> k' <> ":" <> space <>
                                     nest 2 (chomp (removeBlankLines x))
               _                  -> empty
        removeBlankLines BlankLines{} = cr <> text "." <> cr
        removeBlankLines (Concat x y) = removeBlankLines x <>
                                        removeBlankLines y
        removeBlankLines x            = x

plainTitleBlock :: Doc Text -> [Doc Text] -> Doc Text -> Doc Text
plainTitleBlock tit auths dat =
  tit <> cr <>
  (hcat (intersperse (text "; ") auths)) <> cr <>
  dat <> cr

yamlMetadataBlock :: Context Text -> Doc Text
yamlMetadataBlock v = "---" $$ (contextToYaml v) $$ "---"

contextToYaml :: Context Text -> Doc Text
contextToYaml (Context o) =
  vcat $ map keyvalToYaml $ sortBy (comparing fst) $ M.toList o
 where
  keyvalToYaml (k,v) =
          case (text (T.unpack k), v) of
               (k', ListVal vs)
                 | null vs        -> empty
                 | otherwise      -> (k' <> ":") $$ valToYaml v
               (k', MapVal (Context m))
                 | M.null m       -> k' <> ": {}"
                 | otherwise      -> (k' <> ":") $$ nest 2 (valToYaml v)
               (_, SimpleVal x)
                     | isEmpty x  -> empty
               (_, NullVal)       -> empty
               (k', _)            -> k' <> ":" <+> hang 2 "" (valToYaml v)

valToYaml :: Val Text -> Doc Text
valToYaml (ListVal xs) =
  vcat $ map (\v -> hang 2 "- " (valToYaml v)) xs
valToYaml (MapVal c) = contextToYaml c
valToYaml (SimpleVal x)
  | isEmpty x = empty
  | otherwise =
      if hasNewlines x
         then hang 0 ("|" <> cr) x
         else if any hasPunct x
           then "'" <> fmap escapeSingleQuotes x <> "'"
           else x
    where
      hasNewlines NewLine = True
      hasNewlines BlankLines{} = True
      hasNewlines CarriageReturn = True
      hasNewlines (Concat w z) = hasNewlines w || hasNewlines z
      hasNewlines _ = False
      hasPunct = T.any isYamlPunct
      isYamlPunct = (`elem` ['-','?',':',',','[',']','{','}',
                             '#','&','*','!','|','>','\'','"',
                             '%','@','`',',','[',']','{','}'])
      escapeSingleQuotes = T.replace "'" "''"
valToYaml _ = empty

-- | Return markdown representation of document.
pandocToMarkdown :: PandocMonad m => WriterOptions -> Pandoc -> MD m Text
pandocToMarkdown opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  isPlain <- asks envPlain
  metadata <- metaToContext'
               (blockListToMarkdown opts)
               (inlineListToMarkdown opts)
               meta
  let title' = maybe empty id $ getField "title" metadata
  let authors' = maybe [] id $ getField "author" metadata
  let date' = maybe empty id $ getField "date" metadata
  let titleblock = case writerTemplate opts of
                        Just _ | isPlain ->
                                 plainTitleBlock title' authors' date'
                               | isEnabled Ext_yaml_metadata_block opts ->
                                   yamlMetadataBlock metadata
                               | isEnabled Ext_pandoc_title_block opts ->
                                   pandocTitleBlock title' authors' date'
                               | isEnabled Ext_mmd_title_block opts ->
                                   mmdTitleBlock metadata
                               | otherwise -> empty
                        Nothing -> empty
  let headerBlocks = filter isHeaderBlock blocks
  toc <- if writerTableOfContents opts
         then blockToMarkdown opts ( toTableOfContents opts headerBlocks )
         else return mempty
  -- Strip off final 'references' header if markdown citations enabled
  let blocks' = if isEnabled Ext_citations opts
                   then case reverse blocks of
                             (Div ("refs",_,_) _):xs -> reverse xs
                             _                       -> blocks
                   else blocks
  body <- blockListToMarkdown opts blocks'
  notesAndRefs' <- notesAndRefs opts
  let main = body <> notesAndRefs'
  let context  = -- for backwards compatibility we populate toc
                 -- with the contents of the toc, rather than a
                 -- boolean:
                 defField "toc" toc
               $ defField "table-of-contents" toc
               $ defField "body" main
               $ (if isNullMeta meta
                     then id
                     else defField "titleblock" titleblock)
               $ addVariablesToContext opts metadata
  return $ render colwidth $
    case writerTemplate opts of
       Nothing  -> main
       Just tpl -> renderTemplate tpl context

-- | Return markdown representation of reference key table.
refsToMarkdown :: PandocMonad m => WriterOptions -> Refs -> MD m (Doc Text)
refsToMarkdown opts refs = mapM (keyToMarkdown opts) refs >>= return . vcat

-- | Return markdown representation of a reference key.
keyToMarkdown :: PandocMonad m
              => WriterOptions
              -> Ref
              -> MD m (Doc Text)
keyToMarkdown opts (label', (src, tit), attr) = do
  let tit' = if null tit
                then empty
                else space <> "\"" <> text tit <> "\""
  return $ nest 2 $ hang 2
            ("[" <> text label' <> "]:" <> space) (text src <> tit')
            <+> linkAttributes opts attr

-- | Return markdown representation of notes.
notesToMarkdown :: PandocMonad m => WriterOptions -> [[Block]] -> MD m (Doc Text)
notesToMarkdown opts notes = do
  n <- gets stNoteNum
  notes' <- mapM (\(num, note) -> noteToMarkdown opts num note) (zip [n..] notes)
  modify $ \st -> st { stNoteNum = stNoteNum st + length notes }
  return $ vsep notes'

-- | Return markdown representation of a note.
noteToMarkdown :: PandocMonad m => WriterOptions -> Int -> [Block] -> MD m (Doc Text)
noteToMarkdown opts num blocks = do
  contents  <- blockListToMarkdown opts blocks
  let num' = text $ writerIdentifierPrefix opts ++ show num
  let marker = if isEnabled Ext_footnotes opts
                  then text "[^" <> num' <> text "]:"
                  else text "[" <> num' <> text "]"
  let markerSize = 4 + offset num'
  let spacer = case writerTabStop opts - markerSize of
                     n | n > 0  -> text $ replicate n ' '
                     _ -> text " "
  return $ if isEnabled Ext_footnotes opts
              then hang (writerTabStop opts) (marker <> spacer) contents
              else marker <> spacer <> contents

-- | Escape special characters for Markdown.
escapeString :: WriterOptions -> String -> String
escapeString opts =
  (if writerPreferAscii opts
      then T.unpack . toHtml5Entities . T.pack
      else id) . go
  where
  go [] = []
  go (c:cs) =
    case c of
       '<' | isEnabled Ext_all_symbols_escapable opts ->
              '\\' : '<' : go cs
           | otherwise -> "&lt;" ++ go cs
       '>' | isEnabled Ext_all_symbols_escapable opts ->
              '\\' : '>' : go cs
           | otherwise -> "&gt;" ++ go cs
       '@' | isEnabled Ext_citations opts ->
               case cs of
                    (d:_)
                      | isAlphaNum d || d == '_'
                         -> '\\':'@':go cs
                    _ -> '@':go cs
       _ | c `elem` ['\\','`','*','_','[',']','#'] ->
              '\\':c:go cs
       '|' | isEnabled Ext_pipe_tables opts -> '\\':'|':go cs
       '^' | isEnabled Ext_superscript opts -> '\\':'^':go cs
       '~' | isEnabled Ext_subscript opts ||
             isEnabled Ext_strikeout opts -> '\\':'~':go cs
       '$' | isEnabled Ext_tex_math_dollars opts -> '\\':'$':go cs
       '\'' | isEnabled Ext_smart opts -> '\\':'\'':go cs
       '"' | isEnabled Ext_smart opts -> '\\':'"':go cs
       '-' | isEnabled Ext_smart opts ->
              case cs of
                   '-':_ -> '\\':'-':go cs
                   _     -> '-':go cs
       '.' | isEnabled Ext_smart opts ->
              case cs of
                   '.':'.':rest -> '\\':'.':'.':'.':go rest
                   _            -> '.':go cs
       _   -> c : go cs

attrsToMarkdown :: Attr -> Doc Text
attrsToMarkdown attribs = braces $ hsep [attribId, attribClasses, attribKeys]
        where attribId = case attribs of
                                ([],_,_) -> empty
                                (i,_,_)  -> "#" <> escAttr i
              attribClasses = case attribs of
                                (_,[],_) -> empty
                                (_,cs,_) -> hsep $
                                            map (escAttr . ('.':))
                                            cs
              attribKeys = case attribs of
                                (_,_,[]) -> empty
                                (_,_,ks) -> hsep $
                                            map (\(k,v) -> escAttr k
                                              <> "=\"" <>
                                              escAttr v <> "\"") ks
              escAttr          = mconcat . map escAttrChar
              escAttrChar '"'  = text "\\\""
              escAttrChar '\\' = text "\\\\"
              escAttrChar c    = text [c]

linkAttributes :: WriterOptions -> Attr -> Doc Text
linkAttributes opts attr =
  if isEnabled Ext_link_attributes opts && attr /= nullAttr
     then attrsToMarkdown attr
     else empty

-- | Ordered list start parser for use in Para below.
olMarker :: Parser [Char] ParserState Char
olMarker = do (start, style', delim) <- anyOrderedListMarker
              if delim == Period &&
                          (style' == UpperAlpha || (style' == UpperRoman &&
                          start `elem` [1, 5, 10, 50, 100, 500, 1000]))
                          then spaceChar >> spaceChar
                          else spaceChar

-- | True if string begins with an ordered list marker
beginsWithOrderedListMarker :: String -> Bool
beginsWithOrderedListMarker str =
  case runParser olMarker defaultParserState "para start" (take 10 str) of
         Left  _ -> False
         Right _ -> True

notesAndRefs :: PandocMonad m => WriterOptions -> MD m (Doc Text)
notesAndRefs opts = do
  notes' <- reverse <$> gets stNotes >>= notesToMarkdown opts
  modify $ \s -> s { stNotes = [] }
  refs' <- reverse <$> gets stRefs >>= refsToMarkdown opts
  modify $ \s -> s { stPrevRefs = stPrevRefs s ++ stRefs s
                   , stRefs = []}

  let endSpacing =
        if | writerReferenceLocation opts == EndOfDocument -> empty
           | isEmpty notes' && isEmpty refs' -> empty
           | otherwise -> blankline

  return $
    (if isEmpty notes' then empty else blankline <> notes') <>
    (if isEmpty refs' then empty else blankline <> refs') <>
    endSpacing

-- | Convert Pandoc block element to markdown.
blockToMarkdown :: PandocMonad m
                => WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> MD m (Doc Text)
blockToMarkdown opts blk =
  local (\env -> env {envBlockLevel = envBlockLevel env + 1}) $
  do doc <- blockToMarkdown' opts blk
     blkLevel <- asks envBlockLevel
     if writerReferenceLocation opts == EndOfBlock && blkLevel == 1
       then notesAndRefs opts >>= (\d -> return $ doc <> d)
       else return doc

blockToMarkdown' :: PandocMonad m
                 => WriterOptions -- ^ Options
                 -> Block         -- ^ Block element
                 -> MD m (Doc Text)
blockToMarkdown' _ Null = return empty
blockToMarkdown' opts (Div attrs ils) = do
  contents <- blockListToMarkdown opts ils
  return $
    case () of
         _ | isEnabled Ext_fenced_divs opts &&
             attrs /= nullAttr ->
                nowrap (text ":::" <+> attrsToMarkdown attrs) $$
                chomp contents $$
                text ":::" <> blankline
           | isEnabled Ext_native_divs opts ||
             (isEnabled Ext_raw_html opts &&
              isEnabled Ext_markdown_in_html_blocks opts) ->
                tagWithAttrs "div" attrs <> blankline <>
                contents <> blankline <> "</div>" <> blankline
           | isEnabled Ext_raw_html opts &&
             isEnabled Ext_markdown_attribute opts ->
                tagWithAttrs "div" attrs' <> blankline <>
                contents <> blankline <> "</div>" <> blankline
           | otherwise -> contents <> blankline
       where (id',classes',kvs') = attrs
             attrs' = (id',classes',("markdown","1"):kvs')
blockToMarkdown' opts (Plain inlines) = do
  contents <- inlineListToMarkdown opts inlines
  -- escape if para starts with ordered list marker
  isPlain <- asks envPlain
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  let rendered = T.unpack $ render colwidth contents
  let escapeMarker (x:xs) | x `elem` (".()" :: String) = '\\':x:xs
                          | otherwise                  = x : escapeMarker xs
      escapeMarker []                                  = []
  let contents' =
       case rendered of
            '%':_ | isEnabled Ext_pandoc_title_block opts &&
                    isEnabled Ext_all_symbols_escapable opts ->
                    "\\" <> contents
            '+':s:_ | not isPlain && isSpace s -> "\\" <> contents
            '*':s:_ | not isPlain && isSpace s -> "\\" <> contents
            '-':s:_ | not isPlain && isSpace s -> "\\" <> contents
            '+':[]  | not isPlain -> "\\" <> contents
            '*':[]  | not isPlain -> "\\" <> contents
            '-':[]  | not isPlain -> "\\" <> contents
            '|':_ | (isEnabled Ext_line_blocks opts ||
                     isEnabled Ext_pipe_tables opts)
                    && isEnabled Ext_all_symbols_escapable opts
                  -> "\\" <> contents
            _ | not isPlain && beginsWithOrderedListMarker rendered
                  && isEnabled Ext_all_symbols_escapable opts
                  -> text $ escapeMarker rendered
              | otherwise -> contents
  return $ contents' <> cr
-- title beginning with fig: indicates figure
blockToMarkdown' opts (Para [Image attr alt (src,'f':'i':'g':':':tit)])
  | isEnabled Ext_raw_html opts &&
    not (isEnabled Ext_link_attributes opts) &&
    attr /= nullAttr = -- use raw HTML
    ((<> blankline) . text . T.unpack . T.strip) <$>
      writeHtml5String opts{ writerTemplate = Nothing }
        (Pandoc nullMeta [Para [Image attr alt (src,"fig:" ++ tit)]])
  | otherwise = blockToMarkdown opts (Para [Image attr alt (src,tit)])
blockToMarkdown' opts (Para inlines) =
  (<> blankline) `fmap` blockToMarkdown opts (Plain inlines)
blockToMarkdown' opts (LineBlock lns) =
  if isEnabled Ext_line_blocks opts
  then do
    mdLines <- mapM (inlineListToMarkdown opts) lns
    return $ (vcat $ map (hang 2 (text "| ")) mdLines) <> blankline
  else blockToMarkdown opts $ linesToPara lns
blockToMarkdown' opts b@(RawBlock f str) = do
  plain <- asks envPlain
  let Format fmt = f
  let rawAttribBlock = return $
         (text "```{=" <> text fmt <> "}") $$
         text str $$
         (text "```" <> text "\n")
  let renderEmpty = mempty <$ report (BlockNotRendered b)
  case () of
    _ | plain -> renderEmpty
      | isEnabled Ext_raw_attribute opts -> rawAttribBlock
      | f `elem` ["markdown", "markdown_github", "markdown_phpextra",
                  "markdown_mmd", "markdown_strict"] ->
            return $ text str <> text "\n"
      | f `elem` ["html", "html5", "html4"] ->
            case () of
              _ | isEnabled Ext_markdown_attribute opts -> return $
                    text (addMarkdownAttribute str) <> text "\n"
                | isEnabled Ext_raw_html opts -> return $
                    text str <> text "\n"
                | isEnabled Ext_raw_attribute opts -> rawAttribBlock
                | otherwise -> renderEmpty
      | f `elem` ["latex", "tex"] ->
            case () of
              _ | isEnabled Ext_raw_tex opts -> return $
                    text str <> text "\n"
                | isEnabled Ext_raw_attribute opts -> rawAttribBlock
                | otherwise -> renderEmpty
      | otherwise -> renderEmpty
blockToMarkdown' opts HorizontalRule = do
  return $ blankline <> text (replicate (writerColumns opts) '-') <> blankline
blockToMarkdown' opts (Header level attr inlines) = do
  -- first, if we're putting references at the end of a section, we
  -- put them here.
  blkLevel <- asks envBlockLevel
  refs <- if writerReferenceLocation opts == EndOfSection && blkLevel == 1
          then notesAndRefs opts
          else return empty

  plain <- asks envPlain
  -- we calculate the id that would be used by auto_identifiers
  -- so we know whether to print an explicit identifier
  ids <- gets stIds
  let autoId = uniqueIdent (writerExtensions opts) inlines ids
  modify $ \st -> st{ stIds = Set.insert autoId ids }
  let attr' = case attr of
                   ("",[],[]) -> empty
                   (id',[],[]) | isEnabled Ext_auto_identifiers opts
                                 && id' == autoId -> empty
                   (id',_,_)   | isEnabled Ext_mmd_header_identifiers opts ->
                                    space <> brackets (text id')
                   _ | isEnabled Ext_header_attributes opts ->
                                    space <> attrsToMarkdown attr
                     | otherwise -> empty
  contents <- inlineListToMarkdown opts $
                 -- ensure no newlines; see #3736
                 walk lineBreakToSpace $
                 if level == 1 && plain && isEnabled Ext_gutenberg opts
                    then capitalize inlines
                    else inlines
  let setext = writerSetextHeaders opts
      hdr = nowrap $ case level of
            1 | plain ->
                if isEnabled Ext_gutenberg opts
                   then blanklines 3 <> contents <> blanklines 2
                   else contents <> blankline
              | setext ->
                  contents <> attr' <> cr <> text (replicate (offset contents) '=') <>
                  blankline
            2 | plain ->
                if isEnabled Ext_gutenberg opts
                   then blanklines 2 <> contents <> blankline
                   else contents <> blankline
              | setext ->
                  contents <> attr' <> cr <> text (replicate (offset contents) '-') <>
                  blankline
            -- ghc interprets '#' characters in column 1 as linenum specifiers.
            _ | plain || isEnabled Ext_literate_haskell opts ->
                contents <> blankline
            _ -> text (replicate level '#') <> space <> contents <> attr' <> blankline

  return $ refs <> hdr
blockToMarkdown' opts (CodeBlock (_,classes,_) str)
  | "haskell" `elem` classes && "literate" `elem` classes &&
    isEnabled Ext_literate_haskell opts =
  return $ prefixed "> " (text str) <> blankline
blockToMarkdown' opts (CodeBlock attribs str) = return $
  case attribs == nullAttr of
     False | isEnabled Ext_backtick_code_blocks opts ->
          backticks <> attrs <> cr <> text str <> cr <> backticks <> blankline
           | isEnabled Ext_fenced_code_blocks opts ->
          tildes <> attrs <> cr <> text str <> cr <> tildes <> blankline
     _ -> nest (writerTabStop opts) (text str) <> blankline
   where endline c = text $ case [length ln
                                   | ln <- map trim (lines str)
                                   , [c,c,c] `isPrefixOf` ln
                                   , all (== c) ln] of
                               [] -> replicate 3 c
                               xs -> replicate (maximum xs + 1) c
         backticks = endline '`'
         tildes = endline '~'
         attrs  = if isEnabled Ext_fenced_code_attributes opts
                     then nowrap $ " " <> attrsToMarkdown attribs
                     else case attribs of
                                (_,(cls:_),_) -> " " <> text cls
                                _             -> empty
blockToMarkdown' opts (BlockQuote blocks) = do
  plain <- asks envPlain
  -- if we're writing literate haskell, put a space before the bird tracks
  -- so they won't be interpreted as lhs...
  let leader = if isEnabled Ext_literate_haskell opts
                  then " > "
                  else if plain then "  " else "> "
  contents <- blockListToMarkdown opts blocks
  return $ (prefixed leader contents) <> blankline
blockToMarkdown' opts t@(Table caption aligns widths headers rows) =  do
  let numcols = maximum (length aligns : length widths :
                           map length (headers:rows))
  caption' <- inlineListToMarkdown opts caption
  let caption'' = if null caption || not (isEnabled Ext_table_captions opts)
                     then blankline
                     else blankline $$ (": " <> caption') $$ blankline
  let hasSimpleCells = onlySimpleTableCells $ headers:rows
  let isSimple = hasSimpleCells && all (==0) widths
  let isPlainBlock (Plain _) = True
      isPlainBlock _         = False
  let hasBlocks = not (all isPlainBlock $ concat . concat $ headers:rows)
  let padRow r = case numcols - length r of
                       x | x > 0 -> r ++ replicate x empty
                         | otherwise -> r
  let aligns' = case numcols - length aligns of
                     x | x > 0 -> aligns ++ replicate x AlignDefault
                       | otherwise -> aligns
  let widths' = case numcols - length widths of
                     x | x > 0 -> widths ++ replicate x 0.0
                       | otherwise -> widths
  (nst,tbl) <-
     case True of
          _ | isSimple &&
              isEnabled Ext_simple_tables opts -> do
                rawHeaders <- padRow <$> mapM (blockListToMarkdown opts) headers
                rawRows <- mapM (fmap padRow . mapM (blockListToMarkdown opts))
                           rows
                (nest 2,) <$> pandocTable opts False (all null headers)
                                aligns' widths' rawHeaders rawRows
            | isSimple &&
              isEnabled Ext_pipe_tables opts -> do
                rawHeaders <- padRow <$> mapM (blockListToMarkdown opts) headers
                rawRows <- mapM (fmap padRow . mapM (blockListToMarkdown opts))
                           rows
                (id,) <$> pipeTable (all null headers) aligns' rawHeaders rawRows
            | not hasBlocks &&
              isEnabled Ext_multiline_tables opts -> do
                rawHeaders <- padRow <$> mapM (blockListToMarkdown opts) headers
                rawRows <- mapM (fmap padRow . mapM (blockListToMarkdown opts))
                           rows
                (nest 2,) <$> pandocTable opts True (all null headers)
                                aligns' widths' rawHeaders rawRows
            | isEnabled Ext_grid_tables opts &&
               writerColumns opts >= 8 * numcols -> (id,) <$>
                gridTable opts blockListToMarkdown
                  (all null headers) aligns' widths' headers rows
            | hasSimpleCells &&
              isEnabled Ext_pipe_tables opts -> do
                rawHeaders <- padRow <$> mapM (blockListToMarkdown opts) headers
                rawRows <- mapM (fmap padRow . mapM (blockListToMarkdown opts))
                           rows
                (id,) <$> pipeTable (all null headers) aligns' rawHeaders rawRows
            | isEnabled Ext_raw_html opts -> fmap (id,) $
                   (text . T.unpack) <$>
                   (writeHtml5String opts{ writerTemplate = Nothing } $ Pandoc nullMeta [t])
            | otherwise -> return $ (id, text "[TABLE]")
  return $ nst (tbl $$ caption'') $$ blankline
blockToMarkdown' opts (BulletList items) = do
  contents <- inList $ mapM (bulletListItemToMarkdown opts) items
  return $ (if isTightList items then vcat else vsep) contents <> blankline
blockToMarkdown' opts (OrderedList (start,sty,delim) items) = do
  let start' = if isEnabled Ext_startnum opts then start else 1
  let sty'   = if isEnabled Ext_fancy_lists opts then sty else DefaultStyle
  let delim' = if isEnabled Ext_fancy_lists opts then delim else DefaultDelim
  let attribs = (start', sty', delim')
  let markers  = orderedListMarkers attribs
  let markers' = map (\m -> if length m < 3
                               then m ++ replicate (3 - length m) ' '
                               else m) markers
  contents <- inList $
              mapM (\(item, num) -> orderedListItemToMarkdown opts item num) $
              zip markers' items
  return $ (if isTightList items then vcat else vsep) contents <> blankline
blockToMarkdown' opts (DefinitionList items) = do
  contents <- inList $ mapM (definitionListItemToMarkdown opts) items
  return $ mconcat contents <> blankline

inList :: Monad m => MD m a -> MD m a
inList p = local (\env -> env {envInList = True}) p

addMarkdownAttribute :: String -> String
addMarkdownAttribute s =
  case span isTagText $ reverse $ parseTags s of
       (xs,(TagOpen t attrs:rest)) ->
            renderTags' $ reverse rest ++ (TagOpen t attrs' : reverse xs)
              where attrs' = ("markdown","1"):[(x,y) | (x,y) <- attrs,
                                 x /= "markdown"]
       _ -> s

pipeTable :: PandocMonad m
          => Bool -> [Alignment] -> [Doc Text] -> [[Doc Text]]
          -> MD m (Doc Text)
pipeTable headless aligns rawHeaders rawRows = do
  let sp = text " "
  let blockFor AlignLeft   x y = lblock (x + 2) (sp <> y) <> lblock 0 empty
      blockFor AlignCenter x y = cblock (x + 2) (sp <> y) <> lblock 0 empty
      blockFor AlignRight  x y = rblock (x + 2) (sp <> y) <> lblock 0 empty
      blockFor _           x y = lblock (x + 2) (sp <> y) <> lblock 0 empty
  let widths = map (max 3 . maximum . map offset) $ transpose (rawHeaders : rawRows)
  let torow cs = nowrap $ text "|" <>
                    hcat (intersperse (text "|") $
                          zipWith3 blockFor aligns widths (map chomp cs))
                    <> text "|"
  let toborder (a, w) = text $ case a of
                             AlignLeft    -> ':':replicate (w + 1) '-'
                             AlignCenter  -> ':':replicate w '-' ++ ":"
                             AlignRight   -> replicate (w + 1) '-' ++ ":"
                             AlignDefault -> replicate (w + 2) '-'
  -- note:  pipe tables can't completely lack a
  -- header; for a headerless table, we need a header of empty cells.
  -- see jgm/pandoc#1996.
  let header = if headless
                  then torow (replicate (length aligns) empty)
                  else torow rawHeaders
  let border = nowrap $ text "|" <> hcat (intersperse (text "|") $
                        map toborder $ zip aligns widths) <> text "|"
  let body   = vcat $ map torow rawRows
  return $ header $$ border $$ body

pandocTable :: PandocMonad m
            => WriterOptions -> Bool -> Bool -> [Alignment] -> [Double]
            -> [Doc Text] -> [[Doc Text]] -> MD m (Doc Text)
pandocTable opts multiline headless aligns widths rawHeaders rawRows = do
  let isSimple = all (==0) widths
  let alignHeader alignment = case alignment of
                                AlignLeft    -> lblock
                                AlignCenter  -> cblock
                                AlignRight   -> rblock
                                AlignDefault -> lblock
  -- Number of characters per column necessary to output every cell
  -- without requiring a line break.
  -- The @+2@ is needed for specifying the alignment.
  let numChars    = (+ 2) . maximum . map offset
  -- Number of characters per column necessary to output every cell
  -- without requiring a line break *inside a word*.
  -- The @+2@ is needed for specifying the alignment.
  let minNumChars = (+ 2) . maximum . map minOffset
  let columns = transpose (rawHeaders : rawRows)
  -- minimal column width without wrapping a single word
  let relWidth w col =
         max (floor $ fromIntegral (writerColumns opts - 1) * w)
             (if writerWrapText opts == WrapAuto
                 then minNumChars col
                 else numChars col)
  let widthsInChars
        | isSimple  = map numChars columns
        | otherwise = zipWith relWidth widths columns
  let makeRow = hcat . intersperse (lblock 1 (text " ")) .
                   (zipWith3 alignHeader aligns widthsInChars)
  let rows' = map makeRow rawRows
  let head' = makeRow rawHeaders
  let underline = mconcat $ intersperse (text " ") $
                  map (\width -> text (replicate width '-')) widthsInChars
  let border = if multiline
                  then text (replicate (sum widthsInChars +
                          length widthsInChars - 1) '-')
                  else if headless
                          then underline
                          else empty
  let head'' = if headless
                  then empty
                  else border <> cr <> head'
  let body = if multiline
                then vsep rows' $$
                     if length rows' < 2
                        then blankline -- #4578
                        else empty
                else vcat rows'
  let bottom = if headless
                  then underline
                  else border
  return $ head'' $$ underline $$ body $$ bottom

itemEndsWithTightList :: [Block] -> Bool
itemEndsWithTightList bs =
  case bs of
        [Plain _, BulletList xs]    -> isTightList xs
        [Plain _, OrderedList _ xs] -> isTightList xs
        _                           -> False

-- | Convert bullet list item (list of blocks) to markdown.
bulletListItemToMarkdown :: PandocMonad m => WriterOptions -> [Block] -> MD m (Doc Text)
bulletListItemToMarkdown opts bs = do
  let exts = writerExtensions opts
  contents <- blockListToMarkdown opts $ taskListItemToAscii exts bs
  let sps = replicate (writerTabStop opts - 2) ' '
  let start = text ('-' : ' ' : sps)
  -- remove trailing blank line if item ends with a tight list
  let contents' = if itemEndsWithTightList bs
                     then chomp contents <> cr
                     else contents
  return $ hang (writerTabStop opts) start $ contents'

-- | Convert ordered list item (a list of blocks) to markdown.
orderedListItemToMarkdown :: PandocMonad m
                          => WriterOptions -- ^ options
                          -> String        -- ^ list item marker
                          -> [Block]       -- ^ list item (list of blocks)
                          -> MD m (Doc Text)
orderedListItemToMarkdown opts marker bs = do
  let exts = writerExtensions opts
  contents <- blockListToMarkdown opts $ taskListItemToAscii exts bs
  let sps = case writerTabStop opts - length marker of
                   n | n > 0 -> text $ replicate n ' '
                   _ -> text " "
  let ind = if isEnabled Ext_four_space_rule opts
               then writerTabStop opts
               else max (writerTabStop opts) (length marker + 1)
  let start = text marker <> sps
  -- remove trailing blank line if item ends with a tight list
  let contents' = if itemEndsWithTightList bs
                     then chomp contents <> cr
                     else contents
  return $ hang ind start $ contents'

-- | Convert definition list item (label, list of blocks) to markdown.
definitionListItemToMarkdown :: PandocMonad m
                             => WriterOptions
                             -> ([Inline],[[Block]])
                             -> MD m (Doc Text)
definitionListItemToMarkdown opts (label, defs) = do
  labelText <- blockToMarkdown opts (Plain label)
  defs' <- mapM (mapM (blockToMarkdown opts)) defs
  if isEnabled Ext_definition_lists opts
     then do
       let tabStop = writerTabStop opts
       isPlain <- asks envPlain
       let leader  = if isPlain then "   " else ":  "
       let sps = case writerTabStop opts - 3 of
                      n | n > 0   -> text $ replicate n ' '
                      _ -> text " "
       let isTight = case defs of
                        ((Plain _ : _): _) -> True
                        _                  -> False
       if isEnabled Ext_compact_definition_lists opts
          then do
            let contents = vcat $ map (\d -> hang tabStop (leader <> sps)
                                $ vcat d <> cr) defs'
            return $ nowrap labelText <> cr <> contents <> cr
          else do
            let contents = (if isTight then vcat else vsep) $ map
                            (\d -> hang tabStop (leader <> sps) $ vcat d)
                            defs'
            return $ blankline <> nowrap labelText $$
                     (if isTight then empty else blankline) <> contents <> blankline
     else do
       return $ nowrap (chomp labelText <> text "  " <> cr) <>
                vsep (map vsep defs') <> blankline

-- | Convert list of Pandoc block elements to markdown.
blockListToMarkdown :: PandocMonad m
                    => WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> MD m (Doc Text)
blockListToMarkdown opts blocks = do
  inlist <- asks envInList
  isPlain <- asks envPlain
  -- a) insert comment between list and indented code block, or the
  -- code block will be treated as a list continuation paragraph
  -- b) change Plain to Para unless it's followed by a RawBlock
  -- or has a list as its parent (#3487)
  let fixBlocks (b : CodeBlock attr x : rest)
         | (not (isEnabled Ext_fenced_code_blocks opts) || attr == nullAttr)
             && isListBlock b = b : commentSep : CodeBlock attr x :
                                fixBlocks rest
      fixBlocks (b1@(BulletList _) : b2@(BulletList _) : bs) =
           b1 : commentSep : fixBlocks (b2:bs)
      fixBlocks (b1@(OrderedList _ _) : b2@(OrderedList _ _) : bs) =
           b1 : commentSep : fixBlocks (b2:bs)
      fixBlocks (b1@(DefinitionList _) : b2@(DefinitionList _) : bs) =
           b1 : commentSep : fixBlocks (b2:bs)
      fixBlocks (Plain ils : bs@(RawBlock{}:_)) =
           Plain ils : fixBlocks bs
      fixBlocks (Plain ils : bs) | inlist =
           Plain ils : fixBlocks bs
      fixBlocks (Plain ils : bs) =
           Para ils : fixBlocks bs
      fixBlocks (r@(RawBlock f raw) : b : bs)
        | not (null raw)
        , last raw /= '\n' =
        case b of
             Plain{}    -> r : fixBlocks (b:bs)
             RawBlock{} -> r : fixBlocks (b:bs)
             _          -> RawBlock f (raw ++ "\n") : fixBlocks (b:bs) -- #4629
      fixBlocks (x : xs)             = x : fixBlocks xs
      fixBlocks []                   = []
      isListBlock (BulletList _)     = True
      isListBlock (OrderedList _ _)  = True
      isListBlock (DefinitionList _) = True
      isListBlock _                  = False
      commentSep  = if isPlain
                       then Null
                       else if isEnabled Ext_raw_html opts
                            then RawBlock "html" "<!-- -->\n"
                            else RawBlock "markdown" "&nbsp;\n"
  mapM (blockToMarkdown opts) (fixBlocks blocks) >>= return . mconcat

getKey :: Doc Text -> Key
getKey = toKey . T.unpack . render Nothing

findUsableIndex :: [String] -> Int -> Int
findUsableIndex lbls i = if (show i) `elem` lbls
                         then findUsableIndex lbls (i + 1)
                         else i

getNextIndex :: PandocMonad m => MD m Int
getNextIndex = do
  prevRefs <- gets stPrevRefs
  refs <- gets stRefs
  i <- (+ 1) <$> gets stLastIdx
  let refLbls = map (\(r,_,_) -> r) $ prevRefs ++ refs
  return $ findUsableIndex refLbls i

-- | Get reference for target; if none exists, create unique one and return.
--   Prefer label if possible; otherwise, generate a unique key.
getReference :: PandocMonad m => Attr -> Doc Text -> Target -> MD m String
getReference attr label target = do
  refs <- gets stRefs
  case find (\(_,t,a) -> t == target && a == attr) refs of
    Just (ref, _, _) -> return ref
    Nothing       -> do
      keys <- gets stKeys
      case M.lookup (getKey label) keys of
           Nothing -> do -- no other refs with this label
             (lab', idx) <- if isEmpty label
                               then do
                                 i <- getNextIndex
                                 modify $ \s -> s{ stLastIdx = i }
                                 return (show i, i)
                               else
                                 return (T.unpack (render Nothing label), 0)
             modify (\s -> s{
               stRefs = (lab', target, attr) : refs,
               stKeys = M.insert (getKey label)
                           (M.insert (target, attr) idx mempty)
                                 (stKeys s) })
             return lab'

           Just km -> do -- we have refs with this label
             case M.lookup (target, attr) km of
                  Just i -> do
                    let lab' = T.unpack $ render Nothing $
                               label <> if i == 0
                                           then mempty
                                           else text (show i)
                    -- make sure it's in stRefs; it may be
                    -- a duplicate that was printed in a previous
                    -- block:
                    when ((lab', target, attr) `notElem` refs) $
                       modify (\s -> s{
                         stRefs = (lab', target, attr) : refs })
                    return lab'
                  Nothing -> do -- but this one is to a new target
                    i <- getNextIndex
                    modify $ \s -> s{ stLastIdx = i }
                    let lab' = show i
                    modify (\s -> s{
                       stRefs = (lab', target, attr) : refs,
                       stKeys = M.insert (getKey label)
                                   (M.insert (target, attr) i km)
                                         (stKeys s) })
                    return lab'

-- | Convert list of Pandoc inline elements to markdown.
inlineListToMarkdown :: PandocMonad m => WriterOptions -> [Inline] -> MD m (Doc Text)
inlineListToMarkdown opts lst = do
  inlist <- asks envInList
  go (if inlist then avoidBadWrapsInList lst else lst)
  where go [] = return empty
        go (i:is) = case i of
            (Link _ _ _) -> case is of
                -- If a link is followed by another link, or '[', '(' or ':'
                -- then we don't shortcut
                (Link _ _ _):_                    -> unshortcutable
                Space:(Link _ _ _):_              -> unshortcutable
                Space:(Str('[':_)):_              -> unshortcutable
                Space:(RawInline _ ('[':_)):_     -> unshortcutable
                Space:(Cite _ _):_                -> unshortcutable
                SoftBreak:(Link _ _ _):_          -> unshortcutable
                SoftBreak:(Str('[':_)):_          -> unshortcutable
                SoftBreak:(RawInline _ ('[':_)):_ -> unshortcutable
                SoftBreak:(Cite _ _):_            -> unshortcutable
                LineBreak:(Link _ _ _):_          -> unshortcutable
                LineBreak:(Str('[':_)):_          -> unshortcutable
                LineBreak:(RawInline _ ('[':_)):_ -> unshortcutable
                LineBreak:(Cite _ _):_            -> unshortcutable
                (Cite _ _):_                      -> unshortcutable
                Str ('[':_):_                     -> unshortcutable
                Str ('(':_):_                     -> unshortcutable
                Str (':':_):_                     -> unshortcutable
                (RawInline _ ('[':_)):_           -> unshortcutable
                (RawInline _ ('(':_)):_           -> unshortcutable
                (RawInline _ (':':_)):_           -> unshortcutable
                (RawInline _ (' ':'[':_)):_       -> unshortcutable
                _                                 -> shortcutable
            _ -> shortcutable
          where shortcutable = liftM2 (<>) (inlineToMarkdown opts i) (go is)
                unshortcutable = do
                    iMark <- local
                             (\env -> env { envRefShortcutable = False })
                             (inlineToMarkdown opts i)
                    fmap (iMark <>) (go is)

isSp :: Inline -> Bool
isSp Space     = True
isSp SoftBreak = True
isSp _         = False

avoidBadWrapsInList :: [Inline] -> [Inline]
avoidBadWrapsInList [] = []
avoidBadWrapsInList (s:Str ('>':cs):xs) | isSp s =
  Str (' ':'>':cs) : avoidBadWrapsInList xs
avoidBadWrapsInList (s:Str [c]:[])
  | isSp s && c `elem` ['-','*','+'] = Str [' ', c] : []
avoidBadWrapsInList (s:Str [c]:Space:xs)
  | isSp s && c `elem` ['-','*','+'] =
    Str [' ', c] : Space : avoidBadWrapsInList xs
avoidBadWrapsInList (s:Str cs:Space:xs)
  | isSp s && isOrderedListMarker cs =
    Str (' ':cs) : Space : avoidBadWrapsInList xs
avoidBadWrapsInList (s:Str cs:[])
  | isSp s && isOrderedListMarker cs = Str (' ':cs) : []
avoidBadWrapsInList (x:xs) = x : avoidBadWrapsInList xs

isOrderedListMarker :: String -> Bool
isOrderedListMarker xs = not (null xs) && (last xs `elem` ['.',')']) &&
              isRight (runParser (anyOrderedListMarker >> eof)
                       defaultParserState "" xs)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left  _) = False

-- | Convert Pandoc inline element to markdown.
inlineToMarkdown :: PandocMonad m => WriterOptions -> Inline -> MD m (Doc Text)
inlineToMarkdown opts (Span ("",["emoji"],kvs) [Str s]) = do
  case lookup "data-emoji" kvs of
       Just emojiname | isEnabled Ext_emoji opts ->
            return $ ":" <> text emojiname <> ":"
       _ -> inlineToMarkdown opts (Str s)
inlineToMarkdown opts (Span attrs ils) = do
  plain <- asks envPlain
  contents <- inlineListToMarkdown opts ils
  return $ case plain of
                True -> contents
                False | attrs == nullAttr -> contents
                      | isEnabled Ext_bracketed_spans opts ->
                        let attrs' = if attrs /= nullAttr
                                        then attrsToMarkdown attrs
                                        else empty
                        in "[" <> contents <> "]" <> attrs'
                      | isEnabled Ext_raw_html opts ||
                        isEnabled Ext_native_spans opts ->
                        tagWithAttrs "span" attrs <> contents <> text "</span>"
                      | otherwise -> contents
inlineToMarkdown _ (Emph []) = return empty
inlineToMarkdown opts (Emph lst) = do
  plain <- asks envPlain
  contents <- inlineListToMarkdown opts lst
  return $ if plain
              then if isEnabled Ext_gutenberg opts
                      then "_" <> contents <> "_"
                      else contents
              else "*" <> contents <> "*"
inlineToMarkdown _ (Strong []) = return empty
inlineToMarkdown opts (Strong lst) = do
  plain <- asks envPlain
  if plain
     then inlineListToMarkdown opts $
          if isEnabled Ext_gutenberg opts
             then capitalize lst
             else lst
     else do
       contents <- inlineListToMarkdown opts lst
       return $ "**" <> contents <> "**"
inlineToMarkdown _ (Strikeout []) = return empty
inlineToMarkdown opts (Strikeout lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ if isEnabled Ext_strikeout opts
              then "~~" <> contents <> "~~"
              else if isEnabled Ext_raw_html opts
                       then "<s>" <> contents <> "</s>"
                       else contents
inlineToMarkdown _ (Superscript []) = return empty
inlineToMarkdown opts (Superscript lst) =
  local (\env -> env {envEscapeSpaces = True}) $ do
    contents <- inlineListToMarkdown opts lst
    return $ if isEnabled Ext_superscript opts
                then "^" <> contents <> "^"
                else if isEnabled Ext_raw_html opts
                         then "<sup>" <> contents <> "</sup>"
                         else
                           let rendered = T.unpack $ render Nothing contents
                           in  case mapM toSuperscript rendered of
                                    Just r  -> text r
                                    Nothing -> text $ "^(" ++ rendered ++ ")"
inlineToMarkdown _ (Subscript []) = return empty
inlineToMarkdown opts (Subscript lst) =
  local (\env -> env {envEscapeSpaces = True}) $ do
    contents <- inlineListToMarkdown opts lst
    return $ if isEnabled Ext_subscript opts
                then "~" <> contents <> "~"
                else if isEnabled Ext_raw_html opts
                         then "<sub>" <> contents <> "</sub>"
                         else
                           let rendered = T.unpack $ render Nothing contents
                           in  case mapM toSubscript rendered of
                                    Just r  -> text r
                                    Nothing -> text $ "_(" ++ rendered ++ ")"
inlineToMarkdown opts (SmallCaps lst) = do
  plain <- asks envPlain
  if not plain &&
     (isEnabled Ext_raw_html opts || isEnabled Ext_native_spans opts)
     then inlineToMarkdown opts (Span ("",["smallcaps"],[]) lst)
     else inlineListToMarkdown opts $ capitalize lst
inlineToMarkdown opts (Quoted SingleQuote lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ if isEnabled Ext_smart opts
              then "'" <> contents <> "'"
              else
                if writerPreferAscii opts
                   then "&lsquo;" <> contents <> "&rsquo;"
                   else "‘" <> contents <> "’"
inlineToMarkdown opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ if isEnabled Ext_smart opts
              then "\"" <> contents <> "\""
              else
                if writerPreferAscii opts
                   then "&ldquo;" <> contents <> "&rdquo;"
                   else "“" <> contents <> "”"
inlineToMarkdown opts (Code attr str) = do
  let tickGroups = filter (\s -> '`' `elem` s) $ group str
  let longest    = if null tickGroups
                     then 0
                     else maximum $ map length tickGroups
  let marker     = replicate (longest + 1) '`'
  let spacer     = if (longest == 0) then "" else " "
  let attrs      = if isEnabled Ext_inline_code_attributes opts && attr /= nullAttr
                      then attrsToMarkdown attr
                      else empty
  plain <- asks envPlain
  if plain
     then return $ text str
     else return $ text (marker ++ spacer ++ str ++ spacer ++ marker) <> attrs
inlineToMarkdown opts (Str str) = do
  isPlain <- asks envPlain
  let str' = (if isEnabled Ext_smart opts
                 then unsmartify opts
                 else id) $
              if isPlain
                 then str
                 else escapeString opts str
  return $ text str'
inlineToMarkdown opts (Math InlineMath str) =
  case writerHTMLMathMethod opts of
       WebTeX url -> inlineToMarkdown opts
                       (Image nullAttr [Str str] (url ++ urlEncode str, str))
       _ | isEnabled Ext_tex_math_dollars opts ->
             return $ "$" <> text str <> "$"
         | isEnabled Ext_tex_math_single_backslash opts ->
             return $ "\\(" <> text str <> "\\)"
         | isEnabled Ext_tex_math_double_backslash opts ->
             return $ "\\\\(" <> text str <> "\\\\)"
         | otherwise -> do
             plain <- asks envPlain
             texMathToInlines InlineMath str >>=
               inlineListToMarkdown opts .
                 (if plain then makeMathPlainer else id)
inlineToMarkdown opts (Math DisplayMath str) =
  case writerHTMLMathMethod opts of
      WebTeX url -> (\x -> blankline <> x <> blankline) `fmap`
             inlineToMarkdown opts (Image nullAttr [Str str]
                    (url ++ urlEncode str, str))
      _ | isEnabled Ext_tex_math_dollars opts ->
            return $ "$$" <> text str <> "$$"
        | isEnabled Ext_tex_math_single_backslash opts ->
            return $ "\\[" <> text str <> "\\]"
        | isEnabled Ext_tex_math_double_backslash opts ->
            return $ "\\\\[" <> text str <> "\\\\]"
        | otherwise -> (\x -> cr <> x <> cr) `fmap`
            (texMathToInlines DisplayMath str >>= inlineListToMarkdown opts)
inlineToMarkdown opts il@(RawInline f str) = do
  let tickGroups = filter (\s -> '`' `elem` s) $ group str
  let numticks   = if null tickGroups
                     then 1
                     else 1 + maximum (map length tickGroups)
  plain <- asks envPlain
  let Format fmt = f
  let rawAttribInline = return $
         text (replicate numticks '`') <> text str <>
         text (replicate numticks '`') <> text "{=" <> text fmt <> text "}"
  let renderEmpty = mempty <$ report (InlineNotRendered il)
  case () of
    _ | plain -> renderEmpty
      | f `elem` ["markdown", "markdown_github", "markdown_phpextra",
                  "markdown_mmd", "markdown_strict"] ->
            return $ text str
      | isEnabled Ext_raw_attribute opts -> rawAttribInline
      | f `elem` ["html", "html5", "html4"] ->
            case () of
              _ | isEnabled Ext_raw_html opts -> return $ text str
                | isEnabled Ext_raw_attribute opts -> rawAttribInline
                | otherwise -> renderEmpty
      | f `elem` ["latex", "tex"] ->
            case () of
              _ | isEnabled Ext_raw_tex opts -> return $ text str
                | isEnabled Ext_raw_attribute opts -> rawAttribInline
                | otherwise -> renderEmpty
      | otherwise -> renderEmpty
inlineToMarkdown opts (LineBreak) = do
  plain <- asks envPlain
  if plain || isEnabled Ext_hard_line_breaks opts
     then return cr
     else return $
          if isEnabled Ext_escaped_line_breaks opts
             then "\\" <> cr
             else "  " <> cr
inlineToMarkdown _ Space = do
  escapeSpaces <- asks envEscapeSpaces
  return $ if escapeSpaces then "\\ " else space
inlineToMarkdown opts SoftBreak = do
  escapeSpaces <- asks envEscapeSpaces
  let space' = if escapeSpaces then "\\ " else space
  return $ case writerWrapText opts of
                WrapNone     -> space'
                WrapAuto     -> space'
                WrapPreserve -> cr
inlineToMarkdown opts (Cite [] lst) = inlineListToMarkdown opts lst
inlineToMarkdown opts (Cite (c:cs) lst)
  | not (isEnabled Ext_citations opts) = inlineListToMarkdown opts lst
  | otherwise =
      if citationMode c == AuthorInText
         then do
           suffs <- inlineListToMarkdown opts $ citationSuffix c
           rest <- mapM convertOne cs
           let inbr = suffs <+> joincits rest
               br   = if isEmpty inbr then empty else char '[' <> inbr <> char ']'
           return $ text ("@" ++ citationId c) <+> br
         else do
           cits <- mapM convertOne (c:cs)
           return $ text "[" <> joincits cits <> text "]"
  where
        joincits = hcat . intersperse (text "; ") . filter (not . isEmpty)
        convertOne Citation { citationId      = k
                            , citationPrefix  = pinlines
                            , citationSuffix  = sinlines
                            , citationMode    = m }
                               = do
           pdoc <- inlineListToMarkdown opts pinlines
           sdoc <- inlineListToMarkdown opts sinlines
           let k' = text (modekey m ++ "@" ++ k)
               r = case sinlines of
                        Str (y:_):_ | y `elem` (",;]@" :: String) -> k' <> sdoc
                        _                                         -> k' <+> sdoc
           return $ pdoc <+> r
        modekey SuppressAuthor = "-"
        modekey _              = ""
inlineToMarkdown opts lnk@(Link attr txt (src, tit))
  | isEnabled Ext_raw_html opts &&
    not (isEnabled Ext_link_attributes opts) &&
    attr /= nullAttr = -- use raw HTML
    (text . T.unpack . T.strip) <$>
      writeHtml5String opts{ writerTemplate = Nothing } (Pandoc nullMeta [Plain [lnk]])
  | otherwise = do
  plain <- asks envPlain
  linktext <- inlineListToMarkdown opts txt
  let linktitle = if null tit
                     then empty
                     else text $ " \"" ++ tit ++ "\""
  let srcSuffix = fromMaybe src (stripPrefix "mailto:" src)
  let useAuto = isURI src &&
                case txt of
                      [Str s] | escapeURI s == srcSuffix -> True
                      _       -> False
  let useRefLinks = writerReferenceLinks opts && not useAuto
  shortcutable <- asks envRefShortcutable
  let useShortcutRefLinks = shortcutable &&
                            isEnabled Ext_shortcut_reference_links opts
  reftext <- if useRefLinks
                then text <$> getReference attr linktext (src, tit)
                else return mempty
  return $ if useAuto
              then if plain
                      then text srcSuffix
                      else "<" <> text srcSuffix <> ">"
              else if useRefLinks
                      then let first  = "[" <> linktext <> "]"
                               second = if getKey linktext == getKey reftext
                                           then if useShortcutRefLinks
                                                   then ""
                                                   else "[]"
                                           else "[" <> reftext <> "]"
                           in  first <> second
                      else if plain
                              then linktext
                              else "[" <> linktext <> "](" <>
                                   text src <> linktitle <> ")" <>
                                   linkAttributes opts attr
inlineToMarkdown opts img@(Image attr alternate (source, tit))
  | isEnabled Ext_raw_html opts &&
    not (isEnabled Ext_link_attributes opts) &&
    attr /= nullAttr = -- use raw HTML
    (text . T.unpack . T.strip) <$>
      writeHtml5String opts{ writerTemplate = Nothing } (Pandoc nullMeta [Plain [img]])
  | otherwise = do
  plain <- asks envPlain
  let txt = if null alternate || alternate == [Str source]
                                 -- to prevent autolinks
               then [Str ""]
               else alternate
  linkPart <- inlineToMarkdown opts (Link attr txt (source, tit))
  return $ if plain
              then "[" <> linkPart <> "]"
              else "!" <> linkPart
inlineToMarkdown opts (Note contents) = do
  modify (\st -> st{ stNotes = contents : stNotes st })
  st <- get
  let ref = text $ writerIdentifierPrefix opts ++ show (stNoteNum st + (length $ stNotes st) - 1)
  if isEnabled Ext_footnotes opts
     then return $ "[^" <> ref <> "]"
     else return $ "[" <> ref <> "]"

makeMathPlainer :: [Inline] -> [Inline]
makeMathPlainer = walk go
  where
  go (Emph xs) = Span nullAttr xs
  go x         = x

lineBreakToSpace :: Inline -> Inline
lineBreakToSpace LineBreak = Space
lineBreakToSpace SoftBreak = Space
lineBreakToSpace x         = x
