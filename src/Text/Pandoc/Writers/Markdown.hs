{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
{- |
   Module      : Text.Pandoc.Writers.Markdown
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to markdown-formatted plain text.

Markdown:  <http://daringfireball.net/projects/markdown/>
-}
module Text.Pandoc.Writers.Markdown (
  writeMarkdown,
  writeCommonMark,
  writePlain) where
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Char (isAlphaNum)
import Data.Default
import Data.List (find, intersperse, sortBy, transpose)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP (urlEncode)
import Text.HTML.TagSoup (Tag (..), isTagText, parseTags)
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
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
import Data.Coerce (coerce)

type Notes = [[Block]]
type Ref   = (Text, Target, Attr)
type Refs  = [Ref]

type MD m = ReaderT WriterEnv (StateT WriterState m)

evalMD :: PandocMonad m => MD m a -> WriterEnv -> WriterState -> m a
evalMD md env st = evalStateT (runReaderT md env) st

data WriterEnv = WriterEnv { envInList          :: Bool
                           , envVariant         :: MarkdownVariant
                           , envRefShortcutable :: Bool
                           , envBlockLevel      :: Int
                           , envEscapeSpaces    :: Bool
                           }

data MarkdownVariant =
      PlainText
    | Commonmark
    | Markdown
    deriving (Show, Eq)

instance Default WriterEnv
  where def = WriterEnv { envInList          = False
                        , envVariant         = Markdown
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
                               , stIds     :: Set.Set Text
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
  evalMD (pandocToMarkdown opts document) def{ envVariant = PlainText } def

-- | Convert Pandoc to Commonmark.
writeCommonMark :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeCommonMark opts document =
  evalMD (pandocToMarkdown opts document) def{ envVariant = Commonmark } def

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
                                            mapMaybe fromVal xs)
               (k', SimpleVal x)
                      | isEmpty x -> empty
                      | otherwise -> k' <> ":" <> space <>
                                     nest 2 (removeBlankLines (chomp x))
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
  variant <- asks envVariant
  metadata <- metaToContext'
               (blockListToMarkdown opts)
               (inlineListToMarkdown opts)
               meta
  let title' = fromMaybe empty $ getField "title" metadata
  let authors' = fromMaybe [] $ getField "author" metadata
  let date' = fromMaybe empty $ getField "date" metadata
  let titleblock = case writerTemplate opts of
                        Just _ | variant == PlainText ->
                                 plainTitleBlock title' authors' date'
                               | isEnabled Ext_yaml_metadata_block opts ->
                                   yamlMetadataBlock metadata
                               | isEnabled Ext_pandoc_title_block opts ->
                                   pandocTitleBlock title' authors' date'
                               | isEnabled Ext_mmd_title_block opts ->
                                   mmdTitleBlock metadata
                               | otherwise -> empty
                        Nothing -> empty
  toc <- if writerTableOfContents opts
         then blockToMarkdown opts ( toTableOfContents opts blocks )
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
  let tit' = if T.null tit
                then empty
                else space <> "\"" <> literal tit <> "\""
  return $ nest 2 $ hang 2
            ("[" <> literal label' <> "]:" <> space) (literal src <> tit')
            <+> linkAttributes opts attr

-- | Return markdown representation of notes.
notesToMarkdown :: PandocMonad m => WriterOptions -> [[Block]] -> MD m (Doc Text)
notesToMarkdown opts notes = do
  n <- gets stNoteNum
  notes' <- zipWithM (noteToMarkdown opts) [n..] notes
  modify $ \st -> st { stNoteNum = stNoteNum st + length notes }
  return $ vsep notes'

-- | Return markdown representation of a note.
noteToMarkdown :: PandocMonad m => WriterOptions -> Int -> [Block] -> MD m (Doc Text)
noteToMarkdown opts num blocks = do
  contents  <- blockListToMarkdown opts blocks
  let num' = literal $ writerIdentifierPrefix opts <> tshow num
  let marker = if isEnabled Ext_footnotes opts
                  then literal "[^" <> num' <> literal "]:"
                  else literal "[" <> num' <> literal "]"
  let markerSize = 4 + offset num'
  let spacer = case writerTabStop opts - markerSize of
                     n | n > 0  -> literal $ T.replicate n " "
                     _ -> literal " "
  return $ if isEnabled Ext_footnotes opts
              then hang (writerTabStop opts) (marker <> spacer) contents
              else marker <> spacer <> contents

-- | Escape special characters for Markdown.
escapeText :: WriterOptions -> Text -> Text
escapeText opts = T.pack . go . T.unpack
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
       _   -> case cs of
                '_':x:xs
                  | isEnabled Ext_intraword_underscores opts
                  , isAlphaNum c
                  , isAlphaNum x -> c : '_' : x : go xs
                _                -> c : go cs

attrsToMarkdown :: Attr -> Doc Text
attrsToMarkdown attribs = braces $ hsep [attribId, attribClasses, attribKeys]
        where attribId = case attribs of
                                ("",_,_) -> empty
                                (i,_,_)  -> "#" <> escAttr i
              attribClasses = case attribs of
                                (_,[],_) -> empty
                                (_,cs,_) -> hsep $
                                            map (escAttr . ("."<>))
                                            cs
              attribKeys = case attribs of
                                (_,_,[]) -> empty
                                (_,_,ks) -> hsep $
                                            map (\(k,v) -> escAttr k
                                              <> "=\"" <>
                                              escAttr v <> "\"") ks
              escAttr          = mconcat . map escAttrChar . T.unpack
              escAttrChar '"'  = literal "\\\""
              escAttrChar '\\' = literal "\\\\"
              escAttrChar c    = literal $ T.singleton c

linkAttributes :: WriterOptions -> Attr -> Doc Text
linkAttributes opts attr =
  if isEnabled Ext_link_attributes opts && attr /= nullAttr
     then attrsToMarkdown attr
     else empty

-- | Ordered list start parser for use in Para below.
olMarker :: Parser Text ParserState ()
olMarker = do (start, style', delim) <- anyOrderedListMarker
              if delim == Period &&
                          (style' == UpperAlpha || (style' == UpperRoman &&
                          start `elem` [1, 5, 10, 50, 100, 500, 1000]))
                          then mzero -- it needs 2 spaces anyway
                          else eof

-- | True if string begins with an ordered list marker
beginsWithOrderedListMarker :: Text -> Bool
beginsWithOrderedListMarker str =
  case runParser olMarker defaultParserState "para start" (T.take 10 str) of
         Left  _ -> False
         Right _ -> True

notesAndRefs :: PandocMonad m => WriterOptions -> MD m (Doc Text)
notesAndRefs opts = do
  notes' <- gets stNotes >>= notesToMarkdown opts . reverse
  modify $ \s -> s { stNotes = [] }
  refs' <- gets stRefs >>= refsToMarkdown opts . reverse
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
                nowrap (literal ":::" <+> attrsToMarkdown attrs) $$
                chomp contents $$
                literal ":::" <> blankline
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
  -- escape if para starts with ordered list marker
  variant <- asks envVariant
  let escapeMarker = T.concatMap $ \x -> if x `elemText` ".()"
                                         then T.pack ['\\', x]
                                         else T.singleton x
  let startsWithSpace (Space:_)     = True
      startsWithSpace (SoftBreak:_) = True
      startsWithSpace _             = False
  let inlines' =
        if variant == PlainText
           then inlines
           else case inlines of
                  (Str t:ys)
                    | (null ys || startsWithSpace ys)
                    , beginsWithOrderedListMarker t
                    -> RawInline (Format "markdown") (escapeMarker t):ys
                  (Str t:_)
                    | t == "+" || t == "-" ||
                      (t == "%" && isEnabled Ext_pandoc_title_block opts &&
                                   isEnabled Ext_all_symbols_escapable opts)
                    -> RawInline (Format "markdown") "\\" : inlines
                  _ -> inlines
  contents <- inlineListToMarkdown opts inlines'
  return $ contents <> cr
-- title beginning with fig: indicates figure
blockToMarkdown' opts (Para [Image attr alt (src,tgt@(T.stripPrefix "fig:" -> Just tit))])
  | isEnabled Ext_raw_html opts &&
    not (isEnabled Ext_link_attributes opts) &&
    attr /= nullAttr = -- use raw HTML
    ((<> blankline) . literal . T.strip) <$>
      writeHtml5String opts{ writerTemplate = Nothing }
        (Pandoc nullMeta [Para [Image attr alt (src,tgt)]])
  | otherwise = blockToMarkdown opts (Para [Image attr alt (src,tit)])
blockToMarkdown' opts (Para inlines) =
  (<> blankline) `fmap` blockToMarkdown opts (Plain inlines)
blockToMarkdown' opts (LineBlock lns) =
  if isEnabled Ext_line_blocks opts
  then do
    mdLines <- mapM (inlineListToMarkdown opts) lns
    return $ (vcat $ map (hang 2 (literal "| ")) mdLines) <> blankline
  else blockToMarkdown opts $ linesToPara lns
blockToMarkdown' opts b@(RawBlock f str) = do
  variant <- asks envVariant
  let Format fmt = f
  let rawAttribBlock = return $
         (literal "```{=" <> literal fmt <> "}") $$
         literal str $$
         (literal "```" <> literal "\n")
  let renderEmpty = mempty <$ report (BlockNotRendered b)
  case variant of
    PlainText -> renderEmpty
    _ | f `elem` ["markdown", "markdown_github", "markdown_phpextra",
                  "markdown_mmd", "markdown_strict"] ->
            return $ literal str <> literal "\n"
      | isEnabled Ext_raw_attribute opts -> rawAttribBlock
      | f `elem` ["html", "html5", "html4"] ->
            case () of
              _ | isEnabled Ext_markdown_attribute opts -> return $
                    literal (addMarkdownAttribute str) <> literal "\n"
                | isEnabled Ext_raw_html opts -> return $
                    literal str <> literal "\n"
                | isEnabled Ext_raw_attribute opts -> rawAttribBlock
                | otherwise -> renderEmpty
      | f `elem` ["latex", "tex"] ->
            case () of
              _ | isEnabled Ext_raw_tex opts -> return $
                    literal str <> literal "\n"
                | isEnabled Ext_raw_attribute opts -> rawAttribBlock
                | otherwise -> renderEmpty
      | otherwise -> renderEmpty
blockToMarkdown' opts HorizontalRule =
  return $ blankline <> literal (T.replicate (writerColumns opts) "-") <> blankline
blockToMarkdown' opts (Header level attr inlines) = do
  -- first, if we're putting references at the end of a section, we
  -- put them here.
  blkLevel <- asks envBlockLevel
  refs <- if writerReferenceLocation opts == EndOfSection && blkLevel == 1
          then notesAndRefs opts
          else return empty

  variant <- asks envVariant
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
                                    space <> brackets (literal id')
                   _ | isEnabled Ext_header_attributes opts ->
                                    space <> attrsToMarkdown attr
                     | otherwise -> empty
  contents <- inlineListToMarkdown opts $
                 -- ensure no newlines; see #3736
                 walk lineBreakToSpace $
                   if level == 1 && variant == PlainText &&
                      isEnabled Ext_gutenberg opts
                      then capitalize inlines
                      else inlines
  let setext = writerSetextHeaders opts
      hdr = nowrap $ case level of
            1 | variant == PlainText ->
                if isEnabled Ext_gutenberg opts
                   then blanklines 3 <> contents <> blanklines 2
                   else contents <> blankline
              | setext ->
                  contents <> attr' <> cr <> literal (T.replicate (offset contents) "=") <>
                  blankline
            2 | variant == PlainText ->
                if isEnabled Ext_gutenberg opts
                   then blanklines 2 <> contents <> blankline
                   else contents <> blankline
              | setext ->
                  contents <> attr' <> cr <> literal (T.replicate (offset contents) "-") <>
                  blankline
            -- ghc interprets '#' characters in column 1 as linenum specifiers.
            _ | variant == PlainText || isEnabled Ext_literate_haskell opts ->
                contents <> blankline
            _ -> literal (T.replicate level "#") <> space <> contents <> attr' <> blankline

  return $ refs <> hdr
blockToMarkdown' opts (CodeBlock (_,classes,_) str)
  | "haskell" `elem` classes && "literate" `elem` classes &&
    isEnabled Ext_literate_haskell opts =
  return $ prefixed "> " (literal str) <> blankline
blockToMarkdown' opts (CodeBlock attribs str) = return $
  case attribs == nullAttr of
     False | isEnabled Ext_backtick_code_blocks opts ->
          backticks <> attrs <> cr <> literal str <> cr <> backticks <> blankline
           | isEnabled Ext_fenced_code_blocks opts ->
          tildes <> attrs <> cr <> literal str <> cr <> tildes <> blankline
     _ -> nest (writerTabStop opts) (literal str) <> blankline
   where endline c = literal $ case [T.length ln
                                   | ln <- map trim (T.lines str)
                                   , T.pack [c,c,c] `T.isPrefixOf` ln
                                   , T.all (== c) ln] of
                               [] -> T.replicate 3 $ T.singleton c
                               xs -> T.replicate (maximum xs + 1) $ T.singleton c
         backticks = endline '`'
         tildes = endline '~'
         attrs  = if isEnabled Ext_fenced_code_attributes opts
                     then nowrap $ " " <> attrsToMarkdown attribs
                     else case attribs of
                                (_,(cls:_),_) -> " " <> literal cls
                                _             -> empty
blockToMarkdown' opts (BlockQuote blocks) = do
  variant <- asks envVariant
  -- if we're writing literate haskell, put a space before the bird tracks
  -- so they won't be interpreted as lhs...
  let leader = if isEnabled Ext_literate_haskell opts
                  then " > "
                  else if variant == PlainText then "  " else "> "
  contents <- blockListToMarkdown opts blocks
  return $ (prefixed leader contents) <> blankline
blockToMarkdown' opts t@(Table _ blkCapt specs thead tbody tfoot) = do
  let (caption, aligns, widths, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
  let numcols = maximum (length aligns : length widths :
                           map length (headers:rows))
  caption' <- inlineListToMarkdown opts caption
  let caption'' = if null caption
                     then blankline
                     else
                       if isEnabled Ext_table_captions opts
                          then blankline $$ (": " <> caption') $$ blankline
                          else blankline $$ caption' $$ blankline
  let hasSimpleCells = onlySimpleTableCells $ headers : rows
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
                   literal <$>
                   (writeHtml5String opts{ writerTemplate = Nothing } $ Pandoc nullMeta [t])
            | otherwise -> return (id, literal "[TABLE]")
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
  let markers' = map (\m -> if T.length m < 3
                               then m <> T.replicate (3 - T.length m) " "
                               else m) markers
  contents <- inList $
              zipWithM (orderedListItemToMarkdown opts) markers' items
  return $ (if isTightList items then vcat else vsep) contents <> blankline
blockToMarkdown' opts (DefinitionList items) = do
  contents <- inList $ mapM (definitionListItemToMarkdown opts) items
  return $ mconcat contents <> blankline

inList :: Monad m => MD m a -> MD m a
inList p = local (\env -> env {envInList = True}) p

addMarkdownAttribute :: Text -> Text
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
  let sp = literal " "
  let blockFor AlignLeft   x y = lblock (x + 2) (sp <> y) <> lblock 0 empty
      blockFor AlignCenter x y = cblock (x + 2) (sp <> y <> sp) <> lblock 0 empty
      blockFor AlignRight  x y = rblock (x + 2) (y <> sp) <> lblock 0 empty
      blockFor _           x y = lblock (x + 2) (sp <> y) <> lblock 0 empty
  let widths = map (max 3 . maximum . map offset) $ transpose (rawHeaders : rawRows)
  let torow cs = nowrap $ literal "|" <>
                    hcat (intersperse (literal "|") $
                          zipWith3 blockFor aligns widths (map chomp cs))
                    <> literal "|"
  let toborder a w = literal $ case a of
                          AlignLeft    -> ":" <> T.replicate (w + 1) "-"
                          AlignCenter  -> ":" <> T.replicate w "-" <> ":"
                          AlignRight   -> T.replicate (w + 1) "-" <> ":"
                          AlignDefault -> T.replicate (w + 2) "-"
  -- note:  pipe tables can't completely lack a
  -- header; for a headerless table, we need a header of empty cells.
  -- see jgm/pandoc#1996.
  let header = if headless
                  then torow (replicate (length aligns) empty)
                  else torow rawHeaders
  let border = nowrap $ literal "|" <> hcat (intersperse (literal "|") $
                        zipWith toborder aligns widths) <> literal "|"
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
  let makeRow = hcat . intersperse (lblock 1 (literal " ")) .
                   (zipWith3 alignHeader aligns widthsInChars)
  let rows' = map makeRow rawRows
  let head' = makeRow rawHeaders
  let underline = mconcat $ intersperse (literal " ") $
                  map (\width -> literal (T.replicate width "-")) widthsInChars
  let border = if multiline
                  then literal (T.replicate (sum widthsInChars +
                          length widthsInChars - 1) "-")
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
  let sps = T.replicate (writerTabStop opts - 2) " "
  let start = literal $ "- " <> sps
  -- remove trailing blank line if item ends with a tight list
  let contents' = if itemEndsWithTightList bs
                     then chomp contents <> cr
                     else contents
  return $ hang (writerTabStop opts) start contents'

-- | Convert ordered list item (a list of blocks) to markdown.
orderedListItemToMarkdown :: PandocMonad m
                          => WriterOptions -- ^ options
                          -> Text        -- ^ list item marker
                          -> [Block]       -- ^ list item (list of blocks)
                          -> MD m (Doc Text)
orderedListItemToMarkdown opts marker bs = do
  let exts = writerExtensions opts
  contents <- blockListToMarkdown opts $ taskListItemToAscii exts bs
  let sps = case writerTabStop opts - T.length marker of
                   n | n > 0 -> literal $ T.replicate n " "
                   _ -> literal " "
  let ind = if isEnabled Ext_four_space_rule opts
               then writerTabStop opts
               else max (writerTabStop opts) (T.length marker + 1)
  let start = literal marker <> sps
  -- remove trailing blank line if item ends with a tight list
  let contents' = if itemEndsWithTightList bs
                     then chomp contents <> cr
                     else contents
  return $ hang ind start contents'

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
       variant <- asks envVariant
       let leader  = if variant == PlainText then "   " else ":  "
       let sps = case writerTabStop opts - 3 of
                      n | n > 0   -> literal $ T.replicate n " "
                      _ -> literal " "
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
     else
       return $ nowrap (chomp labelText <> literal "  " <> cr) <>
                vsep (map vsep defs') <> blankline

-- | Convert list of Pandoc block elements to markdown.
blockListToMarkdown :: PandocMonad m
                    => WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> MD m (Doc Text)
blockListToMarkdown opts blocks = do
  inlist <- asks envInList
  variant <- asks envVariant
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
        | not (T.null raw)
        , T.last raw /= '\n' =
        case b of
             Plain{}    -> r : fixBlocks (b:bs)
             RawBlock{} -> r : fixBlocks (b:bs)
             _          -> RawBlock f (raw <> "\n") : fixBlocks (b:bs) -- #4629
      fixBlocks (x : xs)             = x : fixBlocks xs
      fixBlocks []                   = []
      isListBlock (BulletList _)     = True
      isListBlock (OrderedList _ _)  = True
      isListBlock (DefinitionList _) = True
      isListBlock _                  = False
      commentSep  = if variant == PlainText
                       then Null
                       else if isEnabled Ext_raw_html opts
                            then RawBlock "html" "<!-- -->\n"
                            else RawBlock "markdown" "&nbsp;\n"
  mapM (blockToMarkdown opts) (fixBlocks blocks) >>= return . mconcat

getKey :: Doc Text -> Key
getKey = toKey . render Nothing

findUsableIndex :: [Text] -> Int -> Int
findUsableIndex lbls i = if (tshow i) `elem` lbls
                         then findUsableIndex lbls (i + 1)
                         else i

getNextIndex :: PandocMonad m => MD m Int
getNextIndex = do
  prevRefs <- gets stPrevRefs
  refs <- gets stRefs
  i <- (+ 1) <$> gets stLastIdx
  modify $ \s -> s{ stLastIdx = i }
  let refLbls = map (\(r,_,_) -> r) $ prevRefs ++ refs
  return $ findUsableIndex refLbls i

-- | Get reference for target; if none exists, create unique one and return.
--   Prefer label if possible; otherwise, generate a unique key.
getReference :: PandocMonad m => Attr -> Doc Text -> Target -> MD m Text
getReference attr label target = do
  refs <- gets stRefs
  case find (\(_,t,a) -> t == target && a == attr) refs of
    Just (ref, _, _) -> return ref
    Nothing       -> do
      keys <- gets stKeys
      let key = getKey label
      let rawkey = coerce key
      case M.lookup key keys of
           Nothing -> do -- no other refs with this label
             (lab', idx) <- if T.null rawkey ||
                                 T.length rawkey > 999 ||
                                 T.any (\c -> c == '[' || c == ']') rawkey
                               then do
                                 i <- getNextIndex
                                 return (tshow i, i)
                               else
                                 return (render Nothing label, 0)
             modify (\s -> s{
               stRefs = (lab', target, attr) : refs,
               stKeys = M.insert (getKey label)
                           (M.insert (target, attr) idx mempty)
                                 (stKeys s) })
             return lab'

           Just km ->    -- we have refs with this label
             case M.lookup (target, attr) km of
                  Just i -> do
                    let lab' = render Nothing $
                               label <> if i == 0
                                           then mempty
                                           else literal (tshow i)
                    -- make sure it's in stRefs; it may be
                    -- a duplicate that was printed in a previous
                    -- block:
                    when ((lab', target, attr) `notElem` refs) $
                       modify (\s -> s{
                         stRefs = (lab', target, attr) : refs })
                    return lab'
                  Nothing -> do -- but this one is to a new target
                    i <- getNextIndex
                    let lab' = tshow i
                    modify (\s -> s{
                       stRefs = (lab', target, attr) : refs,
                       stKeys = M.insert key
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
                (Link _ _ _):_                                  -> unshortcutable
                Space:(Link _ _ _):_                            -> unshortcutable
                Space:(Str(thead -> Just '[')):_                -> unshortcutable
                Space:(RawInline _ (thead -> Just '[')):_       -> unshortcutable
                Space:(Cite _ _):_                              -> unshortcutable
                SoftBreak:(Link _ _ _):_                        -> unshortcutable
                SoftBreak:(Str(thead -> Just '[')):_            -> unshortcutable
                SoftBreak:(RawInline _ (thead -> Just '[')):_   -> unshortcutable
                SoftBreak:(Cite _ _):_                          -> unshortcutable
                LineBreak:(Link _ _ _):_                        -> unshortcutable
                LineBreak:(Str(thead -> Just '[')):_            -> unshortcutable
                LineBreak:(RawInline _ (thead -> Just '[')):_   -> unshortcutable
                LineBreak:(Cite _ _):_                          -> unshortcutable
                (Cite _ _):_                                    -> unshortcutable
                Str (thead -> Just '['):_                       -> unshortcutable
                Str (thead -> Just '('):_                       -> unshortcutable
                Str (thead -> Just ':'):_                       -> unshortcutable
                (RawInline _ (thead -> Just '[')):_             -> unshortcutable
                (RawInline _ (thead -> Just '(')):_             -> unshortcutable
                (RawInline _ (thead -> Just ':')):_             -> unshortcutable
                (RawInline _ (T.stripPrefix " [" -> Just _ )):_ -> unshortcutable
                _                                               -> shortcutable
            _ -> shortcutable
          where shortcutable = liftM2 (<>) (inlineToMarkdown opts i) (go is)
                unshortcutable = do
                    iMark <- local
                             (\env -> env { envRefShortcutable = False })
                             (inlineToMarkdown opts i)
                    fmap (iMark <>) (go is)
                thead = fmap fst . T.uncons

isSp :: Inline -> Bool
isSp Space     = True
isSp SoftBreak = True
isSp _         = False

avoidBadWrapsInList :: [Inline] -> [Inline]
avoidBadWrapsInList [] = []
avoidBadWrapsInList (s:Str (T.uncons -> Just ('>',cs)):xs) | isSp s =
  Str (" >" <> cs) : avoidBadWrapsInList xs
avoidBadWrapsInList (s:Str (T.uncons -> Just (c, cs)):[])
  | T.null cs && isSp s && c `elem` ['-','*','+'] = Str (T.pack [' ', c]) : []
avoidBadWrapsInList (s:Str (T.uncons -> Just (c, cs)):Space:xs)
  | T.null cs && isSp s && c `elem` ['-','*','+'] =
    Str (T.pack [' ', c]) : Space : avoidBadWrapsInList xs
avoidBadWrapsInList (s:Str cs:Space:xs)
  | isSp s && isOrderedListMarker cs =
    Str (" " <> cs) : Space : avoidBadWrapsInList xs
avoidBadWrapsInList (s:Str cs:[])
  | isSp s && isOrderedListMarker cs = Str (" " <> cs) : []
avoidBadWrapsInList (x:xs) = x : avoidBadWrapsInList xs

isOrderedListMarker :: Text -> Bool
isOrderedListMarker xs = not (T.null xs) && (T.last xs `elem` ['.',')']) &&
              isRight (runParser (anyOrderedListMarker >> eof)
                       defaultParserState "" xs)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left  _) = False

-- | Convert Pandoc inline element to markdown.
inlineToMarkdown :: PandocMonad m => WriterOptions -> Inline -> MD m (Doc Text)
inlineToMarkdown opts (Span ("",["emoji"],kvs) [Str s]) =
  case lookup "data-emoji" kvs of
       Just emojiname | isEnabled Ext_emoji opts ->
            return $ ":" <> literal emojiname <> ":"
       _ -> inlineToMarkdown opts (Str s)
inlineToMarkdown opts (Span attrs ils) = do
  variant <- asks envVariant
  contents <- inlineListToMarkdown opts ils
  return $ case variant of
                PlainText -> contents
                _     | attrs == nullAttr -> contents
                      | isEnabled Ext_bracketed_spans opts ->
                        let attrs' = if attrs /= nullAttr
                                        then attrsToMarkdown attrs
                                        else empty
                        in "[" <> contents <> "]" <> attrs'
                      | isEnabled Ext_raw_html opts ||
                        isEnabled Ext_native_spans opts ->
                        tagWithAttrs "span" attrs <> contents <> literal "</span>"
                      | otherwise -> contents
inlineToMarkdown _ (Emph []) = return empty
inlineToMarkdown opts (Emph lst) = do
  variant <- asks envVariant
  contents <- inlineListToMarkdown opts lst
  return $ case variant of
             PlainText
               | isEnabled Ext_gutenberg opts -> "_" <> contents <> "_"
               | otherwise ->  contents
             _ -> "*" <> contents <> "*"
inlineToMarkdown _ (Underline []) = return empty
inlineToMarkdown opts (Underline lst) = do
  variant <- asks envVariant
  contents <- inlineListToMarkdown opts lst
  case variant of
    PlainText -> return contents
    _     | isEnabled Ext_bracketed_spans opts ->
            return $ "[" <> contents <> "]" <> "{.ul}"
          | isEnabled Ext_native_spans opts ->
            return $ tagWithAttrs "span" ("", ["underline"], [])
              <> contents
              <> literal "</span>"
          | isEnabled Ext_raw_html opts ->
            return $ "<u>" <> contents <> "</u>"
          | otherwise -> inlineToMarkdown opts (Emph lst)
inlineToMarkdown _ (Strong []) = return empty
inlineToMarkdown opts (Strong lst) = do
  variant <- asks envVariant
  case variant of
    PlainText ->
             inlineListToMarkdown opts $
               if isEnabled Ext_gutenberg opts
                  then capitalize lst
                  else lst
    _ -> do
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
  local (\env -> env {envEscapeSpaces = (envVariant env == Markdown)}) $ do
    contents <- inlineListToMarkdown opts lst
    if isEnabled Ext_superscript opts
       then return $ "^" <> contents <> "^"
       else if isEnabled Ext_raw_html opts
                then return $ "<sup>" <> contents <> "</sup>"
                else
                  case traverse toSuperscriptInline lst of
                    Just xs' | not (writerPreferAscii opts)
                      -> inlineListToMarkdown opts xs'
                    _ -> do
                      let rendered = render Nothing contents
                      return $
                        case mapM toSuperscript (T.unpack rendered) of
                           Just r  -> literal $ T.pack r
                           Nothing -> literal $ "^(" <> rendered <> ")"
inlineToMarkdown _ (Subscript []) = return empty
inlineToMarkdown opts (Subscript lst) =
  local (\env -> env {envEscapeSpaces = (envVariant env == Markdown)}) $ do
    contents <- inlineListToMarkdown opts lst
    if isEnabled Ext_subscript opts
       then return $ "~" <> contents <> "~"
       else if isEnabled Ext_raw_html opts
                then return $ "<sub>" <> contents <> "</sub>"
                else
                  case traverse toSubscriptInline lst of
                    Just xs' | not (writerPreferAscii opts)
                      -> inlineListToMarkdown opts xs'
                    _ -> do
                      let rendered = render Nothing contents
                      return $
                        case mapM toSuperscript (T.unpack rendered) of
                           Just r  -> literal $ T.pack r
                           Nothing -> literal $ "_(" <> rendered <> ")"
inlineToMarkdown opts (SmallCaps lst) = do
  variant <- asks envVariant
  if variant /= PlainText &&
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
  let tickGroups = filter (T.any (== '`')) $ T.group str
  let longest    = if null tickGroups
                     then 0
                     else maximum $ map T.length tickGroups
  let marker     = T.replicate (longest + 1) "`"
  let spacer     = if (longest == 0) then "" else " "
  let attrs      = if isEnabled Ext_inline_code_attributes opts && attr /= nullAttr
                      then attrsToMarkdown attr
                      else empty
  variant <- asks envVariant
  case variant of
     PlainText -> return $ literal str
     _     ->  return $ literal
                  (marker <> spacer <> str <> spacer <> marker) <> attrs
inlineToMarkdown opts (Str str) = do
  variant <- asks envVariant
  let str' = (if writerPreferAscii opts
                 then toHtml5Entities
                 else id) .
             (if isEnabled Ext_smart opts
                 then unsmartify opts
                 else id) .
             (if variant == PlainText
                 then id
                 else escapeText opts) $ str
  return $ literal str'
inlineToMarkdown opts (Math InlineMath str) =
  case writerHTMLMathMethod opts of
       WebTeX url -> inlineToMarkdown opts
                       (Image nullAttr [Str str] (url <> T.pack (urlEncode $ T.unpack str), str))
       _ | isEnabled Ext_tex_math_dollars opts ->
             return $ "$" <> literal str <> "$"
         | isEnabled Ext_tex_math_single_backslash opts ->
             return $ "\\(" <> literal str <> "\\)"
         | isEnabled Ext_tex_math_double_backslash opts ->
             return $ "\\\\(" <> literal str <> "\\\\)"
         | otherwise -> do
             variant <- asks envVariant
             texMathToInlines InlineMath str >>=
               inlineListToMarkdown opts .
                 (if variant == PlainText then makeMathPlainer else id)
inlineToMarkdown opts (Math DisplayMath str) =
  case writerHTMLMathMethod opts of
      WebTeX url -> (\x -> blankline <> x <> blankline) `fmap`
             inlineToMarkdown opts (Image nullAttr [Str str]
                    (url <> T.pack (urlEncode $ T.unpack str), str))
      _ | isEnabled Ext_tex_math_dollars opts ->
            return $ "$$" <> literal str <> "$$"
        | isEnabled Ext_tex_math_single_backslash opts ->
            return $ "\\[" <> literal str <> "\\]"
        | isEnabled Ext_tex_math_double_backslash opts ->
            return $ "\\\\[" <> literal str <> "\\\\]"
        | otherwise -> (\x -> cr <> x <> cr) `fmap`
            (texMathToInlines DisplayMath str >>= inlineListToMarkdown opts)
inlineToMarkdown opts il@(RawInline f str) = do
  let tickGroups = filter (T.any (== '`')) $ T.group str
  let numticks   = if null tickGroups
                     then 1
                     else 1 + maximum (map T.length tickGroups)
  variant <- asks envVariant
  let Format fmt = f
  let rawAttribInline = return $
         literal (T.replicate numticks "`") <> literal str <>
         literal (T.replicate numticks "`") <> literal "{=" <> literal fmt <> literal "}"
  let renderEmpty = mempty <$ report (InlineNotRendered il)
  case variant of
    PlainText -> renderEmpty
    _ | f `elem` ["markdown", "markdown_github", "markdown_phpextra",
                  "markdown_mmd", "markdown_strict"] ->
            return $ literal str
      | isEnabled Ext_raw_attribute opts -> rawAttribInline
      | f `elem` ["html", "html5", "html4"] ->
            case () of
              _ | isEnabled Ext_raw_html opts -> return $ literal str
                | isEnabled Ext_raw_attribute opts -> rawAttribInline
                | otherwise -> renderEmpty
      | f `elem` ["latex", "tex"] ->
            case () of
              _ | isEnabled Ext_raw_tex opts -> return $ literal str
                | isEnabled Ext_raw_attribute opts -> rawAttribInline
                | otherwise -> renderEmpty
      | otherwise -> renderEmpty
inlineToMarkdown opts LineBreak = do
  variant <- asks envVariant
  if variant == PlainText || isEnabled Ext_hard_line_breaks opts
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
           return $ literal ("@" <> citationId c) <+> br
         else do
           cits <- mapM convertOne (c:cs)
           return $ literal "[" <> joincits cits <> literal "]"
  where
        joincits = hcat . intersperse (literal "; ") . filter (not . isEmpty)
        convertOne Citation { citationId      = k
                            , citationPrefix  = pinlines
                            , citationSuffix  = sinlines
                            , citationMode    = m }
                               = do
           pdoc <- inlineListToMarkdown opts pinlines
           sdoc <- inlineListToMarkdown opts sinlines
           let k' = literal (modekey m <> "@" <> k)
               r = case sinlines of
                        Str (T.uncons -> Just (y,_)):_ | y `elem` (",;]@" :: String) -> k' <> sdoc
                        _                                         -> k' <+> sdoc
           return $ pdoc <+> r
        modekey SuppressAuthor = "-"
        modekey _              = ""
inlineToMarkdown opts lnk@(Link attr txt (src, tit))
  | isEnabled Ext_raw_html opts &&
    not (isEnabled Ext_link_attributes opts) &&
    attr /= nullAttr = -- use raw HTML
    (literal . T.strip) <$>
      writeHtml5String opts{ writerTemplate = Nothing } (Pandoc nullMeta [Plain [lnk]])
  | otherwise = do
  variant <- asks envVariant
  linktext <- inlineListToMarkdown opts txt
  let linktitle = if T.null tit
                     then empty
                     else literal $ " \"" <> tit <> "\""
  let srcSuffix = fromMaybe src (T.stripPrefix "mailto:" src)
  let useAuto = isURI src &&
                case txt of
                      [Str s] | escapeURI s == srcSuffix -> True
                      _       -> False
  let useRefLinks = writerReferenceLinks opts && not useAuto
  shortcutable <- asks envRefShortcutable
  let useShortcutRefLinks = shortcutable &&
                            isEnabled Ext_shortcut_reference_links opts
  reftext <- if useRefLinks
                then literal <$> getReference attr linktext (src, tit)
                else return mempty
  return $ if useAuto
              then case variant of
                     PlainText -> literal srcSuffix
                     _ -> "<" <> literal srcSuffix <> ">"
              else if useRefLinks
                      then let first  = "[" <> linktext <> "]"
                               second = if getKey linktext == getKey reftext
                                           then if useShortcutRefLinks
                                                   then ""
                                                   else "[]"
                                           else "[" <> reftext <> "]"
                           in  first <> second
                      else case variant of
                             PlainText -> linktext
                             _ -> "[" <> linktext <> "](" <>
                                   literal src <> linktitle <> ")" <>
                                   linkAttributes opts attr
inlineToMarkdown opts img@(Image attr alternate (source, tit))
  | isEnabled Ext_raw_html opts &&
    not (isEnabled Ext_link_attributes opts) &&
    attr /= nullAttr = -- use raw HTML
    (literal . T.strip) <$>
      writeHtml5String opts{ writerTemplate = Nothing } (Pandoc nullMeta [Plain [img]])
  | otherwise = do
  variant <- asks envVariant
  let txt = if null alternate || alternate == [Str source]
                                 -- to prevent autolinks
               then [Str ""]
               else alternate
  linkPart <- inlineToMarkdown opts (Link attr txt (source, tit))
  return $ case variant of
             PlainText -> "[" <> linkPart <> "]"
             _     -> "!" <> linkPart
inlineToMarkdown opts (Note contents) = do
  modify (\st -> st{ stNotes = contents : stNotes st })
  st <- get
  let ref = literal $ writerIdentifierPrefix opts <> tshow (stNoteNum st + (length $ stNotes st) - 1)
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

toSubscriptInline :: Inline -> Maybe Inline
toSubscriptInline Space = Just Space
toSubscriptInline (Span attr ils) = Span attr <$> traverse toSubscriptInline ils
toSubscriptInline (Str s) = Str . T.pack <$> traverse toSubscript (T.unpack s)
toSubscriptInline LineBreak = Just LineBreak
toSubscriptInline SoftBreak = Just SoftBreak
toSubscriptInline _ = Nothing

toSuperscriptInline :: Inline -> Maybe Inline
toSuperscriptInline Space = Just Space
toSuperscriptInline (Span attr ils) = Span attr <$> traverse toSuperscriptInline ils
toSuperscriptInline (Str s) = Str . T.pack <$> traverse toSuperscript (T.unpack s)
toSuperscriptInline LineBreak = Just LineBreak
toSuperscriptInline SoftBreak = Just SoftBreak
toSuperscriptInline _ = Nothing
