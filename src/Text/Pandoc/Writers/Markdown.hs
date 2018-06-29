{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-
Copyright (C) 2006-2018 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Writers.Markdown
   Copyright   : Copyright (C) 2006-2018 John MacFarlane
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
import Data.Char (chr, isPunctuation, isSpace, ord, isAlphaNum)
import Data.Default
import qualified Data.HashMap.Strict as H
import Data.List (find, group, intersperse, sortBy, stripPrefix, transpose)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Any (..))
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson (Value (Array, Bool, Number, Object, String))
import Network.HTTP (urlEncode)
import Text.HTML.TagSoup (Tag (..), isTagText, parseTags)
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (blankline, blanklines, char, space)
import Text.Pandoc.Pretty
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Walk
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Writers.Math (texMathToInlines)
import Text.Pandoc.Writers.Shared

type Notes = [[Block]]
type Ref   = (Doc, Target, Attr)
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
                               , stRefs    :: Refs
                               , stKeys    :: M.Map Key
                                                (M.Map (Target, Attr) Int)
                               , stLastIdx  :: Int
                               , stIds     :: Set.Set String
                               , stNoteNum :: Int
                               }

instance Default WriterState
  where def = WriterState{ stNotes = []
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

pandocTitleBlock :: Doc -> [Doc] -> Doc -> Doc
pandocTitleBlock tit auths dat =
  hang 2 (text "% ") tit <> cr <>
  hang 2 (text "% ") (vcat $ map nowrap auths) <> cr <>
  hang 2 (text "% ") dat <> cr

mmdTitleBlock :: Value -> Doc
mmdTitleBlock (Object hashmap) =
  vcat $ map go $ sortBy (comparing fst) $ H.toList hashmap
  where go (k,v) =
          case (text (T.unpack k), v) of
               (k', Array vec)
                 | V.null vec     -> empty
                 | otherwise      -> k' <> ":" <> space <>
                                      hcat (intersperse "; "
                                           (map fromstr $ V.toList vec))
               (_, String "")  -> empty
               (k', x)         -> k' <> ":" <> space <> nest 2 (fromstr x)
        fromstr (String s) = text (removeBlankLines $ T.unpack s)
        fromstr (Bool b)   = text (show b)
        fromstr (Number n) = text (show n)
        fromstr _          = empty
        -- blank lines not allowed in MMD metadata - we replace with .
        removeBlankLines   = trimr . unlines . map (\x ->
                               if all isSpace x then "." else x) . lines
mmdTitleBlock _ = empty

plainTitleBlock :: Doc -> [Doc] -> Doc -> Doc
plainTitleBlock tit auths dat =
  tit <> cr <>
  (hcat (intersperse (text "; ") auths)) <> cr <>
  dat <> cr

yamlMetadataBlock :: Value -> Doc
yamlMetadataBlock v = "---" $$ (jsonToYaml v) $$ "---"

jsonToYaml :: Value -> Doc
jsonToYaml (Object hashmap) =
  vcat $ map (\(k,v) ->
          case (text (T.unpack k), v, jsonToYaml v) of
               (k', Array vec, x)
                 | V.null vec     -> empty
                 | otherwise      -> (k' <> ":") $$ x
               (k', Object _, x)  -> (k' <> ":") $$ nest 2 x
               (_, String "", _)  -> empty
               (k', _, x)         -> k' <> ":" <> space <> hang 2 "" x)
       $ sortBy (comparing fst) $ H.toList hashmap
jsonToYaml (Array vec) =
  vcat $ map (\v -> hang 2 "- " (jsonToYaml v)) $ V.toList vec
jsonToYaml (String "") = empty
jsonToYaml (String s) =
  case T.unpack s of
     x | '\n' `elem` x -> hang 2 ("|" <> cr) $ text x
       | not (any isPunctuation x) -> text x
       | otherwise     -> text $ "'" ++ substitute "'" "''" x ++ "'"
jsonToYaml (Bool b) = text $ show b
jsonToYaml (Number n) = text $ show n
jsonToYaml _ = empty

-- | Return markdown representation of document.
pandocToMarkdown :: PandocMonad m => WriterOptions -> Pandoc -> MD m Text
pandocToMarkdown opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  isPlain <- asks envPlain
  let render' :: Doc -> Text
      render' = render colwidth . chomp
  metadata <- metaToJSON'
               (fmap render' . blockListToMarkdown opts)
               (fmap render' . blockToMarkdown opts . Plain)
               meta
  let title' = maybe empty text $ getField "title" metadata
  let authors' = maybe [] (map text) $ getField "author" metadata
  let date' = maybe empty text $ getField "date" metadata
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
         then render' <$> tableOfContents opts headerBlocks
         else return ""
  -- Strip off final 'references' header if markdown citations enabled
  let blocks' = if isEnabled Ext_citations opts
                   then case reverse blocks of
                             (Div (_,["references"],_) _):xs -> reverse xs
                             _                               -> blocks
                   else blocks
  body <- blockListToMarkdown opts blocks'
  notesAndRefs' <- notesAndRefs opts
  let main = render' $ body <> notesAndRefs'
  let context  = -- for backwards compatibility we populate toc
                 -- with the contents of the toc, rather than a
                 -- boolean:
                 defField "toc" toc
               $ defField "table-of-contents" toc
               $ defField "body" main
               $ (if isNullMeta meta
                     then id
                     else defField "titleblock" (render' titleblock))
               $ addVariablesToJSON opts metadata
  case writerTemplate opts of
       Nothing  -> return main
       Just tpl -> renderTemplate' tpl context

-- | Return markdown representation of reference key table.
refsToMarkdown :: PandocMonad m => WriterOptions -> Refs -> MD m Doc
refsToMarkdown opts refs = mapM (keyToMarkdown opts) refs >>= return . vcat

-- | Return markdown representation of a reference key.
keyToMarkdown :: PandocMonad m
              => WriterOptions
              -> Ref
              -> MD m Doc
keyToMarkdown opts (label', (src, tit), attr) = do
  let tit' = if null tit
                then empty
                else space <> "\"" <> text tit <> "\""
  return $ nest 2 $ hang 2
            ("[" <> label' <> "]:" <> space) (text src <> tit')
            <+> linkAttributes opts attr

-- | Return markdown representation of notes.
notesToMarkdown :: PandocMonad m => WriterOptions -> [[Block]] -> MD m Doc
notesToMarkdown opts notes = do
  n <- gets stNoteNum
  notes' <- mapM (\(num, note) -> noteToMarkdown opts num note) (zip [n..] notes)
  modify $ \st -> st { stNoteNum = stNoteNum st + length notes }
  return $ vsep notes'

-- | Return markdown representation of a note.
noteToMarkdown :: PandocMonad m => WriterOptions -> Int -> [Block] -> MD m Doc
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
escapeString _  [] = []
escapeString opts (c:cs) =
  case c of
       '<' | isEnabled Ext_all_symbols_escapable opts ->
              '\\' : '<' : escapeString opts cs
           | otherwise -> "&lt;" ++ escapeString opts cs
       '>' | isEnabled Ext_all_symbols_escapable opts ->
              '\\' : '>' : escapeString opts cs
           | otherwise -> "&gt;" ++ escapeString opts cs
       '@' | isEnabled Ext_citations opts ->
               case cs of
                    (d:_)
                      | isAlphaNum d || d == '_'
                         -> '\\':'@':escapeString opts cs
                    _ -> '@':escapeString opts cs
       _ | c `elem` ['\\','`','*','_','[',']','#'] ->
              '\\':c:escapeString opts cs
       '|' | isEnabled Ext_pipe_tables opts -> '\\':'|':escapeString opts cs
       '^' | isEnabled Ext_superscript opts -> '\\':'^':escapeString opts cs
       '~' | isEnabled Ext_subscript opts -> '\\':'~':escapeString opts cs
       '$' | isEnabled Ext_tex_math_dollars opts -> '\\':'$':escapeString opts cs
       '\'' | isEnabled Ext_smart opts -> '\\':'\'':escapeString opts cs
       '"' | isEnabled Ext_smart opts -> '\\':'"':escapeString opts cs
       '-' | isEnabled Ext_smart opts ->
              case cs of
                   '-':_ -> '\\':'-':escapeString opts cs
                   _     -> '-':escapeString opts cs
       '.' | isEnabled Ext_smart opts ->
              case cs of
                   '.':'.':rest -> '\\':'.':'.':'.':escapeString opts rest
                   _            -> '.':escapeString opts cs
       _ -> c : escapeString opts cs

-- | Construct table of contents from list of header blocks.
tableOfContents :: PandocMonad m => WriterOptions -> [Block] -> MD m Doc
tableOfContents opts headers = do
  contents <- BulletList <$> mapM (elementToListItem opts) (hierarchicalize headers)
  blockToMarkdown opts contents

-- | Converts an Element to a list item for a table of contents,
elementToListItem :: PandocMonad m => WriterOptions -> Element -> MD m [Block]
elementToListItem opts (Sec lev _nums (ident,_,_) headerText subsecs)
  = do isPlain <- asks envPlain
       let headerLink = if null ident || isPlain
                         then walk deNote headerText
                         else [Link nullAttr (walk deNote headerText)
                                 ('#':ident, "")]
       listContents <- if null subsecs || lev >= writerTOCDepth opts
                          then return []
                          else mapM (elementToListItem opts) subsecs
       return [Plain headerLink, BulletList listContents]
elementToListItem _ (Blk _) = return []

attrsToMarkdown :: Attr -> Doc
attrsToMarkdown attribs = braces $ hsep [attribId, attribClasses, attribKeys]
        where attribId = case attribs of
                                ([],_,_) -> empty
                                (i,_,_)  -> "#" <> text i
              attribClasses = case attribs of
                                (_,[],_) -> empty
                                (_,cs,_) -> hsep $
                                            map (text . ('.':))
                                            cs
              attribKeys = case attribs of
                                (_,_,[]) -> empty
                                (_,_,ks) -> hsep $
                                            map (\(k,v) -> text k
                                              <> "=\"" <> text v <> "\"") ks

linkAttributes :: WriterOptions -> Attr -> Doc
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

notesAndRefs :: PandocMonad m => WriterOptions -> MD m Doc
notesAndRefs opts = do
  notes' <- reverse <$> gets stNotes >>= notesToMarkdown opts
  modify $ \s -> s { stNotes = [] }
  refs' <- reverse <$> gets stRefs >>= refsToMarkdown opts
  modify $ \s -> s { stRefs = [] }

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
                -> MD m Doc
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
                 -> MD m Doc
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
           | otherwise -> contents <> blankline
blockToMarkdown' opts (Plain inlines) = do
  contents <- inlineListToMarkdown opts inlines
  -- escape if para starts with ordered list marker
  isPlain <- asks envPlain
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  let rendered = render colwidth contents
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
    (text . T.unpack . T.strip) <$>
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
blockToMarkdown' opts b@(RawBlock f str)
  | f `elem` ["markdown", "markdown_github", "markdown_phpextra",
              "markdown_mmd", "markdown_strict"]
              = return $ text str <> text "\n"
  | f `elem` ["html", "html5", "html4"] && isEnabled Ext_raw_html opts = do
    plain <- asks envPlain
    return $ if plain
                then empty
                else if isEnabled Ext_markdown_attribute opts
                        then text (addMarkdownAttribute str) <> text "\n"
                        else text str <> text "\n"
  | f `elem` ["latex", "tex"] && isEnabled Ext_raw_tex opts = do
    plain <- asks envPlain
    return $ if plain
                then empty
                else text str <> text "\n"
  | otherwise = do
      report $ BlockNotRendered b
      return empty
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
  let autoId = uniqueIdent inlines ids
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
                 if level == 1 && plain
                    then capitalize inlines
                    else inlines
  let setext = writerSetextHeaders opts
      hdr = nowrap $ case level of
            1 | plain -> blanklines 3 <> contents <> blanklines 2
              | setext ->
                  contents <> attr' <> cr <> text (replicate (offset contents) '=') <>
                  blankline
            2 | plain -> blanklines 2 <> contents <> blankline
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
   where tildes    = text $ case [ln | ln <- lines str, all (=='~') ln] of
                               [] -> "~~~~"
                               xs -> case maximum $ map length xs of
                                          n | n < 3 -> "~~~~"
                                            | otherwise -> replicate (n+1) '~'
         backticks = text $ case [ln | ln <- lines str, all (=='`') ln] of
                               [] -> "```"
                               xs -> case maximum $ map length xs of
                                          n | n < 3 -> "```"
                                            | otherwise -> replicate (n+1) '`'
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
  let isLineBreak LineBreak = Any True
      isLineBreak _         = Any False
  let hasLineBreak = getAny . query isLineBreak
  let isSimpleCell [Plain ils] = not (hasLineBreak ils)
      isSimpleCell [Para ils ] = not (hasLineBreak ils)
      isSimpleCell []          = True
      isSimpleCell _           = False
  let hasSimpleCells = all isSimpleCell (concat (headers:rows))
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
            | isEnabled Ext_raw_html opts -> fmap (id,) $
                   (text . T.unpack) <$>
                   (writeHtml5String opts{ writerTemplate = Nothing } $ Pandoc nullMeta [t])
            | hasSimpleCells &&
              isEnabled Ext_pipe_tables opts -> do
                rawHeaders <- padRow <$> mapM (blockListToMarkdown opts) headers
                rawRows <- mapM (fmap padRow . mapM (blockListToMarkdown opts))
                           rows
                (id,) <$> pipeTable (all null headers) aligns' rawHeaders rawRows
            | otherwise -> return $ (id, text "[TABLE]")
  return $ nst $ tbl $$ caption'' $$ blankline
blockToMarkdown' opts (BulletList items) = do
  contents <- inList $ mapM (bulletListItemToMarkdown opts) items
  return $ cat contents <> blankline
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
  return $ cat contents <> blankline
blockToMarkdown' opts (DefinitionList items) = do
  contents <- inList $ mapM (definitionListItemToMarkdown opts) items
  return $ cat contents <> blankline

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

pipeTable :: PandocMonad m => Bool -> [Alignment] -> [Doc] -> [[Doc]] -> MD m Doc
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
            -> [Doc] -> [[Doc]] -> MD m Doc
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
  let underline = cat $ intersperse (text " ") $
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
bulletListItemToMarkdown :: PandocMonad m => WriterOptions -> [Block] -> MD m Doc
bulletListItemToMarkdown opts bs = do
  contents <- blockListToMarkdown opts bs
  let sps = replicate (writerTabStop opts - 2) ' '
  let start = text ('-' : ' ' : sps)
  -- remove trailing blank line if item ends with a tight list
  let contents' = if itemEndsWithTightList bs
                     then chomp contents <> cr
                     else contents
  return $ hang (writerTabStop opts) start $ contents' <> cr

-- | Convert ordered list item (a list of blocks) to markdown.
orderedListItemToMarkdown :: PandocMonad m
                          => WriterOptions -- ^ options
                          -> String        -- ^ list item marker
                          -> [Block]       -- ^ list item (list of blocks)
                          -> MD m Doc
orderedListItemToMarkdown opts marker bs = do
  contents <- blockListToMarkdown opts bs
  let sps = case length marker - writerTabStop opts of
                   n | n > 0 -> text $ replicate n ' '
                   _ -> text " "
  let start = text marker <> sps
  -- remove trailing blank line if item ends with a tight list
  let contents' = if itemEndsWithTightList bs
                     then chomp contents <> cr
                     else contents
  return $ hang (writerTabStop opts) start $ contents' <> cr

-- | Convert definition list item (label, list of blocks) to markdown.
definitionListItemToMarkdown :: PandocMonad m
                             => WriterOptions
                             -> ([Inline],[[Block]])
                             -> MD m Doc
definitionListItemToMarkdown opts (label, defs) = do
  labelText <- inlineListToMarkdown opts label
  defs' <- mapM (mapM (blockToMarkdown opts)) defs
  if isEnabled Ext_definition_lists opts
     then do
       let tabStop = writerTabStop opts
       isPlain <- asks envPlain
       let leader  = if isPlain then "   " else ":  "
       let sps = case writerTabStop opts - 3 of
                      n | n > 0   -> text $ replicate n ' '
                      _ -> text " "
       if isEnabled Ext_compact_definition_lists opts
          then do
            let contents = vcat $ map (\d -> hang tabStop (leader <> sps)
                                $ vcat d <> cr) defs'
            return $ nowrap labelText <> cr <> contents <> cr
          else do
            let contents = vcat $ map (\d -> hang tabStop (leader <> sps)
                                $ vcat d <> cr) defs'
            let isTight = case defs of
                               ((Plain _ : _): _) -> True
                               _                  -> False
            return $ blankline <> nowrap labelText <>
                     (if isTight then cr else blankline) <> contents <> blankline
     else do
       return $ nowrap labelText <> text "  " <> cr <>
                vsep (map vsep defs') <> blankline

-- | Convert list of Pandoc block elements to markdown.
blockListToMarkdown :: PandocMonad m
                    => WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> MD m Doc
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
  mapM (blockToMarkdown opts) (fixBlocks blocks) >>= return . cat

getKey :: Doc -> Key
getKey = toKey . render Nothing

-- | Get reference for target; if none exists, create unique one and return.
--   Prefer label if possible; otherwise, generate a unique key.
getReference :: PandocMonad m => Attr -> Doc -> Target -> MD m Doc
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
                                 i <- (+ 1) <$> gets stLastIdx
                                 modify $ \s -> s{ stLastIdx = i }
                                 return (text (show i), i)
                               else return (label, 0)
             modify (\s -> s{
               stRefs = (lab', target, attr) : refs,
               stKeys = M.insert (getKey label)
                           (M.insert (target, attr) idx mempty)
                                 (stKeys s) })
             return lab'

           Just km -> do -- we have refs with this label
             case M.lookup (target, attr) km of
                  Just i -> do
                    let lab' = label <> if i == 0
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
                    i <- (+ 1) <$> gets stLastIdx
                    modify $ \s -> s{ stLastIdx = i }
                    let lab' = text (show i)
                    modify (\s -> s{
                       stRefs = (lab', target, attr) : refs,
                       stKeys = M.insert (getKey label)
                                   (M.insert (target, attr) i km)
                                         (stKeys s) })
                    return lab'

-- | Convert list of Pandoc inline elements to markdown.
inlineListToMarkdown :: PandocMonad m => WriterOptions -> [Inline] -> MD m Doc
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
inlineToMarkdown :: PandocMonad m => WriterOptions -> Inline -> MD m Doc
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
              then "_" <> contents <> "_"
              else "*" <> contents <> "*"
inlineToMarkdown _ (Strong []) = return empty
inlineToMarkdown opts (Strong lst) = do
  plain <- asks envPlain
  if plain
     then inlineListToMarkdown opts $ capitalize lst
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
                           let rendered = render Nothing contents
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
                           let rendered = render Nothing contents
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
              else "‘" <> contents <> "’"
inlineToMarkdown opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ if isEnabled Ext_smart opts
              then "\"" <> contents <> "\""
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
  plain <- asks envPlain
  if (plain && f == "plain") || (not plain &&
     ( f `elem` ["markdown", "markdown_github", "markdown_phpextra",
                 "markdown_mmd", "markdown_strict"] ||
       (isEnabled Ext_raw_tex opts && (f == "latex" || f == "tex")) ||
       (isEnabled Ext_raw_html opts && f `elem` ["html", "html4", "html5"])
     ))
    then return $ text str
    else do
      report $ InlineNotRendered il
      return empty
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
  reftext <- if useRefLinks then getReference attr linktext (src, tit)
                            else return empty
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

toSuperscript :: Char -> Maybe Char
toSuperscript '1' = Just '\x00B9'
toSuperscript '2' = Just '\x00B2'
toSuperscript '3' = Just '\x00B3'
toSuperscript '+' = Just '\x207A'
toSuperscript '-' = Just '\x207B'
toSuperscript '=' = Just '\x207C'
toSuperscript '(' = Just '\x207D'
toSuperscript ')' = Just '\x207E'
toSuperscript c
  | c >= '0' && c <= '9' =
                 Just $ chr (0x2070 + (ord c - 48))
  | isSpace c = Just c
  | otherwise = Nothing

toSubscript :: Char -> Maybe Char
toSubscript '+' = Just '\x208A'
toSubscript '-' = Just '\x208B'
toSubscript '=' = Just '\x208C'
toSubscript '(' = Just '\x208D'
toSubscript ')' = Just '\x208E'
toSubscript c
  | c >= '0' && c <= '9' =
                 Just $ chr (0x2080 + (ord c - 48))
  | isSpace c = Just c
  | otherwise = Nothing

lineBreakToSpace :: Inline -> Inline
lineBreakToSpace LineBreak = Space
lineBreakToSpace SoftBreak = Space
lineBreakToSpace x         = x
