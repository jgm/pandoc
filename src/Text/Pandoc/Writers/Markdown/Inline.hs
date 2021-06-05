{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{- |
   Module      : Text.Pandoc.Writers.Markdown.Inline
   Copyright   : Copyright (C) 2006-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable
-}
module Text.Pandoc.Writers.Markdown.Inline (
  inlineListToMarkdown,
  linkAttributes,
  attrsToMarkdown
  ) where
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Char (isAlphaNum, isDigit)
import Data.List (find, intersperse)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP (urlEncode)
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (blankline, blanklines, char, space)
import Text.DocLayout
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Walk
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Writers.Math (texMathToInlines)
import Text.Pandoc.XML (toHtml5Entities)
import Data.Coerce (coerce)
import Text.Pandoc.Writers.Markdown.Types (MarkdownVariant(..),
                                           WriterState(..),
                                           WriterEnv(..), MD)

-- | Escape special characters for Markdown.
escapeText :: WriterOptions -> Text -> Text
escapeText opts = T.pack . go . T.unpack
 where
  startsWithSpace (' ':_) = True
  startsWithSpace ('\t':_) = True
  startsWithSpace [] = True
  startsWithSpace _ = False
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
                      | isAlphaNum d || d == '_' || d == '{'
                         -> '\\':'@':go cs
                    _ -> '@':go cs
       '#' | isEnabled Ext_space_in_atx_header opts
           , startsWithSpace cs
           -> '\\':'#':go cs
       _ | c `elem` ['\\','`','*','_','[',']'] ->
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
                '#':xs           -> c : '#' : go xs
                '>':xs           -> c : '>' : go xs
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
  if (isEnabled Ext_link_attributes opts || isEnabled Ext_attributes opts) && attr /= nullAttr
     then attrsToMarkdown attr
     else empty

getKey :: Doc Text -> Key
getKey = toKey . render Nothing

findUsableIndex :: [Text] -> Int -> Int
findUsableIndex lbls i = if tshow i `elem` lbls
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
        go (x@Math{}:y@(Str t):zs)
          | T.all isDigit (T.take 1 t) -- starts with digit -- see #7058
          = liftM2 (<>) (inlineToMarkdown opts x)
              (go (RawInline (Format "html") "<!-- -->" : y : zs))
        go (i:is) = case i of
            Link {} -> case is of
                -- If a link is followed by another link, or '[', '(' or ':'
                -- then we don't shortcut
                Link {}:_                                       -> unshortcutable
                Space:Link {}:_                                 -> unshortcutable
                Space:(Str(thead -> Just '[')):_                -> unshortcutable
                Space:(RawInline _ (thead -> Just '[')):_       -> unshortcutable
                Space:(Cite _ _):_                              -> unshortcutable
                SoftBreak:Link {}:_                             -> unshortcutable
                SoftBreak:(Str(thead -> Just '[')):_            -> unshortcutable
                SoftBreak:(RawInline _ (thead -> Just '[')):_   -> unshortcutable
                SoftBreak:(Cite _ _):_                          -> unshortcutable
                LineBreak:Link {}:_                             -> unshortcutable
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
avoidBadWrapsInList [s, Str (T.uncons -> Just (c, cs))]
  | T.null cs && isSp s && c `elem` ['-','*','+'] = [Str $ T.pack [' ', c]]
avoidBadWrapsInList (s:Str (T.uncons -> Just (c, cs)):Space:xs)
  | T.null cs && isSp s && c `elem` ['-','*','+'] =
    Str (T.pack [' ', c]) : Space : avoidBadWrapsInList xs
avoidBadWrapsInList (s:Str cs:Space:xs)
  | isSp s && isOrderedListMarker cs =
    Str (" " <> cs) : Space : avoidBadWrapsInList xs
avoidBadWrapsInList [s, Str cs]
  | isSp s && isOrderedListMarker cs = [Str $ " " <> cs]
avoidBadWrapsInList (x:xs) = x : avoidBadWrapsInList xs

isOrderedListMarker :: Text -> Bool
isOrderedListMarker xs = not (T.null xs) && (T.last xs `elem` ['.',')']) &&
              isRight (runParser (anyOrderedListMarker >> eof)
                       defaultParserState "" xs)
 where
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
  return $ case attrs of
             (_,["csl-block"],_) -> (cr <>)
             (_,["csl-left-margin"],_) -> (cr <>)
             (_,["csl-indent"],_) -> (cr <>)
             _ -> id
         $ case variant of
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
  local (\env -> env {envEscapeSpaces = envVariant env == Markdown}) $ do
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
  local (\env -> env {envEscapeSpaces = envVariant env == Markdown}) $ do
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
  let tickGroups   = filter (T.any (== '`')) $ T.group str
  let longest      = maybe 0 maximum $ nonEmpty $ map T.length tickGroups
  let marker       = T.replicate (longest + 1) "`"
  let spacer       = if longest == 0 then "" else " "
  let attrsEnabled = isEnabled Ext_inline_code_attributes opts ||
                     isEnabled Ext_attributes opts
  let attrs        = if attrsEnabled && attr /= nullAttr
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
  let numticks   = 1 + maybe 0 maximum (nonEmpty (map T.length tickGroups))
  variant <- asks envVariant
  let Format fmt = f
  let rawAttribInline = return $
         literal (T.replicate numticks "`") <> literal str <>
         literal (T.replicate numticks "`") <> literal "{=" <> literal fmt <> literal "}"
  let renderEmpty = mempty <$ report (InlineNotRendered il)
  case variant of
    PlainText -> renderEmpty
    Commonmark
      | f `elem` ["gfm", "commonmark", "commonmark_x", "markdown"]
         -> return $ literal str
    Markdown
      | f `elem` ["markdown", "markdown_github", "markdown_phpextra",
                  "markdown_mmd", "markdown_strict"]
         -> return $ literal str
    _ | isEnabled Ext_raw_attribute opts -> rawAttribInline
      | f `elem` ["html", "html5", "html4"]
      , isEnabled Ext_raw_html opts
         -> return $ literal str
      | f `elem` ["latex", "tex"]
      , isEnabled Ext_raw_tex opts
         -> return $ literal str
    _ -> renderEmpty


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
           return $ literal ("@" <> maybeInBraces (citationId c)) <+> br
         else do
           cits <- mapM convertOne (c:cs)
           return $ literal "[" <> joincits cits <> literal "]"
  where
        maybeInBraces key =
          case readWith (citeKey False >> spaces >> eof)
                 defaultParserState ("@" <> key) of
            Left _  -> "{" <> key <> "}"
            Right _ -> key
        joincits = hcat . intersperse (literal "; ") . filter (not . isEmpty)
        convertOne Citation { citationId      = k
                            , citationPrefix  = pinlines
                            , citationSuffix  = sinlines
                            , citationMode    = m }
                               = do
           pdoc <- inlineListToMarkdown opts pinlines
           sdoc <- inlineListToMarkdown opts sinlines
           let k' = literal (modekey m <> "@" <> maybeInBraces k)
               r = case sinlines of
                        Str (T.uncons -> Just (y,_)):_ | y `elem` (",;]@" :: String) -> k' <> sdoc
                        _                                         -> k' <+> sdoc
           return $ pdoc <+> r
        modekey SuppressAuthor = "-"
        modekey _              = ""
inlineToMarkdown opts lnk@(Link attr txt (src, tit)) = do
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
  case variant of
    PlainText
      | useAuto -> return $ literal srcSuffix
      | otherwise -> return linktext
    _ | useAuto -> return $ "<" <> literal srcSuffix <> ">"
      | useRefLinks ->
           let first  = "[" <> linktext <> "]"
               second = if getKey linktext == getKey reftext
                           then if useShortcutRefLinks
                                   then ""
                                   else "[]"
                           else "[" <> reftext <> "]"
           in  return $ first <> second
      | isEnabled Ext_raw_html opts
      , not (isEnabled Ext_link_attributes opts || isEnabled Ext_attributes opts)
      , attr /= nullAttr -> -- use raw HTML to render attributes
          literal . T.strip <$>
            writeHtml5String opts{ writerTemplate = Nothing }
            (Pandoc nullMeta [Plain [lnk]])
      | otherwise -> return $
         "[" <> linktext <> "](" <> literal src <> linktitle <> ")" <>
         linkAttributes opts attr
inlineToMarkdown opts img@(Image attr alternate (source, tit))
  | isEnabled Ext_raw_html opts &&
    not (isEnabled Ext_link_attributes opts || isEnabled Ext_attributes opts) &&
    attr /= nullAttr = -- use raw HTML
    literal . T.strip <$>
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
  let ref = literal $ writerIdentifierPrefix opts <> tshow (stNoteNum st + length (stNotes st) - 1)
  if isEnabled Ext_footnotes opts
     then return $ "[^" <> ref <> "]"
     else return $ "[" <> ref <> "]"

makeMathPlainer :: [Inline] -> [Inline]
makeMathPlainer = walk go
  where
  go (Emph xs) = Span nullAttr xs
  go x         = x

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
