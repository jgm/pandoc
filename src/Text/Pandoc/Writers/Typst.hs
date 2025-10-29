{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Writers.Typst
   Copyright   : Copyright (C) 2023 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' format into Typst markup
(<https://typst.app>).
-}
module Text.Pandoc.Writers.Typst (
    writeTypst
  ) where
import Text.Pandoc.Definition
import Text.Pandoc.Class ( PandocMonad, report )
import Text.Pandoc.ImageSize ( dimension, Dimension(Pixel), Direction(..),
                               showInInch )
import Text.Pandoc.Options ( WriterOptions(..), WrapOption(..), isEnabled,
                             CaptionPosition(..), HighlightMethod(..) )
import Data.Text (Text)
import Data.List (intercalate, intersperse)
import Data.Bifunctor (first, second)
import Network.URI (unEscapeString)
import qualified Data.Text as T
import Control.Monad (unless)
import Control.Monad.State ( StateT, evalStateT, gets, modify )
import Text.Pandoc.Writers.Shared ( lookupMetaInlines, lookupMetaString,
                                    metaToContext, defField, resetField,
                                    setupTranslations )
import Text.Pandoc.Shared (isTightList, orderedListMarkers, tshow)
import Text.Pandoc.Highlighting (highlight, formatTypstBlock, formatTypstInline,
                                 styleToTypst)
import Text.Pandoc.Translations (Term(Abstract), translateTerm)
import Text.Pandoc.Walk (query)
import Text.Pandoc.Writers.Math (convertMath)
import qualified Text.TeXMath as TM
import Text.DocLayout
import Text.DocTemplates (renderTemplate)
import Text.Pandoc.Extensions (Extension(..))
import Text.Pandoc.Logging (LogMessage(..))
import Text.Collate.Lang (Lang(..), parseLang)
import Text.Printf (printf)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Unicode.Char (isXIDContinue)

-- | Convert Pandoc to Typst.
writeTypst :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeTypst options document =
  evalStateT (pandocToTypst options document)
    WriterState{ stOptions = options,
                 stEscapeContext = NormalContext,
                 stHighlighting = False }

data EscapeContext = NormalContext | TermContext
  deriving (Show, Eq)

data WriterState =
  WriterState {
    stOptions :: WriterOptions,
    stEscapeContext :: EscapeContext,
    stHighlighting :: Bool
    }

type TW m = StateT WriterState m

pandocToTypst :: PandocMonad m
              => WriterOptions -> Pandoc -> TW m Text
pandocToTypst options (Pandoc meta blocks) = do
  let colwidth = if writerWrapText options == WrapAuto
                    then Just $ writerColumns options
                    else Nothing
  setupTranslations meta
  metadata <- metaToContext options
              blocksToTypst
              (fmap chomp . inlinesToTypst)
              meta
  main <- blocksToTypst blocks
  abstractTitle <- translateTerm Abstract
  let toPosition :: CaptionPosition -> Text
      toPosition CaptionAbove = "top"
      toPosition CaptionBelow = "bottom"
  let nociteIds = query (\case
                           Cite cs _ -> map citationId cs
                           _         -> [])
                  $ lookupMetaInlines "nocite" meta
  hasHighlighting <- gets stHighlighting

  let context = defField "body" main
              $ defField "toc" (writerTableOfContents options)
              $ (if isEnabled Ext_citations options
                    then defField "citations" True
                       . defField "nocite-ids" (filter (/= "*") nociteIds)
                       . defField "full-bibliography" ("*" `elem` nociteIds)
                    else id)
              $ (case lookupMetaString "lang" meta of
                    "" -> id
                    lang ->
                      case parseLang lang of
                        Left _ -> id
                        Right l ->
                          resetField "lang" (langLanguage l) .
                          maybe id (resetField "region") (langRegion l))
              $ defField "csl" (lookupMetaString "citation-style" meta) -- #10661
              $ defField "smart" (isEnabled Ext_smart options)
              $ defField "abstract-title" abstractTitle
              $ defField "toc-depth" (tshow $ writerTOCDepth options)
              $ (if hasHighlighting
                    then case writerHighlightMethod options of
                           Skylighting sty ->
                              defField "highlighting-definitions"
                                (T.stripEnd $ styleToTypst sty)
                           _ -> id
                    else id)
              $ defField "figure-caption-position"
                   (toPosition $ writerFigureCaptionPosition options)
              $ defField "table-caption-position"
                   (toPosition $ writerTableCaptionPosition options)
              $ defField "page-numbering" ("1" :: Text)
              $ (if writerNumberSections options
                    then defField "section-numbering" ("1.1.1.1.1" :: Text)
                    else id)
              metadata
  return $ render colwidth $
    case writerTemplate options of
       Nothing  -> main
       Just tpl -> renderTemplate tpl context

pickTypstAttrs :: [(Text, Text)] -> ([(Text, Text)],[(Text, Text)])
pickTypstAttrs = foldr go ([],[])
  where
    go (k,v) =
      case T.splitOn ":" k of
        ["typst", "text", x] -> second ((x,v):)
        ["typst", x] -> first ((x,v):)
        _ -> id

formatTypstProp :: (Text, Text) -> Text
formatTypstProp (k,v) = k <> ": " <> v

toTypstPropsListSep :: [(Text, Text)] -> Doc Text
toTypstPropsListSep = hsep . intersperse "," . map (literal . formatTypstProp)

toTypstPropsListTerm :: [(Text, Text)] -> Doc Text
toTypstPropsListTerm [] = ""
toTypstPropsListTerm typstAttrs = toTypstPropsListSep typstAttrs <> ","

toTypstPropsListParens :: [(Text, Text)] -> Doc Text
toTypstPropsListParens [] = ""
toTypstPropsListParens typstAttrs = parens $ toTypstPropsListSep typstAttrs

toTypstTextElement :: [(Text, Text)] -> Doc Text -> Doc Text
toTypstTextElement [] content = content
toTypstTextElement typstTextAttrs content = "#text" <> toTypstPropsListParens typstTextAttrs <> brackets content

toTypstSetText :: [(Text, Text)] -> Doc Text
toTypstSetText [] = ""
toTypstSetText typstTextAttrs = "set text" <> parens (toTypstPropsListSep typstTextAttrs) <> "; "

toTypstPoundSetText :: [(Text, Text)] -> Doc Text
toTypstPoundSetText [] = ""
toTypstPoundSetText typstTextAttrs = "#" <> toTypstSetText typstTextAttrs

toTypstBracesSetText :: [(Text, Text)] -> Doc Text -> Doc Text
toTypstBracesSetText [] x = "#" <> x
toTypstBracesSetText typstTextAttrs x = "#" <> braces (toTypstSetText typstTextAttrs <> x)

blocksToTypst :: PandocMonad m => [Block] -> TW m (Doc Text)
blocksToTypst blocks = vcat <$> mapM blockToTypst blocks

blockToTypst :: PandocMonad m => Block -> TW m (Doc Text)
blockToTypst block =
  case block of
    Plain inlines -> inlinesToTypst inlines
    Para inlines -> do
      ($$ blankline) <$> inlinesToTypst inlines
    Header level (ident,cls,_) inlines -> do
      contents <- inlinesToTypst inlines
      let lab = toLabel FreestandingLabel ident
      let headingAttrs =
            ["outlined: false" | "unlisted" `elem` cls] ++
            ["numbering: none" | "unnumbered" `elem` cls]
      return $
        if null headingAttrs
           then nowrap
                 (literal (T.replicate level "=") <> space <> contents) <>
                 cr <> lab
           else literal "#heading" <>
                  parens (literal (T.intercalate ", "
                              ("level: " <> tshow level : headingAttrs))) <>
                  brackets contents <> cr <> lab
    RawBlock fmt str ->
      case fmt of
        Format "typst" -> return $ literal str
        _ -> return mempty
    CodeBlock (ident,cls,kvs) code -> do
      let go :: Char -> (Int, Int) -> (Int, Int)
          go '`' (longest, current) =
            let !new = current + 1 in (max longest new, new)
          go _ (longest, _) = (longest, 0)
      let (longestBacktickSequence, _) = T.foldr go (0,0) code
      let fence = literal $ T.replicate (max 3 (longestBacktickSequence + 1)) "`"
      let lang = case cls of
                   (cl:_) -> literal cl
                   _ -> mempty
      opts <-  gets stOptions
      case writerHighlightMethod opts of
        Skylighting _ ->
          case highlight (writerSyntaxMap opts) formatTypstBlock
                (ident,cls ++ ["default"],kvs) code of
            Left msg -> do
              unless (T.null msg) $ report $ CouldNotHighlight msg
              return $ fence <> lang <> cr <> literal code <> cr <> fence <> blankline
            Right h -> do
              modify (\s -> s{ stHighlighting = True })
              return (literal h)
        NoHighlighting -> return $ fence <> cr <> literal code <> cr <> fence <> blankline
        _ -> return $ fence <> lang <> cr <> literal code <> cr <> fence <> blankline
    LineBlock lns -> do
      contents <- inlinesToTypst (intercalate [LineBreak] lns)
      return $ contents <> blankline
    BlockQuote blocks -> do
      contents <- blocksToTypst blocks
      return $ "#quote(block: true)[" $$ chomp contents $$ "]" $$ blankline
    HorizontalRule ->
      return $ blankline <> "#horizontalrule" <> blankline
    OrderedList attribs items -> do
      let addBlock = case attribs of
                       (1, DefaultStyle, DefaultDelim) -> id
                       (1, Decimal, Period) -> id
                       (start, sty, delim) -> \x ->
                              "#block[" $$
                               ("#set enum" <>
                                  parens (
                                    "numbering: " <>
                                    doubleQuoted
                                     (case orderedListMarkers (1, sty, delim) of
                                          (m:_) -> m
                                          [] -> "1.") <>
                                    ", start: " <>
                                      text (show start) )) $$
                               x $$
                               "]"
      items' <- mapM (fmap chomp . listItemToTypst 2 ("+")) items
      return $ addBlock
               (if isTightList items
                   then vcat items'
                   else vsep items')
              $$ blankline
    BulletList items -> do
      items' <- mapM (fmap chomp . listItemToTypst 2 "-") items
      return $ (if isTightList items
                   then vcat items'
                   else vsep items') $$ blankline
    DefinitionList items ->
      ($$ blankline) . vsep <$> mapM defListItemToTypst items
    Table (ident,tabclasses,tabkvs) (Caption _ caption) colspecs thead tbodies tfoot -> do
      let lab = toLabel FreestandingLabel ident
      capt' <- if null caption
                  then return mempty
                  else do
                    captcontents <- blocksToTypst caption
                    return $ ", caption: " <> brackets captcontents
      let typstFigureKind = literal (", kind: " <> fromMaybe "table" (lookup "typst:figure:kind" tabkvs))
      let numcols = length colspecs
      let (aligns, widths) = unzip colspecs
      let commaSep = hcat . intersperse ", "
      let toPercentage (ColWidth w) =
            literal $ (T.dropWhileEnd (== '.') . T.dropWhileEnd (== '0'))
                         (T.pack (printf "%0.2f" (w * 100))) <> "%"
          toPercentage ColWidthDefault = literal "auto"
      let columns = if all (== ColWidthDefault) widths
                       then literal $ tshow numcols
                       else parens (commaSep (map toPercentage widths))
      let formatalign AlignLeft = "left,"
          formatalign AlignRight = "right,"
          formatalign AlignCenter = "center,"
          formatalign AlignDefault = "auto,"
      let alignarray = parens $ mconcat $ map formatalign aligns

      let fromCell (Cell (_,_,kvs) alignment rowspan colspan bs) = do
            let (typstAttrs, typstTextAttrs) = pickTypstAttrs kvs
            let valign =
                  (case lookup "align" typstAttrs of
                    Just va -> [va]
                    _ -> [])
            let typstAttrs2 = filter ((/="align") . fst) typstAttrs
            let halign =
                  (case alignment of
                    AlignDefault -> []
                    AlignLeft -> [ "left" ]
                    AlignRight -> [ "right" ]
                    AlignCenter -> [ "center" ])
            let cellaligns = valign ++ halign
            let cellattrs =
                  (case cellaligns of
                    [] -> []
                    _ -> [ "align: " <> T.intercalate " + " cellaligns ]) ++
                  (case rowspan of
                     RowSpan 1 -> []
                     RowSpan n -> [ "rowspan: " <> tshow n ]) ++
                  (case colspan of
                     ColSpan 1 -> []
                     ColSpan n -> [ "colspan: " <> tshow n ]) ++
                  map formatTypstProp typstAttrs2
            cellContents <- blocksToTypst bs
            let contents2 = brackets (toTypstPoundSetText typstTextAttrs <> cellContents)
            pure $ if null cellattrs
                      then contents2
                      else "table.cell" <>
                            parens
                             (literal (T.intercalate ", " cellattrs)) <>
                            contents2
      let fromRow (Row _ cs) =
            (<> ",") . commaSep <$> mapM fromCell cs
      let fromHead (TableHead _attr headRows) =
            if null headRows
               then pure mempty
               else (($$ "table.hline(),") .
                      (<> ",") . ("table.header" <>) . parens . nest 2 . vcat)
                      <$> mapM fromRow headRows
      let fromFoot (TableFoot _attr footRows) =
            if null footRows
               then pure mempty
               else (("table.hline()," $$) .
                      (<> ",") . ("table.footer" <>) . parens . nest 2 . vcat)
                      <$> mapM fromRow footRows
      let fromTableBody (TableBody _attr _rowHeadCols headRows bodyRows) = do
            hrows <- mapM fromRow headRows
            brows <- mapM fromRow bodyRows
            pure $ vcat (hrows ++ ["table.hline()," | not (null hrows)] ++ brows)
      let (typstAttrs, typstTextAttrs) = pickTypstAttrs tabkvs
      header <- fromHead thead
      footer <- fromFoot tfoot
      body <- vcat <$> mapM fromTableBody tbodies
      let table = "table("
            $$ nest 2
                (  "columns: " <> columns <> ","
                $$ "align: " <> alignarray <> ","
                $$ toTypstPropsListTerm typstAttrs
                $$ header
                $$ body
                $$ footer
            )
            $$ ")"
      return $ if "typst:no-figure" `elem` tabclasses
        then toTypstBracesSetText typstTextAttrs table
        else "#figure("
            $$
            nest 2
            ("align(center)[" <> toTypstPoundSetText typstTextAttrs <> "#" <> table <> "]"
              $$ capt'
              $$ typstFigureKind
              $$ ")")
            $$ lab
          $$ blankline
    Figure (ident,_,_) (Caption _mbshort capt) blocks -> do
      caption <- blocksToTypst capt
      opts <-  gets stOptions
      contents <- case blocks of
                     -- don't need #box around block-level image
                     [Para [Image attr _ (src, _)]]
                       -> pure $ mkImage opts False src attr
                     [Plain [Image attr _ (src, _)]]
                       -> pure $ mkImage opts False src attr
                     _ -> brackets <$> blocksToTypst blocks
      let lab = toLabel FreestandingLabel ident
      return $ "#figure(" <> nest 2 ((contents <> ",")
                                     $$
                                     ("caption: [" $$ nest 2 caption $$ "]")
                                    )
                          $$ ")" $$ lab $$ blankline
    Div (ident,_,_) (Header lev ("",cls,kvs) ils:rest) ->
      blocksToTypst (Header lev (ident,cls,kvs) ils:rest)
    Div (ident,_,kvs) blocks -> do
      let lab = toLabel FreestandingLabel ident
      let (typstAttrs,typstTextAttrs) = pickTypstAttrs kvs
      -- Handle lang attribute for Div elements
      let langAttrs = case lookup "lang" kvs of
                        Nothing -> []
                        Just lang -> case parseLang lang of
                                       Left _ -> []
                                       Right l -> [("lang",
                                                    tshow (langLanguage l))]
      let allTypstTextAttrs = typstTextAttrs ++ langAttrs
      contents <- blocksToTypst blocks
      return $ "#block" <> toTypstPropsListParens typstAttrs <> "["
        $$ toTypstPoundSetText allTypstTextAttrs <> contents
        $$ ("]" <+> lab)

defListItemToTypst :: PandocMonad m => ([Inline], [[Block]]) -> TW m (Doc Text)
defListItemToTypst (term, defns) = do
  modify $ \st -> st{ stEscapeContext = TermContext }
  term' <- inlinesToTypst term
  modify $ \st -> st{ stEscapeContext = NormalContext }
  defns' <- mapM blocksToTypst defns
  return $ nowrap ("/ " <> term' <> ": " <> "#block[") $$
            chomp (vsep defns') $$ "]"

listItemToTypst :: PandocMonad m => Int -> Doc Text -> [Block] -> TW m (Doc Text)
listItemToTypst ind marker blocks = do
  contents <- blocksToTypst blocks
  return $ hang ind (marker <> space) contents

inlinesToTypst :: PandocMonad m => [Inline] -> TW m (Doc Text)
inlinesToTypst ils = hcat <$> mapM inlineToTypst (escapeParens ils)

-- Add an escape before a parenthesis right after a non-space element.
-- Otherwise we risk `#emph[test](3)` which will error. See #11210.
escapeParens :: [Inline] -> [Inline]
escapeParens [] = []
escapeParens (s : x : xs)
  | isSpacey s
    = s : x : escapeParens xs
escapeParens (Str t : xs)
  | Just ('(',_) <- T.uncons t
    = RawInline (Format "typst") "\\" : Str t : escapeParens xs
escapeParens (x : xs) = x : escapeParens xs

isSpacey :: Inline -> Bool
isSpacey Space = True
isSpacey SoftBreak = True
isSpacey LineBreak = True
isSpacey _ = False

inlineToTypst :: PandocMonad m => Inline -> TW m (Doc Text)
inlineToTypst inline =
  case inline of
    Str txt -> do
      opts <-  gets stOptions
      context <- gets stEscapeContext
      return $ escapeTypst (isEnabled Ext_smart opts) context txt
    Space -> return space
    SoftBreak -> do
      wrapText <- gets $ writerWrapText . stOptions
      case wrapText of
        WrapPreserve -> return cr
        WrapAuto     -> return space
        WrapNone     -> return space
    LineBreak -> return (space <> "\\" <> cr)
    Math mathType str -> do
      res <- convertMath TM.writeTypst mathType str
      case res of
          Left il -> inlineToTypst il
          Right r ->
            (case extractLabel str of -- #10805
              Nothing -> id
              Just lab -> (<> (toLabel FreestandingLabel lab))) <$>
             case mathType of
               InlineMath -> return $ "$" <> literal r <> "$"
               DisplayMath -> return $ "$ " <> literal r <> " $"
    Code (ident,cls,kvs) code -> do
      opts <- gets stOptions
      let defaultHighlightedCode =
            case cls of
              (lang:_) | writerHighlightMethod opts /= NoHighlighting
                       -> "#raw(lang:" <> doubleQuoted lang <>
                              ", " <> doubleQuoted code <> ")"
              _ | T.any (=='`') code -> "#raw(" <> doubleQuoted code <> ")"
                | otherwise -> "`" <> literal code <> "`"
      case writerHighlightMethod opts of
        Skylighting _ ->
          case highlight (writerSyntaxMap opts) formatTypstInline
                (ident,cls ++ ["default"],kvs) code of
            Left msg -> do
              unless (T.null msg) $ report $ CouldNotHighlight msg
              return defaultHighlightedCode
            Right h -> do
              modify (\s -> s{ stHighlighting = True })
              return (literal h)
        _ -> return defaultHighlightedCode
    RawInline fmt str ->
      case fmt of
        Format "typst" -> return $ literal str
        _ -> return mempty
    Strikeout inlines -> textstyle "#strike" inlines
    Emph inlines -> textstyle "#emph" inlines
    Underline inlines -> textstyle "#underline" inlines
    Strong inlines -> textstyle "#strong" inlines
    Superscript inlines -> textstyle "#super" inlines
    Subscript inlines -> textstyle "#sub" inlines
    SmallCaps inlines -> textstyle "#smallcaps" inlines
    Span (ident,cls,kvs) inlines -> do
      let lab = toLabel FreestandingLabel ident
      let (_, typstTextAttrs) = pickTypstAttrs kvs
      contents <- inlinesToTypst inlines
      let addHl x = "#highlight" <> brackets x
      return $ (if "mark" `elem` cls
                   then addHl
                   else id)
               (toTypstTextElement typstTextAttrs contents) <> lab
    Quoted quoteType inlines -> do
      opts <- gets stOptions
      let smart = isEnabled Ext_smart opts
      contents <- inlinesToTypst inlines
      return $
        case quoteType of
           DoubleQuote
             | smart -> "\"" <> contents <> "\""
             | otherwise -> "“" <> contents <> "”"
           SingleQuote
             | smart -> "'" <> contents <> "'"
             | otherwise -> "‘" <> contents <> "’"
    Cite citations inlines -> do
      opts <-  gets stOptions
      if isEnabled Ext_citations opts
         -- Note: this loses prefix
         then mconcat <$> mapM toCite citations
         else inlinesToTypst inlines
    Link (_,_,kvs) inlines (src,_tit) -> do
      case lookup "reference-type" kvs of
        Just "ref"
          | Just ('#', ident) <- T.uncons src
          -> if T.all isIdentChar ident
                then pure $ literal $ "@" <> ident
                else pure $ "#ref" <> parens (toLabel ArgumentLabel ident)
        _ -> do
          contents <- inlinesToTypst inlines
          let dest = case T.uncons src of
                       Just ('#', ident) -> toLabel ArgumentLabel ident
                       _ -> doubleQuoted src
          pure $ "#link" <> parens dest <>
                    (if inlines == [Str src]
                          then mempty
                          else nowrap $ brackets contents)
    Image attr _inlines (src,_tit) -> do
      opts <-  gets stOptions
      pure $ mkImage opts True src attr
    Note blocks -> do
      contents <- blocksToTypst blocks
      return $ "#footnote" <> brackets (chomp contents)

-- see #9104; need box or image is treated as block-level
mkImage :: WriterOptions -> Bool -> Text -> Attr -> Doc Text
mkImage opts useBox src attr
  | useBox = "#box" <> parens coreImage
  | otherwise = coreImage
 where
  src' = T.pack $ unEscapeString $ T.unpack src -- #9389
  showDim (Pixel a) = literal (showInInch opts (Pixel a) <> "in")
  showDim dim = text (show dim)
  dimAttrs =
     (case dimension Height attr of
        Nothing -> mempty
        Just dim -> ", height: " <> showDim dim) <>
     (case dimension Width attr of
        Nothing -> mempty
        Just dim -> ", width: " <> showDim dim)
  isData = "data:" `T.isPrefixOf` src'
  dataSvg = "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><image xlink:href=\"" <> src' <> "\" /></svg>"
  coreImage
    | isData = "image.decode" <> parens(doubleQuoted dataSvg <> dimAttrs)
    | otherwise = "image" <> parens (doubleQuoted src' <> dimAttrs)

textstyle :: PandocMonad m => Doc Text -> [Inline] -> TW m (Doc Text)
textstyle s inlines = do
  (s <>) . brackets . fixInitialAfterBreakEscape
    <$> inlinesToTypst inlines

fixInitialAfterBreakEscape :: Doc Text -> Doc Text
fixInitialAfterBreakEscape (Concat x y) =
  Concat (fixInitialAfterBreakEscape x) y
-- make an initial AfterBreak escape unconditional (it will be rendered
-- in a block [..] and there won't be an actual break to trigger it, but
-- typst still needs the escape)
fixInitialAfterBreakEscape (AfterBreak "\\") = Text 1 "\\"
fixInitialAfterBreakEscape x = x

isOrderedListMarker :: Text -> Bool
isOrderedListMarker t = not (T.null ds) && rest == "."
  where (ds, rest) = T.span isDigit t

escapeTypst :: Bool -> EscapeContext -> Text -> Doc Text
escapeTypst smart context t =
  (case T.uncons t of
    Just (c, _)
      | c == ';' -> char '\\' -- see #9252
      | needsEscapeAtLineStart c || isOrderedListMarker t
        -> afterBreak "\\"
    _ -> mempty) <>
  (literal (T.replace "//" "\\/\\/"
    (if T.any needsEscape t
        then T.concatMap escapeChar t
        else t)))
  where
    escapeChar c
      | c == '\160' = "~"
      | c == '\8217', smart = "'" -- apostrophe
      | c == '\8212', smart = "---" -- em dash
      | c == '\8211', smart = "--" -- en dash
      | needsEscape c = "\\" <> T.singleton c
      | otherwise = T.singleton c
    needsEscape '\160' = True
    needsEscape '\8217' = smart
    needsEscape '\8212' = smart
    needsEscape '\8211' = smart
    needsEscape '\'' = smart
    needsEscape '"' = smart
    needsEscape '[' = True
    needsEscape ']' = True
    needsEscape '#' = True
    needsEscape '<' = True
    needsEscape '>' = True
    needsEscape '@' = True
    needsEscape '$' = True
    needsEscape '\\' = True
    needsEscape '`' = True
    needsEscape '_' = True
    needsEscape '*' = True
    needsEscape '~' = True
    needsEscape ':' = context == TermContext
    needsEscape _ = False

needsEscapeAtLineStart :: Char -> Bool
needsEscapeAtLineStart '/' = True
needsEscapeAtLineStart '+' = True
needsEscapeAtLineStart '-' = True
needsEscapeAtLineStart '=' = True
needsEscapeAtLineStart _ = False

data LabelType =
    FreestandingLabel
  | ArgumentLabel
  deriving (Show, Eq)

toLabel :: LabelType -> Text -> Doc Text
toLabel labelType ident
  | T.null ident = mempty
  | T.all isIdentChar ident'
    = "<" <> literal ident' <> ">"
  | otherwise
     = case labelType of
          FreestandingLabel -> "#label" <> parens (doubleQuoted ident')
          ArgumentLabel -> "label" <> parens (doubleQuoted ident')
 where
   ident' = T.pack $ unEscapeString $ T.unpack ident

isIdentChar :: Char -> Bool
isIdentChar c = isXIDContinue c || c == '_' || c == '-' || c == '.' || c == ':'

toCite :: PandocMonad m => Citation -> TW m (Doc Text)
toCite cite = do
  let ident' = T.pack $ unEscapeString $ T.unpack $ citationId cite
  -- typst inserts comma and we get a doubled one if supplement contains it:
  let eatComma (Str "," : Space : xs) = xs
      eatComma xs = xs
  if citationMode cite == NormalCitation && T.all isIdentChar ident'
     then do
       suppl <- case citationSuffix cite of
                  [] -> pure mempty
                  suff -> brackets <$> inlinesToTypst (eatComma suff)
       pure $ "@" <> literal ident' <> suppl
     else do
       let label = if T.all isIdentChar ident'
                      then "<" <> literal ident' <> ">"
                      else "label" <> parens (doubleQuoted ident')
       let form = case citationMode cite of
                     NormalCitation -> mempty
                     SuppressAuthor -> ", form: \"year\""
                     AuthorInText -> ", form: \"prose\""
       suppl <- case citationSuffix cite of
                  [] -> pure mempty
                  suff -> (", supplement: " <>) . brackets
                             <$> inlinesToTypst (eatComma suff)
       pure $ (if citationMode cite == SuppressAuthor  -- see #11044
                  then parens
                  else id)
            $ "#cite" <> parens (label <> form <> suppl)

doubleQuoted :: Text -> Doc Text
doubleQuoted = doubleQuotes . literal . escape
 where
  escape = T.concatMap escapeChar
  escapeChar '\\' = "\\\\"
  escapeChar '"' = "\\\""
  escapeChar c = T.singleton c

extractLabel :: Text -> Maybe Text
extractLabel = go . T.unpack
 where
   go [] = Nothing
   go ('\\':'l':'a':'b':'e':'l':'{':xs) = Just (T.pack (takeWhile (/='}') xs))
   go (_:xs) = go xs
