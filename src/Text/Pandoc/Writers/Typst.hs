{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE BangPatterns        #-}
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
import Text.Pandoc.Class ( PandocMonad, fetchItem )
import Text.Pandoc.ImageSize (imageSize, sizeInPoints)
import Text.Pandoc.Options ( WriterOptions(..), WrapOption(..), isEnabled )
import Data.Text (Text)
import Data.List (intercalate)
import Network.URI (unEscapeString)
import qualified Data.Text as T
import Control.Monad.State ( StateT, evalStateT, gets, modify )
import Text.Pandoc.Writers.Shared ( metaToContext, defField, resetField,
                                    toLegacyTable, lookupMetaString )
import Text.Pandoc.Shared (isTightList, orderedListMarkers, tshow)
import Text.Pandoc.Writers.Math (convertMath)
import qualified Text.TeXMath as TM
import Text.DocLayout
import Text.DocTemplates (renderTemplate)
import Control.Monad.Except (catchError)
import Text.Pandoc.Extensions (Extension(..))
import Text.Collate.Lang (Lang(..), parseLang)
import Data.Char (isAlphaNum)

-- | Convert Pandoc to Typst.
writeTypst :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeTypst options document =
  evalStateT (pandocToTypst options document)
    WriterState{ stOptions = options,
                 stEscapeContext = NormalContext }

data EscapeContext = NormalContext | TermContext
  deriving (Show, Eq)

data WriterState =
  WriterState {
    stOptions :: WriterOptions,
    stEscapeContext :: EscapeContext }

type TW m = StateT WriterState m

pandocToTypst :: PandocMonad m
              => WriterOptions -> Pandoc -> TW m Text
pandocToTypst options (Pandoc meta blocks) = do
  let colwidth = if writerWrapText options == WrapAuto
                    then Just $ writerColumns options
                    else Nothing
  metadata <- metaToContext options
              blocksToTypst
              (fmap chomp . inlinesToTypst)
              meta
  main <- blocksToTypst blocks
  let context = defField "body" main
              $ defField "toc" (writerTableOfContents options)
              $ (if isEnabled Ext_citations options
                    then defField "citations" True
                    else id)
              $ (case lookupMetaString "lang" meta of
                    "" -> id
                    lang ->
                      case parseLang lang of
                        Left _ -> id
                        Right l ->
                          resetField "lang" (langLanguage l) .
                          maybe id (resetField "region") (langRegion l))
              $ defField "toc-depth" (tshow $ writerTOCDepth options)
              $ (if writerNumberSections options
                    then defField "numbering" ("1.1.1.1.1" :: Text)
                    else id)
              $ metadata
  return $ render colwidth $
    case writerTemplate options of
       Nothing  -> main
       Just tpl -> renderTemplate tpl context

blocksToTypst :: PandocMonad m => [Block] -> TW m (Doc Text)
blocksToTypst blocks = vcat <$> mapM blockToTypst blocks

blockToTypst :: PandocMonad m => Block -> TW m (Doc Text)
blockToTypst block =
  case block of
    Plain inlines -> inlinesToTypst inlines
    Para inlines -> ($$ blankline) <$> inlinesToTypst inlines
    Header level (ident,cls,_) inlines -> do
      contents <- inlinesToTypst inlines
      let lab = toLabel FreestandingLabel ident
      return $
        if "unlisted" `elem` cls
           then literal "#heading(outlined: false)" <> brackets contents <>
                 cr <> lab
           else nowrap
                 (literal (T.replicate level "=") <> space <> contents) <>
                 cr <> lab
    RawBlock fmt str ->
      case fmt of
        Format "typst" -> return $ literal str
        _ -> return mempty
    CodeBlock (_,cls,_) code -> do
      let go :: Char -> (Int, Int) -> (Int, Int)
          go '`' (longest, current) =
            let !new = current + 1 in (max longest new, new)
          go _ (longest, _) = (longest, 0)
      let (longestBacktickSequence, _) = T.foldr go (0,0) code
      let fence = literal $ T.replicate (max 3 (longestBacktickSequence + 1)) "`"
      let lang = case cls of
                   (cl:_) -> literal cl
                   _ -> mempty
      return $ fence <> lang <> cr <> literal code <> cr <> fence <> blankline
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
                                      (head (orderedListMarkers
                                             (1, sty, delim))) <>
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
    Table (ident,_,_) blkCapt colspecs thead tbodies tfoot -> do
      let (caption, aligns, _, headers, rows) =
            toLegacyTable blkCapt colspecs thead tbodies tfoot
      let numcols = length aligns
      headers' <- mapM blocksToTypst headers
      rows' <- mapM (mapM blocksToTypst) rows
      capt' <- if null caption
                  then return mempty
                  else do
                    captcontents <- inlinesToTypst caption
                    return $ ", caption: " <> brackets captcontents
      let lab = toLabel FreestandingLabel ident
      let formatalign AlignLeft = "left,"
          formatalign AlignRight = "right,"
          formatalign AlignCenter = "center,"
          formatalign AlignDefault = "auto,"
      let alignarray = parens $ mconcat $ map formatalign aligns
      return $ "#figure(" $$
        "align(center)[#table("
        $$ nest 2
           (  "columns: " <> text (show numcols) <> "," -- auto
           $$ "align: (col, row) => " <> alignarray <> ".at(col),"
           $$ "inset: 6pt" <> ","
           $$ hsep (map ((<>",") . brackets) headers')
           $$ vcat (map (\x -> brackets x <> ",") (concat rows'))
           )
        $$ ")]"
        $$ capt'
        $$ ")"
        $$ lab
        $$ blankline
    Figure (ident,_,_) (Caption _mbshort capt) blocks -> do
      caption <- blocksToTypst capt
      contents <- blocksToTypst blocks
      let lab = toLabel FreestandingLabel ident
      return $ "#figure(" <> nest 2 (brackets contents <> "," <> cr <>
                                     ("caption: [" $$ nest 2 caption $$ "]"))
                          $$ ")" $$ lab $$ blankline
    Div (ident,_,_) (Header lev ("",cls,kvs) ils:rest) ->
      blocksToTypst (Header lev (ident,cls,kvs) ils:rest)
    Div (ident,_,_) blocks -> do
      let lab = toLabel FreestandingLabel ident
      contents <- blocksToTypst blocks
      return $ "#block[" $$ contents $$ ("]" <+> lab)

defListItemToTypst :: PandocMonad m => ([Inline], [[Block]]) -> TW m (Doc Text)
defListItemToTypst (term, defns) = do
  modify $ \st -> st{ stEscapeContext = TermContext }
  term' <- inlinesToTypst term
  modify $ \st -> st{ stEscapeContext = NormalContext }
  defns' <- mapM blocksToTypst defns
  return $ nowrap ("/ " <> term' <> ": " <> "#block[") $$
            chomp (vcat defns') $$ "]"

listItemToTypst :: PandocMonad m => Int -> Doc Text -> [Block] -> TW m (Doc Text)
listItemToTypst ind marker blocks = do
  contents <- blocksToTypst blocks
  return $ hang ind (marker <> space) contents

inlinesToTypst :: PandocMonad m => [Inline] -> TW m (Doc Text)
inlinesToTypst ils@(Str t : _) -- need to escape - in '[-]' #9478
  | Just (c, _) <- T.uncons t
  , needsEscapeAtLineStart c = ("\\" <>) . hcat <$> mapM inlineToTypst ils
inlinesToTypst ils = hcat <$> mapM inlineToTypst ils

inlineToTypst :: PandocMonad m => Inline -> TW m (Doc Text)
inlineToTypst inline =
  case inline of
    Str txt -> do
      context <- gets stEscapeContext
      return $ escapeTypst context txt
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
            case mathType of
              InlineMath -> return $ "$" <> literal r <> "$"
              DisplayMath -> return $ "$ " <> literal r <> " $"
    Code (_,cls,_) code -> return $
      case cls of
        (lang:_) -> "#raw(lang:" <> doubleQuoted lang <>
                        ", " <> doubleQuoted code <> ")" <> endCode
        _ | T.any (=='`') code -> "#raw(" <> doubleQuoted code <> ")"
                                     <> endCode
          | otherwise -> "`" <> literal code <> "`"
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
    Span (ident,_,_) inlines -> do
      let lab = toLabel FreestandingLabel ident
      (lab $$) <$> inlinesToTypst inlines
    Quoted quoteType inlines -> do
      let q = case quoteType of
                   DoubleQuote -> literal "\""
                   SingleQuote -> literal "'"
      contents <- inlinesToTypst inlines
      return $ q <> contents <> q
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
                            <> endCode
        _ -> do
          contents <- inlinesToTypst inlines
          let dest = case T.uncons src of
                       Just ('#', ident) -> toLabel ArgumentLabel ident
                       _ -> doubleQuoted src
          pure $ "#link" <> parens dest <>
                    (if inlines == [Str src]
                          then mempty
                          else nowrap $ brackets contents) <> endCode
    Image (_,_,kvs) _inlines (src,_tit) -> do
      opts <- gets stOptions
      let mbHeight = lookup "height" kvs
      let mdWidth = lookup "width" kvs
      let src' = T.pack $ unEscapeString $ T.unpack src -- #9389
      let coreImage = "image" <> parens (doubleQuoted src')
      -- see #9104; we need a box or the image is treated as block-level:
      case (mdWidth, mbHeight) of
        (Nothing, Nothing) -> do
          realWidth <- catchError
                  (do (bs, _mt) <- fetchItem src
                      case imageSize opts bs of
                        Right x -> pure $ Just $ T.pack $
                                      show (fst (sizeInPoints x)) <> "pt"
                        Left _ -> pure Nothing)
                    (\_ -> pure Nothing)
          case realWidth of
            Just w -> return $ "#box" <>
                        parens ("width: " <> literal w <> ", " <> coreImage)
                        <> endCode
            Nothing -> return $ "#" <> coreImage <> endCode
        (Just w, _) -> return $ "#box" <>
                        parens ("width: " <> literal w <> ", " <> coreImage)
                        <> endCode
        (_, Just h) -> return $ "#box" <>
                        parens ("height: " <> literal h <> ", " <> coreImage)
                        <> endCode
    Note blocks -> do
      contents <- blocksToTypst blocks
      return $ "#footnote" <> brackets (chomp contents) <> endCode

textstyle :: PandocMonad m => Doc Text -> [Inline] -> TW m (Doc Text)
textstyle s inlines =
  (<> endCode) . (s <>) . brackets <$> inlinesToTypst inlines

escapeTypst :: EscapeContext -> Text -> Doc Text
escapeTypst context t =
  (case T.uncons t of
    Just (c, _)
      | needsEscapeAtLineStart c
        -> afterBreak "\\"
    _ -> mempty) <>
  (literal (T.replace "//" "\\/\\/"
    (if T.any needsEscape t
        then T.concatMap escapeChar t
        else t)))
  where
    escapeChar c
      | c == '\160' = "~"
      | needsEscape c = "\\" <> T.singleton c
      | otherwise = T.singleton c
    needsEscape '\160' = True
    needsEscape '[' = True
    needsEscape ']' = True
    needsEscape '#' = True
    needsEscape '<' = True
    needsEscape '>' = True
    needsEscape '@' = True
    needsEscape '$' = True
    needsEscape '\\' = True
    needsEscape '\'' = True
    needsEscape '"' = True
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
isIdentChar c = isAlphaNum c || c == '_' || c == '-' || c == '.' || c == ':'

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
                  suff -> (<> endCode) . brackets
                            <$> inlinesToTypst (eatComma suff)
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
       pure $ "#cite" <> parens (label <> form <> suppl) <> endCode

doubleQuoted :: Text -> Doc Text
doubleQuoted = doubleQuotes . literal . escape
 where
  escape = T.concatMap escapeChar
  escapeChar '\\' = "\\\\"
  escapeChar '"' = "\\\""
  escapeChar c = T.singleton c

endCode :: Doc Text
endCode = beforeNonBlank ";"
