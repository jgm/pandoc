{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
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
import Text.Pandoc.Class.PandocMonad ( PandocMonad )
import Text.Pandoc.Options ( WriterOptions(..), WrapOption(..), isEnabled )
import Data.Text (Text)
import Data.List (intercalate, intersperse)
import qualified Data.Text as T
import Control.Monad.State ( StateT, evalStateT, gets, modify )
import Text.Pandoc.Writers.Shared ( metaToContext, defField, resetField,
                                    toLegacyTable, lookupMetaString )
import Text.Pandoc.Shared (isTightList, orderedListMarkers)
import Text.Pandoc.Writers.Math (convertMath)
import qualified Text.TeXMath as TM
import Text.DocLayout
import Text.DocTemplates (renderTemplate)
import Text.Pandoc.Extensions (Extension(..))
import Text.Collate.Lang (Lang(..), parseLang)

-- | Convert Pandoc to Typst.
writeTypst :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeTypst options document =
  evalStateT (pandocToTypst options document)
    WriterState{ stOptions = options,
                 stEscapeContext = NormalContext,
                 stNotes = [] }

data EscapeContext = NormalContext | TermContext
  deriving (Show, Eq)

data WriterState =
  WriterState {
    stOptions :: WriterOptions,
    stEscapeContext :: EscapeContext,
    stNotes   :: [Doc Text]
  }

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
  noteContents <- reverse <$> gets stNotes
  let notes = vsep $ zipWith
                      (\(num :: Int) cont ->
                        "#endnote" <> parens (brackets (text (show num))
                                   <> ", " <>  brackets (chomp cont <> cr)))
                      [1..] noteContents
  let context = defField "body" main
              $ defField "notes" notes
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
    Header level (ident,_,_) inlines -> do
      contents <- inlinesToTypst inlines
      let lab = toLabel ident
      return $ literal (T.replicate level "=") <> space <> contents <> cr <> lab
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
      return $ "#blockquote[" $$ chomp contents $$ "]" $$ blankline
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
                    return $ "#align(center, " <> brackets captcontents <> ")"
      let lab = toLabel ident
      let formatalign AlignLeft = "left,"
          formatalign AlignRight = "right,"
          formatalign AlignCenter = "center,"
          formatalign AlignDefault = "auto,"
      let alignarray = parens $ mconcat $ map formatalign aligns
      return $ "#align(center)[#table("
        $$ nest 2
           (  "columns: " <> text (show numcols) <> "," -- auto
           $$ "align: (col, row) => " <> alignarray <> ".at(col),"
           $$ "inset: 6pt" <> ","
           $$ hsep (map ((<>",") . brackets) headers')
           $$ vcat (map (\x -> brackets x <> ",") (concat rows'))
           )
        $$ ")"
        $$ capt'
        $$ lab
        $$ "]"
        $$ blankline
    Figure (ident,_,_) (Caption _mbshort capt) blocks -> do
      caption <- blocksToTypst capt
      contents <- blocksToTypst blocks
      let lab = toLabel ident
      return $ "#figure(" <> nest 2 (brackets contents <> "," <> cr <>
                                     ("caption: [" $$ nest 2 caption $$ "]"))
                          $$ ")" $$ lab $$ blankline
    Div (ident,_,_) blocks -> do
      let lab = toLabel ident
      contents <- blocksToTypst blocks
      return $ lab $$ contents

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
inlinesToTypst ils = hcat <$> mapM inlineToTypst ils

inlineToTypst :: PandocMonad m => Inline -> TW m (Doc Text)
inlineToTypst inline =
  case inline of
    Str txt -> do
      context <- gets stEscapeContext
      return $ literal $ escapeTypst context txt
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
                        ", " <> doubleQuoted code <> ")"
        _ | T.any (=='`') code -> "#raw(" <> doubleQuoted code <> ")"
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
      let lab = toLabel ident
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
         then return $ -- Note: this loses locators, prefix, suffix
              "#cite" <> parens
                (mconcat $ intersperse ", " $
                  map (doubleQuoted . citationId) citations)
         else inlinesToTypst inlines
    Link _attrs inlines (src,_tit) -> do
      contents <- inlinesToTypst inlines
      let dest = case T.uncons src of
                   Just ('#', ident) -> "<" <> literal ident <> ">"
                   _ -> doubleQuoted src
      return $ "#link" <> parens dest <>
                if render Nothing contents == src
                   then mempty
                   else nowrap $ brackets contents
    Image (_,_,kvs) _inlines (src,_tit) -> do
      let width' = maybe mempty ((", width: " <>) . literal) $ lookup "width" kvs
      let height' = maybe mempty ((", height: " <>) . literal) $
                    lookup "height" kvs
      return $ "#image(" <> doubleQuoted src <> width' <> height' <> ")"
    Note blocks -> do -- currently typst has no footnotes!
      -- TODO create endnotes with manual typesetting
      contents <- blocksToTypst blocks
      modify $ \st -> st{ stNotes = contents : stNotes st }
      num <- text . show . length <$> gets stNotes
      return $ "#super" <> brackets num

textstyle :: PandocMonad m => Doc Text -> [Inline] -> TW m (Doc Text)
textstyle s inlines = (s <>) . brackets <$> inlinesToTypst inlines

escapeTypst :: EscapeContext -> Text -> Text
escapeTypst context t =
  if T.any needsEscape t
     then T.concatMap escapeChar t
     else t
  where
    escapeChar c
      | needsEscape c = "\\" <> T.singleton c
      | otherwise = T.singleton c
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
    needsEscape '=' = True
    needsEscape '_' = True
    needsEscape '*' = True
    needsEscape ':' = context == TermContext
    needsEscape _ = False

toLabel :: Text -> Doc Text
toLabel ident =
  if T.null ident
     then mempty
     else "<" <> literal ident <> ">"

doubleQuoted :: Text -> Doc Text
doubleQuoted = doubleQuotes . literal . escape
 where
  escape = T.concatMap escapeChar
  escapeChar '\\' = "\\\\"
  escapeChar '"' = "\\\""
  escapeChar c = T.singleton c
