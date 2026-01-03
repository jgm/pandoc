{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
   Module      : Text.Pandoc.Readers.AsciiDoc
   Copyright   : Copyright (C) 2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Reads and evaluates a AsciiDoc document as a Pandoc AST.
-}
module Text.Pandoc.Readers.AsciiDoc
  ( readAsciiDoc
  )
where

import Text.Pandoc.Class
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.Shared (addPandocAttributes, blocksToInlines, safeRead,
                           tshow)
import qualified Text.Pandoc.UTF8 as UTF8
import qualified AsciiDoc as A
import Text.Pandoc.Error
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Readers.HTML (readHtml)
import Control.Monad.Except (throwError)
import Control.Monad
import Text.Pandoc.Parsing (newPos, sourceName)
import Text.Pandoc.Logging
import Text.Pandoc.Sources
import Control.Monad.State
import qualified Data.List as L
import Data.Char (chr, ord)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

-- import Debug.Trace

-- | Read AsciiDoc from an input string and return a Pandoc document.
readAsciiDoc :: (PandocMonad m, ToSources a) => ReaderOptions -> a -> m Pandoc
readAsciiDoc _opts inp = do
  let Sources sources = toSources inp
  (mconcat <$> mapM
   (\(sourcepos, t) ->
     A.parseDocument getIncludeFile raiseError (sourceName sourcepos) t)
    sources)
   >>= resolveFootnotes
   >>= resolveStem
   >>= resolveIcons
   >>= toPandoc
 where
  getIncludeFile fp = UTF8.toText <$> readFileStrict fp
  raiseError fp pos msg = throwError $ PandocParseError $ T.pack
                            $ msg <> " at " <> show fp <>
                              " char " <> show pos

toPandoc :: PandocMonad m => A.Document -> m Pandoc
toPandoc doc =
  Pandoc <$> doMeta (A.docMeta doc)
         <*> (B.toList <$> doBlocks (A.docBlocks doc))

resolveFootnotes :: Monad m => A.Document -> m A.Document
resolveFootnotes doc = do
  evalStateT (A.mapInlines go doc) (mempty :: M.Map T.Text [A.Inline])
 where
   go (A.Inline attr (A.Footnote (Just (A.FootnoteId fnid)) ils)) = do
     fnmap <- get
     case M.lookup fnid fnmap of
       Just ils' ->
         pure $ A.Inline attr (A.Footnote (Just (A.FootnoteId fnid)) ils')
       Nothing -> do
         put $ M.insert fnid ils fnmap
         pure $ A.Inline attr (A.Footnote (Just (A.FootnoteId fnid)) ils)
   go x = pure x

resolveStem :: Monad m => A.Document -> m A.Document
resolveStem doc = do
  let defaultType = case M.lookup "stem" (A.docAttributes (A.docMeta doc)) of
                      Just "asciimath" -> A.AsciiMath
                      _ -> A.LaTeXMath
  let doInlineStem (A.Inline attr (A.Math Nothing t)) =
        pure $ A.Inline attr (A.Math (Just defaultType) t)
      doInlineStem x = pure x
  let doBlockStem (A.Block attr mbtit (A.MathBlock Nothing t)) =
        pure $ A.Block attr mbtit (A.MathBlock (Just defaultType) t)
      doBlockStem x = A.mapInlines doInlineStem x
  A.mapBlocks doBlockStem doc

-- resolve icons as either characters in an icon font or images
resolveIcons :: Monad m => A.Document -> m A.Document
resolveIcons doc = A.mapInlines fromIcon doc
 where
   docattrs = A.docAttributes (A.docMeta doc)
   iconFont = case M.lookup "icons" docattrs of
                Just "font" -> True
                _ -> False
   iconsdir = fromMaybe "./images/icons" $ M.lookup "iconsdir" docattrs
   icontype = fromMaybe "png" $ M.lookup "icontype" docattrs
   addClasses cls (A.Attr ps kvs) =
     A.Attr ps $
      case M.lookup "role" kvs of
       Just r -> M.insert "role" (T.unwords (r : cls)) kvs
       Nothing -> M.insert "role" (T.unwords cls) kvs
   fromIcon (A.Inline attr (A.Icon name)) =
     if iconFont
        then pure $
              A.Inline (addClasses ["fa", "fa-" <> name] attr) (A.Span [])
        else pure $ -- default is to use an image
              A.Inline (addClasses ["icon"] attr)
                 (A.InlineImage
                   (A.Target
                      (iconsdir <> "/" <> name <> "." <> icontype))
                      Nothing Nothing Nothing)
   fromIcon x = pure x

addAttribution :: Maybe A.Attribution -> B.Blocks -> B.Blocks
addAttribution Nothing bs = bs
addAttribution (Just (A.Attribution t)) bs = B.fromList $
  case B.toList bs of
    [B.Div attr bls] -> [B.Div attr (bls ++ [attrBlock])]
    [B.BlockQuote bls] -> [B.BlockQuote (bls ++ [attrBlock])]
    xs -> xs ++ [attrBlock]
 where
   attrBlock = Para (B.toList $ B.text $ "\x2014 " <> t)

doMeta :: PandocMonad m => A.Meta -> m B.Meta
doMeta meta = do
  tit' <- doInlines (A.docTitle meta)
  pure $
    (if tit' == mempty
        then id
        else B.setMeta "title" tit') .
    (case A.docAuthors meta of
       [] -> id
       as -> B.setMeta "author" (map fromAuthor as)) .
    (case A.docRevision meta of
       Nothing -> id
       Just (A.Revision vers mbdate mbremark) ->
         B.setMeta "version" vers .
         maybe id (B.setMeta "date") mbdate .
         maybe id (B.setMeta "remark") mbremark) .
    flip (L.foldl' (\m (k,v) ->
                    -- leave out flags that are set just for processing
                    if k == "sectids" || k == "stem"
                       then m
                       else if T.null v
                               then B.setMeta k True m
                               else B.setMeta k v m))
      (M.toList (A.docAttributes meta))
    $ mempty

fromAuthor :: A.Author -> B.Inlines
fromAuthor au = B.text (A.authorName au) <>
  maybe mempty (\email ->
    " (" <> B.link ("mailto:" <> email) "" (B.str email) <> ")")
    (A.authorEmail au)

doBlocks :: PandocMonad m => [A.Block] -> m B.Blocks
doBlocks = fmap mconcat . mapM doBlock

addBlockAttr :: A.Attr -> B.Blocks -> B.Blocks
addBlockAttr (A.Attr _ kvs') bs =
  case B.toList bs of
    x@(B.OrderedList{}) : xs -> -- "start" is handled in list attribs
      addPandocAttributes (M.toList $ M.delete "start" kvs)
             (B.singleton x) <> B.fromList xs
    x:xs -> addPandocAttributes (M.toList kvs) (B.singleton x)
                <> B.fromList xs
    [] -> mempty
 where
  kvs = M.mapKeys (\k -> if k == "role" then "class" else k) kvs'

addBlockTitle :: B.Inlines -> B.Blocks -> B.Blocks
addBlockTitle tit' bs =
  let tit = B.toList tit'
  in case B.toList bs of
    [B.Table attr _ colspecs thead tbody tfoot] ->
      B.singleton $ B.Table attr (B.Caption Nothing [B.Plain tit])
                     colspecs thead tbody tfoot
    [B.Figure attr _ bs'] ->
      B.singleton $ B.Figure attr (B.Caption Nothing [B.Plain tit]) bs'
    [B.Div attr (B.Div ("",["title"],[]) [Para _] : bs')] ->
      -- replace existing title, which might be e.g. "Note"
      B.singleton $ B.Div attr (B.Div ("",["title"],[]) [B.Para tit] : bs')
    [B.Div attr bs'] -> -- put title Div inside
      B.singleton $ B.Div attr (B.Div ("",["title"],[]) [B.Para tit] : bs')
    _ -> B.divWith B.nullAttr (B.divWith ("",["title"],[]) (B.para tit') <> bs)

doBlock :: PandocMonad m => A.Block -> m B.Blocks
doBlock (A.Block attr@(A.Attr ps kvs) mbtitle bt) = do
  mbtitle' <- case mbtitle of
                Nothing -> pure Nothing
                Just (A.BlockTitle ils) -> Just <$> doInlines ils
  addBlockAttr attr . maybe id addBlockTitle mbtitle' <$>
   case bt of
    A.Section (A.Level lev) ils bs -> do
      ils' <- doInlines ils
      bs' <- doBlocks bs
      pure $ (B.header lev ils') <> bs'
    A.DiscreteHeading (A.Level lev) ils ->
      B.header lev <$> doInlines ils
    A.Paragraph ils -> B.para <$> doInlines ils
    A.LiteralBlock t -> pure $ B.codeBlock t
    A.Listing mblang lns -> do
      let fromCallout (A.Callout i)
            | i <= 20 = T.pack [' ', chr (0x2460 + i - 1)]
            | otherwise = "<" <> tshow i <> ">"
      let fromSourceLine (A.SourceLine t callouts) =
            t <> mconcat (map fromCallout callouts)
      let code = T.intercalate "\n" $ map fromSourceLine lns
      let classes = case mblang of
                      Nothing -> []
                      Just (A.Language l) -> [l]
      pure $ B.codeBlockWith ("", classes, []) code
    A.IncludeListing _ _ Nothing -> pure mempty
    A.IncludeListing mblang _fp (Just lns) ->
      doBlock (A.Block mempty mbtitle (A.Listing mblang lns))
    A.ExampleBlock bs -> B.divWith ("",["example"],[]) <$> doBlocks bs
    A.Sidebar bs -> B.divWith ("",["sidebar"],[]) <$> doBlocks bs
    A.OpenBlock bs -> B.divWith ("",[],[]) <$> doBlocks bs
    A.QuoteBlock mbattrib bs ->
      addAttribution mbattrib . B.blockQuote <$> doBlocks bs
    A.Verse mbattrib bs ->
      addAttribution mbattrib . B.blockQuote <$> doBlocks bs
    -- TODO when texmath's asciimath parser works, convert:
    A.MathBlock (Just A.AsciiMath) t -> pure $ B.para $ B.displayMath t
    A.MathBlock (Just A.LaTeXMath) t -> pure $ B.para $ B.displayMath t
    A.MathBlock Nothing _ ->
      throwError $ PandocParseError "Encountered math type Nothing"
    A.List (A.BulletList _) items ->
      B.bulletList <$> mapM doItem items
    A.List A.CheckList items ->
      B.bulletList <$> mapM doItem items
    A.List (A.OrderedList _ mbstart) items -> do
      let start = fromMaybe (1 :: Int)
                    (mbstart `mplus` (M.lookup "start" kvs >>= safeRead))
      let getStyle xs = case xs of
                  "arabic":_ -> Decimal
                  "decimal":_ -> Decimal
                  "loweralpha":_ -> LowerAlpha
                  "upperalpha":_ -> UpperAlpha
                  "lowerroman":_ -> LowerRoman
                  "upperroman":_ -> UpperRoman
                  _:rest -> getStyle rest
                  [] -> DefaultStyle
      let sty = getStyle ps
      let delim = DefaultDelim
      B.orderedListWith (start, sty, delim) <$> mapM doItem items
    A.List A.CalloutList items ->
      B.divWith ("",["callout-list"],[]) . B.orderedList <$> mapM doItem items
    A.DefinitionList items
      | "ordered" `elem` ps ->
          B.orderedList <$>
             mapM (fmap (B.definitionList . (:[])) . doDefListItem) items
      | otherwise -> B.definitionList <$> mapM doDefListItem items
    A.Table specs mbHeader rows mbFooter -> do
      let toAlign A.AlignLeft = B.AlignLeft
          toAlign A.AlignCenter = B.AlignCenter
          toAlign A.AlignRight = B.AlignRight
      let fromCell (A.TableCell bs mbHorizAlign _mbVertAlign colspan rowspan) =
            B.Cell B.nullAttr (maybe B.AlignDefault toAlign mbHorizAlign)
                        (B.RowSpan rowspan) (B.ColSpan colspan) . B.toList
                   <$> doBlocks bs
      let fromRow (A.TableRow cs) = B.Row B.nullAttr <$> mapM fromCell cs
      tbody <- B.TableBody B.nullAttr (B.RowHeadColumns 0) [] <$> mapM fromRow rows
      thead <- B.TableHead B.nullAttr <$> maybe (pure []) (mapM fromRow) mbHeader
      tfoot <- B.TableFoot B.nullAttr <$> maybe (pure []) (mapM fromRow) mbFooter
      let totalWidth = sum $ map (fromMaybe 1 . A.colWidth) specs
      let toColSpec spec = (maybe B.AlignDefault toAlign (A.colHorizAlign spec),
                             maybe B.ColWidthDefault
                               (B.ColWidth . (\x ->
                                   fromIntegral x / fromIntegral totalWidth))
                               (A.colWidth spec))
      let colspecs = map toColSpec specs
      pure $ B.table (B.Caption Nothing mempty) -- added by addBlockTitle
                colspecs thead [tbody] tfoot
    A.BlockImage target mbalt mbw mbh -> do
      img' <- doInline (A.Inline mempty (A.InlineImage target mbalt mbw mbh))
      -- TODO have a global function that adds the title to caption here:
      pure $ B.figure (Caption Nothing mempty) -- added by addBlockTitle
                      (B.plain img')
    -- TODO alt text?
    A.BlockAudio (A.Target t) ->
      pure $ B.plain $ B.image t "" (B.str t)
    -- TODO alt text?
    A.BlockVideo (A.Target t) ->
      pure $ B.plain $ B.image t "" (B.str t)
    A.TOC -> pure $ B.divWith ("toc",[],[]) mempty
    A.Admonition admonitionType bs -> do
      let admon = T.pack $ show admonitionType
      bs' <- doBlocks bs
      pure $ B.divWith ("",[T.toLower admon],[])
           $ B.divWith ("",["title"],[]) (B.para (B.str admon)) <> bs'
    A.PageBreak ->
      pure $ B.divWith ("", ["page-break"], [("wrapper", "1")]) B.horizontalRule
    A.ThematicBreak -> pure $ B.horizontalRule
    A.Include fp (Just bs) ->
      B.divWith ("",["included"],[("path",T.pack fp)]) <$> doBlocks bs
    A.Include fp Nothing -> do
      report $ CouldNotLoadIncludeFile (T.pack fp) (newPos "" 0 0)
      pure mempty
    A.PassthroughBlock t ->
         case runPure (readHtml def{
                               readerExtensions = extensionsFromList [Ext_raw_html]
                               } t) of
        Left _ -> pure $ B.rawBlock "html" t
        Right (Pandoc _ bs) -> pure $ B.fromList bs

doItem :: PandocMonad m => A.ListItem -> m B.Blocks
doItem (A.ListItem Nothing bs) = doBlocks bs
doItem (A.ListItem (Just checkstate) bs) = do
  bs' <- doBlocks bs
  let check = case checkstate of
                A.Checked -> Str "\9746"
                A.Unchecked -> Str "\9744"
  pure $ B.fromList
       $ case B.toList bs' of
           (B.Para ils : rest) -> B.Para (check : B.Space : ils) : rest
           (B.Plain ils : rest) -> B.Plain (check : B.Space : ils) : rest
           rest -> B.Para [check] : rest

doDefListItem :: PandocMonad m
              => ([A.Inline], [A.Block]) -> m (B.Inlines , [B.Blocks])
doDefListItem (lab, bs) = do
  lab' <- doInlines lab
  bs' <- doBlocks bs
  pure (lab', [bs'])

doInlines :: PandocMonad m => [A.Inline] -> m B.Inlines
doInlines = fmap mconcat . mapM doInline

doInline :: PandocMonad m => A.Inline -> m B.Inlines
doInline (A.Inline (A.Attr _ps kvs') it) = do
  let kvs = M.mapKeys (\k -> if k == "role" then "class" else k) kvs'
  addPandocAttributes (M.toList kvs) <$>
   case it of
    A.Str t -> pure $ B.text t
    A.HardBreak -> pure B.linebreak
    A.Bold ils -> B.strong <$> doInlines ils
    A.Italic ils -> B.emph <$> doInlines ils
    A.Monospace ils -> walk monospaceStr <$> doInlines ils
    A.Superscript ils -> B.superscript <$> doInlines ils
    A.Subscript ils -> B.subscript <$> doInlines ils
    A.Highlight ils -> B.spanWith ("",["mark"],[]) <$> doInlines ils
    A.Strikethrough ils -> B.strikeout <$> doInlines ils
    A.DoubleQuoted ils -> B.doubleQuoted <$> doInlines ils
    A.SingleQuoted ils -> B.singleQuoted <$> doInlines ils
    -- TODO when texmath's asciimath parser works, convert:
    A.Math (Just A.AsciiMath) t -> pure $ B.math t
    A.Math (Just A.LaTeXMath) t -> pure $ B.math t
    A.Math Nothing _ ->
      throwError $ PandocParseError "Encountered math type Nothing"
    A.Icon t -> pure $ B.spanWith ("",["icon"],[("name",t)])
                         (B.str ("[" <> t <> "]"))
    A.Button t -> pure $ B.spanWith ("",["button"],[])
                         (B.strong $ B.str ("[" <> t <> "]"))
    A.Kbd ts -> pure $ mconcat $ L.intersperse (B.str "+") $
         map (B.spanWith ("",["kbd"],[]) . B.strong . B.str) ts
    A.Menu ts -> pure $ B.spanWith ("",["menu"],[]) $
        B.strong $ B.text $ T.intercalate " › " ts
    -- TODO do we need linktype?
    A.Link _linkType (A.Target t) ils -> B.link t "" <$> doInlines ils
    A.InlineImage (A.Target url) mbalt mbwidth mbheight -> do
      let alt = case mbalt of
                  Just (A.AltText t) -> B.text t
                  Nothing -> mempty
          width = case mbwidth of
                  Just (A.Width n) -> [("width", T.pack $ show n <> "px")]
                  Nothing -> []
          height = case mbheight of
                  Just (A.Height n) -> [("height", T.pack $ show n <> "px")]
                  Nothing -> []
      pure $ B.imageWith ("",[], width ++ height) url "" alt
    A.Footnote _ ils -> B.note . B.para <$> doInlines ils
    A.InlineAnchor t _ -> pure $ B.spanWith (t, [], []) mempty
    A.BibliographyAnchor t _ -> pure $ B.spanWith (t, [], []) mempty
    A.CrossReference t Nothing ->
      pure $ B.linkWith ("",["cross-reference"],[]) ("#" <> t) "" (B.str t)
    A.CrossReference t (Just ils) -> do
      B.linkWith ("",["cross-reference"],[]) ("#" <> t) "" <$> doInlines ils
    A.AttributeReference (A.AttributeName t) -> -- if this is here, it's unresolved
      pure $ B.str ("{" <> t <> "}")
    A.Span ils -> B.spanWith B.nullAttr <$> doInlines ils
    A.IndexEntry (A.TermInText t) ->
      pure $ B.spanWith ("",["index"],[("term",t)]) (B.text t)
    A.IndexEntry (A.TermConcealed ts) ->
      pure $ B.spanWith ("",["index"],[("term",T.intercalate "," ts)]) mempty
    A.Counter name ctype val ->
      pure $ B.spanWith ("",["counter"],[("name",name)]) $ B.str $
        case ctype of
          A.DecimalCounter -> tshow val
          A.UpperAlphaCounter -> T.singleton $ chr (ord 'A' + val - 1)
          A.LowerAlphaCounter -> T.singleton $ chr (ord 'a' + val - 1)
    -- Passthrough is hard to get right, because pandoc's RawInline needs
    -- a format specifier. Often in asciidoc passthrough is used as a form
    -- of escaping, so the best approach seems to be treating it as HTML
    -- and parsing it:
    A.Passthrough t -> do
      case runPure (readHtml def{
                               readerExtensions = extensionsFromList [Ext_raw_html]
                               } t) of
        Left _ -> pure $ B.rawInline "html" t
        Right (Pandoc _ bs) -> pure $ B.fromList . blocksToInlines $ bs

monospaceStr :: Inline -> Inline
monospaceStr (Str t) = Code B.nullAttr t
monospaceStr x = x
