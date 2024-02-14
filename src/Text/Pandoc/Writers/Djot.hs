{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Writers.Djot
   Copyright   : Copyright (C) 2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' format into Djot markup (<https://djot.net>).
-}
module Text.Pandoc.Writers.Djot (
    writeDjot
  ) where
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Class ( PandocMonad , report )
import Text.Pandoc.Options ( WriterOptions(..), WrapOption(..))
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Writers.Shared ( metaToContext, defField, toLegacyTable )
import Text.Pandoc.Shared (isTightList, tshow, stringify, onlySimpleTableCells,
                           makeSections)
import Text.DocLayout
import Text.DocTemplates (renderTemplate)

import Control.Monad.State (StateT(..), gets, modify)
import Control.Monad (zipWithM, when)
import Data.Maybe (fromMaybe)
import qualified Djot.AST as D
import Djot (renderDjot, RenderOptions(..), toIdentifier)
import Text.Pandoc.UTF8 (fromText)

-- | Convert Pandoc to Djot.
writeDjot :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeDjot opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  let ropts = RenderOptions{ preserveSoftBreaks =
                               writerWrapText opts == WrapPreserve }
  metadata <- metaToContext opts
               (fmap (renderDjot ropts) . bodyToDjot opts)
               (fmap (chomp . renderDjot ropts) . bodyToDjot opts .
                  (:[]) . Plain)
               meta
  main <- renderDjot ropts <$>
            bodyToDjot opts (makeSections False Nothing blocks)
  let context  = defField "body" main metadata
  return $ render colwidth $
    case writerTemplate opts of
       Nothing  -> main
       Just tpl -> renderTemplate tpl context

data DjotState =
  DjotState
  { footnotes :: D.NoteMap
  , references :: D.ReferenceMap
  , autoReferences :: D.ReferenceMap
  , autoIds :: Set B.ByteString
  , options :: WriterOptions }

bodyToDjot :: PandocMonad m => WriterOptions -> [Block] -> m D.Doc
bodyToDjot opts bls = do
  (bs, st) <- runStateT (blocksToDjot bls)
               (DjotState mempty mempty mempty mempty opts)
  let D.ReferenceMap autos = autoReferences st
  let D.ReferenceMap refs = references st
  pure $ D.Doc{ D.docBlocks = bs
              , D.docFootnotes = footnotes st
              , D.docReferences = D.ReferenceMap $ M.difference refs autos
              , D.docAutoReferences = D.ReferenceMap autos
              , D.docAutoIdentifiers = autoIds st
              }

blocksToDjot :: PandocMonad m => [Block] -> StateT DjotState m D.Blocks
blocksToDjot = fmap mconcat . mapM blockToDjot

blockToDjot :: PandocMonad m => Block -> StateT DjotState m D.Blocks
blockToDjot (Para ils) = D.para <$> inlinesToDjot ils
blockToDjot (Plain ils) = D.para <$> inlinesToDjot ils
blockToDjot (LineBlock ls) =
  D.para . mconcat . intersperse D.hardBreak <$> mapM inlinesToDjot ls
blockToDjot (CodeBlock attr@(_,_,kvs) t) = do
  let lang = fromMaybe mempty $ lookup "lang" kvs
  pure $ D.addAttr (toDjotAttr attr)
       <$> D.codeBlock (fromText lang) (fromText t)
blockToDjot (RawBlock (Format f) t) =
  pure $ D.rawBlock (D.Format (fromText f)) (fromText t)
blockToDjot (BlockQuote bls) = D.blockQuote <$> blocksToDjot bls
blockToDjot (Header lev attr ils) =
  fmap (D.addAttr (toDjotAttr attr)) . D.heading lev <$> inlinesToDjot ils
blockToDjot HorizontalRule = pure D.thematicBreak
blockToDjot (Div (ident,"section":cls,kvs) bls@(Header _ _ ils : _)) = do
  ilsBs <- D.inlinesToByteString <$> inlinesToDjot ils
  let ident' = toIdentifier ilsBs
  let label = D.normalizeLabel ilsBs
  let autoid = UTF8.toText ident' == ident
  when autoid $
    modify $ \st -> st{ autoIds = Set.insert ident' (autoIds st) }
  modify $ \st -> st{ autoReferences = D.insertReference label
                          (B8.cons '#' ident', mempty) (autoReferences st) }
  fmap
    (D.addAttr (toDjotAttr (if autoid then "" else ident,
                         filter (/= "section") cls,
                         filter (\(k,_) -> k /= "wrapper") kvs))) . D.section
     <$> blocksToDjot bls
blockToDjot (Div attr@(ident,cls,kvs) bls)
  | Just "1" <- lookup "wrapper" kvs
    = fmap (D.addAttr
             (toDjotAttr (ident,cls,filter (\(k,_) -> k /= "wrapper") kvs)))
       <$> blocksToDjot bls
  | otherwise
    = fmap (D.addAttr (toDjotAttr attr)) . D.div <$> blocksToDjot bls
blockToDjot (BulletList items) =
  D.bulletList spacing <$> mapM blocksToDjot items
 where
   spacing = if isTightList items then D.Tight else D.Loose
blockToDjot (OrderedList (start, sty, delim) items) =
  D.orderedList listAttr spacing <$> mapM blocksToDjot items
 where
   spacing = if isTightList items then D.Tight else D.Loose
   listAttr = D.OrderedListAttributes {
                D.orderedListStyle =
                  case sty of
                    DefaultStyle -> D.Decimal
                    Example -> D.Decimal
                    Decimal -> D.Decimal
                    LowerRoman -> D.RomanLower
                    UpperRoman -> D.RomanUpper
                    LowerAlpha -> D.LetterLower
                    UpperAlpha -> D.LetterUpper,
                D.orderedListDelim =
                    case delim of
                      DefaultDelim -> D.RightPeriod
                      Period -> D.RightPeriod
                      OneParen -> D.RightParen
                      TwoParens -> D.LeftRightParen,
                D.orderedListStart = start }
blockToDjot (DefinitionList items) =
  D.definitionList spacing <$> mapM toDLItem items
 where
   spacing = if isTightList (map (concat . snd) items)
                then D.Tight
                else D.Loose
   toDLItem (term, defs) = do
     term' <- inlinesToDjot term
     def' <- mconcat <$> mapM blocksToDjot defs
     pure (term', def')
blockToDjot (Figure attr (Caption _ capt) bls) = do
  content <- blocksToDjot bls
  caption <- fmap (D.addAttr (D.Attr [("class","caption")])) . D.div <$>
               blocksToDjot capt
  pure $ fmap (D.addAttr (toDjotAttr attr)) $ D.div $ content <> caption
blockToDjot (Table attr capt' colspecs thead tbodies tfoot) = do
  let (capt, aligns, _, headRow, bodyRows) =
        toLegacyTable capt' colspecs thead tbodies tfoot
  if onlySimpleTableCells (headRow : bodyRows)
     then do
       let alignToAlign al = case al of
                               AlignDefault -> D.AlignDefault
                               AlignLeft -> D.AlignLeft
                               AlignRight -> D.AlignRight
                               AlignCenter -> D.AlignCenter
       let defAligns = map alignToAlign aligns
       let cellToCell isHeader bls al =
             D.Cell (if isHeader then D.HeadCell else D.BodyCell) al
               <$> case bls of
                     [Para ils] -> inlinesToDjot ils
                     [Plain ils] -> inlinesToDjot ils
                     [] -> pure mempty
                     bs -> do
                       mapM_ (report . BlockNotRendered) bs
                       pure $ D.str "((omitted))"
       let rowToRow isHeader cells = zipWithM (cellToCell isHeader) cells defAligns
       hrows <- if null headRow
                   then pure []
                   else (:[]) <$> rowToRow True headRow
       rows <- mapM (rowToRow False) bodyRows
       caption <- case capt of
                       [] -> pure Nothing
                       _ -> Just . D.Caption . D.para <$> inlinesToDjot capt
       pure $ D.addAttr (toDjotAttr attr) <$> D.table caption (hrows <> rows)
     else do -- table can't be represented as a simple pipe table, use list
       tableList <- D.bulletList D.Loose <$> mapM
                     (fmap (D.bulletList D.Loose) . mapM blocksToDjot)
                      (headRow:bodyRows)
       pure $ D.addAttr (D.Attr [("class", "table")]) <$> tableList

inlinesToDjot :: PandocMonad m => [Inline] -> StateT DjotState m D.Inlines
inlinesToDjot = fmap mconcat . mapM inlineToDjot

inlineToDjot :: PandocMonad m => Inline -> StateT DjotState m D.Inlines
inlineToDjot (Str t) = pure $ D.str (fromText t)
inlineToDjot Space = pure $ D.str " "
inlineToDjot SoftBreak = pure D.softBreak
inlineToDjot LineBreak = pure D.hardBreak
inlineToDjot (Emph ils) = D.emph <$> inlinesToDjot ils
inlineToDjot (Underline ils) =
  fmap (D.addAttr (D.Attr [("class","underline")])) . D.span_
    <$> inlinesToDjot ils
inlineToDjot (Strong ils) = D.strong <$> inlinesToDjot ils
inlineToDjot (Strikeout ils) = D.delete <$> inlinesToDjot ils
inlineToDjot (Subscript ils) = D.subscript <$> inlinesToDjot ils
inlineToDjot (Superscript ils) = D.superscript <$> inlinesToDjot ils
inlineToDjot (Span attr@(ident,cls,kvs) ils)
  | Just "1" <- lookup "wrapper" kvs
    = fmap (D.addAttr
            (toDjotAttr (ident,cls,filter (\(k,_) -> k /= "wrapper") kvs)))
       <$> inlinesToDjot ils
  | otherwise
    = fmap (D.addAttr (toDjotAttr attr)) . D.span_ <$> inlinesToDjot ils
inlineToDjot (SmallCaps ils) =
  fmap (D.addAttr (D.Attr [("class","smallcaps")])) . D.span_
    <$> inlinesToDjot ils
inlineToDjot (Quoted DoubleQuote ils) = D.doubleQuoted <$> inlinesToDjot ils
inlineToDjot (Quoted SingleQuote ils) = D.singleQuoted <$> inlinesToDjot ils
inlineToDjot (Cite _cs ils) = inlinesToDjot ils
inlineToDjot (Code attr t) =
  pure $ D.addAttr (toDjotAttr attr) <$> D.verbatim (fromText t)
inlineToDjot (Math mt t) =
  pure $ (if mt == InlineMath
             then D.inlineMath
             else D.displayMath) (fromText t)
inlineToDjot (RawInline (Format f) t) =
  pure $ D.rawInline (D.Format (fromText f)) (fromText t)
inlineToDjot (Link attr ils (src,tit)) = do
  opts <- gets options
  description <- inlinesToDjot ils
  let ilstring = stringify ils
  let autolink = ilstring == src
  let email = ("mailto:" <> ilstring) == src
  let removeClass name (ident, cls, kvs) = (ident, filter (/= name) cls, kvs)
  let attr' = D.Attr [("title", fromText tit) | not (T.null tit)] <>
               toDjotAttr ( (if autolink
                                then removeClass "uri"
                                else id) .
                            (if email
                                then removeClass "email"
                                else id) $ attr)
  case () of
    _ | autolink -> pure $ D.addAttr attr' <$> D.urlLink (fromText ilstring)
      | email -> pure $ D.addAttr attr' <$> D.emailLink (fromText ilstring)
      | writerReferenceLinks opts
        -> do refs@(D.ReferenceMap m) <- gets references
              autoRefs <- gets autoReferences
              let lab' = D.inlinesToByteString description
              lab <- case D.lookupReference lab' (refs <> autoRefs) of
                       Just _ -> pure lab'
                       Nothing -> do
                         let refnum = M.size m + 1
                         let lab = fromText $ tshow refnum
                         modify $ \st -> st{ references =
                                               D.insertReference lab
                                                 (fromText src, attr') refs }
                         pure lab
              pure $ D.addAttr attr' <$> D.link description (D.Reference lab)
      | otherwise
         -> pure $ D.addAttr attr' <$> D.link description (D.Direct (fromText src))
inlineToDjot (Image attr ils (src,tit)) = do
  opts <- gets options
  description <- inlinesToDjot ils
  let attr' = D.Attr [("title", fromText tit) | not (T.null tit)] <>
                toDjotAttr attr
  if writerReferenceLinks opts
     then do
       refs@(D.ReferenceMap m) <- gets references
       let refnum = M.size m + 1
       let lab = fromText $ tshow refnum
       modify $ \st -> st{ references =
                             D.insertReference lab
                               (fromText src, attr') refs }
       pure $ D.addAttr attr' <$> D.image description (D.Reference lab)
     else pure $ D.addAttr attr' <$> D.image description (D.Direct (fromText src))
inlineToDjot (Note bs) = do
  notes@(D.NoteMap m) <- gets footnotes
  let notenum = M.size m + 1
  let lab = fromText $ tshow notenum
  contents <- blocksToDjot bs
  modify $ \st -> st{ footnotes = D.insertNote lab contents notes }
  pure $ D.footnoteReference lab

toDjotAttr :: (Text, [Text], [(Text, Text)]) -> D.Attr
toDjotAttr (ident, classes, kvs) =
  D.Attr $ [("id", fromText ident) | not (T.null ident)] ++
           [("class", fromText (T.unwords classes)) | not (null classes)] ++
           map (\(k,v) -> (fromText k, fromText v)) kvs
