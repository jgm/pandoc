{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
   Module      : Text.Pandoc.Readers.Djot
   Copyright   : Copyright (C) 2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Reads and evaluates a Djot document as a Pandoc AST.
-}
module Text.Pandoc.Readers.Djot
  ( readDjot
  )
where

import Text.Pandoc.Class
import Text.Pandoc.Sources
import Text.Parsec.Pos (newPos)
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Shared (addPandocAttributes, tshow)
import qualified Text.Pandoc.UTF8 as UTF8
import Djot (ParseOptions(..), SourcePosOption(..), parseDoc, Pos(..))
import qualified Djot.AST as D
import Text.Pandoc.Error (PandocError(..))
import Control.Monad.Except (throwError)
import qualified Data.Text as T
import Text.Pandoc.Builder
import Text.Pandoc.Logging
import Text.Pandoc.Emoji (emojiToInline)
import Control.Monad.Reader
import qualified Data.Foldable as F
import Data.List (foldl')
import Data.ByteString (ByteString)
-- import Debug.Trace

-- | Read Djot from an input string and return a Pandoc document.
readDjot :: (PandocMonad m, ToSources a) => ReaderOptions -> a -> m Pandoc
readDjot opts inp = do
  let sources = toSources inp
  case parseDoc ParseOptions{ sourcePositions =
                              if isEnabled Ext_sourcepos opts
                                 then AllSourcePos
                                 else NoSourcePos }
          (UTF8.fromText $ sourcesToText sources) of
    Left e -> throwError $ PandocParseError $ T.pack $ show e
    Right d ->
      runReaderT (doc <$> convertBlocks (D.docBlocks d))
        Env{ references = D.docReferences d <> D.docAutoReferences d
           , footnotes = D.docFootnotes d
           }

data Env =
  Env{ references :: D.ReferenceMap
     , footnotes :: D.NoteMap
     }
  deriving (Show, Ord, Eq)

convertBlocks :: PandocMonad m => D.Blocks -> ReaderT Env m Blocks
convertBlocks =  fmap mconcat . mapM convertBlock . F.toList . D.unMany

convertBlock :: PandocMonad m => D.Node D.Block -> ReaderT Env m Blocks
convertBlock (D.Node pos attr bl) =  addAttrToBlock pos attr <$>
  case bl of
    D.Para ils -> para <$> convertInlines ils
    D.Section bls -> divWith ("",["section"],[]) <$> convertBlocks bls
    D.Heading lev ils -> header lev <$> convertInlines ils
    D.BlockQuote bls -> blockQuote <$> convertBlocks bls
    D.CodeBlock lang bs -> pure $
      codeBlockWith ("", [UTF8.toText lang], []) $ UTF8.toText bs
    D.Div bls -> divWith nullAttr <$> convertBlocks bls
    D.OrderedList olattr listSpacing items ->
      orderedListWith olattr' .
        (case listSpacing of
           D.Tight -> map toTight
           D.Loose -> id) <$> mapM convertBlocks items
     where
       olattr' = ( D.orderedListStart olattr
                 , case D.orderedListStyle olattr of
                     D.Decimal -> Decimal
                     D.LetterUpper -> UpperAlpha
                     D.LetterLower -> LowerAlpha
                     D.RomanUpper -> UpperRoman
                     D.RomanLower -> LowerRoman
                 , case D.orderedListDelim olattr of
                     D.RightPeriod -> Period
                     D.RightParen -> OneParen
                     D.LeftRightParen -> TwoParens
                 )
    D.BulletList listSpacing items ->
        bulletList .
        (case listSpacing of
           D.Tight -> map toTight
           D.Loose -> id) <$> mapM convertBlocks items
    D.TaskList listSpacing items ->
        bulletList .
        (case listSpacing of
           D.Tight -> map toTight
           D.Loose -> id) <$> mapM toTaskListItem items
    D.DefinitionList listSpacing items ->
      definitionList .
        (case listSpacing of
           D.Tight -> map (\(t,d) -> (t, map toTight d))
           D.Loose -> id) <$> mapM toDlItem items
     where
       toDlItem (ils,bls) = (,) <$> convertInlines ils
                                <*> ((:[]) <$> convertBlocks bls)
    D.ThematicBreak -> pure horizontalRule
    D.Table mbCaption rows -> do
      capt <- case mbCaption of
                Just (D.Caption bls') ->
                  Caption Nothing . toList <$> convertBlocks bls'
                Nothing -> pure $ Caption Nothing mempty
      let toAlign D.AlignLeft = AlignLeft
          toAlign D.AlignRight = AlignRight
          toAlign D.AlignCenter = AlignCenter
          toAlign D.AlignDefault = AlignDefault
      let toColSpec (D.Cell _ align _) = (toAlign align, ColWidthDefault)
      let colspecs = case rows of
                      [] -> []
                      (cells:_) -> map toColSpec cells
      let (headrow, rest) =
            case rows of
              (r@(D.Cell D.HeadCell _ _ : _) : rs) -> (r, rs)
              _ -> ([],rows)
      let getBody bods row =
            case row of
              (D.Cell D.HeadCell _ _ : _) ->
                   case bods of
                     [] -> [([row],[])]
                     ([],_):_ -> (([row],[]):bods)
                     (hs,bs):rs -> (hs,row:bs):rs
              _ -> case bods of
                     (hs,bs):rs -> (hs,row:bs):rs
                     [] -> [([],[row])]
      let reverseSnd (as,bs) = (as,reverse bs)
      let bodies = reverse $ map reverseSnd $ foldl' getBody [] rest
      let toCell (D.Cell _ al ils) =
            Cell nullAttr (toAlign al) (RowSpan 1) (ColSpan 1)
                   . (\is -> [Para $ toList is]) <$> convertInlines ils
      let toRow = fmap (Row nullAttr) . mapM toCell
      thead <- TableHead mempty <$> mapM toRow [headrow]
      let toTableBody (hs, rs) =
            TableBody mempty (RowHeadColumns 0) <$>
              mapM toRow hs <*> mapM toRow rs
      tbodies <- mapM toTableBody bodies
      let tfoot = TableFoot mempty []
      pure $ singleton $ Table mempty capt colspecs thead tbodies tfoot
    D.RawBlock (D.Format fmt) bs -> pure $
      rawBlock (UTF8.toText fmt) (UTF8.toText bs)

addAttrToBlock :: Pos -> D.Attr -> Blocks -> Blocks
addAttrToBlock pos (D.Attr as) =
  addPandocAttributes $
    case pos of
        NoPos -> textkvs
        Pos sl sc el ec ->
          ("data-pos", tshow sl <> ":" <> tshow sc <>
                 "-" <> tshow el <> ":" <> tshow ec) : textkvs
     where
       textkvs = (map (\(k,v) -> (UTF8.toText k, UTF8.toText v))
                    (filter (not . internalAttribute) as))

addAttrToInline :: Pos -> D.Attr -> Inlines -> Inlines
addAttrToInline pos (D.Attr as) =
  addPandocAttributes $
    case pos of
        NoPos -> textkvs
        Pos sl sc el ec ->
          ("data-pos", tshow sl <> ":" <> tshow sc <>
                 "-" <> tshow el <> ":" <> tshow ec) : textkvs
     where
       textkvs = (map (\(k,v) -> (UTF8.toText k, UTF8.toText v))
                    (filter (not . internalAttribute) as))

convertInlines :: PandocMonad m => D.Inlines -> ReaderT Env m Inlines
convertInlines = fmap mconcat . mapM convertInline . F.toList . D.unMany

convertInline :: PandocMonad m => D.Node D.Inline -> ReaderT Env m Inlines
convertInline (D.Node pos attr il) = addAttrToInline pos attr <$>
  case il of
    D.Str bs -> pure $ str (UTF8.toText bs)
    D.Emph ils -> emph <$> convertInlines ils
    D.Strong ils -> strong <$> convertInlines ils
    D.Highlight ils -> spanWith ("",["highlighted"],[]) <$> convertInlines ils
    D.Insert ils -> spanWith ("",["inserted"],[]) <$> convertInlines ils
    D.Delete ils -> spanWith ("",["deleted"],[]) <$> convertInlines ils
    D.Subscript ils -> subscript <$> convertInlines ils
    D.Superscript ils -> superscript <$> convertInlines ils
    D.Span ils -> spanWith nullAttr <$> convertInlines ils
    D.Quoted D.DoubleQuotes ils -> doubleQuoted <$> convertInlines ils
    D.Quoted D.SingleQuotes ils -> singleQuoted <$> convertInlines ils
    D.Verbatim bs -> pure $ code (UTF8.toText bs)
    D.Symbol bs -> pure $
      let s = UTF8.toText bs
       in maybe (spanWith ("",["symbol"],[]) (str s)) singleton $ emojiToInline s
    D.Math sty bs -> pure $
      (case sty of
        D.DisplayMath -> displayMath
        D.InlineMath -> math) (UTF8.toText bs)
    D.Link ils target ->
      case target of
        D.Direct url -> link (UTF8.toText url) "" <$> convertInlines ils
        D.Reference label -> do
          refs <- asks references
          case D.lookupReference label refs of
            Just (url, lattr) ->
              addAttrToInline pos lattr .
                link (UTF8.toText url) "" <$> convertInlines ils
            Nothing -> do
              report $ ReferenceNotFound (UTF8.toText label) (newPos "" 0 0)
              link "" "" <$> convertInlines ils
    D.Image ils target ->
      case target of
        D.Direct url -> image (UTF8.toText url) "" <$> convertInlines ils
        D.Reference label -> do
          refs <- asks references
          case D.lookupReference label refs of
            Just (url, lattr) ->
              addAttrToInline pos lattr .
                image (UTF8.toText url) "" <$> convertInlines ils
            Nothing -> do
              report $ ReferenceNotFound (UTF8.toText label) (newPos "" 0 0)
              image "" "" <$> convertInlines ils
    D.FootnoteReference bs -> do
      notes <- asks footnotes
      case D.lookupNote bs notes of
        Just bls -> note <$> convertBlocks bls
        Nothing -> do
          -- TODO consider new warning for this?
          report $ IgnoredElement ("Undefined footnote reference " <> tshow bs)
          pure mempty
    D.UrlLink bs -> do
      let url = UTF8.toText bs
      pure $ linkWith ("",["uri"],[]) url "" (str url)
    D.EmailLink bs -> do
      let email = UTF8.toText bs
      pure $ linkWith ("",["email"],[]) ("mailto:" <> email) "" (str email)
    D.RawInline (D.Format fbs) bs -> pure $
      rawInline (UTF8.toText fbs) (UTF8.toText bs)
    D.NonBreakingSpace -> pure $ str "\160"
    D.SoftBreak -> pure softbreak
    D.HardBreak -> pure linebreak

internalAttribute :: (ByteString, ByteString) -> Bool
internalAttribute ("_implicit",_) = True
internalAttribute ("_autogen",_) = True
internalAttribute _ = False

toTight :: Blocks -> Blocks
toTight (Many bls) = Many $ paraToPlain <$> bls
 where
   paraToPlain (Para ils) = Plain ils
   paraToPlain x = x

toTaskListItem :: PandocMonad m
               => (D.TaskStatus, D.Blocks) -> ReaderT Env m Blocks
toTaskListItem (status, bls) = do
  bls' <- convertBlocks bls
  case toList bls' of
    (Para ils : rest) -> pure $
      fromList $ Para (Str taskmarker : Space : ils) : rest
    _ -> pure $ para (str taskmarker) <> bls'
 where
   taskmarker
     | status == D.Complete = "[X]"
     | otherwise = "[ ]"
