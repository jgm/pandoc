{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
   Module      : Text.Pandoc.Readers.Typst
   Copyright   : Copyright (C) 2023 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Reads and evaluates a Typst document as a Pandoc AST.
-}
module Text.Pandoc.Readers.Typst
  ( readTypst
  )
where

import Text.Pandoc.Class
import Text.Pandoc.Sources
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Typst ( parseTypst, evaluateTypst )
import Text.Pandoc.Error (PandocError(..))
import Control.Monad.Except (throwError)
import Control.Monad (MonadPlus (mplus), void)
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Walk
import Text.Parsec
import Text.TeXMath (writeTeX)
import Text.TeXMath.Shared (getSpaceChars)
import Text.Pandoc.Readers.Typst.Math (pMathMany)
import Text.Pandoc.Readers.Typst.Parsing (pTok, ignored, chunks, getField, P)
import Typst.Methods (applyPureFunction, formatNumber)
import Typst.Types


-- | Read Typst from an input string and return a Pandoc document.
readTypst :: (PandocMonad m, ToSources a)
           => ReaderOptions -> a -> m Pandoc
readTypst _opts inp = do
  let sources = toSources inp
  let inputName = case sources of
        Sources ((pos, _):_) -> sourceName pos
        _ -> ""
  case parseTypst inputName (sourcesToText sources) of
    Left e -> throwError $ PandocParseError $ T.pack $ show e
    Right parsed ->
      evaluateTypst readFileStrict getCurrentTime inputName parsed >>=
                  either (throwError . PandocParseError . T.pack . show) pure >>=
                  runParserT pPandoc () inputName . F.toList >>=
                  either (throwError . PandocParseError . T.pack . show) pure

pPandoc :: PandocMonad m => P m B.Pandoc
pPandoc = B.doc <$> pBlocks

pBlocks :: PandocMonad m => P m B.Blocks
pBlocks = mconcat <$> many pBlock

pBlock :: PandocMonad m => P m B.Blocks
pBlock = pPara <|> pBlockElt

pBlockElt :: PandocMonad m => P m B.Blocks
pBlockElt = pTok isBlock >>= handleBlock

pSpace :: PandocMonad m => P m Content
pSpace = pTok
      ( \case
          Txt t | T.all (== ' ') t -> True
          _ -> False
      )


pLab :: PandocMonad m => P m Text
pLab = try $ do
  optional pSpace
  Lab t <- pTok
       ( \case
           Lab _ -> True
           _ -> False
       )
  pure t

handleBlock :: PandocMonad m => Content -> P m B.Blocks
handleBlock tok = do
  -- check for following label
  mbident <- option Nothing $ Just <$> pLab
  case tok of
    Txt {} -> fail "pBlockElt encountered Txt"
    Lab {} -> pure mempty
    Elt "heading" _ fields -> do
      body <- getField "body" fields
      lev <- getField "level" fields <|> pure 1
      B.headerWith (fromMaybe "" mbident,[],[]) lev <$> pWithContents pInlines body
    Elt "list" _ fields -> do
      children <- V.toList <$> getField "children" fields
      B.bulletList <$> mapM (pWithContents pBlocks) children
    Elt "list.item" _ fields ->
      getField "body" fields >>= pWithContents pBlocks
    Elt "enum" _ fields -> do
      children <- V.toList <$> getField "children" fields
      mbstart <- getField "start" fields
      start <- case mbstart of
        Nothing -> pure 1
        Just x
          | x >= 0 -> pure x
          | otherwise -> fail "number must be positive"
      (numbering :: Text) <- getField "numbering" fields `mplus` pure ""
      let (sty, delim) =
            case numbering of
              "1." -> (B.Decimal, B.Period)
              "1)" -> (B.Decimal, B.OneParen)
              "(1)" -> (B.Decimal, B.TwoParens)
              "a." -> (B.LowerAlpha, B.Period)
              "a)" -> (B.LowerAlpha, B.OneParen)
              "(a)" -> (B.LowerAlpha, B.TwoParens)
              "A." -> (B.UpperAlpha, B.Period)
              "A)" -> (B.UpperAlpha, B.OneParen)
              "(A)" -> (B.UpperAlpha, B.TwoParens)
              "i." -> (B.LowerRoman, B.Period)
              "i)" -> (B.LowerRoman, B.OneParen)
              "(i)" -> (B.LowerRoman, B.TwoParens)
              "I." -> (B.UpperRoman, B.Period)
              "I)" -> (B.UpperRoman, B.OneParen)
              "(I)" -> (B.UpperRoman, B.TwoParens)
              _ -> (B.DefaultStyle, B.DefaultDelim)
      let listAttr = (start, sty, delim)
      B.orderedListWith listAttr <$> mapM (pWithContents pBlocks) children
    Elt "enum.item" _ fields ->
      getField "body" fields >>= pWithContents pBlocks
    Elt "terms" _ fields -> do
      children <- V.toList <$> getField "children" fields
      B.definitionList
        <$> mapM
          ( \case
              VTermItem t d -> do
                t' <- pWithContents pInlines t
                d' <- pWithContents pBlocks d
                pure (t', [d'])
              _ -> pure (mempty, [])
          )
          children
    Elt "terms.item" _ fields ->
      getField "body" fields >>= pWithContents pBlocks
    Elt "raw" _ fields -> do
      txt <- getField "text" fields
      mblang <- getField "lang" fields
      let attr = (fromMaybe "" mbident, maybe [] (\l -> [l]) mblang, [])
      pure $ B.codeBlockWith attr txt
    Elt "parbreak" _ _ -> pure mempty
    Elt "block" _ fields ->
      B.divWith (fromMaybe "" mbident, [], [])
        <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "place" pos fields -> do
      ignored "parameters of place"
      handleBlock (Elt "block" pos fields)
    Elt "columns" _ fields -> do
      (cnt :: Integer) <- getField "count" fields
      B.divWith ("", ["columns-flow"], [("count", T.pack (show cnt))])
        <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "rect" _ fields ->
      B.divWith ("", ["rect"], []) <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "circle" _ fields ->
      B.divWith ("", ["circle"], []) <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "ellipse" _ fields ->
      B.divWith ("", ["ellipse"], []) <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "polygon" _ fields ->
      B.divWith ("", ["polygon"], []) <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "square" _ fields ->
      B.divWith ("", ["square"], []) <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "align" _ fields -> do
      alignment <- getField "alignment" fields
      B.divWith ("", [], [("align", repr alignment)])
        <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "stack" _ fields -> do
      (dir :: Direction) <- getField "dir" fields `mplus` pure Ltr
      rawchildren <- getField "children" fields
      children <-
        mapM
          ( \case
              val@(VFraction {}) ->
                pure $ B.divWith ("", [], [("space", repr val)]) mempty
              val -> fromVal val >>= pWithContents pBlocks
          )
          (V.toList rawchildren)
      pure $
        B.divWith ("", [], [("stack", repr (VDirection dir))]) $
          mconcat $
            map (B.divWith ("", [], [])) children
    Elt "grid" _ fields -> do
      children <- getField "children" fields >>= mapM (pWithContents pBlocks) . V.toList
      (columns :: Val) <- getField "columns" fields
      let toWidth (VFraction f) = Just (floor $ 1000 * f)
          toWidth _ = Nothing
      let normalizeWidths xs =
            let givenwidths = catMaybes xs
                (totgivenwidth :: Int) = sum givenwidths
                avgwidth = totgivenwidth `div` length givenwidths
                totwidth = avgwidth * length xs
             in if null givenwidths
                  then replicate (length xs) B.ColWidthDefault
                  else
                    map
                      ( \case
                          Just x -> B.ColWidth (fromIntegral x / fromIntegral totwidth)
                          Nothing ->
                            B.ColWidth (fromIntegral avgwidth / fromIntegral totwidth)
                      )
                      xs
      widths <- case columns of
        VInteger x -> pure $ replicate (fromIntegral x) B.ColWidthDefault
        VArray x -> pure $ normalizeWidths $ map toWidth (V.toList x)
        VNone -> pure [B.ColWidthDefault]
        _ -> fail $ "Could not determine number of columns: " <> show columns
      let numcols = length widths
      align <- getField "align" fields
      let toAlign (VAlignment (Just horiz) _) =
            case horiz of
              HorizStart -> B.AlignLeft
              HorizLeft -> B.AlignLeft
              HorizEnd -> B.AlignRight
              HorizRight -> B.AlignRight
              HorizCenter -> B.AlignCenter
          toAlign _ = B.AlignDefault
      aligns <-
        case align of
          VAlignment {} -> pure $ replicate numcols (toAlign align)
          VArray v -> pure $ map toAlign (V.toList v)
          VFunction _ _ f -> do
            mapM
              ( \colnum -> case applyPureFunction
                  f
                  [VInteger colnum, VInteger 0] of
                  Success x -> pure $ toAlign x
                  Failure e -> fail e
              )
              [0 .. (fromIntegral numcols - 1)]
          _ -> pure $ replicate numcols B.AlignDefault
      let colspecs = zip (aligns ++ repeat B.AlignDefault) widths
      let rows =
            map (B.Row B.nullAttr) $
              chunks numcols $
                map
                  ( B.Cell
                      B.nullAttr
                      B.AlignDefault
                      (B.RowSpan 1)
                      (B.ColSpan 1)
                      . B.toList
                  )
                  children
      pure $
        B.tableWith
          (fromMaybe "" mbident, [], [])
          (B.Caption mempty mempty)
          colspecs
          (B.TableHead B.nullAttr [])
          [B.TableBody B.nullAttr 0 [] rows]
          (B.TableFoot B.nullAttr [])
    Elt "table" pos fields -> handleBlock (Elt "grid" pos fields)
    Elt "figure" _ fields -> do
      body <- getField "body" fields >>= pWithContents pBlocks
      (mbCaption :: Maybe (Seq Content)) <- getField "caption" fields
      (caption :: B.Blocks) <- maybe mempty (pWithContents pBlocks) mbCaption
      pure $ case B.toList body of
        [B.Table attr _ colspecs thead tbodies tfoot] ->
          B.singleton
            (B.Table attr (B.Caption Nothing (B.toList caption)) colspecs thead tbodies tfoot)
        _ -> B.figureWith (fromMaybe "" mbident, [], [])
                          (B.Caption Nothing (B.toList caption)) body
    Elt "line" _ fields
      | isNothing
          ( M.lookup "start" fields
              >> M.lookup "end" fields
              >> M.lookup "angle" fields
          ) -> do
          pure $ B.horizontalRule
    Elt "numbering" _ fields -> do
      numStyle <- getField "numbering" fields
      (nums :: V.Vector Integer) <- getField "numbers" fields
      let toText v = fromMaybe "" $ fromVal v
      let toNum n =
            case numStyle of
              VString t -> formatNumber t (fromIntegral n)
              VFunction _ _ f ->
                case applyPureFunction f [VInteger n] of
                  Success x -> toText x
                  Failure _ -> "?"
              _ -> "?"
      pure $ B.plain . B.text . mconcat . map toNum $ V.toList nums
    Elt "footnote.entry" _ fields ->
      getField "body" fields >>= pWithContents pBlocks
    Elt (Identifier tname) _ _ -> do
      ignored ("unknown block element " <> tname)
      pure mempty

pPara :: PandocMonad m => P m B.Blocks
pPara =
  B.para . B.trimInlines . mconcat <$> (many1 pInline <* optional pParBreak)

pParBreak :: PandocMonad m => P m ()
pParBreak =
  void $
    pTok
      ( \case
          Elt "parbreak" _ _ -> True
          _ -> False
      )

isInline :: Content -> Bool
isInline (Lab {}) = True
isInline (Txt {}) = True
isInline x = not (isBlock x)

isBlock :: Content -> Bool
isBlock (Txt {}) = False
isBlock (Lab {}) = True
isBlock (Elt name _ fields) =
  case name of
    "align" -> True
    "bibliography" -> True
    "block" -> True
    "circle" -> True
    "colbreak" -> True
    "columns" -> True
    "csv" -> True
    "ellipse" -> True
    "enum" -> True
    "enum.item" -> True
    "figure" -> True
    "grid" -> True
    "heading" -> True
    "json" -> True
    "line" -> True
    "list" -> True
    "list.item" -> True
    "numbering" -> True
    "footnote.entry" -> True
    "outline" -> True
    "page" -> True
    "pagebreak" -> True
    "par" -> True
    "parbreak" -> True
    "place" -> True
    "polygon" -> True
    "raw" -> M.lookup "block" fields == Just (VBoolean True)
    "read" -> True
    "rect" -> True
    "square" -> True
    "stack" -> True
    "table" -> True
    "terms" -> True
    "terms.item" -> True
    "toml" -> True
    "v" -> True
    "xml" -> True
    "yaml" -> True
    _ -> False

pWithContents :: PandocMonad m => P m a -> Seq Content -> P m a
pWithContents pa cs = do
  inp <- getInput
  setInput $ F.toList cs
  res <- pa
  setInput inp
  pure res

pInlines :: PandocMonad m => P m B.Inlines
pInlines = mconcat <$> many pInline

pInline :: PandocMonad m => P m B.Inlines
pInline = pTok isInline >>= handleInline

handleInline :: PandocMonad m => Content -> P m B.Inlines
handleInline tok =
  case tok of
    Txt t -> pure $ B.text t
    Lab name -> pure $ B.spanWith (name, [], []) mempty
    Elt "ref" _ fields -> do
      VLabel target <- getField "target" fields
      supplement' <- getField "supplement" fields
      supplement <- case supplement' of
                      VAuto -> -- TODO for now, until we can locate the element
                        pure $ B.text ("[" <> target <> "]")
                      VContent cs -> pWithContents pInlines cs
                      VFunction _ _ _f -> -- TODO for now, until we can locate the element
                           pure $ B.text ("[" <> target <> "]")
                      _ -> pure mempty
      pure $ B.linkWith ("", ["ref"], []) ("#" <> target) "" supplement
    Elt "linebreak" _ _ -> pure B.linebreak
    Elt "text" _ fields -> do
      body <- getField "body" fields
      (mbweight :: Maybe Text) <- getField "weight" fields
      case mbweight of
        Just "bold" -> B.strong <$> pWithContents pInlines body
        _ -> pWithContents pInlines body
    Elt "raw" _ fields -> B.code <$> getField "text" fields
    Elt "footnote" _ fields ->
      B.note <$> (getField "body" fields >>= pWithContents pBlocks)
    Elt "cite" _ fields -> do
      keys <- V.toList <$> getField "keys" fields
      let toCitation key =
            B.Citation
              { B.citationId = key,
                B.citationPrefix = mempty,
                B.citationSuffix = mempty,
                B.citationMode = B.NormalCitation,
                B.citationNoteNum = 0,
                B.citationHash = 0
              }
      let citations = map toCitation keys
      pure $ B.cite citations (B.text $ "[" <> T.intercalate "," keys <> "]")
    Elt "lower" _ fields -> do
      body <- getField "text" fields
      walk (modString T.toLower) <$> pWithContents pInlines body
    Elt "upper" _ fields -> do
      body <- getField "text" fields
      walk (modString T.toUpper) <$> pWithContents pInlines body
    Elt "emph" _ fields -> do
      body <- getField "body" fields
      B.emph <$> pWithContents pInlines body
    Elt "strong" _ fields -> do
      body <- getField "body" fields
      B.strong <$> pWithContents pInlines body
    Elt "sub" _ fields -> do
      body <- getField "body" fields
      B.subscript <$> pWithContents pInlines body
    Elt "super" _ fields -> do
      body <- getField "body" fields
      B.superscript <$> pWithContents pInlines body
    Elt "strike" _ fields -> do
      body <- getField "body" fields
      B.strikeout <$> pWithContents pInlines body
    Elt "smallcaps" _ fields -> do
      body <- getField "body" fields
      B.smallcaps <$> pWithContents pInlines body
    Elt "underline" _ fields -> do
      body <- getField "body" fields
      B.underline <$> pWithContents pInlines body
    Elt "link" _ fields -> do
      dest <- getField "dest" fields
      src <- case dest of
        VString t -> pure t
        VLabel t -> pure $ "#" <> t
        VDict _ -> do
          ignored "link to location, linking to #"
          pure "#"
        _ -> fail $ "Expected string or label for dest"
      body <- getField "body" fields
      description <-
        if null body
          then
            pure $
              B.text $
                if "mailto:" `T.isPrefixOf` src
                  then T.drop 7 src
                  else
                    if "tel:" `T.isPrefixOf` src
                      then T.drop 4 src
                      else src
          else pWithContents pInlines body
      pure $ B.link src "" description
    Elt "image" _ fields -> do
      path <- getField "path" fields
      alt <- (B.text <$> getField "alt" fields) `mplus` pure mempty
      (mbwidth :: Maybe Text) <-
        fmap (renderLength False) <$> getField "width" fields
      (mbheight :: Maybe Text) <-
        fmap (renderLength False) <$> getField "height" fields
      let attr =
            ( "",
              [],
              maybe [] (\x -> [("width", x)]) mbwidth
                ++ maybe [] (\x -> [("height", x)]) mbheight
            )
      pure $ B.imageWith attr path "" alt
    Elt "box" _ fields -> do
      body <- getField "body" fields
      B.spanWith ("", ["box"], []) <$> pWithContents pInlines body
    Elt "h" _ fields -> do
      amount <- getField "amount" fields `mplus` pure (LExact 1 LEm)
      let em = case amount of
            LExact x LEm -> toRational x
            _ -> case amount <> LExact 0 LPt of -- force to Pt
              LExact x LPt -> toRational x / 12
              _ -> 1 / 3 -- guess!
      pure $ B.text $ getSpaceChars em
    Elt "style" _ fields -> do
      Function f <- getField "func" fields
      case applyPureFunction (Function f) [VStyles] of
        Success (VContent cs) -> pWithContents pInlines cs
        Success x -> pure $ B.text $ repr x
        Failure e -> fail e
    Elt "math.equation" _ fields -> do
      body <- getField "body" fields
      display <- getField "block" fields
      (if display then B.displayMath else B.math) . writeTeX <$> pMathMany body
    Elt (Identifier tname) _ _
      | "math." `T.isPrefixOf` tname ->
          B.math . writeTeX <$> pMathMany (Seq.singleton tok)
    Elt (Identifier tname) _ _ -> do
      ignored ("unknown inline element " <> tname)
      pure mempty

modString :: (Text -> Text) -> B.Inline -> B.Inline
modString f (B.Str t) = B.Str (f t)
modString _ x = x
