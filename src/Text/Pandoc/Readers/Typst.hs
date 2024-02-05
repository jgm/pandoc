{-# LANGUAGE RankNTypes #-}
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
import Text.Pandoc.Shared (tshow, blocksToInlines)
import Control.Monad.Except (throwError)
import Control.Monad (MonadPlus (mplus), void, mzero, guard)
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Walk
import Text.Parsec
import Text.TeXMath (writeTeX)
import Text.TeXMath.Shared (getSpaceChars)
import Text.Pandoc.Readers.Typst.Math (pMathMany)
import Text.Pandoc.Readers.Typst.Parsing (pTok, ignored, chunks, getField, P,
                                          PState(..), defaultPState)
import Typst.Methods (formatNumber, applyPureFunction)
import Typst.Types

-- import Debug.Trace

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
    Right parsed -> do
      let ops = Operations {
                  loadBytes = readFileStrict,
                  currentUTCTime = getCurrentTime,
                  lookupEnvVar = fmap (fmap T.unpack) . lookupEnv . T.pack,
                  checkExistence = fileExists }
      res <- evaluateTypst ops inputName parsed
      case res of
        Left e -> throwError $ PandocParseError $ tshow e
        Right content -> do
          let labs = findLabels [content]
          runParserT pPandoc defaultPState{ sLabels = labs }
            inputName [content] >>=
              either (throwError . PandocParseError . T.pack . show) pure

pBlockElt :: PandocMonad m => P m B.Blocks
pBlockElt = try $ do
  res <- pTok (\t -> isBlock t || not (isInline t))
  -- check for following label
  mbident <- option Nothing $ Just <$> pLab
  case res of
    Elt name@(Identifier tname) pos fields -> do
      case M.lookup name blockHandlers of
        Nothing -> do
          ignored ("unknown block element " <> tname <>
                   " at " <> tshow pos)
          pure mempty
        Just handler -> handler mbident fields
    _ -> pure mempty

pInline :: PandocMonad m => P m B.Inlines
pInline = try $ do
  res <- pTok (\t -> isInline t || not (isBlock t))
  case res of
    Txt t -> pure $ B.text t
    Lab name -> pure $ B.spanWith (name, [], []) mempty
    Elt (Identifier tname) _ _
      | "math." `T.isPrefixOf` tname
      , tname /= "math.equation" ->
          B.math . writeTeX <$> pMathMany (Seq.singleton res)
    Elt name@(Identifier tname) pos fields -> do
      labs <- sLabels <$> getState
      labelTarget <- (do VLabel t <- getField "target" fields
                         True <$ guard (t `elem` labs))
                  <|> pure False
      if tname == "ref" && not labelTarget
         then do
           -- @foo is a citation unless it links to a lab in the doc:
           let targetToKey (Identifier "target") = Identifier "key"
               targetToKey k = k
           case M.lookup "cite" inlineHandlers of
             Nothing -> do
               ignored ("unknown inline element " <> tname <>
                        " at " <> tshow pos)
               pure mempty
             Just handler -> handler Nothing (M.mapKeys targetToKey fields)
         else do
          case M.lookup name inlineHandlers of
            Nothing -> do
              ignored ("unknown inline element " <> tname <>
                       " at " <> tshow pos)
              pure mempty
            Just handler -> handler Nothing fields

pPandoc :: PandocMonad m => P m B.Pandoc
pPandoc = do
  Elt "document" _ fields <- pTok isDocument
  bs <- getField "body" fields >>= pWithContents pBlocks
  pure $ B.doc bs
  -- The following alternative code would add metadata from the
  -- fields on the document element. It is commented out because
  -- the typst metadata doesn't print anything by default, in contrast
  -- to pandoc with its usual templates.  Hence, with this code,
  -- converting a typst document might yield a double title, author, etc.
  --
  -- title <- (getField "title" fields >>= pWithContents pInlines) <|>
  --             pure mempty
  -- authors <- (getField "author" fields >>=
  --                         mapM (pWithContents pInlines) . V.toList) <|>
  --            ((:[]) <$> (getField "author" fields >>=
  --                          (\x -> guard (not (null x)) *>
  --                            pWithContents pInlines x))) <|>
  --             pure []
  -- date <- (getField "date" fields >>= pWithContents pInlines) <|>
  --             pure mempty
  -- keywords <- (getField "keywords" fields >>=
  --                mapM (pWithContents pInlines) . V.toList)
  --               <|> pure []
  -- pure $
  --   (if title == mempty
  --       then id
  --       else B.setMeta "title" title) .
  --   (if null authors
  --       then id
  --       else B.setMeta "author" authors) .
  --   (if null date
  --       then id
  --       else B.setMeta "date" date) .
  --   (if null keywords
  --       then id
  --       else B.setMeta "keywords" keywords) $ B.doc bs

pBlocks :: PandocMonad m => P m B.Blocks
pBlocks = mconcat <$> many pBlock

pBlock :: PandocMonad m => P m B.Blocks
pBlock = pPara <|> pBlockElt

pSpace :: PandocMonad m => P m Content
pSpace = pTok
      ( \case
          Txt t | T.all (== ' ') t -> True
          _ -> False )

pLab :: PandocMonad m => P m Text
pLab = try $ do
  optional pSpace
  Lab t <- pTok
       ( \case
           Lab _ -> True
           _ -> False
       )
  pure t

isDocument :: Content -> Bool
isDocument (Elt "document" _ _) = True
isDocument _ = False

isBlock :: Content -> Bool
isBlock (Elt "raw" _ fields) = M.lookup "block" fields == Just (VBoolean True)
isBlock (Elt name _ _) = name `Set.member` blockKeys
isBlock Lab{} = True
isBlock _ = False

isInline :: Content -> Bool
isInline (Elt "raw" _ fields) = M.lookup "block" fields /= Just (VBoolean True)
isInline (Elt name _ _) = name `Set.member` inlineKeys
isInline Lab{} = True
isInline Txt{} = True

blockKeys :: Set.Set Identifier
blockKeys = Set.fromList $ M.keys
  (blockHandlers :: M.Map Identifier
     (Maybe Text -> M.Map Identifier Val -> P PandocPure B.Blocks))

inlineKeys :: Set.Set Identifier
inlineKeys = Set.fromList $ M.keys
  (inlineHandlers :: M.Map Identifier
     (Maybe Text -> M.Map Identifier Val -> P PandocPure B.Inlines))

blockHandlers :: PandocMonad m =>
                   M.Map Identifier
                   (Maybe Text -> M.Map Identifier Val -> P m B.Blocks)
blockHandlers = M.fromList
  [("text", \_ fields -> do
      body <- getField "body" fields
      -- sometimes text elements include para breaks
      notFollowedBy $ void $ pWithContents pInlines body
      pWithContents pBlocks body)
  ,("box", \_ fields -> do
      body <- getField "body" fields
      B.divWith ("", ["box"], []) <$> pWithContents pBlocks body)
  ,("heading", \mbident fields -> do
      body <- getField "body" fields
      lev <- getField "level" fields <|> pure 1
      B.headerWith (fromMaybe "" mbident,[],[]) lev
         <$> pWithContents pInlines body)
  ,("quote", \_ fields -> do
      getField "block" fields >>= guard
      body <- getField "body" fields >>= pWithContents pBlocks
      attribution <-
        ((\x -> B.para ("\x2104\xa0" <> x)) <$>
          (getField "attribution" fields >>= pWithContents pInlines))
        <|> pure mempty
      pure $ B.blockQuote $ body <> attribution)
  ,("list", \_ fields -> do
      children <- V.toList <$> getField "children" fields
      B.bulletList <$> mapM (pWithContents pBlocks) children)
  ,("list.item", \_ fields -> getField "body" fields >>= pWithContents pBlocks)
  ,("enum", \_ fields -> do
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
      B.orderedListWith listAttr <$> mapM (pWithContents pBlocks) children)
  ,("enum.item", \_ fields -> getField "body" fields >>= pWithContents pBlocks)
  ,("terms", \_ fields -> do
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
          children)
  ,("terms.item", \_ fields -> getField "body" fields >>= pWithContents pBlocks)
  ,("raw", \mbident fields -> do
      txt <- T.filter (/= '\r') <$> getField "text" fields
      mblang <- getField "lang" fields
      let attr = (fromMaybe "" mbident, maybe [] (\l -> [l]) mblang, [])
      pure $ B.codeBlockWith attr txt)
  ,("parbreak", \_ _ -> pure mempty)
  ,("block", \mbident fields ->
      B.divWith (fromMaybe "" mbident, [], [])
        <$> (getField "body" fields >>= pWithContents pBlocks))
  ,("place", \_ fields -> do
      ignored "parameters of place"
      getField "body" fields >>= pWithContents pBlocks)
  ,("columns", \_ fields -> do
      (cnt :: Integer) <- getField "count" fields
      B.divWith ("", ["columns-flow"], [("count", T.pack (show cnt))])
        <$> (getField "body" fields >>= pWithContents pBlocks))
  ,("rect", \_ fields ->
      B.divWith ("", ["rect"], []) <$> (getField "body" fields >>= pWithContents pBlocks))
  ,("circle", \_ fields ->
      B.divWith ("", ["circle"], []) <$> (getField "body" fields >>= pWithContents pBlocks))
  ,("ellipse", \_ fields ->
      B.divWith ("", ["ellipse"], []) <$> (getField "body" fields >>= pWithContents pBlocks))
  ,("polygon", \_ fields ->
      B.divWith ("", ["polygon"], []) <$> (getField "body" fields >>= pWithContents pBlocks))
  ,("square", \_ fields ->
      B.divWith ("", ["square"], []) <$> (getField "body" fields >>= pWithContents pBlocks))
  ,("align", \_ fields -> do
      alignment <- getField "alignment" fields
      B.divWith ("", [], [("align", repr alignment)])
        <$> (getField "body" fields >>= pWithContents pBlocks))
  ,("stack", \_ fields -> do
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
            map (B.divWith ("", [], [])) children)
  ,("grid", \mbident fields -> do
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
          (B.TableFoot B.nullAttr []))
  ,("table", \mbident fields ->
       maybe mzero (\f -> f mbident fields) $ M.lookup "grid" blockHandlers)
  ,("figure", \mbident fields -> do
      body <- getField "body" fields >>= pWithContents pBlocks
      (mbCaption :: Maybe (Seq Content)) <- getField "caption" fields
      (caption :: B.Blocks) <- maybe mempty (pWithContents pBlocks) mbCaption
      pure $ case B.toList body of
        [B.Table attr _ colspecs thead tbodies tfoot] ->
          B.singleton
            (B.Table attr (B.Caption Nothing (B.toList caption)) colspecs thead tbodies tfoot)
        _ -> B.figureWith (fromMaybe "" mbident, [], [])
                          (B.Caption Nothing (B.toList caption)) body)
  ,("line", \_ fields ->
      case ( M.lookup "start" fields
              >> M.lookup "end" fields
              >> M.lookup "angle" fields ) of
        Nothing -> pure B.horizontalRule
        _ -> pure mempty)
  ,("numbering", \_ fields -> do
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
      pure $ B.plain . B.text . mconcat . map toNum $ V.toList nums)
  ,("footnote.entry", \_ fields ->
      getField "body" fields >>= pWithContents pBlocks)
  ]

inlineHandlers :: PandocMonad m =>
    M.Map Identifier (Maybe Text -> M.Map Identifier Val -> P m B.Inlines)
inlineHandlers = M.fromList
  [("ref", \_ fields -> do
      VLabel target <- getField "target" fields
      supplement' <- getField "supplement" fields
      supplement <- case supplement' of
                      VAuto -> -- TODO for now, until we can locate the element
                        pure $ B.text ("[" <> target <> "]")
                      VContent cs -> pWithContents pInlines cs
                      VFunction _ _ _f -> -- TODO for now, until we can locate the element
                           pure $ B.text ("[" <> target <> "]")
                      _ -> pure mempty
      pure $ B.linkWith ("", ["ref"], []) ("#" <> target) "" supplement)
  ,("linebreak", \_ _ -> pure B.linebreak)
  ,("text", \_ fields -> do
      body <- getField "body" fields
      (mbweight :: Maybe Text) <- getField "weight" fields
      case mbweight of
        Just "bold" -> B.strong <$> pWithContents pInlines body
        _ -> pWithContents pInlines body)
  ,("raw", \_ fields -> B.code . T.filter (/= '\r') <$> getField "text" fields)
  ,("footnote", \_ fields ->
      B.note <$> (getField "body" fields >>= pWithContents pBlocks))
  ,("cite", \_ fields -> do
      VLabel key <- getField "key" fields
      (form :: Text) <- getField "form" fields <|> pure "normal"
      let citation =
            B.Citation
              { B.citationId = key,
                B.citationPrefix = mempty,
                B.citationSuffix = mempty,
                B.citationMode = case form of
                                    "year" -> B.SuppressAuthor
                                    _ -> B.NormalCitation,
                B.citationNoteNum = 0,
                B.citationHash = 0
              }
      pure $ B.cite [citation] (B.text $ "[" <> key <> "]"))
  ,("lower", \_ fields -> do
      body <- getField "text" fields
      walk (modString T.toLower) <$> pWithContents pInlines body)
  ,("upper", \_ fields -> do
      body <- getField "text" fields
      walk (modString T.toUpper) <$> pWithContents pInlines body)
  ,("emph", \_ fields -> do
      body <- getField "body" fields
      B.emph <$> pWithContents pInlines body)
  ,("strong", \_ fields -> do
      body <- getField "body" fields
      B.strong <$> pWithContents pInlines body)
  ,("sub", \_ fields -> do
      body <- getField "body" fields
      B.subscript <$> pWithContents pInlines body)
  ,("super", \_ fields -> do
      body <- getField "body" fields
      B.superscript <$> pWithContents pInlines body)
  ,("strike", \_ fields -> do
      body <- getField "body" fields
      B.strikeout <$> pWithContents pInlines body)
  ,("smallcaps", \_ fields -> do
      body <- getField "body" fields
      B.smallcaps <$> pWithContents pInlines body)
  ,("underline", \_ fields -> do
      body <- getField "body" fields
      B.underline <$> pWithContents pInlines body)
  ,("quote", \_ fields -> do
      (getField "block" fields <|> pure False) >>= guard . not
      body <- getInlineBody fields >>= pWithContents pInlines
      pure $ B.doubleQuoted $ B.trimInlines body)
  ,("link", \_ fields -> do
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
          else pWithContents pInlines body <|>
               pWithContents
                (B.fromList . blocksToInlines . B.toList <$> pBlocks) body
      pure $ B.link src "" description)
  ,("image", \_ fields -> do
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
      pure $ B.imageWith attr path "" alt)
  ,("box", \_ fields -> do
      body <- getField "body" fields
      B.spanWith ("", ["box"], []) <$> pWithContents pInlines body)
  ,("h", \_ fields -> do
      amount <- getField "amount" fields `mplus` pure (LExact 1 LEm)
      let em = case amount of
            LExact x LEm -> toRational x
            _ -> case amount <> LExact 0 LPt of -- force to Pt
              LExact x LPt -> toRational x / 12
              _ -> 1 / 3 -- guess!
      pure $ B.text $ getSpaceChars em)
  ,("place", \_ fields -> do
      ignored "parameters of place"
      getField "body" fields >>= pWithContents pInlines)
  ,("align", \_ fields -> do
      alignment <- getField "alignment" fields
      B.spanWith ("", [], [("align", repr alignment)])
        <$> (getField "body" fields >>= pWithContents pInlines))
  ,("sys.version", \_ _ -> pure $ B.text "typst-hs")
  ,("math.equation", \_ fields -> do
      body <- getField "body" fields
      display <- getField "block" fields
      (if display then B.displayMath else B.math) . writeTeX <$> pMathMany body)
  ]

getInlineBody :: PandocMonad m => M.Map Identifier Val -> P m (Seq Content)
getInlineBody fields =
  parbreaksToLinebreaks <$> getField "body" fields

parbreaksToLinebreaks :: Seq Content -> Seq Content
parbreaksToLinebreaks =
  fmap go . Seq.dropWhileL isParbreak . Seq.dropWhileR isParbreak
 where
   go (Elt "parbreak" pos _) = Elt "linebreak" pos mempty
   go x = x
   isParbreak (Elt "parbreak" _ _) = True
   isParbreak _ = False

pPara :: PandocMonad m => P m B.Blocks
pPara =
  B.para . B.trimInlines . collapseAdjacentCites . mconcat
    <$> (many1 pInline <* optional pParBreak)

pParBreak :: PandocMonad m => P m ()
pParBreak =
  void $
    pTok
      ( \case
          Elt "parbreak" _ _ -> True
          _ -> False
      )

pWithContents :: PandocMonad m => P m a -> Seq Content -> P m a
pWithContents pa cs = try $ do
  inp <- getInput
  setInput $ F.toList cs
  res <- pa
  eof
  setInput inp
  pure res

pInlines :: PandocMonad m => P m B.Inlines
pInlines =
  collapseAdjacentCites . mconcat <$> many pInline

collapseAdjacentCites :: B.Inlines -> B.Inlines
collapseAdjacentCites = B.fromList . foldr go [] . B.toList
 where
   go (Cite cs1 ils1) (Cite cs2 ils2 : xs) =
     Cite (cs1 ++ cs2) (ils1 <> ils2) : xs
   go (Cite cs1 ils1) (Space : Cite cs2 ils2 : xs) =
     Cite (cs1 ++ cs2) (ils1 <> ils2) : xs
   go x xs = x:xs

modString :: (Text -> Text) -> B.Inline -> B.Inline
modString f (B.Str t) = B.Str (f t)
modString _ x = x

findLabels :: Seq.Seq Content -> [Text]
findLabels = foldr go []
 where
   go (Txt{}) = id
   go (Lab t) = (t :)
   go (Elt{ eltFields = fs }) = \ts -> foldr go' ts fs
   go' (VContent cs) = (findLabels cs ++)
   go' _ = id
