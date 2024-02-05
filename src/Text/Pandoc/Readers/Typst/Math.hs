{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Readers.Typst.Math
  ( pMathMany
  )
where

import Control.Monad (MonadPlus (mplus))
import Data.Char (isAlphaNum, isDigit)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Pandoc.Parsing ( many )
import Text.Pandoc.Class ( PandocMonad )
import Text.TeXMath.Types
  ( Alignment (..),
    Exp (..),
    FractionType (..),
    TeXSymbolType (..),
    TextType (..),
  )
import Text.TeXMath.Unicode.ToTeX (getSymbolType)
import Text.Pandoc.Readers.Typst.Parsing
    ( P, pTok, ignored, pWithContents, getField, chunks )
import Typst.Types

withGroup :: [Exp] -> Exp
withGroup [x] = x
withGroup xs = EGrouped xs

data AttachmentStyle = Limits | LimitsDisplay | Scripts
  deriving (Eq, Show)

getAttachmentStyle :: PandocMonad m => M.Map Identifier Val -> P m (Maybe AttachmentStyle)
getAttachmentStyle fields = do
  (base :: Seq Content) <- getField "base" fields
  case base of
    [Elt "math.op" _ fs] -> do
      limits <- getField "limits" fs
      if limits == VBoolean True
         then pure $ Just Limits
         else pure Nothing
    [Elt "math.limits" _ fs] -> do
      inl <- getField "inline" fs
      if inl == VBoolean False
         then pure $ Just LimitsDisplay
         else pure $ Just Limits
    [Elt "math.scripts" _ _] -> pure $ Just Scripts
    _ -> pure Nothing

pMath :: PandocMonad m => P m Exp
pMath = pTok (const True) >>= handleMath

handleMath :: PandocMonad m => Content -> P m Exp
handleMath tok =
  case tok of
    Lab t -> do
      ignored ("label " <> t)
      pure (EGrouped [])
    Txt t
      | T.any isDigit t -> pure $ ENumber t
      | T.length t == 1 ->
          case T.unpack t of
            [c] | not (isAlphaNum c) -> pure $ ESymbol (getSymbolType c) t
            _ -> pure $ EIdentifier t
      | otherwise -> pure $ EText TextNormal t
    Elt "math.dif" _ _ -> pure $ EIdentifier "d"
    Elt "math.Dif" _ _ -> pure $ EIdentifier "D"
    Elt "math.equation" _ fields -> getField "body" fields >>= pMathGrouped
    Elt "text" _ fields -> do
      body <- getField "body" fields
      (mbweight :: Maybe Text) <- getField "weight" fields
      case mbweight of
        Just "bold" -> EStyled TextBold <$> pMathMany body
        _ -> pMathGrouped body
    Elt "math.op" _ fields -> EMathOperator <$> getField "text" fields
    Elt "math.frac" _ fields -> do
      num <- getField "num" fields >>= pMathGrouped
      denom <- getField "denom" fields >>= pMathGrouped
      pure $ EFraction NormalFrac num denom
    Elt "math.accent" _ fields -> do
      base <- getField "base" fields >>= pMathGrouped
      acc <- getField "accent" fields >>= pMathGrouped
      let acc' = case acc of
            ESymbol _ "\8901" -> ESymbol Accent "\775" -- \dot
            ESymbol _ "\168" -> ESymbol Accent "\776" -- \ddot
            ESymbol _ "\8764" -> ESymbol Accent "\771" -- \tilde
            ESymbol _ t -> ESymbol Accent t
            _ -> acc
      pure $ EOver False base acc'
    Elt "math.attach" _ fields -> do
      base <- getField "base" fields >>= pMathGrouped
      t' <- getField "t" fields
      b' <- getField "b" fields
      tr' <- getField "tr" fields
      tl' <- getField "tl" fields
      br' <- getField "br" fields
      bl' <- getField "bl" fields
      attachmentStyle <- getAttachmentStyle fields
      let limits = case attachmentStyle of
                     Just Limits -> True
                     Just LimitsDisplay -> True
                     _ -> False
      let convertible = attachmentStyle == Just LimitsDisplay
      let (mbt, mbtr) =
            case (t', tr') of
              (Just top, Just topright) -> (Just top, Just topright)
              (Just top, Nothing)
                | limits -> (Just top, Nothing)
                | otherwise -> (Nothing, Just top)
              (Nothing, Just topright) -> (Nothing, Just topright)
              (Nothing, Nothing) -> (Nothing, Nothing)
      let (mbb, mbbr) =
            case (b', br') of
              (Just bot, Just botright) -> (Just bot, Just botright)
              (Just bot, Nothing)
                | limits -> (Just bot, Nothing)
                | otherwise -> (Nothing, Just bot)
              (Nothing, Just topright) -> (Nothing, Just topright)
              (Nothing, Nothing) -> (Nothing, Nothing)
      let dummy = EGrouped []
      let addPrefix x =
            case (tl', bl') of
              (Nothing, Nothing) -> pure x
              (Just top, Nothing) -> do
                res <- ESuper dummy <$> pMathGrouped top
                pure $ EGrouped [res, x]
              (Nothing, Just bot) -> do
                res <- ESub dummy <$> pMathGrouped bot
                pure $ EGrouped [res, x]
              (Just top, Just bot) -> do
                res <- ESubsup dummy <$> pMathGrouped bot <*> pMathGrouped top
                pure $ EGrouped [res, x]

      base' <- case (mbtr, mbbr) of
        (Nothing, Nothing) -> pure base
        (Nothing, Just br) -> ESub base <$> pMathGrouped br
        (Just tr, Nothing) -> ESuper base <$> pMathGrouped tr
        (Just tr, Just br) -> ESubsup base <$> pMathGrouped br <*> pMathGrouped tr

      suffix <- case (mbt, mbb) of
        (Nothing, Nothing) -> pure base'
        (Nothing, Just bot) -> EUnder convertible base' <$> pMathGrouped bot
        (Just top, Nothing) -> EOver convertible base' <$> pMathGrouped top
        (Just top, Just bot) -> EUnderover convertible base'
                                  <$> pMathGrouped bot <*> pMathGrouped top

      addPrefix suffix
    Elt "math.serif" _ fields ->
      EStyled TextNormal <$> (getField "body" fields >>= pMathMany)
    Elt "math.sans" _ fields ->
      EStyled TextSansSerif <$> (getField "body" fields >>= pMathMany)
    Elt "math.frak" _ fields ->
      EStyled TextFraktur <$> (getField "body" fields >>= pMathMany)
    Elt "math.mono" _ fields ->
      EStyled TextMonospace <$> (getField "body" fields >>= pMathMany)
    Elt "math.cal" _ fields ->
      EStyled TextScript <$> (getField "body" fields >>= pMathMany)
    Elt "math.bb" _ fields ->
      EStyled TextDoubleStruck <$> (getField "body" fields >>= pMathMany)
    Elt "math.upright" _ fields ->
      EStyled TextNormal <$> (getField "body" fields >>= pMathMany)
    Elt "math.bold" _ fields ->
      EStyled TextBold <$> (getField "body" fields >>= pMathMany)
    Elt "math.italic" _ fields ->
      EStyled TextItalic <$> (getField "body" fields >>= pMathMany)
    Elt "math.underline" _ fields ->
      EUnder False
        <$> (getField "body" fields >>= pMathGrouped)
        <*> pure (ESymbol TUnder "_")
    Elt "math.overline" _ fields ->
      EOver False
        <$> (getField "body" fields >>= pMathGrouped)
        <*> pure (ESymbol TOver "\175")
    Elt "math.underbrace" _ fields -> do
      mbAnn <- getField "annotation" fields
      body <- getField "body" fields >>= pMathGrouped
      let x = EUnder False body (ESymbol TUnder "\9183")
      case mbAnn of
        Nothing -> pure x
        Just ann -> EUnder False x <$> pMathGrouped ann
    Elt "math.overbrace" _ fields -> do
      mbAnn <- getField "annotation" fields
      body <- getField "body" fields >>= pMathGrouped
      let x = EOver False body (ESymbol TOver "\9182")
      case mbAnn of
        Nothing -> pure x
        Just ann -> EOver False x <$> pMathGrouped ann
    Elt "math.underbracket" _ fields -> do
      mbAnn <- getField "annotation" fields
      body <- getField "body" fields >>= pMathGrouped
      let x = EUnder False body (ESymbol TUnder "\9141")
      case mbAnn of
        Nothing -> pure x
        Just ann -> EUnder False x <$> pMathGrouped ann
    Elt "math.overbracket" _ fields -> do
      mbAnn <- getField "annotation" fields
      body <- getField "body" fields >>= pMathGrouped
      let x = EOver False body (ESymbol TOver "\9140")
      case mbAnn of
        Nothing -> pure x
        Just ann -> EOver False x <$> pMathGrouped ann
    Elt "math.scripts" _ fields -> getField "body" fields >>= pMathGrouped
    Elt "math.limits" _ fields -> getField "body" fields >>= pMathGrouped
    Elt "math.root" _ fields -> do
      mbindex <- getField "index" fields
      radicand <- getField "radicand" fields >>= pMathGrouped
      case mbindex of
        Nothing -> pure $ ESqrt radicand
        Just index -> do
          index' <- pMathGrouped index
          pure $ ERoot index' radicand
    Elt "math.sqrt" _ fields ->
      ESqrt <$> (getField "radicand" fields >>= pMathGrouped)
    Elt "math.abs" _ fields -> do
      body <- getField "body" fields >>= pMathGrouped
      pure $ EDelimited "|" "|" [Right body]
    Elt "math.floor" _ fields -> do
      body <- getField "body" fields >>= pMathGrouped
      pure $ EDelimited "\8970" "\8971" [Right body]
    Elt "math.ceil" _ fields -> do
      body <- getField "body" fields >>= pMathGrouped
      pure $ EDelimited "\8968" "\8969" [Right body]
    Elt "math.norm" _ fields -> do
      body <- getField "body" fields >>= pMathGrouped
      pure $ EDelimited "\8214" "\8214" [Right body]
    Elt "math.round" _ fields -> do
      body <- getField "body" fields >>= pMathGrouped
      pure $ EDelimited "\8970" "\8969" [Right body]
    Elt "math.lr" _ fields -> do
      bodyparts <- getField "body" fields >>= mapM pMathMany . V.toList
      let rawbody = intercalate [ESymbol Pun ","] bodyparts
      let (op, rest) =
            case rawbody of
              (ESymbol _ t : xs) -> (t, xs)
              _ -> ("", rawbody)
      let (body, cl) =
            case reverse rest of
              (ESymbol _ t : _) -> (map Right (init rest), t)
              _ -> (map Right rest, "")
      pure $ EDelimited op cl body
    Elt "math.binom" _ fields -> do
      up <- getField "upper" fields >>= pMathGrouped
      low <- getField "lower" fields >>= pMathGrouped
      pure $ EDelimited "(" ")" [Right (EFraction NoLineFrac up low)]
    Elt "math.cases" _ fields -> do
      (delim :: Maybe Text) <- getField "delim" fields
      (children :: [Seq Content]) <-
        map valToContent . V.toList <$> getField "children" fields
      let isAlignPoint (Elt "math.alignpoint" _ _) = True
          isAlignPoint _ = False
      let formatRow vs = case Seq.breakl isAlignPoint vs of
            (xs, ys) -> do
              case Seq.viewl ys of
                _ Seq.:< rest -> do
                  xs' <- pMathMany xs
                  ys' <- pMathMany rest
                  pure [xs', ys']
                _ -> (: []) <$> pMathMany vs
      rows <- mapM formatRow children
      pure $
        EDelimited
          (fromMaybe "{" delim)
          ""
          [Right (EArray [AlignLeft, AlignLeft] rows)]
    Elt "math.vec" _ fields -> do
      (op, cl) <- arrayDelims fields
      rows <-
        getField "children" fields
          >>= mapM (fmap (: []) . pMathMany) . V.toList
      pure $
        EDelimited
          op
          cl
          [Right (EArray [AlignCenter] rows)]
    Elt "math.mat" _ fields -> do
      (op, cl) <- arrayDelims fields
      let formatCell x = do
            let content = valToContent x
            let align = case Seq.viewl content of
                  Elt "math.alignpoint" _ _ Seq.:< _ -> AlignLeft
                  _ -> case Seq.viewr content of
                    _ Seq.:> Elt "math.alignpoint" _ _ -> AlignRight
                    _ -> AlignCenter
            exp' <- pMathMany content
            pure (align, exp')
      let formatRow (VArray vs) = mapM formatCell (V.toList vs)
          formatRow _ = fail "mat expected array"
      (rawrows :: V.Vector Val) <- getField "rows" fields
      rows <- mapM formatRow (V.toList rawrows)
      let aligns =
            case rows of
              [] -> []
              (r : _) -> map fst r
      pure $
        EDelimited
          op
          cl
          [Right (EArray aligns (map (map snd) rows))]
    Elt "hide" _ fields -> do
      EPhantom <$> (getField "body" fields >>= pMathGrouped)
    Elt "h" _ fields -> do
      amount <- getField "amount" fields
      let em = case amount of
            LExact x LEm -> toRational x
            _ -> case amount <> LExact 0 LPt of -- force to Pt
              LExact x LPt -> toRational x / 12
              _ -> 1 / 3 -- guess!
      pure $ ESpace em
    Elt "grid" _ fields -> do
      children <- getField "children" fields >>= mapM pMathMany . V.toList
      (columns :: Val) <- getField "columns" fields
      numcols <- case columns of
        VInteger x -> pure $ fromIntegral x
        VArray x -> pure $ V.length x
        VNone -> pure 1
        _ -> fail $ "Could not determine number of columns: " <> show columns
      let rows = chunks numcols children
      pure $ EArray (replicate numcols AlignLeft) rows
    Elt "table" pos fields -> handleMath (Elt "grid" pos fields)
    Elt "link" _ fields -> do
      body <- getField "body" fields
      ignored "hyperlink in math"
      pMathGrouped body
    Elt "math.display" _ fields -> do
      content <- getField "content" fields
      ignored "display"
      pMathGrouped content
    Elt "math.inline" _ fields -> do
      content <- getField "content" fields
      ignored "inline"
      pMathGrouped content
    Elt "math.script" _ fields -> do
      content <- getField "content" fields
      ignored "script"
      pMathGrouped content
    Elt "math.sscript" _ fields -> do
      content <- getField "content" fields
      ignored "sscript"
      pMathGrouped content
    Elt (Identifier name) _ fields -> do
      body <- getField "body" fields `mplus` pure mempty
      ignored name
      pMathGrouped body

arrayDelims :: PandocMonad m => M.Map Identifier Val -> P m (Text, Text)
arrayDelims fields = do
  (mbdelim :: Maybe Text) <- getField "delim" fields
  pure $ case mbdelim of
    Just "(" -> ("(", ")")
    Just "[" -> ("[", "]")
    Just "{" -> ("{", "}")
    Just "|" -> ("|", "|")
    Just "||" -> ("\8741", "\8741")
    _ -> ("(", ")")

pMathMany :: PandocMonad m => Seq Content -> P m [Exp]
pMathMany cs = do
  -- check for "alignpoint" and "linebreak" elements
  -- and use an array structure for alignment
  let lns = splitOnLinebreaks cs
  case lns of
    [] -> pure []
    [ln] | not (any isAlignpoint ln) -> pWithContents (many pMath) ln
    _ -> do
      rows <- mapM (mapM (pWithContents (many pMath)) . splitOnAlignpoints) lns
      let numcols = maximum $ map length rows
      let cols = take numcols $ AlignRight : cycle [AlignLeft, AlignRight]
      pure [EArray cols rows]

pMathGrouped :: PandocMonad m => Seq Content -> P m Exp
pMathGrouped = fmap withGroup . pMathMany

splitOnLinebreaks :: Seq Content -> [Seq Content]
splitOnLinebreaks xs =
  if Seq.null bs
    then
      if null as
        then []
        else [as]
    else as : splitOnLinebreaks (Seq.drop 1 bs)
  where
    (as, bs) = Seq.breakl isLinebreak xs
    isLinebreak (Elt "linebreak" _ _) = True
    isLinebreak _ = False

splitOnAlignpoints :: Seq Content -> [Seq Content]
splitOnAlignpoints xs =
  if Seq.null bs
    then
      if null as
        then []
        else [as]
    else as : splitOnAlignpoints (Seq.drop 1 bs)
  where
    (as, bs) = Seq.breakl isAlignpoint xs

isAlignpoint :: Content -> Bool
isAlignpoint (Elt "math.alignpoint" _ _) = True
isAlignpoint _ = False
