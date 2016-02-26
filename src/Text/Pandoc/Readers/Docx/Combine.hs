{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
    PatternGuards #-}

module Text.Pandoc.Readers.Docx.Combine ( smushInlines
                                        , smushBlocks
                                        )
       where

import Text.Pandoc.Builder
import Data.List
import Data.Sequence (ViewR(..), ViewL(..), viewl, viewr, (><), (|>))
import qualified Data.Sequence as Seq (null)

data Modifier a = Modifier (a -> a)
                | AttrModifier (Attr -> a -> a) Attr
                | NullModifier

spaceOutInlinesL :: Inlines -> (Inlines, Inlines)
spaceOutInlinesL ms = (l, stackInlines fs (m' <> r))
  where (l, m, r) = spaceOutInlines ms
        (fs, m')  = unstackInlines m

spaceOutInlinesR :: Inlines -> (Inlines, Inlines)
spaceOutInlinesR ms = (stackInlines fs (l <> m'), r)
  where (l, m, r) = spaceOutInlines ms
        (fs, m')  = unstackInlines m

spaceOutInlines :: Inlines -> (Inlines, Inlines, Inlines)
spaceOutInlines ils =
  let (fs, ils') = unstackInlines ils
      contents = unMany ils'
      left  = case viewl contents of
        (Space :< _) -> space
        _            -> mempty
      right = case viewr contents of
        (_ :> Space) -> space
        _            -> mempty in
  (left, (stackInlines fs $ trimInlines . Many $ contents), right)

stackInlines :: [Modifier Inlines] -> Inlines -> Inlines
stackInlines [] ms = ms
stackInlines (NullModifier : fs) ms = stackInlines fs ms
stackInlines ((Modifier f) : fs) ms =
  if isEmpty ms
  then stackInlines fs ms
  else f $ stackInlines fs ms
stackInlines ((AttrModifier f attr) : fs) ms = f attr $ stackInlines fs ms

unstackInlines :: Inlines -> ([Modifier Inlines], Inlines)
unstackInlines ms = case ilModifier ms of
  NullModifier -> ([], ms)
  _            -> (f : fs, ms') where
    f = ilModifier ms
    (fs, ms') = unstackInlines $ ilInnards ms

ilModifier :: Inlines -> Modifier Inlines
ilModifier ils = case viewl (unMany ils) of
  (x :< xs) | Seq.null xs -> case x of
    (Emph _)        -> Modifier emph
    (Strong _)      -> Modifier strong
    (SmallCaps _)   -> Modifier smallcaps
    (Strikeout _)   -> Modifier strikeout
    (Superscript _) -> Modifier superscript
    (Subscript _)   -> Modifier subscript
    (Link attr _ tgt) -> Modifier $ linkWith attr (fst tgt) (snd tgt)
    (Span attr _)   -> AttrModifier spanWith attr
    _               -> NullModifier
  _ -> NullModifier

ilInnards :: Inlines -> Inlines
ilInnards ils = case viewl (unMany ils) of
  (x :< xs) | Seq.null xs -> case x of
    (Emph lst)        -> fromList lst
    (Strong lst)      -> fromList lst
    (SmallCaps lst)   -> fromList lst
    (Strikeout lst)   -> fromList lst
    (Superscript lst) -> fromList lst
    (Subscript lst)   -> fromList lst
    (Link _ lst _)    -> fromList lst
    (Span _ lst)      -> fromList lst
    _        -> ils
  _          -> ils

inlinesL :: Inlines -> (Inlines, Inlines)
inlinesL ils = case viewl $ unMany ils of
  (s :< sq) -> (singleton s, Many sq)
  _          -> (mempty, ils)

inlinesR :: Inlines -> (Inlines, Inlines)
inlinesR ils = case viewr $ unMany ils of
  (sq :> s) -> (Many sq, singleton s)
  _          -> (ils, mempty)

combineInlines :: Inlines -> Inlines -> Inlines
combineInlines x y =
  let (xs', x') = inlinesR x
      (y', ys') = inlinesL y
  in
   xs' <> (combineSingletonInlines x' y') <> ys'

combineSingletonInlines :: Inlines -> Inlines -> Inlines
combineSingletonInlines x y =
  let (xfs, xs) = unstackInlines x
      (yfs, ys) = unstackInlines y
      shared = xfs `intersect` yfs
      x_remaining = xfs \\ shared
      y_remaining = yfs \\ shared
      x_rem_attr = filter isAttrModifier x_remaining
      y_rem_attr = filter isAttrModifier y_remaining
  in
   case null shared of
     True | isEmpty xs && isEmpty ys ->
            stackInlines (x_rem_attr ++ y_rem_attr) mempty
          | isEmpty xs ->
            let (sp, y') = spaceOutInlinesL y in
            (stackInlines x_rem_attr mempty) <> sp <> y'
          | isEmpty ys ->
            let (x', sp) = spaceOutInlinesR x in
            x' <> sp <> (stackInlines y_rem_attr mempty)
          | otherwise ->
              let (x', xsp) = spaceOutInlinesR x
                  (ysp, y') = spaceOutInlinesL y
              in
               x' <> xsp <> ysp <> y'
     False -> stackInlines shared $
              combineInlines
              (stackInlines x_remaining xs)
              (stackInlines y_remaining ys)

combineBlocks :: Blocks -> Blocks -> Blocks
combineBlocks bs cs
  | bs' :> (BlockQuote bs'') <- viewr (unMany bs)
  , (BlockQuote cs'') :< cs' <- viewl (unMany cs) =
      Many $ (bs' |> (BlockQuote (bs'' <> cs''))) >< cs'
combineBlocks bs cs = bs <> cs

instance (Monoid a, Eq a) => Eq (Modifier a) where
  (Modifier f) == (Modifier g) = (f mempty == g mempty)
  (AttrModifier f attr) == (AttrModifier g attr') = (f attr mempty == g attr' mempty)
  (NullModifier) == (NullModifier) = True
  _ == _ = False

isEmpty :: (Monoid a, Eq a) => a -> Bool
isEmpty x = x == mempty

isAttrModifier :: Modifier a -> Bool
isAttrModifier (AttrModifier _ _) = True
isAttrModifier _                  = False

smushInlines :: [Inlines] -> Inlines
smushInlines xs = foldl combineInlines mempty xs

smushBlocks :: [Blocks] -> Blocks
smushBlocks xs = foldl combineBlocks mempty xs
