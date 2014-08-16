{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
    PatternGuards #-}

module Text.Pandoc.Readers.Docx.Reducible ( concatReduce
                                          , (<+>)
                                          )
       where


import Text.Pandoc.Builder
import Data.Monoid
import Data.List
import Data.Sequence (ViewR(..), ViewL(..), viewl, viewr)
import qualified Data.Sequence as Seq (null)

data Modifier a = Modifier (a -> a)
                | AttrModifier (Attr -> a -> a) Attr
                | NullModifier

class (Eq a) => Modifiable a where
  modifier :: a -> Modifier a
  innards :: a -> a
  getL    :: a -> (a, a)
  getR    :: a -> (a, a)
  spaceOut :: a -> (a, a, a)

spaceOutL :: (Monoid a, Modifiable a) => a -> (a, a)
spaceOutL ms = (l, stack fs (m' <> r))
  where (l, m, r) = spaceOut ms
        (fs, m')  = unstack m

spaceOutR :: (Monoid a, Modifiable a) => a -> (a, a)
spaceOutR ms = (stack fs (l <> m'), r)
  where (l, m, r) = spaceOut ms
        (fs, m')  = unstack m

instance (Monoid a, Show a) => Show (Modifier a) where
  show (Modifier f) = show $ f mempty
  show (AttrModifier f attr) = show $ f attr mempty
  show (NullModifier) = "NullModifier"

instance (Monoid a, Eq a) => Eq (Modifier a) where
  (Modifier f) == (Modifier g) = (f mempty == g mempty)
  (AttrModifier f attr) == (AttrModifier g attr') = (f attr mempty == g attr' mempty)
  (NullModifier) == (NullModifier) = True
  _ == _ = False

instance Modifiable Inlines where
  modifier ils = case viewl (unMany ils) of
    (x :< xs) | Seq.null xs -> case x of
      (Emph _)        -> Modifier emph
      (Strong _)      -> Modifier strong
      (SmallCaps _)   -> Modifier smallcaps
      (Strikeout _)   -> Modifier strikeout
      (Superscript _) -> Modifier superscript
      (Subscript _)   -> Modifier subscript
      (Span attr _)   -> AttrModifier spanWith attr
      _               -> NullModifier
    _ -> NullModifier

  innards ils = case viewl (unMany ils) of
    (x :< xs) | Seq.null xs -> case x of
      (Emph lst)        -> fromList lst
      (Strong lst)      -> fromList lst
      (SmallCaps lst)   -> fromList lst
      (Strikeout lst)   -> fromList lst
      (Superscript lst) -> fromList lst
      (Subscript lst)   -> fromList lst
      (Span _ lst)      -> fromList lst
      _        -> ils
    _          -> ils

  getL ils = case viewl $ unMany ils of
    (s :< sq) -> (singleton s, Many sq)
    _          -> (mempty, ils)

  getR ils = case viewr $ unMany ils of
    (sq :> s) -> (Many sq, singleton s)
    _          -> (ils, mempty)

  spaceOut ils =
    let (fs, ils') = unstack ils
        contents = unMany ils'
        left  = case viewl contents of
          (Space :< _) -> space
          _            -> mempty
        right = case viewr contents of
          (_ :> Space) -> space
          _            -> mempty in
    (left, (stack fs $ trimInlines .Many $ contents), right)

instance Modifiable Blocks where
  modifier blks = case viewl (unMany blks) of
    (x :< xs) | Seq.null xs -> case x of
      (BlockQuote _) -> Modifier blockQuote
      -- (Div attr _)   -> AttrModifier divWith attr
      _               -> NullModifier
    _ -> NullModifier

  innards blks = case viewl (unMany blks) of
    (x :< xs) | Seq.null xs -> case x of
      (BlockQuote lst) -> fromList lst
      -- (Div attr lst)   -> fromList lst
      _        -> blks
    _          -> blks

  spaceOut blks = (mempty, blks, mempty)

  getL ils = case viewl $ unMany ils of
    (s :< sq) -> (singleton s, Many sq)
    _          -> (mempty, ils)

  getR ils = case viewr $ unMany ils of
    (sq :> s) -> (Many sq, singleton s)
    _          -> (ils, mempty)


unstack :: (Modifiable a) => a -> ([Modifier a], a)
unstack ms = case modifier ms of
  NullModifier -> ([], ms)
  _            -> (f : fs, ms') where
    f = modifier ms
    (fs, ms') = unstack $ innards ms

stack :: (Monoid a, Modifiable a) => [Modifier a] -> a -> a
stack [] ms = ms
stack (NullModifier : fs) ms = stack fs ms
stack ((Modifier f) : fs) ms =
  if isEmpty ms
  then stack fs ms
  else f $ stack fs ms
stack ((AttrModifier f attr) : fs) ms = f attr $ stack fs ms

isEmpty :: (Monoid a, Eq a) => a -> Bool
isEmpty x = x == mempty


combine :: (Monoid a, Modifiable a, Eq a) => a -> a -> a
combine x y =
  let (xs', x') = getR x
      (y', ys') = getL y
  in
   xs' <> (combineSingleton x' y') <> ys'

isAttrModifier :: Modifier a -> Bool
isAttrModifier (AttrModifier _ _) = True
isAttrModifier _                  = False

combineSingleton :: (Monoid a, Modifiable a, Eq a) => a -> a -> a
combineSingleton x y =
  let (xfs, xs) = unstack x
      (yfs, ys) = unstack y
      shared = xfs `intersect` yfs
      x_remaining = xfs \\ shared
      y_remaining = yfs \\ shared
      x_rem_attr = filter isAttrModifier x_remaining
      y_rem_attr = filter isAttrModifier y_remaining
  in
   case null shared of
     True | isEmpty xs && isEmpty ys ->
            stack (x_rem_attr ++ y_rem_attr) mempty
          | isEmpty xs ->
            let (sp, y') = spaceOutL y in
            (stack x_rem_attr mempty) <> sp <> y'
          | isEmpty ys ->
            let (x', sp) = spaceOutR x in
            x' <> sp <> (stack y_rem_attr mempty)
          | otherwise ->
              let (x', xsp) = spaceOutR x
                  (ysp, y') = spaceOutL y
              in
               x' <> xsp <> ysp <> y'
     False -> stack shared $
              combine
              (stack x_remaining xs)
              (stack y_remaining ys)

(<+>) :: (Monoid a, Modifiable a, Eq a) => a -> a -> a
x <+> y = combine x y

concatReduce :: (Monoid a, Modifiable a) => [a] -> a
concatReduce xs = foldl combine mempty xs
