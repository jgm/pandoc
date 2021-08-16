{-# LANGUAGE OverloadedStrings    #-}
{- |
   Module      : Text.Pandoc.Readers.Docx.Combine
   Copyright   : © 2014-2020 Jesse Rosenthal <jrosenthal@jhu.edu>,
                   2014-2021 John MacFarlane <jgm@berkeley.edu>,
                   2020 Nikolay Yakimov <root@livid.pp.ru>
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Flatten sequences of elements.
-}

{-
The purpose of this module is to combine the formatting of separate
runs, which have *non-nesting* formatting. Because the formatting
doesn't nest, you can't actually tell the nesting order until you
combine with the runs that follow.

For example, say you have a something like `<em><strong>foo</strong>
bar</em>`. Then in ooxml, you'll get these two runs:

~~~
<w:r>
 <w:rPr>
  <w:b />
  <w:i />
 </w:rPr>
 <w:t>Foo</w:t>
</w:r>
<w:r>
 <w:rPr>
  <w:i />
 </w:rPr>
 <w:t> Bar</w:t>
</w:r>
~~~

Note that this is an ideal situation. In practice, it will probably be
more---if, for example, the user turned italics
off and then on.

So, when you get the first run, which is marked as both bold and italic,
you have no idea whether it's `Strong [Emph [Str "Foo"]]` or `Emph
[Strong [Str "Foo"]]`.

We combine two runs, then, by taking off the formatting that modifies an
inline, seeing what is shared between them, and rebuilding an inline. We
fold this to combine the inlines.

-}

module Text.Pandoc.Readers.Docx.Combine ( smushInlines
                                        , smushBlocks
                                        )
       where

import Data.List
import Data.Bifunctor
import Data.Sequence ( ViewL (..), ViewR (..), viewl, viewr, spanr, spanl
                     , (><), (|>) )
import Text.Pandoc.Builder as B

data Modifier a = Modifier (a -> a)
                | AttrModifier (Attr -> a -> a) Attr

spaceOutInlinesL :: Inlines -> (Inlines, Inlines)
spaceOutInlinesL ms = (l, stackInlines fs (m' <> r))
  where (l, (fs, m'), r) = spaceOutInlines ms

spaceOutInlinesR :: Inlines -> (Inlines, Inlines)
spaceOutInlinesR ms = (stackInlines fs (l <> m'), r)
  where (l, (fs, m'), r) = spaceOutInlines ms

spaceOutInlines :: Inlines -> (Inlines, ([Modifier Inlines], Inlines), Inlines)
spaceOutInlines ils =
  let (fs, ils') = unstackInlines ils
      (left, (right, contents')) = second (spanr isSpace) $ spanl isSpace $ unMany ils'
      -- NOTE: spanr counterintuitively returns suffix as the FIRST tuple element
  in (Many left, (fs, Many contents'), Many right)

isSpace :: Inline -> Bool
isSpace Space = True
isSpace SoftBreak = True
isSpace _ = False

stackInlines :: [Modifier Inlines] -> Inlines -> Inlines
stackInlines [] ms = ms
stackInlines (Modifier f : fs) ms =
  if null ms
  then stackInlines fs ms
  else f $ stackInlines fs ms
stackInlines (AttrModifier f attr : fs) ms = f attr $ stackInlines fs ms

unstackInlines :: Inlines -> ([Modifier Inlines], Inlines)
unstackInlines ms = case ilModifierAndInnards ms of
  Nothing         -> ([], ms)
  Just (f, inner) -> first (f :) $ unstackInlines inner

ilModifierAndInnards :: Inlines -> Maybe (Modifier Inlines, Inlines)
ilModifierAndInnards ils = case viewl $ unMany ils of
  x :< xs | null xs -> second fromList <$> case x of
    Emph lst          -> Just (Modifier emph, lst)
    Strong lst        -> Just (Modifier strong, lst)
    SmallCaps lst     -> Just (Modifier smallcaps, lst)
    Strikeout lst     -> Just (Modifier strikeout, lst)
    Underline lst     -> Just (Modifier underline, lst)
    Superscript lst   -> Just (Modifier superscript, lst)
    Subscript lst     -> Just (Modifier subscript, lst)
    Link attr lst tgt -> Just (Modifier $ uncurry (linkWith attr) tgt, lst)
    Span attr lst     -> Just (AttrModifier spanWith attr, lst)
    _                 -> Nothing
  _ -> Nothing

inlinesL :: Inlines -> (Inlines, Inlines)
inlinesL ils = case viewl $ unMany ils of
  (s :< sq) -> (B.singleton s, Many sq)
  _         -> (mempty, ils)

inlinesR :: Inlines -> (Inlines, Inlines)
inlinesR ils = case viewr $ unMany ils of
  (sq :> s) -> (Many sq, B.singleton s)
  _         -> (ils, mempty)

combineInlines :: Inlines -> Inlines -> Inlines
combineInlines x y =
  let (xs', x') = inlinesR x
      (y', ys') = inlinesL y
  in
   xs' <> combineSingletonInlines x' y' <> ys'

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
     True | null xs && null ys ->
            stackInlines (x_rem_attr <> y_rem_attr) mempty
          | null xs ->
            let (sp, y') = spaceOutInlinesL y in
            stackInlines x_rem_attr mempty <> sp <> y'
          | null ys ->
            let (x', sp) = spaceOutInlinesR x in
            x' <> sp <> stackInlines y_rem_attr mempty
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
  | bs' :> BlockQuote bs'' <- viewr (unMany bs)
  , BlockQuote cs'' :< cs' <- viewl (unMany cs) =
      Many $ (bs' |> BlockQuote (bs'' <> cs'')) >< cs'
  | bs' :> CodeBlock attr codeStr <- viewr (unMany bs)
  , CodeBlock attr' codeStr' :< cs' <- viewl (unMany cs)
  , attr == attr' =
      Many $ (bs' |> CodeBlock attr (codeStr <> "\n" <> codeStr')) >< cs'
combineBlocks bs cs = bs <> cs

instance (Monoid a, Eq a) => Eq (Modifier a) where
  (Modifier f) == (Modifier g) = f mempty == g mempty
  (AttrModifier f attr) == (AttrModifier g attr') = f attr mempty == g attr' mempty
  _ == _ = False

isAttrModifier :: Modifier a -> Bool
isAttrModifier (AttrModifier _ _) = True
isAttrModifier _                  = False

smushInlines :: [Inlines] -> Inlines
smushInlines xs = combineInlines xs' mempty
  where xs' = foldl' combineInlines mempty xs

smushBlocks :: [Blocks] -> Blocks
smushBlocks xs = foldl' combineBlocks mempty xs
