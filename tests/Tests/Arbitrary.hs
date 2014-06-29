{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
-- provides Arbitrary instance for Pandoc types
module Tests.Arbitrary ()
where
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Control.Monad (liftM, liftM2)
import Text.Pandoc.Definition
import Text.Pandoc.Shared (normalize, escapeURI)
import Text.Pandoc.Builder

realString :: Gen String
realString = resize 8 $ listOf $ frequency [ (9, elements [' '..'\127'])
                                           , (1, elements ['\128'..'\9999']) ]

arbAttr :: Gen Attr
arbAttr = do
  id' <- elements ["","loc"]
  classes <- elements [[],["haskell"],["c","numberLines"]]
  keyvals <- elements [[],[("start","22")],[("a","11"),("b_2","a b c")]]
  return (id',classes,keyvals)

instance Arbitrary Inlines where
  arbitrary = liftM (fromList :: [Inline] -> Inlines) arbitrary

instance Arbitrary Blocks where
  arbitrary = liftM (fromList :: [Block] -> Blocks) arbitrary

instance Arbitrary Inline where
  arbitrary = resize 3 $ arbInline 2

arbInlines :: Int -> Gen [Inline]
arbInlines n = listOf1 (arbInline n) `suchThat` (not . startsWithSpace)
  where startsWithSpace (Space:_) = True
        startsWithSpace        _  = False

-- restrict to 3 levels of nesting max; otherwise we get
-- bogged down in indefinitely large structures
arbInline :: Int -> Gen Inline
arbInline n = frequency $ [ (60, liftM Str realString)
                          , (60, return Space)
                          , (10, liftM2 Code arbAttr realString)
                          , (5,  elements [ RawInline (Format "html") "<a id=\"eek\">"
                                          , RawInline (Format "latex") "\\my{command}" ])
                          ] ++ [ x | x <- nesters, n > 1]
   where nesters = [ (10,  liftM Emph $ arbInlines (n-1))
                   , (10,  liftM Strong $ arbInlines (n-1))
                   , (10,  liftM Strikeout $ arbInlines (n-1))
                   , (10,  liftM Superscript $ arbInlines (n-1))
                   , (10,  liftM Subscript $ arbInlines (n-1))
                   , (10,  liftM SmallCaps $ arbInlines (n-1))
                   , (10,  do x1 <- arbitrary
                              x2 <- arbInlines (n-1)
                              return $ Quoted x1 x2)
                   , (10,  do x1 <- arbitrary
                              x2 <- realString
                              return $ Math x1 x2)
                   , (10,  do x1 <- arbInlines (n-1)
                              x3 <- realString
                              x2 <- liftM escapeURI realString
                              return $ Link x1 (x2,x3))
                   , (10,  do x1 <- arbInlines (n-1)
                              x3 <- realString
                              x2 <- liftM escapeURI realString
                              return $ Image x1 (x2,x3))
                   , (2,  liftM2 Cite arbitrary (arbInlines 1))
                   , (2,  liftM Note $ resize 3 $ listOf1 $ arbBlock (n-1))
                   ]

instance Arbitrary Block where
  arbitrary = resize 3 $ arbBlock 2

arbBlock :: Int -> Gen Block
arbBlock n = frequency $ [ (10, liftM Plain $ arbInlines (n-1))
                         , (15, liftM Para $ arbInlines (n-1))
                         , (5,  liftM2 CodeBlock arbAttr realString)
                         , (2,  elements [ RawBlock (Format "html")
                                            "<div>\n*&amp;*\n</div>"
                                         , RawBlock (Format "latex")
                                            "\\begin[opt]{env}\nhi\n{\\end{env}"
                                         ])
                         , (5,  do x1 <- choose (1 :: Int, 6)
                                   x2 <- arbInlines (n-1)
                                   return (Header x1 nullAttr x2))
                         , (2, return HorizontalRule)
                         ] ++ [x | x <- nesters, n > 0]
   where nesters = [ (5,  liftM BlockQuote $ listOf1 $ arbBlock (n-1))
                   , (5,  do x2 <- arbitrary
                             x3 <- arbitrary
                             x1 <- arbitrary `suchThat` (> 0)
                             x4 <- listOf1 $ listOf1 $ arbBlock (n-1)
                             return $ OrderedList (x1,x2,x3) x4 )
                   , (5,  liftM BulletList $ (listOf1 $ listOf1 $ arbBlock (n-1)))
                   , (5,  do items <- listOf1 $ do
                                        x1 <- listOf1 $ listOf1 $ arbBlock (n-1)
                                        x2 <- arbInlines (n-1)
                                        return (x2,x1)
                             return $ DefinitionList items)
                   , (2, do rs <- choose (1 :: Int, 4)
                            cs <- choose (1 :: Int, 4)
                            x1 <- arbInlines (n-1)
                            x2 <- vector cs
                            x3 <- vectorOf cs $ elements [0, 0.25]
                            x4 <- vectorOf cs $ listOf $ arbBlock (n-1)
                            x5 <- vectorOf rs $ vectorOf cs
                                  $ listOf $ arbBlock (n-1)
                            return (Table x1 x2 x3 x4 x5))
                   ]

instance Arbitrary Pandoc where
        arbitrary = resize 8 $ liftM normalize
                             $ liftM2 Pandoc arbitrary arbitrary

instance Arbitrary CitationMode where
        arbitrary
          = do x <- choose (0 :: Int, 2)
               case x of
                   0 -> return AuthorInText
                   1 -> return SuppressAuthor
                   2 -> return NormalCitation
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

instance Arbitrary Citation where
        arbitrary
          = do x1 <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
               x2 <- arbInlines 1
               x3 <- arbInlines 1
               x4 <- arbitrary
               x5 <- arbitrary
               x6 <- arbitrary
               return (Citation x1 x2 x3 x4 x5 x6)

instance Arbitrary MathType where
        arbitrary
          = do x <- choose (0 :: Int, 1)
               case x of
                   0 -> return DisplayMath
                   1 -> return InlineMath
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

instance Arbitrary QuoteType where
        arbitrary
          = do x <- choose (0 :: Int, 1)
               case x of
                   0 -> return SingleQuote
                   1 -> return DoubleQuote
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

instance Arbitrary Meta where
        arbitrary
          = do (x1 :: Inlines) <- arbitrary
               (x2 :: [Inlines]) <- liftM (filter (not . isNull)) arbitrary
               (x3 :: Inlines) <- arbitrary
               return $ setMeta "title" x1
                      $ setMeta "author" x2
                      $ setMeta "date" x3
                      $ nullMeta

instance Arbitrary Alignment where
        arbitrary
          = do x <- choose (0 :: Int, 3)
               case x of
                   0 -> return AlignLeft
                   1 -> return AlignRight
                   2 -> return AlignCenter
                   3 -> return AlignDefault
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

instance Arbitrary ListNumberStyle where
        arbitrary
          = do x <- choose (0 :: Int, 6)
               case x of
                   0 -> return DefaultStyle
                   1 -> return Example
                   2 -> return Decimal
                   3 -> return LowerRoman
                   4 -> return UpperRoman
                   5 -> return LowerAlpha
                   6 -> return UpperAlpha
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

instance Arbitrary ListNumberDelim where
        arbitrary
          = do x <- choose (0 :: Int, 3)
               case x of
                   0 -> return DefaultDelim
                   1 -> return Period
                   2 -> return OneParen
                   3 -> return TwoParens
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

