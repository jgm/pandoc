{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- provides Arbitrary instance for Pandoc types
module Tests.Arbitrary ()
where
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Control.Monad (liftM, liftM2)
import Text.Pandoc
import Text.Pandoc.Shared
import Text.Pandoc.Builder

realString :: Gen String
realString = resize 8 arbitrary -- elements wordlist

wordlist :: [String]
wordlist = ["foo","Bar","baz","\\","/",":","\"","'","féé"]

instance Arbitrary Inlines where
  arbitrary = liftM fromList arbitrary

instance Arbitrary Blocks where
  arbitrary = liftM fromList arbitrary

instance Arbitrary Inline where
  arbitrary = resize 3 $ arbInline 3

-- restrict to 3 levels of nesting max; otherwise we get
-- bogged down in indefinitely large structures
arbInline :: Int -> Gen Inline
arbInline n = frequency $ [ (60, liftM Str realString)
                          , (60, return Space)
                          , (10, liftM2 Code arbitrary realString)
                          , (5,  return EmDash)
                          , (5,  return EnDash)
                          , (5,  return Apostrophe)
                          , (5,  return Ellipses)
                          , (5,  elements [ RawInline "html" "<a>*&amp;*</a>"
                                          , RawInline "latex" "\\my{command}" ])
                          ] ++ [ x | x <- nesters, n > 1]
   where nesters = [ (10,  liftM Emph $ listOf $ arbInline (n-1))
                   , (10,  liftM Strong $ listOf $ arbInline (n-1))
                   , (10,  liftM Strikeout $ listOf $ arbInline (n-1))
                   , (10,  liftM Superscript $ listOf $ arbInline (n-1))
                   , (10,  liftM Subscript $ listOf $ arbInline (n-1))
                   , (10,  liftM SmallCaps $ listOf $ arbInline (n-1))
                   , (10,  do x1 <- arbitrary
                              x2 <- listOf $ arbInline (n-1)
                              return $ Quoted x1 x2)
                   , (10,  do x1 <- arbitrary
                              x2 <- realString
                              return $ Math x1 x2)
                   , (10,  do x1 <- listOf $ arbInline (n-1)
                              x3 <- realString
                              x2 <- realString
                              return $ Link x1 (x2,x3))
                   , (10,  do x1 <- listOf $ arbInline (n-1)
                              x3 <- realString
                              x2 <- realString
                              return $ Image x1 (x2,x3))
                   , (2,  liftM Note $ resize 3 $ listOf1 arbitrary)
                   ]

instance Arbitrary Block where
  arbitrary = resize 3 $ arbBlock 3

arbBlock :: Int -> Gen Block
arbBlock n = frequency $ [ (10, liftM Plain arbitrary)
                         , (15, liftM Para arbitrary)
                         , (5,  liftM2 CodeBlock arbitrary realString)
                         , (2,  elements [ RawBlock "html"
                                            "<div>\n*&amp;*\n</div>"
                                         , RawBlock "latex"
                                            "\\begin[opt]{env}\nhi\n{\\end{env}"
                                         ])
                         , (5,  do x1 <- choose (1 :: Int, 6)
                                   x2 <- arbitrary
                                   return (Header x1 x2))
                         , (2, return HorizontalRule)
                         ] ++ [x | x <- nesters, n > 0]
   where nesters = [ (5,  liftM BlockQuote $ listOf $ arbBlock (n-1))
                   , (5,  liftM2 OrderedList arbitrary
                          $ (listOf $ listOf $ arbBlock (n-1)))
                   , (5,  liftM BulletList $ (listOf $ listOf $ arbBlock (n-1)))
                   , (5,  do x1 <- listOf $ listOf $ listOf $ arbBlock (n-1)
                             x2 <- arbitrary
                             return (DefinitionList $ zip x2 x1))
                   , (2, do rs <- choose (1 :: Int, 4)
                            cs <- choose (1 :: Int, 4)
                            x1 <- arbitrary
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

{-
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
          = do x1 <- liftM (filter (`notElem` ",;]@ \t\n")) arbitrary
               x2 <- arbitrary
               x3 <- arbitrary
               x4 <- arbitrary
               x5 <- arbitrary
               x6 <- arbitrary
               return (Citation x1 x2 x3 x4 x5 x6)
-}

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
          = do x1 <- arbitrary
               x2 <- liftM (filter (not . null)) arbitrary
               x3 <- arbitrary
               return (Meta x1 x2 x3)

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

