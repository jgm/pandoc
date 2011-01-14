module Tests.Shared (tests) where
import Test.Framework.Providers.QuickCheck2
import Test.Framework
import Tests.Arbitrary
import Text.Pandoc.Shared
import Text.Pandoc

normalize_rt :: Pandoc -> Bool
normalize_rt d = normalize (normalize d) == normalize d

tests :: [Test]
tests = [ testProperty "normalize_rt" normalize_rt ]

