{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.AsciiDoc (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Tests.Arbitrary()
import Data.Monoid

asciidoc :: (ToString a, ToPandoc a) => a -> String
asciidoc = writeAsciiDoc def{ writerWrapText = False } . toPandoc

tests :: [Test]
tests = [ testGroup "tables"
          [ test asciidoc "empty cells" $
               simpleTable [] [[mempty],[mempty]] =?> unlines
                                           [ "[cols=\"\",]"
                                           , "|===="
                                           , "|"
                                           , "|"
                                           , "|===="
                                           ]
          , test asciidoc "multiblock cells" $
               simpleTable [] [[para "Para 1" <> para "Para 2"]]
                                           =?> unlines
                                           [ "[cols=\"\",]"
                                           , "|====="
                                           , "a|"
                                           , "Para 1"
                                           , ""
                                           , "Para 2"
                                           , ""
                                           , "|====="
                                           ]
          ]
        ]
