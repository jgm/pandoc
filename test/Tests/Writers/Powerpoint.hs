module Tests.Writers.Powerpoint (tests) where

import Tests.Writers.OOXML (ooxmlTest)
import Text.Pandoc
import Test.Tasty

pptxTest :: String -> WriterOptions -> FilePath -> FilePath -> TestTree
pptxTest = ooxmlTest writePowerpoint

tests :: [TestTree]
tests = [ pptxTest
          "Inline formatting"
          def
          "pptx/inline_formatting.native"
          "pptx/inline_formatting.pptx"
        , pptxTest
          "Slide breaks (default slide-level)"
          def
          "pptx/slide_breaks.native"
          "pptx/slide_breaks.pptx"
        , pptxTest
          "slide breaks (slide-level set to 1)"
          def{ writerSlideLevel = Just 1 }
          "pptx/slide_breaks.native"
          "pptx/slide_breaks_slide_level_1.pptx"
        , pptxTest
          "table of contents"
          def{ writerTableOfContents = True }
          "pptx/slide_breaks.native"
          "pptx/slide_breaks_toc.pptx"
        , pptxTest
          "end notes"
          def
          "pptx/endnotes.native"
          "pptx/endnotes.pptx"
        , pptxTest
          "end notes, with table of contents"
          def { writerTableOfContents = True }
          "pptx/endnotes.native"
          "pptx/endnotes_toc.pptx"
        , pptxTest
          "images"
          def
          "pptx/images.native"
          "pptx/images.pptx"
        , pptxTest
          "two-column layout"
          def
          "pptx/two_column.native"
          "pptx/two_column.pptx"
        ]
