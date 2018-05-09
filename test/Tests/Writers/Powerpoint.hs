{-# LANGUAGE NoImplicitPrelude #-}
module Tests.Writers.Powerpoint (tests) where

import Prelude
import Tests.Writers.OOXML (ooxmlTest)
import Text.Pandoc
import Test.Tasty
import System.FilePath

-- templating is important enough, and can break enough things, that
-- we want to run all our tests with both default formatting and a
-- template.

modifyPptxName :: FilePath -> FilePath
modifyPptxName fp =
  addExtension (dropExtension fp ++ "_templated") "pptx"

pptxTests :: String -> WriterOptions -> FilePath -> FilePath -> (TestTree, TestTree)
pptxTests name opts native pptx =
  let referenceDoc = "pptx/reference_depth.pptx"
  in
    ( ooxmlTest
      writePowerpoint
      name
      opts{writerReferenceDoc=Nothing}
      native
      pptx
    , ooxmlTest
      writePowerpoint
      name
      opts{writerReferenceDoc=Just referenceDoc}
      native
      (modifyPptxName pptx)
    )

groupPptxTests :: [(TestTree, TestTree)] -> [TestTree]
groupPptxTests pairs =
  let (noRefs, refs) = unzip pairs
  in
    [ testGroup "Default slide formatting" noRefs
    , testGroup "With `--reference-doc` pptx file" refs
    ]


tests :: [TestTree]
tests = groupPptxTests [ pptxTests "Inline formatting"
                         def
                         "pptx/inline_formatting.native"
                         "pptx/inline_formatting.pptx"
                       , pptxTests "Slide breaks (default slide-level)"
                         def
                         "pptx/slide_breaks.native"
                         "pptx/slide_breaks.pptx"
                       , pptxTests "slide breaks (slide-level set to 1)"
                         def{ writerSlideLevel = Just 1 }
                         "pptx/slide_breaks.native"
                         "pptx/slide_breaks_slide_level_1.pptx"
                       , pptxTests "lists"
                         def
                         "pptx/lists.native"
                         "pptx/lists.pptx"
                       , pptxTests "tables"
                         def
                         "pptx/tables.native"
                         "pptx/tables.pptx"
                       , pptxTests "table of contents"
                         def{ writerTableOfContents = True }
                         "pptx/slide_breaks.native"
                         "pptx/slide_breaks_toc.pptx"
                       , pptxTests "end notes"
                         def
                         "pptx/endnotes.native"
                         "pptx/endnotes.pptx"
                       , pptxTests "end notes, with table of contents"
                         def { writerTableOfContents = True }
                         "pptx/endnotes.native"
                         "pptx/endnotes_toc.pptx"
                       , pptxTests "images"
                         def
                         "pptx/images.native"
                         "pptx/images.pptx"
                       , pptxTests "two-column layout"
                         def
                         "pptx/two_column.native"
                         "pptx/two_column.pptx"
                       , pptxTests "speaker notes"
                         def
                         "pptx/speaker_notes.native"
                         "pptx/speaker_notes.pptx"
                       , pptxTests "speaker notes after a separating block"
                         def
                         "pptx/speaker_notes_afterseps.native"
                         "pptx/speaker_notes_afterseps.pptx"
                       , pptxTests "remove empty slides"
                         def
                         "pptx/remove_empty_slides.native"
                         "pptx/remove_empty_slides.pptx"

                       ]
