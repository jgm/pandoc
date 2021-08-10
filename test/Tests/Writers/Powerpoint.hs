module Tests.Writers.Powerpoint (tests) where

import Tests.Writers.OOXML (ooxmlTest)
import Text.Pandoc
import Test.Tasty
import System.FilePath
import Text.DocTemplates (ToContext(toVal), Context(..))
import qualified Data.Map as M
import Data.Text (pack)
import Data.List (unzip4)

-- templating is important enough, and can break enough things, that
-- we want to run all our tests with both default formatting and a
-- template.

modifyPptxName :: FilePath -> String -> FilePath
modifyPptxName fp suffix =
  addExtension (dropExtension fp ++ suffix) "pptx"

pptxTests :: String -> WriterOptions -> FilePath -> FilePath -> (TestTree, TestTree, TestTree, TestTree)
pptxTests name opts native pptx =
  let referenceDoc = "pptx/reference_depth.pptx"
      movedLayoutsReferenceDoc = "pptx/reference_moved_layouts.pptx"
      deletedLayoutsReferenceDoc = "pptx/reference_deleted_layouts.pptx"
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
      (modifyPptxName pptx "_templated")
    , ooxmlTest
      writePowerpoint
      name
      opts{writerReferenceDoc=Just movedLayoutsReferenceDoc}
      native
      (modifyPptxName pptx "_moved_layouts")
    , ooxmlTest
      writePowerpoint
      name
      opts{writerReferenceDoc=Just deletedLayoutsReferenceDoc}
      native
      (modifyPptxName pptx "_deleted_layouts")
    )

groupPptxTests :: [(TestTree, TestTree, TestTree, TestTree)] -> [TestTree]
groupPptxTests pairs =
  let (noRefs, refs, movedLayouts, deletedLayouts) = unzip4 pairs
  in
    [ testGroup "Default slide formatting" noRefs
    , testGroup "With `--reference-doc` pptx file" refs
    , testGroup "With layouts in reference doc moved" movedLayouts
    , testGroup "With layouts in reference doc deleted" deletedLayouts
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
                       , pptxTests "start ordered list at specified num"
                         def
                         "pptx/start_numbering_at.native"
                         "pptx/start_numbering_at.pptx"
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
                       , pptxTests "speaker notes after a separating header"
                         def
                         "pptx/speaker_notes_afterheader.native"
                         "pptx/speaker_notes_afterheader.pptx"
                       , pptxTests "speaker notes after metadata"
                         def
                         "pptx/speaker_notes_after_metadata.native"
                         "pptx/speaker_notes_after_metadata.pptx"
                       , pptxTests "remove empty slides"
                         def
                         "pptx/remove_empty_slides.native"
                         "pptx/remove_empty_slides.pptx"
                       , pptxTests "raw ooxml"
                         def
                         "pptx/raw_ooxml.native"
                         "pptx/raw_ooxml.pptx"
                       , pptxTests "metadata, custom properties"
                         def
                         "pptx/document-properties.native"
                         "pptx/document-properties.pptx"
                       , pptxTests "metadata, short description"
                         def
                         "pptx/document-properties-short-desc.native"
                         "pptx/document-properties-short-desc.pptx"
                       , pptxTests "inline code and code blocks"
                         def
                         "pptx/code.native"
                         "pptx/code.pptx"
                       , pptxTests "inline code and code blocks, custom formatting"
                         def { writerVariables = Context $ M.fromList
                                 [(pack "monofont", toVal $ pack "Consolas")] }
                         "pptx/code.native"
                         "pptx/code-custom.pptx"
                       ]
