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
  addExtension (takeDirectory fp ++ suffix) "pptx"

pptxTests :: String -> WriterOptions -> FilePath -> FilePath -> (TestTree, TestTree, TestTree, TestTree)
pptxTests name opts native pptx =
  let referenceDoc = "pptx/reference-depth.pptx"
      movedLayoutsReferenceDoc = "pptx/reference-moved-layouts.pptx"
      deletedLayoutsReferenceDoc = "pptx/reference-deleted-layouts.pptx"
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
      (modifyPptxName pptx "/templated")
    , ooxmlTest
      writePowerpoint
      name
      opts{writerReferenceDoc=Just movedLayoutsReferenceDoc}
      native
      (modifyPptxName pptx "/moved-layouts")
    , ooxmlTest
      writePowerpoint
      name
      opts{writerReferenceDoc=Just deletedLayoutsReferenceDoc}
      native
      (modifyPptxName pptx "/deleted-layouts")
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
                         "pptx/inline-formatting/input.native"
                         "pptx/inline-formatting/output.pptx"
                       , pptxTests "Slide breaks (default slide-level)"
                         def
                         "pptx/slide-breaks/input.native"
                         "pptx/slide-breaks/output.pptx"
                       , pptxTests "slide breaks (slide-level set to 1)"
                         def{ writerSlideLevel = Just 1 }
                         "pptx/slide-breaks/input.native"
                         "pptx/slide-breaks-slide-level-1/output.pptx"
                       , pptxTests "lists"
                         def
                         "pptx/lists/input.native"
                         "pptx/lists/output.pptx"
                       , pptxTests "start ordered list at specified num"
                         def
                         "pptx/start-numbering-at/input.native"
                         "pptx/start-numbering-at/output.pptx"
                       , pptxTests "tables"
                         def
                         "pptx/tables/input.native"
                         "pptx/tables/output.pptx"
                       , pptxTests "table of contents"
                         def{ writerTableOfContents = True }
                         "pptx/slide-breaks/input.native"
                         "pptx/slide-breaks-toc/output.pptx"
                       , pptxTests "end notes"
                         def
                         "pptx/endnotes/input.native"
                         "pptx/endnotes/output.pptx"
                       , pptxTests "end notes, with table of contents"
                         def { writerTableOfContents = True }
                         "pptx/endnotes/input.native"
                         "pptx/endnotes-toc/output.pptx"
                       , pptxTests "images"
                         def
                         "pptx/images/input.native"
                         "pptx/images/output.pptx"
                       , pptxTests "two-column layout"
                         def
                         "pptx/two-column/input.native"
                         "pptx/two-column/output.pptx"
                       , pptxTests "speaker notes"
                         def
                         "pptx/speaker-notes/input.native"
                         "pptx/speaker-notes/output.pptx"
                       , pptxTests "speaker notes after a separating block"
                         def
                         "pptx/speaker-notes-afterseps/input.native"
                         "pptx/speaker-notes-afterseps/output.pptx"
                       , pptxTests "speaker notes after a separating header"
                         def
                         "pptx/speaker-notes-afterheader/input.native"
                         "pptx/speaker-notes-afterheader/output.pptx"
                       , pptxTests "speaker notes after metadata"
                         def
                         "pptx/speaker-notes-after-metadata/input.native"
                         "pptx/speaker-notes-after-metadata/output.pptx"
                       , pptxTests "remove empty slides"
                         def
                         "pptx/remove-empty-slides/input.native"
                         "pptx/remove-empty-slides/output.pptx"
                       , pptxTests "raw ooxml"
                         def
                         "pptx/raw-ooxml/input.native"
                         "pptx/raw-ooxml/output.pptx"
                       , pptxTests "metadata, custom properties"
                         def
                         "pptx/document-properties/input.native"
                         "pptx/document-properties/output.pptx"
                       , pptxTests "metadata, short description"
                         def
                         "pptx/document-properties-short-desc/input.native"
                         "pptx/document-properties-short-desc/output.pptx"
                       , pptxTests "inline code and code blocks"
                         def
                         "pptx/code/input.native"
                         "pptx/code/output.pptx"
                       , pptxTests "inline code and code blocks, custom formatting"
                         def { writerVariables = Context $ M.fromList
                                 [(pack "monofont", toVal $ pack "Consolas")] }
                         "pptx/code/input.native"
                         "pptx/code-custom/output.pptx"
                       , pptxTests ("Using slide level 0, if the first thing on "
                                    <> "a slide is a h1 it's used as the "
                                    <> "slide title")
                         def { writerSlideLevel = Just 0 }
                         "pptx/slide-level-0/h1-with-image/input.native"
                         "pptx/slide-level-0/h1-with-image/output.pptx"
                       , pptxTests ("Using slide level 0, if the first thing on "
                                    <> "a slide is a h2 it's used as the "
                                    <> "slide title")
                         def { writerSlideLevel = Just 0 }
                         "pptx/slide-level-0/h2-with-image/input.native"
                         "pptx/slide-level-0/h2-with-image/output.pptx"
                       , pptxTests ("Using slide level 0, if the first thing on "
                                    <> "a slide is a heading it's used as the "
                                    <> "slide title (works with a table)")
                         def { writerSlideLevel = Just 0 }
                         "pptx/slide-level-0/h1-with-table/input.native"
                         "pptx/slide-level-0/h1-with-table/output.pptx"
                       , pptxTests ("Using slide level 0, if the first thing on "
                                    <> "a slide is a heading it's used as the "
                                    <> "slide title (two headings forces a "
                                    <> "slide break though)")
                         def { writerSlideLevel = Just 0 }
                         "pptx/slide-level-0/h1-h2-with-table/input.native"
                         "pptx/slide-level-0/h1-h2-with-table/output.pptx"
                       ]
