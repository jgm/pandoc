module Tests.Writers.Powerpoint (tests) where

import Control.Arrow ((***))
import Tests.Writers.OOXML (ooxmlTest)
import Text.Pandoc
import Test.Tasty
import System.FilePath
import Text.DocTemplates (ToContext(toVal), Context(..))
import qualified Data.Map as M
import Data.Text (pack)

-- templating is important enough, and can break enough things, that
-- we want to run all our tests with both default formatting and a
-- template.

modifyPptxName :: FilePath -> String -> FilePath
modifyPptxName fp suffix =
  addExtension (takeDirectory fp ++ suffix) "pptx"

pptxTests :: String -> WriterOptions -> FilePath -> FilePath -> (TestTree, TestTree)
pptxTests name opts native pptx =
  let referenceDoc = "pptx/reference-depth.pptx"
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
    )

groupPptxTests :: [(TestTree, TestTree)] -> [TestTree]
groupPptxTests pairs =
  let (noRefs, refs) = unzip pairs
  in
    [ testGroup "Default slide formatting" noRefs
    , testGroup "With `--reference-doc` pptx file" refs
    ]

testGroup' :: String -> [(TestTree, TestTree)] -> (TestTree, TestTree)
testGroup' descr = (testGroup descr *** testGroup descr) . unzip


tests :: [TestTree]
tests = let
  regularTests = groupPptxTests
    [ pptxTests "Inline formatting"
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
    , pptxTests "List continuation paragraph indentation"
      def
      "pptx/list-level/input.native"
      "pptx/list-level/output.pptx"
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
      "pptx/two-column/all-text/input.native"
      "pptx/two-column/all-text/output.pptx"
    , pptxTests "two-column (not comparison)"
      def
      "pptx/two-column/text-and-image/input.native"
      "pptx/two-column/text-and-image/output.pptx"
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
    , testGroup' "Using slide level 0, if the first thing on a slide is"
      [ pptxTests ("a h1 it's used as the slide title")
        def { writerSlideLevel = Just 0 }
        "pptx/slide-level-0/h1-with-image/input.native"
        "pptx/slide-level-0/h1-with-image/output.pptx"
      , pptxTests ("a h2 it's used as the "
                   <> "slide title")
        def { writerSlideLevel = Just 0 }
        "pptx/slide-level-0/h2-with-image/input.native"
        "pptx/slide-level-0/h2-with-image/output.pptx"
      , testGroup' "a heading it's used as the slide title"
        [ pptxTests "(works with a table)"
          def { writerSlideLevel = Just 0 }
          "pptx/slide-level-0/h1-with-table/input.native"
          "pptx/slide-level-0/h1-with-table/output.pptx"
        , pptxTests ("(content with caption layout)")
          def { writerSlideLevel = Just 0 }
          "pptx/slide-level-0/h1-h2-with-table/input.native"
          "pptx/slide-level-0/h1-h2-with-table/output.pptx"
        ]
      ]
    , testGroup' "comparison layout"
      [ testGroup' "comparison layout is used..."
        [ pptxTests "when two columns contain text + non-text"
          def
          "pptx/comparison/both-columns/input.native"
          "pptx/comparison/both-columns/output.pptx"
        , pptxTests "even when only one col contains text + non-text"
          def
          "pptx/comparison/one-column/input.native"
          "pptx/comparison/one-column/output.pptx"
        ]
      , testGroup' "extra ... in one column gets overlaid"
        [ pptxTests "text"
          def
          "pptx/comparison/extra-text/input.native"
          "pptx/comparison/extra-text/output.pptx"
        , pptxTests "image"
          def
          "pptx/comparison/extra-image/input.native"
          "pptx/comparison/extra-image/output.pptx"
        ]
      , pptxTests "is not used if the non-text comes first"
        def
        "pptx/comparison/non-text-first/input.native"
        "pptx/comparison/non-text-first/output.pptx"
      ]
    , testGroup' "Content with Caption layout is ..."
      [ pptxTests "used for heading, text, image on the same slide"
        def
        "pptx/content-with-caption/heading-text-image/input.native"
        "pptx/content-with-caption/heading-text-image/output.pptx"
      , pptxTests "used for text and an image on the same slide"
        def
        "pptx/content-with-caption/text-image/input.native"
        "pptx/content-with-caption/text-image/output.pptx"
      , pptxTests "not used if the image comes first"
        def
        "pptx/content-with-caption/image-text/input.native"
        "pptx/content-with-caption/image-text/output.pptx"
      ]
    , testGroup' "The Blank layout is used if a slide contains only..."
      [ pptxTests "speaker notes"
        def
        "pptx/blanks/just-speaker-notes/input.native"
        "pptx/blanks/just-speaker-notes/output.pptx"
      , pptxTests "an empty heading with a body of only NBSPs"
        def
        "pptx/blanks/nbsp-in-body/input.native"
        "pptx/blanks/nbsp-in-body/output.pptx"
      , pptxTests "a heading containing only non-breaking spaces"
        def
        "pptx/blanks/nbsp-in-heading/input.native"
        "pptx/blanks/nbsp-in-heading/output.pptx"
      ]
    , pptxTests ("Incremental lists are supported")
      def { writerIncremental = True }
      "pptx/incremental-lists/with-flag/input.native"
      "pptx/incremental-lists/with-flag/output.pptx"
    , pptxTests ("One-off incremental lists are supported")
      def
      "pptx/incremental-lists/without-flag/input.native"
      "pptx/incremental-lists/without-flag/output.pptx"
    , pptxTests "Background images"
      def
      "pptx/background-image/input.native"
      "pptx/background-image/output.pptx"
    ]
  referenceSpecificTests =
    [ ooxmlTest
      writePowerpoint
      "Basic footer"
      def { writerReferenceDoc = Just "pptx/footer/basic/reference.pptx"}
      "pptx/footer/input.native"
      "pptx/footer/basic/output.pptx"
    , ooxmlTest
      writePowerpoint
      "Footer with fixed date, replaced by meta block date"
      def { writerReferenceDoc = Just "pptx/footer/fixed-date/reference.pptx"}
      "pptx/footer/input.native"
      "pptx/footer/fixed-date/output.pptx"
    , ooxmlTest
      writePowerpoint
      "Footer not shown on title slide"
      def { writerReferenceDoc = Just "pptx/footer/no-title-slide/reference.pptx"}
      "pptx/footer/input.native"
      "pptx/footer/no-title-slide/output.pptx"
    , ooxmlTest
      writePowerpoint
      "Footer with slide number starting from 3"
      def { writerReferenceDoc = Just "pptx/footer/higher-slide-number/reference.pptx"}
      "pptx/footer/input.native"
      "pptx/footer/higher-slide-number/output.pptx"
    , ooxmlTest
      writePowerpoint
      "Layouts can be moved around in reference doc"
      def {writerReferenceDoc = Just "pptx/reference-moved-layouts.pptx"}
      "pptx/layouts/input.native"
      "pptx/layouts/moved.pptx"
    , ooxmlTest
      writePowerpoint
      "Layouts can be missing from the reference doc"
      def {writerReferenceDoc = Just "pptx/reference-deleted-layouts.pptx"}
      "pptx/layouts/input.native"
      "pptx/layouts/deleted.pptx"
    ]
  in regularTests <> referenceSpecificTests
