{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Pptx.Slides
   Copyright   : © 2025 Anton Antic
   License     : GNU GPL, version 2 or above

   Maintainer  : Anton Antic <anton@everworker.ai>
   Stability   : alpha
   Portability : portable

Conversion of PPTX slides to Pandoc AST blocks.
-}
module Text.Pandoc.Readers.Pptx.Slides
  ( pptxToOutput
  ) where

import Codec.Archive.Zip (Archive)
import Data.List (find)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Readers.OOXML.Shared
import Text.Pandoc.Readers.Pptx.Parse
import Text.Pandoc.Readers.Pptx.Shapes
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.XML.Light

-- | Convert Pptx intermediate representation to Pandoc AST
pptxToOutput :: PandocMonad m => ReaderOptions -> Pptx -> m (Meta, [Block])
pptxToOutput _opts pptx = do
  let slides = pptxSlides pptx
      archive = pptxArchive pptx

  -- Convert each slide to blocks
  slideBlocks <- concat <$> mapM (slideToBlocks archive) slides

  return (mempty, slideBlocks)

-- | Convert slide to blocks
slideToBlocks :: PandocMonad m => Archive -> PptxSlide -> m [Block]
slideToBlocks archive slide = do
  let SlideId n = slideId slide
      slideElem = slideElement slide
      rels = slideRels slide
      ns = elemToNameSpaces slideElem

      -- Extract title from title placeholder
      title = extractSlideTitle ns slideElem

      -- Create header
      slideIdent = "slide-" <> T.pack (show n)
      headerText = if T.null title
                   then "Slide " <> T.pack (show n)
                   else title
      header = Header 2 (slideIdent, [], []) (B.toList (B.text headerText))

  -- Parse shapes and convert to blocks
  case findChildByName ns "p" "cSld" slideElem >>=
       findChildByName ns "p" "spTree" of
    Nothing -> return $ header : notesBlocks
    Just spTree -> do
      -- Filter out title placeholder shapes before parsing
      let allShapeElems = ungroupShapes ns (onlyElems $ elContent spTree)
          nonTitleShapeElems = filter (not . isTitlePlaceholder ns) allShapeElems
          shapes = mapMaybe (parseShape ns) nonTitleShapeElems
      shapeBlocks <- concat <$> mapM (shapeToBlocks archive rels) shapes
      return $ header : shapeBlocks ++ notesBlocks
  where
    notesBlocks = maybe [] notesToBlocks (slideNotes slide)

-- | Convert a notes slide (@p:notes@) to a @notes@ div, the form the pptx
-- writer turns into speaker notes. The notes text is in the shape holding
-- the body placeholder; the slide image and slide number placeholders are
-- ignored. Notes without text yield nothing.
notesToBlocks :: Element -> [Block]
notesToBlocks notesElem =
  case filter isBody shapeElems >>= textBoxBlocks of
    blocks | T.null (T.strip (stringify blocks)) -> []
           | otherwise -> [Div ("", ["notes"], []) blocks]
  where
    ns = elemToNameSpaces notesElem
    shapeElems = maybe [] elChildren $
      findChildByName ns "p" "cSld" notesElem >>=
      findChildByName ns "p" "spTree"
    isBody el =
      case findChildByName ns "p" "nvSpPr" el >>=
           findChildByName ns "p" "nvPr" >>=
           findChildByName ns "p" "ph" of
        Just ph -> findAttr (unqual "type") ph == Just "body"
        Nothing -> False
    textBoxBlocks el = case parseShape ns el of
      Just (PptxTextBox paras) -> paragraphsToBlocks paras
      _ -> []

-- | Replace each group shape (@p:grpSp@, possibly nested) by the shapes
-- it contains, keeping document order.
ungroupShapes :: NameSpaces -> [Element] -> [Element]
ungroupShapes ns = concatMap ungroup
  where
    ungroup el
      | isElem ns "p" "grpSp" el = ungroupShapes ns (elChildren el)
      | otherwise = [el]

-- | Extract title from title placeholder
extractSlideTitle :: NameSpaces -> Element -> Text
extractSlideTitle ns slideElem =
  case findChildByName ns "p" "cSld" slideElem >>=
       findChildByName ns "p" "spTree" of
    Nothing -> ""
    Just spTree ->
      -- Find shape with ph type="title"
      let shapes = onlyElems $ elContent spTree
          titleShape = find (isTitlePlaceholder ns) shapes
       in maybe "" extractDrawingMLText titleShape

-- isTitlePlaceholder is imported from Shapes module
