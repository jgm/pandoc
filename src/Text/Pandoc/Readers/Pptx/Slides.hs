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
import Control.Applicative ((<|>))
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Readers.OOXML.Shared
import Text.Pandoc.Readers.Pptx.Parse
import Text.Pandoc.Readers.Pptx.Shapes
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
      slideElem = resolveAlternateContent (slideElement slide)
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
    Nothing -> return [header]
    Just spTree -> do
      -- Filter out title placeholder shapes before parsing
      let allShapeElems = ungroupShapes ns (onlyElems $ elContent spTree)
          nonTitleShapeElems = filter (not . isTitlePlaceholder ns) allShapeElems
          shapes = mapMaybe (parseShape ns) nonTitleShapeElems
      shapeBlocks <- concat <$> mapM (shapeToBlocks archive rels) shapes
      return $ header : shapeBlocks

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

-- | Replace each markup-compatibility block (@mc:AlternateContent@) by
-- the content of its first @mc:Choice@ whose required namespaces the
-- reader understands, or else by its @mc:Fallback@. PowerPoint and the
-- pptx writer put the shapes holding equations (@a14:m@) in such blocks.
resolveAlternateContent :: Element -> Element
resolveAlternateContent = resolve mempty
  where
    resolve ns el =
      let ns' = elemToNameSpaces el <> ns
       in el{ elContent = concatMap (content ns') (elContent el) }
    content ns (Elem el)
      | isMce "AlternateContent" el =
          let ns' = elemToNameSpaces el <> ns
              branches = elChildren el
              chosen = find (understood ns') branches <|>
                       find (isMce "Fallback") branches
           in case chosen of
                Just branch ->
                  concatMap (content (elemToNameSpaces branch <> ns'))
                            (elContent branch)
                Nothing -> []
      | otherwise = [Elem (resolve ns el)]
    content _ c = [c]
    understood ns branch =
      isMce "Choice" branch &&
      all (\prefix -> M.lookup prefix (elemToNameSpaces branch <> ns)
                         `elem` map Just understoodNamespaces)
          (T.words $ fromMaybe "" $ findAttr (unqual "Requires") branch)
    isMce name el =
      qName (elName el) == name &&
      qURI (elName el) ==
        Just "http://schemas.openxmlformats.org/markup-compatibility/2006"
    understoodNamespaces =
      [ "http://schemas.microsoft.com/office/drawing/2010/main" ] -- a14
