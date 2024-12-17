{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Writers.XML (writeXML) where
-- import Text.Pandoc
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options ( WriterOptions(..))
import qualified Data.Text as T
import Text.XML.Light

-- | Convert Pandoc document to string in ICML format.
writeXML :: PandocMonad m => WriterOptions -> Pandoc -> m T.Text
writeXML _ doc = return $ T.pack $ showElement $ docToXML doc

docToXML :: Pandoc -> Element
docToXML (Pandoc meta blocks) = unode "Pandoc" [ (metaElement meta), (blocksElement blocks) ]

metaElement :: Meta -> Element
metaElement (meta) = unode "meta" ([] :: [Content]) -- TODO: convert single meta values

blocksElement :: [Block] -> Element
blocksElement (blocks) = unode "blocks" (map blockToXML blocks)

mkCData :: T.Text -> [Content]
mkCData text = [Elem $ blank_element { elContent = [CRef $ T.unpack text] }]

textToCData :: T.Text -> CData
textToCData text = blank_cdata { cdData = T.unpack text }

extractFormatName :: Format -> String
extractFormatName (Format fmt) = T.unpack fmt 

blockToXML :: Block -> Element
blockToXML blk = case blk of
  Para inlines -> unode "Para" (map inlineToContent inlines)
  Plain inlines -> unode "Plain" (map inlineToContent inlines)
  Header _ _ inlines -> unode "Header" (map inlineToContent inlines)
  CodeBlock _ text -> unode "CodeBlock" (mkCData text)
  RawBlock format text -> unode "RawBlock" ([rawAttr format], (textToCData text))
  _ -> unode "Unknown" ()

inlineToContent :: Inline -> Content
inlineToContent inl = case inl of
  Space -> Text $ textToCData " "
  Str text -> Text $ textToCData text
  Emph inlines -> Elem $ unode "Emph" (map inlineToContent inlines)
  Strong inlines -> Elem $ unode "Strong" (map inlineToContent inlines)
  SmallCaps inlines -> Elem $ unode "SmallCaps" (map inlineToContent inlines)
  Superscript inlines -> Elem $ unode "Superscript" (map inlineToContent inlines)
  Subscript inlines -> Elem $ unode "Subscript" (map inlineToContent inlines)
  Underline inlines -> Elem $ unode "Underline" (map inlineToContent inlines)
  Code _ text -> Elem $ unode "Code" (textToCData text)
  RawInline format text -> Elem $ unode "RawInline" ([rawAttr format], (textToCData text))
  _ -> Elem $ unode "Unknown" ()

rawAttr :: Format -> Text.XML.Light.Attr
rawAttr format = Attr (unqual "format") (extractFormatName format)