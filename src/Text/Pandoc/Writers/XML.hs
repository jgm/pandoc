{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Writers.XML (writeXML) where

-- import Text.Pandoc

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isJust)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
<<<<<<< HEAD
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
=======
import Text.Pandoc.Options (WriterOptions (..))
import Text.XML.Light

-- | Convert Pandoc document to string in ICML format.
writeXML :: (PandocMonad m) => WriterOptions -> Pandoc -> m T.Text
writeXML _ doc = do
  let value = toJSON doc
  return $ T.pack $ showTopElement $ valueToXML value

valueToXML :: Value -> Element
valueToXML value =
  if isPandocObject value
    then
      Element
        { elName = unqual "Pandoc", -- Tag name
          elAttribs = [Attr (unqual "class") "my-class"], -- Attributes
          elContent = [Text (CData CDataText "Hello, World!" Nothing)], -- Child content
          elLine = Nothing -- Line number (optional, usually `Nothing`)
        }
    else
      Element
        { elName = unqual "ERROR",
          elAttribs = [], -- Attributes
          elContent = [], -- Child content
          elLine = Nothing -- Line number (optional, usually `Nothing`)
        }

objectHasKey :: T.Text -> Value -> Bool
objectHasKey key (Object obj) = isJust (KM.lookup (K.fromText key) obj)
objectHasKey _ _ = False

isPandocObject :: Value -> Bool
isPandocObject value =
  (objectHasKey "blocks" value)
    && (objectHasKey "meta" value)
    && (objectHasKey "pandoc-api-version" value)

objectHasType :: Value -> Bool
objectHasType value = objectHasKey "t" value

objectHasContent :: Value -> Bool
objectHasContent value = objectHasKey "c" value

objectHasTypeAndContent :: Value -> Bool
objectHasTypeAndContent value = (objectHasContent value) && (objectHasType value)

-- Element "Pandoc" [] [] Nothing
-- jsonToXML (Object obj) =
--     let tVal = obj .: "t"
--         cVal = obj .: "c"
--     in case tVal of
--         Just (String "Space") -> Element "Space" mempty [NodeContent " "]
--         Just (String "Str") -> Element "Str" mempty [NodeContent (extractText cVal)]
--         Just (String "Emph") -> Element "Emph" mempty (convertChildren cVal)
--         _ -> Element "Unknown" mempty []  -- Fallback for unsupported types
-- jsonToXML _ = Element "Invalid" mempty []

-- Helper to extract Text content (assuming cVal is an array)
-- extractText :: Maybe Value -> T.Text
-- extractText (Just (String text)) = text
-- extractText _ = T.empty

-- -- Handle child nodes (e.g., arrays of Inline or Block)
-- convertChildren :: Maybe Value -> [Text.XML.Light.Element]
-- convertChildren (Just (Array arr)) = map jsonToXML (V.toList arr)
-- convertChildren _ = []

-- -- Check if a Value is a Pandoc Object with "blocks", "pandoc-api-version", and "meta" keys
-- isPandocObject :: Value -> Bool
-- isPandocObject = withObject "PandocObject" $ \obj -> do
--     _ <- obj .: "blocks"             -- Access "blocks"
--     _ <- obj .: "pandoc-api-version" -- Access "pandoc-api-version"
--     _ <- obj .: "meta"               -- Access "meta"
--     return True

-- -- Extract the fields for "blocks", "pandoc-api-version", and "meta" using the `Parser` monad
-- extractPandocFields :: Value -> Parser (Value, Value, Value)
-- extractPandocFields = withObject "PandocObject" $ \obj -> do
--     blocks <- obj .: "blocks"
--     version <- obj .: "pandoc-api-version"
--     meta <- obj .: "meta"
--     return (blocks, version, meta)
>>>>>>> d64d68a92 (indentify a Pandoc Object)

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

<<<<<<< HEAD
rawAttr :: Format -> Text.XML.Light.Attr
rawAttr format = Attr (unqual "format") (extractFormatName format)
=======
-- blocksElement :: [Block] -> Element
-- blocksElement (blocks) = unode "blocks" (map blockToXML blocks)

-- mkCData :: T.Text -> [Content]
-- mkCData text = [Elem $ blank_element { elContent = [CRef $ T.unpack text] }]

-- textToCData :: T.Text -> CData
-- textToCData text = blank_cdata { cdData = T.unpack text }

-- extractFormatName :: Format -> String
-- extractFormatName (Format fmt) = T.unpack fmt

-- blockToXML :: Block -> Element
-- blockToXML blk = case blk of
--   Para inlines -> unode "Para" (map inlineToContent inlines)
--   Plain inlines -> unode "Plain" (map inlineToContent inlines)
--   Header _ _ inlines -> unode "Header" (map inlineToContent inlines)
--   CodeBlock _ text -> unode "CodeBlock" (mkCData text)
--   RawBlock format text -> unode "RawBlock" ([rawAttr format], (textToCData text))
--   _ -> unode "Unknown" ()

-- inlineToContent :: Inline -> Content
-- inlineToContent inl = case inl of
--   Space -> Text $ textToCData " "
--   Str text -> Text $ textToCData text
--   Emph inlines -> Elem $ unode "Emph" (map inlineToContent inlines)
--   Strong inlines -> Elem $ unode "Strong" (map inlineToContent inlines)
--   SmallCaps inlines -> Elem $ unode "SmallCaps" (map inlineToContent inlines)
--   Superscript inlines -> Elem $ unode "Superscript" (map inlineToContent inlines)
--   Subscript inlines -> Elem $ unode "Subscript" (map inlineToContent inlines)
--   Underline inlines -> Elem $ unode "Underline" (map inlineToContent inlines)
--   Code _ text -> Elem $ unode "Code" (textToCData text)
--   RawInline format text -> Elem $ unode "RawInline" ([rawAttr format], (textToCData text))
--   _ -> Elem $ unode "Unknown" ()

-- rawAttr :: Format -> Text.XML.Light.Attr
-- rawAttr format = Attr (unqual "format") (extractFormatName format)
>>>>>>> d64d68a92 (indentify a Pandoc Object)
