{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Text.Pandoc.Readers.XML (readXML) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (StateT (runStateT), gets, modify)
import Data.Char (isDigit, isSpace)
import Data.Default (Default (..))
import Data.Generics
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set ((\\))
import qualified Data.Set as S (fromList, member)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import Data.Version (Version, makeVersion, versionBranch)
import Text.Pandoc.Builder
import Text.Pandoc.Class.PandocMonad
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing (ToSources, toSources, unexpected)
import Text.Pandoc.Shared (extractSpaces)
import Text.Pandoc.Sources (sourcesToText)
import Text.Pandoc.Version (pandocVersion)
import Text.Pandoc.XML (lookupEntity)
import Text.Pandoc.XML.Light

type XMLReader m = StateT XMLReaderState m

data XMLReaderState = XMLReaderState
  { xmlApiVersion :: Version,
    xmlMeta :: Meta,
    xmlContent :: [Content]
  }
  deriving (Show)

instance Default XMLReaderState where
  def =
    XMLReaderState
      { xmlApiVersion = pandocVersion,
        xmlMeta = mempty,
        xmlContent = []
      }

readXML :: (PandocMonad m, ToSources a) => ReaderOptions -> a -> m Pandoc
readXML _ inp = do
  let sources = toSources inp
  tree <-
    either (throwError . PandocXMLError "") return $
      parseXMLContents (fromStrict . sourcesToText $ sources)
  (bs, st') <- flip runStateT (def {xmlContent = tree}) $ mapM (parseBlock "root") tree
  let blockList = toList $ mconcat bs
  return $ Pandoc (xmlMeta st') blockList

getBlocks :: (PandocMonad m) => Text -> Element -> XMLReader m Blocks
getBlocks parent e =
  mconcat
    <$> mapM (parseBlock parent) (elContent e)

attrValue :: Text -> Element -> Text
attrValue attr =
  fromMaybe "" . maybeAttrValue attr

maybeAttrValue :: Text -> Element -> Maybe Text
maybeAttrValue attr elt =
  lookupAttrBy (\x -> qName x == attr) (elAttribs elt)

reportUnexpected :: (PandocMonad m, Eq b) => Text -> Text -> Text -> b -> XMLReader m b
reportUnexpected what context parent whatever = do
  report $ UnexpectedXmlElement (what <> " \"" <> context <> "\"") parent
  return whatever

parseBlock :: (PandocMonad m) => Text -> Content -> XMLReader m Blocks
parseBlock _ (Text (CData CDataRaw _ _)) = return mempty -- DOCTYPE
parseBlock parent (Text (CData _ s _)) =
  if T.all isSpace s
    then return mempty
    else reportUnexpected "text" s parent mempty
-- parseBlock (CRef x) = return $ plain $ str $ T.toUpper x
parseBlock parent (CRef x) = reportUnexpected "reference" x parent mempty
parseBlock parent (Elem e) = do
  let name = qName $ elName e
  -- modify $ \st -> st {xmlContextElement = name, xmlContextParent = parent}
  case (name) of
    "Pandoc" -> parsePandoc
    "?xml" -> return mempty
    "blocks" -> getBlocks name e
    "Para" -> parseMixed para (elContent e)
    "Plain" -> parseMixed plain (elContent e)
    _ -> reportUnexpected "element" name parent mempty
  where
    parsePandoc = do
      let version = maybeAttrValue "api-version" e
          apiversion = case (version) of
            Just (v) -> makeVersion $ map (read . T.unpack) $ T.splitOn "," v
            Nothing -> pandocVersion
       in modify $ \st -> st {xmlApiVersion = apiversion}
      getBlocks "Pandoc" e

    parseMixed container conts = do
      let (ils, rest) = break isBlockElement conts
      ils' <- trimInlines . mconcat <$> mapM parseInline ils
      let p = if ils' == mempty then mempty else container ils'
      case rest of
        [] -> return p
        (r : rs) -> do
          b <- parseBlock parent r
          x <- parseMixed container rs
          return $ p <> b <> x

isBlockElement :: Content -> Bool
isBlockElement (Elem e) = qName (elName e) `S.member` blocktags
  where
    blocktags = S.fromList (paragraphLevel ++ lists ++ formulae ++ other) \\ S.fromList canBeInline
    paragraphLevel = ["Div", "Para", "Plain", "Header"]
    lists = ["BulletList", "DefinitionList", "OrderedList"]
    formulae = ["tex-math", "mml:math"]
    other =
      [ "p",
        "related-article",
        "related-object",
        "ack",
        "disp-quote",
        "speech",
        "statement",
        "verse-group",
        "x"
      ]
    canBeInline = ["tex-math", "mml:math", "related-object", "x"]
-- onlyOneChild x = length (allChildren x) == 1
-- allChildren x = filterChildren (const True) x

isBlockElement _ = False

strContentRecursive :: Element -> Text
strContentRecursive =
  strContent
    . (\e' -> e' {elContent = map elementToStr $ elContent e'})

elementToStr :: Content -> Content
elementToStr (Elem e') = Text $ CData CDataText (strContentRecursive e') Nothing
elementToStr x = x

parseInline :: (PandocMonad m) => Content -> XMLReader m Inlines
parseInline (Text (CData _ s _)) = return $ text s
parseInline (CRef ref) =
  return $
    maybe (text $ T.toUpper ref) text $
      lookupEntity ref
parseInline (Elem e) =
  case qName (elName e) of
    "Emph" -> innerInlines emph
    "Strong" -> innerInlines strong
    "Strikeout" -> innerInlines strikeout
    "Subscript" -> innerInlines subscript
    "Superscript" -> innerInlines superscript
    "Underline" -> innerInlines underline
    "LineBreak" -> return linebreak
    "SmallCaps" -> innerInlines smallcaps
    "Code" -> codeWithLang
    -- "monospace" -> codeWithLang
    -- "inline-graphic" -> getGraphic e
    _ -> innerInlines id
  where
    innerInlines f =
      extractSpaces f . mconcat
        <$> mapM parseInline (elContent e)
    codeWithLang = do
      let classes' = case attrValue "language" e of
            "" -> []
            l -> [l]
      return $ codeWith (attrValue "id" e, classes', []) $ strContentRecursive e
