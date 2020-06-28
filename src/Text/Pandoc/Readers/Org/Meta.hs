{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Org.Meta
   Copyright   : Copyright (C) 2014-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Parsers for Org-mode meta declarations.
-}
module Text.Pandoc.Readers.Org.Meta
  ( metaExport
  , metaKey
  , metaLine
  ) where

import Text.Pandoc.Readers.Org.BlockStarts
import Text.Pandoc.Readers.Org.ExportSettings (exportSettings)
import Text.Pandoc.Readers.Org.Inlines
import Text.Pandoc.Readers.Org.ParserState
import Text.Pandoc.Readers.Org.Parsing

import Text.Pandoc.Builder (Blocks, Inlines, ToMetaValue)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Shared (blocksToInlines, safeRead)

import Control.Monad (mzero, void)
import Data.List (intercalate, intersperse)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP (urlEncode)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Returns the current meta, respecting export options.
metaExport :: Monad m => OrgParser m (F Meta)
metaExport = do
  st <- getState
  let settings = orgStateExportSettings st
  return $ (if exportWithAuthor  settings then id else removeMeta "author")
         . (if exportWithCreator settings then id else removeMeta "creator")
         . (if exportWithEmail   settings then id else removeMeta "email")
        <$> orgStateMeta st

removeMeta :: Text -> Meta -> Meta
removeMeta key meta' =
  let metaMap = unMeta meta'
  in Meta $ Map.delete key metaMap

-- | Parse and handle a single line containing meta information
-- The order, in which blocks are tried, makes sure that we're not looking at
-- the beginning of a block, so we don't need to check for it
metaLine :: PandocMonad m => OrgParser m Blocks
metaLine = mempty <$ metaLineStart <* (optionLine <|> declarationLine)

declarationLine :: PandocMonad m => OrgParser m ()
declarationLine = try $ do
  key   <- T.toLower <$> metaKey
  case Map.lookup key exportSettingHandlers of
    Nothing -> () <$ anyLine
    Just hd -> hd

metaKey :: Monad m => OrgParser m Text
metaKey = T.toLower <$> many1Char (noneOf ": \n\r")
                    <*  char ':'
                    <*  skipSpaces

exportSettingHandlers :: PandocMonad m => Map Text (OrgParser m ())
exportSettingHandlers = Map.fromList
  [ ("result"     , fmap pure anyLine `parseThen` discard)        -- RESULT is never an export setting
  , ("author"     , lineOfInlines   `parseThen` collectLines "author")
  , ("keywords"   , lineOfInlines   `parseThen` collectLines "keywords")
  , ("date"       , lineOfInlines   `parseThen` setField "date")
  , ("description", lineOfInlines   `parseThen` collectLines "description")
  , ("title"      , lineOfInlines   `parseThen` collectLines "title")
  , ("nocite"     , lineOfInlines   `parseThen` collectAsList "nocite")
  , ("latex_class", fmap pure anyLine `parseThen` setField "documentclass")
  , ("latex_class_options", (pure . T.filter (`notElem` ("[]" :: String)) <$> anyLine)
                            `parseThen` setField "classoption")
  , ("latex_header", metaExportSnippet "latex" `parseThen`
                     collectAsList "header-includes")
  , ("html_head"  , metaExportSnippet "html" `parseThen`
                    collectAsList "header-includes")
  ]

parseThen :: PandocMonad m
          => OrgParser m (F a)
          -> (a -> Meta -> Meta)
          -> OrgParser m ()
parseThen p modMeta = do
  value <- p
  meta  <- orgStateMeta <$> getState
  updateState (\st -> st { orgStateMeta = modMeta <$> value <*> meta })

discard :: a -> Meta -> Meta
discard = const id

collectLines :: Text -> Inlines -> Meta -> Meta
collectLines key value meta =
  let value' = appendValue meta (B.toList value)
  in B.setMeta key value' meta
 where
  appendValue :: Meta -> [Inline] -> MetaValue
  appendValue m v = MetaInlines $ curInlines m <> v

  curInlines m = case collectInlines <$> lookupMeta key m of
    Nothing -> []
    Just [] -> []
    Just xs -> xs <> [B.SoftBreak]

  collectInlines :: MetaValue -> [Inline]
  collectInlines = \case
    MetaInlines inlns -> inlns
    MetaList ml       -> intercalate [B.SoftBreak] $ map collectInlines ml
    MetaString s      -> [B.Str s]
    MetaBlocks blks   -> blocksToInlines blks
    MetaMap _map      -> []
    MetaBool _bool    -> []

-- | Accumulate the result as a MetaList under the given key.
collectAsList :: Text -> Inlines -> Meta -> Meta
collectAsList key value meta =
  let value' = metaListAppend meta (B.toMetaValue value)
  in B.setMeta key value' meta
 where
  metaListAppend m v = MetaList (curList m ++ [v])
  curList m = case lookupMeta key m of
                Just (MetaList ms) -> ms
                Just x             -> [x]
                _                  -> []

setField :: ToMetaValue a => Text -> a -> Meta -> Meta
setField field value meta = B.setMeta field (B.toMetaValue value) meta

-- | Read an format specific meta definition
metaExportSnippet :: Monad m => Text -> OrgParser m (F Inlines)
metaExportSnippet format = pure . B.rawInline format <$> anyLine

--
-- export options
--
optionLine :: PandocMonad m => OrgParser m ()
optionLine = try $ do
  key <- metaKey
  case key of
    "link"     -> parseLinkFormat >>= uncurry addLinkFormat
    "options"  -> exportSettings
    "todo"     -> todoSequence >>= updateState . registerTodoSequence
    "seq_todo" -> todoSequence >>= updateState . registerTodoSequence
    "typ_todo" -> todoSequence >>= updateState . registerTodoSequence
    "macro"    -> macroDefinition >>= updateState . registerMacro
    "exclude_tags" -> tagList >>= updateState . setExcludedTags
    "select_tags" -> tagList >>= updateState . setSelectedTags
    "pandoc-emphasis-pre" -> emphChars >>= updateState . setEmphasisPreChar
    "pandoc-emphasis-post" -> emphChars >>= updateState . setEmphasisPostChar
    _          -> mzero

addLinkFormat :: Monad m => Text
              -> (Text -> Text)
              -> OrgParser m ()
addLinkFormat key formatter = updateState $ \s ->
  let fs = orgStateLinkFormatters s
  in s{ orgStateLinkFormatters = Map.insert key formatter fs }

parseLinkFormat :: Monad m => OrgParser m (Text, Text -> Text)
parseLinkFormat = try $ do
  linkType <- T.cons <$> letter <*> manyChar (alphaNum <|> oneOf "-_") <* skipSpaces
  linkSubst <- parseFormat
  return (linkType, linkSubst)

-- | An ad-hoc, single-argument-only implementation of a printf-style format
-- parser.
parseFormat :: Monad m => OrgParser m (Text -> Text)
parseFormat = try $ replacePlain <|> replaceUrl <|> justAppend
 where
   -- inefficient, but who cares
   replacePlain = try $ (\x -> T.concat . flip intersperse x)
                     <$> sequence [tillSpecifier 's', rest]
   replaceUrl   = try $ (\x -> T.concat . flip intersperse x . T.pack . urlEncode . T.unpack)
                     <$> sequence [tillSpecifier 'h', rest]
   justAppend   = try $ (<>) <$> rest

   rest            = manyTillChar anyChar         (eof <|> () <$ oneOf "\n\r")
   tillSpecifier c = manyTillChar (noneOf "\n\r") (try $ string ('%':c:""))

tagList :: Monad m => OrgParser m [Tag]
tagList = do
  skipSpaces
  map Tag <$> many (orgTagWord <* skipSpaces) <* newline

setExcludedTags :: [Tag] -> OrgParserState -> OrgParserState
setExcludedTags tags st =
  let finalSet = if orgStateExcludeTagsChanged st
                   then foldr Set.insert (orgStateExcludeTags st) tags
                   else Set.fromList tags
  in st { orgStateExcludeTags = finalSet, orgStateExcludeTagsChanged = True }

setSelectedTags :: [Tag] -> OrgParserState -> OrgParserState
setSelectedTags tags st =
  let finalSet = if orgStateSelectTagsChanged st
                   then foldr Set.insert (orgStateSelectTags st) tags
                   else Set.fromList tags
  in st { orgStateSelectTags = finalSet, orgStateSelectTagsChanged = True }

setEmphasisPreChar :: Maybe [Char] -> OrgParserState -> OrgParserState
setEmphasisPreChar csMb st =
  let preChars = fromMaybe (orgStateEmphasisPostChars defaultOrgParserState) csMb
  in st { orgStateEmphasisPreChars = preChars }

setEmphasisPostChar :: Maybe [Char] -> OrgParserState -> OrgParserState
setEmphasisPostChar csMb st =
  let postChars = fromMaybe (orgStateEmphasisPostChars defaultOrgParserState) csMb
  in st { orgStateEmphasisPostChars = postChars }

emphChars :: Monad m => OrgParser m (Maybe [Char])
emphChars = do
  skipSpaces
  safeRead <$> anyLine

lineOfInlines :: PandocMonad m => OrgParser m (F Inlines)
lineOfInlines = do
  updateLastPreCharPos
  trimInlinesF . mconcat <$> manyTill inline newline

--
-- ToDo Sequences and Keywords
--
todoSequence :: Monad m => OrgParser m TodoSequence
todoSequence = try $ do
  todoKws <- todoKeywords
  doneKws <- optionMaybe $ todoDoneSep *> todoKeywords
  newline
  -- There must be at least one DONE keyword. The last TODO keyword is taken if
  -- necessary.
  case doneKws of
    Just done  -> return $ keywordsToSequence todoKws done
    Nothing    -> case reverse todoKws of
                    []     -> mzero  -- no keywords present
                    (x:xs) -> return $ keywordsToSequence (reverse xs) [x]

 where
   todoKeywords :: Monad m => OrgParser m [Text]
   todoKeywords = try $
     let keyword = many1Char nonspaceChar <* skipSpaces
         endOfKeywords = todoDoneSep <|> void newline
     in manyTill keyword (lookAhead endOfKeywords)

   todoDoneSep :: Monad m => OrgParser m ()
   todoDoneSep = void . try $ skipSpaces *> char '|' <* skipSpaces1

   keywordsToSequence :: [Text] -> [Text] -> TodoSequence
   keywordsToSequence todo done =
     let todoMarkers = map (TodoMarker Todo) todo
         doneMarkers = map (TodoMarker Done) done
     in todoMarkers ++ doneMarkers

macroDefinition :: Monad m => OrgParser m (Text, [Text] -> Text)
macroDefinition = try $ do
  macroName <- many1Char nonspaceChar <* skipSpaces
  firstPart <- expansionPart
  (elemOrder, parts) <- unzip <$> many ((,) <$> placeholder <*> expansionPart)
  let expander = mconcat . alternate (firstPart:parts) . reorder elemOrder
  return (macroName, expander)
 where
  placeholder :: Monad m => OrgParser m Int
  placeholder = try . fmap (fromMaybe 1 . safeRead) $ char '$' *> many1Char digit

  expansionPart :: Monad m => OrgParser m Text
  expansionPart = try $ manyChar (notFollowedBy placeholder *> noneOf "\n\r")

  alternate :: [a] -> [a] -> [a]
  alternate []     ys     = ys
  alternate xs     []     = xs
  alternate (x:xs) (y:ys) = x : y : alternate xs ys

  reorder :: [Int] -> [Text] -> [Text]
  reorder perm xs =
    let element n = take 1 $ drop (n - 1) xs
    in concatMap element perm
