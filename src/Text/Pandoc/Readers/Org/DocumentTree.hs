{-
Copyright (C) 2014-2018 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{- |
   Module      : Text.Pandoc.Readers.Org.DocumentTree
   Copyright   : Copyright (C) 2014-2018 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Parsers for org-mode headlines and document subtrees
-}
module Text.Pandoc.Readers.Org.DocumentTree
  ( documentTree
  , headlineToBlocks
  ) where

import Prelude
import Control.Arrow ((***))
import Control.Monad (guard, void)
import Data.Char (toLower, toUpper)
import Data.List (intersperse)
import Text.Pandoc.Builder (Blocks, Inlines)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Readers.Org.BlockStarts
import Text.Pandoc.Readers.Org.ParserState
import Text.Pandoc.Readers.Org.Parsing

import qualified Data.Set as Set
import qualified Text.Pandoc.Builder as B

--
-- Org headers
--

-- | Parse input as org document tree.
documentTree :: PandocMonad m
             => OrgParser m (F Blocks)
             -> OrgParser m (F Inlines)
             -> OrgParser m (F Headline)
documentTree blocks inline = do
  initialBlocks <- blocks
  headlines <- sequence <$> manyTill (headline blocks inline 1) eof
  title <- fmap docTitle . orgStateMeta <$> getState
  return $ do
    headlines' <- headlines
    initialBlocks' <- initialBlocks
    title' <- title
    return Headline
      { headlineLevel = 0
      , headlineTodoMarker = Nothing
      , headlineText = B.fromList title'
      , headlineTags = mempty
      , headlinePlanning = emptyPlanning
      , headlineProperties = mempty
      , headlineContents = initialBlocks'
      , headlineChildren = headlines'
      }

-- | Create a tag containing the given string.
toTag :: String -> Tag
toTag = Tag

-- | The key (also called name or type) of a property.
newtype PropertyKey = PropertyKey { fromKey :: String }
  deriving (Show, Eq, Ord)

-- | Create a property key containing the given string.  Org mode keys are
-- case insensitive and are hence converted to lower case.
toPropertyKey :: String -> PropertyKey
toPropertyKey = PropertyKey . map toLower

-- | The value assigned to a property.
newtype PropertyValue = PropertyValue { fromValue :: String }

-- | Create a property value containing the given string.
toPropertyValue :: String -> PropertyValue
toPropertyValue = PropertyValue

-- | Check whether the property value is non-nil (i.e. truish).
isNonNil :: PropertyValue -> Bool
isNonNil p = map toLower (fromValue p) `notElem` ["()", "{}", "nil"]

-- | Key/value pairs from a PROPERTIES drawer
type Properties = [(PropertyKey, PropertyValue)]

-- | Org mode headline (i.e. a document subtree).
data Headline = Headline
  { headlineLevel      :: Int
  , headlineTodoMarker :: Maybe TodoMarker
  , headlineText       :: Inlines
  , headlineTags       :: [Tag]
  , headlinePlanning   :: PlanningInfo -- ^ subtree planning information
  , headlineProperties :: Properties
  , headlineContents   :: Blocks
  , headlineChildren   :: [Headline]
  }

-- | Read an Org mode headline and its contents (i.e. a document subtree).
-- @lvl@ gives the minimum acceptable level of the tree.
headline :: PandocMonad m
         => OrgParser m (F Blocks)
         -> OrgParser m (F Inlines)
         -> Int
         -> OrgParser m (F Headline)
headline blocks inline lvl = try $ do
  level <- headerStart
  guard (lvl <= level)
  todoKw <- optionMaybe todoKeyword
  title <- trimInlinesF . mconcat <$> manyTill inline endOfTitle
  tags  <- option [] headerTags
  newline
  planning   <- option emptyPlanning planningInfo
  properties <- option mempty propertiesDrawer
  contents   <- blocks
  children   <- many (headline blocks inline (level + 1))
  return $ do
    title'    <- title
    contents' <- contents
    children' <- sequence children
    return Headline
      { headlineLevel = level
      , headlineTodoMarker = todoKw
      , headlineText = title'
      , headlineTags = tags
      , headlinePlanning = planning
      , headlineProperties = properties
      , headlineContents = contents'
      , headlineChildren = children'
      }
 where
   endOfTitle :: Monad m => OrgParser m ()
   endOfTitle = void . lookAhead $ optional headerTags *> newline

   headerTags :: Monad m => OrgParser m [Tag]
   headerTags = try $
     let tag = orgTagWord <* char ':'
     in map toTag <$> (skipSpaces *> char ':' *> many1 tag <* skipSpaces)

-- | Convert an Org mode headline (i.e. a document tree) into pandoc's Blocks
headlineToBlocks :: Monad m => Headline -> OrgParser m Blocks
headlineToBlocks hdln = do
  maxLevel <- getExportSetting exportHeadlineLevels
  let tags = headlineTags hdln
  let text = headlineText hdln
  let level = headlineLevel hdln
  shouldNotExport <- hasDoNotExportTag tags
  case () of
    _ | shouldNotExport -> return mempty
    _ | any isArchiveTag  tags -> archivedHeadlineToBlocks hdln
    _ | isCommentTitle text    -> return mempty
    _ | maxLevel <= level      -> headlineToHeaderWithList hdln
    _ | otherwise              -> headlineToHeaderWithContents hdln

hasDoNotExportTag :: Monad m => [Tag] -> OrgParser m Bool
hasDoNotExportTag tags = containsExcludedTag . orgStateExcludedTags <$> getState
  where containsExcludedTag s = any (`Set.member` s) tags

isArchiveTag :: Tag -> Bool
isArchiveTag = (== toTag "ARCHIVE")

-- | Check if the title starts with COMMENT.
-- FIXME: This accesses builder internals not intended for use in situations
-- like these.  Replace once keyword parsing is supported.
isCommentTitle :: Inlines -> Bool
isCommentTitle inlns = case B.toList inlns of
  (Str "COMMENT":_) -> True
  _ -> False

archivedHeadlineToBlocks :: Monad m => Headline -> OrgParser m Blocks
archivedHeadlineToBlocks hdln = do
  archivedTreesOption <- getExportSetting exportArchivedTrees
  case archivedTreesOption of
    ArchivedTreesNoExport     -> return mempty
    ArchivedTreesExport       -> headlineToHeaderWithContents hdln
    ArchivedTreesHeadlineOnly -> headlineToHeader hdln

headlineToHeaderWithList :: Monad m => Headline -> OrgParser m Blocks
headlineToHeaderWithList hdln = do
  maxHeadlineLevels <- getExportSetting exportHeadlineLevels
  header        <- headlineToHeader hdln
  listElements  <- mapM headlineToBlocks (headlineChildren hdln)
  planningBlock <- planningToBlock (headlinePlanning hdln)
  let listBlock  = if null listElements
                   then mempty
                   else B.orderedList listElements
  let headerText = if maxHeadlineLevels == headlineLevel hdln
                   then header
                   else flattenHeader header
  return . mconcat $
    [ headerText
    , headlineContents hdln
    , planningBlock
    , listBlock
    ]
 where
   flattenHeader :: Blocks -> Blocks
   flattenHeader blks =
     case B.toList blks of
       (Header _ _ inlns:_) -> B.para (B.fromList inlns)
       _                    -> mempty

headlineToHeaderWithContents :: Monad m => Headline -> OrgParser m Blocks
headlineToHeaderWithContents hdln = do
  header         <- headlineToHeader hdln
  planningBlock <- planningToBlock (headlinePlanning hdln)
  childrenBlocks <- mconcat <$> mapM headlineToBlocks (headlineChildren hdln)
  return $ header <> planningBlock <> headlineContents hdln <> childrenBlocks

headlineToHeader :: Monad m => Headline -> OrgParser m Blocks
headlineToHeader hdln = do
  exportTodoKeyword <- getExportSetting exportWithTodoKeywords
  exportTags        <- getExportSetting exportWithTags
  let todoText    = if exportTodoKeyword
                    then case headlineTodoMarker hdln of
                      Just kw -> todoKeywordToInlines kw <> B.space
                      Nothing -> mempty
                    else mempty
  let text        = todoText <> headlineText hdln <>
                    if exportTags
                    then tagsToInlines (headlineTags hdln)
                    else mempty
  let propAttr    = propertiesToAttr (headlineProperties hdln)
  attr           <- registerHeader propAttr (headlineText hdln)
  return $ B.headerWith attr (headlineLevel hdln) text

todoKeyword :: Monad m => OrgParser m TodoMarker
todoKeyword = try $ do
  taskStates <- activeTodoMarkers <$> getState
  let kwParser tdm = try (tdm <$ string (todoMarkerName tdm) <* spaceChar)
  choice (map kwParser taskStates)

todoKeywordToInlines :: TodoMarker -> Inlines
todoKeywordToInlines tdm =
  let todoText  = todoMarkerName tdm
      todoState = map toLower . show $ todoMarkerState tdm
      classes = [todoState, todoText]
  in B.spanWith (mempty, classes, mempty) (B.str todoText)

propertiesToAttr :: Properties -> Attr
propertiesToAttr properties =
  let
    toStringPair = fromKey *** fromValue
    customIdKey = toPropertyKey "custom_id"
    classKey    = toPropertyKey "class"
    unnumberedKey = toPropertyKey "unnumbered"
    specialProperties = [customIdKey, classKey, unnumberedKey]
    id'  = maybe mempty fromValue . lookup customIdKey $ properties
    cls  = maybe mempty fromValue . lookup classKey    $ properties
    kvs' = map toStringPair . filter ((`notElem` specialProperties) . fst)
           $ properties
    isUnnumbered =
      maybe False isNonNil . lookup unnumberedKey $ properties
  in
    (id', words cls ++ ["unnumbered" | isUnnumbered], kvs')

tagsToInlines :: [Tag] -> Inlines
tagsToInlines [] = mempty
tagsToInlines tags =
  (B.space <>) . mconcat . intersperse (B.str "\160") . map tagToInline $ tags
 where
  tagToInline :: Tag -> Inlines
  tagToInline t = tagSpan t . B.smallcaps . B.str $ fromTag t

-- | Wrap the given inline in a span, marking it as a tag.
tagSpan :: Tag -> Inlines -> Inlines
tagSpan t = B.spanWith ("", ["tag"], [("tag-name", fromTag t)])

-- | Render planning info as a block iff the respective export setting is
-- enabled.
planningToBlock :: Monad m => PlanningInfo -> OrgParser m Blocks
planningToBlock planning = do
  includePlanning <- getExportSetting exportWithPlanning
  return $
    if includePlanning
    then B.plain . mconcat . intersperse B.space . filter (/= mempty) $
         [ datumInlines planningClosed "CLOSED"
         , datumInlines planningDeadline "DEADLINE"
         , datumInlines planningScheduled "SCHEDULED"
         ]
    else mempty
 where
  datumInlines field name =
    case field planning of
      Nothing -> mempty
      Just time ->   B.strong (B.str name <> B.str ":")
                  <> B.space
                  <> B.emph (B.str time)

-- | An Org timestamp, including repetition marks. TODO: improve
type Timestamp = String

timestamp :: Monad m => OrgParser m Timestamp
timestamp = try $ do
  openChar <- oneOf "<["
  let isActive = openChar == '<'
  let closeChar = if isActive then '>' else ']'
  content <- many1Till anyChar (char closeChar)
  return (openChar : content ++ [closeChar])

-- | Planning information for a subtree/headline.
data PlanningInfo = PlanningInfo
  { planningClosed :: Maybe Timestamp
  , planningDeadline :: Maybe Timestamp
  , planningScheduled :: Maybe Timestamp
  }

emptyPlanning :: PlanningInfo
emptyPlanning = PlanningInfo Nothing Nothing Nothing

-- | Read a single planning-related and timestamped line.
planningInfo :: Monad m => OrgParser m PlanningInfo
planningInfo = try $ do
  updaters <- many1 planningDatum <* skipSpaces <* newline
  return $ foldr ($) emptyPlanning updaters
 where
  planningDatum = skipSpaces *> choice
    [ updateWith (\s p -> p { planningScheduled = Just s}) "SCHEDULED"
    , updateWith (\d p -> p { planningDeadline = Just d}) "DEADLINE"
    , updateWith (\c p -> p { planningClosed = Just c}) "CLOSED"
    ]
  updateWith fn cs = fn <$> (string cs *> char ':' *> skipSpaces *> timestamp)

-- | Read a :PROPERTIES: drawer and return the key/value pairs contained
-- within.
propertiesDrawer :: Monad m => OrgParser m Properties
propertiesDrawer = try $ do
  drawerType <- drawerStart
  guard $ map toUpper drawerType == "PROPERTIES"
  manyTill property (try endOfDrawer)
 where
   property :: Monad m => OrgParser m (PropertyKey, PropertyValue)
   property = try $ (,) <$> key <*> value

   key :: Monad m => OrgParser m PropertyKey
   key = fmap toPropertyKey . try $
         skipSpaces *> char ':' *> many1Till nonspaceChar (char ':')

   value :: Monad m => OrgParser m PropertyValue
   value = fmap toPropertyValue . try $
           skipSpaces *> manyTill anyChar (try $ skipSpaces *> newline)

   endOfDrawer :: Monad m => OrgParser m String
   endOfDrawer = try $
     skipSpaces *> stringAnyCase ":END:" <* skipSpaces <* newline
