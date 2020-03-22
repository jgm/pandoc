{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{- |
   Module      : Text.Pandoc.Readers.Org.DocumentTree
   Copyright   : Copyright (C) 2014-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Parsers for org-mode headlines and document subtrees
-}
module Text.Pandoc.Readers.Org.DocumentTree
  ( documentTree
  , unprunedHeadlineToBlocks
  ) where

import Control.Arrow ((***), first)
import Control.Monad (guard)
import Data.List (intersperse)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Text.Pandoc.Builder (Blocks, Inlines)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Readers.Org.BlockStarts
import Text.Pandoc.Readers.Org.ParserState
import Text.Pandoc.Readers.Org.Parsing

import qualified Data.Set as Set
import qualified Data.Text as T
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
toTag :: Text -> Tag
toTag = Tag

-- | The key (also called name or type) of a property.
newtype PropertyKey = PropertyKey { fromKey :: Text }
  deriving (Show, Eq, Ord)

-- | Create a property key containing the given string.  Org mode keys are
-- case insensitive and are hence converted to lower case.
toPropertyKey :: Text -> PropertyKey
toPropertyKey = PropertyKey . T.toLower

-- | The value assigned to a property.
newtype PropertyValue = PropertyValue { fromValue :: Text }

-- | Create a property value containing the given string.
toPropertyValue :: Text -> PropertyValue
toPropertyValue = PropertyValue

-- | Check whether the property value is non-nil (i.e. truish).
isNonNil :: PropertyValue -> Bool
isNonNil p = T.toLower (fromValue p) `notElem` ["()", "{}", "nil"]

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
  (title, tags) <- manyThen inline endOfTitle
  planning   <- option emptyPlanning planningInfo
  properties <- option mempty propertiesDrawer
  contents   <- blocks
  children   <- many (headline blocks inline (level + 1))
  return $ do
    title'    <- trimInlinesF (mconcat title)
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
   endOfTitle :: Monad m => OrgParser m [Tag]
   endOfTitle = try $ do
     skipSpaces
     tags <- option [] (headerTags <* skipSpaces)
     newline
     return tags

   headerTags :: Monad m => OrgParser m [Tag]
   headerTags = try $ do
     char ':'
     endBy1 (toTag <$> orgTagWord) (char ':')

   manyThen :: Monad m
            => OrgParser m a
            -> OrgParser m b
            -> OrgParser m ([a], b)
   manyThen p end = (([],) <$> try end) <|> do
     x <- p
     first (x:) <$> manyThen p end

   -- titleFollowedByTags :: Monad m => OrgParser m (Inlines, [Tag])
   -- titleFollowedByTags = do


unprunedHeadlineToBlocks :: Monad m => Headline -> OrgParserState -> OrgParser m [Block]
unprunedHeadlineToBlocks hdln st =
  let usingSelectedTags = docContainsSelectTags hdln st
      rootNode = if not usingSelectedTags
                   then hdln
                   else includeRootAndSelected hdln st
      rootNode' = removeExplicitlyExcludedNodes rootNode st
  in if not usingSelectedTags ||
        any (`Set.member` orgStateSelectTags st) (headlineTags rootNode')
        then do headlineBlocks <- headlineToBlocks rootNode'
                -- ignore first headline, it's the document's title
                return . drop 1 . B.toList $ headlineBlocks
        else do headlineBlocks <- mconcat <$> mapM headlineToBlocks
                                                   (headlineChildren rootNode')
                return . B.toList $ headlineBlocks

-- | Convert an Org mode headline (i.e. a document tree) into pandoc's Blocks
headlineToBlocks :: Monad m => Headline -> OrgParser m Blocks
headlineToBlocks hdln = do
  maxLevel <- getExportSetting exportHeadlineLevels
  let tags = headlineTags hdln
  let text = headlineText hdln
  let level = headlineLevel hdln
  case () of
    _ | any isArchiveTag  tags -> archivedHeadlineToBlocks hdln
    _ | isCommentTitle text    -> return mempty
    _ | maxLevel <= level      -> headlineToHeaderWithList hdln
    _ | otherwise              -> headlineToHeaderWithContents hdln

removeExplicitlyExcludedNodes :: Headline -> OrgParserState -> Headline
removeExplicitlyExcludedNodes hdln st =
  hdln { headlineChildren =
           [removeExplicitlyExcludedNodes childHdln st |
              childHdln <- headlineChildren hdln,
              not $ headlineContainsExcludeTags childHdln st] }

includeRootAndSelected :: Headline -> OrgParserState -> Headline
includeRootAndSelected hdln st =
  hdln { headlineChildren = mapMaybe (`includeAncestorsAndSelected` st)
                                     (headlineChildren hdln)}

docContainsSelectTags :: Headline -> OrgParserState -> Bool
docContainsSelectTags hdln st =
  headlineContainsSelectTags hdln st ||
  any (`docContainsSelectTags` st) (headlineChildren hdln)

includeAncestorsAndSelected :: Headline -> OrgParserState -> Maybe Headline
includeAncestorsAndSelected hdln st =
  if headlineContainsSelectTags hdln st
    then Just hdln
    else let children = mapMaybe (`includeAncestorsAndSelected` st)
                                 (headlineChildren hdln)
         in case children of
              [] -> Nothing
              _ -> Just $ hdln { headlineChildren = children }

headlineContainsSelectTags :: Headline -> OrgParserState -> Bool
headlineContainsSelectTags hdln st =
  any (`Set.member` orgStateSelectTags st) (headlineTags hdln)

headlineContainsExcludeTags :: Headline -> OrgParserState -> Bool
headlineContainsExcludeTags hdln st =
  any (`Set.member` orgStateExcludeTags st) (headlineTags hdln)

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
    , planningBlock
    , headlineContents hdln
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
  let kwParser tdm = try (tdm <$ textStr (todoMarkerName tdm)
                              <* spaceChar
                              <* updateLastPreCharPos)
  choice (map kwParser taskStates)

todoKeywordToInlines :: TodoMarker -> Inlines
todoKeywordToInlines tdm =
  let todoText  = todoMarkerName tdm
      todoState = T.toLower . T.pack . show $ todoMarkerState tdm
      classes = [todoState, todoText]
  in B.spanWith (mempty, classes, mempty) (B.str todoText)

propertiesToAttr :: Properties -> Attr
propertiesToAttr properties =
  let
    toTextPair = fromKey *** fromValue
    customIdKey = toPropertyKey "custom_id"
    classKey    = toPropertyKey "class"
    unnumberedKey = toPropertyKey "unnumbered"
    specialProperties = [customIdKey, classKey, unnumberedKey]
    id'  = maybe mempty fromValue . lookup customIdKey $ properties
    cls  = maybe mempty fromValue . lookup classKey    $ properties
    kvs' = map toTextPair . filter ((`notElem` specialProperties) . fst)
           $ properties
    isUnnumbered =
      maybe False isNonNil . lookup unnumberedKey $ properties
  in
    (id', T.words cls ++ ["unnumbered" | isUnnumbered], kvs')

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
type Timestamp = Text

timestamp :: Monad m => OrgParser m Timestamp
timestamp = try $ do
  openChar <- oneOf "<["
  let isActive = openChar == '<'
  let closeChar = if isActive then '>' else ']'
  content <- many1TillChar anyChar (char closeChar)
  return $ T.cons openChar $ content `T.snoc` closeChar

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
  guard $ T.toUpper drawerType == "PROPERTIES"
  manyTill property (try endOfDrawer)
 where
   property :: Monad m => OrgParser m (PropertyKey, PropertyValue)
   property = try $ (,) <$> key <*> value

   key :: Monad m => OrgParser m PropertyKey
   key = fmap toPropertyKey . try $
         skipSpaces *> char ':' *> many1TillChar nonspaceChar (char ':')

   value :: Monad m => OrgParser m PropertyValue
   value = fmap toPropertyValue . try $
           skipSpaces *> manyTillChar anyChar (try $ skipSpaces *> newline)

   endOfDrawer :: Monad m => OrgParser m Text
   endOfDrawer = try $
     skipSpaces *> stringAnyCase ":END:" <* skipSpaces <* newline
