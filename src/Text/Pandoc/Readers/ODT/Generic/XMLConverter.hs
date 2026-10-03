{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.ODT.Generic.XMLConverter
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

A generalized monadic XML parser. The parser navigates through an XML
tree, always looking at a \"current element\", and carries some
additional, converter-specific state.
-}

module Text.Pandoc.Readers.ODT.Generic.XMLConverter
( ElementName
, XMLConverter
, runConverter
, fromMaybeF
, fromFallible
, tryC
, getExtraState
, setExtraState
, modifyExtraState
, getCurrentElement
, elName
, findChild'
, filterChildrenName'
, isSet'
, isSetWithDefault
, searchAttr
, lookupAttr
, lookupAttr'
, lookupDefaultingAttr
, findAttr'
, findAttr
, findAttrWithDefault
, readAttr
, readAttr'
, readAttrWithDefault
, getAttr
, executeIn
, executeInSub
, withEveryL
, tryAll
, ElementMatcher
, matchContent'
, matchContent
) where

import Control.Applicative ( Alternative(..), optional )
import Control.Monad ( filterM, foldM )
import Control.Monad.Except ( ExceptT, runExceptT, throwError, catchError )
import Control.Monad.State ( State, evalState, get, gets, put, modify )

import qualified Data.Map as M
import Data.Text (Text)
import Data.Default
import Data.Maybe

import qualified Text.Pandoc.XML.Light as XML

import Text.Pandoc.Readers.ODT.Generic.Namespaces
import Text.Pandoc.Readers.ODT.Generic.Utils
import Text.Pandoc.Readers.ODT.Generic.Fallible

--------------------------------------------------------------------------------
--  Basis types for readability
--------------------------------------------------------------------------------

type ElementName           = Text
type AttributeName         = Text
type AttributeValue        = Text

type NameSpacePrefix       = Text

--------------------------------------------------------------------------------
-- Converter state
--------------------------------------------------------------------------------

data XMLConverterState nsID extraState = XMLConverterState
  { -- | The element that is currently being read
    currentElement    :: XML.Element
    -- | A map from internal namespace IDs to the namespace prefixes
    -- used in XML elements
  , namespacePrefixes :: M.Map nsID NameSpacePrefix
    -- | A map from internal namespace IDs to namespace IRIs
    -- (Only necessary for matching namespace IDs and prefixes)
  , namespaceIRIs     :: NameSpaceIRIs nsID
    -- | Converter-specific state
  , moreState         :: extraState
  }

createStartState :: (NameSpaceID nsID)
                 => XML.Element
                 -> extraState
                 -> XMLConverterState nsID extraState
createStartState element extraState =
  XMLConverterState
       { currentElement    = element
       , namespacePrefixes = M.empty
       , namespaceIRIs     = getInitialIRImap
       , moreState         = extraState
       }

--------------------------------------------------------------------------------
-- Main type
--------------------------------------------------------------------------------

-- | A converter that can read from an XML tree, may fail (with
-- 'throwError' \/ 'empty'), and carries some additional state.
-- Note that state modifications survive failure; in particular, the
-- current element must be restored explicitly where necessary
-- (see 'executeIn').
type XMLConverter nsID extraState
   = ExceptT () (State (XMLConverterState nsID extraState))

-- | Run a converter on an XML element, with a given initial extra state.
runConverter :: (NameSpaceID nsID)
             => XMLConverter nsID extraState a
             -> extraState
             -> XML.Element
             -> Fallible a
runConverter converter extraState element
  = evalState (runExceptT (readNSattributes >> converter))
              (createStartState element extraState)

-- | Lift a 'Maybe' value into the converter, failing on 'Nothing'.
fromMaybeF :: Maybe a -> XMLConverter nsID extraState a
fromMaybeF = maybe (throwError ()) return

-- | Lift a 'Fallible' value into the converter.
fromFallible :: Fallible a -> XMLConverter nsID extraState a
fromFallible = either throwError return

-- | Run a converter, catching failure.
tryC :: XMLConverter nsID extraState a
     -> XMLConverter nsID extraState (Fallible a)
tryC converter = catchError (Right <$> converter) (return . Left)

--
getCurrentElement :: XMLConverter nsID extraState XML.Element
getCurrentElement = gets currentElement

--
getExtraState :: XMLConverter nsID extraState extraState
getExtraState = gets moreState

--
setExtraState :: extraState -> XMLConverter nsID extraState ()
setExtraState x = modify $ \state -> state { moreState = x }

--
modifyExtraState :: (extraState -> extraState)
                 -> XMLConverter nsID extraState ()
modifyExtraState f = modify $ \state -> state { moreState = f (moreState state) }

--------------------------------------------------------------------------------
-- Work in namespaces
--------------------------------------------------------------------------------

--
lookupNSiri :: (NameSpaceID nsID)
            => nsID
            -> XMLConverter nsID extraState (Maybe NameSpaceIRI)
lookupNSiri nsID = gets $ getIRI nsID . namespaceIRIs

--
lookupNSprefix :: (NameSpaceID nsID)
               => nsID
               -> XMLConverter nsID extraState (Maybe NameSpacePrefix)
lookupNSprefix nsID = gets $ M.lookup nsID . namespacePrefixes

-- | Extracts namespace attributes from the current element and tries to
-- update the current mapping accordingly
readNSattributes :: (NameSpaceID nsID) => XMLConverter nsID extraState ()
readNSattributes = do
  state <- get
  maybe (throwError ()) put (extractNSAttrs state)
  where
    extractNSAttrs :: (NameSpaceID nsID)
                   => XMLConverterState nsID extraState
                   -> Maybe (XMLConverterState nsID extraState)
    extractNSAttrs startState = foldM addNS startState nsAttribs
      where nsAttribs    = mapMaybe readNSattr
                                    (XML.elAttribs $ currentElement startState)
            readNSattr (XML.Attr (XML.QName name _ (Just "xmlns")) iri)
                         = Just (name, iri)
            readNSattr _ = Nothing
    addNS state (prefix, iri) = updateState
                                <$> getNamespaceID iri (namespaceIRIs state)
      where updateState (iris, nsID)
              = state { namespaceIRIs     = iris
                      , namespacePrefixes = M.insert nsID prefix
                                            $ namespacePrefixes state
                      }

--------------------------------------------------------------------------------
-- Common namespace accessors
--------------------------------------------------------------------------------

-- | Given a namespace id and an element name, creates a 'XML.QName' for
-- internal use
qualifyName :: (NameSpaceID nsID)
            => nsID -> ElementName
            -> XMLConverter nsID extraState XML.QName
qualifyName nsID name = XML.QName name <$> lookupNSiri nsID
                                       <*> lookupNSprefix nsID

-- | Checks if a given element matches both a specified namespace id
-- and a predicate
elemNameMatches :: (NameSpaceID nsID)
                => nsID -> (ElementName -> Bool)
                -> XML.Element
                -> XMLConverter nsID extraState Bool
elemNameMatches nsID f element = do
  iri <- lookupNSiri nsID
  let name = XML.elName element
  return $ f (XML.qName name) && XML.qURI name == iri

-- | Checks if a given element matches both a specified namespace id
-- and a specified element name
elemNameIs :: (NameSpaceID nsID)
           => nsID -> ElementName
           -> XML.Element
           -> XMLConverter nsID extraState Bool
elemNameIs nsID name = elemNameMatches nsID (== name)

--------------------------------------------------------------------------------
-- General content
--------------------------------------------------------------------------------

elName :: XML.Element -> ElementName
elName = XML.qName . XML.elName

--------------------------------------------------------------------------------
-- Children
--------------------------------------------------------------------------------

--
findChildren :: (NameSpaceID nsID)
             => nsID -> ElementName
             -> XMLConverter nsID extraState [XML.Element]
findChildren nsID name = XML.findChildren <$> qualifyName nsID name
                                          <*> getCurrentElement

--
findChild' :: (NameSpaceID nsID)
           => nsID -> ElementName
           -> XMLConverter nsID extraState (Maybe XML.Element)
findChild' nsID name = XML.findChild <$> qualifyName nsID name
                                     <*> getCurrentElement

--
findChild :: (NameSpaceID nsID)
          => nsID -> ElementName
          -> XMLConverter nsID extraState XML.Element
findChild nsID name = findChild' nsID name >>= fromMaybeF

--
filterChildrenName' :: (NameSpaceID nsID)
                    => nsID
                    -> (ElementName -> Bool)
                    -> XMLConverter nsID extraState [XML.Element]
filterChildrenName' nsID f = getCurrentElement
                             >>= filterM (elemNameMatches nsID f) . XML.elChildren

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

--
isSet' :: (NameSpaceID nsID)
       => nsID -> AttributeName
       -> XMLConverter nsID extraState (Maybe Bool)
isSet' nsID attrName = (>>= stringToBool') <$> findAttr' nsID attrName

isSetWithDefault :: (NameSpaceID nsID)
                 => nsID -> AttributeName
                 -> Bool
                 -> XMLConverter nsID extraState Bool
isSetWithDefault nsID attrName def' =
  fromMaybe def' <$> isSet' nsID attrName

-- | Lookup value in a dictionary, fail if no attribute found or value
-- not in dictionary
searchAttrIn :: (NameSpaceID nsID)
             => nsID -> AttributeName
             -> [(AttributeValue,a)]
             -> XMLConverter nsID extraState a
searchAttrIn nsID attrName dict = do
  value <- findAttr nsID attrName
  fromMaybeF $ lookup value dict

-- | Lookup value in a dictionary. If attribute or value not found,
-- return default value
searchAttr :: (NameSpaceID nsID)
           => nsID -> AttributeName
           -> a
           -> [(AttributeValue,a)]
           -> XMLConverter nsID extraState a
searchAttr nsID attrName defV dict =
  searchAttrIn nsID attrName dict <|> return defV

-- | Read a 'Lookupable' attribute. Fail if no match.
lookupAttr :: (NameSpaceID nsID, Lookupable a)
           => nsID -> AttributeName
           -> XMLConverter nsID extraState a
lookupAttr nsID attrName = lookupAttr' nsID attrName >>= fromMaybeF

-- | Read a 'Lookupable' attribute. Return the result as a 'Maybe'.
lookupAttr' :: (NameSpaceID nsID, Lookupable a)
            => nsID -> AttributeName
            -> XMLConverter nsID extraState (Maybe a)
lookupAttr' nsID attrName =
  (>>= readLookupable) <$> findAttr' nsID attrName

-- | Read a 'Lookupable' attribute with explicit default
lookupAttrWithDefault :: (NameSpaceID nsID, Lookupable a)
                      => nsID -> AttributeName
                      -> a
                      -> XMLConverter nsID extraState a
lookupAttrWithDefault nsID attrName deflt =
  fromMaybe deflt <$> lookupAttr' nsID attrName

-- | Read a 'Lookupable' attribute with implicit default
lookupDefaultingAttr :: (NameSpaceID nsID, Lookupable a, Default a)
                     => nsID -> AttributeName
                     -> XMLConverter nsID extraState a
lookupDefaultingAttr nsID attrName =
  lookupAttrWithDefault nsID attrName def

-- | Return value as a (Maybe Text)
findAttr' :: (NameSpaceID nsID)
          => nsID -> AttributeName
          -> XMLConverter nsID extraState (Maybe AttributeValue)
findAttr' nsID attrName = XML.findAttr <$> qualifyName nsID attrName
                                       <*> getCurrentElement

-- | Return value or fail
findAttr :: (NameSpaceID nsID)
         => nsID -> AttributeName
         -> XMLConverter nsID extraState AttributeValue
findAttr nsID attrName = findAttr' nsID attrName >>= fromMaybeF

-- | Return value or return provided default value
findAttrWithDefault :: (NameSpaceID nsID)
                    => nsID -> AttributeName
                    -> AttributeValue
                    -> XMLConverter nsID extraState AttributeValue
findAttrWithDefault nsID attrName deflt =
  fromMaybe deflt <$> findAttr' nsID attrName

-- | Read and return value or fail
readAttr :: (NameSpaceID nsID, Read attrValue)
         => nsID -> AttributeName
         -> XMLConverter nsID extraState attrValue
readAttr nsID attrName = readAttr' nsID attrName >>= fromMaybeF

-- | Read and return value or return Nothing
readAttr' :: (NameSpaceID nsID, Read attrValue)
          => nsID -> AttributeName
          -> XMLConverter nsID extraState (Maybe attrValue)
readAttr' nsID attrName = (>>= tryToRead) <$> findAttr' nsID attrName

-- | Read and return value or return provided default value
readAttrWithDefault :: (NameSpaceID nsID, Read attrValue)
                    => nsID -> AttributeName
                    -> attrValue
                    -> XMLConverter nsID extraState attrValue
readAttrWithDefault nsID attrName deflt =
  fromMaybe deflt <$> readAttr' nsID attrName

-- | Read and return value or return default value from 'Default' instance
getAttr :: (NameSpaceID nsID, Read attrValue, Default attrValue)
        => nsID -> AttributeName
        -> XMLConverter nsID extraState attrValue
getAttr nsID attrName = readAttrWithDefault nsID attrName def

--------------------------------------------------------------------------------
-- Movements
--------------------------------------------------------------------------------

-- | Execute a converter in a specific element, then come back.
-- The current element is restored even if the converter fails.
executeIn :: XML.Element
          -> XMLConverter nsID extraState a
          -> XMLConverter nsID extraState a
executeIn element converter = do
  oldElement <- getCurrentElement
  modify $ \state -> state { currentElement = element }
  result <- tryC converter
  modify $ \state -> state { currentElement = oldElement }
  fromFallible result

-- | Execute a converter in a sub-element of the current element,
-- then come back. Fails if there is no such sub-element.
executeInSub :: (NameSpaceID nsID)
             => nsID -> ElementName
             -> XMLConverter nsID extraState a
             -> XMLConverter nsID extraState a
executeInSub nsID name converter = do
  child <- findChild nsID name
  executeIn child converter

--------------------------------------------------------------------------------
-- Iterating over children
--------------------------------------------------------------------------------

-- | Applies a converter to every child element of a specific type.
-- Fails completely if any conversion fails.
withEveryL :: (NameSpaceID nsID)
           => nsID -> ElementName
           -> XMLConverter nsID extraState a
           -> XMLConverter nsID extraState [a]
withEveryL nsID name converter = do
  children <- findChildren nsID name
  mapM (`executeIn` converter) children

-- | Applies a converter to every child element of a specific type.
-- Collects all successful results in a list.
tryAll :: (NameSpaceID nsID)
       => nsID -> ElementName
       -> XMLConverter nsID extraState a
       -> XMLConverter nsID extraState [a]
tryAll nsID name converter = do
  children <- findChildren nsID name
  catMaybes <$> mapM (\child -> optional (executeIn child converter)) children

--------------------------------------------------------------------------------
-- Matching children
--------------------------------------------------------------------------------

-- | A converter for a child element with a specific name in a specific
-- namespace. The converter produces that element's contribution to the
-- overall result.
type ElementMatcher nsID extraState a
   = (nsID, ElementName, XMLConverter nsID extraState a)

-- | Like 'matchContent', but ignores non-matching content.
matchContent' :: (NameSpaceID nsID, Monoid a)
              => [ElementMatcher nsID extraState a]
              -> XMLConverter nsID extraState a
matchContent' lookups = matchContent lookups (\_ -> return mempty)

-- | Takes a list of element matchers and a fallback converter, and
-- converts the content of the current element in order: for each child
-- element, the first matcher with a matching name (if any) is applied
-- in that element; all other content is passed to the fallback
-- converter. The results are combined with 'mappend'.
-- If a matched converter fails, the corresponding element contributes
-- nothing to the result.
matchContent :: (NameSpaceID nsID, Monoid a)
             => [ElementMatcher nsID extraState a]
             -> (XML.Content -> XMLConverter nsID extraState a)
             -> XMLConverter nsID extraState a
matchContent lookups fallback = do
  contents <- XML.elContent <$> getCurrentElement
  mconcat <$> mapM matchOne contents
  where
    matchOne content@(XML.Elem element) = do
      mConverter <- findConverter element lookups
      case mConverter of
        Just converter -> executeIn element converter <|> return mempty
        Nothing        -> fallback content
    matchOne content = fallback content

    findConverter _ [] = return Nothing
    findConverter element ((nsID, name, converter):rest) = do
      matches <- elemNameIs nsID name element
      if matches
        then return $ Just converter
        else findConverter element rest

--------------------------------------------------------------------------------
-- Internals
--------------------------------------------------------------------------------

stringToBool' :: Text -> Maybe Bool
stringToBool' val | val `elem` trueValues  = Just True
                  | val `elem` falseValues = Just False
                  | otherwise              = Nothing
  where trueValues  = ["true" ,"on" ,"1"]
        falseValues = ["false","off","0"]
