{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PatternGuards   #-}
{- |
   Module      : Text.Pandoc.Readers.Odt.Generic.XMLConverter
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

A generalized XML parser based on stateful arrows.
It might be sufficient to define this reader as a comonad, but there is
not a lot of use in trying.
-}

module Text.Pandoc.Readers.Odt.Generic.XMLConverter
( ElementName
, XMLConverterState
, XMLConverter
, FallibleXMLConverter
, runConverter'
, getExtraState
, setExtraState
, modifyExtraState
, producingExtraState
, findChild'
, filterChildrenName'
, isSet'
, isSetWithDefault
, elName
, searchAttr
, lookupAttr
, lookupAttr'
, lookupDefaultingAttr
, findAttr'
, findAttrText'
, findAttr
, findAttrText
, findAttrWithDefault
, findAttrTextWithDefault
, readAttr
, readAttr'
, readAttrWithDefault
, getAttr
, executeIn
, executeInSub
, withEveryL
, tryAll
, matchContent'
, matchContent
) where

import           Control.Applicative  hiding ( liftA, liftA2 )
import           Control.Monad               ( MonadPlus )
import           Control.Arrow

import           Data.Bool ( bool )
import           Data.Either ( rights )
import qualified Data.Map             as M
import qualified Data.Text            as T
import           Data.Default
import           Data.Maybe

import qualified Text.XML.Light       as XML

import           Text.Pandoc.Readers.Odt.Arrows.State
import           Text.Pandoc.Readers.Odt.Arrows.Utils

import           Text.Pandoc.Readers.Odt.Generic.Namespaces
import           Text.Pandoc.Readers.Odt.Generic.Utils
import           Text.Pandoc.Readers.Odt.Generic.Fallible

--------------------------------------------------------------------------------
--  Basis types for readability
--------------------------------------------------------------------------------

--
type ElementName           = String
type AttributeName         = String
type AttributeValue        = String
type TextAttributeValue    = T.Text

--
type NameSpacePrefix       = String

--
type NameSpacePrefixes nsID = M.Map nsID NameSpacePrefix

--------------------------------------------------------------------------------
-- Main converter state
--------------------------------------------------------------------------------

-- GADT so some of the NameSpaceID restrictions can be deduced
data XMLConverterState nsID extraState where
  XMLConverterState :: NameSpaceID nsID
    => { -- | A stack of parent elements. The top element is the current one.
         -- Arguably, a real Zipper would be better. But that is an
         -- optimization that can be made at a later time, e.g. when
         -- replacing Text.XML.Light.
         parentElements    :: [XML.Element]
         -- | A map from internal namespace IDs to the namespace prefixes
         -- used in XML elements
       , namespacePrefixes :: NameSpacePrefixes nsID
         -- | A map from internal namespace IDs to namespace IRIs
         -- (Only necessary for matching namespace IDs and prefixes)
       , namespaceIRIs     :: NameSpaceIRIs nsID
         -- | A place to put "something else". This feature is used heavily
         -- to keep the main code cleaner. More specifically, the main reader
         -- is divided into different stages. Each stage lifts something up
         -- here, which the next stage can then use. This could of course be
         -- generalized to a state-tree or used for the namespace IRIs. The
         -- border between states and values is an imaginary one, after all.
         -- But the separation as it is seems to be enough for now.
       , moreState         :: extraState
       }
    -> XMLConverterState nsID extraState

--
createStartState :: (NameSpaceID nsID)
                    => XML.Element
                    -> extraState
                    -> XMLConverterState nsID extraState
createStartState element extraState =
  XMLConverterState
       { parentElements    = [element]
       , namespacePrefixes = M.empty
       , namespaceIRIs     = getInitialIRImap
       , moreState         = extraState
       }

-- | Functor over extra state
instance Functor (XMLConverterState nsID) where
  fmap f ( XMLConverterState parents prefixes iRIs    extraState  )
       =   XMLConverterState parents prefixes iRIs (f extraState)

--
replaceExtraState   :: extraState
                    -> XMLConverterState nsID _x
                    -> XMLConverterState nsID extraState
replaceExtraState x s
                     = fmap (const x) s

--
currentElement      :: XMLConverterState nsID extraState
                    -> XML.Element
currentElement state = head (parentElements state)

-- | Replace the current position by another, modifying the extra state
-- in the process
swapStack'          :: XMLConverterState nsID extraState
                    -> [XML.Element]
                    -> ( XMLConverterState nsID extraState , [XML.Element] )
swapStack' state stack
                     = ( state { parentElements = stack }
                       , parentElements state
                       )

--
pushElement         :: XML.Element
                    -> XMLConverterState nsID extraState
                    -> XMLConverterState nsID extraState
pushElement e state  = state { parentElements = e:parentElements state }

-- | Pop the top element from the call stack, unless it is the last one.
popElement          :: XMLConverterState nsID extraState
                    -> Maybe (XMLConverterState nsID extraState)
popElement state
  | _:es@(_:_) <- parentElements state = Just $ state { parentElements = es }
  | otherwise                          = Nothing

--------------------------------------------------------------------------------
-- Main type
--------------------------------------------------------------------------------

-- It might be a good idea to pack the converters in a GADT
-- Downside: data instead of type
-- Upside: 'Failure' could be made a parameter as well.

--
type XMLConverter nsID extraState input output
      = ArrowState (XMLConverterState nsID extraState ) input output

type FallibleXMLConverter nsID extraState input output
     = XMLConverter nsID extraState input (Fallible output)

--
runConverter     :: XMLConverter nsID extraState input output
                 -> XMLConverterState nsID extraState
                 -> input
                 -> output
runConverter converter state input = snd $ runArrowState converter (state,input)

runConverter' :: (NameSpaceID nsID)
              => FallibleXMLConverter nsID extraState () success
              -> extraState
              -> XML.Element
              -> Fallible success
runConverter' converter extraState element = runConverter (readNSattributes >>? converter) (createStartState element extraState) ()

--
getCurrentElement :: XMLConverter nsID extraState x XML.Element
getCurrentElement  = extractFromState currentElement

--
getExtraState     :: XMLConverter nsID extraState x extraState
getExtraState      = extractFromState moreState

--
setExtraState     :: XMLConverter nsID extraState extraState extraState
setExtraState      = withState $ \state extra
                                  -> (replaceExtraState extra state , extra)


-- | Lifts a function to the extra state.
modifyExtraState  :: (extraState -> extraState)
                  -> XMLConverter nsID extraState x x
modifyExtraState   = modifyState.fmap


-- | First sets the extra state to the new value. Then modifies the original
-- extra state with a converter that uses the new state. Finally, the
-- intermediate state is dropped and the extra state is lifted into the
-- state as it was at the beginning of the function.
-- As a result, exactly the extra state and nothing else is changed.
-- The resulting converter even behaves like an identity converter on the
-- value level.
--
-- (The -ing form is meant to be mnemonic in a sequence of arrows as in
--  convertingExtraState () converter >>> doOtherStuff)
--
convertingExtraState :: extraState'
                     -> FallibleXMLConverter nsID extraState' extraState extraState
                     -> FallibleXMLConverter nsID extraState x x
convertingExtraState v a = withSubStateF setVAsExtraState modifyWithA
  where
    setVAsExtraState     = liftAsSuccess $ extractFromState id >>^ replaceExtraState v
    modifyWithA          = keepingTheValue (moreState ^>> a)
                           >>^ spreadChoice >>?% flip replaceExtraState

-- | First sets the extra state to the new value. Then produces a new
-- extra state with a converter that uses the new state. Finally, the
-- intermediate state is dropped and the extra state is lifted into the
-- state as it was at the beginning of the function.
-- As a result, exactly the extra state and nothing else is changed.
-- The resulting converter even behaves like an identity converter on the
-- value level.
--
-- Equivalent to
--
-- > \v x a -> convertingExtraState v (returnV x >>> a)
--
-- (The -ing form is meant to be mnemonic in a sequence of arrows as in
--  producingExtraState () () producer >>> doOtherStuff)
--
producingExtraState  :: extraState'
                     -> a
                     -> FallibleXMLConverter nsID extraState' a extraState
                     -> FallibleXMLConverter nsID extraState x x
producingExtraState v x a = convertingExtraState v (returnV x >>> a)


--------------------------------------------------------------------------------
-- Work in namespaces
--------------------------------------------------------------------------------

-- | Arrow version of 'getIRI'
lookupNSiri             :: (NameSpaceID nsID)
                        => nsID
                        -> XMLConverter nsID extraState x (Maybe NameSpaceIRI)
lookupNSiri nsID        = extractFromState
                          $ \state -> getIRI nsID $ namespaceIRIs state

--
lookupNSprefix           :: (NameSpaceID nsID)
                         => nsID
                         -> XMLConverter nsID extraState x (Maybe NameSpacePrefix)
lookupNSprefix nsID      = extractFromState
                           $ \state -> M.lookup nsID $ namespacePrefixes state

-- | Extracts namespace attributes from the current element and tries to
-- update the current mapping accordingly
readNSattributes         :: (NameSpaceID nsID)
                         => FallibleXMLConverter nsID extraState x ()
readNSattributes         = fromState $ \state -> maybe (state, failEmpty     )
                                                       (     , succeedWith ())
                                                       (extractNSAttrs state )
  where
    extractNSAttrs       :: (NameSpaceID nsID)
                         => XMLConverterState nsID extraState
                         -> Maybe (XMLConverterState nsID extraState)
    extractNSAttrs startState
                         = foldl (\state d -> state >>= addNS d)
                                 (Just startState)
                                 nsAttribs
      where nsAttribs    = mapMaybe readNSattr (XML.elAttribs element)
            element      = currentElement startState
            readNSattr (XML.Attr (XML.QName name _ (Just "xmlns")) iri)
                         = Just (name, iri)
            readNSattr _ = Nothing
    addNS  (prefix, iri) state = fmap updateState
                                 $ getNamespaceID iri
                                 $ namespaceIRIs state
      where updateState (iris,nsID)
                         = state { namespaceIRIs     = iris
                                 , namespacePrefixes = M.insert nsID prefix
                                                       $ namespacePrefixes state
                                 }

--------------------------------------------------------------------------------
-- Common namespace accessors
--------------------------------------------------------------------------------

-- | Given a namespace id and an element name, creates a 'XML.QName' for
-- internal use
qualifyName              :: (NameSpaceID nsID)
                         => nsID -> ElementName
                         -> XMLConverter nsID extraState x XML.QName
qualifyName nsID name    =         lookupNSiri nsID
                               &&& lookupNSprefix nsID
                           >>% XML.QName name

-- | Checks if a given element matches both a specified namespace id
-- and a predicate
elemNameMatches          :: (NameSpaceID nsID)
                         => nsID -> (ElementName -> Bool)
                         -> XMLConverter nsID extraState XML.Element Bool
elemNameMatches nsID f    = keepingTheValue (lookupNSiri nsID) >>% hasMatchingName
  where hasMatchingName e iri = let name = XML.elName e
                                in     f (XML.qName name)
                                    && XML.qURI name == iri

-- | Checks if a given element matches both a specified namespace id
-- and a specified element name
elemNameIs               :: (NameSpaceID nsID)
                         => nsID -> ElementName
                         -> XMLConverter nsID extraState XML.Element Bool
elemNameIs nsID name     = elemNameMatches nsID (== name)

--------------------------------------------------------------------------------
-- General content
--------------------------------------------------------------------------------

elName :: XML.Element -> ElementName
elName = XML.qName . XML.elName

--
elContent               :: XMLConverter nsID extraState x [XML.Content]
elContent               =     getCurrentElement
                           >>^ XML.elContent

--------------------------------------------------------------------------------
-- Children
--------------------------------------------------------------------------------

--
--
findChildren             :: (NameSpaceID nsID)
                         => nsID -> ElementName
                         -> XMLConverter nsID extraState x [XML.Element]
findChildren nsID name   =         qualifyName nsID name
                               &&& getCurrentElement
                           >>% XML.findChildren

--
findChild'              :: (NameSpaceID nsID)
                        => nsID
                        -> ElementName
                        -> XMLConverter nsID extraState x (Maybe XML.Element)
findChild' nsID name    =         qualifyName nsID name
                              &&& getCurrentElement
                          >>% XML.findChild

--
findChild              :: (NameSpaceID nsID)
                       => nsID -> ElementName
                       -> FallibleXMLConverter nsID extraState x XML.Element
findChild nsID name    =     findChild' nsID name
                         >>> maybeToChoice

filterChildrenName'        :: (NameSpaceID nsID)
                           => nsID
                           -> (ElementName -> Bool)
                           -> XMLConverter nsID extraState x [XML.Element]
filterChildrenName' nsID f =     getCurrentElement
                             >>> arr XML.elChildren
                             >>> iterateS (keepingTheValue (elemNameMatches nsID f))
                             >>> arr (map fst . filter snd)

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

--
isSet'                   :: (NameSpaceID nsID)
                         => nsID -> AttributeName
                         -> XMLConverter nsID extraState x (Maybe Bool)
isSet' nsID attrName     =     findAttr' nsID attrName
                           >>^ (>>= stringToBool')

isSetWithDefault         :: (NameSpaceID nsID)
                         => nsID -> AttributeName
                         -> Bool
                         -> XMLConverter nsID extraState x Bool
isSetWithDefault nsID attrName def'
                         =     isSet' nsID attrName
                           >>^ fromMaybe def'

-- | Lookup value in a dictionary, fail if no attribute found or value
-- not in dictionary
searchAttrIn             :: (NameSpaceID nsID)
                         => nsID -> AttributeName
                         -> [(AttributeValue,a)]
                         -> FallibleXMLConverter nsID extraState x a
searchAttrIn nsID attrName dict
                         =       findAttr nsID attrName
                           >>?^? maybeToChoice.(`lookup` dict )

-- | Lookup value in a dictionary. If attribute or value not found,
-- return default value
searchAttr               :: (NameSpaceID nsID)
                         => nsID -> AttributeName
                         -> a
                         -> [(AttributeValue,a)]
                         -> XMLConverter nsID extraState x a
searchAttr nsID attrName defV dict
                         =     searchAttrIn nsID attrName dict
                           >>> const defV ^|||^ id

-- | Read a 'Lookupable' attribute. Fail if no match.
lookupAttr               :: (NameSpaceID nsID, Lookupable a)
                         => nsID -> AttributeName
                         -> FallibleXMLConverter nsID extraState x a
lookupAttr nsID attrName =     lookupAttr' nsID attrName
                           >>^ maybeToChoice


-- | Read a 'Lookupable' attribute. Return the result as a 'Maybe'.
lookupAttr'              :: (NameSpaceID nsID, Lookupable a)
                         => nsID -> AttributeName
                         -> XMLConverter nsID extraState x (Maybe a)
lookupAttr' nsID attrName
                         =     findAttr' nsID attrName
                           >>^ (>>= readLookupable)

-- | Read a 'Lookupable' attribute with explicit default
lookupAttrWithDefault    :: (NameSpaceID nsID, Lookupable a)
                         => nsID -> AttributeName
                         -> a
                         -> XMLConverter nsID extraState x a
lookupAttrWithDefault nsID attrName deflt
                         =     lookupAttr' nsID attrName
                           >>^ fromMaybe deflt

-- | Read a 'Lookupable' attribute with implicit default
lookupDefaultingAttr     :: (NameSpaceID nsID, Lookupable a, Default a)
                         => nsID -> AttributeName
                         -> XMLConverter nsID extraState x a
lookupDefaultingAttr nsID attrName
                         = lookupAttrWithDefault nsID attrName def

-- | Return value as a (Maybe String)
findAttr'               :: (NameSpaceID nsID)
                        => nsID -> AttributeName
                        -> XMLConverter nsID extraState x (Maybe AttributeValue)
findAttr' nsID attrName =         qualifyName nsID attrName
                              &&& getCurrentElement
                          >>% XML.findAttr

-- | Return value as a (Maybe Text)
findAttrText'           :: (NameSpaceID nsID)
                        => nsID -> AttributeName
                        -> XMLConverter nsID extraState x (Maybe TextAttributeValue)
findAttrText' nsID attrName
                        =         qualifyName nsID attrName
                              &&& getCurrentElement
                          >>% XML.findAttr
                          >>^ fmap T.pack

-- | Return value as string or fail
findAttr               :: (NameSpaceID nsID)
                       => nsID -> AttributeName
                       -> FallibleXMLConverter nsID extraState x AttributeValue
findAttr nsID attrName =     findAttr' nsID attrName
                         >>> maybeToChoice

-- | Return value as text or fail
findAttrText           :: (NameSpaceID nsID)
                       => nsID -> AttributeName
                       -> FallibleXMLConverter nsID extraState x TextAttributeValue
findAttrText nsID attrName
                       = findAttr' nsID attrName
                         >>^ fmap T.pack
                         >>> maybeToChoice

-- | Return value as string or return provided default value
findAttrWithDefault    :: (NameSpaceID nsID)
                       => nsID -> AttributeName
                       -> AttributeValue
                       -> XMLConverter nsID extraState x AttributeValue
findAttrWithDefault nsID attrName deflt
                       = findAttr' nsID attrName
                         >>^ fromMaybe deflt

-- | Return value as string or return provided default value
findAttrTextWithDefault :: (NameSpaceID nsID)
                        => nsID -> AttributeName
                        -> TextAttributeValue
                        -> XMLConverter nsID extraState x TextAttributeValue
findAttrTextWithDefault nsID attrName deflt
                       = findAttr' nsID attrName
                         >>^ maybe deflt T.pack

-- | Read and return value or fail
readAttr               :: (NameSpaceID nsID, Read attrValue)
                       => nsID -> AttributeName
                       -> FallibleXMLConverter nsID extraState x attrValue
readAttr nsID attrName =     readAttr' nsID attrName
                         >>> maybeToChoice

-- | Read and return value or return Nothing
readAttr'              :: (NameSpaceID nsID, Read attrValue)
                       => nsID -> AttributeName
                       -> XMLConverter nsID extraState x (Maybe attrValue)
readAttr' nsID attrName =     findAttr' nsID attrName
                          >>^ (>>= tryToRead)

-- | Read and return value or return provided default value
readAttrWithDefault    :: (NameSpaceID nsID, Read attrValue)
                       => nsID -> AttributeName
                       -> attrValue
                       -> XMLConverter nsID extraState x attrValue
readAttrWithDefault nsID attrName deflt
                       =     findAttr' nsID attrName
                         >>^ (>>= tryToRead)
                         >>^ fromMaybe deflt

-- | Read and return value or return default value from 'Default' instance
getAttr                :: (NameSpaceID nsID, Read attrValue, Default attrValue)
                       => nsID -> AttributeName
                       -> XMLConverter nsID extraState x attrValue
getAttr nsID attrName  = readAttrWithDefault nsID attrName def

--------------------------------------------------------------------------------
-- Movements
--------------------------------------------------------------------------------

--
jumpThere              :: XMLConverter nsID extraState XML.Element XML.Element
jumpThere              = withState (\state element
                                     -> ( pushElement element state , element )
                                   )

--
swapStack             :: XMLConverter nsID extraState [XML.Element] [XML.Element]
swapStack             = withState swapStack'

--
jumpBack               :: FallibleXMLConverter nsID extraState _x _x
jumpBack               = tryModifyState (popElement >>> maybeToChoice)

-- | Support function for "procedural" converters: jump to an element, execute
-- a converter, jump back.
-- This version is safer than 'executeThere', because it does not rely on the
-- internal stack. As a result, the converter can not move around in arbitrary
-- ways. The downside is of course that some of the environment is not
-- accessible to the converter.
switchingTheStack      :: XMLConverter nsID moreState a b
                       -> XMLConverter nsID moreState (a, XML.Element) b
switchingTheStack a    =     second ( (:[]) ^>> swapStack )
                         >>> first  a
                         >>> second swapStack
                         >>^ fst

-- | Support function for "procedural" converters: jumps to an element, executes
-- a converter, jumps back.
-- Make sure that the converter is well-behaved; that is it should
-- return to the exact position it started from in /every possible path/ of
-- execution, even if it "fails". If it does not, you may encounter
-- strange bugs. If you are not sure about the behaviour or want to use
-- shortcuts, you can often use 'switchingTheStack' instead.
executeThere           :: FallibleXMLConverter nsID moreState a b
                       -> FallibleXMLConverter nsID moreState (a, XML.Element) b
executeThere a         =      second jumpThere
                          >>> fst
                          ^>> a
                          >>> jumpBack -- >>? jumpBack  would not ensure the jump.
                          >>^ collapseEither


-- | Do something in a specific element, then come back
executeIn   :: XMLConverter nsID extraState XML.Element s
            -> XMLConverter nsID extraState XML.Element s
executeIn a = duplicate >>> switchingTheStack a

-- | Do something in a sub-element, then come back
executeInSub              :: (NameSpaceID nsID)
                          => nsID -> ElementName
                          -> FallibleXMLConverter nsID extraState f s
                          -> FallibleXMLConverter nsID extraState f s
executeInSub nsID name a  =     keepingTheValue
                                  (findChild nsID name)
                            >>> ignoringState liftFailure
                            >>? switchingTheStack a
  where liftFailure (_, Left  f) = Left  f
        liftFailure (x, Right e) = Right (x, e)

--------------------------------------------------------------------------------
-- Iterating over children
--------------------------------------------------------------------------------

-- Helper converter to prepare different types of iterations.
-- It lifts the children (of a certain type) of the current element
-- into the value level and pairs each one with the current input value.
prepareIteration       :: (NameSpaceID nsID)
                       => nsID -> ElementName
                       -> XMLConverter nsID extraState b [(b, XML.Element)]
prepareIteration nsID name =     keepingTheValue
                                   (findChildren nsID name)
                             >>% distributeValue

--
withEveryL             :: (NameSpaceID nsID)
                       => nsID -> ElementName
                       -> FallibleXMLConverter nsID extraState a  b
                       -> FallibleXMLConverter nsID extraState a [b]
withEveryL = withEvery

-- | Applies a converter to every child element of a specific type.
-- Collects results in a 'MonadPlus'.
-- Fails completely if any conversion fails.
withEvery              :: (NameSpaceID nsID, MonadPlus m)
                       => nsID -> ElementName
                       -> FallibleXMLConverter nsID extraState a    b
                       -> FallibleXMLConverter nsID extraState a (m b)
withEvery nsID name a      =     prepareIteration nsID name
                             >>> iterateS' (switchingTheStack a)

-- | Applies a converter to every child element of a specific type.
-- Collects all successful results in a list.
tryAll                 :: (NameSpaceID nsID)
                       => nsID -> ElementName
                       -> FallibleXMLConverter nsID extraState b  a
                       ->         XMLConverter nsID extraState b [a]
tryAll nsID name a         =     prepareIteration nsID name
                             >>> iterateS (switchingTheStack a)
                             >>^ rights

--------------------------------------------------------------------------------
-- Matching children
--------------------------------------------------------------------------------

type IdXMLConverter nsID moreState x
   = XMLConverter   nsID moreState x x

type MaybeCConverter nsID moreState x
   = Maybe (IdXMLConverter nsID moreState (x, XML.Content))

-- Chainable converter that helps deciding which converter to actually use.
type ContentMatchConverter nsID extraState x
   = IdXMLConverter  nsID
                     extraState
                     (MaybeCConverter nsID extraState x, XML.Content)

-- Helper function: The @c@ is actually a converter that is to be selected by
-- matching XML content to the first two parameters.
-- The fold used to match elements however is very simple, so to use it,
-- this function wraps the converter in another converter that unifies
-- the accumulator. Think of a lot of converters with the resulting type
-- chained together. The accumulator not only transports the element
-- unchanged to the next matcher, it also does the actual selecting by
-- combining the intermediate results with '(<|>)'.
makeMatcherC           :: (NameSpaceID nsID)
                       => nsID -> ElementName
                       -> FallibleXMLConverter  nsID extraState a a
                       -> ContentMatchConverter nsID extraState a
makeMatcherC nsID name c = (    second (    contentToElem
                                         >>> returnV Nothing
                                         ||| (    elemNameIs nsID name
                                              >>^ bool Nothing (Just cWithJump)
                                             )
                                        )
                             >>% (<|>)
                           ) &&&^ snd
  where cWithJump =      ( fst
                           ^&&& (      second contentToElem
                                  >>>  spreadChoice
                                  ^>>? executeThere c
                                )
                            >>% recover)
                    &&&^ snd
        contentToElem :: FallibleXMLConverter nsID extraState XML.Content XML.Element
        contentToElem = arr $ \e -> case e of
                                     XML.Elem e' -> succeedWith e'
                                     _           -> failEmpty

-- Creates and chains a bunch of matchers
prepareMatchersC      :: (NameSpaceID nsID)
                       => [(nsID, ElementName, FallibleXMLConverter nsID extraState x x)]
                       -> ContentMatchConverter nsID extraState x
--prepareMatchersC      = foldSs . (map $ uncurry3  makeMatcherC)
prepareMatchersC      = reverseComposition . map (uncurry3  makeMatcherC)

-- | Takes a list of element-data - converter groups and
-- * Finds all content of the current element
-- * Matches each group to each piece of content in order
--   (at most one group per piece of content)
-- * Filters non-matched content
-- * Chains all found converters in content-order
-- * Applies the chain to the input element
matchContent'           :: (NameSpaceID nsID)
                       => [(nsID, ElementName, FallibleXMLConverter nsID extraState a a)]
                       -> XMLConverter nsID extraState a a
matchContent' lookups   = matchContent lookups (arr fst)

-- | Takes a list of element-data - converter groups and
-- * Finds all content of the current element
-- * Matches each group to each piece of content in order
--   (at most one group per piece of content)
-- * Adds a default converter for all non-matched content
-- * Chains all found converters in content-order
-- * Applies the chain to the input element
matchContent          :: (NameSpaceID nsID)
                       => [(nsID, ElementName, FallibleXMLConverter nsID extraState a a)]
                       -> XMLConverter nsID extraState (a,XML.Content) a
                       -> XMLConverter nsID extraState a a
matchContent lookups fallback
                        = let matcher = prepareMatchersC lookups
                          in  keepingTheValue (
                                   elContent
                               >>> map (Nothing,)
                               ^>> iterateSL matcher
                               >>^ map swallowOrFallback
                              -- >>> foldSs
                               >>> reverseComposition
                             )
                         >>> swap
                         ^>> app
  where
        -- let the converter swallow the content and drop the content
        -- in the return value
        swallowOrFallback (Just converter,content) = (,content) ^>> converter >>^ fst
        swallowOrFallback (Nothing       ,content) = (,content) ^>> fallback

--------------------------------------------------------------------------------
-- Internals
--------------------------------------------------------------------------------

stringToBool' :: String -> Maybe Bool
stringToBool' val | val `elem` trueValues  = Just True
                  | val `elem` falseValues = Just False
                  | otherwise              = Nothing
  where trueValues  = ["true" ,"on" ,"1"]
        falseValues = ["false","off","0"]


distributeValue ::  a -> [b] -> [(a,b)]
distributeValue = map.(,)

--------------------------------------------------------------------------------

{-
NOTES
It might be a good idea to refactor the namespace stuff.
E.g.: if a namespace constructor took a string as a parameter, things like
> a ?>/< (NsText,"body")
would be nicer.
Together with a rename and some trickery, something like
> |< NsText "body" >< NsText "p" ?> a </> </>|
might even be possible.

Some day, XML.Light should be replaced by something better.
While doing that, it might be useful to replace String as the type of element
names with something else, too. (Of course with OverloadedStrings).
While doing that, maybe the types can be created in a way that something like
> NsText:"body"
could be used. Overloading (:) does not sounds like the best idea, but if the
element name type was a list, this might be possible.
Of course that would be a bit hackish, so the "right" way would probably be
something like
> InNS NsText "body"
but isn't that a bit boring? ;)
-}
