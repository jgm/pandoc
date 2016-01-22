{-# LANGUAGE Arrows          #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

{-
Copyright (C) 2015 Martin Linnemann <theCodingMarlin@googlemail.com>

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

{- |
   Module      : Text.Pandoc.Readers.Odt.ContentReader
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

The core of the odt reader that converts odt features into Pandoc types.
-}

module Text.Pandoc.Readers.Odt.ContentReader
( readerState
, read_body
) where

import           Control.Arrow
import           Control.Applicative    hiding ( liftA, liftA2, liftA3 )

import qualified Data.Map               as M
import           Data.List                     ( find )
import           Data.Maybe

import qualified Text.XML.Light         as XML

import           Text.Pandoc.Definition
import           Text.Pandoc.Builder
import           Text.Pandoc.Shared

import           Text.Pandoc.Readers.Odt.Base
import           Text.Pandoc.Readers.Odt.Namespaces
import           Text.Pandoc.Readers.Odt.StyleReader

import           Text.Pandoc.Readers.Odt.Arrows.Utils
import           Text.Pandoc.Readers.Odt.Generic.XMLConverter
import           Text.Pandoc.Readers.Odt.Generic.Fallible
import           Text.Pandoc.Readers.Odt.Generic.Utils

import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

type Anchor = String

data ReaderState
   = ReaderState { -- | A collection of styles read somewhere else.
                   -- It is only queried here, not modified.
                   styleSet         :: Styles
                   -- | A stack of the styles of parent elements.
                   -- Used to look up inherited style properties.
                 , styleTrace       :: [Style]
                   -- | Keeps track of the current depth in nested lists
                 , currentListLevel :: ListLevel
                   -- | Lists may provide their own style, but they don't have
                   -- to. If they do not, the style of a parent list may be used
                   -- or even a default list style from the paragraph style.
                   -- This value keeps track of the closest list style there
                   -- currently is.
                 , currentListStyle :: Maybe ListStyle
                   -- | A map from internal anchor names to "pretty" ones.
                   -- The mapping is a purely cosmetic one.
                 , bookmarkAnchors  :: M.Map Anchor Anchor

--               , sequences
--               , trackedChangeIDs
                 }
  deriving ( Show )

readerState :: Styles -> ReaderState
readerState styles = ReaderState styles [] 0 Nothing M.empty

--
pushStyle'  :: Style -> ReaderState -> ReaderState
pushStyle' style state = state { styleTrace = style : styleTrace state }

--
popStyle'   :: ReaderState -> ReaderState
popStyle' state = case styleTrace state of
                   _:trace -> state  { styleTrace = trace  }
                   _       -> state

--
modifyListLevel :: (ListLevel -> ListLevel) -> (ReaderState -> ReaderState)
modifyListLevel f state = state { currentListLevel = f (currentListLevel state) }

--
shiftListLevel :: ListLevel -> (ReaderState -> ReaderState)
shiftListLevel diff = modifyListLevel (+ diff)

--
swapCurrentListStyle :: Maybe ListStyle -> ReaderState
                     -> (ReaderState, Maybe ListStyle)
swapCurrentListStyle mListStyle state = ( state { currentListStyle = mListStyle }
                                        ,  currentListStyle state
                                        )

--
lookupPrettyAnchor :: Anchor -> ReaderState -> Maybe Anchor
lookupPrettyAnchor anchor ReaderState{..} = M.lookup anchor bookmarkAnchors

--
putPrettyAnchor :: Anchor -> Anchor -> ReaderState -> ReaderState
putPrettyAnchor ugly pretty state@ReaderState{..}
  = state { bookmarkAnchors = M.insert ugly pretty bookmarkAnchors }

--
usedAnchors :: ReaderState -> [Anchor]
usedAnchors ReaderState{..} = M.elems bookmarkAnchors

--------------------------------------------------------------------------------
-- Reader type and associated tools
--------------------------------------------------------------------------------

type OdtReader      a b = XMLReader     ReaderState a b

type OdtReaderSafe  a b = XMLReaderSafe ReaderState a b

-- | Extract something from the styles
fromStyles :: (a -> Styles -> b) -> OdtReaderSafe a b
fromStyles f =     keepingTheValue
                     (getExtraState >>^ styleSet)
               >>% f

--
getStyleByName :: OdtReader StyleName Style
getStyleByName = fromStyles lookupStyle >>^ maybeToChoice

--
findStyleFamily :: OdtReader Style StyleFamily
findStyleFamily = fromStyles getStyleFamily >>^ maybeToChoice

--
lookupListStyle :: OdtReader StyleName ListStyle
lookupListStyle = fromStyles lookupListStyleByName >>^ maybeToChoice

--
switchCurrentListStyle :: OdtReaderSafe (Maybe ListStyle) (Maybe ListStyle)
switchCurrentListStyle =     keepingTheValue getExtraState
                         >>% swapCurrentListStyle
                         >>> first setExtraState
                         >>^ snd

--
pushStyle :: OdtReaderSafe Style Style
pushStyle =     keepingTheValue (
                  (     keepingTheValue getExtraState
                    >>% pushStyle'
                  )
                  >>> setExtraState
                )
            >>^ fst

--
popStyle :: OdtReaderSafe x x
popStyle =     keepingTheValue (
                     getExtraState
                 >>> arr popStyle'
                 >>> setExtraState
               )
           >>^ fst

--
getCurrentListLevel :: OdtReaderSafe _x ListLevel
getCurrentListLevel = getExtraState >>^ currentListLevel


type AnchorPrefix = String

-- | An adaptation of 'uniqueIdent' from "Text.Pandoc.Shared" that generates a
-- unique identifier but without assuming that the id should be for a header.
-- Second argument is a list of already used identifiers.
uniqueIdentFrom :: AnchorPrefix -> [Anchor] -> Anchor
uniqueIdentFrom baseIdent usedIdents =
  let  numIdent n = baseIdent ++ "-" ++ show n
  in  if baseIdent `elem` usedIdents
        then case find (\x -> numIdent x `notElem` usedIdents) ([1..60000] :: [Int]) of
                  Just x  -> numIdent x
                  Nothing -> baseIdent   -- if we have more than 60,000, allow repeats
        else baseIdent

-- | First argument: basis for a new "pretty" anchor if none exists yet
-- Second argument: a key ("ugly" anchor)
-- Returns: saved "pretty" anchor or created new one
getPrettyAnchor :: OdtReaderSafe (AnchorPrefix, Anchor) Anchor
getPrettyAnchor = proc (baseIdent, uglyAnchor) -> do
  state <- getExtraState -< ()
  case lookupPrettyAnchor uglyAnchor state of
    Just prettyAnchor -> returnA -< prettyAnchor
    Nothing           -> do
      let newPretty = uniqueIdentFrom baseIdent (usedAnchors state)
      modifyExtraState (putPrettyAnchor uglyAnchor newPretty) -<< newPretty

-- | Input: basis for a new header anchor
-- Ouput: saved new anchor
getHeaderAnchor :: OdtReaderSafe Inlines Anchor
getHeaderAnchor = proc title -> do
  state <- getExtraState -< ()
  let anchor = uniqueIdent (toList title) (Set.fromList $ usedAnchors state)
  modifyExtraState (putPrettyAnchor anchor anchor) -<< anchor


--------------------------------------------------------------------------------
-- Working with styles
--------------------------------------------------------------------------------

--
readStyleByName :: OdtReader _x Style
readStyleByName = findAttr NsText "style-name" >>? getStyleByName

--
isStyleToTrace :: OdtReader Style Bool
isStyleToTrace = findStyleFamily >>?^ (==FaText)

--
withNewStyle :: OdtReaderSafe x Inlines -> OdtReaderSafe x Inlines
withNewStyle a = proc x -> do
  fStyle <- readStyleByName -< ()
  case fStyle of
    Right style -> do
      mFamily    <- arr styleFamily -< style
      fTextProps <- arr ( maybeToChoice
                        . textProperties
                        . styleProperties
                        )           -< style
      case fTextProps of
        Right textProps -> do
          state        <- getExtraState             -< ()
          let triple = (state, textProps, mFamily)
          modifier     <- arr modifierFromStyleDiff -< triple
          fShouldTrace <- isStyleToTrace            -< style
          case fShouldTrace of
            Right shouldTrace -> do
              if shouldTrace
                then do
                  pushStyle      -< style
                  inlines   <- a -< x
                  popStyle       -< ()
                  arr modifier   -<< inlines
                else
    -- In case anything goes wrong
                      a -< x
            Left _ -> a -< x
        Left _     -> a -< x
    Left _         -> a -< x


type PropertyTriple = (ReaderState, TextProperties, Maybe StyleFamily)
type InlineModifier = Inlines -> Inlines

-- | Given data about the local style changes, calculates how to modify
-- an instance of 'Inlines'
modifierFromStyleDiff :: PropertyTriple -> InlineModifier
modifierFromStyleDiff propertyTriple  =
  composition $
  (getVPosModifier propertyTriple)
  : map (first ($ propertyTriple) >>> ifThen_else ignore)
        [ (hasEmphChanged           , emph      )
        , (hasChanged isStrong      , strong    )
        , (hasChanged strikethrough , strikeout )
        ]
  where
    ifThen_else else' (if',then') = if if' then then' else else'

    ignore = id :: InlineModifier

    getVPosModifier :: PropertyTriple -> InlineModifier
    getVPosModifier triple@(_,textProps,_) =
        let getVPos = Just . verticalPosition
        in  case lookupPreviousValueM getVPos triple of
              Nothing      -> ignore
              Just oldVPos -> getVPosModifier' (oldVPos,verticalPosition textProps)

    getVPosModifier' (oldVPos , newVPos   ) | oldVPos == newVPos = ignore
    getVPosModifier' ( _      , VPosSub   )                      = subscript
    getVPosModifier' ( _      , VPosSuper )                      = superscript
    getVPosModifier' ( _      ,  _        )                      = ignore

    hasEmphChanged :: PropertyTriple -> Bool
    hasEmphChanged = swing any [ hasChanged  isEmphasised
                               , hasChangedM pitch
                               , hasChanged  underline
                               ]

    hasChanged property triple@(_, property -> newProperty, _) =
        maybe True (/=newProperty) (lookupPreviousValue property triple)

    hasChangedM property triple@(_, textProps,_) =
      fromMaybe False $ (/=) <$> property textProps <*> lookupPreviousValueM property triple

    lookupPreviousValue f = lookupPreviousStyleValue ((fmap f).textProperties)

    lookupPreviousValueM f = lookupPreviousStyleValue ((f =<<).textProperties)

    lookupPreviousStyleValue f (ReaderState{..},_,mFamily)
      =     ( findBy f $ extendedStylePropertyChain styleTrace styleSet )
        <|> ( f =<< fmap (lookupDefaultStyle' styleSet) mFamily         )


type ParaModifier = Blocks -> Blocks

_MINIMUM_INDENTATION_FOR_BLOCKQUOTES_IN_MM_      :: Int
_MINIMUM_INDENTATION_FOR_BLOCKQUOTES_IN_PERCENT_ :: Int
_MINIMUM_INDENTATION_FOR_BLOCKQUOTES_IN_MM_      = 5
_MINIMUM_INDENTATION_FOR_BLOCKQUOTES_IN_PERCENT_ = 5

-- | Returns either 'id' or 'blockQuote' depending on the current indentation
getParaModifier :: Style -> ParaModifier
getParaModifier Style{..} | Just props <- paraProperties styleProperties
                          , isBlockQuote (indentation props)
                                         (margin_left props)
                          = blockQuote
                          | otherwise
                          = id
  where
  isBlockQuote mIndent mMargin
    | LengthValueMM indent <- mIndent
    ,  indent          > _MINIMUM_INDENTATION_FOR_BLOCKQUOTES_IN_MM_
     = True
    | LengthValueMM margin <- mMargin
    ,           margin > _MINIMUM_INDENTATION_FOR_BLOCKQUOTES_IN_MM_
     = True
    | LengthValueMM indent <- mIndent
    , LengthValueMM margin <- mMargin
     = indent + margin > _MINIMUM_INDENTATION_FOR_BLOCKQUOTES_IN_MM_

    | PercentValue  indent <- mIndent
    ,  indent          > _MINIMUM_INDENTATION_FOR_BLOCKQUOTES_IN_PERCENT_
     = True
    | PercentValue  margin <- mMargin
    ,           margin > _MINIMUM_INDENTATION_FOR_BLOCKQUOTES_IN_PERCENT_
     = True
    | PercentValue  indent <- mIndent
    , PercentValue  margin <- mMargin
     = indent + margin > _MINIMUM_INDENTATION_FOR_BLOCKQUOTES_IN_PERCENT_

    | otherwise
     = False

--
constructPara :: OdtReaderSafe Blocks Blocks -> OdtReaderSafe Blocks Blocks
constructPara reader = proc blocks -> do
  fStyle <- readStyleByName -< blocks
  case fStyle of
    Left   _    -> reader -< blocks
    Right style -> do
      let modifier = getParaModifier style
      blocks' <- reader -< blocks
      arr modifier -<< blocks'



type ListConstructor = [Blocks] -> Blocks

getListConstructor :: ListLevelStyle -> ListConstructor
getListConstructor ListLevelStyle{..} =
  case listLevelType of
    LltBullet   -> bulletList
    LltImage    -> bulletList
    LltNumbered -> let listNumberStyle = toListNumberStyle listItemFormat
                       listNumberDelim = toListNumberDelim listItemPrefix
                                                           listItemSuffix
                   in  orderedListWith (1, listNumberStyle, listNumberDelim)
  where
    toListNumberStyle  LinfNone      = DefaultStyle
    toListNumberStyle  LinfNumber    = Decimal
    toListNumberStyle  LinfRomanLC   = LowerRoman
    toListNumberStyle  LinfRomanUC   = UpperRoman
    toListNumberStyle  LinfAlphaLC   = LowerAlpha
    toListNumberStyle  LinfAlphaUC   = UpperAlpha
    toListNumberStyle (LinfString _) = Example

    toListNumberDelim  Nothing   (Just ".") = Period
    toListNumberDelim (Just "" ) (Just ".") = Period
    toListNumberDelim  Nothing   (Just ")") = OneParen
    toListNumberDelim (Just "" ) (Just ")") = OneParen
    toListNumberDelim (Just "(") (Just ")") = TwoParens
    toListNumberDelim     _          _      = DefaultDelim


-- | Determines which style to use for a list, which level to use of that
-- style, and which type of list to create as a result of this information.
-- Then prepares the state for eventual child lists and constructs the list from
-- the results.
-- Two main cases are handled: The list may provide its own style or it may
-- rely on a parent list's style. I the former case the current style in the
-- state must be switched before and after the call to the child converter
-- while in the latter the child converter can be called directly.
-- If anything goes wrong, a default ordered-list-constructor is used.
constructList :: OdtReaderSafe x [Blocks] -> OdtReaderSafe x Blocks
constructList reader = proc x -> do
  modifyExtraState (shiftListLevel 1)        -< ()
  listLevel  <- getCurrentListLevel          -< ()
  fStyleName <- findAttr NsText "style-name" -< ()
  case fStyleName of
    Right styleName -> do
      fListStyle <- lookupListStyle -< styleName
      case fListStyle of
        Right listStyle -> do
          fLLS <- arr (uncurry getListLevelStyle) -< (listLevel,listStyle)
          case fLLS of
            Just listLevelStyle -> do
              oldListStyle <- switchCurrentListStyle           -<  Just listStyle
              blocks       <- constructListWith listLevelStyle -<< x
              switchCurrentListStyle                           -<  oldListStyle
              returnA                                          -<  blocks
            Nothing             -> constructOrderedList        -< x
        Left _                  -> constructOrderedList        -< x
    Left _ -> do
      state      <- getExtraState        -< ()
      mListStyle <- arr currentListStyle -< state
      case mListStyle of
        Just listStyle -> do
          fLLS <- arr (uncurry getListLevelStyle) -< (listLevel,listStyle)
          case fLLS of
            Just listLevelStyle -> constructListWith listLevelStyle -<< x
            Nothing             -> constructOrderedList             -<  x
        Nothing                 -> constructOrderedList             -<  x
  where
    constructOrderedList =
          reader
      >>> modifyExtraState (shiftListLevel (-1))
      >>^ orderedList
    constructListWith listLevelStyle =
          reader
      >>> getListConstructor listLevelStyle
      ^>> modifyExtraState (shiftListLevel (-1))

--------------------------------------------------------------------------------
-- Readers
--------------------------------------------------------------------------------

type ElementMatcher result = (Namespace, ElementName, OdtReader result result)

type InlineMatcher = ElementMatcher Inlines

type BlockMatcher  = ElementMatcher Blocks


--
matchingElement :: (Monoid e)
                => Namespace -> ElementName
                -> OdtReaderSafe  e e
                -> ElementMatcher e
matchingElement ns name reader = (ns, name, asResultAccumulator reader)
  where
   asResultAccumulator :: (ArrowChoice a, Monoid m) => a m m -> a m (Fallible m)
   asResultAccumulator a = liftAsSuccess $ keepingTheValue a >>% (<>)

--
matchChildContent'   :: (Monoid result)
                     => [ElementMatcher result]
                     ->  OdtReaderSafe _x result
matchChildContent' ls = returnV mempty >>> matchContent' ls

--
matchChildContent    :: (Monoid result)
                     => [ElementMatcher result]
                     ->  OdtReaderSafe  (result, XML.Content) result
                     ->  OdtReaderSafe _x result
matchChildContent ls fallback = returnV mempty >>> matchContent ls fallback


--------------------------------------------
-- Matchers
--------------------------------------------

----------------------
-- Basics
----------------------

--
-- | Open Document allows several consecutive spaces if they are marked up
read_plain_text :: OdtReaderSafe (Inlines, XML.Content) Inlines
read_plain_text =  fst ^&&& read_plain_text' >>% recover
  where
    -- fallible version
    read_plain_text' :: OdtReader (Inlines, XML.Content) Inlines
    read_plain_text' =      (     second ( arr extractText )
                              >>^ spreadChoice >>?! second text
                            )
                       >>?% (<>)
    --
    extractText     :: XML.Content -> Fallible String
    extractText (XML.Text cData) = succeedWith (XML.cdData cData)
    extractText         _        = failEmpty


-- specifically. I honor that, although the current implementation of '(<>)'
-- for 'Inlines' in "Text.Pandoc.Builder" will collaps them agein.
-- The rational is to be prepared for future modifications.
read_spaces      :: InlineMatcher
read_spaces       = matchingElement NsText "s" (
                          readAttrWithDefault NsText "c" 1 -- how many spaces?
                      >>^ fromList.(`replicate` Space)
                    )
--
read_line_break  :: InlineMatcher
read_line_break   = matchingElement NsText "line-break"
                    $ returnV linebreak

--
read_span        :: InlineMatcher
read_span         = matchingElement NsText "span"
                    $ withNewStyle
                    $ matchChildContent [ read_span
                                        , read_spaces
                                        , read_line_break
                                        , read_link
                                        , read_note
                                        , read_citation
                                        , read_bookmark
                                        , read_bookmark_start
                                        , read_reference_start
                                        , read_bookmark_ref
                                        , read_reference_ref
                                        ] read_plain_text

--
read_paragraph   :: BlockMatcher
read_paragraph    = matchingElement NsText "p"
                    $ constructPara
                    $ liftA para
                    $ withNewStyle
                    $ matchChildContent [ read_span
                                        , read_spaces
                                        , read_line_break
                                        , read_link
                                        , read_note
                                        , read_citation
                                        , read_bookmark
                                        , read_bookmark_start
                                        , read_reference_start
                                        , read_bookmark_ref
                                        , read_reference_ref
                                        ] read_plain_text


----------------------
-- Headers
----------------------

--
read_header      :: BlockMatcher
read_header       = matchingElement NsText "h"
                    $  proc blocks -> do
  level    <- ( readAttrWithDefault NsText "outline-level" 1
              ) -< blocks
  children <- ( matchChildContent [ read_span
                                  , read_spaces
                                  , read_line_break
                                  , read_link
                                  , read_note
                                  , read_citation
                                  , read_bookmark
                                  , read_bookmark_start
                                  , read_reference_start
                                  , read_bookmark_ref
                                  , read_reference_ref
                                  ] read_plain_text
              ) -< blocks
  anchor   <- getHeaderAnchor -< children
  let idAttr = (anchor, [], []) -- no classes, no key-value pairs
  arr (uncurry3 headerWith) -< (idAttr, level, children)

----------------------
-- Lists
----------------------

--
read_list        :: BlockMatcher
read_list         = matchingElement NsText "list"
--                  $ withIncreasedListLevel
                    $ constructList
--                  $ liftA bulletList
                    $ matchChildContent' [ read_list_item
                                         ]
--
read_list_item   :: ElementMatcher [Blocks]
read_list_item    = matchingElement NsText "list-item"
                    $ liftA (compactify'.(:[]))
                      ( matchChildContent' [ read_paragraph
                                           , read_header
                                           , read_list
                                           ]
                      )


----------------------
-- Links
----------------------

read_link        :: InlineMatcher
read_link         = matchingElement NsText "a"
                    $ liftA3 link
                      ( findAttrWithDefault NsXLink  "href"  ""              )
                      ( findAttrWithDefault NsOffice "title" ""              )
                      ( matchChildContent [ read_span
                                          , read_note
                                          , read_citation
                                          , read_bookmark
                                          , read_bookmark_start
                                          , read_reference_start
                                          , read_bookmark_ref
                                          , read_reference_ref
                                          ] read_plain_text                  )


-------------------------
-- Footnotes
-------------------------

read_note        :: InlineMatcher
read_note         = matchingElement NsText "note"
                    $ liftA note
                    $ matchChildContent' [ read_note_body ]

read_note_body   :: BlockMatcher
read_note_body    = matchingElement NsText "note-body"
                    $ matchChildContent' [ read_paragraph ]

-------------------------
-- Citations
-------------------------

read_citation    :: InlineMatcher
read_citation     = matchingElement NsText "bibliography-mark"
                    $ liftA2 cite
                      ( liftA2 makeCitation
                        ( findAttrWithDefault NsText "identifier" ""     )
                        ( readAttrWithDefault NsText "number" 0          )
                      )
                      ( matchChildContent [] read_plain_text                 )
  where
   makeCitation :: String -> Int -> [Citation]
   makeCitation citeId num = [Citation citeId [] [] NormalCitation num 0]


----------------------
-- Tables
----------------------

--
read_table        :: BlockMatcher
read_table         = matchingElement NsTable "table"
                     $ liftA (simpleTable [])
                     $ matchChildContent'  [ read_table_row
                                           ]

--
read_table_row    :: ElementMatcher [[Blocks]]
read_table_row     = matchingElement NsTable "table-row"
                     $ liftA (:[])
                     $ matchChildContent'  [ read_table_cell
                                           ]

--
read_table_cell   :: ElementMatcher [Blocks]
read_table_cell    = matchingElement NsTable "table-cell"
                     $ liftA (compactify'.(:[]))
                     $ matchChildContent' [ read_paragraph
                                          ]

----------------------
-- Internal links
----------------------

_ANCHOR_PREFIX_ :: String
_ANCHOR_PREFIX_ = "anchor"

--
readAnchorAttr :: OdtReader _x Anchor
readAnchorAttr = findAttr NsText "name"

-- | Beware: may fail
findAnchorName :: OdtReader AnchorPrefix Anchor
findAnchorName = (      keepingTheValue readAnchorAttr
                   >>^  spreadChoice
                 ) >>?! getPrettyAnchor


--
maybeAddAnchorFrom :: OdtReader Inlines AnchorPrefix
                   -> OdtReaderSafe Inlines Inlines
maybeAddAnchorFrom anchorReader =
  keepingTheValue (anchorReader >>? findAnchorName >>?^ toAnchorElem)
  >>>
  proc (inlines, fAnchorElem) -> do
  case fAnchorElem of
    Right anchorElem ->
      arr (anchorElem <>) -<< inlines
    Left _ -> returnA -< inlines
  where
    toAnchorElem :: Anchor -> Inlines
    toAnchorElem anchorID = spanWith (anchorID, [], []) mempty
                            -- no classes, no key-value pairs

--
read_bookmark     :: InlineMatcher
read_bookmark      = matchingElement NsText "bookmark"
                     $ maybeAddAnchorFrom (liftAsSuccess $ returnV _ANCHOR_PREFIX_)

--
read_bookmark_start :: InlineMatcher
read_bookmark_start = matchingElement NsText "bookmark-start"
                     $ maybeAddAnchorFrom (liftAsSuccess $ returnV _ANCHOR_PREFIX_)

--
read_reference_start :: InlineMatcher
read_reference_start = matchingElement NsText "reference-mark-start"
                     $ maybeAddAnchorFrom readAnchorAttr

-- | Beware: may fail
findAnchorRef :: OdtReader _x Anchor
findAnchorRef = (      findAttr NsText "ref-name"
                  >>?^ (_ANCHOR_PREFIX_,)
                ) >>?! getPrettyAnchor


--
maybeInAnchorRef :: OdtReaderSafe Inlines Inlines
maybeInAnchorRef = proc inlines -> do
  fRef <- findAnchorRef -< ()
  case fRef of
    Right anchor ->
      arr (toAnchorRef anchor) -<< inlines
    Left _ -> returnA -< inlines
  where
    toAnchorRef :: Anchor -> Inlines -> Inlines
    toAnchorRef anchor = link ('#':anchor) "" -- no title

--
read_bookmark_ref :: InlineMatcher
read_bookmark_ref = matchingElement NsText "bookmark-ref"
                    $    maybeInAnchorRef
                     <<< matchChildContent [] read_plain_text

--
read_reference_ref :: InlineMatcher
read_reference_ref = matchingElement NsText "reference-ref"
                    $    maybeInAnchorRef
                     <<< matchChildContent [] read_plain_text


----------------------
-- Entry point
----------------------

--read_plain_content :: OdtReaderSafe _x Inlines
--read_plain_content = strContent >>^ text

read_text :: OdtReaderSafe _x Pandoc
read_text = matchChildContent' [ read_header
                               , read_paragraph
                               , read_list
                               , read_table
                               ]
            >>^ doc

read_body :: OdtReader _x Pandoc
read_body = executeIn NsOffice "body"
          $ executeIn NsOffice "text"
          $ liftAsSuccess read_text

