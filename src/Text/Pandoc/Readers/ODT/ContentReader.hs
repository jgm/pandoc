{-# LANGUAGE Arrows            #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.ODT.ContentReader
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

The core of the odt reader that converts odt features into Pandoc types.
-}

module Text.Pandoc.Readers.ODT.ContentReader
( readerState
, read_body
) where

import Prelude hiding (Applicative(..))
import Control.Applicative hiding (liftA, liftA2, liftA3)
import Control.Arrow
import Control.Monad ((<=<))

import qualified Data.ByteString.Lazy as B
import Data.Foldable (fold)
import Data.List (find)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe
import Data.Monoid (Alt (..))

import Text.TeXMath (readMathML, writeTeX)
import qualified Text.Pandoc.XML.Light as XML

import Text.Pandoc.Builder hiding (underline)
import Text.Pandoc.MediaBag (MediaBag, insertMedia)
import Text.Pandoc.Shared
import Text.Pandoc.Extensions (extensionsFromList, Extension(..))
import qualified Text.Pandoc.UTF8 as UTF8

import Text.Pandoc.Readers.ODT.Base
import Text.Pandoc.Readers.ODT.Namespaces
import Text.Pandoc.Readers.ODT.StyleReader

import Text.Pandoc.Readers.ODT.Arrows.State (foldS)
import Text.Pandoc.Readers.ODT.Arrows.Utils
import Text.Pandoc.Readers.ODT.Generic.Fallible
import Text.Pandoc.Readers.ODT.Generic.Utils
import Text.Pandoc.Readers.ODT.Generic.XMLConverter

import Network.URI (parseRelativeReference, URI(uriPath))
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

type Anchor = T.Text
type Media = [(FilePath, B.ByteString)]

data ReaderState
   = ReaderState { -- | A collection of styles read somewhere else.
                   -- It is only queried here, not modified.
                   styleSet         :: Styles
                   -- | A stack of the styles of parent elements.
                   -- Used to look up inherited style properties.
                 , styleTrace       :: [Style]
                   -- | Keeps track of the current depth in nested lists
                 , currentListLevel :: ListLevel
                   -- | Keeps track of the previous list start counters,
                   -- so whenever a new list continues numbering,
                   -- we know what number to start from.
                   -- If  list does not continue numbering, the counter
                   -- is being reset.
                 , listContinuationStartCounters :: M.Map ListLevel Int
                   -- | Lists may provide their own style, but they don't have
                   -- to. If they do not, the style of a parent list may be used
                   -- or even a default list style from the paragraph style.
                   -- This value keeps track of the closest list style there
                   -- currently is.
                 , currentListStyle :: Maybe ListStyle
                   -- | A map from internal anchor names to "pretty" ones.
                   -- The mapping is a purely cosmetic one.
                 , bookmarkAnchors  :: M.Map Anchor Anchor
                   -- | A map of files / binary data from the archive
                 , envMedia         :: Media
                   -- | Hold binary resources used in the document
                 , odtMediaBag      :: MediaBag
                 }
  deriving ( Show )

readerState :: Styles -> Media -> ReaderState
readerState styles media = ReaderState styles [] 0 M.empty Nothing M.empty media mempty

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
modifyListContinuationStartCounter :: ListLevel -> Int -> (ReaderState -> ReaderState)
modifyListContinuationStartCounter listLevel count state =
    state { listContinuationStartCounters = M.insert listLevel count (listContinuationStartCounters state) }

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

getMediaBag :: ReaderState -> MediaBag
getMediaBag ReaderState{..} = odtMediaBag

getMediaEnv :: ReaderState -> Media
getMediaEnv ReaderState{..} = envMedia

insertMedia' :: (FilePath, B.ByteString) -> ReaderState ->  ReaderState
insertMedia' (fp, bs) state@ReaderState{..}
  = state { odtMediaBag = insertMedia fp Nothing bs odtMediaBag }

--------------------------------------------------------------------------------
-- Reader type and associated tools
--------------------------------------------------------------------------------

type ODTReader      a b = XMLReader     ReaderState a b

type ODTReaderSafe  a b = XMLReaderSafe ReaderState a b

-- | Extract something from the styles
fromStyles :: (a -> Styles -> b) -> ODTReaderSafe a b
fromStyles f =     keepingTheValue
                     (getExtraState >>^ styleSet)
               >>% f

--
getStyleByName :: ODTReader StyleName Style
getStyleByName = fromStyles lookupStyle >>^ maybeToChoice

--
findStyleFamily :: ODTReader Style StyleFamily
findStyleFamily = fromStyles getStyleFamily >>^ maybeToChoice

--
lookupListStyle :: ODTReader StyleName ListStyle
lookupListStyle = fromStyles lookupListStyleByName >>^ maybeToChoice

--
switchCurrentListStyle :: ODTReaderSafe (Maybe ListStyle) (Maybe ListStyle)
switchCurrentListStyle =     keepingTheValue getExtraState
                         >>% swapCurrentListStyle
                         >>> first setExtraState
                         >>^ snd

--
pushStyle :: ODTReaderSafe Style Style
pushStyle =     keepingTheValue (
                  (     keepingTheValue getExtraState
                    >>% pushStyle'
                  )
                  >>> setExtraState
                )
            >>^ fst

--
popStyle :: ODTReaderSafe x x
popStyle =     keepingTheValue (
                     getExtraState
                 >>> arr popStyle'
                 >>> setExtraState
               )
           >>^ fst

--
getCurrentListLevel :: ODTReaderSafe _x ListLevel
getCurrentListLevel = getExtraState >>^ currentListLevel

--
getListContinuationStartCounters :: ODTReaderSafe _x (M.Map ListLevel Int)
getListContinuationStartCounters = getExtraState >>^ listContinuationStartCounters


--
getPreviousListStartCounter :: ODTReaderSafe ListLevel Int
getPreviousListStartCounter = proc listLevel -> do
    counts <- getListContinuationStartCounters -< ()
    returnA -< M.findWithDefault 0 listLevel counts

--
updateMediaWithResource :: ODTReaderSafe (FilePath, B.ByteString) (FilePath, B.ByteString)
updateMediaWithResource = keepingTheValue (
                 (keepingTheValue getExtraState
                  >>% insertMedia'
                  )
                 >>> setExtraState
               )
           >>^ fst

lookupResource :: ODTReaderSafe FilePath (FilePath, B.ByteString)
lookupResource = proc target -> do
    state <- getExtraState -< ()
    case lookup target (getMediaEnv state) of
      Just bs -> returnV (target, bs) -<< ()
      Nothing -> returnV ("", B.empty) -< ()

type AnchorPrefix = T.Text

-- | An adaptation of 'uniqueIdent' from "Text.Pandoc.Shared" that generates a
-- unique identifier but without assuming that the id should be for a header.
-- Second argument is a list of already used identifiers.
uniqueIdentFrom :: AnchorPrefix -> [Anchor] -> Anchor
uniqueIdentFrom baseIdent usedIdents =
  let  numIdent n = baseIdent <> "-" <> T.pack (show n)
  in  if baseIdent `elem` usedIdents
        then maybe baseIdent numIdent
             $ find (\x -> numIdent x `notElem` usedIdents) ([1..60000] :: [Int])
               -- if we have more than 60,000, allow repeats
        else baseIdent

-- | First argument: basis for a new "pretty" anchor if none exists yet
-- Second argument: a key ("ugly" anchor)
-- Returns: saved "pretty" anchor or created new one
getPrettyAnchor :: ODTReaderSafe (AnchorPrefix, Anchor) Anchor
getPrettyAnchor = proc (baseIdent, uglyAnchor) -> do
  state <- getExtraState -< ()
  case lookupPrettyAnchor uglyAnchor state of
    Just prettyAnchor -> returnA -< prettyAnchor
    Nothing           -> do
      let newPretty = uniqueIdentFrom baseIdent (usedAnchors state)
      modifyExtraState (putPrettyAnchor uglyAnchor newPretty) -<< newPretty

-- | Input: basis for a new header anchor
-- Output: saved new anchor
getHeaderAnchor :: ODTReaderSafe Inlines Anchor
getHeaderAnchor = proc title -> do
  state <- getExtraState -< ()
  let exts = extensionsFromList [Ext_auto_identifiers]
  let anchor = uniqueIdent exts (toList title)
                (Set.fromList $ usedAnchors state)
  modifyExtraState (putPrettyAnchor anchor anchor) -<< anchor


--------------------------------------------------------------------------------
-- Working with styles
--------------------------------------------------------------------------------

--
readStyleByName :: ODTReader _x (StyleName, Style)
readStyleByName =
  findAttr NsText "style-name" >>? keepingTheValue getStyleByName >>^ liftE
  where
    liftE :: (StyleName, Fallible Style) -> Fallible (StyleName, Style)
    liftE (name, Right v) = Right (name, v)
    liftE (_, Left v)     = Left v

--
isStyleToTrace :: ODTReader Style Bool
isStyleToTrace = findStyleFamily >>?^ (==FaText)

--
withNewStyle :: ODTReaderSafe x Inlines -> ODTReaderSafe x Inlines
withNewStyle a = proc x -> do
  fStyle <- readStyleByName -< ()
  case fStyle of
    Right (styleName, _) | isCodeStyle styleName -> do
      inlines <- a -< x
      arr inlineCode -<< inlines
    Right (_, style) -> do
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
            Right shouldTrace ->
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
  where
    isCodeStyle :: StyleName -> Bool
    isCodeStyle "Source_Text" = True
    isCodeStyle _             = False

    inlineCode :: Inlines -> Inlines
    inlineCode = code . T.concat . map stringify . toList

type PropertyTriple = (ReaderState, TextProperties, Maybe StyleFamily)
type InlineModifier = Inlines -> Inlines

-- | Given data about the local style changes, calculates how to modify
-- an instance of 'Inlines'
modifierFromStyleDiff :: PropertyTriple -> InlineModifier
modifierFromStyleDiff propertyTriple  =
  composition $
  getVPosModifier propertyTriple
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
              Just oldVPos -> getVPosModifier' (oldVPos, verticalPosition textProps)

    getVPosModifier' (oldVPos , newVPos   ) | oldVPos == newVPos = ignore
    getVPosModifier' ( _      , VPosSub   ) = subscript
    getVPosModifier' ( _      , VPosSuper ) = superscript
    getVPosModifier' ( _      ,  _        ) = ignore

    hasEmphChanged :: PropertyTriple -> Bool
    hasEmphChanged = swing any [ hasChanged  isEmphasised
                               , hasChangedM pitch
                               , hasChanged  underline
                               ]

    hasChanged property triple@(_, property -> newProperty, _) =
        (/= Just newProperty) (lookupPreviousValue property triple)

    hasChangedM property triple@(_, textProps,_) =
      fromMaybe False $ (/=) <$> property textProps <*> lookupPreviousValueM property triple

    lookupPreviousValue f = lookupPreviousStyleValue (fmap f . textProperties)

    lookupPreviousValueM f = lookupPreviousStyleValue (f <=< textProperties)

    lookupPreviousStyleValue f (ReaderState{..},_,mFamily)
      =     findBy f (extendedStylePropertyChain styleTrace styleSet)
        <|> (f . lookupDefaultStyle' styleSet =<< mFamily)


type ParaModifier = Blocks -> Blocks

_MINIMUM_INDENTATION_FOR_BLOCKQUOTES_IN_MM_      :: Int
_MINIMUM_INDENTATION_FOR_BLOCKQUOTES_IN_PERCENT_ :: Int
_MINIMUM_INDENTATION_FOR_BLOCKQUOTES_IN_MM_      = 5
_MINIMUM_INDENTATION_FOR_BLOCKQUOTES_IN_PERCENT_ = 5

-- | Returns either 'id' or 'blockQuote' depending if any of the StyleProperties
-- are indented at quote level.
getParaModifier :: ListLevel -> [StyleProperties] -> ParaModifier
getParaModifier listLevel props
  | listLevel > 0 = id -- see #9505, list paragraphs need indentation
  | any isBlockQuote props = blockQuote
  | otherwise = id
  where
  isBlockQuote SProps {..} | Just paraProps <- paraProperties
                                    , isQuoteWidth (margin_left paraProps)
                                    = True
                                    | otherwise
                                    = False
  isQuoteWidth mMargin
    | LengthValueMM margin <- mMargin
    ,           margin > _MINIMUM_INDENTATION_FOR_BLOCKQUOTES_IN_MM_
     = True
    | PercentValue  margin <- mMargin
    ,           margin > _MINIMUM_INDENTATION_FOR_BLOCKQUOTES_IN_PERCENT_
     = True
    | otherwise
     = False

--
constructPara :: ODTReaderSafe Blocks Blocks -> ODTReaderSafe Blocks Blocks
constructPara reader = proc blocks -> do
  fStyle <- readStyleByName -< blocks
  case fStyle of
    Left   _    -> reader -< blocks
    Right (styleName, _) | isTableCaptionStyle styleName -> do
      blocks' <- reader   -< blocks
      arr tableCaptionP  -< blocks'
    Right (_, style) -> do
      props <- fromStyles extendedStylePropertyChain -< [style]
      listLevel <- getCurrentListLevel -< ()
      let modifier = getParaModifier listLevel props
      blocks' <- reader   -<  blocks
      arr modifier        -<< blocks'
  where
    isTableCaptionStyle :: StyleName -> Bool
    isTableCaptionStyle "Table" = True
    isTableCaptionStyle _       = False
    tableCaptionP b = divWith ("", ["caption"], []) b

type ListConstructor = [Blocks] -> Blocks

getListConstructor :: ListLevelStyle -> Int -> ListConstructor
getListConstructor ListLevelStyle{..} startNum =
  case listLevelType of
    LltBullet   -> bulletList
    LltImage    -> bulletList
    LltNumbered -> let listNumberStyle = toListNumberStyle listItemFormat
                       listNumberDelim = toListNumberDelim listItemPrefix
                                                           listItemSuffix
                   in  orderedListWith (startNum, listNumberStyle, listNumberDelim)
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
constructList :: ODTReaderSafe x [Blocks] -> ODTReaderSafe x Blocks
constructList reader = proc x -> do
  modifyExtraState (shiftListLevel 1)                                  -< ()
  listLevel                    <- getCurrentListLevel                  -< ()
  listContinuationStartCounter <- getPreviousListStartCounter          -< listLevel
  fStyleName                   <- findAttr NsText "style-name"         -< ()
  fContNumbering               <- findAttr NsText "continue-numbering" -< ()
  listItemCount                <- reader >>^ length                    -< x

  let continueNumbering = case fContNumbering of
                            Right "true" -> True
                            _            -> False

  let startNumForListLevelStyle = listStartingNumber continueNumbering listContinuationStartCounter
  let defaultOrderedListConstructor = constructOrderedList (startNumForListLevelStyle Nothing) listLevel listItemCount

  case fStyleName of
    Right styleName -> do
      fListStyle <- lookupListStyle -< styleName
      case fListStyle of
        Right listStyle -> do
          fListLevelStyle <- arr (uncurry getListLevelStyle) -< (listLevel, listStyle)
          case fListLevelStyle of
            Just listLevelStyle -> do
              let startNum = startNumForListLevelStyle $ Just listLevelStyle
              oldListStyle <- switchCurrentListStyle                    -<  Just listStyle
              blocks       <- constructListWith listLevelStyle startNum listLevel listItemCount -<< x
              switchCurrentListStyle                                    -<  oldListStyle
              returnA                                                   -<  blocks
            Nothing             -> defaultOrderedListConstructor -<< x
        Left _                  -> defaultOrderedListConstructor -<< x
    Left _ -> do
      state      <- getExtraState        -< ()
      mListStyle <- arr currentListStyle -< state
      case mListStyle of
        Just listStyle -> do
          fListLevelStyle <- arr (uncurry getListLevelStyle) -< (listLevel, listStyle)
          case fListLevelStyle of
            Just listLevelStyle -> do
              let startNum = startNumForListLevelStyle $ Just listLevelStyle
              constructListWith listLevelStyle startNum listLevel listItemCount -<< x
            Nothing             -> defaultOrderedListConstructor -<< x
        Nothing                 -> defaultOrderedListConstructor -<< x
  where
    listStartingNumber continueNumbering listContinuationStartCounter mListLevelStyle
      | continueNumbering                = listContinuationStartCounter
      | isJust mListLevelStyle           = listItemStart (fromJust mListLevelStyle)
      | otherwise                        = 1
    constructOrderedList startNum listLevel listItemCount =
          reader
      >>> modifyExtraState (shiftListLevel (-1))
      >>> modifyExtraState (modifyListContinuationStartCounter listLevel (startNum + listItemCount))
      >>^ orderedListWith (startNum, DefaultStyle, DefaultDelim)
    constructListWith listLevelStyle startNum listLevel listItemCount =
          reader
      >>> getListConstructor listLevelStyle startNum
      ^>> modifyExtraState (shiftListLevel (-1))
      >>> modifyExtraState (modifyListContinuationStartCounter listLevel (startNum + listItemCount))

--------------------------------------------------------------------------------
-- Readers
--------------------------------------------------------------------------------

type ElementMatcher result = (Namespace, ElementName, ODTReader result result)

type InlineMatcher = ElementMatcher Inlines

type BlockMatcher  = ElementMatcher Blocks

newtype FirstMatch a = FirstMatch (Alt Maybe a)
  deriving (Foldable, Monoid, Semigroup)

firstMatch :: a -> FirstMatch a
firstMatch = FirstMatch . Alt . Just

--
matchingElement :: (Monoid e)
                => Namespace -> ElementName
                -> ODTReaderSafe  e e
                -> ElementMatcher e
matchingElement ns name reader = (ns, name, asResultAccumulator reader)
  where
   asResultAccumulator :: (ArrowChoice a, Monoid m) => a m m -> a m (Fallible m)
   asResultAccumulator a = liftAsSuccess $ keepingTheValue a >>% mappend

--
matchChildContent'   :: (Monoid result)
                     => [ElementMatcher result]
                     ->  ODTReaderSafe _x result
matchChildContent' ls = returnV mempty >>> matchContent' ls

--
matchChildContent    :: (Monoid result)
                     => [ElementMatcher result]
                     ->  ODTReaderSafe  (result, XML.Content) result
                     ->  ODTReaderSafe _x result
matchChildContent ls fallback = returnV mempty >>> matchContent ls fallback

--------------------------------------------
-- Matchers
--------------------------------------------

----------------------
-- Basics
----------------------

--
-- | Open Document allows several consecutive spaces if they are marked up
read_plain_text :: ODTReaderSafe (Inlines, XML.Content) Inlines
read_plain_text =  fst ^&&& read_plain_text' >>% recover
  where
    -- fallible version
    read_plain_text' :: ODTReader (Inlines, XML.Content) Inlines
    read_plain_text' =      (     second ( arr extractText )
                              >>^ spreadChoice >>?! second text
                            )
                       >>?% mappend
    --
    extractText     :: XML.Content -> Fallible T.Text
    extractText (XML.Text cData) = succeedWith (XML.cdData cData)
    extractText         _        = failEmpty

read_text_seq :: InlineMatcher
read_text_seq  = matchingElement NsText "sequence"
                 $ matchChildContent [] read_plain_text


-- specifically. I honor that, although the current implementation of 'mappend'
-- for 'Inlines' in "Text.Pandoc.Builder" will collapse them again.
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
read_tab         :: InlineMatcher
read_tab          = matchingElement NsText "tab"
                    $ returnV space
--
read_span        :: InlineMatcher
read_span         = matchingElement NsText "span"
                    $ withNewStyle
                    $ matchChildContent [ read_span
                                        , read_spaces
                                        , read_line_break
                                        , read_tab
                                        , read_link
                                        , read_frame
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
                                        , read_tab
                                        , read_link
                                        , read_note
                                        , read_citation
                                        , read_bookmark
                                        , read_bookmark_start
                                        , read_reference_start
                                        , read_bookmark_ref
                                        , read_reference_ref
                                        , read_frame
                                        , read_text_seq
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
                                  , read_tab
                                  , read_link
                                  , read_note
                                  , read_citation
                                  , read_bookmark
                                  , read_bookmark_start
                                  , read_reference_start
                                  , read_bookmark_ref
                                  , read_reference_ref
                                  , read_frame
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
                    $ constructList
                    $ matchChildContent' [ read_list_item
                                         , read_list_header
                                         ]
--
read_list_item   :: ElementMatcher [Blocks]
read_list_item    = read_list_element "list-item"

read_list_header :: ElementMatcher [Blocks]
read_list_header  = read_list_element "list-header"

read_list_element               :: ElementName -> ElementMatcher [Blocks]
read_list_element listElement   = matchingElement NsText listElement
                                  $ liftA (compactify.(:[]))
                                    ( matchChildContent' [ read_paragraph
                                                         , read_header
                                                         , read_list
                                                         , read_section
                                                         ]
                                    )

----------------------
-- Sections
----------------------

read_section :: ElementMatcher Blocks
read_section = matchingElement NsText "section"
                 $ liftA (divWith nullAttr)
                 $ matchChildContent' [ read_paragraph
                                      , read_header
                                      , read_list
                                      , read_table
                                      , read_section
                                      ]


----------------------
-- Links
----------------------

read_link        :: InlineMatcher
read_link         = matchingElement NsText "a"
                    $ liftA3 link
                      ( findAttrTextWithDefault NsXLink  "href"  ""
                        >>> arr fixRelativeLink                              )
                      ( findAttrTextWithDefault NsOffice "title" ""          )
                      ( matchChildContent [ read_span
                                          , read_note
                                          , read_citation
                                          , read_bookmark
                                          , read_bookmark_start
                                          , read_reference_start
                                          , read_bookmark_ref
                                          , read_reference_ref
                                          ] read_plain_text                  )

fixRelativeLink :: T.Text -> T.Text
fixRelativeLink uri =
    case parseRelativeReference (T.unpack uri) of
      Nothing -> uri
      Just u  ->
        case uriPath u of
          '.':'.':'/':xs -> tshow $ u{ uriPath = xs }
          _ -> uri

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
                        ( findAttrTextWithDefault NsText "identifier" "" )
                        ( readAttrWithDefault NsText "number" 0          )
                      )
                      ( matchChildContent [] read_plain_text             )
  where
   makeCitation :: T.Text -> Int -> [Citation]
   makeCitation citeId num = [Citation citeId [] [] NormalCitation num 0]


----------------------
-- Tables
----------------------

--
read_table        :: BlockMatcher
read_table         = matchingElement NsTable "table"
                     $ liftA table'
                     $ (matchChildContent' [read_table_header]) &&&
                       (matchChildContent' [read_table_row])

-- | A table without a caption.
table' :: ([[Cell]], [[Cell]]) -> Blocks
table' (headers, rows) =
  table emptyCaption (replicate numcols defaults) th [tb] tf
  where
    defaults = (AlignDefault, ColWidthDefault)
    numcols = maximum $ map length $ headers ++ rows
    toRow = Row nullAttr
    th = TableHead nullAttr $ map toRow headers
    tb = TableBody nullAttr 0 [] $ map toRow rows
    tf = TableFoot nullAttr []

--
read_table_header :: ElementMatcher [[Cell]]
read_table_header = matchingElement NsTable "table-header-rows"
                      $ matchChildContent' [ read_table_row
                                           ]

--
read_table_row    :: ElementMatcher [[Cell]]
read_table_row     = matchingElement NsTable "table-row"
                     $ liftA (:[])
                     $ matchChildContent'  [ read_table_cell
                                           ]

--
read_table_cell   :: ElementMatcher [Cell]
read_table_cell    = matchingElement NsTable "table-cell"
                     $ liftA3 cell'
                       (readAttrWithDefault NsTable "number-rows-spanned" 1 >>^ RowSpan)
                       (readAttrWithDefault NsTable "number-columns-spanned" 1 >>^ ColSpan)
                     $ matchChildContent' [ read_paragraph
                                          , read_list
                                          ]
  where
    cell' rowSpan colSpan blocks = map (cell AlignDefault rowSpan colSpan) $ compactify [blocks]

----------------------
-- Frames
----------------------

--
read_frame :: InlineMatcher
read_frame = matchingElement NsDraw "frame"
             $ filterChildrenName' NsDraw (`elem` ["image", "object", "text-box"])
           >>> foldS read_frame_child
           >>> arr fold

read_frame_child :: ODTReaderSafe XML.Element (FirstMatch Inlines)
read_frame_child =
  proc child -> case elName child of
    "image"    -> read_frame_img      -< child
    "object"   -> read_frame_mathml   -< child
    "text-box" -> read_frame_text_box -< child
    _          -> returnV mempty      -< ()

read_frame_img :: ODTReaderSafe XML.Element (FirstMatch Inlines)
read_frame_img =
  proc img -> do
    src <- executeIn (findAttr' NsXLink "href") -< img
    case fold src of
      ""   -> returnV mempty -< ()
      src' -> do
        let exts = extensionsFromList [Ext_auto_identifiers]
        resource   <- lookupResource                          -< T.unpack src'
        _          <- updateMediaWithResource                 -< resource
        w          <- findAttrText' NsSVG "width"             -< ()
        h          <- findAttrText' NsSVG "height"            -< ()
        titleNodes <- matchChildContent' [ read_frame_title ] -< ()
        alt        <- matchChildContent [] read_plain_text    -< ()
        arr (firstMatch . uncurry4 imageWith)                 -<
          (image_attributes w h, src', inlineListToIdentifier exts (toList titleNodes), alt)

read_frame_title :: InlineMatcher
read_frame_title = matchingElement NsSVG "title" (matchChildContent [] read_plain_text)

image_attributes :: Maybe T.Text -> Maybe T.Text -> Attr
image_attributes x y =
  ( "", [], dim "width" x ++ dim "height" y)
  where
    dim _ (Just "")   = []
    dim name (Just v) = [(name, v)]
    dim _ Nothing     = []

read_frame_mathml :: ODTReaderSafe XML.Element (FirstMatch Inlines)
read_frame_mathml =
  proc obj -> do
    src <- executeIn (findAttr' NsXLink "href") -< obj
    case fold src of
      ""   -> returnV mempty -< ()
      src' -> do
        let path = T.unpack $
                    fromMaybe src' (T.stripPrefix "./" src') <> "/content.xml"
        (_, mathml) <- lookupResource -< path
        case readMathML (UTF8.toText $ B.toStrict mathml) of
          Left _     -> returnV mempty -< ()
          Right exps -> arr (firstMatch . displayMath . writeTeX) -< exps

read_frame_text_box :: ODTReaderSafe XML.Element (FirstMatch Inlines)
read_frame_text_box = proc box -> do
    paragraphs <- executeIn (matchChildContent' [ read_paragraph ]) -< box
    arr read_img_with_caption -< toList paragraphs

read_img_with_caption :: [Block] -> FirstMatch Inlines
read_img_with_caption (Para [Image attr alt (src,title)] : _) =
  firstMatch $ singleton (Image attr alt (src, "fig:" <> title))   -- no text, default caption
read_img_with_caption (Para (Image attr _ (src,title) : txt) : _) =
  firstMatch $ singleton (Image attr txt (src, "fig:" <> title) )  -- override caption with the text that follows
read_img_with_caption  ( Para (_ : xs) : ys) =
  read_img_with_caption (Para xs : ys)
read_img_with_caption _ =
  mempty

----------------------
-- Internal links
----------------------

_ANCHOR_PREFIX_ :: T.Text
_ANCHOR_PREFIX_ = "anchor"

--
readAnchorAttr :: ODTReader _x Anchor
readAnchorAttr = findAttrText NsText "name"

-- | Beware: may fail
findAnchorName :: ODTReader AnchorPrefix Anchor
findAnchorName = (      keepingTheValue readAnchorAttr
                   >>^  spreadChoice
                 ) >>?! getPrettyAnchor


--
maybeAddAnchorFrom :: ODTReader Inlines AnchorPrefix
                   -> ODTReaderSafe Inlines Inlines
maybeAddAnchorFrom anchorReader =
  keepingTheValue (anchorReader >>? findAnchorName >>?^ toAnchorElem)
  >>>
  proc (inlines, fAnchorElem) -> do
  case fAnchorElem of
    Right anchorElem -> returnA -< anchorElem
    Left _           -> returnA -< inlines
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
findAnchorRef :: ODTReader _x Anchor
findAnchorRef = (      findAttrText NsText "ref-name"
                  >>?^ (_ANCHOR_PREFIX_,)
                ) >>?! getPrettyAnchor


--
maybeInAnchorRef :: ODTReaderSafe Inlines Inlines
maybeInAnchorRef = proc inlines -> do
  fRef <- findAnchorRef -< ()
  case fRef of
    Right anchor ->
      arr (toAnchorRef anchor) -<< inlines
    Left _ -> returnA -< inlines
  where
    toAnchorRef :: Anchor -> Inlines -> Inlines
    toAnchorRef anchor = link ("#" <> anchor) "" -- no title

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

read_text :: ODTReaderSafe _x Pandoc
read_text = matchChildContent' [ read_header
                               , read_paragraph
                               , read_list
                               , read_section
                               , read_table
                               ]
            >>^ doc

post_process :: Pandoc -> Pandoc
post_process (Pandoc m blocks) =
  Pandoc m (post_process' blocks)

post_process' :: [Block] -> [Block]
post_process' (Table attr _ specs th tb tf : Div ("", ["caption"], _) blks : xs)
  = Table attr (Caption Nothing blks) specs th tb tf : post_process' xs
post_process' bs = bs

read_body :: ODTReader _x (Pandoc, MediaBag)
read_body = executeInSub NsOffice "body"
          $ executeInSub NsOffice "text"
          $ liftAsSuccess
          $ proc inlines -> do
             txt   <- read_text     -< inlines
             state <- getExtraState -< ()
             returnA                -< (post_process txt, getMediaBag state)
