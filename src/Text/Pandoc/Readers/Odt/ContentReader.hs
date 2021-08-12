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

import Text.Pandoc.Readers.Odt.Base
import Text.Pandoc.Readers.Odt.Namespaces
import Text.Pandoc.Readers.Odt.StyleReader

import Text.Pandoc.Readers.Odt.Arrows.State (foldS)
import Text.Pandoc.Readers.Odt.Arrows.Utils
import Text.Pandoc.Readers.Odt.Generic.Fallible
import Text.Pandoc.Readers.Odt.Generic.Utils
import Text.Pandoc.Readers.Odt.Generic.XMLConverter

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
readerState styles media = ReaderState styles [] 0 Nothing M.empty media mempty

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

--
updateMediaWithResource :: OdtReaderSafe (FilePath, B.ByteString) (FilePath, B.ByteString)
updateMediaWithResource = keepingTheValue (
                 (keepingTheValue getExtraState
                  >>% insertMedia'
                  )
                 >>> setExtraState
               )
           >>^ fst

lookupResource :: OdtReaderSafe FilePath (FilePath, B.ByteString)
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
getPrettyAnchor :: OdtReaderSafe (AnchorPrefix, Anchor) Anchor
getPrettyAnchor = proc (baseIdent, uglyAnchor) -> do
  state <- getExtraState -< ()
  case lookupPrettyAnchor uglyAnchor state of
    Just prettyAnchor -> returnA -< prettyAnchor
    Nothing           -> do
      let newPretty = uniqueIdentFrom baseIdent (usedAnchors state)
      modifyExtraState (putPrettyAnchor uglyAnchor newPretty) -<< newPretty

-- | Input: basis for a new header anchor
-- Output: saved new anchor
getHeaderAnchor :: OdtReaderSafe Inlines Anchor
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
readStyleByName :: OdtReader _x (StyleName, Style)
readStyleByName =
  findAttr NsText "style-name" >>? keepingTheValue getStyleByName >>^ liftE
  where
    liftE :: (StyleName, Fallible Style) -> Fallible (StyleName, Style)
    liftE (name, Right v) = Right (name, v)
    liftE (_, Left v)     = Left v

--
isStyleToTrace :: OdtReader Style Bool
isStyleToTrace = findStyleFamily >>?^ (==FaText)

--
withNewStyle :: OdtReaderSafe x Inlines -> OdtReaderSafe x Inlines
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
    Right (styleName, _) | isTableCaptionStyle styleName -> do
      blocks' <- reader   -< blocks
      arr tableCaptionP  -< blocks'
    Right (_, style) -> do
      let modifier = getParaModifier style
      blocks' <- reader   -<  blocks
      arr modifier        -<< blocks'
  where
    isTableCaptionStyle :: StyleName -> Bool
    isTableCaptionStyle "Table" = True
    isTableCaptionStyle _       = False
    tableCaptionP b = divWith ("", ["caption"], []) b

type ListConstructor = [Blocks] -> Blocks

getListConstructor :: ListLevelStyle -> ListConstructor
getListConstructor ListLevelStyle{..} =
  case listLevelType of
    LltBullet   -> bulletList
    LltImage    -> bulletList
    LltNumbered -> let listNumberStyle = toListNumberStyle listItemFormat
                       listNumberDelim = toListNumberDelim listItemPrefix
                                                           listItemSuffix
                   in  orderedListWith (listItemStart, listNumberStyle, listNumberDelim)
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

newtype FirstMatch a = FirstMatch (Alt Maybe a)
  deriving (Foldable, Monoid, Semigroup)

firstMatch :: a -> FirstMatch a
firstMatch = FirstMatch . Alt . Just

--
matchingElement :: (Monoid e)
                => Namespace -> ElementName
                -> OdtReaderSafe  e e
                -> ElementMatcher e
matchingElement ns name reader = (ns, name, asResultAccumulator reader)
  where
   asResultAccumulator :: (ArrowChoice a, Monoid m) => a m m -> a m (Fallible m)
   asResultAccumulator a = liftAsSuccess $ keepingTheValue a >>% mappend

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
--                  $ withIncreasedListLevel
                    $ constructList
--                  $ liftA bulletList
                    $ matchChildContent' [ read_list_item
                                         ]
--
read_list_item   :: ElementMatcher [Blocks]
read_list_item    = matchingElement NsText "list-item"
                    $ liftA (compactify.(:[]))
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
                      ( findAttrTextWithDefault NsXLink  "href"  ""          )
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
                     $ liftA simpleTable'
                     $ matchChildContent'  [ read_table_row
                                           ]

-- | A simple table without a caption or headers
-- | Infers the number of headers from rows
simpleTable' :: [[Blocks]] -> Blocks
simpleTable' []         = simpleTable [] []
simpleTable' (x : rest) = simpleTable (fmap (const defaults) x) (x : rest)
  where defaults = fromList []

--
read_table_row    :: ElementMatcher [[Blocks]]
read_table_row     = matchingElement NsTable "table-row"
                     $ liftA (:[])
                     $ matchChildContent'  [ read_table_cell
                                           ]

--
read_table_cell   :: ElementMatcher [Blocks]
read_table_cell    = matchingElement NsTable "table-cell"
                     $ liftA (compactify.(:[]))
                     $ matchChildContent' [ read_paragraph
                                          ]

----------------------
-- Frames
----------------------

--
read_frame :: InlineMatcher
read_frame = matchingElement NsDraw "frame"
             $ filterChildrenName' NsDraw (`elem` ["image", "object", "text-box"])
           >>> foldS read_frame_child
           >>> arr fold

read_frame_child :: OdtReaderSafe XML.Element (FirstMatch Inlines)
read_frame_child =
  proc child -> case elName child of
    "image"    -> read_frame_img      -< child
    "object"   -> read_frame_mathml   -< child
    "text-box" -> read_frame_text_box -< child
    _          -> returnV mempty      -< ()

read_frame_img :: OdtReaderSafe XML.Element (FirstMatch Inlines)
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

read_frame_mathml :: OdtReaderSafe XML.Element (FirstMatch Inlines)
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

read_frame_text_box :: OdtReaderSafe XML.Element (FirstMatch Inlines)
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
readAnchorAttr :: OdtReader _x Anchor
readAnchorAttr = findAttrText NsText "name"

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
findAnchorRef :: OdtReader _x Anchor
findAnchorRef = (      findAttrText NsText "ref-name"
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

read_text :: OdtReaderSafe _x Pandoc
read_text = matchChildContent' [ read_header
                               , read_paragraph
                               , read_list
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

read_body :: OdtReader _x (Pandoc, MediaBag)
read_body = executeInSub NsOffice "body"
          $ executeInSub NsOffice "text"
          $ liftAsSuccess
          $ proc inlines -> do
             txt   <- read_text     -< inlines
             state <- getExtraState -< ()
             returnA                -< (post_process txt, getMediaBag state)
