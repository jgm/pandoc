{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RecordWildCards   #-}
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

import Control.Applicative ((<|>))
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

import Text.Pandoc.Readers.Docx.Combine (combineBlocks)

import Text.Pandoc.Readers.ODT.Namespaces
import Text.Pandoc.Readers.ODT.StyleReader

import Text.Pandoc.Readers.ODT.Generic.Utils (findBy)
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

type ODTReader a = XMLConverter Namespace ReaderState a

-- | Extract the styles from the reader state
getStyles :: ODTReader Styles
getStyles = styleSet <$> getExtraState

--
getStyleByName :: StyleName -> ODTReader Style
getStyleByName name = getStyles >>= fromMaybeF . lookupStyle name

--
findStyleFamily :: Style -> ODTReader StyleFamily
findStyleFamily style = getStyles >>= fromMaybeF . getStyleFamily style

--
lookupListStyle :: StyleName -> ODTReader ListStyle
lookupListStyle name = getStyles >>= fromMaybeF . lookupListStyleByName name

--
switchCurrentListStyle :: Maybe ListStyle -> ODTReader (Maybe ListStyle)
switchCurrentListStyle mListStyle = do
  state <- getExtraState
  setExtraState $ state { currentListStyle = mListStyle }
  return $ currentListStyle state

--
pushStyle :: Style -> ODTReader ()
pushStyle style = modifyExtraState (pushStyle' style)

--
popStyle :: ODTReader ()
popStyle = modifyExtraState popStyle'

--
getCurrentListLevel :: ODTReader ListLevel
getCurrentListLevel = currentListLevel <$> getExtraState

--
getPreviousListStartCounter :: ListLevel -> ODTReader Int
getPreviousListStartCounter listLevel =
  M.findWithDefault 0 listLevel . listContinuationStartCounters
    <$> getExtraState

--
updateMediaWithResource :: (FilePath, B.ByteString) -> ODTReader ()
updateMediaWithResource resource = modifyExtraState (insertMedia' resource)

--
lookupResource :: FilePath -> ODTReader (FilePath, B.ByteString)
lookupResource target = do
  state <- getExtraState
  case lookup target (getMediaEnv state) of
    Just bs -> return (target, bs)
    Nothing -> return ("", B.empty)

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
getPrettyAnchor :: AnchorPrefix -> Anchor -> ODTReader Anchor
getPrettyAnchor baseIdent uglyAnchor = do
  state <- getExtraState
  case lookupPrettyAnchor uglyAnchor state of
    Just prettyAnchor -> return prettyAnchor
    Nothing           -> do
      let newPretty = uniqueIdentFrom baseIdent (usedAnchors state)
      modifyExtraState (putPrettyAnchor uglyAnchor newPretty)
      return newPretty

-- | Input: basis for a new header anchor
-- Output: saved new anchor
getHeaderAnchor :: Inlines -> ODTReader Anchor
getHeaderAnchor title = do
  state <- getExtraState
  let exts = extensionsFromList [Ext_auto_identifiers]
  let anchor = uniqueIdent exts (toList title)
                (Set.fromList $ usedAnchors state)
  modifyExtraState (putPrettyAnchor anchor anchor)
  return anchor


--------------------------------------------------------------------------------
-- Working with styles
--------------------------------------------------------------------------------

-- | Read the style referenced by the current element's
-- style-name attribute. Fails if there is no such attribute or if the
-- style cannot be found.
readStyleByName :: ODTReader (StyleName, Style)
readStyleByName = do
  name  <- findAttr NsText "style-name"
  style <- getStyleByName name
  return (name, style)

--
isStyleToTrace :: Style -> ODTReader Bool
isStyleToTrace style = (== FaText) <$> findStyleFamily style

--
withNewStyle :: ODTReader Inlines -> ODTReader Inlines
withNewStyle reader = do
  fStyle <- tryC readStyleByName
  case fStyle of
    Right (styleName, _) | isCodeStyle styleName ->
      inlineCode <$> reader
    Right (_, style)
      | Just textProps <- textProperties (styleProperties style) -> do
          state <- getExtraState
          let mFamily  = styleFamily style
              modifier = modifierFromStyleDiff (state, textProps, mFamily)
          fShouldTrace <- tryC (isStyleToTrace style)
          case fShouldTrace of
            Right True -> do
              pushStyle style
              inlines <- reader
              popStyle
              return $ modifier inlines
            -- In case anything goes wrong
            _ -> reader
    _ -> reader
  where
    isCodeStyle :: StyleName -> Bool
    isCodeStyle "Source_Text" = True
    isCodeStyle "Source_20_Text" = True
    isCodeStyle _             = False

    inlineCode :: Inlines -> Inlines
    inlineCode = code . stringifyInlines

type PropertyTriple = (ReaderState, TextProperties, Maybe StyleFamily)
type InlineModifier = Inlines -> Inlines

-- | Given data about the local style changes, calculates how to modify
-- an instance of 'Inlines'
modifierFromStyleDiff :: PropertyTriple -> InlineModifier
modifierFromStyleDiff propertyTriple  =
  foldr (.) id $
  getVPosModifier propertyTriple
  : map (\(hasChanged', modifier) ->
           if hasChanged' propertyTriple then modifier else ignore)
        [ (hasEmphChanged           , emph      )
        , (hasChanged isStrong      , strong    )
        , (hasChanged strikethrough , strikeout )
        ]
  where
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
    hasEmphChanged triple = any ($ triple) [ hasChanged  isEmphasised
                                           , hasChanged  underline
                                           ]

    hasChanged property triple@(_, property -> newProperty, _) =
        (/= Just newProperty) (lookupPreviousValue property triple)

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
constructPara :: ODTReader Blocks -> ODTReader Blocks
constructPara reader = do
  fStyle <- tryC readStyleByName
  case fStyle of
    Left   _    -> reader
    Right (styleName, _) | isTableCaptionStyle styleName ->
      tableCaptionP <$> reader
    Right (_, style) -> do
      styles <- getStyles
      let props = extendedStylePropertyChain [style] styles
      listLevel <- getCurrentListLevel
      getParaModifier listLevel props <$> reader
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
-- rely on a parent list's style. In the former case the current style in the
-- state must be switched before and after the call to the child converter
-- while in the latter the child converter can be called directly.
-- If anything goes wrong, a default ordered-list-constructor is used.
constructList :: ODTReader [Blocks] -> ODTReader Blocks
constructList reader = do
  modifyExtraState (shiftListLevel 1)
  listLevel                    <- getCurrentListLevel
  listContinuationStartCounter <- getPreviousListStartCounter listLevel
  fStyleName                   <- tryC (findAttr NsText "style-name")
  fContNumbering               <- tryC (findAttr NsText "continue-numbering")

  let continueNumbering = fContNumbering == Right "true"

      startNumForListLevelStyle mListLevelStyle
        | continueNumbering      = listContinuationStartCounter
        | isJust mListLevelStyle = listItemStart (fromJust mListLevelStyle)
        | otherwise              = 1

      constructWith startNum constructor = do
        items <- reader
        modifyExtraState (shiftListLevel (-1))
        modifyExtraState (modifyListContinuationStartCounter listLevel
                           (startNum + length items))
        return $ constructor items

      constructOrderedList =
        let startNum = startNumForListLevelStyle Nothing
        in  constructWith startNum
              (orderedListWith (startNum, DefaultStyle, DefaultDelim))

      constructListWith listLevelStyle =
        let startNum = startNumForListLevelStyle (Just listLevelStyle)
        in  constructWith startNum (getListConstructor listLevelStyle startNum)

  case fStyleName of
    Right styleName -> do
      fListStyle <- tryC (lookupListStyle styleName)
      case fListStyle of
        Right listStyle ->
          case getListLevelStyle listLevel listStyle of
            Just listLevelStyle -> do
              oldListStyle <- switchCurrentListStyle (Just listStyle)
              blocks       <- constructListWith listLevelStyle
              _            <- switchCurrentListStyle oldListStyle
              return blocks
            Nothing             -> constructOrderedList
        Left _                  -> constructOrderedList
    Left _ -> do
      mListStyle <- currentListStyle <$> getExtraState
      case mListStyle of
        Just listStyle ->
          case getListLevelStyle listLevel listStyle of
            Just listLevelStyle -> constructListWith listLevelStyle
            Nothing             -> constructOrderedList
        Nothing                 -> constructOrderedList

--------------------------------------------------------------------------------
-- Readers
--------------------------------------------------------------------------------

type Matcher result = ElementMatcher Namespace ReaderState result

type InlineMatcher = Matcher Inlines

type BlockMatcher  = Matcher Blocks

newtype FirstMatch a = FirstMatch (Alt Maybe a)
  deriving (Foldable, Monoid, Semigroup)

firstMatch :: a -> FirstMatch a
firstMatch = FirstMatch . Alt . Just

newtype CombiningBlocks = CombiningBlocks { unCombiningBlocks :: Blocks }

instance Semigroup CombiningBlocks where
  CombiningBlocks l <> CombiningBlocks r = CombiningBlocks (combineBlocks l r)

instance Monoid CombiningBlocks where
  mempty = CombiningBlocks mempty

--
matchingElement :: Namespace -> ElementName
                -> ODTReader e
                -> Matcher e
matchingElement ns name reader = (ns, name, reader)

--
matchSmushedChildBlocks' :: [Matcher CombiningBlocks] -> ODTReader Blocks
matchSmushedChildBlocks' ls = unCombiningBlocks <$> matchContent' ls

--------------------------------------------
-- Matchers
--------------------------------------------

----------------------
-- Basics
----------------------

--
-- | Open Document allows several consecutive spaces if they are marked up.
-- Text content of the current element is read with this fallback converter.
read_plain_text :: XML.Content -> ODTReader Inlines
read_plain_text (XML.Text cData) = return $ text $ XML.cdData cData
read_plain_text _                = return mempty

read_text_seq :: InlineMatcher
read_text_seq  = matchingElement NsText "sequence"
                 $ matchContent [] read_plain_text


-- specifically. I honor that, although the current implementation of 'mappend'
-- for 'Inlines' in "Text.Pandoc.Builder" will collapse them again.
-- The rational is to be prepared for future modifications.
read_spaces      :: InlineMatcher
read_spaces       = matchingElement NsText "s" $ do
                      count <- readAttrWithDefault NsText "c" 1 -- how many spaces?
                      return $ fromList (replicate count Space)
--
read_line_break  :: InlineMatcher
read_line_break   = matchingElement NsText "line-break"
                    $ return linebreak
--
read_tab         :: InlineMatcher
read_tab          = matchingElement NsText "tab"
                    $ return space
--
read_span        :: InlineMatcher
read_span         = matchingElement NsText "span"
                    $ withNewStyle
                    $ matchContent [ read_span
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
read_paragraph   :: Matcher CombiningBlocks
read_paragraph    = matchingElement NsText "p" $ fmap CombiningBlocks $ do
                      fStyle <- tryC readStyleByName
                      case fStyle of
                        Right style | isPreformattedStyle style ->
                          codeBlock . stringifyInlines <$> matchParagraphContent
                        _ ->
                          constructPara (para <$> withNewStyle matchParagraphContent)
                    where
                      isPreformattedStyle :: (StyleName, Style) -> Bool
                      isPreformattedStyle ("Preformatted_20_Text", _) = True
                      isPreformattedStyle (_, Style { styleParentName = Just "Preformatted_20_Text" }) = True
                      isPreformattedStyle _ = False


matchParagraphContent :: ODTReader Inlines
matchParagraphContent = matchContent [ read_span
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
read_header      :: Matcher CombiningBlocks
read_header       = matchingElement NsText "h" $ do
  level    <- readAttrWithDefault NsText "outline-level" 1
  children <- matchContent [ read_span
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
  anchor   <- getHeaderAnchor children
  let idAttr = (anchor, [], []) -- no classes, no key-value pairs
  return $ CombiningBlocks $ headerWith idAttr level children

----------------------
-- Lists
----------------------

--
read_list        :: Matcher CombiningBlocks
read_list         = matchingElement NsText "list"
                    $ CombiningBlocks
                    <$> constructList
                        ( matchContent' [ read_list_item
                                        , read_list_header
                                        ] )
--
read_list_item   :: Matcher [Blocks]
read_list_item    = read_list_element "list-item"

read_list_header :: Matcher [Blocks]
read_list_header  = read_list_element "list-header"

read_list_element               :: ElementName -> Matcher [Blocks]
read_list_element listElement   = matchingElement NsText listElement
                                  $ compactify . (:[])
                                  <$> matchSmushedChildBlocks'
                                        [ read_paragraph
                                        , read_header
                                        , read_list
                                        , read_section
                                        ]

----------------------
-- Sections
----------------------

read_section :: Matcher CombiningBlocks
read_section = matchingElement NsText "section"
                 $ CombiningBlocks . divWith nullAttr
                 <$> matchSmushedChildBlocks' [ read_paragraph
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
                    $ link
                      <$> (fixRelativeLink
                             <$> findAttrWithDefault NsXLink  "href"  "")
                      <*> findAttrWithDefault NsOffice "title" ""
                      <*> matchContent [ read_span
                                       , read_note
                                       , read_citation
                                       , read_bookmark
                                       , read_bookmark_start
                                       , read_reference_start
                                       , read_bookmark_ref
                                       , read_reference_ref
                                       ] read_plain_text

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
                    $ note <$> matchContent' [ read_note_body ]

read_note_body   :: BlockMatcher
read_note_body    = matchingElement NsText "note-body"
                    $ matchSmushedChildBlocks' [ read_paragraph ]

-------------------------
-- Citations
-------------------------

read_citation    :: InlineMatcher
read_citation     = matchingElement NsText "bibliography-mark"
                    $ cite
                      <$> ( makeCitation
                              <$> findAttrWithDefault NsText "identifier" ""
                              <*> readAttrWithDefault NsText "number" 0 )
                      <*> matchContent [] read_plain_text
  where
   makeCitation :: T.Text -> Int -> [Citation]
   makeCitation citeId num = [Citation citeId [] [] NormalCitation num 0]


----------------------
-- Tables
----------------------

--
read_table        :: Matcher CombiningBlocks
read_table         = matchingElement NsTable "table"
                     $ fmap (CombiningBlocks . table')
                     $ (,) <$> matchContent' [read_table_header]
                           <*> matchContent' [read_table_row]

-- | A table without a caption.
table' :: ([[Cell]], [[Cell]]) -> Blocks
table' (headers, rows) = compactifyTable $
    table emptyCaption (replicate numcols defaults) th [tb] tf
  where
    defaults = (AlignDefault, ColWidthDefault)
    numcols = maximum $ map length $ headers ++ rows
    toRow = Row nullAttr
    th = TableHead nullAttr $ map toRow headers
    tb = TableBody nullAttr 0 [] $ map toRow rows
    tf = TableFoot nullAttr []

--
read_table_header :: Matcher [[Cell]]
read_table_header = matchingElement NsTable "table-header-rows"
                      $ matchContent' [ read_table_row
                                      ]

--
read_table_row    :: Matcher [[Cell]]
read_table_row     = matchingElement NsTable "table-row"
                     $ (:[])
                     <$> matchContent' [ read_table_cell
                                       ]

--
read_table_cell   :: Matcher [Cell]
read_table_cell    = matchingElement NsTable "table-cell"
                     $ cell'
                       <$> (RowSpan <$> readAttrWithDefault NsTable "number-rows-spanned" 1)
                       <*> (ColSpan <$> readAttrWithDefault NsTable "number-columns-spanned" 1)
                       <*> matchSmushedChildBlocks' [ read_paragraph
                                                    , read_list
                                                    ]
  where
    cell' rowSpan colSpan blocks = map (cell AlignDefault rowSpan colSpan) $ compactify [blocks]

----------------------
-- Frames
----------------------

--
read_frame :: InlineMatcher
read_frame = matchingElement NsDraw "frame" $ do
  children <- filterChildrenName' NsDraw (`elem` ["image", "object", "text-box"])
  fold . mconcat <$> mapM read_frame_child children

read_frame_child :: XML.Element -> ODTReader (FirstMatch Inlines)
read_frame_child child =
  case elName child of
    "image"    -> read_frame_img      child
    "object"   -> read_frame_mathml   child
    "text-box" -> read_frame_text_box child
    _          -> return mempty

read_frame_img :: XML.Element -> ODTReader (FirstMatch Inlines)
read_frame_img img = do
  src <- executeIn img (findAttr' NsXLink "href")
  case fold src of
    ""   -> return mempty
    src' -> do
      let exts = extensionsFromList [Ext_auto_identifiers]
          src'' = fixRelativeLink src'
      resource   <- lookupResource (T.unpack src'')
      updateMediaWithResource resource
      w          <- findAttr' NsSVG "width"
      h          <- findAttr' NsSVG "height"
      titleNodes <- matchContent' [ read_frame_title ]
      alt        <- matchContent [] read_plain_text
      return $ firstMatch
             $ imageWith (image_attributes w h) src''
                         (inlineListToIdentifier exts (toList titleNodes))
                         alt

read_frame_title :: InlineMatcher
read_frame_title = matchingElement NsSVG "title" (matchContent [] read_plain_text)

image_attributes :: Maybe T.Text -> Maybe T.Text -> Attr
image_attributes x y =
  ( "", [], dim "width" x ++ dim "height" y)
  where
    dim _ (Just "")   = []
    dim name (Just v) = [(name, v)]
    dim _ Nothing     = []

read_frame_mathml :: XML.Element -> ODTReader (FirstMatch Inlines)
read_frame_mathml obj = do
  src <- executeIn obj (findAttr' NsXLink "href")
  case fold src of
    ""   -> return mempty
    src' -> do
      let path = T.unpack $
                  fromMaybe src' (T.stripPrefix "./" src') <> "/content.xml"
      (_, mathml) <- lookupResource path
      case readMathML (UTF8.toText $ B.toStrict mathml) of
        Left _     -> return mempty
        Right exps -> return $ firstMatch $ displayMath $ writeTeX exps

read_frame_text_box :: XML.Element -> ODTReader (FirstMatch Inlines)
read_frame_text_box box = do
  paragraphs <- executeIn box (matchSmushedChildBlocks' [ read_paragraph ])
  return $ read_img_with_caption $ toList paragraphs

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
readAnchorAttr :: ODTReader Anchor
readAnchorAttr = findAttr NsText "name"

-- | Beware: may fail
findAnchorName :: AnchorPrefix -> ODTReader Anchor
findAnchorName anchorPrefix = do
  uglyAnchor <- readAnchorAttr
  getPrettyAnchor anchorPrefix uglyAnchor

--
maybeAddAnchorFrom :: ODTReader AnchorPrefix -> ODTReader Inlines
maybeAddAnchorFrom anchorReader =
      (toAnchorElem <$> (anchorReader >>= findAnchorName))
  <|> return mempty
  where
    toAnchorElem :: Anchor -> Inlines
    toAnchorElem anchorID = spanWith (anchorID, [], []) mempty
                            -- no classes, no key-value pairs

--
read_bookmark     :: InlineMatcher
read_bookmark      = matchingElement NsText "bookmark"
                     $ maybeAddAnchorFrom (return _ANCHOR_PREFIX_)

--
read_bookmark_start :: InlineMatcher
read_bookmark_start = matchingElement NsText "bookmark-start"
                     $ maybeAddAnchorFrom (return _ANCHOR_PREFIX_)

--
read_reference_start :: InlineMatcher
read_reference_start = matchingElement NsText "reference-mark-start"
                     $ maybeAddAnchorFrom readAnchorAttr

-- | Beware: may fail
findAnchorRef :: ODTReader Anchor
findAnchorRef = do
  uglyAnchor <- findAttr NsText "ref-name"
  getPrettyAnchor _ANCHOR_PREFIX_ uglyAnchor

--
maybeInAnchorRef :: Inlines -> ODTReader Inlines
maybeInAnchorRef inlines = do
  fRef <- tryC findAnchorRef
  case fRef of
    Right anchor -> return $ toAnchorRef anchor inlines
    Left _       -> return inlines
  where
    toAnchorRef :: Anchor -> Inlines -> Inlines
    toAnchorRef anchor = link ("#" <> anchor) "" -- no title

--
read_bookmark_ref :: InlineMatcher
read_bookmark_ref = matchingElement NsText "bookmark-ref"
                    $ matchContent [] read_plain_text >>= maybeInAnchorRef

--
read_reference_ref :: InlineMatcher
read_reference_ref = matchingElement NsText "reference-ref"
                    $ matchContent [] read_plain_text >>= maybeInAnchorRef


----------------------
-- Entry point
----------------------

read_text :: ODTReader Pandoc
read_text = doc <$> matchSmushedChildBlocks' [ read_header
                                             , read_paragraph
                                             , read_list
                                             , read_section
                                             , read_table
                                             ]

post_process :: Pandoc -> Pandoc
post_process (Pandoc m blocks) =
  Pandoc m (post_process' blocks)

post_process' :: [Block] -> [Block]
post_process' (Table attr _ specs th tb tf : Div ("", ["caption"], _) blks : xs)
  = Table attr (Caption Nothing blks) specs th tb tf : post_process' xs
post_process' bs = bs

read_body :: ODTReader (Pandoc, MediaBag)
read_body = executeInSub NsOffice "body"
          $ executeInSub NsOffice "text"
          $ do
             txt   <- read_text
             state <- getExtraState
             return (post_process txt, getMediaBag state)
