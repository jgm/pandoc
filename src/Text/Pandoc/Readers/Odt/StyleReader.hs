{-# LANGUAGE CPP             #-}
{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{- |
   Module      : Text.Pandoc.Readers.Odt.StyleReader
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

Reader for the style information in an odt document.
-}

module Text.Pandoc.Readers.Odt.StyleReader
( Style                (..)
, StyleName
, StyleFamily          (..)
, Styles               (..)
, StyleProperties      (..)
, TextProperties       (..)
, ParaProperties       (..)
, VerticalTextPosition (..)
, ListItemNumberFormat (..)
, ListLevel
, ListStyle            (..)
, ListLevelStyle       (..)
, ListLevelType        (..)
, LengthOrPercent      (..)
, lookupStyle
, getListLevelStyle
, getStyleFamily
, lookupDefaultStyle'
, lookupListStyleByName
, extendedStylePropertyChain
, readStylesAt
) where

import Control.Applicative hiding (liftA, liftA2, liftA3)
import Control.Arrow

import Data.Default
import qualified Data.Foldable as F
import Data.List (unfoldr)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

import qualified Text.XML.Light as XML

import Text.Pandoc.Shared (safeRead)

import Text.Pandoc.Readers.Odt.Arrows.Utils

import Text.Pandoc.Readers.Odt.Generic.Fallible
import qualified Text.Pandoc.Readers.Odt.Generic.SetMap as SM
import Text.Pandoc.Readers.Odt.Generic.Utils
import Text.Pandoc.Readers.Odt.Generic.XMLConverter

import Text.Pandoc.Readers.Odt.Base
import Text.Pandoc.Readers.Odt.Namespaces

readStylesAt :: XML.Element -> Fallible Styles
readStylesAt e = runConverter' readAllStyles mempty e

--------------------------------------------------------------------------------
-- Reader for font declarations and font pitches
--------------------------------------------------------------------------------

-- Pandoc has no support for different font pitches. Yet knowing them can be
-- very helpful in cases where Pandoc has more semantics than OpenDocument.
-- In these cases, the pitch can help deciding as what to define a block of
-- text. So let's start with a type for font pitches:

data FontPitch    = PitchVariable | PitchFixed
  deriving ( Eq, Show )

instance Lookupable FontPitch where
  lookupTable = [ ("variable" , PitchVariable)
                , ("fixed"    , PitchFixed   )
                ]

instance Default FontPitch where
  def = PitchVariable

-- The font pitch can be specified in a style directly. Normally, however,
-- it is defined in the font. That is also the specs' recommendation.
--
-- Thus, we want

type FontFaceName = String

type FontPitches = M.Map FontFaceName FontPitch

-- To get there, the fonts have to be read and the pitches extracted.
-- But the resulting map are only needed at one later place, so it should not be
-- transported on the value level, especially as we already use a state arrow.
-- So instead, the resulting map is lifted into the state of the reader.
-- (An alternative might be ImplicitParams, but again, we already have a state.)
--
-- So the main style readers will have the types
type StyleReader     a b  = XMLReader     FontPitches a b
-- and
type StyleReaderSafe a b  = XMLReaderSafe FontPitches a b
-- respectively.
--
-- But before we can work with these, we need to define the reader that reads
-- the fonts:

-- | A reader for font pitches
fontPitchReader :: XMLReader _s _x FontPitches
fontPitchReader = executeInSub NsOffice "font-face-decls" (
                          withEveryL NsStyle "font-face" (liftAsSuccess (
                              findAttr' NsStyle "name"
                              &&&
                              lookupDefaultingAttr NsStyle "font-pitch"
                            ))
                    >>?^ ( M.fromList . foldl accumLegalPitches [] )
                  ) `ifFailedDo` returnV (Right M.empty)
  where accumLegalPitches ls (Nothing,_) = ls
        accumLegalPitches ls (Just n,p)  = (n,p):ls


-- | A wrapper around the font pitch reader that lifts the result into the
-- state.
readFontPitches :: StyleReader x x
readFontPitches = producingExtraState () () fontPitchReader


-- | Looking up a pitch in the state of the arrow.
--
-- The function does the following:
-- * Look for the font pitch in an attribute.
-- * If that fails, look for the font name, look up the font in the state
--   and use the pitch from there.
-- * Return the result in a Maybe
--
findPitch :: XMLReaderSafe FontPitches _x (Maybe FontPitch)
findPitch =     ( lookupAttr NsStyle "font-pitch"
                  `ifFailedDo`     findAttr NsStyle "font-name"
                               >>? (     keepingTheValue getExtraState
                                     >>% M.lookup
                                     >>^ maybeToChoice
                                   )
                )
            >>> choiceToMaybe

--------------------------------------------------------------------------------
-- Definitions of main data
--------------------------------------------------------------------------------

type StyleName        = String

-- | There are two types of styles: named styles with a style family and an
-- optional style parent, and default styles for each style family,
-- defining default style properties
data Styles           = Styles
                          { stylesByName     :: M.Map StyleName   Style
                          , listStylesByName :: M.Map StyleName   ListStyle
                          , defaultStyleMap  :: M.Map StyleFamily StyleProperties
                          }
  deriving ( Show )

-- Styles from a monoid under union
instance Semigroup Styles where
  (Styles sBn1 dSm1 lsBn1) <> (Styles sBn2 dSm2 lsBn2)
          = Styles (M.union sBn1  sBn2)
                   (M.union dSm1  dSm2)
                   (M.union lsBn1 lsBn2)
instance Monoid Styles where
  mempty  = Styles M.empty M.empty M.empty
  mappend = (<>)

-- Not all families from the specifications are implemented, only those we need.
-- But there are none that are not mentioned here.
data StyleFamily      = FaText    | FaParagraph
--                    | FaTable   | FaTableCell | FaTableColumn | FaTableRow
--                    | FaGraphic | FaDrawing   | FaChart
--                    | FaPresentation
--                    | FaRuby
  deriving ( Eq, Ord, Show )

instance Lookupable StyleFamily where
  lookupTable = [ ( "text"         , FaText         )
                , ( "paragraph"    , FaParagraph    )
--              , ( "table"        , FaTable        )
--              , ( "table-cell"   , FaTableCell    )
--              , ( "table-column" , FaTableColumn  )
--              , ( "table-row"    , FaTableRow     )
--              , ( "graphic"      , FaGraphic      )
--              , ( "drawing-page" , FaDrawing      )
--              , ( "chart"        , FaChart        )
--              , ( "presentation" , FaPresentation )
--              , ( "ruby"         , FaRuby         )
                ]

-- | A named style
data Style            = Style  { styleFamily     :: Maybe StyleFamily
                               , styleParentName :: Maybe StyleName
                               , listStyle       :: Maybe StyleName
                               , styleProperties :: StyleProperties
                               }
  deriving ( Eq, Show )

data StyleProperties  = SProps { textProperties :: Maybe TextProperties
                               , paraProperties :: Maybe ParaProperties
--                             , tableColProperties  :: Maybe TColProperties
--                             , tableRowProperties  :: Maybe TRowProperties
--                             , tableCellProperties :: Maybe TCellProperties
--                             , tableProperties     :: Maybe TableProperties
--                             , graphicProperties   :: Maybe GraphProperties
                               }
  deriving ( Eq, Show )

instance  Default StyleProperties where
  def =                SProps { textProperties       = Just def
                               , paraProperties      = Just def
                               }

data TextProperties   = PropT  { isEmphasised     :: Bool
                               , isStrong         :: Bool
                               , pitch            :: Maybe FontPitch
                               , verticalPosition :: VerticalTextPosition
                               , underline        :: Maybe UnderlineMode
                               , strikethrough    :: Maybe UnderlineMode
                               }
  deriving ( Eq, Show )

instance Default TextProperties where
  def =                 PropT  { isEmphasised     = False
                               , isStrong         = False
                               , pitch            = Just def
                               , verticalPosition = def
                               , underline        = Nothing
                               , strikethrough    = Nothing
                               }

data ParaProperties   = PropP { paraNumbering :: ParaNumbering
                              , indentation   :: LengthOrPercent
                              , margin_left   :: LengthOrPercent
                              }
  deriving ( Eq, Show )

instance Default ParaProperties where
  def =                 PropP { paraNumbering = NumberingNone
                              , indentation   = def
                              , margin_left   = def
                              }

----
-- All the little data types that make up the properties
----

data VerticalTextPosition = VPosNormal | VPosSuper | VPosSub
  deriving ( Eq, Show )

instance Default VerticalTextPosition where
  def = VPosNormal

instance Read VerticalTextPosition where
  readsPrec _ s =    [ (VPosSub        , s') | ("sub"   , s') <- lexS          ]
                  ++ [ (VPosSuper      , s') | ("super" , s') <- lexS          ]
                  ++ [ (signumToVPos n , s') | (  n     , s') <- readPercent s ]
    where
      lexS = lex s
      signumToVPos n | n < 0     = VPosSub
                     | n > 0     = VPosSuper
                     | otherwise = VPosNormal

data UnderlineMode = UnderlineModeNormal | UnderlineModeSkipWhitespace
  deriving ( Eq, Show )

instance Lookupable UnderlineMode where
  lookupTable = [ ( "continuous"       , UnderlineModeNormal         )
                , ( "skip-white-space" , UnderlineModeSkipWhitespace )
                ]


data ParaNumbering = NumberingNone | NumberingKeep | NumberingRestart Int
  deriving ( Eq, Show )

data LengthOrPercent = LengthValueMM Int | PercentValue Int
  deriving ( Eq, Show )

instance Default LengthOrPercent where
  def = LengthValueMM 0

instance Read LengthOrPercent where
  readsPrec _ s =
      [ (PercentValue  percent  , s' ) | (percent , s' ) <- readPercent s]
   ++ [ (LengthValueMM lengthMM , s'') | (length' , s' ) <- reads s
                                       , (unit    , s'') <- reads s'
                                       , let lengthMM = estimateInMillimeter
                                                                   length' unit
                                       ]

data XslUnit = XslUnitMM | XslUnitCM
             | XslUnitInch
             | XslUnitPoints | XslUnitPica
             | XslUnitPixel
             | XslUnitEM

instance Show XslUnit where
  show XslUnitMM     = "mm"
  show XslUnitCM     = "cm"
  show XslUnitInch   = "in"
  show XslUnitPoints = "pt"
  show XslUnitPica   = "pc"
  show XslUnitPixel  = "px"
  show XslUnitEM     = "em"

instance Read XslUnit where
  readsPrec _ "mm" = [(XslUnitMM     , "")]
  readsPrec _ "cm" = [(XslUnitCM     , "")]
  readsPrec _ "in" = [(XslUnitInch   , "")]
  readsPrec _ "pt" = [(XslUnitPoints , "")]
  readsPrec _ "pc" = [(XslUnitPica   , "")]
  readsPrec _ "px" = [(XslUnitPixel  , "")]
  readsPrec _ "em" = [(XslUnitEM     , "")]
  readsPrec _  _   = []

-- | Rough conversion of measures into millimetres.
-- Pixels and em's are actually implementation dependent/relative measures,
-- so I could not really easily calculate anything exact here even if I wanted.
-- But I do not care about exactness right now, as I only use measures
-- to determine if a paragraph is "indented" or not.
estimateInMillimeter :: Int -> XslUnit -> Int
estimateInMillimeter n XslUnitMM     = n
estimateInMillimeter n XslUnitCM     = n * 10
estimateInMillimeter n XslUnitInch   = n * 25    -- \*             25.4
estimateInMillimeter n XslUnitPoints = n `div` 3 -- \*      1/72 * 25.4
estimateInMillimeter n XslUnitPica   = n * 4     -- \* 12 * 1/72 * 25.4
estimateInMillimeter n XslUnitPixel  = n `div`3  -- \*      1/72 * 25.4
estimateInMillimeter n XslUnitEM     = n * 7     -- \* 16 * 1/72 * 25.4


----
-- List styles
----

type ListLevel = Int

newtype ListStyle = ListStyle { levelStyles :: M.Map ListLevel ListLevelStyle
                              }
  deriving ( Eq, Show )

--
getListLevelStyle :: ListLevel -> ListStyle -> Maybe ListLevelStyle
getListLevelStyle level ListStyle{..} =
  let (lower , exactHit , _) = M.splitLookup level levelStyles
  in  exactHit <|> fmap fst (M.maxView lower)
  -- findBy (`M.lookup` levelStyles) [level, (level-1) .. 1]
  -- \^ simpler, but in general less efficient

data ListLevelStyle = ListLevelStyle { listLevelType  :: ListLevelType
                                     , listItemPrefix :: Maybe String
                                     , listItemSuffix :: Maybe String
                                     , listItemFormat :: ListItemNumberFormat
                                     , listItemStart  :: Int
                                     }
  deriving ( Eq, Ord )

instance Show ListLevelStyle where
  show ListLevelStyle{..} =    "<LLS|"
                            ++ show listLevelType
                            ++ "|"
                            ++ maybeToString listItemPrefix
                            ++ show listItemFormat
                            ++ maybeToString listItemSuffix
                            ++ ">"
    where maybeToString = fromMaybe ""

data ListLevelType = LltBullet | LltImage | LltNumbered
  deriving ( Eq, Ord, Show )

data ListItemNumberFormat = LinfNone
                          | LinfNumber
                          | LinfRomanLC | LinfRomanUC
                          | LinfAlphaLC | LinfAlphaUC
                          | LinfString String
  deriving ( Eq, Ord )

instance Show ListItemNumberFormat where
  show  LinfNone      = ""
  show  LinfNumber    = "1"
  show  LinfRomanLC   = "i"
  show  LinfRomanUC   = "I"
  show  LinfAlphaLC   = "a"
  show  LinfAlphaUC   = "A"
  show (LinfString s) =  s

instance Default ListItemNumberFormat where
  def = LinfNone

instance Read ListItemNumberFormat where
  readsPrec _ ""  = [(LinfNone     , "")]
  readsPrec _ "1" = [(LinfNumber   , "")]
  readsPrec _ "i" = [(LinfRomanLC  , "")]
  readsPrec _ "I" = [(LinfRomanUC  , "")]
  readsPrec _ "a" = [(LinfAlphaLC  , "")]
  readsPrec _ "A" = [(LinfAlphaUC  , "")]
  readsPrec _  s  = [(LinfString s , "")]

--------------------------------------------------------------------------------
-- Readers
--
-- ...it seems like a whole lot of this should be automatically derivable
--    or at least moveable into a class. Most of this is data concealed in
--    code.
--------------------------------------------------------------------------------

--
readAllStyles :: StyleReader _x Styles
readAllStyles = (      readFontPitches
                  >>?! (     readAutomaticStyles
                         &&& readStyles ))
                  >>?%? chooseMax
 -- all top elements are always on the same hierarchy level

--
readStyles :: StyleReader _x Styles
readStyles = executeInSub NsOffice "styles" $ liftAsSuccess
  $ liftA3 Styles
    ( tryAll NsStyle "style"         readStyle        >>^ M.fromList )
    ( tryAll NsText  "list-style"    readListStyle    >>^ M.fromList )
    ( tryAll NsStyle "default-style" readDefaultStyle >>^ M.fromList )

--
readAutomaticStyles :: StyleReader _x Styles
readAutomaticStyles = executeInSub NsOffice "automatic-styles" $ liftAsSuccess
  $ liftA3 Styles
    ( tryAll NsStyle "style"         readStyle        >>^ M.fromList )
    ( tryAll NsText  "list-style"    readListStyle    >>^ M.fromList )
    ( returnV M.empty                                                )

--
readDefaultStyle :: StyleReader _x (StyleFamily, StyleProperties)
readDefaultStyle =      lookupAttr NsStyle "family"
                   >>?! keepingTheValue readStyleProperties

--
readStyle :: StyleReader _x (StyleName,Style)
readStyle =      findAttr NsStyle "name"
            >>?! keepingTheValue
                   ( liftA4 Style
                       ( lookupAttr' NsStyle "family"            )
                       ( findAttr'   NsStyle "parent-style-name" )
                       ( findAttr'   NsStyle "list-style-name"   )
                       readStyleProperties
                   )

--
readStyleProperties :: StyleReaderSafe _x StyleProperties
readStyleProperties = liftA2 SProps
                       ( readTextProperties >>> choiceToMaybe )
                       ( readParaProperties >>> choiceToMaybe )

--
readTextProperties :: StyleReader _x TextProperties
readTextProperties =
  executeInSub NsStyle "text-properties" $ liftAsSuccess
    ( liftA6 PropT
       ( searchAttr   NsXSL_FO "font-style"  False isFontEmphasised )
       ( searchAttr   NsXSL_FO "font-weight" False isFontBold       )
       findPitch
       ( getAttr      NsStyle  "text-position"                      )
       readUnderlineMode
       readStrikeThroughMode
     )
  where isFontEmphasised = [("normal",False),("italic",True),("oblique",True)]
        isFontBold = ("normal",False):("bold",True)
                    :map ((,True).show) ([100,200..900]::[Int])

readUnderlineMode     :: StyleReaderSafe _x (Maybe UnderlineMode)
readUnderlineMode     = readLineMode "text-underline-mode"
                                     "text-underline-style"

readStrikeThroughMode :: StyleReaderSafe _x (Maybe UnderlineMode)
readStrikeThroughMode = readLineMode "text-line-through-mode"
                                     "text-line-through-style"

readLineMode :: String -> String -> StyleReaderSafe _x (Maybe UnderlineMode)
readLineMode modeAttr styleAttr = proc x -> do
  isUL <- searchAttr  NsStyle styleAttr False isLinePresent -< x
  mode <- lookupAttr' NsStyle  modeAttr                     -< x
  if isUL
    then case mode of
           Just m  -> returnA -< Just m
           Nothing -> returnA -< Just UnderlineModeNormal
    else              returnA -< Nothing
  where
    isLinePresent = ("none",False) : map (,True)
                    [ "dash"      , "dot-dash" , "dot-dot-dash" , "dotted"
                    , "long-dash" , "solid"    , "wave"
                    ]

--
readParaProperties :: StyleReader _x ParaProperties
readParaProperties =
   executeInSub NsStyle "paragraph-properties" $ liftAsSuccess
     ( liftA3 PropP
       ( liftA2 readNumbering
         ( isSet'           NsText   "number-lines"           )
         ( readAttr'        NsText   "line-number"            )
       )
       ( liftA2 readIndentation
         ( isSetWithDefault NsStyle  "auto-text-indent" False )
         ( getAttr          NsXSL_FO "text-indent"            )
       )
       (   getAttr          NsXSL_FO "margin-left"            )
     )
  where readNumbering (Just True) (Just n) = NumberingRestart n
        readNumbering (Just True)  _       = NumberingKeep
        readNumbering      _       _       = NumberingNone

        readIndentation False indent = indent
        readIndentation True  _      = def

----
-- List styles
----

--
readListStyle :: StyleReader _x (StyleName, ListStyle)
readListStyle =
       findAttr NsStyle "name"
  >>?! keepingTheValue
       ( liftA ListStyle
         $ liftA3 SM.union3
             ( readListLevelStyles NsText "list-level-style-number" LltNumbered )
             ( readListLevelStyles NsText "list-level-style-bullet" LltBullet   )
             ( readListLevelStyles NsText "list-level-style-image"  LltImage    ) >>^ M.mapMaybe chooseMostSpecificListLevelStyle
       )
--
readListLevelStyles :: Namespace -> ElementName
                    -> ListLevelType
                    -> StyleReaderSafe _x (SM.SetMap Int ListLevelStyle)
readListLevelStyles namespace elementName levelType =
  tryAll namespace elementName (readListLevelStyle levelType)
    >>^ SM.fromList

--
readListLevelStyle :: ListLevelType -> StyleReader _x (Int, ListLevelStyle)
readListLevelStyle levelType =      readAttr NsText "level"
                               >>?! keepingTheValue
                                    ( liftA5 toListLevelStyle
                                      ( returnV       levelType             )
                                      ( findAttr'     NsStyle "num-prefix"  )
                                      ( findAttr'     NsStyle "num-suffix"  )
                                      ( getAttr       NsStyle "num-format"  )
                                      ( findAttrText' NsText  "start-value" )
                                    )
  where
  toListLevelStyle _ p s LinfNone b         = ListLevelStyle LltBullet p s LinfNone (startValue b)
  toListLevelStyle _ p s f@(LinfString _) b = ListLevelStyle LltBullet p s f (startValue b)
  toListLevelStyle t p s f b                = ListLevelStyle t      p s f (startValue b)
  startValue mbx = fromMaybe 1 (mbx >>= safeRead)

--
chooseMostSpecificListLevelStyle :: S.Set ListLevelStyle -> Maybe ListLevelStyle
chooseMostSpecificListLevelStyle ls | ls == mempty = Nothing
                                    | otherwise    = Just ( F.foldr1 select ls )
  where
   select ( ListLevelStyle       t1            p1          s1          f1          b1 )
          ( ListLevelStyle       t2            p2          s2          f2          _ )
        =   ListLevelStyle (select' t1 t2) (p1 <|> p2) (s1 <|> s2) (selectLinf f1 f2) b1
   select' LltNumbered _           = LltNumbered
   select' _           LltNumbered = LltNumbered
   select' _           _           = LltBullet
   selectLinf LinfNone       f2             = f2
   selectLinf f1             LinfNone       = f1
   selectLinf (LinfString _) f2             = f2
   selectLinf f1             (LinfString _) = f1
   selectLinf f1             _              = f1


--------------------------------------------------------------------------------
-- Tools to access style data
--------------------------------------------------------------------------------

--
lookupStyle           :: StyleName   -> Styles -> Maybe Style
lookupStyle name Styles{..} = M.lookup name stylesByName

--
lookupDefaultStyle'   :: Styles -> StyleFamily -> StyleProperties
lookupDefaultStyle' Styles{..} family = fromMaybe def
                                        (M.lookup family defaultStyleMap)

--
lookupListStyleByName :: StyleName   -> Styles -> Maybe ListStyle
lookupListStyleByName name Styles{..} = M.lookup name listStylesByName


-- | Returns a chain of parent of the current style. The direct parent will
-- be the first element of the list, followed by its parent and so on.
-- The current style is not in the list.
parents               :: Style       -> Styles ->      [Style]
parents style styles = unfoldr findNextParent style -- Ha!
  where findNextParent Style{..}
          = fmap duplicate $ (`lookupStyle` styles) =<< styleParentName

-- | Looks up the style family of the current style. Normally, every style
-- should have one. But if not, all parents are searched.
getStyleFamily        :: Style       -> Styles -> Maybe StyleFamily
getStyleFamily style@Style{..} styles
  =     styleFamily
    <|> F.asum (map (`getStyleFamily` styles) $ parents style styles)

-- | Each 'Style' has certain 'StyleProperties'. But sometimes not all property
-- values are specified. Instead, a value might be inherited from a
-- parent style. This function makes this chain of inheritance
-- concrete and easily accessible by encapsulating the necessary lookups.
-- The resulting list contains the direct properties of the style as the first
-- element, the ones of the direct parent element as the next one, and so on.
--
-- Note: There should also be default properties for each style family. These
--       are @not@ contained in this list because properties inherited from
--       parent elements take precedence over default styles.
--
-- This function is primarily meant to be used through convenience wrappers.
--
stylePropertyChain    :: Style       -> Styles -> [StyleProperties]
stylePropertyChain style styles
  = map styleProperties (style : parents style styles)

--
extendedStylePropertyChain :: [Style] -> Styles -> [StyleProperties]
extendedStylePropertyChain [] _ = []
extendedStylePropertyChain [style]       styles =    stylePropertyChain style styles
                                                  ++ maybeToList (fmap (lookupDefaultStyle' styles) (getStyleFamily style styles))
extendedStylePropertyChain (style:trace) styles =    stylePropertyChain style styles
                                                  ++ extendedStylePropertyChain trace styles
