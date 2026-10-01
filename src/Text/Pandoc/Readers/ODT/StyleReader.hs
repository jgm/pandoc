{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.ODT.StyleReader
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

Reader for the style information in an odt document.
-}

module Text.Pandoc.Readers.ODT.StyleReader
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

import Control.Applicative ((<|>), optional)

import Data.Default
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as S

import qualified Text.Pandoc.XML.Light as XML

import Text.Pandoc.Shared (safeRead, tshow)

import Text.Pandoc.Readers.ODT.Generic.Fallible
import qualified Text.Pandoc.Readers.ODT.Generic.SetMap as SM
import Text.Pandoc.Readers.ODT.Generic.Utils
import Text.Pandoc.Readers.ODT.Generic.XMLConverter

import Text.Pandoc.Readers.ODT.Namespaces

readStylesAt :: XML.Element -> Fallible Styles
readStylesAt e = runConverter readAllStyles mempty e

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

type FontFaceName = Text

type FontPitches = M.Map FontFaceName FontPitch

-- To get there, the fonts have to be read and the pitches extracted.
-- But the resulting map is only needed at one later place, so it should not be
-- transported on the value level. Instead, it is lifted into the state of the
-- reader.
--
-- So the main style readers will have the type
type StyleReader a = XMLConverter Namespace FontPitches a
--
-- But before we can work with this, we need to define the reader that reads
-- the fonts. It is polymorphic in the extra state, since the pitches are not
-- yet available while it runs.

-- | A reader for font pitches
fontPitchReader :: XMLConverter Namespace s FontPitches
fontPitchReader =
  executeInSub NsOffice "font-face-decls" (do
    fonts <- withEveryL NsStyle "font-face" $ do
               name  <- findAttr' NsStyle "name"
               pitch <- lookupDefaultingAttr NsStyle "font-pitch"
               return (name, pitch)
    return $ M.fromList [ (n, p) | (Just n, p) <- fonts ])
  <|> return M.empty

-- | Looking up a pitch in the extra state.
--
-- The function does the following:
-- * Look for the font pitch in an attribute.
-- * If that fails, look for the font name, look up the font in the state
--   and use the pitch from there.
-- * Return the result in a Maybe
--
findPitch :: StyleReader (Maybe FontPitch)
findPitch = optional
  $     lookupAttr NsStyle "font-pitch"
    <|> (do fontName <- findAttr NsStyle "font-name"
            pitches  <- getExtraState
            fromMaybeF $ M.lookup fontName pitches)

--------------------------------------------------------------------------------
-- Definitions of main data
--------------------------------------------------------------------------------

type StyleName        = Text

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
estimateInMillimeter :: Double -> XslUnit -> Int
estimateInMillimeter n XslUnitMM     = round n
estimateInMillimeter n XslUnitCM     = round $ n * 10
estimateInMillimeter n XslUnitInch   = round $ n * 25.4
estimateInMillimeter n XslUnitPoints = round $ n * (1/72) * 25.4
estimateInMillimeter n XslUnitPica   = round $ n * 12 * (1/72) * 25.4
estimateInMillimeter n XslUnitPixel  = round $ n * (1/72) * 25.4
estimateInMillimeter n XslUnitEM     = round $ n * 16 * (1/72) * 25.4


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
                                     , listItemPrefix :: Maybe Text
                                     , listItemSuffix :: Maybe Text
                                     , listItemFormat :: ListItemNumberFormat
                                     , listItemStart  :: Int
                                     }
  deriving ( Eq, Ord )

instance Show ListLevelStyle where
  show ListLevelStyle{..} =    "<LLS|"
                            ++ show listLevelType
                            ++ "|"
                            ++ maybeToString (T.unpack <$> listItemPrefix)
                            ++ show listItemFormat
                            ++ maybeToString (T.unpack <$> listItemSuffix)
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
readAllStyles :: StyleReader Styles
readAllStyles = do
  fontPitchReader >>= setExtraState
  autoStyles <- tryC readAutomaticStyles
  styles     <- tryC readStyles
  fromFallible $ chooseMax autoStyles styles
 -- all top elements are always on the same hierarchy level

--
readStyles :: StyleReader Styles
readStyles = executeInSub NsOffice "styles" $
  Styles <$> (M.fromList <$> tryAll NsStyle "style"         readStyle       )
         <*> (M.fromList <$> tryAll NsText  "list-style"    readListStyle   )
         <*> (M.fromList <$> tryAll NsStyle "default-style" readDefaultStyle)

--
readAutomaticStyles :: StyleReader Styles
readAutomaticStyles = executeInSub NsOffice "automatic-styles" $
  Styles <$> (M.fromList <$> tryAll NsStyle "style"         readStyle       )
         <*> (M.fromList <$> tryAll NsText  "list-style"    readListStyle   )
         <*> pure M.empty

--
readDefaultStyle :: StyleReader (StyleFamily, StyleProperties)
readDefaultStyle = (,) <$> lookupAttr NsStyle "family"
                       <*> readStyleProperties

--
readStyle :: StyleReader (StyleName, Style)
readStyle = do
  name  <- findAttr NsStyle "name"
  style <- Style <$> lookupAttr' NsStyle "family"
                 <*> findAttr'   NsStyle "parent-style-name"
                 <*> findAttr'   NsStyle "list-style-name"
                 <*> readStyleProperties
  return (name, style)

--
readStyleProperties :: StyleReader StyleProperties
readStyleProperties = SProps <$> optional readTextProperties
                             <*> optional readParaProperties

--
readTextProperties :: StyleReader TextProperties
readTextProperties =
  executeInSub NsStyle "text-properties" $
    PropT <$> searchAttr NsXSL_FO "font-style"  False isFontEmphasised
          <*> searchAttr NsXSL_FO "font-weight" False isFontBold
          <*> findPitch
          <*> getAttr    NsStyle  "text-position"
          <*> readUnderlineMode
          <*> readStrikeThroughMode
  where isFontEmphasised = [("normal",False),("italic",True),("oblique",True)]
        -- As in CSS, `bold` is weight 700; lighter weights are not bold.
        isFontBold = ("normal",False):("bold",True)
                    :map (\w -> (tshow w, w >= 700)) ([100,200..900]::[Int])

readUnderlineMode     :: StyleReader (Maybe UnderlineMode)
readUnderlineMode     = readLineMode "text-underline-mode"
                                     "text-underline-style"

readStrikeThroughMode :: StyleReader (Maybe UnderlineMode)
readStrikeThroughMode = readLineMode "text-line-through-mode"
                                     "text-line-through-style"

readLineMode :: Text -> Text -> StyleReader (Maybe UnderlineMode)
readLineMode modeAttr styleAttr = do
  isUL <- searchAttr  NsStyle styleAttr False isLinePresent
  mode <- lookupAttr' NsStyle  modeAttr
  return $ if isUL
             then mode <|> Just UnderlineModeNormal
             else Nothing
  where
    isLinePresent = ("none",False) : map (,True)
                    [ "dash"      , "dot-dash" , "dot-dot-dash" , "dotted"
                    , "long-dash" , "solid"    , "wave"
                    ]

--
readParaProperties :: StyleReader ParaProperties
readParaProperties =
  executeInSub NsStyle "paragraph-properties" $
    PropP <$> ( readNumbering
                  <$> isSet'    NsText "number-lines"
                  <*> readAttr' NsText "line-number" )
          <*> ( readIndentation
                  <$> isSetWithDefault NsStyle  "auto-text-indent" False
                  <*> getAttr          NsXSL_FO "text-indent" )
          <*> getAttr NsXSL_FO "margin-left"
  where readNumbering (Just True) (Just n) = NumberingRestart n
        readNumbering (Just True)  _       = NumberingKeep
        readNumbering      _       _       = NumberingNone

        readIndentation False indent = indent
        readIndentation True  _      = def

----
-- List styles
----

--
readListStyle :: StyleReader (StyleName, ListStyle)
readListStyle = do
  name   <- findAttr NsStyle "name"
  styles <- SM.union3
              <$> readListLevelStyles NsText "list-level-style-number" LltNumbered
              <*> readListLevelStyles NsText "list-level-style-bullet" LltBullet
              <*> readListLevelStyles NsText "list-level-style-image"  LltImage
  return ( name
         , ListStyle $ M.mapMaybe chooseMostSpecificListLevelStyle styles )

--
readListLevelStyles :: Namespace -> ElementName
                    -> ListLevelType
                    -> StyleReader (SM.SetMap Int ListLevelStyle)
readListLevelStyles namespace elementName levelType =
  SM.fromList <$> tryAll namespace elementName (readListLevelStyle levelType)

--
readListLevelStyle :: ListLevelType -> StyleReader (Int, ListLevelStyle)
readListLevelStyle levelType = do
  level <- readAttr NsText "level"
  style <- toListLevelStyle
             <$> findAttr' NsStyle "num-prefix"
             <*> findAttr' NsStyle "num-suffix"
             <*> getAttr   NsStyle "num-format"
             <*> findAttr' NsText  "start-value"
  return (level, style)
  where
  toListLevelStyle p s LinfNone b         = ListLevelStyle LltBullet p s LinfNone (startValue b)
  toListLevelStyle p s f@(LinfString _) b = ListLevelStyle LltBullet p s f (startValue b)
  toListLevelStyle p s f b                = ListLevelStyle levelType p s f (startValue b)
  startValue mbx = fromMaybe 1 (mbx >>= safeRead)

--
chooseMostSpecificListLevelStyle :: S.Set ListLevelStyle -> Maybe ListLevelStyle
chooseMostSpecificListLevelStyle ls = F.foldr select Nothing ls
  where
   select l Nothing = Just l
   select ( ListLevelStyle t1 p1 s1 f1 b1 )
          ( Just ( ListLevelStyle t2 p2 s2 f2 _ ))
        =   Just $ ListLevelStyle (select' t1 t2) (p1 <|> p2) (s1 <|> s2)
                                  (selectLinf f1 f2) b1
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
-- Note that @style:parent-style-name@ references can be cyclic, so we
-- keep track of the names already seen to avoid looping forever.
parents               :: Style       -> Styles ->      [Style]
parents style styles = go S.empty style
  where
    go seen Style{..}
      | Just name <- styleParentName
      , not (name `S.member` seen)
      , Just parent <- lookupStyle name styles
      = parent : go (S.insert name seen) parent
      | otherwise
      = []

-- | Looks up the style family of the current style. Normally, every style
-- should have one. But if not, all parents are searched.
-- Searching the parents recursively would repeatedly walk the same
-- chains, taking time exponential in its length; the chain is linear,
-- so we can just walk it once.
getStyleFamily        :: Style       -> Styles -> Maybe StyleFamily
getStyleFamily style styles
  = F.asum $ map styleFamily $ style : parents style styles

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
