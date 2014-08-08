{-# LANGUAGE PatternGuards #-}

{-
Copyright (C) 2014 Jesse Rosenthal <jrosenthal@jhu.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
-}

{- |
 Module : Text.Pandoc.Readers.Docx.Math
 Copyright : Copyright (C) 2014 Jesse Rosenthal
 License : GNU GPL, version 2 or above

 Maintainer : Jesse Rosenthal <jrosenthal@jhu.edu>
 Stability : alpha
 Portability : portable

Types and functions for conversion of OMML into TeXMath.
-}

module Text.Pandoc.Readers.Docx.OMath  ( elemToExps
                                       ) where

import Text.XML.Light
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (intersperse)
import qualified Text.TeXMath.Types as TM
import Control.Applicative ((<$>))

isElem :: String -> String -> Element -> Bool
isElem prefix name element =
  let qp = fromMaybe "" (qPrefix (elName element))
  in
   qName (elName element) == name &&
   qp == prefix

hasElemName:: String -> String -> QName -> Bool
hasElemName prefix name qn =
  let qp = fromMaybe "" (qPrefix qn)
  in
   qName qn == name &&
   qp       == prefix

data OMath = OMath [OMathElem]
          deriving Show

data OMathElem = Accent AccentStyle Base
              | Bar BarStyle Base
              | Box Base
              | BorderBox Base
              | Delimiter DelimStyle [Base]
              | EquationArray [Base]
              | Fraction [OMathElem] [OMathElem]
              | Function [OMathElem] Base
              | Group GroupStyle Base
              | LowerLimit Base [OMathElem]
              | UpperLimit Base [OMathElem]
              | Matrix [[Base]]
              | NAry NAryStyle [OMathElem] [OMathElem] Base
              | Phantom Base
              | Radical [OMathElem] Base
              | PreSubSuper [OMathElem] [OMathElem] Base
              | Sub Base [OMathElem]
              | SubSuper Base [OMathElem] [OMathElem]
              | Super Base [OMathElem]
              | OMathRun OMathRunStyle [OMathRunElem]
              deriving Show

data OMathRunElem = TextRun String
                  | LnBrk
                  | Tab
                    deriving Show

data Base = Base [OMathElem]
          deriving Show

data TopBottom = Top | Bottom
               deriving Show

data AccentStyle = AccentStyle { accentChar :: Maybe Char }
                 deriving Show

data BarStyle = BarStyle { barPos :: TopBottom}
              deriving Show

data NAryStyle = NAryStyle { nAryChar :: Maybe Char
                           , nAryLimLoc :: LimLoc}
               deriving Show

data OMathRunStyle = OMathRunStyle { oMathLit :: Bool
                                   , oMathRunTextStyle :: OMathRunTextStyle }
                   deriving Show

data OMathRunTextStyle = NoStyle
                       | Normal
                       | Styled { oMathScript :: Maybe OMathTextScript
                                , oMathStyle  :: Maybe OMathTextStyle }
                       deriving Show

data OMathTextScript = ORoman
                     | OScript
                     | OFraktur
                     | ODoubleStruck
                     | OSansSerif
                     | OMonospace
                     deriving (Show, Eq)

data OMathTextStyle = OPlain
                    | OBold
                    | OItalic
                    | OBoldItalic
                    deriving (Show, Eq)

defaultNAryStyle :: NAryStyle
defaultNAryStyle = NAryStyle { nAryChar = Nothing -- integral, in practice
                             , nAryLimLoc = SubSup }

data LimLoc = SubSup | UnderOver deriving Show

data DelimStyle = DelimStyle { delimBegChar :: Maybe Char
                             , delimSepChar :: Maybe Char
                             , delimEndChar :: Maybe Char}
                  deriving Show

defaultDelimStyle :: DelimStyle
defaultDelimStyle = DelimStyle { delimBegChar = Nothing
                               , delimSepChar = Nothing
                               , delimEndChar = Nothing }

data GroupStyle = GroupStyle { groupChr :: Maybe Char
                             , groupPos :: Maybe TopBottom }
                  deriving Show

defaultGroupStyle :: GroupStyle
defaultGroupStyle = GroupStyle {groupChr = Nothing, groupPos = Nothing}

elemToMath :: Element -> Maybe OMath
elemToMath element  | isElem "m" "oMath" element =
  Just $ OMath $ mapMaybe (elemToMathElem) (elChildren element)
elemToMath _ = Nothing

elemToBase :: Element -> Maybe Base
elemToBase element | isElem "m" "e" element =
  Just $ Base $ mapMaybe (elemToMathElem) (elChildren element)
elemToBase _ = Nothing

-- TODO: The right way to do this is to use the ampersand to break the
-- text lines into multiple columns. That's tricky, though, and this
-- will get us most of the way for the time being.
filterAmpersand :: OMathElem -> OMathElem
filterAmpersand (OMathRun mrPr elems) =
  let f (TextRun s) = TextRun $ filter ('&' /=) s
      f re          = re
  in
   OMathRun mrPr (map f elems)
filterAmpersand e = e

elemToBaseNoAmpersand :: Element -> Maybe Base
elemToBaseNoAmpersand element | isElem "m" "e" element =
  return $ Base $ 
  mapMaybe
  (\e -> (elemToMathElem e >>= (return . filterAmpersand)))
  (elChildren element)
elemToBaseNoAmpersand _ = Nothing

elemToOMathRunStyle :: Element -> OMathRunStyle
elemToOMathRunStyle element =
  let lit =
        case
          filterChildName (hasElemName"m" "lit") element >>=
          findAttrBy (hasElemName"m" "val")
        of
          Just "on" -> True
          _         -> False
  in
   OMathRunStyle { oMathLit = lit
                 , oMathRunTextStyle = (elemToOMathRunTextStyle element)
                 }

elemToOMathRunTextStyle :: Element -> OMathRunTextStyle
elemToOMathRunTextStyle element
  | Just mrPr <- filterChildName (hasElemName"m" "rPr") element
  , Just _    <- filterChildName (hasElemName"m" "nor") mrPr =
    Normal
  | Just mrPr <- filterChildName (hasElemName"m" "rPr") element =
    let scr =
          case
            filterChildName (hasElemName"m" "scr") mrPr >>=
            findAttrBy (hasElemName"m" "val") 
          of
            Just "roman"         -> Just ORoman
            Just "script"        -> Just OScript
            Just "fraktur"       -> Just OFraktur
            Just "double-struck" -> Just ODoubleStruck
            Just "sans-serif"    -> Just OSansSerif
            Just "monospace"     -> Just OMonospace
            _                    -> Nothing

        sty =
          case
            filterChildName (hasElemName"m" "sty") mrPr >>=
            findAttrBy (hasElemName"m" "val")
          of
            Just "p"             -> Just OPlain
            Just "b"             -> Just OBold
            Just "i"             -> Just OItalic
            Just "bi"            -> Just OBoldItalic
            _                    -> Nothing
    in
     Styled { oMathScript = scr, oMathStyle = sty }
  | otherwise = NoStyle



elemToNAryStyle :: Element -> NAryStyle
elemToNAryStyle element
  | Just narypr <- filterChildName (hasElemName"m" "naryPr") element =
  let
    chr = filterChildName (hasElemName"m" "chr") narypr >>=
          findAttrBy (hasElemName"m" "val") >>=
          Just . head
    limLoc = filterChildName (hasElemName"m" "limLoc") narypr >>=
             findAttrBy (hasElemName"m" "val")
    limLoc' = case limLoc of
      Just "undOver" -> UnderOver
      Just "subSup"  -> SubSup
      _              -> SubSup
  in
   NAryStyle { nAryChar = chr, nAryLimLoc = limLoc'}
elemToNAryStyle _ = defaultNAryStyle

elemToDelimStyle :: Element -> DelimStyle
elemToDelimStyle element
  | Just dPr <- filterChildName (hasElemName"m" "dPr") element =
    let begChr = filterChildName (hasElemName"m" "begChr") dPr >>=
                 findAttrBy (hasElemName"m" "val") >>=
                 (\c -> if null c then (Just ' ') else (Just $ head c))
        sepChr = filterChildName (hasElemName"m" "sepChr") dPr >>=
                 findAttrBy (hasElemName"m" "val") >>=
                 (\c -> if null c then (Just ' ') else (Just $ head c))
        endChr = filterChildName (hasElemName"m" "endChr") dPr >>=
                 findAttrBy (hasElemName"m" "val") >>=
                 (\c -> if null c then (Just ' ') else (Just $ head c))
    in
     DelimStyle { delimBegChar = begChr
                , delimSepChar = sepChr
                , delimEndChar = endChr}
elemToDelimStyle _ = defaultDelimStyle

elemToGroupStyle :: Element -> GroupStyle
elemToGroupStyle element
  | Just gPr <- filterChildName (hasElemName"m" "groupChrPr") element =
    let chr = filterChildName (hasElemName"m" "chr") gPr >>=
              findAttrBy (hasElemName"m" "val") >>=
              Just . head
        pos = filterChildName (hasElemName"m" "pos") gPr >>=
              findAttrBy (hasElemName"m" "val") >>=
              (\s -> Just $ if s == "top" then Top else Bottom)
    in
     GroupStyle { groupChr = chr, groupPos = pos }
elemToGroupStyle _ = defaultGroupStyle

elemToMathElem :: Element -> Maybe OMathElem
elemToMathElem element | isElem "m" "acc" element = do
  let accChar =
        filterChildName (hasElemName"m" "accPr") element >>=
        filterChildName (hasElemName"m" "chr") >>=
        findAttrBy (hasElemName"m" "val") >>=
        Just . head
      accPr = AccentStyle { accentChar = accChar}
  base <- filterChildName (hasElemName"m" "e") element >>=
         elemToBase
  return $ Accent accPr base
elemToMathElem element | isElem "m" "bar" element = do
  barPr <- filterChildName (hasElemName"m" "barPr") element >>=
           filterChildName (hasElemName"m" "pos") >>=
           findAttrBy (hasElemName"m" "val") >>=
           (\s ->
             Just $ BarStyle {
               barPos = (if s == "bot" then Bottom else Top)
               })
  base <- filterChildName (hasElemName"m" "e") element >>=
          elemToBase
  return $ Bar barPr base
elemToMathElem element | isElem "m" "box" element =
  filterChildName (hasElemName"m" "e") element >>=
  elemToBase >>=
  (\b -> return $ Box b)
elemToMathElem element | isElem "m" "borderBox" element =
  filterChildName (hasElemName"m" "e") element >>=
  elemToBase >>=
  (\b -> return $ BorderBox b)
elemToMathElem element | isElem "m" "d" element =
  let style = elemToDelimStyle element
  in
   return $ Delimiter style $ mapMaybe (elemToBase) (elChildren element)
elemToMathElem element | isElem "m" "eqArr" element =
  return $ EquationArray $ mapMaybe (elemToBaseNoAmpersand) (elChildren element) 
elemToMathElem element | isElem "m" "f" element = do
  num <- filterChildName (hasElemName"m" "num") element
  den <- filterChildName (hasElemName"m" "den") element
  let numElems = mapMaybe (elemToMathElem) (elChildren num)
      denElems = mapMaybe (elemToMathElem) (elChildren den)
  return $ Fraction numElems denElems
elemToMathElem element | isElem "m" "func" element = do
  fName <- filterChildName (hasElemName"m" "fName") element
  base <- filterChildName (hasElemName"m" "e") element >>=
          elemToBase
  let fnElems = mapMaybe (elemToMathElem) (elChildren fName)
  return $ Function fnElems base
elemToMathElem element | isElem "m" "groupChr" element =
  let style = elemToGroupStyle element
  in
   filterChildName (hasElemName"m" "e") element >>=
   elemToBase >>=
   (\b -> return $ Group style b)
elemToMathElem element | isElem "m" "limLow" element = do
  base <- filterChildName (hasElemName"m" "e") element
          >>= elemToBase
  lim <- filterChildName (hasElemName"m" "lim") element
  let limElems = mapMaybe (elemToMathElem) (elChildren lim)
  return $ LowerLimit base limElems
elemToMathElem element | isElem "m" "limUpp" element = do
  base <- filterChildName (hasElemName"m" "e") element
          >>= elemToBase
  lim <- filterChildName (hasElemName"m" "lim") element
  let limElems = mapMaybe (elemToMathElem) (elChildren lim)
  return $ UpperLimit base limElems
elemToMathElem element | isElem "m" "m" element = do
  let rows = filterChildrenName (hasElemName"m" "mr") element
  let bases = mapMaybe (\mr -> mapM (elemToBase) (elChildren mr)) rows
  return $ Matrix bases
elemToMathElem element | isElem "m" "nary" element = do
  let style = elemToNAryStyle element
  sub <- filterChildName (hasElemName"m" "sub") element >>=
         (\e -> return $ mapMaybe (elemToMathElem) (elChildren e))
  sup <- filterChildName (hasElemName"m" "sup") element >>=
         (\e -> return $ mapMaybe (elemToMathElem) (elChildren e))
  base <- filterChildName (hasElemName"m" "e") element >>=
          elemToBase
  return $ NAry style sub sup base
elemToMathElem element | isElem "m" "rad" element = do
  deg <- filterChildName (hasElemName"m" "deg") element >>=
         (\e -> return $ mapMaybe (elemToMathElem) (elChildren e))
  base <- filterChildName (hasElemName"m" "e") element >>=
          elemToBase
  return $ Radical deg base
elemToMathElem element | isElem "m" "phant" element = do
  base <- filterChildName (hasElemName"m" "e") element >>=
          elemToBase
  return $ Phantom base
elemToMathElem element | isElem "m" "sPre" element = do
  sub <- filterChildName (hasElemName"m" "sub") element >>=
         (\e -> return $ mapMaybe (elemToMathElem) (elChildren e))
  sup <- filterChildName (hasElemName"m" "sup") element >>=
         (\e -> return $ mapMaybe (elemToMathElem) (elChildren e))
  base <- filterChildName (hasElemName"m" "e") element >>=
          elemToBase
  return $ PreSubSuper sub sup base
elemToMathElem element | isElem "m" "sSub" element = do
  base <- filterChildName (hasElemName"m" "e") element >>=
          elemToBase
  sub <- filterChildName (hasElemName"m" "sub") element >>=
         (\e -> return $ mapMaybe (elemToMathElem) (elChildren e))
  return $ Sub base sub
elemToMathElem element | isElem "m" "sSubSup" element = do
  base <- filterChildName (hasElemName"m" "e") element >>=
          elemToBase
  sub <- filterChildName (hasElemName"m" "sub") element >>=
         (\e -> return $ mapMaybe (elemToMathElem) (elChildren e))
  sup <- filterChildName (hasElemName"m" "sup") element >>=
         (\e -> return $ mapMaybe (elemToMathElem) (elChildren e))
  return $ SubSuper base sub sup
elemToMathElem element | isElem "m" "sSup" element = do
  base <- filterChildName (hasElemName"m" "e") element >>=
          elemToBase
  sup <- filterChildName (hasElemName"m" "sup") element >>=
         (\e -> return $ mapMaybe (elemToMathElem) (elChildren e))
  return $ Super base sup
elemToMathElem element | isElem "m" "r" element = do
  let mrPr = elemToOMathRunStyle element
  mrElems <- elemToOMathRunElems element
  return $ OMathRun mrPr mrElems
elemToMathElem _ = Nothing

elemToOMathRunElem :: Element -> Maybe OMathRunElem
elemToOMathRunElem element
  | isElem "w" "t" element
    || isElem "m" "t" element 
    || isElem "w" "delText" element = Just $ TextRun $ strContent element
  | isElem "w" "br" element = Just LnBrk
  | isElem "w" "tab" element = Just Tab
  | otherwise = Nothing

elemToOMathRunElems :: Element -> Maybe [OMathRunElem]
elemToOMathRunElems element
  | isElem "w" "r" element
    || isElem "m" "r" element =
      Just $ mapMaybe (elemToOMathRunElem) (elChildren element)
elemToOMathRunElems _ = Nothing

----- And now the TeXMath Creation

oMathRunElemToString :: OMathRunElem -> String
oMathRunElemToString (TextRun s) = s
oMathRunElemToString (LnBrk) = ['\n']
oMathRunElemToString (Tab) = ['\t']

oMathRunElemsToString :: [OMathRunElem] -> String
oMathRunElemsToString = concatMap oMathRunElemToString

oMathElemToString :: OMathElem -> String
oMathElemToString (OMathRun _ oMathRunElems) =
  oMathRunElemsToString oMathRunElems
oMathElemToString _ = ""


oMathToExps :: OMath -> [TM.Exp]
oMathToExps (OMath oMathElems) = concatMap oMathElemToExps oMathElems

oMathElemToExps :: OMathElem -> [TM.Exp]
oMathElemToExps (Accent style base) = 
  let baseExp = baseToExp base
      chr = case accentChar style of
        Just c -> c
        Nothing -> '\180'       -- default to acute.
  in
   [TM.EOver False baseExp (TM.ESymbol TM.Accent [chr])]
oMathElemToExps(Bar style base) =
  let baseExp = baseToExp base
  in
   case barPos style of
     Top    -> [TM.EOver False baseExp (TM.ESymbol TM.Accent "\175")]
     Bottom -> [TM.EUnder False baseExp (TM.ESymbol TM.Accent "\818")]
oMathElemToExps (Box base) = [baseToExp base]
oMathElemToExps (BorderBox base) =
  -- TODO: This should be "\\boxed" somehow
  [baseToExp base]
oMathElemToExps (Delimiter dPr bases) =
  let baseExps = map baseToExp bases
      inDelimExps = map Right baseExps
      beg = fromMaybe '(' (delimBegChar dPr)
      end = fromMaybe ')' (delimEndChar dPr)
      sep = fromMaybe '|' (delimSepChar dPr)
      exps = intersperse (Left [sep]) inDelimExps
  in
   [TM.EDelimited [beg] [end] exps]
oMathElemToExps (EquationArray bases) =
  let baseExps = map (\b -> [baseToExp' b]) bases
  in
   [TM.EArray [] baseExps]
oMathElemToExps (Fraction num denom) =
  let numExp = TM.EGrouped $ concatMap oMathElemToExps num
      denExp = TM.EGrouped $ concatMap oMathElemToExps denom
  in
   [TM.EFraction TM.NormalFrac numExp denExp]
oMathElemToExps (Function fname base) =
  -- We need a string for the fname, but omml gives it to us as a
  -- series of oMath elems. We're going to filter out the oMathRuns,
  -- which should work for us most of the time.
  let fnameString = concatMap oMathElemToString fname
      baseExp  = baseToExp base
  in
   [TM.EMathOperator fnameString, baseExp]
oMathElemToExps (Group style base)
  | Just Top <- groupPos style =
    let baseExp = baseToExp base
        chr = case groupChr style of
          Just c -> c
          Nothing -> '\65079'   -- default to overbrace
    in
     [TM.EOver False baseExp (TM.ESymbol TM.Accent [chr])]
  | otherwise = 
    let baseExp = baseToExp base 
        chr = case groupChr style of
          Just c -> c
          Nothing -> '\65080'   -- default to underbrace
    in
     [TM.EUnder False baseExp (TM.ESymbol TM.Accent [chr])]
oMathElemToExps (LowerLimit base limElems) = do
  let baseExp = baseToExp base
      lim = TM.EGrouped $ concatMap oMathElemToExps limElems
   in
    [TM.EUnder True lim baseExp]
oMathElemToExps (UpperLimit base limElems) =
  let baseExp = baseToExp base
      lim = TM.EGrouped $ concatMap oMathElemToExps limElems
  in
   [TM.EOver True lim baseExp]
oMathElemToExps (Matrix bases) =
  let rows = map (map baseToExp') bases
  in
   [TM.EArray [TM.AlignCenter] rows]
oMathElemToExps (NAry style sub sup base) =
  let 
    subExps = concatMap oMathElemToExps sub
    supExps = concatMap oMathElemToExps sup
    baseExp =  baseToExp base
    opChar = case nAryChar style of
      Just c -> c
      -- default to integral
      Nothing -> '\8747'
  in [ TM.ESubsup
       (TM.ESymbol TM.Op [opChar])
       (TM.EGrouped subExps)
       (TM.EGrouped supExps)
     , baseExp]
oMathElemToExps (Phantom base) =
  [TM.EPhantom $ baseToExp base]
oMathElemToExps (Radical degree base) =
  let degExps = concatMap oMathElemToExps degree
      baseExp = baseToExp base
  in
   case degExps of
     [] -> [TM.ESqrt baseExp]
     ds -> [TM.ERoot (TM.EGrouped ds) baseExp]
oMathElemToExps (PreSubSuper sub sup base) =
  let subExps = concatMap oMathElemToExps sub
      supExps = concatMap oMathElemToExps sup
      baseExp = baseToExp base
  in [ TM.ESubsup
       (TM.EIdentifier "") (TM.EGrouped subExps) (TM.EGrouped supExps)
     , baseExp]
oMathElemToExps (Sub base sub) =
  let baseExp = baseToExp base 
      subExps = concatMap oMathElemToExps sub
  in
   [TM.ESub baseExp (TM.EGrouped subExps)]
oMathElemToExps (SubSuper base sub sup) =
  let baseExp = baseToExp base 
      subExps = concatMap oMathElemToExps sub
      supExps = concatMap oMathElemToExps sup
  in
   [TM.ESubsup baseExp (TM.EGrouped subExps) (TM.EGrouped supExps)]
oMathElemToExps (Super base sup) =
  let baseExp = baseToExp base
      supExps = concatMap oMathElemToExps sup
  in
   [TM.ESuper baseExp (TM.EGrouped supExps)]
oMathElemToExps (OMathRun sty elems)
  | NoStyle <- oMathRunTextStyle sty =
    [TM.EIdentifier $ oMathRunElemsToString elems]
  | Nothing <- oMathRunStyleToTextType sty =
    [TM.EIdentifier $ oMathRunElemsToString elems]
  | Just textType <-  oMathRunStyleToTextType sty =
      if oMathLit sty
      then [TM.EText textType (oMathRunElemsToString elems)]
      else [TM.EStyled textType [TM.EIdentifier $ oMathRunElemsToString elems]]
oMathElemToExps (OMathRun _ _) = []

oMathRunTextStyleToTextType :: OMathRunTextStyle -> Maybe TM.TextType
oMathRunTextStyleToTextType (Normal) = Just $ TM.TextNormal
oMathRunTextStyleToTextType (NoStyle) = Nothing
oMathRunTextStyleToTextType (Styled scr sty) 
  | Just OBold <- sty
  , Just OSansSerif <- scr =
    Just $ TM.TextSansSerifBold
  | Just OBoldItalic <- sty
  , Just OSansSerif <- scr =
    Just $ TM.TextSansSerifBoldItalic
  | Just OBold <- sty
  , Just OScript <- scr =
    Just $ TM.TextBoldScript
  | Just OBold <- sty
  , Just OFraktur <- scr =
    Just $ TM.TextBoldFraktur
  | Just OItalic <- sty
  , Just OSansSerif <- scr =
    Just $ TM.TextSansSerifItalic
  | Just OBold <- sty =
    Just $ TM.TextBold
  | Just OItalic <- sty =
    Just $ TM.TextItalic
  | Just OMonospace <- scr =
    Just $ TM.TextMonospace
  | Just OSansSerif <- scr =
    Just $ TM.TextSansSerif
  | Just ODoubleStruck <- scr =
    Just $ TM.TextDoubleStruck
  | Just OScript <- scr =
    Just $ TM.TextDoubleStruck
  | Just OFraktur <- scr =
    Just $ TM.TextFraktur
  | Just OBoldItalic <- sty =
    Just $ TM.TextBoldItalic
  | otherwise = Nothing


oMathRunStyleToTextType :: OMathRunStyle -> Maybe TM.TextType
oMathRunStyleToTextType mrPr
  | Normal <- oMathRunTextStyle mrPr =
    Just $ TM.TextNormal
  | Styled scr sty  <- oMathRunTextStyle mrPr
    ,Just OBold <- sty
  , Just OSansSerif <- scr =
    Just $ TM.TextSansSerifBold
  | Styled scr sty  <- oMathRunTextStyle mrPr
  , Just OBoldItalic <- sty
  , Just OSansSerif <- scr =
    Just $ TM.TextSansSerifBoldItalic
  | Styled scr sty  <- oMathRunTextStyle mrPr
  , Just OBold <- sty
  , Just OScript <- scr =
    Just $ TM.TextBoldScript
  | Styled scr sty  <- oMathRunTextStyle mrPr
  , Just OBold <- sty
  , Just OFraktur <- scr =
    Just $ TM.TextBoldFraktur
  | Styled scr sty  <- oMathRunTextStyle mrPr
  , Just OItalic <- sty
  , Just OSansSerif <- scr =
    Just $ TM.TextSansSerifItalic
  | Styled _ sty  <- oMathRunTextStyle mrPr
  , Just OBold <- sty =
    Just $ TM.TextBold
  | Styled _ sty  <- oMathRunTextStyle mrPr
  , Just OItalic <- sty =
    Just $ TM.TextItalic
  | Styled scr _  <- oMathRunTextStyle mrPr
  , Just OMonospace <- scr =
    Just $ TM.TextMonospace
  | Styled scr _  <- oMathRunTextStyle mrPr
  , Just OSansSerif <- scr =
    Just $ TM.TextSansSerif
  | Styled scr _  <- oMathRunTextStyle mrPr
  , Just ODoubleStruck <- scr =
    Just $ TM.TextDoubleStruck
  | Styled scr _  <- oMathRunTextStyle mrPr
  , Just OScript <- scr =
    Just $ TM.TextDoubleStruck
  | Styled scr _  <- oMathRunTextStyle mrPr
  , Just OFraktur <- scr =
    Just $ TM.TextFraktur
  | Styled _ sty  <- oMathRunTextStyle mrPr
  , Just OBoldItalic <- sty =
    Just $ TM.TextBoldItalic
  | otherwise = Nothing

baseToExp :: Base -> TM.Exp
baseToExp b = case baseToExp' b of
  (e : []) -> e
  exps     -> TM.EGrouped exps

-- an ungrouped version of baseToExp
baseToExp' :: Base -> [TM.Exp]
baseToExp' (Base mathElems) =
  concatMap oMathElemToExps mathElems

elemToExps :: Element -> Maybe [TM.Exp]
elemToExps element = oMathToExps <$> (elemToMath element)


elemToExps' :: Element -> Maybe [TM.Exp]
elemToExps' element | isElem "m" "acc" element = do
  let chr = filterChildName (hasElemName "m" "accPr") element >>=
            filterChildName (hasElemName "m" "chr") >>=
            findAttrBy (hasElemName "m" "val") >>=
            Just . head
      chr' = case chr of
        Just c -> c
        Nothing -> '\180'       -- default to acute.
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase >>=
             (return . baseToExp)
  return $ [TM.EOver False baseExp (TM.ESymbol TM.Accent [chr'])]
elemToExps' element | isElem "m" "bar" element = do
  pos <- filterChildName (hasElemName "m" "barPr") element >>=
            filterChildName (hasElemName "m" "pos") >>=
            findAttrBy (hasElemName "m" "val") 
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase >>=
             (return . baseToExp)
  case pos of
    "top" -> Just [TM.EOver False baseExp (TM.ESymbol TM.Accent "\175")]
    "bot" -> Just [TM.EUnder False baseExp (TM.ESymbol TM.Accent "\818")]
    _     -> Nothing
elemToExps' element | isElem "m" "box" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase >>=
             (return . baseToExp)
  return [baseExp]
elemToExps' element | isElem "m" "borderBox" element = do
  -- TODO: This needs to be "\\boxed" somehow.
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase >>=
             (return . baseToExp)
  return [baseExp]
elemToExps' element | isElem "m" "d" element = 
  let baseExps  = mapMaybe
               (\e -> (elemToBase e >>= (return . baseToExp)))
               (elChildren element)
      inDelimExps = map Right baseExps
      dPr = filterChildName (hasElemName "m" "dPr") element
      begChr = dPr >>=
               filterChildName (hasElemName "m" "begChr") >>=
               findAttrBy (hasElemName "m" "val") >>=
               (\c -> if null c then (Just ' ') else (Just $ head c))
      sepChr = dPr >>=
               filterChildName (hasElemName "m" "sepChr") >>=
               findAttrBy (hasElemName "m" "val") >>=
               (\c -> if null c then (Just ' ') else (Just $ head c))
      endChr = dPr >>=
               filterChildName (hasElemName "m" "endChr") >>=
               findAttrBy (hasElemName "m" "val") >>=
               (\c -> if null c then (Just ' ') else (Just $ head c))
      beg = fromMaybe '(' begChr
      end = fromMaybe ')' endChr
      sep = fromMaybe '|' sepChr
      exps = intersperse (Left [sep]) inDelimExps
  in
   Just [TM.EDelimited [beg] [end] exps]
elemToExps' element | isElem "m" "eqArr" element =
  let bases = mapMaybe (elemToBaseNoAmpersand) (elChildren element)
      baseExps = map (\b -> [baseToExp' b]) bases
  in
   return [TM.EArray [] baseExps]
elemToExps' element | isElem "m" "f" element = do
  num <- filterChildName (hasElemName "m" "num") element
  den <- filterChildName (hasElemName "m" "den") element
  let numExp = TM.EGrouped $ concat $ mapMaybe (elemToExps') (elChildren num)
      denExp = TM.EGrouped $ concat $ mapMaybe (elemToExps') (elChildren den)
  return $ [TM.EFraction TM.NormalFrac numExp denExp]
elemToExps' element | isElem "m" "func" element = do
  fName <- filterChildName (hasElemName "m" "fName") element
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
          elemToBase >>=
          (return . baseToExp)
  -- We need a string for the fname, but omml gives it to us as a
  -- series of oMath elems. We're going to filter out the oMathRuns,
  -- which should work for us most of the time.
  let fnameString = concatMap expToString $
                    concat $ mapMaybe (elemToExps') (elChildren fName)
  return [TM.EMathOperator fnameString, baseExp]
elemToExps' element | isElem "m" "groupChr" element = do
  let gPr = filterChildName (hasElemName "m" "groupChrPr") element
      chr = gPr >>=
            filterChildName (hasElemName "m" "chr") >>=
            findAttrBy (hasElemName "m" "val")
      pos = gPr >>=
            filterChildName (hasElemName "m" "pos") >>=
            findAttrBy (hasElemName "m" "val")
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase >>=
             (return . baseToExp)
  case pos of
    Just "top" ->
      let chr' = case chr of
            Just (c:_) -> c
            _           -> '\65079'   -- default to overbrace
      in
       return [TM.EOver False baseExp (TM.ESymbol TM.Accent [chr'])]
    Just "bot" ->
      let chr' = case chr of
            Just (c:_) -> c
            _           -> '\65080'   -- default to underbrace
      in
       return [TM.EUnder False baseExp (TM.ESymbol TM.Accent [chr'])]
    _          -> Nothing
elemToExps' element | isElem "m" "limLow" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element
          >>= elemToBase
          >>= (return . baseToExp)
  limExp <- filterChildName (hasElemName "m" "lim") element
            >>= (\e -> Just $ concat $ mapMaybe (elemToExps') (elChildren e))
            >>= (return . TM.EGrouped)
  return [TM.EUnder True limExp baseExp]
elemToExps' element | isElem "m" "limUpp" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element
          >>= elemToBase
          >>= (return . baseToExp)
  limExp <- filterChildName (hasElemName "m" "lim") element
            >>= (\e -> Just $ concat $ mapMaybe (elemToExps') (elChildren e))
            >>= (return . TM.EGrouped)
  return [TM.EOver True limExp baseExp]
elemToExps' element | isElem "m" "m" element = 
  let rows = filterChildrenName (hasElemName "m" "mr") element
      rowExps = map
                (\mr -> mapMaybe
                        (\e -> (elemToBase e >>= return . baseToExp'))
                        (elChildren mr))
                rows
  in
   return [TM.EArray [TM.AlignCenter] rowExps]
elemToExps' element | isElem "m" "nary" element = do
  let naryPr = filterChildName (hasElemName "m" "naryPr") element
      naryChr = naryPr >>=
                filterChildName (hasElemName "m" "chr") >>=
                findAttrBy (hasElemName "m" "val")
      opChr = case naryChr of
        Just (c:_) -> c
        _          -> '\8747'   -- default to integral
      limLoc = naryPr >>=
               filterChildName (hasElemName "m" "limLoc") >>=
               findAttrBy (hasElemName "m" "val")
  subExps <- filterChildName (hasElemName "m" "sub") element >>=
         (\e -> return $ concat $ mapMaybe (elemToExps') (elChildren e))
  supExps <- filterChildName (hasElemName "m" "sup") element >>=
         (\e -> return $ concat $ mapMaybe (elemToExps') (elChildren e))
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase >>=
             (return . baseToExp)
  case limLoc of
    Just "undOvr" -> return [TM.EUnderover True
                              (TM.ESymbol TM.Op [opChr])
                              (TM.EGrouped subExps)
                              (TM.EGrouped supExps)
                            , baseExp]
    _             -> return [TM.ESubsup
                              (TM.ESymbol TM.Op [opChr])
                              (TM.EGrouped subExps)
                              (TM.EGrouped supExps)
                            , baseExp]

elemToExps' element | isElem "m" "phant" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase >>=
             (return . baseToExp)
  return [TM.EPhantom baseExp]
elemToExps' element | isElem "m" "rad" element = do
  degExps <- filterChildName (hasElemName "m" "deg") element >>=
              (\e -> return $ concat $ mapMaybe (elemToExps') (elChildren e))
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase >>=
             (return . baseToExp)
  return $ case degExps of
    [] -> [TM.ESqrt baseExp]
    ds -> [TM.ERoot (TM.EGrouped ds) baseExp]
elemToExps' element | isElem "m" "sPre" element = do
  subExps <- filterChildName (hasElemName "m" "sub") element >>=
            (\e -> return $ concat $ mapMaybe (elemToExps') (elChildren e))
  supExps <- filterChildName (hasElemName "m" "sup") element >>=
            (\e -> return $ concat $ mapMaybe (elemToExps') (elChildren e))
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase >>=
             (return . baseToExp)
  return [TM.ESubsup
          (TM.EIdentifier "")
          (TM.EGrouped subExps)
          (TM.EGrouped supExps)
         , baseExp]
elemToExps' element | isElem "m" "sSub" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase >>=
             (return . baseToExp)
  subExps <- filterChildName (hasElemName "m" "sub") element >>=
            (\e -> return $ concat $ mapMaybe (elemToExps') (elChildren e))
  return [TM.ESub baseExp (TM.EGrouped subExps)]
elemToExps' element | isElem "m" "sSubSup" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase >>=
             (return . baseToExp)
  subExps <- filterChildName (hasElemName "m" "sub") element >>=
             (\e -> return $ concat $ mapMaybe (elemToExps') (elChildren e))
  supExps <- filterChildName (hasElemName "m" "sup") element >>=
             (\e -> return $ concat $ mapMaybe (elemToExps') (elChildren e))
  return [TM.ESubsup baseExp (TM.EGrouped subExps) (TM.EGrouped supExps)]
elemToExps' element | isElem "m" "sSup" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase >>=
             (return . baseToExp)
  supExps <- filterChildName (hasElemName "m" "sup") element >>=
            (\e -> return $ concat $ mapMaybe (elemToExps') (elChildren e))
  return [TM.ESuper baseExp (TM.EGrouped supExps)]
elemToExps' element | isElem "m" "r" element = do
  let mrPr = filterChildName (hasElemName "m" "rPr") element
      lit = mrPr >>=
            filterChildName (hasElemName "m" "lit") >>=
            findAttrBy (hasElemName "m" "val")
      txtSty = elemToOMathRunTextStyle element
  mrElems <- elemToOMathRunElems element
  return $ case oMathRunTextStyleToTextType txtSty of
    Nothing -> [TM.EIdentifier $ oMathRunElemsToString mrElems]
    Just textType ->
      case lit of
        Just "on" ->
          [TM.EText textType (oMathRunElemsToString mrElems)]
        _         ->
          [TM.EStyled textType [TM.EIdentifier $ oMathRunElemsToString mrElems]]
elemToExps' _ = Nothing

expToString :: TM.Exp -> String
expToString (TM.ENumber s) = s
expToString (TM.EIdentifier s) = s
expToString (TM.EMathOperator s) = s
expToString (TM.ESymbol _ s) = s
expToString (TM.EText _ s) = s
expToString (TM.EGrouped exps) = concatMap expToString exps
expToString (TM.EStyled _ exps) = concatMap expToString exps
expToString _ = ""
