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

type NameSpaces = [(String, String)]

elemName :: NameSpaces -> String -> String -> QName
elemName ns prefix name = (QName name (lookup prefix ns) (Just prefix))

isElem :: NameSpaces -> String -> String -> Element -> Bool
isElem ns prefix name element =
  qName (elName element) == name &&
  qURI (elName element) == (lookup prefix ns)


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

elemToMath :: NameSpaces -> Element -> Maybe OMath
elemToMath ns element  | isElem ns "m" "oMath" element =
  Just $ OMath $ mapMaybe (elemToMathElem ns) (elChildren element)
elemToMath _ _ = Nothing

elemToBase :: NameSpaces -> Element -> Maybe Base
elemToBase ns element | isElem ns "m" "e" element =
  Just $ Base $ mapMaybe (elemToMathElem ns) (elChildren element)
elemToBase _ _ = Nothing

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

elemToBaseNoAmpersand :: NameSpaces -> Element -> Maybe Base
elemToBaseNoAmpersand ns element | isElem ns "m" "e" element =
  return $ Base $ 
  mapMaybe
  (\e -> (elemToMathElem ns e >>= (return . filterAmpersand)))
  (elChildren element)
elemToBaseNoAmpersand _ _ = Nothing

elemToOMathRunStyle :: NameSpaces -> Element -> OMathRunStyle
elemToOMathRunStyle ns element =
  let lit =
        case
          findChild (elemName ns "m" "lit") element >>=
          findAttr (elemName ns "m" "val")
        of
          Just "on" -> True
          _         -> False
  in
   OMathRunStyle { oMathLit = lit
                 , oMathRunTextStyle = (elemToOMathRunTextStyle ns element)
                 }

elemToOMathRunTextStyle :: NameSpaces -> Element -> OMathRunTextStyle
elemToOMathRunTextStyle ns element
  | Just mrPr <- findChild (elemName ns "m" "rPr") element
  , Just _    <- findChild (elemName ns "m" "nor") mrPr =
    Normal
  | Just mrPr <- findChild (elemName ns "m" "rPr") element =
    let scr =
          case
            findChild (elemName ns "m" "scr") mrPr >>=
            findAttr (elemName ns "m" "val") 
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
            findChild (elemName ns "m" "sty") mrPr >>=
            findAttr (elemName ns "m" "val")
          of
            Just "p"             -> Just OPlain
            Just "b"             -> Just OBold
            Just "i"             -> Just OItalic
            Just "bi"            -> Just OBoldItalic
            _                    -> Nothing
    in
     Styled { oMathScript = scr, oMathStyle = sty }
  | otherwise = NoStyle



elemToNAryStyle :: NameSpaces -> Element -> NAryStyle
elemToNAryStyle ns element
  | Just narypr <- findChild (QName "naryPr" (lookup "m" ns) (Just "m")) element =
  let
    chr = findChild (QName "chr" (lookup "m" ns) (Just "m")) narypr >>=
          findAttr (QName "val" (lookup "m" ns) (Just "m")) >>=
          Just . head
    limLoc = findChild (QName "limLoc" (lookup "m" ns) (Just "m")) narypr >>=
             findAttr (QName "val" (lookup "m" ns) (Just "m"))
    limLoc' = case limLoc of
      Just "undOver" -> UnderOver
      Just "subSup"  -> SubSup
      _              -> SubSup
  in
   NAryStyle { nAryChar = chr, nAryLimLoc = limLoc'}
elemToNAryStyle _ _ = defaultNAryStyle

elemToDelimStyle :: NameSpaces -> Element -> DelimStyle
elemToDelimStyle ns element
  | Just dPr <- findChild (QName "dPr" (lookup "m" ns) (Just "m")) element =
    let begChr = findChild (QName "begChr" (lookup "m" ns) (Just "m")) dPr >>=
                 findAttr (QName "val" (lookup "m" ns) (Just "m")) >>=
                 (\c -> if null c then (Just ' ') else (Just $ head c))
        sepChr = findChild (QName "sepChr" (lookup "m" ns) (Just "m")) dPr >>=
                 findAttr (QName "val" (lookup "m" ns) (Just "m")) >>=
                 (\c -> if null c then (Just ' ') else (Just $ head c))
        endChr = findChild (QName "endChr" (lookup "m" ns) (Just "m")) dPr >>=
                 findAttr (QName "val" (lookup "m" ns) (Just "m")) >>=
                 (\c -> if null c then (Just ' ') else (Just $ head c))
    in
     DelimStyle { delimBegChar = begChr
                , delimSepChar = sepChr
                , delimEndChar = endChr}
elemToDelimStyle _ _ = defaultDelimStyle

elemToGroupStyle :: NameSpaces -> Element -> GroupStyle
elemToGroupStyle ns element
  | Just gPr <- findChild (QName "groupChrPr" (lookup "m" ns) (Just "m")) element =
    let chr = findChild (QName "chr" (lookup "m" ns) (Just "m")) gPr >>=
              findAttr (QName "val" (lookup "m" ns) (Just "m")) >>=
              Just . head
        pos = findChild (QName "pos" (lookup "m" ns) (Just "m")) gPr >>=
              findAttr (QName "val" (lookup "m" ns) (Just "m")) >>=
              (\s -> Just $ if s == "top" then Top else Bottom)
    in
     GroupStyle { groupChr = chr, groupPos = pos }
elemToGroupStyle _ _ = defaultGroupStyle

elemToMathElem :: NameSpaces -> Element -> Maybe OMathElem
elemToMathElem ns element | isElem ns "m" "acc" element = do
  let accChar =
        findChild (elemName ns "m" "accPr") element >>=
        findChild (elemName ns "m" "chr") >>=
        findAttr (elemName ns "m" "val") >>=
        Just . head
      accPr = AccentStyle { accentChar = accChar}
  base <- findChild (elemName ns "m" "e") element >>=
         elemToBase ns
  return $ Accent accPr base
elemToMathElem ns element | isElem ns "m" "bar" element = do
  barPr <- findChild (QName "barPr" (lookup "m" ns) (Just "m")) element >>=
           findChild (QName "pos" (lookup "m" ns) (Just "m")) >>=
           findAttr (QName "val" (lookup "m" ns) (Just "m")) >>=
           (\s ->
             Just $ BarStyle {
               barPos = (if s == "bot" then Bottom else Top)
               })
  base <- findChild (QName "e" (lookup "m" ns) (Just "m")) element >>=
          elemToBase ns
  return $ Bar barPr base
elemToMathElem ns element | isElem ns "m" "box" element =
  findChild (elemName ns "m" "e") element >>=
  elemToBase ns >>=
  (\b -> return $ Box b)
elemToMathElem ns element | isElem ns "m" "borderBox" element =
  findChild (elemName ns "m" "e") element >>=
  elemToBase ns >>=
  (\b -> return $ BorderBox b)
elemToMathElem ns element | isElem ns "m" "d" element =
  let style = elemToDelimStyle ns element
  in
   return $ Delimiter style $ mapMaybe (elemToBase ns) (elChildren element)
elemToMathElem ns element | isElem ns "m" "eqArr" element =
  return $ EquationArray $ mapMaybe (elemToBaseNoAmpersand ns) (elChildren element) 
elemToMathElem ns element | isElem ns "m" "f" element = do
  num <- findChild (elemName ns "m" "num") element
  den <- findChild (elemName ns "m" "den") element
  let numElems = mapMaybe (elemToMathElem ns) (elChildren num)
      denElems = mapMaybe (elemToMathElem ns) (elChildren den)
  return $ Fraction numElems denElems
elemToMathElem ns element | isElem ns "m" "func" element = do
  fName <- findChild (elemName ns "m" "fName") element
  base <- findChild (elemName ns "m" "e") element >>=
          elemToBase ns
  let fnElems = mapMaybe (elemToMathElem ns) (elChildren fName)
  return $ Function fnElems base
elemToMathElem ns element | isElem ns "m" "groupChr" element =
  let style = elemToGroupStyle ns element
  in
   findChild (elemName ns "m" "e") element >>=
   elemToBase ns >>=
   (\b -> return $ Group style b)
elemToMathElem ns element | isElem ns "m" "limLow" element = do
  base <- findChild (elemName ns "m" "e") element
          >>= elemToBase ns
  lim <- findChild (elemName ns "m" "lim") element
  let limElems = mapMaybe (elemToMathElem ns) (elChildren lim)
  return $ LowerLimit base limElems
elemToMathElem ns element | isElem ns "m" "limUpp" element = do
  base <- findChild (elemName ns "m" "e") element
          >>= elemToBase ns
  lim <- findChild (elemName ns "m" "lim") element
  let limElems = mapMaybe (elemToMathElem ns) (elChildren lim)
  return $ UpperLimit base limElems
elemToMathElem ns element | isElem ns "m" "m" element = do
  let rows = findChildren (elemName ns "m" "mr") element
  let bases = mapMaybe (\mr -> mapM (elemToBase ns) (elChildren mr)) rows
  return $ Matrix bases
elemToMathElem ns element | isElem ns "m" "nary" element = do
  let style = elemToNAryStyle ns element
  sub <- findChild (elemName ns "m" "sub") element >>=
         (\e -> return $ mapMaybe (elemToMathElem ns) (elChildren e))
  sup <- findChild (elemName ns "m" "sup") element >>=
         (\e -> return $ mapMaybe (elemToMathElem ns) (elChildren e))
  base <- findChild (elemName ns "m" "e") element >>=
          elemToBase ns
  return $ NAry style sub sup base
elemToMathElem ns element | isElem ns "m" "rad" element = do
  deg <- findChild (elemName ns "m" "deg") element >>=
         (\e -> return $ mapMaybe (elemToMathElem ns) (elChildren e))
  base <- findChild (elemName ns "m" "e") element >>=
          elemToBase ns
  return $ Radical deg base
elemToMathElem ns element | isElem ns "m" "phant" element = do
  base <- findChild (elemName ns "m" "e") element >>=
          elemToBase ns
  return $ Phantom base
elemToMathElem ns element | isElem ns "m" "sPre" element = do
  sub <- findChild (elemName ns "m" "sub") element >>=
         (\e -> return $ mapMaybe (elemToMathElem ns) (elChildren e))
  sup <- findChild (elemName ns "m" "sup") element >>=
         (\e -> return $ mapMaybe (elemToMathElem ns) (elChildren e))
  base <- findChild (elemName ns "m" "e") element >>=
          elemToBase ns
  return $ PreSubSuper sub sup base
elemToMathElem ns element | isElem ns "m" "sSub" element = do
  base <- findChild (elemName ns "m" "e") element >>=
          elemToBase ns
  sub <- findChild (elemName ns "m" "sub") element >>=
         (\e -> return $ mapMaybe (elemToMathElem ns) (elChildren e))
  return $ Sub base sub
elemToMathElem ns element | isElem ns "m" "sSubSup" element = do
  base <- findChild (elemName ns "m" "e") element >>=
          elemToBase ns
  sub <- findChild (elemName ns "m" "sub") element >>=
         (\e -> return $ mapMaybe (elemToMathElem ns) (elChildren e))
  sup <- findChild (elemName ns "m" "sup") element >>=
         (\e -> return $ mapMaybe (elemToMathElem ns) (elChildren e))
  return $ SubSuper base sub sup
elemToMathElem ns element | isElem ns "m" "sSup" element = do
  base <- findChild (elemName ns "m" "e") element >>=
          elemToBase ns
  sup <- findChild (elemName ns "m" "sup") element >>=
         (\e -> return $ mapMaybe (elemToMathElem ns) (elChildren e))
  return $ Super base sup
elemToMathElem ns element | isElem ns "m" "r" element = do
  let mrPr = elemToOMathRunStyle ns element
  mrElems <- elemToOMathRunElems ns element
  return $ OMathRun mrPr mrElems
elemToMathElem _ _ = Nothing

elemToOMathRunElem :: NameSpaces -> Element -> Maybe OMathRunElem
elemToOMathRunElem ns element
  | isElem ns "w" "t" element
    || isElem ns "m" "t" element 
    || isElem ns "w" "delText" element = Just $ TextRun $ strContent element
  | isElem ns "w" "br" element = Just LnBrk
  | isElem ns "w" "tab" element = Just Tab
  | otherwise = Nothing

elemToOMathRunElems :: NameSpaces -> Element -> Maybe [OMathRunElem]
elemToOMathRunElems ns element
  | isElem ns "w" "r" element
    || isElem ns "m" "r" element =
      Just $ mapMaybe (elemToOMathRunElem ns) (elChildren element)
elemToOMathRunElems _ _ = Nothing

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

elemToExps :: NameSpaces -> Element -> Maybe [TM.Exp]
elemToExps ns element = oMathToExps <$> (elemToMath ns element)


elemToExps' :: NameSpaces -> Element -> Maybe [TM.Exp]
elemToExps' ns element | isElem ns "m" "acc" element = do
  let chr = findChild (elemName ns "m" "accPr") element >>=
            findChild (elemName ns "m" "chr") >>=
            findAttr (elemName ns "m" "val") >>=
            Just . head
      chr' = case chr of
        Just c -> c
        Nothing -> '\180'       -- default to acute.
  baseExp <- findChild (elemName ns "m" "e") element >>=
             elemToBase ns >>=
             (return . baseToExp)
  return $ [TM.EOver False baseExp (TM.ESymbol TM.Accent [chr'])]
elemToExps' ns element | isElem ns "m" "bar" element = do
  pos <- findChild (elemName ns "m" "barPr") element >>=
            findChild (elemName ns "m" "pos") >>=
            findAttr (elemName ns "m" "val") 
  baseExp <- findChild (elemName ns "m" "e") element >>=
             elemToBase ns >>=
             (return . baseToExp)
  case pos of
    "top" -> Just [TM.EOver False baseExp (TM.ESymbol TM.Accent "\175")]
    "bot" -> Just [TM.EUnder False baseExp (TM.ESymbol TM.Accent "\818")]
    _     -> Nothing
elemToExps' ns element | isElem ns "m" "box" element = do
  baseExp <- findChild (elemName ns "m" "e") element >>=
             elemToBase ns >>=
             (return . baseToExp)
  return [baseExp]
elemToExps' ns element | isElem ns "m" "borderBox" element = do
  -- TODO: This needs to be "\\boxed" somehow.
  baseExp <- findChild (elemName ns "m" "e") element >>=
             elemToBase ns >>=
             (return . baseToExp)
  return [baseExp]
elemToExps' ns element | isElem ns "m" "d" element = 
  let baseExps  = mapMaybe
               (\e -> (elemToBase ns e >>= (return . baseToExp)))
               (elChildren element)
      inDelimExps = map Right baseExps
      dPr = findChild (elemName ns "m" "dPr") element
      begChr = dPr >>=
               findChild (elemName ns "m" "begChr") >>=
               findAttr (elemName ns "m" "val") >>=
               (\c -> if null c then (Just ' ') else (Just $ head c))
      sepChr = dPr >>=
               findChild (elemName ns "m" "sepChr") >>=
               findAttr (elemName ns "m" "val") >>=
               (\c -> if null c then (Just ' ') else (Just $ head c))
      endChr = dPr >>=
               findChild (elemName ns "m" "endChr") >>=
               findAttr (elemName ns "m" "val") >>=
               (\c -> if null c then (Just ' ') else (Just $ head c))
      beg = fromMaybe '(' begChr
      end = fromMaybe ')' endChr
      sep = fromMaybe '|' sepChr
      exps = intersperse (Left [sep]) inDelimExps
  in
   Just [TM.EDelimited [beg] [end] exps]
elemToExps' ns element | isElem ns "m" "eqArr" element =
  let bases = mapMaybe (elemToBaseNoAmpersand ns) (elChildren element)
      baseExps = map (\b -> [baseToExp' b]) bases
  in
   return [TM.EArray [] baseExps]
elemToExps' ns element | isElem ns "m" "f" element = do
  num <- findChild (elemName ns "m" "num") element
  den <- findChild (elemName ns "m" "den") element
  let numExp = TM.EGrouped $ concat $ mapMaybe (elemToExps' ns) (elChildren num)
      denExp = TM.EGrouped $ concat $ mapMaybe (elemToExps' ns) (elChildren den)
  return $ [TM.EFraction TM.NormalFrac numExp denExp]
elemToExps' ns element | isElem ns "m" "func" element = do
  fName <- findChild (elemName ns "m" "fName") element
  baseExp <- findChild (elemName ns "m" "e") element >>=
          elemToBase ns >>=
          (return . baseToExp)
  -- We need a string for the fname, but omml gives it to us as a
  -- series of oMath elems. We're going to filter out the oMathRuns,
  -- which should work for us most of the time.
  let fnameString = concatMap expToString $
                    concat $ mapMaybe (elemToExps' ns) (elChildren fName)
  return [TM.EMathOperator fnameString, baseExp]
elemToExps' ns element | isElem ns "m" "groupChr" element = do
  let gPr = findChild (elemName ns "m" "groupChrPr") element
      chr = gPr >>=
            findChild (elemName ns "m" "chr") >>=
            findAttr (elemName ns "m" "val")
      pos = gPr >>=
            findChild (elemName ns "m" "pos") >>=
            findAttr (elemName ns "m" "val")
  baseExp <- findChild (elemName ns "m" "e") element >>=
             elemToBase ns >>=
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
elemToExps' ns element | isElem ns "m" "limLow" element = do
  baseExp <- findChild (elemName ns "m" "e") element
          >>= elemToBase ns
          >>= (return . baseToExp)
  limExp <- findChild (elemName ns "m" "lim") element
            >>= (\e -> Just $ concat $ mapMaybe (elemToExps' ns) (elChildren e))
            >>= (return . TM.EGrouped)
  return [TM.EUnder True limExp baseExp]
elemToExps' ns element | isElem ns "m" "limUpp" element = do
  baseExp <- findChild (elemName ns "m" "e") element
          >>= elemToBase ns
          >>= (return . baseToExp)
  limExp <- findChild (elemName ns "m" "lim") element
            >>= (\e -> Just $ concat $ mapMaybe (elemToExps' ns) (elChildren e))
            >>= (return . TM.EGrouped)
  return [TM.EOver True limExp baseExp]
elemToExps' ns element | isElem ns "m" "m" element = 
  let rows = findChildren (elemName ns "m" "mr") element
      rowExps = map
                (\mr -> mapMaybe
                        (\e -> (elemToBase ns e >>= return . baseToExp'))
                        (elChildren mr))
                rows
  in
   return [TM.EArray [TM.AlignCenter] rowExps]
elemToExps' ns element | isElem ns "m" "nary" element = do
  let naryPr = findChild (elemName ns "m" "naryPr") element
      naryChr = naryPr >>=
                findChild (elemName ns "m" "chr") >>=
                findAttr (elemName ns "m" "val")
      opChr = case naryChr of
        Just (c:_) -> c
        _          -> '\8747'   -- default to integral
      limLoc = naryPr >>=
               findChild (elemName ns "m" "limLoc") >>=
               findAttr (elemName ns "m" "val")
  subExps <- findChild (elemName ns "m" "sub") element >>=
         (\e -> return $ concat $ mapMaybe (elemToExps' ns) (elChildren e))
  supExps <- findChild (elemName ns "m" "sup") element >>=
         (\e -> return $ concat $ mapMaybe (elemToExps' ns) (elChildren e))
  baseExp <- findChild (elemName ns "m" "e") element >>=
             elemToBase ns >>=
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

elemToExps' ns element | isElem ns "m" "phant" element = do
  baseExp <- findChild (elemName ns "m" "e") element >>=
             elemToBase ns >>=
             (return . baseToExp)
  return [TM.EPhantom baseExp]
elemToExps' ns element | isElem ns "m" "rad" element = do
  degExps <- findChild (elemName ns "m" "deg") element >>=
              (\e -> return $ concat $ mapMaybe (elemToExps' ns) (elChildren e))
  baseExp <- findChild (elemName ns "m" "e") element >>=
             elemToBase ns >>=
             (return . baseToExp)
  return $ case degExps of
    [] -> [TM.ESqrt baseExp]
    ds -> [TM.ERoot (TM.EGrouped ds) baseExp]
elemToExps' ns element | isElem ns "m" "sPre" element = do
  subExps <- findChild (elemName ns "m" "sub") element >>=
            (\e -> return $ concat $ mapMaybe (elemToExps' ns) (elChildren e))
  supExps <- findChild (elemName ns "m" "sup") element >>=
            (\e -> return $ concat $ mapMaybe (elemToExps' ns) (elChildren e))
  baseExp <- findChild (elemName ns "m" "e") element >>=
             elemToBase ns >>=
             (return . baseToExp)
  return [TM.ESubsup
          (TM.EIdentifier "")
          (TM.EGrouped subExps)
          (TM.EGrouped supExps)
         , baseExp]
elemToExps' ns element | isElem ns "m" "sSub" element = do
  baseExp <- findChild (elemName ns "m" "e") element >>=
             elemToBase ns >>=
             (return . baseToExp)
  subExps <- findChild (elemName ns "m" "sub") element >>=
            (\e -> return $ concat $ mapMaybe (elemToExps' ns) (elChildren e))
  return [TM.ESub baseExp (TM.EGrouped subExps)]
elemToExps' ns element | isElem ns "m" "sSubSup" element = do
  baseExp <- findChild (elemName ns "m" "e") element >>=
             elemToBase ns >>=
             (return . baseToExp)
  subExps <- findChild (elemName ns "m" "sub") element >>=
             (\e -> return $ concat $ mapMaybe (elemToExps' ns) (elChildren e))
  supExps <- findChild (elemName ns "m" "sup") element >>=
             (\e -> return $ concat $ mapMaybe (elemToExps' ns) (elChildren e))
  return [TM.ESubsup baseExp (TM.EGrouped subExps) (TM.EGrouped supExps)]
elemToExps' ns element | isElem ns "m" "sSup" element = do
  baseExp <- findChild (elemName ns "m" "e") element >>=
             elemToBase ns >>=
             (return . baseToExp)
  supExps <- findChild (elemName ns "m" "sup") element >>=
            (\e -> return $ concat $ mapMaybe (elemToExps' ns) (elChildren e))
  return [TM.ESuper baseExp (TM.EGrouped supExps)]
elemToExps' ns element | isElem ns "m" "r" element = do
  let mrPr = findChild (elemName ns "m" "rPr") element
      lit = mrPr >>=
            findChild (elemName ns "m" "lit") >>=
            findAttr (elemName ns "m" "val")
      txtSty = elemToOMathRunTextStyle ns element
  mrElems <- elemToOMathRunElems ns element
  return $ case oMathRunTextStyleToTextType txtSty of
    Nothing -> [TM.EIdentifier $ oMathRunElemsToString mrElems]
    Just textType ->
      case lit of
        Just "on" ->
          [TM.EText textType (oMathRunElemsToString mrElems)]
        _         ->
          [TM.EStyled textType [TM.EIdentifier $ oMathRunElemsToString mrElems]]
elemToExps' _ _ = Nothing

expToString :: TM.Exp -> String
expToString (TM.ENumber s) = s
expToString (TM.EIdentifier s) = s
expToString (TM.EMathOperator s) = s
expToString (TM.ESymbol _ s) = s
expToString (TM.EText _ s) = s
expToString (TM.EGrouped exps) = concatMap expToString exps
expToString (TM.EStyled _ exps) = concatMap expToString exps
expToString _ = ""
