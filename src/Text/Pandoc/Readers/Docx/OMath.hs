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

module Text.Pandoc.Readers.Docx.OMath  (readOMML
                                       ) where

import Text.XML.Light
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (intersperse)
import qualified Text.TeXMath.Types as TM

readOMML :: Element -> Maybe [TM.Exp]
readOMML element  | isElem "m" "oMath" element =
  Just $ concat $ mapMaybe (elemToExps') (elChildren element)
readOMML _ = Nothing


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

data OMathRunElem = TextRun String
                  | LnBrk
                  | Tab
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

elemToBase :: Element -> Maybe TM.Exp
elemToBase element | isElem "m" "e" element = do
  bs <- elemToBases element
  return $ case bs of
    (e : []) -> e
    exps     -> TM.EGrouped exps
elemToBase _ = Nothing

elemToBases :: Element -> Maybe [TM.Exp]
elemToBases element | isElem "m" "e" element =
  return $ concat $ mapMaybe elemToExps' (elChildren element)
elemToBases _ = Nothing


-- TODO: The right way to do this is to use the ampersand to break the
-- text lines into multiple columns. That's tricky, though, and this
-- will get us most of the way for the time being.
filterAmpersand :: TM.Exp -> TM.Exp
filterAmpersand (TM.EIdentifier s)   = TM.EIdentifier (filter ('&' /=) s)
filterAmpersand (TM.EText tt s)      = TM.EText tt (filter ('&' /=) s)
filterAmpersand (TM.EStyled tt exps) = TM.EStyled tt (map filterAmpersand exps)
filterAmpersand (TM.EGrouped exps)   = TM.EGrouped (map filterAmpersand exps)
filterAmpersand e                    = e

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
             elemToBase
  return $ [TM.EOver False baseExp (TM.ESymbol TM.Accent [chr'])]
elemToExps' element | isElem "m" "bar" element = do
  pos <- filterChildName (hasElemName "m" "barPr") element >>=
            filterChildName (hasElemName "m" "pos") >>=
            findAttrBy (hasElemName "m" "val")
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  case pos of
    "top" -> Just [TM.EOver False baseExp (TM.ESymbol TM.Accent "\175")]
    "bot" -> Just [TM.EUnder False baseExp (TM.ESymbol TM.Accent "\818")]
    _     -> Nothing
elemToExps' element | isElem "m" "box" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  return [baseExp]
elemToExps' element | isElem "m" "borderBox" element = do
  -- TODO: This needs to be "\\boxed" somehow.
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  return [baseExp]
elemToExps' element | isElem "m" "d" element =
  let baseExps  = mapMaybe
                  elemToBase
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
  let expLst = mapMaybe elemToBases (elChildren element)
      expLst' = map (\es -> [map filterAmpersand es]) expLst
  in
   return [TM.EArray [] expLst']
elemToExps' element | isElem "m" "f" element = do
  num <- filterChildName (hasElemName "m" "num") element
  den <- filterChildName (hasElemName "m" "den") element
  let numExp = TM.EGrouped $ concat $ mapMaybe (elemToExps') (elChildren num)
      denExp = TM.EGrouped $ concat $ mapMaybe (elemToExps') (elChildren den)
  return $ [TM.EFraction TM.NormalFrac numExp denExp]
elemToExps' element | isElem "m" "func" element = do
  fName <- filterChildName (hasElemName "m" "fName") element
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
          elemToBase
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
             elemToBase
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
  limExp <- filterChildName (hasElemName "m" "lim") element
            >>= (\e -> Just $ concat $ mapMaybe (elemToExps') (elChildren e))
            >>= (return . TM.EGrouped)
  return [TM.EUnder True limExp baseExp]
elemToExps' element | isElem "m" "limUpp" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element
          >>= elemToBase
  limExp <- filterChildName (hasElemName "m" "lim") element
            >>= (\e -> Just $ concat $ mapMaybe (elemToExps') (elChildren e))
            >>= (return . TM.EGrouped)
  return [TM.EOver True limExp baseExp]
elemToExps' element | isElem "m" "m" element =
  let rows = filterChildrenName (hasElemName "m" "mr") element
      rowExps = map
                (\mr -> mapMaybe
                        elemToBases
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
             elemToBase
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
             elemToBase
  return [TM.EPhantom baseExp]
elemToExps' element | isElem "m" "rad" element = do
  degExps <- filterChildName (hasElemName "m" "deg") element >>=
              (\e -> return $ concat $ mapMaybe (elemToExps') (elChildren e))
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  return $ case degExps of
    [] -> [TM.ESqrt baseExp]
    ds -> [TM.ERoot (TM.EGrouped ds) baseExp]
elemToExps' element | isElem "m" "sPre" element = do
  subExps <- filterChildName (hasElemName "m" "sub") element >>=
            (\e -> return $ concat $ mapMaybe (elemToExps') (elChildren e))
  supExps <- filterChildName (hasElemName "m" "sup") element >>=
            (\e -> return $ concat $ mapMaybe (elemToExps') (elChildren e))
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  return [TM.ESubsup
          (TM.EIdentifier "")
          (TM.EGrouped subExps)
          (TM.EGrouped supExps)
         , baseExp]
elemToExps' element | isElem "m" "sSub" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  subExps <- filterChildName (hasElemName "m" "sub") element >>=
            (\e -> return $ concat $ mapMaybe (elemToExps') (elChildren e))
  return [TM.ESub baseExp (TM.EGrouped subExps)]
elemToExps' element | isElem "m" "sSubSup" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
  subExps <- filterChildName (hasElemName "m" "sub") element >>=
             (\e -> return $ concat $ mapMaybe (elemToExps') (elChildren e))
  supExps <- filterChildName (hasElemName "m" "sup") element >>=
             (\e -> return $ concat $ mapMaybe (elemToExps') (elChildren e))
  return [TM.ESubsup baseExp (TM.EGrouped subExps) (TM.EGrouped supExps)]
elemToExps' element | isElem "m" "sSup" element = do
  baseExp <- filterChildName (hasElemName "m" "e") element >>=
             elemToBase
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
