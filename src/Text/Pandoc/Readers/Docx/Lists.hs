{-
Copyright (C) 2014 Jesse Rosenthal <jrosenthal@jhu.edu>

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
   Module      : Text.Pandoc.Readers.Docx.Lists
   Copyright   : Copyright (C) 2014 Jesse Rosenthal
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Functions for converting flat docx paragraphs into nested lists.
-}

module Text.Pandoc.Readers.Docx.Lists ( blocksToBullets
                                      , blocksToDefinitions
                                      , listParagraphDivs
                                      ) where

import Text.Pandoc.JSON
import Text.Pandoc.Generic (bottomUp)
import Text.Pandoc.Shared (trim)
import Control.Monad
import Data.List
import Data.Maybe

isListItem :: Block -> Bool
isListItem (Div (_, classes, _) _) | "list-item" `elem` classes = True
isListItem _ = False

getLevel :: Block -> Maybe Integer
getLevel (Div (_, _, kvs) _) =  liftM read $ lookup "level" kvs
getLevel _ = Nothing

getLevelN :: Block -> Integer
getLevelN b = case getLevel b of
  Just n -> n
  Nothing -> -1

getNumId :: Block -> Maybe Integer
getNumId (Div (_, _, kvs) _) =  liftM read $ lookup "num-id" kvs
getNumId _ = Nothing

getNumIdN :: Block -> Integer
getNumIdN b = case getNumId b of
  Just n -> n
  Nothing -> -1

getText :: Block -> Maybe String
getText (Div (_, _, kvs) _) = lookup "text" kvs
getText _ = Nothing

data ListType = Itemized | Enumerated ListAttributes

listStyleMap :: [(String, ListNumberStyle)]
listStyleMap = [("upperLetter", UpperAlpha),
                ("lowerLetter", LowerAlpha),
                ("upperRoman", UpperRoman),
                ("lowerRoman", LowerRoman),
                ("decimal", Decimal)]

listDelimMap :: [(String, ListNumberDelim)]
listDelimMap = [("%1)", OneParen),
                ("(%1)", TwoParens),
                ("%1.", Period)]

getListType :: Block -> Maybe ListType
getListType b@(Div (_, _, kvs) _) | isListItem b =
  let
    start = lookup "start" kvs
    frmt = lookup "format" kvs
    txt  = lookup "text" kvs
  in
   case frmt of
     Just "bullet" -> Just Itemized
     Just f        ->
       case txt of
         Just t -> Just $ Enumerated (
                  read (fromMaybe "1" start) :: Int,
                  fromMaybe DefaultStyle (lookup f listStyleMap),
                  fromMaybe DefaultDelim (lookup t listDelimMap))
         Nothing -> Nothing
     _ -> Nothing
getListType _ = Nothing

listParagraphDivs :: [String]
listParagraphDivs = ["ListParagraph"]

-- This is a first stab at going through and attaching meaning to list
-- paragraphs, without an item marker, following a list item. We
-- assume that these are paragraphs in the same item.

handleListParagraphs :: [Block] -> [Block]
handleListParagraphs [] = []
handleListParagraphs (
  (Div attr1@(_, classes1, _) blks1) :
  (Div (ident2, classes2, kvs2) blks2) :
  blks
  ) | "list-item" `elem` classes1 &&
    not ("list-item" `elem` classes2) &&
    (not . null) (listParagraphDivs `intersect` classes2) =
      -- We don't want to keep this indent.
      let newDiv2 =
            (Div (ident2, classes2, filter (\kv -> fst kv /= "indent") kvs2) blks2)
      in
       handleListParagraphs ((Div attr1 (blks1 ++ [newDiv2])) : blks)
handleListParagraphs (blk:blks) = blk : (handleListParagraphs blks)

separateBlocks' :: Block -> [[Block]] -> [[Block]]
separateBlocks' blk ([] : []) = [[blk]]
separateBlocks' b@(BulletList _) acc = (init acc) ++ [(last acc) ++ [b]]
separateBlocks' b@(OrderedList _ _) acc = (init acc) ++ [(last acc) ++ [b]]
-- The following is for the invisible bullet lists. This is how
-- pandoc-generated ooxml does multiparagraph item lists.
separateBlocks' b acc | liftM trim (getText b) == Just "" =
  (init acc) ++ [(last acc) ++ [b]]
separateBlocks' b acc = acc ++ [[b]]

separateBlocks :: [Block] -> [[Block]]
separateBlocks blks = foldr separateBlocks' [[]] (reverse blks)

flatToBullets' :: Integer -> [Block] -> [Block]
flatToBullets' _ [] = []
flatToBullets' num xs@(b : elems)
  | getLevelN b == num = b : (flatToBullets' num elems)
  | otherwise =
    let bNumId = getNumIdN b
        bLevel = getLevelN b
        (children, remaining) =
          span
          (\b' ->
            ((getLevelN b') > bLevel ||
             ((getLevelN b') == bLevel && (getNumIdN b') == bNumId)))
          xs
    in
     case getListType b of
       Just (Enumerated attr) ->
         (OrderedList attr (separateBlocks $ flatToBullets' bLevel children)) :
         (flatToBullets' num remaining)
       _ ->
         (BulletList (separateBlocks $ flatToBullets' bLevel children)) :
         (flatToBullets' num remaining)

flatToBullets :: [Block] -> [Block]
flatToBullets elems = flatToBullets' (-1) elems

singleItemHeaderToHeader :: Block -> Block
singleItemHeaderToHeader (OrderedList _ [[h@(Header _ _ _)]]) = h
singleItemHeaderToHeader blk = blk


blocksToBullets :: [Block] -> [Block]
blocksToBullets blks =
  map singleItemHeaderToHeader $
  bottomUp removeListDivs $
  flatToBullets $ (handleListParagraphs blks)

plainParaInlines :: Block -> [Inline]
plainParaInlines (Plain ils) = ils
plainParaInlines (Para ils) = ils
plainParaInlines _ = []

blocksToDefinitions' :: [([Inline], [[Block]])] -> [Block] -> [Block] -> [Block]
blocksToDefinitions' []     acc [] = reverse acc
blocksToDefinitions' defAcc acc [] =
  reverse $ (DefinitionList (reverse defAcc)) : acc
blocksToDefinitions' defAcc acc
  ((Div (_, classes1, _) blks1) : (Div (ident2, classes2, kvs2) blks2) : blks)
  | "DefinitionTerm" `elem` classes1 && "Definition"  `elem` classes2 =
    let remainingAttr2 = (ident2, delete "Definition" classes2, kvs2)
        pair = case remainingAttr2 == ("", [], []) of
          True -> (concatMap plainParaInlines blks1, [blks2])
          False -> (concatMap plainParaInlines blks1, [[Div remainingAttr2 blks2]])
    in
     blocksToDefinitions' (pair : defAcc) acc blks
blocksToDefinitions' defAcc acc
  ((Div (ident2, classes2, kvs2) blks2) : blks)
  | (not . null) defAcc && "Definition"  `elem` classes2 =
    let remainingAttr2 = (ident2, delete "Definition" classes2, kvs2)
        defItems2 = case remainingAttr2 == ("", [], []) of
          True -> blks2
          False -> [Div remainingAttr2 blks2]
        ((defTerm, defItems):defs) = defAcc
        defAcc' = case null defItems of
          True -> (defTerm, [defItems2]) : defs
          False -> (defTerm, init defItems ++ [last defItems ++ defItems2]) : defs
    in
     blocksToDefinitions' defAcc' acc blks
blocksToDefinitions' [] acc (b:blks) =
  blocksToDefinitions' [] (b:acc) blks
blocksToDefinitions' defAcc acc (b:blks) =
  blocksToDefinitions' [] (b : (DefinitionList (reverse defAcc)) : acc) blks

removeListDivs' :: Block -> [Block]
removeListDivs' (Div (ident, classes, kvs) blks)
  | "list-item" `elem` classes =
    case delete "list-item" classes of
      [] -> blks
      classes' -> [Div (ident, classes', kvs) $ blks]
removeListDivs' (Div (ident, classes, kvs) blks)
  | not $ null $ listParagraphDivs `intersect` classes =
    case classes \\ listParagraphDivs of
      [] -> blks
      classes' -> [Div (ident, classes', kvs) blks]
removeListDivs' blk = [blk]

removeListDivs :: [Block] -> [Block]
removeListDivs = concatMap removeListDivs'



blocksToDefinitions :: [Block] -> [Block]
blocksToDefinitions = blocksToDefinitions' [] []
