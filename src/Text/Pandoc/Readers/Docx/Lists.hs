{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Docx.Lists
   Copyright   : Copyright (C) 2014-2020 Jesse Rosenthal
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Functions for converting flat docx paragraphs into nested lists.
-}

module Text.Pandoc.Readers.Docx.Lists ( blocksToBullets
                                      , blocksToDefinitions
                                      , listParagraphDivs
                                      , listParagraphStyles
                                      ) where

import Data.List
import Data.Maybe
import Data.String (fromString)
import qualified Data.Text as T
import Text.Pandoc.Generic (bottomUp)
import Text.Pandoc.JSON
import Text.Pandoc.Readers.Docx.Parse (ParaStyleName)
import Text.Pandoc.Shared (trim, safeRead)

isListItem :: Block -> Bool
isListItem (Div (_, classes, _) _) | "list-item" `elem` classes = True
isListItem _                       = False

getLevel :: Block -> Maybe Integer
getLevel (Div (_, _, kvs) _) =  lookup "level" kvs >>= safeRead
getLevel _                   = Nothing

getLevelN :: Block -> Integer
getLevelN b = fromMaybe (-1) (getLevel b)

getNumId :: Block -> Maybe Integer
getNumId (Div (_, _, kvs) _) =  lookup "num-id" kvs >>= safeRead
getNumId _                   = Nothing

getNumIdN :: Block -> Integer
getNumIdN b = fromMaybe (-1) (getNumId b)

getText :: Block -> Maybe T.Text
getText (Div (_, _, kvs) _) = lookup "text" kvs
getText _                   = Nothing

data ListType = Itemized | Enumerated ListAttributes

listStyleMap :: [(T.Text, ListNumberStyle)]
listStyleMap = [("upperLetter", UpperAlpha),
                ("lowerLetter", LowerAlpha),
                ("upperRoman", UpperRoman),
                ("lowerRoman", LowerRoman),
                ("decimal", Decimal)]

listDelimMap :: [(T.Text, ListNumberDelim)]
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
                  fromMaybe 1 (start >>= safeRead) :: Int,
                  fromMaybe DefaultStyle (lookup f listStyleMap),
                  fromMaybe DefaultDelim (lookup t listDelimMap))
         Nothing -> Nothing
     _ -> Nothing
getListType _ = Nothing

listParagraphDivs :: [T.Text]
listParagraphDivs = ["list-paragraph"]

listParagraphStyles :: [ParaStyleName]
listParagraphStyles = map (fromString . T.unpack) listParagraphDivs

-- This is a first stab at going through and attaching meaning to list
-- paragraphs, without an item marker, following a list item. We
-- assume that these are paragraphs in the same item.

handleListParagraphs :: [Block] -> [Block]
handleListParagraphs [] = []
handleListParagraphs (
  Div attr1@(_, classes1, _) blks1 :
  Div (ident2, classes2, kvs2) blks2 :
  blks
  ) | "list-item" `elem` classes1 &&
    notElem "list-item" classes2 &&
    (not . null) (listParagraphDivs `intersect` classes2) =
      -- We don't want to keep this indent.
      let newDiv2 =
            Div (ident2, classes2, filter (\kv -> fst kv /= "indent") kvs2) blks2
      in
       handleListParagraphs (Div attr1 (blks1 ++ [newDiv2]) : blks)
handleListParagraphs (blk:blks) = blk : handleListParagraphs blks

separateBlocks' :: Block -> [[Block]] -> [[Block]]
separateBlocks' blk [[]] = [[blk]]
separateBlocks' b@(BulletList _) acc = init acc ++ [last acc ++ [b]]
separateBlocks' b@(OrderedList _ _) acc = init acc ++ [last acc ++ [b]]
-- The following is for the invisible bullet lists. This is how
-- pandoc-generated ooxml does multiparagraph item lists.
separateBlocks' b acc | fmap trim (getText b) == Just "" =
  init acc ++ [last acc ++ [b]]
separateBlocks' b acc = acc ++ [[b]]

separateBlocks :: [Block] -> [[Block]]
separateBlocks blks = foldr separateBlocks' [[]] (reverse blks)

flatToBullets' :: Integer -> [Block] -> [Block]
flatToBullets' _ [] = []
flatToBullets' num xs@(b : elems)
  | getLevelN b == num = b : flatToBullets' num elems
  | otherwise =
    let bNumId = getNumIdN b
        bLevel = getLevelN b
        (children, remaining) =
          span
          (\b' ->
            getLevelN b' > bLevel ||
             (getLevelN b' == bLevel && getNumIdN b' == bNumId))
          xs
    in
     case getListType b of
       Just (Enumerated attr) ->
         OrderedList attr (separateBlocks $ flatToBullets' bLevel children) :
         flatToBullets' num remaining
       _ ->
         BulletList (separateBlocks $ flatToBullets' bLevel children) :
         flatToBullets' num remaining

flatToBullets :: [Block] -> [Block]
flatToBullets elems = flatToBullets' (-1) elems

singleItemHeaderToHeader :: Block -> Block
singleItemHeaderToHeader (OrderedList _ [[h@Header{}]]) = h
singleItemHeaderToHeader blk                            = blk


blocksToBullets :: [Block] -> [Block]
blocksToBullets blks =
  map singleItemHeaderToHeader $
  bottomUp removeListDivs $flatToBullets (handleListParagraphs blks)

plainParaInlines :: Block -> [Inline]
plainParaInlines (Plain ils) = ils
plainParaInlines (Para ils)  = ils
plainParaInlines _           = []

blocksToDefinitions' :: [([Inline], [[Block]])] -> [Block] -> [Block] -> [Block]
blocksToDefinitions' []     acc [] = reverse acc
blocksToDefinitions' defAcc acc [] =
  reverse $ DefinitionList (reverse defAcc) : acc
blocksToDefinitions' defAcc acc
  (Div (_, classes1, _) blks1 : Div (ident2, classes2, kvs2) blks2 : blks)
  | "Definition-Term" `elem` classes1 && "Definition"  `elem` classes2 =
    let remainingAttr2 = (ident2, delete "Definition" classes2, kvs2)
        pair = if remainingAttr2 == ("", [], []) then (concatMap plainParaInlines blks1, [blks2]) else (concatMap plainParaInlines blks1, [[Div remainingAttr2 blks2]])
    in
     blocksToDefinitions' (pair : defAcc) acc blks
blocksToDefinitions' ((defTerm, defItems):defs) acc
  (Div (ident2, classes2, kvs2) blks2 : blks)
  | "Definition"  `elem` classes2 =
    let remainingAttr2 = (ident2, delete "Definition" classes2, kvs2)
        defItems2 = if remainingAttr2 == ("", [], [])
          then blks2
          else [Div remainingAttr2 blks2]
        defAcc' = if null defItems
          then (defTerm, [defItems2]) : defs
          else (defTerm, init defItems ++ [last defItems ++ defItems2]) : defs
    in
     blocksToDefinitions' defAcc' acc blks
blocksToDefinitions' [] acc (b:blks) =
  blocksToDefinitions' [] (b:acc) blks
blocksToDefinitions' defAcc acc (b:blks) =
  blocksToDefinitions' [] (b : DefinitionList (reverse defAcc) : acc) blks

removeListDivs' :: Block -> [Block]
removeListDivs' (Div (ident, classes, kvs) blks)
  | "list-item" `elem` classes =
    case delete "list-item" classes of
      []       -> blks
      classes' -> [Div (ident, classes', kvs) blks]
removeListDivs' (Div (ident, classes, kvs) blks)
  | not $ null $ listParagraphDivs `intersect` classes =
    case classes \\ listParagraphDivs of
      []       -> blks
      classes' -> [Div (ident, classes', kvs) blks]
removeListDivs' blk = [blk]

removeListDivs :: [Block] -> [Block]
removeListDivs = concatMap removeListDivs'

blocksToDefinitions :: [Block] -> [Block]
blocksToDefinitions = blocksToDefinitions' [] []
