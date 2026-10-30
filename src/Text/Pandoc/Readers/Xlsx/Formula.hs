{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Xlsx.Formula
   Copyright   : © 2026 Nacho García
   License     : GNU GPL, version 2 or above

   Maintainer  : Nacho García <hello@nachogarcia.dev>
   Stability   : alpha
   Portability : portable

Formulas of XLSX cells: moving a shared formula to another cell.
-}
module Text.Pandoc.Readers.Xlsx.Formula
  ( shiftFormula
  ) where

import Control.Applicative ((<|>))
import Data.Char (isAsciiUpper, isAsciiLower, isDigit, ord, chr, toUpper)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Data.Text (Text)

-- | Move a formula by a number of rows and columns, as Excel does when
-- it fills a shared formula into the other cells of its range: each
-- cell or range reference moves, except for the parts anchored with @$@.
-- A reference moved off the sheet becomes @#REF!@.
shiftFormula :: Int -> Int -> Text -> Text
shiftFormula dRow dCol = T.pack . go Nothing . T.unpack
  where
    -- prev: the last significant character copied, to tell a whole
    -- column or row range (A:C, 1:3) from other uses of a word or number
    go _ [] = []
    go _ ('"':cs) = let (s, rest) = quoted '"' cs
                     in '"' : s ++ go (Just '"') rest
    go _ ('\'':cs) = let (s, rest) = quoted '\'' cs
                      in '\'' : s ++ go (Just '\'') rest
    go _ ('[':cs) = let (s, rest) = break (== ']') cs
                     in '[' : s ++ take 1 rest ++ go (Just ']') (drop 1 rest)
    go prev cs@(c:_)
      | isWordChar c =
          let (w, rest) = span isWordChar cs
              next = take 1 rest
           in shiftWord prev w next ++ go (lastOf w) rest
    go _ (c:cs) = c : go (Just c) cs

    lastOf = listToMaybe . reverse

    -- A word is a reference unless it names a function or a sheet. A
    -- bare column (A) or row (1) is one only next to a colon, as in A:C.
    shiftWord prev w next
      | next == "(" || next == "!" = w
      | Just ref <- cellRef w = ref
      | prev == Just ':' || next == ":" =
          fromMaybe w (columnRef w <|> rowRef w)
      | otherwise = w

    cellRef w = do
      let (colAbs, w1) = dollar w
          (letters, w2) = span isAsciiUpper w1
          (rowAbs, w3) = dollar w2
      if null letters || length letters > 3 || null w3 || not (all isDigit w3)
         then Nothing
         else do
           col <- validCol (fromLetters letters)
           row <- validRow (read w3)
           let col' = if colAbs then col else col + dCol
               row' = if rowAbs then row else row + dRow
           return $ case (validCol col', validRow row') of
             (Just c', Just r') -> ['$' | colAbs] ++ toLetters c' ++
                                   ['$' | rowAbs] ++ show r'
             _ -> "#REF!"
    columnRef w = do
      let (colAbs, w1) = dollar w
      if null w1 || length w1 > 3 || not (all isAsciiUpper w1)
         then Nothing
         else do
           col <- validCol (fromLetters w1)
           let col' = if colAbs then col else col + dCol
           return $ maybe "#REF!" (\c' -> ['$' | colAbs] ++ toLetters c')
                          (validCol col')
    rowRef w = do
      let (rowAbs, w1) = dollar w
      if null w1 || not (all isDigit w1)
         then Nothing
         else do
           row <- validRow (read w1)
           let row' = if rowAbs then row else row + dRow
           return $ maybe "#REF!" (\r' -> ['$' | rowAbs] ++ show r')
                          (validRow row')

    dollar ('$':rest) = (True, rest)
    dollar s = (False, s)
    validCol n = if n >= 1 && n <= 16384 then Just n else Nothing
    validRow n = if n >= 1 && n <= (1048576 :: Int) then Just n else Nothing

-- | The text up to and including the closing quote, where a doubled
-- quote stands for the quote itself
quoted :: Char -> String -> (String, String)
quoted q cs = case break (== q) cs of
  (s, _:q':rest) | q' == q -> let (s', rest') = quoted q rest
                               in (s ++ [q, q] ++ s', rest')
  (s, _:rest) -> (s ++ [q], rest)
  (s, []) -> (s, [])

isWordChar :: Char -> Bool
isWordChar c = isAsciiUpper c || isAsciiLower c || isDigit c ||
               c `elem` ("_.$\\" :: String)

fromLetters :: String -> Int
fromLetters = foldl (\acc c -> acc * 26 + ord (toUpper c) - ord 'A' + 1) 0

toLetters :: Int -> String
toLetters n
  | n <= 0 = ""
  | otherwise = let (q, r) = (n - 1) `divMod` 26
                 in toLetters q ++ [chr (ord 'A' + r)]
