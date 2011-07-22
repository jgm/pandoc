{-# LANGUAGE DeriveDataTypeable #-}
{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Shared
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Utility functions and definitions used by the various Pandoc modules.
-}
module Text.Pandoc.Shared (
                     -- * List processing
                     splitBy,
                     splitByIndices,
                     substitute,
                     -- * Text processing
                     backslashEscapes,
                     escapeStringUsing,
                     stripTrailingNewlines,
                     removeLeadingTrailingSpace,
                     removeLeadingSpace,
                     removeTrailingSpace,
                     stripFirstAndLast,
                     camelCaseToHyphenated,
                     toRomanNumeral,
                     escapeURI,
                     unescapeURI,
                     tabFilter,
                     -- * Pandoc block and inline list processing
                     orderedListMarkers,
                     normalizeSpaces,
                     normalize,
                     stringify,
                     compactify,
                     Element (..),
                     hierarchicalize,
                     uniqueIdent,
                     isHeaderBlock,
                     headerShift,
                     -- * Writer options
                     HTMLMathMethod (..),
                     CiteMethod (..),
                     ObfuscationMethod (..),
                     HTMLSlideVariant (..),
                     WriterOptions (..),
                     defaultWriterOptions,
                     -- * File handling
                     inDirectory,
                     findDataFile,
                     readDataFile,
                    ) where

import Text.Pandoc.Definition
import Text.Pandoc.Generic
import qualified Text.Pandoc.UTF8 as UTF8 (readFile)
import Data.Char ( toLower, isLower, isUpper, isAlpha, isAscii,
                   isLetter, isDigit )
import Data.List ( find, isPrefixOf, intercalate )
import Network.URI ( isAllowedInURI, escapeURIString, unEscapeString )
import Codec.Binary.UTF8.String ( encodeString, decodeString )
import System.Directory
import System.FilePath ( (</>) )
import Data.Generics (Typeable, Data)
import qualified Control.Monad.State as S
import Paths_pandoc (getDataFileName)

--
-- List processing
--

-- | Split list by groups of one or more sep.
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy isSep lst =
  let (first, rest) = break isSep lst
      rest'         = dropWhile isSep rest
  in  first:(splitBy isSep rest')

-- | Split list into chunks divided at specified indices.
splitByIndices :: [Int] -> [a] -> [[a]]
splitByIndices [] lst = [lst]
splitByIndices (x:xs) lst =
    let (first, rest) = splitAt x lst in
    first:(splitByIndices (map (\y -> y - x)  xs) rest)

-- | Replace each occurrence of one sublist in a list with another.
substitute :: (Eq a) => [a] -> [a] -> [a] -> [a]
substitute _ _ [] = []
substitute [] _ xs = xs
substitute target replacement lst@(x:xs) =
    if target `isPrefixOf` lst
       then replacement ++ substitute target replacement (drop (length target) lst)
       else x : substitute target replacement xs

--
-- Text processing
--

-- | Returns an association list of backslash escapes for the
-- designated characters.
backslashEscapes :: [Char]    -- ^ list of special characters to escape
                 -> [(Char, String)]
backslashEscapes = map (\ch -> (ch, ['\\',ch]))

-- | Escape a string of characters, using an association list of
-- characters and strings.
escapeStringUsing :: [(Char, String)] -> String -> String
escapeStringUsing _ [] = ""
escapeStringUsing escapeTable (x:xs) = 
  case (lookup x escapeTable) of
       Just str  -> str ++ rest
       Nothing   -> x:rest
  where rest = escapeStringUsing escapeTable xs

-- | Strip trailing newlines from string.
stripTrailingNewlines :: String -> String
stripTrailingNewlines = reverse . dropWhile (== '\n') . reverse

-- | Remove leading and trailing space (including newlines) from string.
removeLeadingTrailingSpace :: String -> String
removeLeadingTrailingSpace = removeLeadingSpace . removeTrailingSpace

-- | Remove leading space (including newlines) from string.
removeLeadingSpace :: String -> String
removeLeadingSpace = dropWhile (`elem` " \n\t")

-- | Remove trailing space (including newlines) from string.
removeTrailingSpace :: String -> String
removeTrailingSpace = reverse . removeLeadingSpace . reverse

-- | Strip leading and trailing characters from string
stripFirstAndLast :: String -> String
stripFirstAndLast str =
  drop 1 $ take ((length str) - 1) str

-- | Change CamelCase word to hyphenated lowercase (e.g., camel-case). 
camelCaseToHyphenated :: String -> String
camelCaseToHyphenated [] = ""
camelCaseToHyphenated (a:b:rest) | isLower a && isUpper b =
  a:'-':(toLower b):(camelCaseToHyphenated rest)
camelCaseToHyphenated (a:rest) = (toLower a):(camelCaseToHyphenated rest)

-- | Convert number < 4000 to uppercase roman numeral.
toRomanNumeral :: Int -> String
toRomanNumeral x =
  if x >= 4000 || x < 0
     then "?"
     else case x of
              _ | x >= 1000 -> "M" ++ toRomanNumeral (x - 1000)
              _ | x >= 900  -> "CM" ++ toRomanNumeral (x - 900)
              _ | x >= 500  -> "D" ++ toRomanNumeral (x - 500)
              _ | x >= 400  -> "CD" ++ toRomanNumeral (x - 400)
              _ | x >= 100  -> "C" ++ toRomanNumeral (x - 100)
              _ | x >= 90   -> "XC" ++ toRomanNumeral (x - 90)
              _ | x >= 50   -> "L"  ++ toRomanNumeral (x - 50)
              _ | x >= 40   -> "XL" ++ toRomanNumeral (x - 40)
              _ | x >= 10   -> "X" ++ toRomanNumeral (x - 10)
              _ | x >= 9    -> "IX" ++ toRomanNumeral (x - 5)
              _ | x >= 5    -> "V" ++ toRomanNumeral (x - 5)
              _ | x >= 4    -> "IV" ++ toRomanNumeral (x - 4)
              _ | x >= 1    -> "I" ++ toRomanNumeral (x - 1)
              _             -> ""

-- | Escape unicode characters in a URI.  Characters that are
-- already valid in a URI, including % and ?, are left alone.
escapeURI :: String -> String
escapeURI = escapeURIString isAllowedInURI . encodeString

-- | Unescape unicode and some special characters in a URI, but
-- without introducing spaces.
unescapeURI :: String -> String
unescapeURI = escapeURIString (\c -> isAllowedInURI c || not (isAscii c)) .
               decodeString . unEscapeString

-- | Convert tabs to spaces and filter out DOS line endings.
-- Tabs will be preserved if tab stop is set to 0.
tabFilter :: Int       -- ^ Tab stop
          -> String    -- ^ Input
          -> String
tabFilter tabStop =
  let go _ [] = ""
      go _ ('\n':xs) = '\n' : go tabStop xs
      go _ ('\r':'\n':xs) = '\n' : go tabStop xs
      go _ ('\r':xs) = '\n' : go tabStop xs
      go spsToNextStop ('\t':xs) =
        if tabStop == 0
           then '\t' : go tabStop xs
           else replicate spsToNextStop ' ' ++ go tabStop xs
      go 1 (x:xs) =
        x : go tabStop xs
      go spsToNextStop (x:xs) =
        x : go (spsToNextStop - 1) xs
  in  go tabStop

--
-- Pandoc block and inline list processing
--

-- | Generate infinite lazy list of markers for an ordered list,
-- depending on list attributes.
orderedListMarkers :: (Int, ListNumberStyle, ListNumberDelim) -> [String]
orderedListMarkers (start, numstyle, numdelim) = 
  let singleton c = [c]
      nums = case numstyle of
                     DefaultStyle -> map show [start..]
                     Example      -> map show [start..]
                     Decimal      -> map show [start..]
                     UpperAlpha   -> drop (start - 1) $ cycle $ 
                                     map singleton ['A'..'Z']
                     LowerAlpha   -> drop (start - 1) $ cycle $
                                     map singleton ['a'..'z']
                     UpperRoman   -> map toRomanNumeral [start..]
                     LowerRoman   -> map (map toLower . toRomanNumeral) [start..]
      inDelim str = case numdelim of
                            DefaultDelim -> str ++ "."
                            Period       -> str ++ "."
                            OneParen     -> str ++ ")"
                            TwoParens    -> "(" ++ str ++ ")"
  in  map inDelim nums

-- | Normalize a list of inline elements: remove leading and trailing
-- @Space@ elements, collapse double @Space@s into singles, and
-- remove empty Str elements.
normalizeSpaces :: [Inline] -> [Inline]
normalizeSpaces = cleanup . dropWhile isSpaceOrEmpty
 where  cleanup [] = []
        cleanup (Space:rest) = let rest' = dropWhile isSpaceOrEmpty rest
                               in  case rest' of
                                   []            -> []
                                   _             -> Space : cleanup rest'
        cleanup ((Str ""):rest) = cleanup rest
        cleanup (x:rest) = x : cleanup rest

isSpaceOrEmpty :: Inline -> Bool
isSpaceOrEmpty Space = True
isSpaceOrEmpty (Str "") = True
isSpaceOrEmpty _ = False

-- | Normalize @Pandoc@ document, consolidating doubled 'Space's,
-- combining adjacent 'Str's and 'Emph's, remove 'Null's and
-- empty elements, etc.
normalize :: (Eq a, Data a) => a -> a
normalize = topDown removeEmptyBlocks .
            topDown consolidateInlines .
            bottomUp (removeEmptyInlines . removeTrailingInlineSpaces)

removeEmptyBlocks :: [Block] -> [Block]
removeEmptyBlocks (Null : xs) = removeEmptyBlocks xs
removeEmptyBlocks (BulletList [] : xs) = removeEmptyBlocks xs
removeEmptyBlocks (OrderedList _ [] : xs) = removeEmptyBlocks xs
removeEmptyBlocks (DefinitionList [] : xs) = removeEmptyBlocks xs
removeEmptyBlocks (RawBlock _ [] : xs) = removeEmptyBlocks xs
removeEmptyBlocks (x:xs) = x : removeEmptyBlocks xs
removeEmptyBlocks [] = []

removeEmptyInlines :: [Inline] -> [Inline]
removeEmptyInlines (Emph [] : zs) = removeEmptyInlines zs
removeEmptyInlines (Strong [] : zs) = removeEmptyInlines zs
removeEmptyInlines (Subscript [] : zs) = removeEmptyInlines zs
removeEmptyInlines (Superscript [] : zs) = removeEmptyInlines zs
removeEmptyInlines (SmallCaps [] : zs) = removeEmptyInlines zs
removeEmptyInlines (Strikeout [] : zs) = removeEmptyInlines zs
removeEmptyInlines (RawInline _ [] : zs) = removeEmptyInlines zs
removeEmptyInlines (Code _ [] : zs) = removeEmptyInlines zs
removeEmptyInlines (Str "" : zs) = removeEmptyInlines zs
removeEmptyInlines (x : xs) = x : removeEmptyInlines xs
removeEmptyInlines [] = []

removeTrailingInlineSpaces :: [Inline] -> [Inline]
removeTrailingInlineSpaces = reverse . removeLeadingInlineSpaces . reverse

removeLeadingInlineSpaces :: [Inline] -> [Inline]
removeLeadingInlineSpaces = dropWhile isSpaceOrEmpty

consolidateInlines :: [Inline] -> [Inline]
consolidateInlines (Str x : ys) =
  case concat (x : map fromStr strs) of
        ""     -> consolidateInlines rest
        n      -> Str n : consolidateInlines rest
   where
     (strs, rest)  = span isStr ys
     isStr (Str _) = True
     isStr _       = False
     fromStr (Str z) = z
     fromStr _       = error "consolidateInlines - fromStr - not a Str"
consolidateInlines (Space : ys) = Space : rest
   where isSpace Space = True
         isSpace _     = False
         rest          = consolidateInlines $ dropWhile isSpace ys
consolidateInlines (Emph xs : Emph ys : zs) = consolidateInlines $
  Emph (xs ++ ys) : zs
consolidateInlines (Strong xs : Strong ys : zs) = consolidateInlines $
  Strong (xs ++ ys) : zs
consolidateInlines (Subscript xs : Subscript ys : zs) = consolidateInlines $
  Subscript (xs ++ ys) : zs
consolidateInlines (Superscript xs : Superscript ys : zs) = consolidateInlines $
  Superscript (xs ++ ys) : zs
consolidateInlines (SmallCaps xs : SmallCaps ys : zs) = consolidateInlines $
  SmallCaps (xs ++ ys) : zs
consolidateInlines (Strikeout xs : Strikeout ys : zs) = consolidateInlines $
  Strikeout (xs ++ ys) : zs
consolidateInlines (RawInline f x : RawInline f' y : zs) | f == f' =
  consolidateInlines $ RawInline f (x ++ y) : zs
consolidateInlines (Code a1 x : Code a2 y : zs) | a1 == a2 =
  consolidateInlines $ Code a1 (x ++ y) : zs
consolidateInlines (x : xs) = x : consolidateInlines xs
consolidateInlines [] = []

-- | Convert list of inlines to a string with formatting removed.
stringify :: [Inline] -> String
stringify = queryWith go
  where go :: Inline -> [Char]
        go Space = " "
        go (Str x) = x
        go (Code _ x) = x
        go (Math _ x) = x
        go EmDash = "--"
        go EnDash = "-"
        go Apostrophe = "'"
        go Ellipses = "..."
        go LineBreak = " "
        go _ = ""

-- | Change final list item from @Para@ to @Plain@ if the list contains
-- no other @Para@ blocks.
compactify :: [[Block]]  -- ^ List of list items (each a list of blocks)
           -> [[Block]]
compactify [] = []
compactify items =
  case (init items, last items) of
       (_,[])          -> items
       (others, final) ->
            case last final of
                 Para a -> case (filter isPara $ concat items) of
                                -- if this is only Para, change to Plain
                                [_] -> others ++ [init final ++ [Plain a]]
                                _   -> items
                 _      -> items

isPara :: Block -> Bool
isPara (Para _) = True
isPara _        = False

-- | Data structure for defining hierarchical Pandoc documents
data Element = Blk Block 
             | Sec Int [Int] String [Inline] [Element]
             --    lvl  num ident  label    contents
             deriving (Eq, Read, Show, Typeable, Data)

-- | Convert Pandoc inline list to plain text identifier.  HTML
-- identifiers must start with a letter, and may contain only
-- letters, digits, and the characters _-.
inlineListToIdentifier :: [Inline] -> String
inlineListToIdentifier =
  dropWhile (not . isAlpha) . intercalate "-" . words .
    map (nbspToSp . toLower) .
    filter (\c -> isLetter c || isDigit c || c `elem` "_-. ") .
    stringify
 where nbspToSp '\160'     =  ' '
       nbspToSp x          =  x

-- | Convert list of Pandoc blocks into (hierarchical) list of Elements
hierarchicalize :: [Block] -> [Element]
hierarchicalize blocks = S.evalState (hierarchicalizeWithIds blocks) ([],[])

hierarchicalizeWithIds :: [Block] -> S.State ([Int],[String]) [Element]
hierarchicalizeWithIds [] = return []
hierarchicalizeWithIds ((Header level title'):xs) = do
  (lastnum, usedIdents) <- S.get
  let ident = uniqueIdent title' usedIdents
  let lastnum' = take level lastnum
  let newnum = if length lastnum' >= level
                  then init lastnum' ++ [last lastnum' + 1] 
                  else lastnum ++ replicate (level - length lastnum - 1) 0 ++ [1]
  S.put (newnum, (ident : usedIdents))
  let (sectionContents, rest) = break (headerLtEq level) xs
  sectionContents' <- hierarchicalizeWithIds sectionContents
  rest' <- hierarchicalizeWithIds rest
  return $ Sec level newnum ident title' sectionContents' : rest'
hierarchicalizeWithIds (x:rest) = do
  rest' <- hierarchicalizeWithIds rest
  return $ (Blk x) : rest'

headerLtEq :: Int -> Block -> Bool
headerLtEq level (Header l _) = l <= level
headerLtEq _ _ = False

-- | Generate a unique identifier from a list of inlines.
-- Second argument is a list of already used identifiers.
uniqueIdent :: [Inline] -> [String] -> String
uniqueIdent title' usedIdents =
  let baseIdent = case inlineListToIdentifier title' of
                        ""   -> "section"
                        x    -> x
      numIdent n = baseIdent ++ "-" ++ show n
  in  if baseIdent `elem` usedIdents
        then case find (\x -> numIdent x `notElem` usedIdents) ([1..60000] :: [Int]) of
                  Just x  -> numIdent x
                  Nothing -> baseIdent   -- if we have more than 60,000, allow repeats
        else baseIdent

-- | True if block is a Header block.
isHeaderBlock :: Block -> Bool
isHeaderBlock (Header _ _) = True
isHeaderBlock _ = False

-- | Shift header levels up or down.
headerShift :: Int -> Pandoc -> Pandoc
headerShift n = bottomUp shift
  where shift :: Block -> Block
        shift (Header level inner) = Header (level + n) inner
        shift x                    = x

--
-- Writer options
--

data HTMLMathMethod = PlainMath 
                    | LaTeXMathML (Maybe String)  -- url of LaTeXMathML.js
                    | JsMath (Maybe String)       -- url of jsMath load script
                    | GladTeX
                    | WebTeX String               -- url of TeX->image script.
                    | MathML (Maybe String)       -- url of MathMLinHTML.js
                    | MathJax String              -- url of MathJax.js
                    deriving (Show, Read, Eq)

data CiteMethod = Citeproc                        -- use citeproc to render them
                  | Natbib                        -- output natbib cite commands
                  | Biblatex                      -- output biblatex cite commands
                deriving (Show, Read, Eq)

-- | Methods for obfuscating email addresses in HTML.
data ObfuscationMethod = NoObfuscation
                       | ReferenceObfuscation
                       | JavascriptObfuscation
                       deriving (Show, Read, Eq)

-- | Varieties of HTML slide shows.
data HTMLSlideVariant = S5Slides
                      | SlidySlides
                      | NoSlides
                      deriving (Show, Read, Eq)

-- | Options for writers
data WriterOptions = WriterOptions
  { writerStandalone       :: Bool   -- ^ Include header and footer
  , writerTemplate         :: String -- ^ Template to use in standalone mode
  , writerVariables        :: [(String, String)] -- ^ Variables to set in template
  , writerEPUBMetadata     :: String -- ^ Metadata to include in EPUB
  , writerTabStop          :: Int    -- ^ Tabstop for conversion btw spaces and tabs
  , writerTableOfContents  :: Bool   -- ^ Include table of contents
  , writerSlideVariant     :: HTMLSlideVariant -- ^ Are we writing S5 or Slidy?
  , writerIncremental      :: Bool   -- ^ True if lists should be incremental
  , writerXeTeX            :: Bool   -- ^ Create latex suitable for use by xetex
  , writerHTMLMathMethod   :: HTMLMathMethod  -- ^ How to print math in HTML
  , writerIgnoreNotes      :: Bool   -- ^ Ignore footnotes (used in making toc)
  , writerNumberSections   :: Bool   -- ^ Number sections in LaTeX
  , writerSectionDivs      :: Bool   -- ^ Put sections in div tags in HTML
  , writerStrictMarkdown   :: Bool   -- ^ Use strict markdown syntax
  , writerReferenceLinks   :: Bool   -- ^ Use reference links in writing markdown, rst
  , writerWrapText         :: Bool   -- ^ Wrap text to line length
  , writerColumns          :: Int    -- ^ Characters in a line (for text wrapping)
  , writerLiterateHaskell  :: Bool   -- ^ Write as literate haskell
  , writerEmailObfuscation :: ObfuscationMethod -- ^ How to obfuscate emails
  , writerIdentifierPrefix :: String -- ^ Prefix for section & note ids in HTML
  , writerSourceDirectory  :: FilePath -- ^ Directory path of 1st source file
  , writerUserDataDir      :: Maybe FilePath -- ^ Path of user data directory
  , writerCiteMethod       :: CiteMethod -- ^ How to print cites
  , writerBiblioFiles      :: [FilePath] -- ^ Biblio files to use for citations
  , writerHtml5            :: Bool       -- ^ Produce HTML5
  , writerChapters         :: Bool       -- ^ Use "chapter" for top-level sects
  , writerListings         :: Bool       -- ^ Use listings package for code
  , writerAscii            :: Bool       -- ^ Avoid non-ascii characters
  } deriving Show

{-# DEPRECATED writerXeTeX "writerXeTeX no longer does anything" #-}
-- | Default writer options.
defaultWriterOptions :: WriterOptions
defaultWriterOptions = 
  WriterOptions { writerStandalone       = False
                , writerTemplate         = ""
                , writerVariables        = []
                , writerEPUBMetadata     = ""
                , writerTabStop          = 4
                , writerTableOfContents  = False
                , writerSlideVariant     = NoSlides
                , writerIncremental      = False
                , writerXeTeX            = False
                , writerHTMLMathMethod   = PlainMath
                , writerIgnoreNotes      = False
                , writerNumberSections   = False
                , writerSectionDivs      = False
                , writerStrictMarkdown   = False
                , writerReferenceLinks   = False
                , writerWrapText         = True
                , writerColumns          = 72
                , writerLiterateHaskell  = False
                , writerEmailObfuscation = JavascriptObfuscation
                , writerIdentifierPrefix = ""
                , writerSourceDirectory  = "."
                , writerUserDataDir      = Nothing
                , writerCiteMethod       = Citeproc
                , writerBiblioFiles      = []
                , writerHtml5            = False
                , writerChapters         = False
                , writerListings         = False
                , writerAscii            = False
                }

--
-- File handling
--

-- | Perform an IO action in a directory, returning to starting directory.
inDirectory :: FilePath -> IO a -> IO a
inDirectory path action = do
  oldDir <- getCurrentDirectory
  setCurrentDirectory path
  result <- action
  setCurrentDirectory oldDir
  return result

-- | Get file path for data file, either from specified user data directory,
-- or, if not found there, from Cabal data directory.
findDataFile :: Maybe FilePath -> FilePath -> IO FilePath
findDataFile Nothing f = getDataFileName f
findDataFile (Just u) f = do
  ex <- doesFileExist (u </> f)
  if ex
     then return (u </> f)
     else getDataFileName f

-- | Read file from specified user data directory or, if not found there, from
-- Cabal data directory.
readDataFile :: Maybe FilePath -> FilePath -> IO String
readDataFile userDir fname = findDataFile userDir fname >>= UTF8.readFile
