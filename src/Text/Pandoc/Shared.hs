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
                     wrapped,
                     wrapIfNeeded,
                     wrappedTeX,
                     wrapTeXIfNeeded,
                     BlockWrapper (..),
                     wrappedBlocksToDoc,
                     tabFilter,
                     -- * Prettyprinting
                     hang',
                     -- * Pandoc block and inline list processing
                     orderedListMarkers,
                     normalizeSpaces,
                     compactify,
                     Element (..),
                     hierarchicalize,
                     uniqueIdent,
                     isHeaderBlock,
                     -- * Writer options
                     HTMLMathMethod (..),
                     ObfuscationMethod (..),
                     WriterOptions (..),
                     defaultWriterOptions,
                     -- * File handling
                     inDirectory,
                     readDataFile
                    ) where

import Text.Pandoc.Definition
import qualified Text.Pandoc.UTF8 as UTF8 (readFile)
import Text.PrettyPrint.HughesPJ ( Doc, fsep, ($$), (<>), empty, isEmpty, text, nest )
import qualified Text.PrettyPrint.HughesPJ as PP
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
splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy sep lst = 
  let (first, rest) = break (== sep) lst
      rest'         = dropWhile (== sep) rest
  in  first:(splitBy sep rest')

-- | Split list into chunks divided at specified indices.
splitByIndices :: [Int] -> [a] -> [[a]]
splitByIndices [] lst = [lst]
splitByIndices (x:xs) lst =
    let (first, rest) = splitAt x lst in
    first:(splitByIndices (map (\y -> y - x)  xs) rest)

-- | Replace each occurrence of one sublist in a list with another.
substitute :: (Eq a) => [a] -> [a] -> [a] -> [a]
substitute _ _ [] = []
substitute [] _ lst = lst
substitute target replacement lst = 
    if target `isPrefixOf` lst
       then replacement ++ (substitute target replacement $ drop (length target) lst)
       else (head lst):(substitute target replacement $ tail lst)

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

-- | Wrap inlines to line length.
wrapped :: Monad m => ([Inline] -> m Doc) -> [Inline] -> m Doc
wrapped listWriter sect = (mapM listWriter $ splitBy Space sect) >>= 
                          return . fsep

-- | Wrap inlines if the text wrap option is selected.
wrapIfNeeded :: Monad m => WriterOptions -> ([Inline] -> m Doc) -> 
                           [Inline] -> m Doc
wrapIfNeeded opts = if writerWrapText opts
                       then wrapped 
                       else ($)

-- auxiliary function for wrappedTeX
isNote :: Inline -> Bool
isNote (Note _) = True
isNote _ = False

-- | Wrap inlines to line length, treating footnotes in a way that
-- makes sense in LaTeX and ConTeXt.
wrappedTeX :: Monad m 
           => Bool
           -> ([Inline] -> m Doc) 
           -> [Inline] 
           -> m Doc
wrappedTeX includePercent listWriter sect = do
  let (firstpart, rest) = break isNote sect
  firstpartWrapped <- wrapped listWriter firstpart
  if null rest
     then return firstpartWrapped
     else do let (note:rest') = rest
             let (rest1, rest2) = break (== Space) rest'
             -- rest1 is whatever comes between the note and a Space.
             -- if the note is followed directly by a Space, rest1 is null.
             -- rest1 is printed after the note but before the line break,
             -- to avoid spurious blank space the note and immediately
             -- following punctuation.
             rest1Out <- if null rest1
                            then return empty
                            else listWriter rest1
             rest2Wrapped <- if null rest2
                                then return empty
                                else wrappedTeX includePercent listWriter (tail rest2)
             noteText <- listWriter [note]
             return $ (firstpartWrapped <> if includePercent then PP.char '%' else empty) $$ 
                      (noteText <> rest1Out) $$
                      rest2Wrapped

-- | Wrap inlines if the text wrap option is selected, specialized 
-- for LaTeX and ConTeXt.
wrapTeXIfNeeded :: Monad m 
                => WriterOptions
                -> Bool
                -> ([Inline] -> m Doc) 
                -> [Inline] 
                -> m Doc
wrapTeXIfNeeded opts includePercent = if writerWrapText opts
                                         then wrappedTeX includePercent
                                         else ($)

-- | Indicates whether block should be surrounded by blank lines (@Pad@) or not (@Reg@).
data BlockWrapper = Pad Doc | Reg Doc

-- | Converts a list of wrapped blocks to a Doc, with appropriate spaces around blocks.
wrappedBlocksToDoc :: [BlockWrapper] -> Doc
wrappedBlocksToDoc = foldr addBlock empty
     where addBlock (Pad d) accum | isEmpty accum = d
           addBlock (Pad d) accum = d $$ text "" $$ accum
           addBlock (Reg d) accum = d $$ accum

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
-- Prettyprinting
--

-- | A version of hang that works like the version in pretty-1.0.0.0
hang' :: Doc -> Int -> Doc -> Doc
hang' d1 n d2 = d1 $$ (nest n d2)

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
normalizeSpaces [] = []
normalizeSpaces list = 
    let removeDoubles [] = []
        removeDoubles (Space:Space:rest) = removeDoubles (Space:rest)
        removeDoubles (Space:(Str ""):Space:rest) = removeDoubles (Space:rest)
        removeDoubles ((Str ""):rest) = removeDoubles rest 
        removeDoubles (x:rest) = x:(removeDoubles rest)
        removeLeading (Space:xs) = removeLeading xs
        removeLeading x = x
        removeTrailing [] = []
        removeTrailing lst = if (last lst == Space)
                                then init lst
                                else lst
    in  removeLeading $ removeTrailing $ removeDoubles list

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
-- letters, digits, and the characters _-:.
inlineListToIdentifier :: [Inline] -> String
inlineListToIdentifier =
  dropWhile (not . isAlpha) . intercalate "-" . words . map toLower .
  filter (\c -> isLetter c || isDigit c || c `elem` "_-:. ") .
  concatMap extractText
    where extractText x = case x of
              Str s           -> s
              Emph lst        -> concatMap extractText lst
              Strikeout lst   -> concatMap extractText lst
              Superscript lst -> concatMap extractText lst
              SmallCaps   lst -> concatMap extractText lst
              Subscript lst   -> concatMap extractText lst
              Strong lst      -> concatMap extractText lst
              Quoted _ lst    -> concatMap extractText lst
              Cite   _ lst    -> concatMap extractText lst
              Code s          -> s
              Space           -> " "
              EmDash          -> "---"
              EnDash          -> "--"
              Apostrophe      -> ""
              Ellipses        -> "..."
              LineBreak       -> " "
              Math _ s        -> s
              TeX _           -> ""
              HtmlInline _    -> ""
              Link lst _      -> concatMap extractText lst
              Image lst _     -> concatMap extractText lst
              Note _          -> ""

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

--
-- Writer options
--

data HTMLMathMethod = PlainMath 
                    | LaTeXMathML (Maybe String)  -- url of LaTeXMathML.js
                    | JsMath (Maybe String)       -- url of jsMath load script
                    | GladTeX
                    | MimeTeX String              -- url of mimetex.cgi 
                    | MathML (Maybe String)       -- url of MathMLinHTML.js
                    deriving (Show, Read, Eq)

-- | Methods for obfuscating email addresses in HTML.
data ObfuscationMethod = NoObfuscation
                       | ReferenceObfuscation
                       | JavascriptObfuscation
                       deriving (Show, Read, Eq)

-- | Options for writers
data WriterOptions = WriterOptions
  { writerStandalone       :: Bool   -- ^ Include header and footer
  , writerTemplate         :: String -- ^ Template to use in standalone mode
  , writerVariables        :: [(String, String)] -- ^ Variables to set in template
  , writerEPUBMetadata     :: String -- ^ Metadata to include in EPUB
  , writerTabStop          :: Int    -- ^ Tabstop for conversion btw spaces and tabs
  , writerTableOfContents  :: Bool   -- ^ Include table of contents
  , writerS5               :: Bool   -- ^ We're writing S5 
  , writerXeTeX            :: Bool   -- ^ Create latex suitable for use by xetex
  , writerHTMLMathMethod   :: HTMLMathMethod  -- ^ How to print math in HTML
  , writerIgnoreNotes      :: Bool   -- ^ Ignore footnotes (used in making toc)
  , writerIncremental      :: Bool   -- ^ Incremental S5 lists
  , writerNumberSections   :: Bool   -- ^ Number sections in LaTeX
  , writerStrictMarkdown   :: Bool   -- ^ Use strict markdown syntax
  , writerReferenceLinks   :: Bool   -- ^ Use reference links in writing markdown, rst
  , writerWrapText         :: Bool   -- ^ Wrap text to line length
  , writerLiterateHaskell  :: Bool   -- ^ Write as literate haskell
  , writerEmailObfuscation :: ObfuscationMethod -- ^ How to obfuscate emails
  , writerIdentifierPrefix :: String -- ^ Prefix for section & note ids in HTML
  } deriving Show

-- | Default writer options.
defaultWriterOptions :: WriterOptions
defaultWriterOptions = 
  WriterOptions { writerStandalone       = False
                , writerTemplate         = ""
                , writerVariables        = []
                , writerEPUBMetadata     = ""
                , writerTabStop          = 4
                , writerTableOfContents  = False
                , writerS5               = False
                , writerXeTeX            = False
                , writerHTMLMathMethod   = PlainMath
                , writerIgnoreNotes      = False
                , writerIncremental      = False
                , writerNumberSections   = False
                , writerStrictMarkdown   = False
                , writerReferenceLinks   = False
                , writerWrapText         = True
                , writerLiterateHaskell  = False
                , writerEmailObfuscation = JavascriptObfuscation
                , writerIdentifierPrefix = ""
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

-- | Read file from specified user data directory or, if not found there, from
-- Cabal data directory.
readDataFile :: Maybe FilePath -> FilePath -> IO String
readDataFile userDir fname =
  case userDir of
       Nothing  -> getDataFileName fname >>= UTF8.readFile
       Just u   -> catch (UTF8.readFile $ u </> fname)
                   (\_ -> getDataFileName fname >>= UTF8.readFile)
