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
                     getMimeType
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
import System.FilePath ( (</>), takeExtension )
import Data.Generics (Typeable, Data)
import qualified Control.Monad.State as S
import qualified Data.Map as M
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

-- | Determine mime type appropriate for file path.
getMimeType :: FilePath -> Maybe String
getMimeType f = M.lookup (map toLower $ drop 1 $ takeExtension f) mimeTypes
  where mimeTypes = M.fromList -- List borrowed from happstack-server.
           [("gz","application/x-gzip")
           ,("cabal","application/x-cabal")
           ,("%","application/x-trash")
           ,("323","text/h323")
           ,("3gp","video/3gpp")
           ,("7z","application/x-7z-compressed")
           ,("abw","application/x-abiword")
           ,("ai","application/postscript")
           ,("aif","audio/x-aiff")
           ,("aifc","audio/x-aiff")
           ,("aiff","audio/x-aiff")
           ,("alc","chemical/x-alchemy")
           ,("art","image/x-jg")
           ,("asc","text/plain")
           ,("asf","video/x-ms-asf")
           ,("asn","chemical/x-ncbi-asn1")
           ,("aso","chemical/x-ncbi-asn1-binary")
           ,("asx","video/x-ms-asf")
           ,("atom","application/atom")
           ,("atomcat","application/atomcat+xml")
           ,("atomsrv","application/atomserv+xml")
           ,("au","audio/basic")
           ,("avi","video/x-msvideo")
           ,("b","chemical/x-molconn-Z")
           ,("bak","application/x-trash")
           ,("bat","application/x-msdos-program")
           ,("bcpio","application/x-bcpio")
           ,("bib","text/x-bibtex")
           ,("bin","application/octet-stream")
           ,("bmp","image/x-ms-bmp")
           ,("boo","text/x-boo")
           ,("book","application/x-maker")
           ,("bsd","chemical/x-crossfire")
           ,("c","text/x-csrc")
           ,("c++","text/x-c++src")
           ,("c3d","chemical/x-chem3d")
           ,("cab","application/x-cab")
           ,("cac","chemical/x-cache")
           ,("cache","chemical/x-cache")
           ,("cap","application/cap")
           ,("cascii","chemical/x-cactvs-binary")
           ,("cat","application/vnd.ms-pki.seccat")
           ,("cbin","chemical/x-cactvs-binary")
           ,("cbr","application/x-cbr")
           ,("cbz","application/x-cbz")
           ,("cc","text/x-c++src")
           ,("cdf","application/x-cdf")
           ,("cdr","image/x-coreldraw")
           ,("cdt","image/x-coreldrawtemplate")
           ,("cdx","chemical/x-cdx")
           ,("cdy","application/vnd.cinderella")
           ,("cef","chemical/x-cxf")
           ,("cer","chemical/x-cerius")
           ,("chm","chemical/x-chemdraw")
           ,("chrt","application/x-kchart")
           ,("cif","chemical/x-cif")
           ,("class","application/java-vm")
           ,("cls","text/x-tex")
           ,("cmdf","chemical/x-cmdf")
           ,("cml","chemical/x-cml")
           ,("cod","application/vnd.rim.cod")
           ,("com","application/x-msdos-program")
           ,("cpa","chemical/x-compass")
           ,("cpio","application/x-cpio")
           ,("cpp","text/x-c++src")
           ,("cpt","application/mac-compactpro")
           ,("crl","application/x-pkcs7-crl")
           ,("crt","application/x-x509-ca-cert")
           ,("csf","chemical/x-cache-csf")
           ,("csh","application/x-csh")
           ,("csm","chemical/x-csml")
           ,("csml","chemical/x-csml")
           ,("css","text/css")
           ,("csv","text/csv")
           ,("ctab","chemical/x-cactvs-binary")
           ,("ctx","chemical/x-ctx")
           ,("cu","application/cu-seeme")
           ,("cub","chemical/x-gaussian-cube")
           ,("cxf","chemical/x-cxf")
           ,("cxx","text/x-c++src")
           ,("d","text/x-dsrc")
           ,("dat","chemical/x-mopac-input")
           ,("dcr","application/x-director")
           ,("deb","application/x-debian-package")
           ,("dif","video/dv")
           ,("diff","text/x-diff")
           ,("dir","application/x-director")
           ,("djv","image/vnd.djvu")
           ,("djvu","image/vnd.djvu")
           ,("dl","video/dl")
           ,("dll","application/x-msdos-program")
           ,("dmg","application/x-apple-diskimage")
           ,("dms","application/x-dms")
           ,("doc","application/msword")
           ,("dot","application/msword")
           ,("dv","video/dv")
           ,("dvi","application/x-dvi")
           ,("dx","chemical/x-jcamp-dx")
           ,("dxr","application/x-director")
           ,("emb","chemical/x-embl-dl-nucleotide")
           ,("embl","chemical/x-embl-dl-nucleotide")
           ,("eml","message/rfc822")
           ,("ent","chemical/x-ncbi-asn1-ascii")
           ,("eps","application/postscript")
           ,("etx","text/x-setext")
           ,("exe","application/x-msdos-program")
           ,("ez","application/andrew-inset")
           ,("fb","application/x-maker")
           ,("fbdoc","application/x-maker")
           ,("fch","chemical/x-gaussian-checkpoint")
           ,("fchk","chemical/x-gaussian-checkpoint")
           ,("fig","application/x-xfig")
           ,("flac","application/x-flac")
           ,("fli","video/fli")
           ,("fm","application/x-maker")
           ,("frame","application/x-maker")
           ,("frm","application/x-maker")
           ,("gal","chemical/x-gaussian-log")
           ,("gam","chemical/x-gamess-input")
           ,("gamin","chemical/x-gamess-input")
           ,("gau","chemical/x-gaussian-input")
           ,("gcd","text/x-pcs-gcd")
           ,("gcf","application/x-graphing-calculator")
           ,("gcg","chemical/x-gcg8-sequence")
           ,("gen","chemical/x-genbank")
           ,("gf","application/x-tex-gf")
           ,("gif","image/gif")
           ,("gjc","chemical/x-gaussian-input")
           ,("gjf","chemical/x-gaussian-input")
           ,("gl","video/gl")
           ,("gnumeric","application/x-gnumeric")
           ,("gpt","chemical/x-mopac-graph")
           ,("gsf","application/x-font")
           ,("gsm","audio/x-gsm")
           ,("gtar","application/x-gtar")
           ,("h","text/x-chdr")
           ,("h++","text/x-c++hdr")
           ,("hdf","application/x-hdf")
           ,("hh","text/x-c++hdr")
           ,("hin","chemical/x-hin")
           ,("hpp","text/x-c++hdr")
           ,("hqx","application/mac-binhex40")
           ,("hs","text/x-haskell")
           ,("hta","application/hta")
           ,("htc","text/x-component")
           ,("htm","text/html")
           ,("html","text/html")
           ,("hxx","text/x-c++hdr")
           ,("ica","application/x-ica")
           ,("ice","x-conference/x-cooltalk")
           ,("ico","image/x-icon")
           ,("ics","text/calendar")
           ,("icz","text/calendar")
           ,("ief","image/ief")
           ,("iges","model/iges")
           ,("igs","model/iges")
           ,("iii","application/x-iphone")
           ,("inp","chemical/x-gamess-input")
           ,("ins","application/x-internet-signup")
           ,("iso","application/x-iso9660-image")
           ,("isp","application/x-internet-signup")
           ,("ist","chemical/x-isostar")
           ,("istr","chemical/x-isostar")
           ,("jad","text/vnd.sun.j2me.app-descriptor")
           ,("jar","application/java-archive")
           ,("java","text/x-java")
           ,("jdx","chemical/x-jcamp-dx")
           ,("jmz","application/x-jmol")
           ,("jng","image/x-jng")
           ,("jnlp","application/x-java-jnlp-file")
           ,("jpe","image/jpeg")
           ,("jpeg","image/jpeg")
           ,("jpg","image/jpeg")
           ,("js","application/x-javascript")
           ,("kar","audio/midi")
           ,("key","application/pgp-keys")
           ,("kil","application/x-killustrator")
           ,("kin","chemical/x-kinemage")
           ,("kml","application/vnd.google-earth.kml+xml")
           ,("kmz","application/vnd.google-earth.kmz")
           ,("kpr","application/x-kpresenter")
           ,("kpt","application/x-kpresenter")
           ,("ksp","application/x-kspread")
           ,("kwd","application/x-kword")
           ,("kwt","application/x-kword")
           ,("latex","application/x-latex")
           ,("lha","application/x-lha")
           ,("lhs","text/x-literate-haskell")
           ,("lsf","video/x-la-asf")
           ,("lsx","video/x-la-asf")
           ,("ltx","text/x-tex")
           ,("lyx","application/x-lyx")
           ,("lzh","application/x-lzh")
           ,("lzx","application/x-lzx")
           ,("m3u","audio/mpegurl")
           ,("m4a","audio/mpeg")
           ,("maker","application/x-maker")
           ,("man","application/x-troff-man")
           ,("mcif","chemical/x-mmcif")
           ,("mcm","chemical/x-macmolecule")
           ,("mdb","application/msaccess")
           ,("me","application/x-troff-me")
           ,("mesh","model/mesh")
           ,("mid","audio/midi")
           ,("midi","audio/midi")
           ,("mif","application/x-mif")
           ,("mm","application/x-freemind")
           ,("mmd","chemical/x-macromodel-input")
           ,("mmf","application/vnd.smaf")
           ,("mml","text/mathml")
           ,("mmod","chemical/x-macromodel-input")
           ,("mng","video/x-mng")
           ,("moc","text/x-moc")
           ,("mol","chemical/x-mdl-molfile")
           ,("mol2","chemical/x-mol2")
           ,("moo","chemical/x-mopac-out")
           ,("mop","chemical/x-mopac-input")
           ,("mopcrt","chemical/x-mopac-input")
           ,("mov","video/quicktime")
           ,("movie","video/x-sgi-movie")
           ,("mp2","audio/mpeg")
           ,("mp3","audio/mpeg")
           ,("mp4","video/mp4")
           ,("mpc","chemical/x-mopac-input")
           ,("mpe","video/mpeg")
           ,("mpeg","video/mpeg")
           ,("mpega","audio/mpeg")
           ,("mpg","video/mpeg")
           ,("mpga","audio/mpeg")
           ,("ms","application/x-troff-ms")
           ,("msh","model/mesh")
           ,("msi","application/x-msi")
           ,("mvb","chemical/x-mopac-vib")
           ,("mxu","video/vnd.mpegurl")
           ,("nb","application/mathematica")
           ,("nc","application/x-netcdf")
           ,("nwc","application/x-nwc")
           ,("o","application/x-object")
           ,("oda","application/oda")
           ,("odb","application/vnd.oasis.opendocument.database")
           ,("odc","application/vnd.oasis.opendocument.chart")
           ,("odf","application/vnd.oasis.opendocument.formula")
           ,("odg","application/vnd.oasis.opendocument.graphics")
           ,("odi","application/vnd.oasis.opendocument.image")
           ,("odm","application/vnd.oasis.opendocument.text-master")
           ,("odp","application/vnd.oasis.opendocument.presentation")
           ,("ods","application/vnd.oasis.opendocument.spreadsheet")
           ,("odt","application/vnd.oasis.opendocument.text")
           ,("oga","audio/ogg")
           ,("ogg","application/ogg")
           ,("ogv","video/ogg")
           ,("ogx","application/ogg")
           ,("old","application/x-trash")
           ,("otg","application/vnd.oasis.opendocument.graphics-template")
           ,("oth","application/vnd.oasis.opendocument.text-web")
           ,("otp","application/vnd.oasis.opendocument.presentation-template")
           ,("ots","application/vnd.oasis.opendocument.spreadsheet-template")
           ,("ott","application/vnd.oasis.opendocument.text-template")
           ,("oza","application/x-oz-application")
           ,("p","text/x-pascal")
           ,("p7r","application/x-pkcs7-certreqresp")
           ,("pac","application/x-ns-proxy-autoconfig")
           ,("pas","text/x-pascal")
           ,("pat","image/x-coreldrawpattern")
           ,("patch","text/x-diff")
           ,("pbm","image/x-portable-bitmap")
           ,("pcap","application/cap")
           ,("pcf","application/x-font")
           ,("pcf.Z","application/x-font")
           ,("pcx","image/pcx")
           ,("pdb","chemical/x-pdb")
           ,("pdf","application/pdf")
           ,("pfa","application/x-font")
           ,("pfb","application/x-font")
           ,("pgm","image/x-portable-graymap")
           ,("pgn","application/x-chess-pgn")
           ,("pgp","application/pgp-signature")
           ,("php","application/x-httpd-php")
           ,("php3","application/x-httpd-php3")
           ,("php3p","application/x-httpd-php3-preprocessed")
           ,("php4","application/x-httpd-php4")
           ,("phps","application/x-httpd-php-source")
           ,("pht","application/x-httpd-php")
           ,("phtml","application/x-httpd-php")
           ,("pk","application/x-tex-pk")
           ,("pl","text/x-perl")
           ,("pls","audio/x-scpls")
           ,("pm","text/x-perl")
           ,("png","image/png")
           ,("pnm","image/x-portable-anymap")
           ,("pot","text/plain")
           ,("ppm","image/x-portable-pixmap")
           ,("pps","application/vnd.ms-powerpoint")
           ,("ppt","application/vnd.ms-powerpoint")
           ,("prf","application/pics-rules")
           ,("prt","chemical/x-ncbi-asn1-ascii")
           ,("ps","application/postscript")
           ,("psd","image/x-photoshop")
           ,("py","text/x-python")
           ,("pyc","application/x-python-code")
           ,("pyo","application/x-python-code")
           ,("qt","video/quicktime")
           ,("qtl","application/x-quicktimeplayer")
           ,("ra","audio/x-pn-realaudio")
           ,("ram","audio/x-pn-realaudio")
           ,("rar","application/rar")
           ,("ras","image/x-cmu-raster")
           ,("rd","chemical/x-mdl-rdfile")
           ,("rdf","application/rdf+xml")
           ,("rgb","image/x-rgb")
           ,("rhtml","application/x-httpd-eruby")
           ,("rm","audio/x-pn-realaudio")
           ,("roff","application/x-troff")
           ,("ros","chemical/x-rosdal")
           ,("rpm","application/x-redhat-package-manager")
           ,("rss","application/rss+xml")
           ,("rtf","application/rtf")
           ,("rtx","text/richtext")
           ,("rxn","chemical/x-mdl-rxnfile")
           ,("sct","text/scriptlet")
           ,("sd","chemical/x-mdl-sdfile")
           ,("sd2","audio/x-sd2")
           ,("sda","application/vnd.stardivision.draw")
           ,("sdc","application/vnd.stardivision.calc")
           ,("sdd","application/vnd.stardivision.impress")
           ,("sdf","application/vnd.stardivision.math")
           ,("sds","application/vnd.stardivision.chart")
           ,("sdw","application/vnd.stardivision.writer")
           ,("ser","application/java-serialized-object")
           ,("sgf","application/x-go-sgf")
           ,("sgl","application/vnd.stardivision.writer-global")
           ,("sh","application/x-sh")
           ,("shar","application/x-shar")
           ,("shtml","text/html")
           ,("sid","audio/prs.sid")
           ,("sik","application/x-trash")
           ,("silo","model/mesh")
           ,("sis","application/vnd.symbian.install")
           ,("sisx","x-epoc/x-sisx-app")
           ,("sit","application/x-stuffit")
           ,("sitx","application/x-stuffit")
           ,("skd","application/x-koan")
           ,("skm","application/x-koan")
           ,("skp","application/x-koan")
           ,("skt","application/x-koan")
           ,("smi","application/smil")
           ,("smil","application/smil")
           ,("snd","audio/basic")
           ,("spc","chemical/x-galactic-spc")
           ,("spl","application/futuresplash")
           ,("spx","audio/ogg")
           ,("src","application/x-wais-source")
           ,("stc","application/vnd.sun.xml.calc.template")
           ,("std","application/vnd.sun.xml.draw.template")
           ,("sti","application/vnd.sun.xml.impress.template")
           ,("stl","application/vnd.ms-pki.stl")
           ,("stw","application/vnd.sun.xml.writer.template")
           ,("sty","text/x-tex")
           ,("sv4cpio","application/x-sv4cpio")
           ,("sv4crc","application/x-sv4crc")
           ,("svg","image/svg+xml")
           ,("svgz","image/svg+xml")
           ,("sw","chemical/x-swissprot")
           ,("swf","application/x-shockwave-flash")
           ,("swfl","application/x-shockwave-flash")
           ,("sxc","application/vnd.sun.xml.calc")
           ,("sxd","application/vnd.sun.xml.draw")
           ,("sxg","application/vnd.sun.xml.writer.global")
           ,("sxi","application/vnd.sun.xml.impress")
           ,("sxm","application/vnd.sun.xml.math")
           ,("sxw","application/vnd.sun.xml.writer")
           ,("t","application/x-troff")
           ,("tar","application/x-tar")
           ,("taz","application/x-gtar")
           ,("tcl","application/x-tcl")
           ,("tex","text/x-tex")
           ,("texi","application/x-texinfo")
           ,("texinfo","application/x-texinfo")
           ,("text","text/plain")
           ,("tgf","chemical/x-mdl-tgf")
           ,("tgz","application/x-gtar")
           ,("tif","image/tiff")
           ,("tiff","image/tiff")
           ,("tk","text/x-tcl")
           ,("tm","text/texmacs")
           ,("torrent","application/x-bittorrent")
           ,("tr","application/x-troff")
           ,("ts","text/texmacs")
           ,("tsp","application/dsptype")
           ,("tsv","text/tab-separated-values")
           ,("txt","text/plain")
           ,("udeb","application/x-debian-package")
           ,("uls","text/iuls")
           ,("ustar","application/x-ustar")
           ,("val","chemical/x-ncbi-asn1-binary")
           ,("vcd","application/x-cdlink")
           ,("vcf","text/x-vcard")
           ,("vcs","text/x-vcalendar")
           ,("vmd","chemical/x-vmd")
           ,("vms","chemical/x-vamas-iso14976")
           ,("vrm","x-world/x-vrml")
           ,("vrml","model/vrml")
           ,("vsd","application/vnd.visio")
           ,("wad","application/x-doom")
           ,("wav","audio/x-wav")
           ,("wax","audio/x-ms-wax")
           ,("wbmp","image/vnd.wap.wbmp")
           ,("wbxml","application/vnd.wap.wbxml")
           ,("wk","application/x-123")
           ,("wm","video/x-ms-wm")
           ,("wma","audio/x-ms-wma")
           ,("wmd","application/x-ms-wmd")
           ,("wml","text/vnd.wap.wml")
           ,("wmlc","application/vnd.wap.wmlc")
           ,("wmls","text/vnd.wap.wmlscript")
           ,("wmlsc","application/vnd.wap.wmlscriptc")
           ,("wmv","video/x-ms-wmv")
           ,("wmx","video/x-ms-wmx")
           ,("wmz","application/x-ms-wmz")
           ,("wp5","application/wordperfect5.1")
           ,("wpd","application/wordperfect")
           ,("wrl","model/vrml")
           ,("wsc","text/scriptlet")
           ,("wvx","video/x-ms-wvx")
           ,("wz","application/x-wingz")
           ,("xbm","image/x-xbitmap")
           ,("xcf","application/x-xcf")
           ,("xht","application/xhtml+xml")
           ,("xhtml","application/xhtml+xml")
           ,("xlb","application/vnd.ms-excel")
           ,("xls","application/vnd.ms-excel")
           ,("xlt","application/vnd.ms-excel")
           ,("xml","application/xml")
           ,("xpi","application/x-xpinstall")
           ,("xpm","image/x-xpixmap")
           ,("xsl","application/xml")
           ,("xtel","chemical/x-xtel")
           ,("xul","application/vnd.mozilla.xul+xml")
           ,("xwd","image/x-xwindowdump")
           ,("xyz","chemical/x-xyz")
           ,("zip","application/zip")
           ,("zmt","chemical/x-mopac-input")
           ]
