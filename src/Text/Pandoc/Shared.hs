{-# LANGUAGE DeriveDataTypeable, CPP, MultiParamTypeClasses,
    FlexibleContexts #-}
{-
Copyright (C) 2006-2013 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2013 John MacFarlane
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
                     splitStringByIndices,
                     substitute,
                     -- * Text processing
                     backslashEscapes,
                     escapeStringUsing,
                     stripTrailingNewlines,
                     trim,
                     triml,
                     trimr,
                     stripFirstAndLast,
                     camelCaseToHyphenated,
                     toRomanNumeral,
                     escapeURI,
                     tabFilter,
                     -- * Date/time
                     normalizeDate,
                     -- * Pandoc block and inline list processing
                     orderedListMarkers,
                     normalizeSpaces,
                     normalize,
                     stringify,
                     compactify,
                     compactify',
                     Element (..),
                     hierarchicalize,
                     uniqueIdent,
                     isHeaderBlock,
                     headerShift,
                     isTightList,
                     addMetaField,
                     makeMeta,
                     -- * TagSoup HTML handling
                     renderTags',
                     -- * File handling
                     inDirectory,
                     readDataFile,
                     readDataFileUTF8,
                     fetchItem,
                     openURL,
                     -- * Error handling
                     err,
                     warn,
                     -- * Safe read
                     safeRead
                    ) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.Generic
import Text.Pandoc.Builder (Blocks, ToMetaValue(..))
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.UTF8 as UTF8
import System.Environment (getProgName)
import System.Exit (exitWith, ExitCode(..))
import Data.Char ( toLower, isLower, isUpper, isAlpha,
                   isLetter, isDigit, isSpace )
import Data.List ( find, isPrefixOf, intercalate )
import qualified Data.Map as M
import Network.URI ( escapeURIString, isAbsoluteURI, unEscapeString )
import System.Directory
import Text.Pandoc.MIME (getMimeType)
import System.FilePath ( (</>), takeExtension, dropExtension )
import Data.Generics (Typeable, Data)
import qualified Control.Monad.State as S
import qualified Control.Exception as E
import Control.Monad (msum, unless)
import Text.Pandoc.Pretty (charWidth)
import System.Locale (defaultTimeLocale)
import Data.Time
import System.IO (stderr)
import Text.HTML.TagSoup (renderTagsOptions, RenderOptions(..), Tag(..),
         renderOptions)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Text.Pandoc.Compat.Monoid

#ifdef EMBED_DATA_FILES
import Text.Pandoc.Data (dataFiles)
import System.FilePath ( joinPath, splitDirectories )
#else
import Paths_pandoc (getDataFileName)
#endif
#ifdef HTTP_CONDUIT
import Data.ByteString.Lazy (toChunks)
import Network.HTTP.Conduit (httpLbs, parseUrl, withManager,
                             responseBody, responseHeaders)
import Network.HTTP.Types.Header ( hContentType)
#else
import Network.URI (parseURI)
import Network.HTTP (findHeader, rspBody,
                     RequestMethod(..), HeaderName(..), mkRequest)
import Network.Browser (browse, setAllowRedirects, setOutHandler, request)
#endif

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

splitByIndices :: [Int] -> [a] -> [[a]]
splitByIndices [] lst = [lst]
splitByIndices (x:xs) lst = first:(splitByIndices (map (\y -> y - x)  xs) rest)
  where (first, rest) = splitAt x lst

-- | Split string into chunks divided at specified indices.
splitStringByIndices :: [Int] -> [Char] -> [[Char]]
splitStringByIndices [] lst = [lst]
splitStringByIndices (x:xs) lst =
  let (first, rest) = splitAt' x lst in
  first : (splitStringByIndices (map (\y -> y - x) xs) rest)

splitAt' :: Int -> [Char] -> ([Char],[Char])
splitAt' _ []          = ([],[])
splitAt' n xs | n <= 0 = ([],xs)
splitAt' n (x:xs)      = (x:ys,zs)
  where (ys,zs) = splitAt' (n - charWidth x) xs

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
trim :: String -> String
trim = triml . trimr

-- | Remove leading space (including newlines) from string.
triml :: String -> String
triml = dropWhile (`elem` " \r\n\t")

-- | Remove trailing space (including newlines) from string.
trimr :: String -> String
trimr = reverse . triml . reverse

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

-- | Escape whitespace in URI.
escapeURI :: String -> String
escapeURI = escapeURIString (not . isSpace)

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
-- Date/time
--

-- | Parse a date and convert (if possible) to "YYYY-MM-DD" format.
normalizeDate :: String -> Maybe String
normalizeDate s = fmap (formatTime defaultTimeLocale "%F")
  (msum $ map (\fs -> parsetimeWith fs s) formats :: Maybe Day)
   where parsetimeWith = parseTime defaultTimeLocale
         formats = ["%x","%m/%d/%Y", "%D","%F", "%d %b %Y",
                    "%d %B %Y", "%b. %d, %Y", "%B %d, %Y"]

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
 where  cleanup []              = []
        cleanup (Space:rest)    = case dropWhile isSpaceOrEmpty rest of
                                        []     -> []
                                        (x:xs) -> Space : x : cleanup xs
        cleanup ((Str ""):rest) = cleanup rest
        cleanup (x:rest)        = x : cleanup rest

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
   where isSp Space = True
         isSp _     = False
         rest       = consolidateInlines $ dropWhile isSp ys
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

-- | Convert pandoc structure to a string with formatting removed.
-- Footnotes are skipped (since we don't want their contents in link
-- labels).
stringify :: Walkable Inline a => a -> String
stringify = query go . walk deNote
  where go :: Inline -> [Char]
        go Space = " "
        go (Str x) = x
        go (Code _ x) = x
        go (Math _ x) = x
        go LineBreak = " "
        go _ = ""
        deNote (Note _) = Str ""
        deNote x = x

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

-- | Change final list item from @Para@ to @Plain@ if the list contains
-- no other @Para@ blocks.  Like compactify, but operates on @Blocks@ rather
-- than @[Block]@.
compactify' :: [Blocks]  -- ^ List of list items (each a list of blocks)
           -> [Blocks]
compactify' [] = []
compactify' items =
  let (others, final) = (init items, last items)
  in  case reverse (B.toList final) of
           (Para a:xs) -> case [Para x | Para x <- concatMap B.toList items] of
                            -- if this is only Para, change to Plain
                            [_] -> others ++ [B.fromList (reverse $ Plain a : xs)]
                            _   -> items
           _      -> items

isPara :: Block -> Bool
isPara (Para _) = True
isPara _        = False

-- | Data structure for defining hierarchical Pandoc documents
data Element = Blk Block
             | Sec Int [Int] Attr [Inline] [Element]
             --    lvl  num attributes label    contents
             deriving (Eq, Read, Show, Typeable, Data)

instance Walkable Inline Element where
  walk f (Blk x) = Blk (walk f x)
  walk f (Sec lev nums attr ils elts) = Sec lev nums attr (walk f ils) (walk f elts)
  walkM f (Blk x) = Blk `fmap` walkM f x
  walkM f (Sec lev nums attr ils elts) = do
    ils' <- walkM f ils
    elts' <- walkM f elts
    return $ Sec lev nums attr ils' elts'
  query f (Blk x) = query f x
  query f (Sec _ _ _ ils elts) = query f ils <> query f elts

instance Walkable Block Element where
  walk f (Blk x) = Blk (walk f x)
  walk f (Sec lev nums attr ils elts) = Sec lev nums attr (walk f ils) (walk f elts)
  walkM f (Blk x) = Blk `fmap` walkM f x
  walkM f (Sec lev nums attr ils elts) = do
    ils' <- walkM f ils
    elts' <- walkM f elts
    return $ Sec lev nums attr ils' elts'
  query f (Blk x) = query f x
  query f (Sec _ _ _ ils elts) = query f ils <> query f elts


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
hierarchicalize blocks = S.evalState (hierarchicalizeWithIds blocks) []

hierarchicalizeWithIds :: [Block] -> S.State [Int] [Element]
hierarchicalizeWithIds [] = return []
hierarchicalizeWithIds ((Header level attr@(_,classes,_) title'):xs) = do
  lastnum <- S.get
  let lastnum' = take level lastnum
  let newnum = case length lastnum' of
                    x | "unnumbered" `elem` classes -> []
                      | x >= level -> init lastnum' ++ [last lastnum' + 1]
                      | otherwise -> lastnum ++
                           replicate (level - length lastnum - 1) 0 ++ [1]
  unless (null newnum) $ S.put newnum
  let (sectionContents, rest) = break (headerLtEq level) xs
  sectionContents' <- hierarchicalizeWithIds sectionContents
  rest' <- hierarchicalizeWithIds rest
  return $ Sec level newnum attr title' sectionContents' : rest'
hierarchicalizeWithIds (x:rest) = do
  rest' <- hierarchicalizeWithIds rest
  return $ (Blk x) : rest'

headerLtEq :: Int -> Block -> Bool
headerLtEq level (Header l _ _) = l <= level
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
isHeaderBlock (Header _ _ _) = True
isHeaderBlock _ = False

-- | Shift header levels up or down.
headerShift :: Int -> Pandoc -> Pandoc
headerShift n = walk shift
  where shift :: Block -> Block
        shift (Header level attr inner) = Header (level + n) attr inner
        shift x                         = x

-- | Detect if a list is tight.
isTightList :: [[Block]] -> Bool
isTightList = and . map firstIsPlain
  where firstIsPlain (Plain _ : _) = True
        firstIsPlain _             = False

-- | Set a field of a 'Meta' object.  If the field already has a value,
-- convert it into a list with the new value appended to the old value(s).
addMetaField :: ToMetaValue a
             => String
             -> a
             -> Meta
             -> Meta
addMetaField key val (Meta meta) =
  Meta $ M.insertWith combine key (toMetaValue val) meta
  where combine newval (MetaList xs) = MetaList (xs ++ [newval])
        combine newval x             = MetaList [x, newval]

-- | Create 'Meta' from old-style title, authors, date.  This is
-- provided to ease the transition from the old API.
makeMeta :: [Inline] -> [[Inline]] -> [Inline] -> Meta
makeMeta title authors date =
      addMetaField "title" (B.fromList title)
    $ addMetaField "author" (map B.fromList authors)
    $ addMetaField "date" (B.fromList date)
    $ nullMeta

--
-- TagSoup HTML handling
--

-- | Render HTML tags.
renderTags' :: [Tag String] -> String
renderTags' = renderTagsOptions
               renderOptions{ optMinimize = \x ->
                                    let y = map toLower x
                                    in  y == "hr" || y == "br" ||
                                        y == "img" || y == "meta" ||
                                        y == "link"
                            , optRawTag = \x ->
                                    let y = map toLower x
                                    in  y == "script" || y == "style" }

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

readDefaultDataFile :: FilePath -> IO BS.ByteString
readDefaultDataFile fname =
#ifdef EMBED_DATA_FILES
  case lookup (makeCanonical fname) dataFiles of
    Nothing       -> err 97 $ "Could not find data file " ++ fname
    Just contents -> return contents
  where makeCanonical = joinPath . transformPathParts . splitDirectories
        transformPathParts = reverse . foldl go []
        go as     "."  = as
        go (_:as) ".." = as
        go as     x    = x : as
#else
  getDataFileName ("data" </> fname) >>= checkExistence >>= BS.readFile
   where checkExistence fn = do
           exists <- doesFileExist fn
           if exists
              then return fn
              else err 97 ("Could not find data file " ++ fname)
#endif

-- | Read file from specified user data directory or, if not found there, from
-- Cabal data directory.
readDataFile :: Maybe FilePath -> FilePath -> IO BS.ByteString
readDataFile Nothing fname = readDefaultDataFile fname
readDataFile (Just userDir) fname = do
  exists <- doesFileExist (userDir </> fname)
  if exists
     then BS.readFile (userDir </> fname)
     else readDefaultDataFile fname

-- | Same as 'readDataFile' but returns a String instead of a ByteString.
readDataFileUTF8 :: Maybe FilePath -> FilePath -> IO String
readDataFileUTF8 userDir fname =
  UTF8.toString `fmap` readDataFile userDir fname

-- | Fetch an image or other item from the local filesystem or the net.
-- Returns raw content and maybe mime type.
fetchItem :: Maybe String -> String
          -> IO (Either E.SomeException (BS.ByteString, Maybe String))
fetchItem sourceURL s
  | isAbsoluteURI s = openURL s
  | otherwise       = case sourceURL of
                           Just u  -> openURL (u ++ "/" ++ s)
                           Nothing -> E.try readLocalFile
  where readLocalFile = do
          let mime = case takeExtension s of
                          ".gz" -> getMimeType $ dropExtension s
                          x     -> getMimeType x
          cont <- BS.readFile s
          return (cont, mime)

-- | Read from a URL and return raw data and maybe mime type.
openURL :: String -> IO (Either E.SomeException (BS.ByteString, Maybe String))
openURL u
  | "data:" `isPrefixOf` u =
    let mime     = takeWhile (/=',') $ drop 5 u
        contents = B8.pack $ unEscapeString $ drop 1 $ dropWhile (/=',') u
    in  return $ Right (contents, Just mime)
#ifdef HTTP_CONDUIT
  | otherwise = E.try $ do
     req <- parseUrl u
     resp <- withManager $ httpLbs req
     return (BS.concat $ toChunks $ responseBody resp,
             UTF8.toString `fmap` lookup hContentType (responseHeaders resp))
#else
  | otherwise = E.try $ getBodyAndMimeType `fmap` browse
              (do S.liftIO $ UTF8.hPutStrLn stderr $ "Fetching " ++ u ++ "..."
                  setOutHandler $ const (return ())
                  setAllowRedirects True
                  request (getRequest' u'))
  where getBodyAndMimeType (_, r) = (rspBody r, findHeader HdrContentType r)
        getRequest' uriString = case parseURI uriString of
                                   Nothing -> error ("Not a valid URL: " ++
                                                        uriString)
                                   Just v  -> mkRequest GET v
        u' = escapeURIString (/= '|') u  -- pipes are rejected by Network.URI
#endif

--
-- Error reporting
--

err :: Int -> String -> IO a
err exitCode msg = do
  name <- getProgName
  UTF8.hPutStrLn stderr $ name ++ ": " ++ msg
  exitWith $ ExitFailure exitCode
  return undefined

warn :: String -> IO ()
warn msg = do
  name <- getProgName
  UTF8.hPutStrLn stderr $ name ++ ": " ++ msg

--
-- Safe read
--

safeRead :: (Monad m, Read a) => String -> m a
safeRead s = case reads s of
                  (d,x):_
                    | all isSpace x -> return d
                  _                 -> fail $ "Could not read `" ++ s ++ "'"


