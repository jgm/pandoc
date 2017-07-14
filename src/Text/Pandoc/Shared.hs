{-# LANGUAGE DeriveDataTypeable, CPP, MultiParamTypeClasses,
    FlexibleContexts, ScopedTypeVariables, PatternGuards,
    ViewPatterns #-}
{-
Copyright (C) 2006-2017 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2017 John MacFarlane
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
                     ordNub,
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
                     crFilter,
                     -- * Date/time
                     normalizeDate,
                     -- * Pandoc block and inline list processing
                     orderedListMarkers,
                     extractSpaces,
                     removeFormatting,
                     deNote,
                     stringify,
                     capitalize,
                     compactify,
                     compactifyDL,
                     linesToPara,
                     Element (..),
                     hierarchicalize,
                     uniqueIdent,
                     inlineListToIdentifier,
                     isHeaderBlock,
                     headerShift,
                     isTightList,
                     addMetaField,
                     makeMeta,
                     eastAsianLineBreakFilter,
                     -- * TagSoup HTML handling
                     renderTags',
                     -- * File handling
                     inDirectory,
                     getDefaultReferenceDocx,
                     getDefaultReferenceODT,
                     readDataFile,
                     readDataFileUTF8,
                     openURL,
                     collapseFilePath,
                     filteredFilesFromArchive,
                     -- * URI handling
                     schemes,
                     isURI,
                     -- * Error handling
                     mapLeft,
                     -- * for squashing blocks
                     blocksToInlines,
                     -- * Safe read
                     safeRead,
                     -- * Temp directory
                     withTempDir,
                     -- * Version
                     pandocVersion
                    ) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.Builder (Inlines, Blocks, ToMetaValue(..))
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Char ( toLower, isLower, isUpper, isAlpha,
                   isLetter, isDigit, isSpace )
import Data.List ( find, stripPrefix, intercalate )
import Data.Maybe (mapMaybe)
import Data.Version ( showVersion )
import qualified Data.Map as M
import Network.URI ( URI(uriScheme), escapeURIString, unEscapeString, parseURI )
import qualified Data.Set as Set
import System.Directory
import System.FilePath (splitDirectories, isPathSeparator)
import qualified System.FilePath.Posix as Posix
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.Error (PandocError(..))
import System.FilePath ( (</>) )
import Data.Generics (Typeable, Data)
import qualified Control.Monad.State.Strict as S
import qualified Control.Exception as E
import Control.Monad (msum, unless, MonadPlus(..))
import Text.Pandoc.Pretty (charWidth)
import Text.Pandoc.Generic (bottomUp)
import Text.Pandoc.Compat.Time
import Data.Time.Clock.POSIX
import System.IO.Error
import System.IO.Temp
import Text.HTML.TagSoup (renderTagsOptions, RenderOptions(..), Tag(..),
         renderOptions)
import Data.Monoid ((<>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Base64 (decodeLenient)
import Data.Sequence (ViewR(..), ViewL(..), viewl, viewr)
import qualified Data.Text as T
import Data.ByteString.Lazy (toChunks, fromChunks)
import qualified Data.ByteString.Lazy as BL
import Paths_pandoc (version)

import Codec.Archive.Zip

#ifdef EMBED_DATA_FILES
import Text.Pandoc.Data (dataFiles)
#else
import Paths_pandoc (getDataFileName)
#endif
import Network.HTTP.Client (httpLbs, responseBody, responseHeaders,
                            Request(port,host,requestHeaders),
                            HttpException)
import Network.HTTP.Client (parseRequest)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.Internal (addProxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnv)
import Network.HTTP.Types.Header ( hContentType, hUserAgent)
import Network (withSocketsDo)

-- | Version number of pandoc library.
pandocVersion :: String
pandocVersion = showVersion version

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
    case stripPrefix target lst of
      Just lst' -> replacement ++ substitute target replacement lst'
      Nothing   -> x : substitute target replacement xs

ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

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
toRomanNumeral x
  | x >= 4000 || x < 0 = "?"
  | x >= 1000 = "M" ++ toRomanNumeral (x - 1000)
  | x >= 900  = "CM" ++ toRomanNumeral (x - 900)
  | x >= 500  = "D" ++ toRomanNumeral (x - 500)
  | x >= 400  = "CD" ++ toRomanNumeral (x - 400)
  | x >= 100  = "C" ++ toRomanNumeral (x - 100)
  | x >= 90   = "XC" ++ toRomanNumeral (x - 90)
  | x >= 50   = "L"  ++ toRomanNumeral (x - 50)
  | x >= 40   = "XL" ++ toRomanNumeral (x - 40)
  | x >= 10   = "X" ++ toRomanNumeral (x - 10)
  | x == 9    = "IX"
  | x >= 5    = "V" ++ toRomanNumeral (x - 5)
  | x == 4    = "IV"
  | x >= 1    = "I" ++ toRomanNumeral (x - 1)
  | otherwise = ""

-- | Escape whitespace and some punctuation characters in URI.
escapeURI :: String -> String
escapeURI = escapeURIString (not . needsEscaping)
  where needsEscaping c = isSpace c || c `elem`
                           ['<','>','|','"','{','}','[',']','^', '`']

-- | Convert tabs to spaces. Tabs will be preserved if tab stop is set to 0.
tabFilter :: Int       -- ^ Tab stop
          -> T.Text    -- ^ Input
          -> T.Text
tabFilter 0 = id
tabFilter tabStop = T.unlines . map go . T.lines
  where go s =
         let (s1, s2) = T.break (== '\t') s
         in  if T.null s2
                then s1
                else s1 <> T.replicate
                       (tabStop - (T.length s1 `mod` tabStop)) (T.pack " ")
                       <> go (T.drop 1 s2)

-- | Strip out DOS line endings.
crFilter :: T.Text -> T.Text
crFilter = T.filter (/= '\r')

--
-- Date/time
--

-- | Parse a date and convert (if possible) to "YYYY-MM-DD" format. We
-- limit years to the range 1601-9999 (ISO 8601 accepts greater than
-- or equal to 1583, but MS Word only accepts dates starting 1601).
normalizeDate :: String -> Maybe String
normalizeDate s = fmap (formatTime defaultTimeLocale "%F")
  (msum $ map (\fs -> parsetimeWith fs s >>= rejectBadYear) formats :: Maybe Day)
  where rejectBadYear day = case toGregorian day of
          (y, _, _) | y >= 1601 && y <= 9999 -> Just day
          _ -> Nothing
        parsetimeWith =
#if MIN_VERSION_time(1,5,0)
             parseTimeM True defaultTimeLocale
#else
             parseTime defaultTimeLocale
#endif
        formats = ["%x","%m/%d/%Y", "%D","%F", "%d %b %Y",
                    "%d %B %Y", "%b. %d, %Y", "%B %d, %Y",
                    "%Y%m%d", "%Y%m", "%Y"]

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

-- | Extract the leading and trailing spaces from inside an inline element
-- and place them outside the element.  SoftBreaks count as Spaces for
-- these purposes.
extractSpaces :: (Inlines -> Inlines) -> Inlines -> Inlines
extractSpaces f is =
  let contents = B.unMany is
      left  = case viewl contents of
                    (Space :< _)     -> B.space
                    (SoftBreak :< _) -> B.softbreak
                    _                -> mempty
      right = case viewr contents of
                    (_ :> Space)     -> B.space
                    (_ :> SoftBreak) -> B.softbreak
                    _                -> mempty in
  (left <> f (B.trimInlines . B.Many $ contents) <> right)

-- | Extract inlines, removing formatting.
removeFormatting :: Walkable Inline a => a -> [Inline]
removeFormatting = query go . walk deNote
  where go :: Inline -> [Inline]
        go (Str xs)     = [Str xs]
        go Space        = [Space]
        go SoftBreak    = [SoftBreak]
        go (Code _ x)   = [Str x]
        go (Math _ x)   = [Str x]
        go LineBreak    = [Space]
        go _            = []

deNote :: Inline -> Inline
deNote (Note _) = Str ""
deNote x        = x

-- | Convert pandoc structure to a string with formatting removed.
-- Footnotes are skipped (since we don't want their contents in link
-- labels).
stringify :: Walkable Inline a => a -> String
stringify = query go . walk deNote
  where go :: Inline -> [Char]
        go Space = " "
        go SoftBreak = " "
        go (Str x) = x
        go (Code _ x) = x
        go (Math _ x) = x
        go (RawInline (Format "html") ('<':'b':'r':_)) = " " -- see #2105
        go LineBreak = " "
        go _ = ""

-- | Bring all regular text in a pandoc structure to uppercase.
--
-- This function correctly handles cases where a lowercase character doesn't
-- match to a single uppercase character – e.g. “Straße” would be converted
-- to “STRASSE”, not “STRAßE”.
capitalize :: Walkable Inline a => a -> a
capitalize = walk go
  where go :: Inline -> Inline
        go (Str s) = Str (T.unpack $ T.toUpper $ T.pack s)
        go x       = x

-- | Change final list item from @Para@ to @Plain@ if the list contains
-- no other @Para@ blocks.  Like compactify, but operates on @Blocks@ rather
-- than @[Block]@.
compactify :: [Blocks]  -- ^ List of list items (each a list of blocks)
           -> [Blocks]
compactify [] = []
compactify items =
  let (others, final) = (init items, last items)
  in  case reverse (B.toList final) of
           (Para a:xs) -> case [Para x | Para x <- concatMap B.toList items] of
                            -- if this is only Para, change to Plain
                            [_] -> others ++ [B.fromList (reverse $ Plain a : xs)]
                            _   -> items
           _      -> items

-- | Like @compactify@, but acts on items of definition lists.
compactifyDL :: [(Inlines, [Blocks])] -> [(Inlines, [Blocks])]
compactifyDL items =
  let defs = concatMap snd items
  in  case reverse (concatMap B.toList defs) of
           (Para x:xs)
             | not (any isPara xs) ->
                   let (t,ds) = last items
                       lastDef = B.toList $ last ds
                       ds' = init ds ++
                             if null lastDef
                                then [B.fromList lastDef]
                                else [B.fromList $ init lastDef ++ [Plain x]]
                    in init items ++ [(t, ds')]
             | otherwise           -> items
           _                       -> items

-- | Combine a list of lines by adding hard linebreaks.
combineLines :: [[Inline]] -> [Inline]
combineLines = intercalate [LineBreak]

-- | Convert a list of lines into a paragraph with hard line breaks. This is
--   useful e.g. for rudimentary support of LineBlock elements in writers.
linesToPara :: [[Inline]] -> Block
linesToPara = Para . combineLines

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
hierarchicalizeWithIds ((Div ("",["references"],[])
                         (Header level (ident,classes,kvs) title' : xs)):ys) =
  hierarchicalizeWithIds ((Header level (ident,("references":classes),kvs)
                           title') : (xs ++ ys))
hierarchicalizeWithIds (x:rest) = do
  rest' <- hierarchicalizeWithIds rest
  return $ (Blk x) : rest'

headerLtEq :: Int -> Block -> Bool
headerLtEq level (Header l _ _) = l <= level
headerLtEq level (Div ("",["references"],[]) (Header l _ _ : _))  = l <= level
headerLtEq _ _ = False

-- | Generate a unique identifier from a list of inlines.
-- Second argument is a list of already used identifiers.
uniqueIdent :: [Inline] -> Set.Set String -> String
uniqueIdent title' usedIdents
  =  let baseIdent = case inlineListToIdentifier title' of
                        ""   -> "section"
                        x    -> x
         numIdent n = baseIdent ++ "-" ++ show n
     in  if baseIdent `Set.member` usedIdents
           then case find (\x -> not $ numIdent x `Set.member` usedIdents) ([1..60000] :: [Int]) of
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
isTightList = all firstIsPlain
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
  where combine newval (MetaList xs) = MetaList (xs ++ tolist newval)
        combine newval x             = MetaList [x, newval]
        tolist (MetaList ys)         = ys
        tolist y                     = [y]

-- | Create 'Meta' from old-style title, authors, date.  This is
-- provided to ease the transition from the old API.
makeMeta :: [Inline] -> [[Inline]] -> [Inline] -> Meta
makeMeta title authors date =
      addMetaField "title" (B.fromList title)
    $ addMetaField "author" (map B.fromList authors)
    $ addMetaField "date" (B.fromList date)
    $ nullMeta

-- | Remove soft breaks between East Asian characters.
eastAsianLineBreakFilter :: Pandoc -> Pandoc
eastAsianLineBreakFilter = bottomUp go
  where go (x:SoftBreak:y:zs) =
         case (stringify x, stringify y) of
               (xs@(_:_), (c:_))
                 | charWidth (last xs) == 2 && charWidth c == 2 -> x:y:zs
               _ -> x:SoftBreak:y:zs
        go xs = xs

--
-- TagSoup HTML handling
--

-- | Render HTML tags.
renderTags' :: [Tag String] -> String
renderTags' = renderTagsOptions
               renderOptions{ optMinimize = matchTags ["hr", "br", "img",
                                                       "meta", "link"]
                            , optRawTag   = matchTags ["script", "style"] }
              where matchTags = \tags -> flip elem tags . map toLower

--
-- File handling
--

-- | Perform an IO action in a directory, returning to starting directory.
inDirectory :: FilePath -> IO a -> IO a
inDirectory path action = E.bracket
                             getCurrentDirectory
                             setCurrentDirectory
                             (const $ setCurrentDirectory path >> action)

getDefaultReferenceDocx :: Maybe FilePath -> IO Archive
getDefaultReferenceDocx datadir = do
  let paths = ["[Content_Types].xml",
               "_rels/.rels",
               "docProps/app.xml",
               "docProps/core.xml",
               "word/document.xml",
               "word/fontTable.xml",
               "word/footnotes.xml",
               "word/numbering.xml",
               "word/settings.xml",
               "word/webSettings.xml",
               "word/styles.xml",
               "word/_rels/document.xml.rels",
               "word/_rels/footnotes.xml.rels",
               "word/theme/theme1.xml"]
  let toLazy = fromChunks . (:[])
  let pathToEntry path = do epochtime <- (floor . utcTimeToPOSIXSeconds) <$>
                                          getCurrentTime
                            contents <- toLazy <$> readDataFile datadir
                                                       ("docx/" ++ path)
                            return $ toEntry path epochtime contents
  mbArchive <- case datadir of
                    Nothing   -> return Nothing
                    Just d    -> do
                       exists <- doesFileExist (d </> "reference.docx")
                       if exists
                          then return (Just (d </> "reference.docx"))
                          else return Nothing
  case mbArchive of
     Just arch -> toArchive <$> BL.readFile arch
     Nothing   -> foldr addEntryToArchive emptyArchive <$>
                     mapM pathToEntry paths

getDefaultReferenceODT :: Maybe FilePath -> IO Archive
getDefaultReferenceODT datadir = do
  let paths = ["mimetype",
               "manifest.rdf",
               "styles.xml",
               "content.xml",
               "meta.xml",
               "settings.xml",
               "Configurations2/accelerator/current.xml",
               "Thumbnails/thumbnail.png",
               "META-INF/manifest.xml"]
  let pathToEntry path = do epochtime <- floor `fmap` getPOSIXTime
                            contents <- (fromChunks . (:[])) `fmap`
                                          readDataFile datadir ("odt/" ++ path)
                            return $ toEntry path epochtime contents
  mbArchive <- case datadir of
                    Nothing   -> return Nothing
                    Just d    -> do
                       exists <- doesFileExist (d </> "reference.odt")
                       if exists
                          then return (Just (d </> "reference.odt"))
                          else return Nothing
  case mbArchive of
     Just arch -> toArchive <$> BL.readFile arch
     Nothing   -> foldr addEntryToArchive emptyArchive <$>
                     mapM pathToEntry paths


readDefaultDataFile :: FilePath -> IO BS.ByteString
readDefaultDataFile "reference.docx" =
  (BS.concat . toChunks . fromArchive) <$> getDefaultReferenceDocx Nothing
readDefaultDataFile "reference.odt" =
  (BS.concat . toChunks . fromArchive) <$> getDefaultReferenceODT Nothing
readDefaultDataFile fname =
#ifdef EMBED_DATA_FILES
  case lookup (makeCanonical fname) dataFiles of
    Nothing       -> E.throwIO $ PandocCouldNotFindDataFileError fname
    Just contents -> return contents
  where makeCanonical = Posix.joinPath . transformPathParts . splitDirectories
        transformPathParts = reverse . foldl go []
        go as     "."  = as
        go (_:as) ".." = as
        go as     x    = x : as
#else
  getDataFileName fname' >>= checkExistence >>= BS.readFile
    where fname' = if fname == "MANUAL.txt" then fname else "data" </> fname

checkExistence :: FilePath -> IO FilePath
checkExistence fn = do
  exists <- doesFileExist fn
  if exists
     then return fn
     else E.throwIO $ PandocCouldNotFindDataFileError fn
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

-- | Read from a URL and return raw data and maybe mime type.
openURL :: String -> IO (Either HttpException (BS.ByteString, Maybe MimeType))
openURL u
  | Just u'' <- stripPrefix "data:" u =
    let mime     = takeWhile (/=',') u''
        contents = B8.pack $ unEscapeString $ drop 1 $ dropWhile (/=',') u''
    in  return $ Right (decodeLenient contents, Just mime)
  | otherwise = E.try $ withSocketsDo $ do
     let parseReq = parseRequest
     (proxy :: Either IOError String) <-
        tryIOError $ getEnv "http_proxy"
     (useragent :: Either IOError String) <-
        tryIOError $ getEnv "USER_AGENT"
     req <- parseReq u
     req' <- case proxy of
                     Left _   -> return req
                     Right pr -> (parseReq pr >>= \r ->
                                  return $ addProxy (host r) (port r) req)
                                  `mplus` return req
     req'' <- case useragent of
                     Left _ -> return req'
                     Right ua -> do
                                   let headers = requestHeaders req'
                                   let useragentheader = (hUserAgent, B8.pack ua)
                                   let headers' = useragentheader:headers
                                   return $ req' {requestHeaders = headers'}
     resp <- newManager tlsManagerSettings >>= httpLbs req''
     return (BS.concat $ toChunks $ responseBody resp,
             UTF8.toString `fmap` lookup hContentType (responseHeaders resp))

--
-- Error reporting
--

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

-- | Remove intermediate "." and ".." directories from a path.
--
-- > collapseFilePath "./foo" == "foo"
-- > collapseFilePath "/bar/../baz" == "/baz"
-- > collapseFilePath "/../baz" == "/../baz"
-- > collapseFilePath "parent/foo/baz/../bar" ==  "parent/foo/bar"
-- > collapseFilePath "parent/foo/baz/../../bar" ==  "parent/bar"
-- > collapseFilePath "parent/foo/.." ==  "parent"
-- > collapseFilePath "/parent/foo/../../bar" ==  "/bar"
collapseFilePath :: FilePath -> FilePath
collapseFilePath = Posix.joinPath . reverse . foldl go [] . splitDirectories
  where
    go rs "." = rs
    go r@(p:rs) ".." = case p of
                            ".." -> ("..":r)
                            (checkPathSeperator -> Just True) -> ("..":r)
                            _ -> rs
    go _ (checkPathSeperator -> Just True) = [[Posix.pathSeparator]]
    go rs x = x:rs
    isSingleton [] = Nothing
    isSingleton [x] = Just x
    isSingleton _ = Nothing
    checkPathSeperator = fmap isPathSeparator . isSingleton

--
-- File selection from the archive
--
filteredFilesFromArchive :: Archive -> (FilePath -> Bool) -> [(FilePath, BL.ByteString)]
filteredFilesFromArchive zf f =
  mapMaybe (fileAndBinary zf) (filter f (filesInArchive zf))
  where
    fileAndBinary :: Archive -> FilePath -> Maybe (FilePath, BL.ByteString)
    fileAndBinary a fp = findEntryByPath fp a >>= \e -> Just (fp, fromEntry e)


--
-- IANA URIs
--

-- | Schemes from http://www.iana.org/assignments/uri-schemes.html plus
-- the unofficial schemes doi, javascript, isbn, pmid.
schemes :: Set.Set String
schemes = Set.fromList
  -- Official IANA schemes
  [ "aaa", "aaas", "about", "acap", "acct", "acr", "adiumxtra", "afp", "afs"
  , "aim", "appdata", "apt", "attachment", "aw", "barion", "beshare", "bitcoin"
  , "blob", "bolo", "browserext", "callto", "cap", "chrome", "chrome-extension"
  , "cid", "coap", "coaps", "com-eventbrite-attendee", "content", "crid", "cvs"
  , "data", "dav", "dict", "dis", "dlna-playcontainer", "dlna-playsingle"
  , "dns", "dntp", "dtn", "dvb", "ed2k", "example", "facetime", "fax", "feed"
  , "feedready", "file", "filesystem", "finger", "fish", "ftp", "geo", "gg"
  , "git", "gizmoproject", "go", "gopher", "graph", "gtalk", "h323", "ham"
  , "hcp", "http", "https", "hxxp", "hxxps", "hydrazone", "iax", "icap", "icon"
  , "im", "imap", "info", "iotdisco", "ipn", "ipp", "ipps", "irc", "irc6"
  , "ircs", "iris", "iris.beep", "iris.lwz", "iris.xpc", "iris.xpcs"
  , "isostore", "itms", "jabber", "jar", "jms", "keyparc", "lastfm", "ldap"
  , "ldaps", "lvlt", "magnet", "mailserver", "mailto", "maps", "market"
  , "message", "mid", "mms", "modem", "mongodb", "moz", "ms-access"
  , "ms-browser-extension", "ms-drive-to", "ms-enrollment", "ms-excel"
  , "ms-gamebarservices", "ms-getoffice", "ms-help", "ms-infopath"
  , "ms-media-stream-id", "ms-officeapp", "ms-project", "ms-powerpoint"
  , "ms-publisher", "ms-search-repair", "ms-secondary-screen-controller"
  , "ms-secondary-screen-setup", "ms-settings", "ms-settings-airplanemode"
  , "ms-settings-bluetooth", "ms-settings-camera", "ms-settings-cellular"
  , "ms-settings-cloudstorage", "ms-settings-connectabledevices"
  , "ms-settings-displays-topology", "ms-settings-emailandaccounts"
  , "ms-settings-language", "ms-settings-location", "ms-settings-lock"
  , "ms-settings-nfctransactions", "ms-settings-notifications"
  , "ms-settings-power", "ms-settings-privacy", "ms-settings-proximity"
  , "ms-settings-screenrotation", "ms-settings-wifi", "ms-settings-workplace"
  , "ms-spd", "ms-sttoverlay", "ms-transit-to", "ms-virtualtouchpad"
  , "ms-visio", "ms-walk-to", "ms-whiteboard", "ms-whiteboard-cmd", "ms-word"
  , "msnim", "msrp", "msrps", "mtqp", "mumble", "mupdate", "mvn", "news", "nfs"
  , "ni", "nih", "nntp", "notes", "ocf", "oid", "onenote", "onenote-cmd"
  , "opaquelocktoken", "pack", "palm", "paparazzi", "pkcs11", "platform", "pop"
  , "pres", "prospero", "proxy", "pwid", "psyc", "qb", "query", "redis"
  , "rediss", "reload", "res", "resource", "rmi", "rsync", "rtmfp", "rtmp"
  , "rtsp", "rtsps", "rtspu", "secondlife", "service", "session", "sftp", "sgn"
  , "shttp", "sieve", "sip", "sips", "skype", "smb", "sms", "smtp", "snews"
  , "snmp", "soap.beep", "soap.beeps", "soldat", "spotify", "ssh", "steam"
  , "stun", "stuns", "submit", "svn", "tag", "teamspeak", "tel", "teliaeid"
  , "telnet", "tftp", "things", "thismessage", "tip", "tn3270", "tool", "turn"
  , "turns", "tv", "udp", "unreal", "urn", "ut2004", "v-event", "vemmi"
  , "ventrilo", "videotex", "vnc", "view-source", "wais", "webcal", "wpid"
  , "ws", "wss", "wtai", "wyciwyg", "xcon", "xcon-userid", "xfire"
  , "xmlrpc.beep", "xmlrpc.beeps", "xmpp", "xri", "ymsgr", "z39.50", "z39.50r"
  , "z39.50s"
  -- Inofficial schemes
  , "doi", "isbn", "javascript", "pmid"
  ]

-- | Check if the string is a valid URL with a IANA or frequently used but
-- unofficial scheme (see @schemes@).
isURI :: String -> Bool
isURI = maybe False hasKnownScheme . parseURI
  where
    hasKnownScheme = (`Set.member` schemes) . map toLower .
                     filter (/= ':') . uriScheme

---
--- Squash blocks into inlines
---

blockToInlines :: Block -> [Inline]
blockToInlines (Plain ils) = ils
blockToInlines (Para ils) = ils
blockToInlines (LineBlock lns) = combineLines lns
blockToInlines (CodeBlock attr str) = [Code attr str]
blockToInlines (RawBlock fmt str) = [RawInline fmt str]
blockToInlines (BlockQuote blks) = blocksToInlines blks
blockToInlines (OrderedList _ blkslst) =
  concatMap blocksToInlines blkslst
blockToInlines (BulletList blkslst) =
  concatMap blocksToInlines blkslst
blockToInlines (DefinitionList pairslst) =
  concatMap f pairslst
  where
    f (ils, blkslst) = ils ++
      [Str ":", Space] ++
      (concatMap blocksToInlines blkslst)
blockToInlines (Header _ _  ils) = ils
blockToInlines (HorizontalRule) = []
blockToInlines (Table _ _ _ headers rows) =
  intercalate [LineBreak] $ map (concatMap blocksToInlines) tbl
  where
    tbl = headers : rows
blockToInlines (Div _ blks) = blocksToInlines blks
blockToInlines Null = []

blocksToInlinesWithSep :: [Inline] -> [Block] -> [Inline]
blocksToInlinesWithSep sep blks = intercalate sep $ map blockToInlines blks

blocksToInlines :: [Block] -> [Inline]
blocksToInlines = blocksToInlinesWithSep [Space, Str "¶", Space]


--
-- Safe read
--

safeRead :: (MonadPlus m, Read a) => String -> m a
safeRead s = case reads s of
                  (d,x):_
                    | all isSpace x -> return d
                  _                 -> mzero

--
-- Temp directory
--

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir =
#ifdef _WINDOWS
  withTempDirectory "."
#else
  withSystemTempDirectory
#endif
