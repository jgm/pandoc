{-# LANGUAGE DeriveDataTypeable, CPP, MultiParamTypeClasses,
    FlexibleContexts, ScopedTypeVariables, PatternGuards,
    ViewPatterns #-}
{-
Copyright (C) 2006-2016 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2016 John MacFarlane
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
                     -- * Date/time
                     normalizeDate,
                     -- * Pandoc block and inline list processing
                     orderedListMarkers,
                     normalizeSpaces,
                     extractSpaces,
                     normalize,
                     normalizeInlines,
                     normalizeBlocks,
                     removeFormatting,
                     stringify,
                     capitalize,
                     compactify,
                     compactify',
                     compactify'DL,
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
                     getDefaultReferenceDocx,
                     getDefaultReferenceODT,
                     readDataFile,
                     readDataFileUTF8,
                     fetchItem,
                     fetchItem',
                     openURL,
                     collapseFilePath,
                     -- * Error handling
                     err,
                     warn,
                     mapLeft,
                     hush,
                     -- * Safe read
                     safeRead,
                     -- * Temp directory
                     withTempDir,
                     -- * Version
                     pandocVersion
                    ) where

import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.MediaBag (MediaBag, lookupMedia)
import Text.Pandoc.Builder (Inlines, Blocks, ToMetaValue(..))
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.UTF8 as UTF8
import System.Environment (getProgName)
import System.Exit (exitWith, ExitCode(..))
import Data.Char ( toLower, isLower, isUpper, isAlpha,
                   isLetter, isDigit, isSpace )
import Data.List ( find, stripPrefix, intercalate )
import Data.Version ( showVersion )
import qualified Data.Map as M
import Network.URI ( escapeURIString, nonStrictRelativeTo,
                     unEscapeString, parseURIReference, isAllowedInURI,
                     parseURI, URI(..) )
import qualified Data.Set as Set
import System.Directory
import System.FilePath (splitDirectories, isPathSeparator)
import qualified System.FilePath.Posix as Posix
import Text.Pandoc.MIME (MimeType, getMimeType)
import System.FilePath ( (</>), takeExtension, dropExtension)
import Data.Generics (Typeable, Data)
import qualified Control.Monad.State as S
import qualified Control.Exception as E
import Control.Monad (msum, unless, MonadPlus(..))
import Text.Pandoc.Pretty (charWidth)
import Text.Pandoc.Compat.Time
import Data.Time.Clock.POSIX
import System.IO (stderr)
import System.IO.Temp
import Text.HTML.TagSoup (renderTagsOptions, RenderOptions(..), Tag(..),
         renderOptions)
import Text.Pandoc.Compat.Monoid ((<>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Base64 (decodeLenient)
import Data.Sequence (ViewR(..), ViewL(..), viewl, viewr)
import qualified Data.Text as T (toUpper, pack, unpack)
import Data.ByteString.Lazy (toChunks, fromChunks)
import qualified Data.ByteString.Lazy as BL
import Paths_pandoc (version)

import Codec.Archive.Zip

#ifdef EMBED_DATA_FILES
import Text.Pandoc.Data (dataFiles)
#else
import Paths_pandoc (getDataFileName)
#endif
#ifdef HTTP_CLIENT
import Network.HTTP.Client (httpLbs, parseUrl,
                            responseBody, responseHeaders,
                            Request(port,host))
#if MIN_VERSION_http_client(0,4,18)
import Network.HTTP.Client (newManager)
#else
import Network.HTTP.Client (withManager)
#endif
import Network.HTTP.Client.Internal (addProxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnv)
import Network.HTTP.Types.Header ( hContentType)
import Network (withSocketsDo)
#else
import Network.URI (parseURI)
import Network.HTTP (findHeader, rspBody,
                     RequestMethod(..), HeaderName(..), mkRequest)
import Network.Browser (browse, setAllowRedirects, setOutHandler, request)
#endif

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
              _ | x == 9    -> "IX"
              _ | x >= 5    -> "V" ++ toRomanNumeral (x - 5)
              _ | x == 4    -> "IV"
              _ | x >= 1    -> "I" ++ toRomanNumeral (x - 1)
              _             -> ""

-- | Escape whitespace and some punctuation characters in URI.
escapeURI :: String -> String
escapeURI = escapeURIString (not . needsEscaping)
  where needsEscaping c = isSpace c || c `elem`
                           ['<','>','|','"','{','}','[',']','^', '`']


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
   where parsetimeWith =
#if MIN_VERSION_time(1,5,0)
             parseTimeM True defaultTimeLocale
#else
             parseTime defaultTimeLocale
#endif
         formats = ["%x","%m/%d/%Y", "%D","%F", "%d %b %Y",
                    "%d %B %Y", "%b. %d, %Y", "%B %d, %Y", "%Y"]

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

-- | Normalize @Pandoc@ document, consolidating doubled 'Space's,
-- combining adjacent 'Str's and 'Emph's, remove 'Null's and
-- empty elements, etc.
normalize :: Pandoc -> Pandoc
normalize (Pandoc (Meta meta) blocks) =
  Pandoc (Meta $ M.map go meta) (normalizeBlocks blocks)
  where go (MetaInlines xs) = MetaInlines $ normalizeInlines xs
        go (MetaBlocks xs)  = MetaBlocks  $ normalizeBlocks xs
        go (MetaList ms)    = MetaList $ map go ms
        go (MetaMap m)      = MetaMap $ M.map go m
        go x                = x

normalizeBlocks :: [Block] -> [Block]
normalizeBlocks (Null : xs) = normalizeBlocks xs
normalizeBlocks (Div attr bs : xs) =
  Div attr (normalizeBlocks bs) : normalizeBlocks xs
normalizeBlocks (BlockQuote bs : xs) =
  case normalizeBlocks bs of
       []    -> normalizeBlocks xs
       bs'   -> BlockQuote bs' : normalizeBlocks xs
normalizeBlocks (BulletList [] : xs) = normalizeBlocks xs
normalizeBlocks (BulletList items : xs) =
  BulletList (map normalizeBlocks items) : normalizeBlocks xs
normalizeBlocks (OrderedList _ [] : xs) = normalizeBlocks xs
normalizeBlocks (OrderedList attr items : xs) =
  OrderedList attr (map normalizeBlocks items) : normalizeBlocks xs
normalizeBlocks (DefinitionList [] : xs) = normalizeBlocks xs
normalizeBlocks (DefinitionList items : xs) =
  DefinitionList (map go items) : normalizeBlocks xs
  where go (ils, bs) = (normalizeInlines ils, map normalizeBlocks bs)
normalizeBlocks (RawBlock _ "" : xs) = normalizeBlocks xs
normalizeBlocks (RawBlock f x : xs) =
   case normalizeBlocks xs of
        (RawBlock f' x' : rest) | f' == f ->
          RawBlock f (x ++ ('\n':x')) : rest
        rest -> RawBlock f x : rest
normalizeBlocks (Para ils : xs) =
  case normalizeInlines ils of
       []   -> normalizeBlocks xs
       ils' -> Para ils' : normalizeBlocks xs
normalizeBlocks (Plain ils : xs) =
  case normalizeInlines ils of
       []   -> normalizeBlocks xs
       ils' -> Plain ils' : normalizeBlocks xs
normalizeBlocks (Header lev attr ils : xs) =
  Header lev attr (normalizeInlines ils) : normalizeBlocks xs
normalizeBlocks (Table capt aligns widths hdrs rows : xs) =
  Table (normalizeInlines capt) aligns widths
    (map normalizeBlocks hdrs) (map (map normalizeBlocks) rows)
  : normalizeBlocks xs
normalizeBlocks (x:xs) = x : normalizeBlocks xs
normalizeBlocks [] = []

normalizeInlines :: [Inline] -> [Inline]
normalizeInlines (Str x : ys) =
  case concat (x : map fromStr strs) of
        ""     -> rest
        n      -> Str n : rest
   where
     (strs, rest)  = span isStr $ normalizeInlines ys
     isStr (Str _) = True
     isStr _       = False
     fromStr (Str z) = z
     fromStr _       = error "normalizeInlines - fromStr - not a Str"
normalizeInlines (Space : SoftBreak : ys) =
  SoftBreak : normalizeInlines ys
normalizeInlines (Space : ys) =
  if null rest
     then []
     else Space : rest
   where isSp Space = True
         isSp _     = False
         rest       = dropWhile isSp $ normalizeInlines ys
normalizeInlines (Emph xs : zs) =
  case normalizeInlines zs of
       (Emph ys : rest) -> normalizeInlines $
         Emph (normalizeInlines $ xs ++ ys) : rest
       rest -> case normalizeInlines xs of
                    []  -> rest
                    xs' -> Emph xs' : rest
normalizeInlines (Strong xs : zs) =
  case normalizeInlines zs of
       (Strong ys : rest) -> normalizeInlines $
         Strong (normalizeInlines $ xs ++ ys) : rest
       rest -> case normalizeInlines xs of
                    []  -> rest
                    xs' -> Strong xs' : rest
normalizeInlines (Subscript xs : zs) =
  case normalizeInlines zs of
       (Subscript ys : rest) -> normalizeInlines $
         Subscript (normalizeInlines $ xs ++ ys) : rest
       rest -> case normalizeInlines xs of
                    []  -> rest
                    xs' -> Subscript xs' : rest
normalizeInlines (Superscript xs : zs) =
  case normalizeInlines zs of
       (Superscript ys : rest) -> normalizeInlines $
         Superscript (normalizeInlines $ xs ++ ys) : rest
       rest -> case normalizeInlines xs of
                    []  -> rest
                    xs' -> Superscript xs' : rest
normalizeInlines (SmallCaps xs : zs) =
  case normalizeInlines zs of
       (SmallCaps ys : rest) -> normalizeInlines $
         SmallCaps (normalizeInlines $ xs ++ ys) : rest
       rest -> case normalizeInlines xs of
                    []  -> rest
                    xs' -> SmallCaps xs' : rest
normalizeInlines (Strikeout xs : zs) =
  case normalizeInlines zs of
       (Strikeout ys : rest) -> normalizeInlines $
         Strikeout (normalizeInlines $ xs ++ ys) : rest
       rest -> case normalizeInlines xs of
                    []  -> rest
                    xs' -> Strikeout xs' : rest
normalizeInlines (RawInline _ [] : ys) = normalizeInlines ys
normalizeInlines (RawInline f xs : zs) =
  case normalizeInlines zs of
       (RawInline f' ys : rest) | f == f' -> normalizeInlines $
         RawInline f (xs ++ ys) : rest
       rest -> RawInline f xs : rest
normalizeInlines (Code _ "" : ys) = normalizeInlines ys
normalizeInlines (Code attr xs : zs) =
  case normalizeInlines zs of
       (Code attr' ys : rest) | attr == attr' -> normalizeInlines $
         Code attr (xs ++ ys) : rest
       rest -> Code attr xs : rest
-- allow empty spans, they may carry identifiers etc.
-- normalizeInlines (Span _ [] : ys) = normalizeInlines ys
normalizeInlines (Span attr xs : zs) =
  case normalizeInlines zs of
       (Span attr' ys : rest) | attr == attr' -> normalizeInlines $
         Span attr (normalizeInlines $ xs ++ ys) : rest
       rest -> Span attr (normalizeInlines xs) : rest
normalizeInlines (Note bs : ys) = Note (normalizeBlocks bs) :
  normalizeInlines ys
normalizeInlines (Quoted qt ils : ys) =
  Quoted qt (normalizeInlines ils) : normalizeInlines ys
normalizeInlines (Link attr ils t : ys) =
  Link attr (normalizeInlines ils) t : normalizeInlines ys
normalizeInlines (Image attr ils t : ys) =
  Image attr (normalizeInlines ils) t : normalizeInlines ys
normalizeInlines (Cite cs ils : ys) =
  Cite cs (normalizeInlines ils) : normalizeInlines ys
normalizeInlines (x : xs) = x : normalizeInlines xs
normalizeInlines [] = []

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
        deNote (Note _) = Str ""
        deNote x = x

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

-- | Like @compactify'@, but acts on items of definition lists.
compactify'DL :: [(Inlines, [Blocks])] -> [(Inlines, [Blocks])]
compactify'DL items =
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
    Nothing       -> err 97 $ "Could not find data file " ++ fname
    Just contents -> return contents
  where makeCanonical = Posix.joinPath . transformPathParts . splitDirectories
        transformPathParts = reverse . foldl go []
        go as     "."  = as
        go (_:as) ".." = as
        go as     x    = x : as
#else
  getDataFileName fname' >>= checkExistence >>= BS.readFile
    where fname' = if fname == "README" then fname else "data" </> fname

checkExistence :: FilePath -> IO FilePath
checkExistence fn = do
  exists <- doesFileExist fn
  if exists
     then return fn
     else err 97 ("Could not find data file " ++ fn)
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

-- | Specialized version of parseURIReference that disallows
-- single-letter schemes.  Reason:  these are usually windows absolute
-- paths.
parseURIReference' :: String -> Maybe URI
parseURIReference' s =
  case parseURIReference s of
       Just u
         | length (uriScheme u) > 2  -> Just u
         | null (uriScheme u)        -> Just u  -- protocol-relative
       _                             -> Nothing

-- | Fetch an image or other item from the local filesystem or the net.
-- Returns raw content and maybe mime type.
fetchItem :: Maybe String -> String
          -> IO (Either E.SomeException (BS.ByteString, Maybe MimeType))
fetchItem sourceURL s =
  case (sourceURL >>= parseURIReference' . ensureEscaped, ensureEscaped s) of
       (Just u, s') -> -- try fetching from relative path at source
          case parseURIReference' s' of
               Just u' -> openURL $ show $ u' `nonStrictRelativeTo` u
               Nothing -> openURL s' -- will throw error
       (Nothing, s') ->
          case parseURI s' of  -- requires absolute URI
               -- We don't want to treat C:/ as a scheme:
               Just u' | length (uriScheme u') > 2 -> openURL (show u')
               Just u' | uriScheme u' == "file:" ->
                    E.try $ readLocalFile $ dropWhile (=='/') (uriPath u')
               _ -> E.try $ readLocalFile fp -- get from local file system
  where readLocalFile f = do
          cont <- BS.readFile f
          return (cont, mime)
        dropFragmentAndQuery = takeWhile (\c -> c /= '?' && c /= '#')
        fp = unEscapeString $ dropFragmentAndQuery s
        mime = case takeExtension fp of
                    ".gz" -> getMimeType $ dropExtension fp
                    ".svgz" -> getMimeType $ dropExtension fp ++ ".svg"
                    x     -> getMimeType x
        ensureEscaped = escapeURIString isAllowedInURI . map convertSlash
        convertSlash '\\' = '/'
        convertSlash x    = x

-- | Like 'fetchItem', but also looks for items in a 'MediaBag'.
fetchItem' :: MediaBag -> Maybe String -> String
           -> IO (Either E.SomeException (BS.ByteString, Maybe MimeType))
fetchItem' media sourceURL s = do
  case lookupMedia s media of
       Nothing -> fetchItem sourceURL s
       Just (mime, bs) -> return $ Right (BS.concat $ toChunks bs, Just mime)

-- | Read from a URL and return raw data and maybe mime type.
openURL :: String -> IO (Either E.SomeException (BS.ByteString, Maybe MimeType))
openURL u
  | Just u'' <- stripPrefix "data:" u =
    let mime     = takeWhile (/=',') u''
        contents = B8.pack $ unEscapeString $ drop 1 $ dropWhile (/=',') u''
    in  return $ Right (decodeLenient contents, Just mime)
#ifdef HTTP_CLIENT
  | otherwise = withSocketsDo $ E.try $ do
     req <- parseUrl u
     (proxy :: Either E.SomeException String) <- E.try $ getEnv "http_proxy"
     let req' = case proxy of
                     Left _   -> req
                     Right pr -> case parseUrl pr of
                                      Just r  -> addProxy (host r) (port r) req
                                      Nothing -> req
#if MIN_VERSION_http_client(0,4,18)
     resp <- newManager tlsManagerSettings >>= httpLbs req'
#else
     resp <- withManager tlsManagerSettings $ httpLbs req'
#endif
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

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

hush :: Either a b -> Maybe b
hush (Left _) = Nothing
hush (Right x) = Just x

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
