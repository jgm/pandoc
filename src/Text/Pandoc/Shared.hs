{-
Copyright (C) 2006-7 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-7 John MacFarlane
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
                     joinWithSep,
                     tabsToSpaces,
                     backslashEscape,
                     endsWith,
                     stripTrailingNewlines,
                     removeLeadingTrailingSpace,
                     removeLeadingSpace,
                     removeTrailingSpace,
                     stripFirstAndLast,
                     -- * Parsing
                     readWith,
                     testStringWith,
                     Reference (..),
                     isNoteBlock,
                     isKeyBlock,
                     isLineClump,
                     HeaderType (..),
                     ParserContext (..),
                     QuoteContext (..),
                     ParserState (..),
                     defaultParserState,
                     nullBlock,
                     escaped,
                     -- * Native format prettyprinting
                     prettyPandoc,
                     -- * Pandoc block and inline list processing
                     normalizeSpaces,
                     compactify,
                     Element (..),
                     hierarchicalize,
                     isHeaderBlock,
                     -- * Writer options
                     WriterOptions (..),
                     defaultWriterOptions,
                     -- * Reference key lookup functions
                     KeyTable,
                     lookupKeySrc,
                     refsMatch,
                    ) where
import Text.Pandoc.Definition
import Text.ParserCombinators.Parsec
import Text.Pandoc.Entities ( decodeEntities, escapeStringForXML )
import Data.Char ( toLower, ord )
import Data.List ( find, groupBy, isPrefixOf )

-- | Parse a string with a given parser and state.
readWith :: GenParser Char ParserState a      -- ^ parser
            -> ParserState                    -- ^ initial state
            -> String                         -- ^ input string
            -> a
readWith parser state input = 
    case runParser parser state "source" input of
      Left err     -> error $ "\nError:\n" ++ show err
      Right result -> result

-- | Parse a string with @parser@ (for testing).
testStringWith :: (Show a) =>
                  GenParser Char ParserState a
                  -> String
                  -> IO ()
testStringWith parser str = putStrLn $ show $ 
                            readWith parser defaultParserState str

data HeaderType 
    = SingleHeader Char  -- ^ Single line of characters underneath
    | DoubleHeader Char  -- ^ Lines of characters above and below
    deriving (Eq, Show)

data ParserContext 
    = ListItemState   -- ^ Used when running parser on list item contents
    | NullState       -- ^ Default state
    deriving (Eq, Show)

data QuoteContext
    = InSingleQuote   -- ^ Used when we're parsing inside single quotes
    | InDoubleQuote   -- ^ Used when we're parsing inside double quotes
    | NoQuote         -- ^ Used when we're not parsing inside quotes
    deriving (Eq, Show)

type KeyTable = [([Inline], Target)]

type NoteTable = [(String, [Block])]

-- | References from preliminary parsing
data Reference
  = KeyBlock [Inline] Target  -- ^ Key for reference-style link (label URL title)
  | NoteBlock String [Block]  -- ^ Footnote reference and contents
  | LineClump String          -- ^ Raw clump of lines with blanks at end
  deriving (Eq, Read, Show)

-- | Auxiliary functions used in preliminary parsing
isNoteBlock :: Reference -> Bool
isNoteBlock (NoteBlock _ _) = True
isNoteBlock _ = False

isKeyBlock :: Reference -> Bool
isKeyBlock (KeyBlock _ _) = True
isKeyBlock _ = False

isLineClump :: Reference -> Bool
isLineClump (LineClump _) = True
isLineClump _ = False

data ParserState = ParserState
    { stateParseRaw        :: Bool,          -- ^ Parse untranslatable HTML 
                                             -- and LaTeX?
      stateParserContext   :: ParserContext, -- ^ What are we parsing?  
      stateQuoteContext    :: QuoteContext,  -- ^ Inside quoted environment?
      stateKeys            :: KeyTable,      -- ^ List of reference keys
      stateNotes           :: NoteTable,     -- ^ List of notes
      stateTabStop         :: Int,           -- ^ Tab stop
      stateStandalone      :: Bool,          -- ^ If @True@, parse 
                                             -- bibliographic info
      stateTitle           :: [Inline],      -- ^ Title of document
      stateAuthors         :: [String],      -- ^ Authors of document
      stateDate            :: String,        -- ^ Date of document
      stateStrict          :: Bool,          -- ^ Use strict markdown syntax
      stateSmart           :: Bool,          -- ^ Use smart typography
      stateColumns         :: Int,           -- ^ Number of columns in
                                             -- terminal (used for tables)
      stateHeaderTable     :: [HeaderType]   -- ^ List of header types used,
                                             -- in what order (rst only)
    }
    deriving Show

defaultParserState :: ParserState
defaultParserState = 
    ParserState { stateParseRaw        = False,
                  stateParserContext   = NullState,
                  stateQuoteContext    = NoQuote,
                  stateKeys            = [],
                  stateNotes           = [],
                  stateTabStop         = 4,
                  stateStandalone      = False,
                  stateTitle           = [],
                  stateAuthors         = [],
                  stateDate            = [],
                  stateStrict          = False,
                  stateSmart           = False,
                  stateColumns         = 80,
                  stateHeaderTable     = [] }

-- | Parses a character and returns 'Null' (so that the parser can move on
-- if it gets stuck).
nullBlock :: GenParser Char st Block
nullBlock = do
  anyChar 
  return Null

-- | Parses backslash, then applies character parser.
escaped :: GenParser Char st Char  -- ^ Parser for character to escape
        -> GenParser Char st Inline
escaped parser = try (do
                        char '\\'
                        result <- parser
                        return (Str [result]))

-- | Indent string as a block.
indentBy :: Int    -- ^ Number of spaces to indent the block 
         -> Int    -- ^ Number of spaces (rel to block) to indent first line
         -> String -- ^ Contents of block to indent
         -> String
indentBy num first [] = ""
indentBy num first str = 
    let (firstLine:restLines) = lines str 
        firstLineIndent = num + first in
    (replicate firstLineIndent ' ') ++ firstLine ++ "\n" ++ (joinWithSep "\n" $ map (\line -> (replicate num ' ') ++ line) restLines)

-- | Prettyprint list of Pandoc blocks elements.
prettyBlockList :: Int       -- ^ Number of spaces to indent list of blocks
                -> [Block]   -- ^ List of blocks
                -> String
prettyBlockList indent [] = indentBy indent 0 "[]"
prettyBlockList indent blocks = indentBy indent (-2) $ "[ " ++ 
                        (joinWithSep "\n, " (map prettyBlock blocks)) ++ " ]"

-- | Prettyprint Pandoc block element.
prettyBlock :: Block -> String
prettyBlock (BlockQuote blocks) = "BlockQuote\n  " ++ 
                                  (prettyBlockList 2 blocks) 
prettyBlock (OrderedList blockLists) = 
   "OrderedList\n" ++ indentBy 2 0 ("[ " ++ (joinWithSep ", " 
   (map (\blocks -> prettyBlockList 2 blocks) blockLists))) ++ " ]"
prettyBlock (BulletList blockLists) = "BulletList\n" ++ 
   indentBy 2 0 ("[ " ++ (joinWithSep ", " 
   (map (\blocks -> prettyBlockList 2 blocks) blockLists))) ++ " ]" 
prettyBlock (DefinitionList blockLists) = "DefinitionList\n" ++ 
   indentBy 2 0 ("[" ++ (joinWithSep ",\n" 
   (map (\(term, blocks) -> "  (" ++ show term ++ ",\n" ++ 
   indentBy 1 2 (prettyBlockList 2 blocks) ++ "  )") blockLists))) ++ " ]" 
prettyBlock (Table caption aligns widths header rows) = 
   "Table " ++ show caption ++ " " ++ show aligns ++ " " ++ 
   show widths ++ "\n" ++ prettyRow header ++ " [\n" ++  
   (joinWithSep ",\n" (map prettyRow rows)) ++ " ]"
   where prettyRow cols = indentBy 2 0 ("[ " ++ (joinWithSep ", "
                          (map (\blocks -> prettyBlockList 2 blocks) 
                          cols))) ++ " ]"
prettyBlock block = show block

-- | Prettyprint Pandoc document.
prettyPandoc :: Pandoc -> String
prettyPandoc (Pandoc meta blocks) = "Pandoc " ++ "(" ++ (show meta) ++ 
    ")\n" ++ (prettyBlockList 0 blocks) ++ "\n"

-- | Convert tabs to spaces (with adjustable tab stop).
tabsToSpaces :: Int     -- ^ Tabstop
             -> String  -- ^ String to convert
             -> String
tabsToSpaces tabstop str =
    unlines (map (tabsInLine tabstop tabstop) (lines str))

-- | Convert tabs to spaces in one line.
tabsInLine :: Int      -- ^ Number of spaces to next tab stop
           -> Int      -- ^ Tabstop
           -> String   -- ^ Line to convert
           -> String
tabsInLine num tabstop "" = ""
tabsInLine num tabstop (c:cs) = 
    let replacement = (if (c == '\t') then (replicate num ' ') else [c]) in
    let nextnumraw = (num - (length replacement)) in
    let nextnum = if (nextnumraw < 1)
                     then (nextnumraw + tabstop)
                     else nextnumraw in
    replacement ++ (tabsInLine nextnum tabstop cs)

-- | Escape designated characters with backslash.
backslashEscape :: [Char]    -- ^ list of special characters to escape
                -> String    -- ^ string input
                -> String 
backslashEscape special [] = []
backslashEscape special (x:xs) = if x `elem` special
    then '\\':x:(backslashEscape special xs)
    else x:(backslashEscape special xs)

-- | Returns @True@ if string ends with given character.
endsWith :: Char -> [Char] -> Bool
endsWith char [] = False
endsWith char str = (char == last str)

-- | Joins a list of lists, separated by another list.
joinWithSep :: [a]    -- ^ List to use as separator
            -> [[a]]  -- ^ Lists to join
            -> [a]
joinWithSep sep [] = []
joinWithSep sep lst = foldr1 (\a b -> a ++ sep ++ b) lst

-- | Strip trailing newlines from string.
stripTrailingNewlines :: String -> String
stripTrailingNewlines "" = ""
stripTrailingNewlines str = 
    if (last str) == '\n'
       then stripTrailingNewlines (init str)
       else str

-- | Remove leading and trailing space (including newlines) from string.
removeLeadingTrailingSpace :: String -> String
removeLeadingTrailingSpace = removeLeadingSpace . removeTrailingSpace

-- | Remove leading space (including newlines) from string.
removeLeadingSpace :: String -> String
removeLeadingSpace = dropWhile (\x -> (x == ' ') || (x == '\n') || 
                                (x == '\t')) 

-- | Remove trailing space (including newlines) from string.
removeTrailingSpace :: String -> String
removeTrailingSpace = reverse . removeLeadingSpace . reverse

-- | Strip leading and trailing characters from string
stripFirstAndLast str =
  drop 1 $ take ((length str) - 1) str

-- | Replace each occurrence of one sublist in a list with another.
substitute :: (Eq a) => [a] -> [a] -> [a] -> [a]
substitute _ _ [] = []
substitute [] _ lst = lst
substitute target replacement lst = 
    if isPrefixOf target lst
       then replacement ++ (substitute target replacement $ drop (length target) lst)
       else (head lst):(substitute target replacement $ tail lst)

-- | Split list into groups separated by sep.
splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy sep lst = 
  let (first, rest) = break (== sep) lst
      rest'         = dropWhile (== sep) rest in
  first:(splitBy sep rest')

-- | Split list into chunks divided at specified indices.
splitByIndices :: [Int] -> [a] -> [[a]]
splitByIndices [] lst = [lst]
splitByIndices (x:xs) lst =
    let (first, rest) = splitAt x lst in
    first:(splitByIndices (map (\y -> y - x)  xs) rest)

-- | Normalize a list of inline elements: remove leading and trailing
-- @Space@ elements, collapse double @Space@s into singles, and
-- remove empty Str elements.
normalizeSpaces :: [Inline] -> [Inline]
normalizeSpaces [] = []
normalizeSpaces list = 
    let removeDoubles [] = []
        removeDoubles (Space:Space:rest) = removeDoubles (Space:rest)
        removeDoubles ((Str ""):rest) = removeDoubles rest 
        removeDoubles (x:rest) = x:(removeDoubles rest) in
    let removeLeading [] = []
        removeLeading lst = if ((head lst) == Space)
                               then tail lst
                               else lst in
    let removeTrailing [] = []
        removeTrailing lst = if ((last lst) == Space)
                               then init lst
                               else lst in
    removeLeading $ removeTrailing $ removeDoubles list

-- | Change final list item from @Para@ to @Plain@ if the list should 
-- be compact.
compactify :: [[Block]]  -- ^ List of list items (each a list of blocks)
           -> [[Block]]
compactify [] = []
compactify items =
    let final = last items
        others = init items in
    case final of
      [Para a]  -> if any containsPara others
                      then items
                      else others ++ [[Plain a]]
      otherwise -> items

containsPara :: [Block] -> Bool
containsPara [] = False
containsPara ((Para a):rest) = True
containsPara ((BulletList items):rest) =  (any containsPara items) ||
                                          (containsPara rest)
containsPara ((OrderedList items):rest) = (any containsPara items) ||
                                          (containsPara rest)
containsPara (x:rest) = containsPara rest

-- | Data structure for defining hierarchical Pandoc documents
data Element = Blk Block 
             | Sec [Inline] [Element] deriving (Eq, Read, Show)

-- | Returns true on Header block with level at least 'level'
headerAtLeast :: Int -> Block -> Bool
headerAtLeast level (Header x _) = x <= level
headerAtLeast level _ = False

-- | Convert list of Pandoc blocks into list of Elements (hierarchical) 
hierarchicalize :: [Block] -> [Element]
hierarchicalize [] = []
hierarchicalize (block:rest) = 
  case block of
    (Header level title) -> let (thisSection, rest') = break (headerAtLeast 
                                                       level) rest in
                            (Sec title (hierarchicalize thisSection)):
                            (hierarchicalize rest') 
    x                    -> (Blk x):(hierarchicalize rest)

-- | True if block is a Header block.
isHeaderBlock :: Block -> Bool
isHeaderBlock (Header _ _) = True
isHeaderBlock _ = False

-- | Options for writers
data WriterOptions = WriterOptions
    { writerStandalone      :: Bool   -- ^ Include header and footer
    , writerTitlePrefix     :: String -- ^ Prefix for HTML titles
    , writerHeader          :: String -- ^ Header for the document
    , writerIncludeBefore   :: String -- ^ String to include before the  body
    , writerIncludeAfter    :: String -- ^ String to include after the body
    , writerTableOfContents :: Bool   -- ^ Include table of contents
    , writerS5              :: Bool   -- ^ We're writing S5 
    , writerIgnoreNotes     :: Bool   -- ^ Ignore footnotes (used in making toc)
    , writerIncremental     :: Bool   -- ^ Incremental S5 lists
    , writerNumberSections  :: Bool   -- ^ Number sections in LaTeX
    , writerStrictMarkdown  :: Bool   -- ^ Use strict markdown syntax
    , writerReferenceLinks  :: Bool   -- ^ Use reference links in writing markdown, rst
    , writerTabStop         :: Int    -- ^ Tabstop for conversion between 
                                      -- spaces and tabs
    } deriving Show

-- | Default writer options.
defaultWriterOptions = 
    WriterOptions { writerStandalone      = True,
                    writerHeader          = "",
                    writerTitlePrefix     = "",
                    writerTabStop         = 4,
                    writerTableOfContents = False,
                    writerS5              = False,
                    writerIgnoreNotes     = False,
                    writerIncremental     = False,
                    writerNumberSections  = False,
                    writerIncludeBefore   = "",
                    writerIncludeAfter    = "",
                    writerStrictMarkdown  = False,
                    writerReferenceLinks  = False }

--
-- code to lookup reference keys in key table
--

-- | Look up key in key table and return target object.
lookupKeySrc :: KeyTable  -- ^ Key table
             -> [Inline]  -- ^ Key
             -> Maybe Target
lookupKeySrc table key = case table of
                           []            -> Nothing
                           (k, src):rest -> if (refsMatch k key) 
                                               then Just src
                                               else lookupKeySrc rest key

-- | Returns @True@ if keys match (case insensitive).
refsMatch :: [Inline] -> [Inline] -> Bool
refsMatch ((Str x):restx) ((Str y):resty) = 
    ((map toLower x) == (map toLower y)) && refsMatch restx resty
refsMatch ((Code x):restx) ((Code y):resty) = 
    ((map toLower x) == (map toLower y)) && refsMatch restx resty
refsMatch ((TeX x):restx) ((TeX y):resty) = 
    ((map toLower x) == (map toLower y)) && refsMatch restx resty
refsMatch ((HtmlInline x):restx) ((HtmlInline y):resty) = 
    ((map toLower x) == (map toLower y)) && refsMatch restx resty
refsMatch ((Emph x):restx) ((Emph y):resty) = 
    refsMatch x y && refsMatch restx resty
refsMatch ((Strong x):restx) ((Strong y):resty) = 
    refsMatch x y && refsMatch restx resty
refsMatch ((Quoted t x):restx) ((Quoted u y):resty) = 
    t == u && refsMatch x y && refsMatch restx resty
refsMatch (x:restx) (y:resty) = (x == y) && refsMatch restx resty
refsMatch [] x = null x
refsMatch x [] = null x


