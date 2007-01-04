{-
Copyright (C) 2006 John MacFarlane <jgm at berkeley dot edu>

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
   Copyright   : Copyright (C) 2006 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm at berkeley dot edu>
   Stability   : alpha
   Portability : portable

Utility functions and definitions used by the various Pandoc modules.
-}
module Text.Pandoc.Shared ( 
                     -- * Text processing
                     gsub,
                     joinWithSep,
                     tabsToSpaces,
                     backslashEscape,
                     escapePreservingRegex,
                     endsWith,
                     stripTrailingNewlines,
                     removeLeadingTrailingSpace,
                     removeLeadingSpace,
                     removeTrailingSpace,
                     stripFirstAndLast,
                     -- * Parsing
                     readWith,
                     testStringWith,
                     HeaderType (..),
                     ParserContext (..),
                     ParserState (..),
                     defaultParserState,
                     -- * Native format prettyprinting
                     prettyPandoc,
                     -- * Pandoc block list processing
                     consolidateList,
                     isNoteBlock,
                     splitBy,
                     normalizeSpaces,
                     compactify,
                     generateReference,
                     WriterOptions (..),
                     KeyTable,
                     keyTable,
                     lookupKeySrc,
                     refsMatch,
                     replaceReferenceLinks,
                     replaceRefLinksBlockList 
                    ) where
import Text.Pandoc.Definition
import Text.ParserCombinators.Parsec
import Text.Pandoc.Entities ( decodeEntities )
import Text.Regex ( matchRegexAll, mkRegex, subRegex, Regex )
import Char ( toLower )
import List ( find, groupBy )

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

data ParserState = ParserState
    { stateParseRaw        :: Bool,          -- ^ Parse untranslatable HTML 
                                             -- and LaTeX?
      stateParserContext   :: ParserContext, -- ^ What are we parsing?  
      stateKeyBlocks       :: [Block],       -- ^ List of reference key blocks
      stateKeysUsed        :: [[Inline]],    -- ^ List of references used
      stateNoteBlocks      :: [Block],       -- ^ List of note blocks
      stateNoteIdentifiers :: [String],      -- ^ List of footnote identifiers
                                             -- in the order encountered
      stateTabStop         :: Int,           -- ^ Tab stop
      stateStandalone      :: Bool,          -- ^ If @True@, parse 
                                             -- bibliographic info
      stateTitle           :: [Inline],      -- ^ Title of document
      stateAuthors         :: [String],      -- ^ Authors of document
      stateDate            :: String,        -- ^ Date of document
      stateStrict          :: Bool,          -- ^ Use strict markdown syntax
      stateHeaderTable     :: [HeaderType]   -- ^ List of header types used,
                                             -- in what order (rst only)
    }
    deriving Show

defaultParserState :: ParserState
defaultParserState = 
    ParserState { stateParseRaw        = False,
                  stateParserContext   = NullState,
                  stateKeyBlocks       = [],
                  stateKeysUsed        = [],
                  stateNoteBlocks      = [],
                  stateNoteIdentifiers = [],
                  stateTabStop         = 4,
                  stateStandalone      = False,
                  stateTitle           = [],
                  stateAuthors         = [],
                  stateDate            = [],
                  stateStrict          = False,
                  stateHeaderTable     = [] }

-- | Consolidate @Str@s and @Space@s in an inline list into one big @Str@.
-- Collapse adjacent @Space@s.
consolidateList :: [Inline] -> [Inline]
consolidateList ((Str a):(Str b):rest) = consolidateList ((Str (a ++ b)):rest)
consolidateList ((Str a):Space:rest) = consolidateList ((Str (a ++ " ")):rest)
consolidateList (Space:(Str a):rest) = consolidateList ((Str (" " ++ a)):rest)
consolidateList (Space:Space:rest) = consolidateList ((Str " "):rest)
consolidateList (inline:rest) = inline:(consolidateList rest)
consolidateList [] = [] 

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
prettyBlock (Note ref blocks) = "Note " ++ (show ref) ++ "\n  " ++ 
                                (prettyBlockList 2 blocks) 
prettyBlock (OrderedList blockLists) = 
   "OrderedList\n" ++ indentBy 2 0 ("[ " ++ (joinWithSep ", " 
   (map (\blocks -> prettyBlockList 2 blocks) blockLists))) ++ " ]"
prettyBlock (BulletList blockLists) = "BulletList\n" ++ 
   indentBy 2 0 ("[ " ++ (joinWithSep ", " 
   (map (\blocks -> prettyBlockList 2 blocks) blockLists))) ++ " ]" 
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

-- | Substitute string for every occurrence of regular expression.
gsub :: String  -- ^ Regular expression (as string) to substitute for
     -> String  -- ^ String to substitute for the regex
     -> String  -- ^ String to be substituted in
     -> String
gsub regex replacement str = subRegex (mkRegex regex) str replacement

-- | Escape designated characters with backslash.
backslashEscape :: [Char]    -- ^ list of special characters to escape
                -> String    -- ^ string input
                -> String 
backslashEscape special [] = []
backslashEscape special (x:xs) = if x `elem` special
    then '\\':x:(backslashEscape special xs)
    else x:(backslashEscape special xs)

-- | Escape string by applying a function, but don't touch anything that matches regex.
escapePreservingRegex :: (String -> String)      -- ^ Escaping function
			 -> Regex                -- ^ Regular expression
			 -> String               -- ^ String to be escaped
			 -> String
escapePreservingRegex escapeFunction regex str =
    case (matchRegexAll regex str) of
      Nothing                          -> escapeFunction str
      Just (before, matched, after, _) -> (escapeFunction before) ++ 
            matched ++ (escapePreservingRegex escapeFunction regex after)

-- | Returns @True@ if string ends with given character.
endsWith :: Char -> [Char] -> Bool
endsWith char [] = False
endsWith char str = (char == last str)

-- | Returns @True@ if block is a @Note@ block
isNoteBlock :: Block -> Bool
isNoteBlock (Note ref blocks) = True
isNoteBlock _ = False

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

-- | Split list into groups separated by sep.
splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy sep lst = 
  let (first, rest) = break (== sep) lst
      rest'         = dropWhile (== sep) rest in
  first:(splitBy sep rest')

-- | Normalize a list of inline elements: remove leading and trailing
-- @Space@ elements, and collapse double @Space@s into singles.
normalizeSpaces :: [Inline] -> [Inline]
normalizeSpaces [] = []
normalizeSpaces list = 
    let removeDoubles [] = []
        removeDoubles (Space:Space:rest) = removeDoubles (Space:rest)
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

-- | Options for writers
data WriterOptions = WriterOptions
    { writerStandalone      :: Bool   -- ^ Include header and footer
    , writerTitlePrefix     :: String -- ^ Prefix for HTML titles
    , writerHeader          :: String -- ^ Header for the document
    , writerIncludeBefore   :: String -- ^ String to include before the  body
    , writerIncludeAfter    :: String -- ^ String to include after the body
    , writerSmart           :: Bool   -- ^ Use smart typography
    , writerS5              :: Bool   -- ^ We're writing S5 
    , writerIncremental     :: Bool   -- ^ Incremental S5 lists
    , writerNumberSections  :: Bool   -- ^ Number sections in LaTeX
    , writerStrictMarkdown  :: Bool   -- ^ Use strict markdown syntax
    , writerTabStop         :: Int    -- ^ Tabstop for conversion between 
                                      -- spaces and tabs
    , writerNotes           :: [Block] -- ^ List of note blocks
    } deriving Show

--
-- Functions for constructing lists of reference keys
--

-- | Returns @Just@ numerical key reference if there's already a key
-- for the specified target in the list of blocks, otherwise @Nothing@.
keyFoundIn :: [Block]        -- ^ List of key blocks to search     
           -> Target         -- ^ Target to search for
           -> Maybe String
keyFoundIn [] src = Nothing
keyFoundIn ((Key [Str num] src1):rest) src = if (src1 == src)
                                                then Just num
                                                else keyFoundIn rest src
keyFoundIn (_:rest) src = keyFoundIn rest src

-- | Return next unique numerical key, given keyList
nextUniqueKey :: [[Inline]] -> String
nextUniqueKey keys = 
  let nums = [1..10000] 
      notAKey n = not (any (== [Str (show n)]) keys) in
  case (find notAKey nums) of
    Just x  -> show x
    Nothing -> error "Could not find unique key for reference link"

-- | Generate a reference for a URL (either an existing reference, if
-- there is one, or a new one, if there isn't) and update parser state.
generateReference :: String   -- ^ URL
		  -> String   -- ^ Title
		  -> GenParser tok ParserState Target
generateReference url title = do
  let src = Src (decodeEntities url) (decodeEntities title)
  state <- getState  
  let keyBlocks = stateKeyBlocks state
  let keysUsed = stateKeysUsed state
  case (keyFoundIn keyBlocks src) of
    Just num -> return (Ref [Str num])
    Nothing  -> do 
                  let nextNum = nextUniqueKey keysUsed
                  updateState (\st -> st { stateKeyBlocks = 
                                           (Key [Str nextNum] src):keyBlocks, 
                                           stateKeysUsed = 
                                          [Str nextNum]:keysUsed })
                  return (Ref [Str nextNum])

--
-- code to replace reference links with real links and remove unneeded key blocks
--

type KeyTable = [([Inline], Target)]

-- | Returns @True@ if block is a Key block
isRefBlock :: Block -> Bool
isRefBlock (Key _ _) = True
isRefBlock _         = False

-- | Returns a pair of a list of pairs of keys and associated sources, and a new 
-- list of blocks with the included key blocks deleted.
keyTable :: [Block] -> (KeyTable, [Block])
keyTable [] = ([],[])
keyTable ((Key ref target):lst) = (((ref, target):table), rest)
                                  where (table, rest) = keyTable lst
keyTable (Null:lst) = keyTable lst  -- get rid of Nulls
keyTable (Blank:lst) = keyTable lst  -- get rid of Blanks
keyTable ((BlockQuote blocks):lst) = ((table1 ++ table2), 
                                      ((BlockQuote rest1):rest2))
                                  where (table1, rest1) = keyTable blocks
                                        (table2, rest2) = keyTable lst
keyTable ((Note ref blocks):lst) = ((table1 ++ table2), 
                                    ((Note ref rest1):rest2))
                                  where (table1, rest1) = keyTable blocks
                                        (table2, rest2) = keyTable lst
keyTable ((OrderedList blockLists):lst) = ((table1 ++ table2), 
                                           ((OrderedList rest1):rest2))
                                  where results    = map keyTable blockLists
                                        rest1      = map snd results
                                        table1     = concatMap fst results
                                        (table2, rest2) = keyTable lst
keyTable ((BulletList blockLists):lst) = ((table1 ++ table2), 
                                          ((BulletList rest1):rest2))
                                  where results    = map keyTable blockLists
                                        rest1      = map snd results
                                        table1     = concatMap fst results
                                        (table2, rest2) = keyTable lst
keyTable (other:lst) = (table, (other:rest))
                       where (table, rest) = keyTable lst

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
refsMatch ((NoteRef x):restx) ((NoteRef y):resty) = 
    ((map toLower x) == (map toLower y)) && refsMatch restx resty
refsMatch ((Emph x):restx) ((Emph y):resty) = 
    refsMatch x y && refsMatch restx resty
refsMatch ((Strong x):restx) ((Strong y):resty) = 
    refsMatch x y && refsMatch restx resty
refsMatch (x:restx) (y:resty) = (x == y) && refsMatch restx resty
refsMatch [] x = null x
refsMatch x [] = null x

-- | Replace reference links with explicit links in list of blocks,
-- removing key blocks.
replaceReferenceLinks :: [Block] -> [Block]
replaceReferenceLinks blocks =
    let (keytable, purged) = keyTable blocks in
    replaceRefLinksBlockList keytable purged

-- | Use key table to replace reference links with explicit links in a list 
-- of blocks
replaceRefLinksBlockList :: KeyTable -> [Block] -> [Block]
replaceRefLinksBlockList keytable lst = 
    map (replaceRefLinksBlock keytable) lst

-- | Use key table to replace reference links with explicit links in a block
replaceRefLinksBlock :: KeyTable -> Block -> Block
replaceRefLinksBlock keytable (Plain lst) = 
    Plain (map (replaceRefLinksInline keytable) lst)
replaceRefLinksBlock keytable (Para lst) = 
    Para (map (replaceRefLinksInline keytable) lst)
replaceRefLinksBlock keytable (Header lvl lst) = 
    Header lvl (map (replaceRefLinksInline keytable) lst)
replaceRefLinksBlock keytable (BlockQuote lst) = 
    BlockQuote (map (replaceRefLinksBlock keytable) lst)
replaceRefLinksBlock keytable (Note ref lst) = 
    Note ref (map (replaceRefLinksBlock keytable) lst)
replaceRefLinksBlock keytable (OrderedList lst) = 
    OrderedList (map (replaceRefLinksBlockList keytable) lst)
replaceRefLinksBlock keytable (BulletList lst) = 
    BulletList (map (replaceRefLinksBlockList keytable) lst)
replaceRefLinksBlock keytable other = other

-- | Use key table to replace reference links with explicit links in an 
-- inline element.
replaceRefLinksInline :: KeyTable -> Inline -> Inline
replaceRefLinksInline keytable (Link text (Ref ref)) = (Link newText newRef)
    where newRef = case lookupKeySrc keytable 
                        (if (null ref) then text else ref) of
                     Nothing  -> (Ref ref)
                     Just src -> src
          newText = map (replaceRefLinksInline keytable) text
replaceRefLinksInline keytable (Image text (Ref ref)) = (Image newText newRef)
    where newRef = case lookupKeySrc keytable 
                        (if (null ref) then text else ref) of
                     Nothing -> (Ref ref)
                     Just src -> src
          newText = map (replaceRefLinksInline keytable) text
replaceRefLinksInline keytable (Emph lst) = 
    Emph (map (replaceRefLinksInline keytable) lst)
replaceRefLinksInline keytable (Strong lst) = 
    Strong (map (replaceRefLinksInline keytable) lst)
replaceRefLinksInline keytable other = other
