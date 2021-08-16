{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{- |
   Module      : Text.Pandoc.Shared
   Copyright   : Copyright (C) 2006-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Utility functions and definitions used by the various Pandoc modules.
-}
module Text.Pandoc.Shared (
                     -- * List processing
                     splitBy,
                     splitTextBy,
                     splitTextByIndices,
                     ordNub,
                     findM,
                     -- * Text processing
                     tshow,
                     elemText,
                     notElemText,
                     stripTrailingNewlines,
                     trim,
                     triml,
                     trimr,
                     trimMath,
                     stripFirstAndLast,
                     camelCaseToHyphenated,
                     camelCaseStrToHyphenated,
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
                     deLink,
                     stringify,
                     capitalize,
                     compactify,
                     compactifyDL,
                     linesToPara,
                     makeSections,
                     uniqueIdent,
                     inlineListToIdentifier,
                     isHeaderBlock,
                     headerShift,
                     stripEmptyParagraphs,
                     onlySimpleTableCells,
                     isTightList,
                     taskListItemFromAscii,
                     taskListItemToAscii,
                     handleTaskListItem,
                     addMetaField,
                     makeMeta,
                     eastAsianLineBreakFilter,
                     htmlSpanLikeElements,
                     splitSentences,
                     filterIpynbOutput,
                     -- * TagSoup HTML handling
                     renderTags',
                     -- * File handling
                     inDirectory,
                     collapseFilePath,
                     uriPathToPath,
                     filteredFilesFromArchive,
                     -- * URI handling
                     schemes,
                     isURI,
                     -- * Error handling
                     mapLeft,
                     -- * for squashing blocks
                     blocksToInlines,
                     blocksToInlines',
                     blocksToInlinesWithSep,
                     defaultBlocksSeparator,
                     -- * Safe read
                     safeRead,
                     safeStrRead,
                     -- * User data directory
                     defaultUserDataDir,
                     -- * Version
                     pandocVersion
                    ) where

import Codec.Archive.Zip
import qualified Control.Exception as E
import Control.Monad (MonadPlus (..), msum, unless)
import qualified Control.Monad.State.Strict as S
import qualified Data.ByteString.Lazy as BL
import qualified Data.Bifunctor as Bifunctor
import Data.Char (isAlpha, isLower, isSpace, isUpper, toLower, isAlphaNum,
                  generalCategory, GeneralCategory(NonSpacingMark,
                  SpacingCombiningMark, EnclosingMark, ConnectorPunctuation))
import Data.List (find, intercalate, intersperse, sortOn, foldl')
import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid (Any (..))
import Data.Sequence (ViewL (..), ViewR (..), viewl, viewr)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Version (showVersion)
import Network.URI (URI (uriScheme), escapeURIString, parseURI)
import Paths_pandoc (version)
import System.Directory
import System.FilePath (isPathSeparator, splitDirectories)
import qualified System.FilePath.Posix as Posix
import Text.HTML.TagSoup (RenderOptions (..), Tag (..), renderOptions,
                          renderTagsOptions)
import Text.Pandoc.Builder (Blocks, Inlines, ToMetaValue (..))
import qualified Text.Pandoc.Builder as B
import Data.Time
import Text.Pandoc.Asciify (toAsciiText)
import Text.Pandoc.Definition
import Text.Pandoc.Extensions (Extensions, Extension(..), extensionEnabled)
import Text.Pandoc.Generic (bottomUp)
import Text.DocLayout (charWidth)
import Text.Pandoc.Walk

-- | Version number of pandoc library.
pandocVersion :: T.Text
pandocVersion = T.pack $ showVersion version

--
-- List processing
--

-- | Split list by groups of one or more sep.
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy isSep lst =
  let (first, rest) = break isSep lst
  in  first:splitBy isSep (dropWhile isSep rest)

-- | Split text by groups of one or more separator.
splitTextBy :: (Char -> Bool) -> T.Text -> [T.Text]
splitTextBy isSep t
  | T.null t = []
  | otherwise = let (first, rest) = T.break isSep t
                in  first : splitTextBy isSep (T.dropWhile isSep rest)

splitTextByIndices :: [Int] -> T.Text -> [T.Text]
splitTextByIndices ns = splitTextByRelIndices (zipWith (-) ns (0:ns)) . T.unpack
 where
  splitTextByRelIndices [] cs = [T.pack cs]
  splitTextByRelIndices (x:xs) cs =
    let (first, rest) = splitAt' x cs
     in T.pack first : splitTextByRelIndices xs rest

-- Note: don't replace this with T.splitAt, which is not sensitive
-- to character widths!
splitAt' :: Int -> [Char] -> ([Char],[Char])
splitAt' _ []          = ([],[])
splitAt' n xs | n <= 0 = ([],xs)
splitAt' n (x:xs)      = (x:ys,zs)
  where (ys,zs) = splitAt' (n - charWidth x) xs

ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

findM :: forall m t a. (Monad m, Foldable t) => (a -> m Bool) -> t a -> m (Maybe a)
findM p = foldr go (pure Nothing)
  where
    go :: a -> m (Maybe a) -> m (Maybe a)
    go x acc = do
      b <- p x
      if b then pure (Just x) else acc

--
-- Text processing
--

tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- | @True@ exactly when the @Char@ appears in the @Text@.
elemText :: Char -> T.Text -> Bool
elemText c = T.any (== c)

-- | @True@ exactly when the @Char@ does not appear in the @Text@.
notElemText :: Char -> T.Text -> Bool
notElemText c = T.all (/= c)

-- | Strip trailing newlines from string.
stripTrailingNewlines :: T.Text -> T.Text
stripTrailingNewlines = T.dropWhileEnd (== '\n')

isWS :: Char -> Bool
isWS ' '  = True
isWS '\r' = True
isWS '\n' = True
isWS '\t' = True
isWS _    = False

-- | Remove leading and trailing space (including newlines) from string.
trim :: T.Text -> T.Text
trim = T.dropAround isWS

-- | Remove leading space (including newlines) from string.
triml :: T.Text -> T.Text
triml = T.dropWhile isWS

-- | Remove trailing space (including newlines) from string.
trimr :: T.Text -> T.Text
trimr = T.dropWhileEnd isWS

-- | Trim leading space and trailing space unless after \.
trimMath :: T.Text -> T.Text
trimMath = triml . T.reverse . stripBeginSpace . T.reverse -- no Text.spanEnd
  where
    stripBeginSpace t
      | T.null pref = t
      | Just ('\\', _) <- T.uncons suff = T.cons (T.last pref) suff
      | otherwise = suff
      where
        (pref, suff) = T.span isWS t

-- | Strip leading and trailing characters from string
stripFirstAndLast :: T.Text -> T.Text
stripFirstAndLast t = case T.uncons t of
  Just (_, t') -> case T.unsnoc t' of
    Just (t'', _) -> t''
    _             -> t'
  _               -> ""

-- | Change CamelCase word to hyphenated lowercase (e.g., camel-case).
camelCaseToHyphenated :: T.Text -> T.Text
camelCaseToHyphenated = T.pack . camelCaseStrToHyphenated . T.unpack

-- This may not work as expected on general Unicode, if it contains
-- letters with a longer lower case form than upper case. I don't know
-- what the camel case practices of affected scripts are, though.
camelCaseStrToHyphenated :: String -> String
camelCaseStrToHyphenated [] = ""
camelCaseStrToHyphenated (a:b:rest)
  | isLower a
  , isUpper b = a:'-':toLower b:camelCaseStrToHyphenated rest
-- handle ABCDef = abc-def
camelCaseStrToHyphenated (a:b:c:rest)
  | isUpper a
  , isUpper b
  , isLower c = toLower a:'-':toLower b:camelCaseStrToHyphenated (c:rest)
camelCaseStrToHyphenated (a:rest) = toLower a:camelCaseStrToHyphenated rest

-- | Convert number < 4000 to uppercase roman numeral.
toRomanNumeral :: Int -> T.Text
toRomanNumeral x
  | x >= 4000 || x < 0 = "?"
  | x >= 1000 = "M" <> toRomanNumeral (x - 1000)
  | x >= 900  = "CM" <> toRomanNumeral (x - 900)
  | x >= 500  = "D" <> toRomanNumeral (x - 500)
  | x >= 400  = "CD" <> toRomanNumeral (x - 400)
  | x >= 100  = "C" <> toRomanNumeral (x - 100)
  | x >= 90   = "XC" <> toRomanNumeral (x - 90)
  | x >= 50   = "L"  <> toRomanNumeral (x - 50)
  | x >= 40   = "XL" <> toRomanNumeral (x - 40)
  | x >= 10   = "X" <> toRomanNumeral (x - 10)
  | x == 9    = "IX"
  | x >= 5    = "V" <> toRomanNumeral (x - 5)
  | x == 4    = "IV"
  | x >= 1    = "I" <> toRomanNumeral (x - 1)
  | otherwise = ""

-- | Escape whitespace and some punctuation characters in URI.
escapeURI :: T.Text -> T.Text
escapeURI = T.pack . escapeURIString (not . needsEscaping) . T.unpack
  where needsEscaping c = isSpace c || c `elemText` "<>|\"{}[]^`"


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

{-# DEPRECATED crFilter "readers filter crs automatically" #-}
-- | Strip out DOS line endings.
crFilter :: T.Text -> T.Text
crFilter = T.filter (/= '\r')

--
-- Date/time
--

-- | Parse a date and convert (if possible) to "YYYY-MM-DD" format. We
-- limit years to the range 1601-9999 (ISO 8601 accepts greater than
-- or equal to 1583, but MS Word only accepts dates starting 1601).
normalizeDate :: T.Text -> Maybe T.Text
normalizeDate = fmap T.pack . normalizeDate' . T.unpack

normalizeDate' :: String -> Maybe String
normalizeDate' s = fmap (formatTime defaultTimeLocale "%F")
  (msum $ map (\fs -> parsetimeWith fs s >>= rejectBadYear) formats :: Maybe Day)
  where rejectBadYear day = case toGregorian day of
          (y, _, _) | y >= 1601 && y <= 9999 -> Just day
          _         -> Nothing
        parsetimeWith = parseTimeM True defaultTimeLocale
        formats = ["%x","%m/%d/%Y", "%D","%F", "%d %b %Y",
                    "%e %B %Y", "%b. %e, %Y", "%B %e, %Y",
                    "%Y%m%d", "%Y%m", "%Y"]

--
-- Pandoc block and inline list processing
--

-- | Generate infinite lazy list of markers for an ordered list,
-- depending on list attributes.
orderedListMarkers :: (Int, ListNumberStyle, ListNumberDelim) -> [T.Text]
orderedListMarkers (start, numstyle, numdelim) =
  let nums = case numstyle of
                     DefaultStyle -> map tshow [start..]
                     Example      -> map tshow [start..]
                     Decimal      -> map tshow [start..]
                     UpperAlpha   -> drop (start - 1) $ cycle $
                                     map T.singleton ['A'..'Z']
                     LowerAlpha   -> drop (start - 1) $ cycle $
                                     map T.singleton ['a'..'z']
                     UpperRoman   -> map toRomanNumeral [start..]
                     LowerRoman   -> map (T.toLower . toRomanNumeral) [start..]
      inDelim str = case numdelim of
                            DefaultDelim -> str <> "."
                            Period       -> str <> "."
                            OneParen     -> str <> ")"
                            TwoParens    -> "(" <> str <> ")"
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
removeFormatting = query go . walk (deNote . deQuote)
  where go :: Inline -> [Inline]
        go (Str xs)   = [Str xs]
        go Space      = [Space]
        go SoftBreak  = [SoftBreak]
        go (Code _ x) = [Str x]
        go (Math _ x) = [Str x]
        go LineBreak  = [Space]
        go _          = []

deNote :: Inline -> Inline
deNote (Note _) = Str ""
deNote x        = x

deLink :: Inline -> Inline
deLink (Link _ ils _) = Span nullAttr ils
deLink x              = x

deQuote :: Inline -> Inline
deQuote (Quoted SingleQuote xs) =
  Span ("",[],[]) (Str "\8216" : xs ++ [Str "\8217"])
deQuote (Quoted DoubleQuote xs) =
  Span ("",[],[]) (Str "\8220" : xs ++ [Str "\8221"])
deQuote x = x

-- | Convert pandoc structure to a string with formatting removed.
-- Footnotes are skipped (since we don't want their contents in link
-- labels).
stringify :: Walkable Inline a => a -> T.Text
stringify = query go . walk (deNote . deQuote)
  where go :: Inline -> T.Text
        go Space                                       = " "
        go SoftBreak                                   = " "
        go (Str x)                                     = x
        go (Code _ x)                                  = x
        go (Math _ x)                                  = x
        go (RawInline (Format "html") (T.unpack -> ('<':'b':'r':_)))
                                                       = " " -- see #2105
        go LineBreak                                   = " "
        go _                                           = ""

-- | Bring all regular text in a pandoc structure to uppercase.
--
-- This function correctly handles cases where a lowercase character doesn't
-- match to a single uppercase character – e.g. “Straße” would be converted
-- to “STRASSE”, not “STRAßE”.
capitalize :: Walkable Inline a => a -> a
capitalize = walk go
  where go :: Inline -> Inline
        go (Str s) = Str $ T.toUpper s
        go x       = x

-- | Change final list item from @Para@ to @Plain@ if the list contains
-- no other @Para@ blocks.  Otherwise (if the list items contain @Para@
-- blocks besides possibly at the end), turn any @Plain@s into @Para@s (#5285).
compactify :: [Blocks]  -- ^ List of list items (each a list of blocks)
           -> [Blocks]
compactify [] = []
compactify items =
  let (others, final) = (init items, last items)
  in  case reverse (B.toList final) of
           (Para a:xs)
             | null [Para x | Para x <- xs ++ concatMap B.toList others]
             -> others ++ [B.fromList (reverse (Plain a : xs))]
           _ | null [Para x | Para x <- concatMap B.toList items]
             -> items
           _ -> map (fmap plainToPara) items

plainToPara :: Block -> Block
plainToPara (Plain ils) = Para ils
plainToPara x = x


-- | Like @compactify@, but acts on items of definition lists.
compactifyDL :: [(Inlines, [Blocks])] -> [(Inlines, [Blocks])]
compactifyDL items =
  case reverse items of
        ((t,ds):ys) ->
           case reverse (map (reverse . B.toList) ds) of
             ((Para x:xs) : zs) | not (any isPara xs) ->
                  reverse ys ++
                    [(t, reverse (map B.fromList zs) ++
                         [B.fromList (reverse (Plain x:xs))])]
             _     -> items
        _          -> items


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

-- | Convert Pandoc inline list to plain text identifier.  HTML
-- identifiers must start with a letter, and may contain only
-- letters, digits, and the characters _-.
inlineListToIdentifier :: Extensions -> [Inline] -> T.Text
inlineListToIdentifier exts =
  dropNonLetter . filterAscii . toIdent . stringify . walk unEmojify
  where
    unEmojify :: [Inline] -> [Inline]
    unEmojify
      | extensionEnabled Ext_gfm_auto_identifiers exts ||
        extensionEnabled Ext_ascii_identifiers exts = walk unEmoji
      | otherwise = id
    unEmoji (Span ("",["emoji"],[("data-emoji",ename)]) _) = Str ename
    unEmoji x = x
    dropNonLetter
      | extensionEnabled Ext_gfm_auto_identifiers exts = id
      | otherwise = T.dropWhile (not . isAlpha)
    filterAscii
      | extensionEnabled Ext_ascii_identifiers exts
        = toAsciiText
      | otherwise = id
    toIdent
      | extensionEnabled Ext_gfm_auto_identifiers exts =
        filterPunct . spaceToDash . T.toLower
      | otherwise = T.intercalate "-" . T.words . filterPunct . T.toLower
    filterPunct = T.filter (\c -> isSpace c || isAlphaNum c || isAllowedPunct c)
    isAllowedPunct c
      | extensionEnabled Ext_gfm_auto_identifiers exts
        = c == '-' || c == '_' ||
          generalCategory c `elem` [NonSpacingMark, SpacingCombiningMark,
                                    EnclosingMark, ConnectorPunctuation]
      | otherwise = c == '_' || c == '-' || c == '.'
    spaceToDash = T.map (\c -> if isSpace c then '-' else c)


-- | Put a list of Pandoc blocks into a hierarchical structure:
-- a list of sections (each a Div with class "section" and first
-- element a Header).  If the 'numbering' parameter is True, Header
-- numbers are added via the number attribute on the header.
-- If the baseLevel parameter is Just n, Header levels are
-- adjusted to be gapless starting at level n.
makeSections :: Bool -> Maybe Int -> [Block] -> [Block]
makeSections numbering mbBaseLevel bs =
  S.evalState (go bs) (mbBaseLevel, [])
 where
  go :: [Block] -> S.State (Maybe Int, [Int]) [Block]
  go (Header level (ident,classes,kvs) title':xs) = do
    (mbLevel, lastnum) <- S.get
    let level' = fromMaybe level mbLevel
    let lastnum' = take level' lastnum
    let newnum =
          if level' > 0
             then case length lastnum' of
                      x | "unnumbered" `elem` classes -> []
                        | x >= level' -> init lastnum' ++ [last lastnum' + 1]
                        | otherwise -> lastnum ++
                             replicate (level' - length lastnum - 1) 0 ++ [1]
             else []
    unless (null newnum) $ S.modify $ \(mbl, _) -> (mbl, newnum)
    let (sectionContents, rest) = break (headerLtEq level) xs
    S.modify $ \(_, ln) -> (fmap (+ 1) mbLevel, ln)
    sectionContents' <- go sectionContents
    S.modify $ \(_, ln) -> (mbLevel, ln)
    rest' <- go rest
    let kvs' = -- don't touch number if already present
               case lookup "number" kvs of
                  Nothing | numbering
                          , "unnumbered" `notElem` classes ->
                        ("number", T.intercalate "." (map tshow newnum)) : kvs
                  _ -> kvs
    let divattr = (ident, "section":classes, kvs')
    let attr = ("",classes,kvs')
    return $
      Div divattr (Header level' attr title' : sectionContents') : rest'
  go (Div divattr@(dident,dclasses,_) (Header level hattr title':ys) : xs)
      | all (\case
               Header level' _ _ -> level' > level
               _                 -> True) ys
      , "column" `notElem` dclasses
      , "columns" `notElem` dclasses = do
    inner <- go (Header level hattr title':ys)
    rest <- go xs
    return $
      case inner of
            [Div divattr'@(dident',_,_) zs]
              | T.null dident || T.null dident' || dident == dident'
              -> Div (combineAttr divattr' divattr) zs : rest
            _ -> Div divattr inner : rest
  go (Div attr xs : rest) = do
    xs' <- go xs
    rest' <- go rest
    return $ Div attr xs' : rest'
  go (x:xs) = (x :) <$> go xs
  go [] = return []

  combineAttr :: Attr -> Attr -> Attr
  combineAttr (id1, classes1, kvs1) (id2, classes2, kvs2) =
    (if T.null id1 then id2 else id1,
     ordNub (classes1 ++ classes2),
     foldr (\(k,v) kvs -> case lookup k kvs of
                             Nothing -> (k,v):kvs
                             Just _  -> kvs) mempty (kvs1 ++ kvs2))

headerLtEq :: Int -> Block -> Bool
headerLtEq level (Header l _ _)  = l <= level
headerLtEq level (Div _ (b:_))   = headerLtEq level b
headerLtEq _ _                   = False

-- | Generate a unique identifier from a list of inlines.
-- Second argument is a list of already used identifiers.
uniqueIdent :: Extensions -> [Inline] -> Set.Set T.Text -> T.Text
uniqueIdent exts title' usedIdents =
  if baseIdent `Set.member` usedIdents
     then maybe baseIdent numIdent
          $ find (\x -> numIdent x `Set.notMember` usedIdents) ([1..60000] :: [Int])
          -- if we have more than 60,000, allow repeats
     else baseIdent
  where
    baseIdent = case inlineListToIdentifier exts title' of
                     "" -> "section"
                     x  -> x
    numIdent n = baseIdent <> "-" <> tshow n

-- | True if block is a Header block.
isHeaderBlock :: Block -> Bool
isHeaderBlock Header{} = True
isHeaderBlock _        = False

-- | Shift header levels up or down.
headerShift :: Int -> Pandoc -> Pandoc
headerShift n (Pandoc meta (Header m _ ils : bs))
  | n < 0
  , m + n == 0 = headerShift n $
                 B.setTitle (B.fromList ils) $ Pandoc meta bs
headerShift n (Pandoc meta bs) = Pandoc meta (walk shift bs)

 where
   shift :: Block -> Block
   shift (Header level attr inner)
     | level + n > 0  = Header (level + n) attr inner
     | otherwise      = Para inner
   shift x            = x

-- | Remove empty paragraphs.
stripEmptyParagraphs :: Pandoc -> Pandoc
stripEmptyParagraphs = walk go
  where go :: [Block] -> [Block]
        go = filter (not . isEmptyParagraph)
        isEmptyParagraph (Para []) = True
        isEmptyParagraph _         = False

-- | Detect if table rows contain only cells consisting of a single
-- paragraph that has no @LineBreak@.
onlySimpleTableCells :: [[[Block]]] -> Bool
onlySimpleTableCells = all isSimpleCell . concat
  where
    isSimpleCell [Plain ils] = not (hasLineBreak ils)
    isSimpleCell [Para ils ] = not (hasLineBreak ils)
    isSimpleCell []          = True
    isSimpleCell _           = False
    hasLineBreak = getAny . query isLineBreak
    isLineBreak LineBreak = Any True
    isLineBreak _         = Any False

-- | Detect if a list is tight.
isTightList :: [[Block]] -> Bool
isTightList = all (\item -> firstIsPlain item || null item)
  where firstIsPlain (Plain _ : _) = True
        firstIsPlain _             = False

-- | Convert a list item containing tasklist syntax (e.g. @[x]@)
-- to using @U+2610 BALLOT BOX@ or @U+2612 BALLOT BOX WITH X@.
taskListItemFromAscii :: Extensions -> [Block] -> [Block]
taskListItemFromAscii = handleTaskListItem fromMd
  where
    fromMd (Str "[" : Space : Str "]" : Space : is) = Str "☐" : Space : is
    fromMd (Str "[x]"                 : Space : is) = Str "☒" : Space : is
    fromMd (Str "[X]"                 : Space : is) = Str "☒" : Space : is
    fromMd is = is

-- | Convert a list item containing text starting with @U+2610 BALLOT BOX@
-- or @U+2612 BALLOT BOX WITH X@ to tasklist syntax (e.g. @[x]@).
taskListItemToAscii :: Extensions -> [Block] -> [Block]
taskListItemToAscii = handleTaskListItem toMd
  where
    toMd (Str "☐" : Space : is) = rawMd "[ ]" : Space : is
    toMd (Str "☒" : Space : is) = rawMd "[x]" : Space : is
    toMd is = is
    rawMd = RawInline (Format "markdown")

handleTaskListItem :: ([Inline] -> [Inline]) -> Extensions -> [Block] -> [Block]
handleTaskListItem handleInlines exts bls =
  if Ext_task_lists `extensionEnabled` exts
  then handleItem bls
  else bls
  where
    handleItem (Plain is : bs) = Plain (handleInlines is) : bs
    handleItem (Para is  : bs) = Para  (handleInlines is) : bs
    handleItem bs = bs

-- | Set a field of a 'Meta' object.  If the field already has a value,
-- convert it into a list with the new value appended to the old value(s).
addMetaField :: ToMetaValue a
             => T.Text
             -> a
             -> Meta
             -> Meta
addMetaField key val (Meta meta) =
  Meta $ M.insertWith combine key (toMetaValue val) meta
  where combine newval (MetaList xs) = MetaList (xs ++ tolist newval)
        combine newval x             = MetaList [x, newval]
        tolist (MetaList ys) = ys
        tolist y             = [y]

-- | Create 'Meta' from old-style title, authors, date.  This is
-- provided to ease the transition from the old API.
makeMeta :: [Inline] -> [[Inline]] -> [Inline] -> Meta
makeMeta title authors date =
      addMetaField "title" (B.fromList title)
    $ addMetaField "author" (map B.fromList authors)
    $ addMetaField "date" (B.fromList date) nullMeta

-- | Remove soft breaks between East Asian characters.
eastAsianLineBreakFilter :: Pandoc -> Pandoc
eastAsianLineBreakFilter = bottomUp go
  where go (x:SoftBreak:y:zs)
          | Just (_, b) <- T.unsnoc $ stringify x
          , Just (c, _) <- T.uncons $ stringify y
          , charWidth b == 2
          , charWidth c == 2
          = x:y:zs
          | otherwise
          = x:SoftBreak:y:zs
        go xs
          = xs

-- | Set of HTML elements that are represented as Span with a class equal as
-- the element tag itself.
htmlSpanLikeElements :: Set.Set T.Text
htmlSpanLikeElements = Set.fromList ["kbd", "mark", "dfn"]

-- | Returns the first sentence in a list of inlines, and the rest.
breakSentence :: [Inline] -> ([Inline], [Inline])
breakSentence [] = ([],[])
breakSentence xs =
  let isSentenceEndInline (Str ys)
        | Just (_, c) <- T.unsnoc ys = c == '.' || c == '?'
      isSentenceEndInline LineBreak  = True
      isSentenceEndInline _          = False
      (as, bs) = break isSentenceEndInline xs
  in  case bs of
        []             -> (as, [])
        [c]            -> (as ++ [c], [])
        (c:Space:cs)   -> (as ++ [c], cs)
        (c:SoftBreak:cs) -> (as ++ [c], cs)
        (Str ".":Str s@(T.uncons -> Just (')',_)):cs)
          -> (as ++ [Str ".", Str s], cs)
        (x@(Str (T.stripPrefix ".)" -> Just _)):cs) -> (as ++ [x], cs)
        (LineBreak:x@(Str (T.uncons -> Just ('.',_))):cs) -> (as ++[LineBreak], x:cs)
        (c:cs)         -> (as ++ [c] ++ ds, es)
          where (ds, es) = breakSentence cs

-- | Split a list of inlines into sentences.
splitSentences :: [Inline] -> [[Inline]]
splitSentences xs =
  let (sent, rest) = breakSentence xs
  in  if null rest then [sent] else sent : splitSentences rest

-- | Process ipynb output cells.  If mode is Nothing,
-- remove all output.  If mode is Just format, select
-- best output for the format.  If format is not ipynb,
-- strip out ANSI escape sequences from CodeBlocks (see #5633).
filterIpynbOutput :: Maybe Format -> Pandoc -> Pandoc
filterIpynbOutput mode = walk go
  where go (Div (ident, "output":os, kvs) bs) =
          case mode of
            Nothing  -> Div (ident, "output":os, kvs) []
            -- "best" for ipynb includes all formats:
            Just fmt
              | fmt == Format "ipynb"
                          -> Div (ident, "output":os, kvs) bs
              | otherwise -> Div (ident, "output":os, kvs) $
                              walk removeANSI $
                              take 1 $ sortOn rank bs
                 where
                  rank (RawBlock (Format "html") _)
                    | fmt == Format "html" = 1 :: Int
                    | fmt == Format "markdown" = 2
                    | otherwise = 3
                  rank (RawBlock (Format "latex") _)
                    | fmt == Format "latex" = 1
                    | fmt == Format "markdown" = 2
                    | otherwise = 3
                  rank (RawBlock f _)
                    | fmt == f = 1
                    | otherwise = 3
                  rank (Para [Image{}]) = 1
                  rank _ = 2
                  removeANSI (CodeBlock attr code) =
                    CodeBlock attr (removeANSIEscapes code)
                  removeANSI x = x
                  removeANSIEscapes t
                    | Just cs <- T.stripPrefix "\x1b[" t =
                        removeANSIEscapes $ T.drop 1 $ T.dropWhile (/='m') cs
                    | Just (c, cs) <- T.uncons t = T.cons c $ removeANSIEscapes cs
                    | otherwise = ""
        go x = x

--
-- TagSoup HTML handling
--

-- | Render HTML tags.
renderTags' :: [Tag T.Text] -> T.Text
renderTags' = renderTagsOptions
               renderOptions{ optMinimize = matchTags ["hr", "br", "img",
                                                       "meta", "link", "col"]
                            , optRawTag   = matchTags ["script", "style"] }
              where matchTags tags = flip elem tags . T.toLower

--
-- File handling
--

-- | Perform an IO action in a directory, returning to starting directory.
inDirectory :: FilePath -> IO a -> IO a
inDirectory path action = E.bracket
                             getCurrentDirectory
                             setCurrentDirectory
                             (const $ setCurrentDirectory path >> action)

--
-- Error reporting
--

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft = Bifunctor.first

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
collapseFilePath = Posix.joinPath . reverse . foldl' go [] . splitDirectories
  where
    go rs "." = rs
    go r@(p:rs) ".." = case p of
                            ".."                              -> "..":r
                            (checkPathSeperator -> Just True) -> "..":r
                            _                                 -> rs
    go _ (checkPathSeperator -> Just True) = [[Posix.pathSeparator]]
    go rs x = x:rs
    isSingleton []  = Nothing
    isSingleton [x] = Just x
    isSingleton _   = Nothing
    checkPathSeperator = fmap isPathSeparator . isSingleton

-- Convert the path part of a file: URI to a regular path.
-- On windows, @/c:/foo@ should be @c:/foo@.
-- On linux, @/foo@ should be @/foo@.
uriPathToPath :: T.Text -> FilePath
uriPathToPath (T.unpack -> path) =
#ifdef _WINDOWS
  case path of
    '/':ps -> ps
    ps     -> ps
#else
  path
#endif

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
schemes :: Set.Set T.Text
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
  -- Unofficial schemes
  , "doi", "isbn", "javascript", "pmid"
  ]

-- | Check if the string is a valid URL with a IANA or frequently used but
-- unofficial scheme (see @schemes@).
isURI :: T.Text -> Bool
isURI = maybe False hasKnownScheme . parseURI . T.unpack
  where
    hasKnownScheme = (`Set.member` schemes) . T.toLower .
                     T.filter (/= ':') . T.pack . uriScheme

---
--- Squash blocks into inlines
---

blockToInlines :: Block -> Inlines
blockToInlines (Plain ils) = B.fromList ils
blockToInlines (Para ils) = B.fromList ils
blockToInlines (LineBlock lns) = B.fromList $ combineLines lns
blockToInlines (CodeBlock attr str) = B.codeWith attr str
blockToInlines (RawBlock (Format fmt) str) = B.rawInline fmt str
blockToInlines (BlockQuote blks) = blocksToInlines' blks
blockToInlines (OrderedList _ blkslst) =
  mconcat $ map blocksToInlines' blkslst
blockToInlines (BulletList blkslst) =
  mconcat $ map blocksToInlines' blkslst
blockToInlines (DefinitionList pairslst) =
  mconcat $ map f pairslst
  where
    f (ils, blkslst) = B.fromList ils <> B.str ":" <> B.space <>
      mconcat (map blocksToInlines' blkslst)
blockToInlines (Header _ _  ils) = B.fromList ils
blockToInlines HorizontalRule = mempty
blockToInlines (Table _ _ _ (TableHead _ hbd) bodies (TableFoot _ fbd)) =
  mconcat $ intersperse B.linebreak $
    map (mconcat . map blocksToInlines') (plainRowBody <$> hbd <> unTableBodies bodies <> fbd)
  where
    plainRowBody (Row _ body) = cellBody <$> body
    cellBody (Cell _ _ _ _ body) = body
    unTableBody (TableBody _ _ hd bd) = hd <> bd
    unTableBodies = concatMap unTableBody
blockToInlines (Div _ blks) = blocksToInlines' blks
blockToInlines Null = mempty

blocksToInlinesWithSep :: Inlines -> [Block] -> Inlines
blocksToInlinesWithSep sep =
  mconcat . intersperse sep . map blockToInlines

blocksToInlines' :: [Block] -> Inlines
blocksToInlines' = blocksToInlinesWithSep defaultBlocksSeparator

blocksToInlines :: [Block] -> [Inline]
blocksToInlines = B.toList . blocksToInlines'

-- | Inline elements used to separate blocks when squashing blocks into
-- inlines.
defaultBlocksSeparator :: Inlines
defaultBlocksSeparator =
  -- This is used in the pandoc.utils.blocks_to_inlines function. Docs
  -- there should be updated if this is changed.
  B.space <> B.str "¶" <> B.space

--
-- Safe read
--

safeRead :: (MonadPlus m, Read a) => T.Text -> m a
safeRead = safeStrRead . T.unpack

safeStrRead :: (MonadPlus m, Read a) => String -> m a
safeStrRead s = case reads s of
                  (d,x):_
                    | all isSpace x -> return d
                  _                 -> mzero
--
-- User data directory
--

-- | Return appropriate user data directory for platform.  We use
-- XDG_DATA_HOME (or its default value), but for backwards compatibility,
-- we fall back to the legacy user data directory ($HOME/.pandoc on *nix)
-- if the XDG_DATA_HOME is missing and this exists.  If neither directory
-- is present, we return the XDG data directory.
defaultUserDataDir :: IO FilePath
defaultUserDataDir = do
  xdgDir <- getXdgDirectory XdgData "pandoc"
  legacyDir <- getAppUserDataDirectory "pandoc"
  xdgExists <- doesDirectoryExist xdgDir
  legacyDirExists <- doesDirectoryExist legacyDir
  if not xdgExists && legacyDirExists
     then return legacyDir
     else return xdgDir
