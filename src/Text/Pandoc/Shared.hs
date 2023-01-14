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
   Copyright   : Copyright (C) 2006-2023 John MacFarlane
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
                     -- * Text processing
                     inquotes,
                     tshow,
                     stripTrailingNewlines,
                     trim,
                     triml,
                     trimr,
                     trimMath,
                     stripFirstAndLast,
                     camelCaseToHyphenated,
                     camelCaseStrToHyphenated,
                     toRomanNumeral,
                     tabFilter,
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
                     figureDiv,
                     makeSections,
                     uniqueIdent,
                     inlineListToIdentifier,
                     textToIdentifier,
                     isHeaderBlock,
                     headerShift,
                     stripEmptyParagraphs,
                     onlySimpleTableCells,
                     isTightList,
                     taskListItemFromAscii,
                     taskListItemToAscii,
                     handleTaskListItem,
                     addMetaField,
                     eastAsianLineBreakFilter,
                     htmlSpanLikeElements,
                     filterIpynbOutput,
                     formatCode,
                     -- * TagSoup HTML handling
                     renderTags',
                     -- * File handling
                     inDirectory,
                     makeCanonical,
                     collapseFilePath,
                     filteredFilesFromArchive,
                     -- * for squashing blocks
                     blocksToInlines,
                     blocksToInlines',
                     blocksToInlinesWithSep,
                     defaultBlocksSeparator,
                     -- * Safe read
                     safeRead,
                     safeStrRead
                    ) where

import Codec.Archive.Zip
import qualified Control.Exception as E
import Control.Monad (MonadPlus (..), msum, unless)
import qualified Control.Monad.State.Strict as S
import qualified Data.ByteString.Lazy as BL
import Data.Containers.ListUtils (nubOrd)
import Data.Char (isAlpha, isLower, isSpace, isUpper, toLower, isAlphaNum,
                  generalCategory, GeneralCategory(NonSpacingMark,
                  SpacingCombiningMark, EnclosingMark, ConnectorPunctuation))
import Data.List (find, foldl', groupBy, intercalate, intersperse,
                  union, sortOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid (Any (..))
import Data.Sequence (ViewL (..), ViewR (..), viewl, viewr)
import qualified Data.Set as Set
import qualified Data.Text as T
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

-- | Split text at the given widths. Note that the break points are
-- /not/ indices but text widths, which will be different for East Asian
-- characters, emojis, etc.
splitTextByIndices :: [Int] -> T.Text -> [T.Text]
splitTextByIndices ns = splitTextByRelIndices (zipWith (-) ns (0:ns)) . T.unpack
 where
  splitTextByRelIndices [] cs = [T.pack cs]
  splitTextByRelIndices (x:xs) cs =
    let (first, rest) = splitAt' x cs
     in T.pack first : splitTextByRelIndices xs rest

-- | Returns a pair whose first element is a prefix of @t@ and that has
-- width @n@, and whose second is the remainder of the string.
--
-- Note: Do *not* replace this with 'T.splitAt', which is not sensitive
-- to character widths!
splitAt' :: Int {-^ n -} -> [Char] {-^ t -} -> ([Char],[Char])
splitAt' _ []          = ([],[])
splitAt' n xs | n <= 0 = ([],xs)
splitAt' n (x:xs)      = (x:ys,zs)
  where (ys,zs) = splitAt' (n - charWidth x) xs

--
-- Text processing
--

-- | Wrap double quotes around a Text
inquotes :: T.Text -> T.Text
inquotes txt = T.cons '\"' (T.snoc txt '\"')

-- | Like @'show'@, but returns a 'T.Text' instead of a 'String'.
tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- | Strip trailing newlines from string.
stripTrailingNewlines :: T.Text -> T.Text
stripTrailingNewlines = T.dropWhileEnd (== '\n')

-- | Returns 'True' for an ASCII whitespace character, viz. space,
-- carriage return, newline, and horizontal tab.
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

--
-- Date/time
--

-- | Parse a date and convert (if possible) to "YYYY-MM-DD" format. We
-- limit years to the range 1601-9999 (ISO 8601 accepts greater than
-- or equal to 1583, but MS Word only accepts dates starting 1601).
normalizeDate :: T.Text -> Maybe T.Text
normalizeDate = fmap T.pack . normalizeDate' . T.unpack

-- | Like @'normalizeDate'@, but acts on 'String' instead of 'T.Text'.
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

-- | Replaces 'Note' elements with empty strings.
deNote :: Inline -> Inline
deNote (Note _) = Str ""
deNote x        = x

-- | Convert pandoc structure to a string with formatting removed.
-- Footnotes are skipped (since we don't want their contents in link
-- labels).
stringify :: Walkable Inline a => a -> T.Text
stringify = query go . walk fixInlines
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

        fixInlines :: Inline -> Inline
        fixInlines (Cite _ ils) = Cite [] ils
        fixInlines (Note _) = Note []
        fixInlines (q@Quoted{}) = deQuote q
        fixInlines x = x

-- | Unwrap 'Quoted' inline elements, enclosing the contents with
-- English-style Unicode quotes instead.
deQuote :: Inline -> Inline
deQuote (Quoted SingleQuote xs) =
  Span ("",[],[]) (Str "\8216" : xs ++ [Str "\8217"])
deQuote (Quoted DoubleQuote xs) =
  Span ("",[],[]) (Str "\8220" : xs ++ [Str "\8221"])
deQuote x = x

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

-- | Creates a Div block from figure components. The intended use is in
-- writers of formats that do not have markup support for figures.
--
-- The resulting div is given the class @figure@ and contains the figure
-- body and the figure caption. The latter is wrapped in a 'Div' of
-- class @caption@, with the stringified @short-caption@ as attribute.
figureDiv :: Attr -> Caption -> [Block] -> Block
figureDiv (ident, classes, kv) (Caption shortcapt longcapt) body =
  let divattr = ( ident
              , ["figure"] `union` classes
              , kv
              )
      captkv = maybe mempty (\s -> [("short-caption", stringify s)]) shortcapt
      capt = [Div ("", ["caption"], captkv) longcapt | not (null longcapt)]
  in Div divattr (body ++ capt)

-- | Returns 'True' iff the given element is a 'Para'.
isPara :: Block -> Bool
isPara (Para _) = True
isPara _        = False

-- | Convert Pandoc inline list to plain text identifier.
inlineListToIdentifier :: Extensions -> [Inline] -> T.Text
inlineListToIdentifier exts =
  textToIdentifier exts . stringify . walk unEmojify
  where
    unEmojify :: [Inline] -> [Inline]
    unEmojify
      | extensionEnabled Ext_gfm_auto_identifiers exts ||
        extensionEnabled Ext_ascii_identifiers exts = walk unEmoji
      | otherwise = id
    unEmoji (Span ("",["emoji"],[("data-emoji",ename)]) _) = Str ename
    unEmoji x = x

-- | Convert string to plain text identifier.
textToIdentifier :: Extensions -> T.Text -> T.Text
textToIdentifier exts =
  dropNonLetter . filterAscii . toIdent
  where
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
      , "columns" `notElem` dclasses
      , "fragment" `notElem` dclasses = do
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
     nubOrd (classes1 ++ classes2),
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
    toMd (Str "❏" : Space : is) = rawMd "[ ]" : Space : is
    toMd (Str "✓" : Space : is) = rawMd "[x]" : Space : is
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
                    | fmt == Format "markdown" = 3
                    | otherwise = 4
                  rank (RawBlock (Format "latex") _)
                    | fmt == Format "latex" = 1
                    | fmt == Format "markdown" = 3
                    | otherwise = 4
                  rank (RawBlock f _)
                    | fmt == f = 1
                    | otherwise = 4
                  rank (Para [Image{}]) = 2
                  rank _ = 3
                  removeANSI (CodeBlock attr code) =
                    CodeBlock attr (removeANSIEscapes code)
                  removeANSI x = x
                  removeANSIEscapes t
                    | Just cs <- T.stripPrefix "\x1b[" t =
                        removeANSIEscapes $ T.drop 1 $ T.dropWhile (/='m') cs
                    | Just (c, cs) <- T.uncons t = T.cons c $ removeANSIEscapes cs
                    | otherwise = ""
        go x = x

-- | Reformat 'Inlines' as code, putting the stringlike parts in 'Code'
-- elements while bringing other inline formatting outside.
-- The idea is that e.g. `[Str "a",Space,Strong [Str "b"]]` should turn
-- into `[Code ("",[],[]) "a ", Strong [Code ("",[],[]) "b"]]`.
-- This helps work around the limitation that pandoc's Code element can
-- only contain string content (see issue #7525).
formatCode :: Attr -> Inlines -> Inlines
formatCode attr = B.fromList . walk fmt . B.toList
  where
    isPlaintext (Str _) = True
    isPlaintext Space = True
    isPlaintext SoftBreak = True
    isPlaintext (Quoted _ _) = True
    isPlaintext _ = False
    fmt = concatMap go . groupBy (\a b -> isPlaintext a && isPlaintext b)
      where
        go xs
          | all isPlaintext xs = B.toList $ B.codeWith attr $ stringify xs
          | otherwise = xs

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

-- | Canonicalizes a file path by removing redundant @.@ and @..@.
makeCanonical :: FilePath -> FilePath
makeCanonical = Posix.joinPath . transformPathParts . splitDirectories
 where  transformPathParts = reverse . foldl' go []
        go as        "."  = as
        go ("..":as) ".." = ["..", ".."] <> as
        go (_:as)    ".." = as
        go as        x    = x : as

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

--
-- File selection from the archive
--
filteredFilesFromArchive :: Archive -> (FilePath -> Bool) -> [(FilePath, BL.ByteString)]
filteredFilesFromArchive zf f =
  mapMaybe (fileAndBinary zf) (filter f (filesInArchive zf))
  where
    fileAndBinary :: Archive -> FilePath -> Maybe (FilePath, BL.ByteString)
    fileAndBinary a fp = findEntryByPath fp a >>= \e -> Just (fp, fromEntry e)

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
blockToInlines (Figure _ _ body) = blocksToInlines' body

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
  B.linebreak

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
