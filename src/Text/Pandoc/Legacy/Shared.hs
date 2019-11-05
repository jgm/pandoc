{-# LANGUAGE FlexibleContexts #-}
module Text.Pandoc.Legacy.Shared (
                     TP.splitBy,
                     TP.splitByIndices,
                     TP.splitStringByIndices,
                     TP.substitute,
                     TP.ordNub,
                     TP.ToString (..),
                     backslashEscapes,
                     escapeStringUsing,
                     stripTrailingNewlines,
                     trim,
                     triml,
                     trimr,
                     trimMath,
                     stripFirstAndLast,
                     TP.camelCaseToHyphenated,
                     toRomanNumeral,
                     escapeURI,
                     TP.tabFilter,
                     TP.crFilter,
                     normalizeDate,
                     orderedListMarkers,
                     TP.extractSpaces,
                     TP.removeFormatting,
                     TP.deNote,
                     TP.deLink,
                     stringify,
                     TP.capitalize,
                     TP.compactify,
                     TP.compactifyDL,
                     TP.linesToPara,
                     TP.makeSections,
                     uniqueIdent,
                     inlineListToIdentifier,
                     TP.isHeaderBlock,
                     TP.headerShift,
                     TP.stripEmptyParagraphs,
                     TP.onlySimpleTableCells,
                     TP.isTightList,
                     TP.taskListItemFromAscii,
                     TP.taskListItemToAscii,
                     addMetaField,
                     TP.makeMeta,
                     TP.eastAsianLineBreakFilter,
                     TP.underlineSpan,
                     TP.htmlSpanLikeElements,
                     TP.splitSentences,
                     TP.filterIpynbOutput,
                     renderTags',
                     TP.inDirectory,
                     TP.collapseFilePath,
                     uriPathToPath,
                     TP.filteredFilesFromArchive,
                     schemes,
                     isURI,
                     TP.mapLeft,
                     TP.blocksToInlines,
                     TP.blocksToInlines',
                     TP.blocksToInlinesWithSep,
                     TP.defaultBlocksSeparator,
                     safeRead,
                     TP.defaultUserDataDirs,
                     pandocVersion
                    ) where

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Text.Pandoc.Extensions as TP
import qualified Text.Pandoc.Shared as TP
import qualified Text.Pandoc.Definition as TP
import qualified Text.Pandoc.Walk as TP
import qualified Data.Map as M
import qualified Text.Pandoc.Legacy.Builder as B
import Text.HTML.TagSoup (Tag)
import Control.Monad (MonadPlus)

unpackAround :: (T.Text -> T.Text) -> String -> String
unpackAround f = T.unpack . f . T.pack

pandocVersion :: String
pandocVersion = T.unpack TP.pandocVersion

backslashEscapes :: [Char]
                 -> [(Char, String)]
backslashEscapes = map (\(x, y) -> (x, T.unpack y)) . TP.backslashEscapes

escapeStringUsing :: [(Char, String)] -> String -> String
escapeStringUsing tbl = unpackAround $ TP.escapeTextUsing (map (\(x, y) -> (x, T.pack y)) tbl)

stripTrailingNewlines :: String -> String
stripTrailingNewlines = unpackAround TP.stripTrailingNewlines

trim :: String -> String
trim = unpackAround TP.trim

triml :: String -> String
triml = unpackAround TP.triml

trimr :: String -> String
trimr = unpackAround TP.trimr

trimMath :: String -> String
trimMath = unpackAround TP.trimMath

stripFirstAndLast :: String -> String
stripFirstAndLast = unpackAround TP.stripFirstAndLast

toRomanNumeral :: Int -> String
toRomanNumeral = T.unpack . TP.toRomanNumeral

escapeURI :: String -> String
escapeURI = unpackAround TP.escapeURI

normalizeDate :: String -> Maybe String
normalizeDate = fmap T.unpack . TP.normalizeDate . T.pack

orderedListMarkers :: (Int, TP.ListNumberStyle, TP.ListNumberDelim) -> [String]
orderedListMarkers = fmap T.unpack . TP.orderedListMarkers

stringify :: TP.Walkable TP.Inline a => a -> String
stringify = T.unpack . TP.stringify

inlineListToIdentifier :: TP.Extensions -> [TP.Inline] -> String
inlineListToIdentifier x = T.unpack . TP.inlineListToIdentifier x

uniqueIdent :: TP.Extensions -> [TP.Inline] -> Set.Set String -> String
uniqueIdent x y = T.unpack . TP.uniqueIdent x y . Set.map T.pack

addMetaField :: B.ToMetaValue a
             => String
             -> a
             -> B.Meta
             -> B.Meta
addMetaField key val (B.Meta meta) = -- temporarily reimplemented because of class issues.
  B.Meta $ M.insertWith combine key (B.toMetaValue val) meta
  where combine newval (B.MetaList xs) = B.MetaList (xs ++ tolist newval)
        combine newval x             = B.MetaList [x, newval]
        tolist (B.MetaList ys) = ys
        tolist y             = [y]

renderTags' :: [Tag String] -> String
renderTags' = T.unpack . TP.renderTags' . map (fmap T.pack)

uriPathToPath :: String -> FilePath
uriPathToPath = TP.uriPathToPath . T.pack

schemes :: Set.Set String
schemes = Set.map T.unpack TP.schemes

isURI :: String -> Bool
isURI = TP.isURI . T.pack

safeRead :: (MonadPlus m, Read a) => String -> m a
safeRead = TP.safeRead . T.pack
