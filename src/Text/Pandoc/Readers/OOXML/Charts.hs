{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.OOXML.Charts
   Copyright   : © 2026 Nacho García
   License     : GNU GPL, version 2 or above

   Maintainer  : Nacho García <hello@nachogarcia.dev>
   Stability   : alpha
   Portability : portable

Reading of DrawingML charts (in PPTX slides and XLSX sheets), from the
values cached in the chart part.
-}
module Text.Pandoc.Readers.OOXML.Charts
  ( Chart(..)
  , ChartSeries(..)
  , readChart
  , chartToBlocks
  ) where

import Codec.Archive.Zip (Archive, findEntryByPath, fromEntry)
import Data.List (find, nub, transpose)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.Text (Text)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition
import Text.Pandoc.Readers.OOXML.Shared
import Text.Pandoc.XML.Light
import Text.Read (readMaybe)

-- | A chart's data, as cached in its chart part
data Chart = Chart
  { chartTitle  :: Text     -- ^ Empty if the chart has no title
  , chartTypes  :: [Text]   -- ^ Such as @bar@ or @line@, one per plot
  , chartSeries :: [ChartSeries]
  } deriving (Show)

-- | One series: its name, and its points as (category, value)
data ChartSeries = ChartSeries
  { seriesName   :: Text
  , seriesPoints :: [(Text, Text)]
  } deriving (Show)

-- | Read a chart part (@c:chartSpace@)
readChart :: Archive -> FilePath -> Either Text Chart
readChart archive path = do
  entry <- maybe (Left $ "Entry not found: " <> T.pack path) Right $
           findEntryByPath path archive
  chartSpace <- parseXMLElement (TL.decodeUtf8 (fromEntry entry))
  chart <- maybe (Left "Missing c:chart") Right $ child "chart" chartSpace
  plotArea <- maybe (Left "Missing c:plotArea") Right $
              child "plotArea" chart
  let plots = filter (("Chart" `T.isSuffixOf`) . qName . elName)
                     (elChildren plotArea)
      series = zipWith parseSeries [1..] (concatMap (children "ser") plots)
      title = maybe "" titleText (child "title" chart)
  if all (all (T.null . snd) . seriesPoints) series
     then Left "Chart has no cached values"
     else Right Chart
            { chartTitle = title
            , chartTypes = nub $ map (T.dropEnd 5 . qName . elName) plots
            , chartSeries = series
            }

-- | Text of a chart title: rich text, or a cached cell reference
titleText :: Element -> Text
titleText title =
  case child "tx" title of
    Just tx | Just rich <- child "rich" tx -> drawingMLText rich
            | otherwise -> T.unwords (cachedValues tx)
    Nothing -> ""

-- | Parse a series (@c:ser@). Its categories come from @c:cat@, or
-- @c:xVal@ in a scatter or bubble chart, and its values from @c:val@ or
-- @c:yVal@. A series without categories is numbered from 1, as in
-- PowerPoint.
parseSeries :: Int -> Element -> ChartSeries
parseSeries n ser =
  ChartSeries
    { seriesName = case child "tx" ser of
        Just tx -> T.unwords (cachedValues tx)
        Nothing -> "Series" <> T.pack (show n)
    , seriesPoints =
        [ (fromMaybe (T.pack (show (i + 1))) (M.lookup i categories), v)
        | i <- [0 .. count - 1]
        , let v = fromMaybe "" (M.lookup i values)
        ]
    }
  where
    points names = maybe (0, M.empty) cachedPoints $
      listToMaybe (mapMaybe (`child` ser) names)
    (catCount, categories) = points ["cat", "xVal"]
    (valCount, values) = points ["val", "yVal"]
    count = maximum [catCount, valCount, size values, size categories]
    size m = maybe 0 ((+ 1) . fst) (M.lookupMax m)

-- | The cached points of a data source (@c:cat@, @c:val@, ...): the
-- point count and the value of each point by index. In multi-level
-- categories only the innermost level is read.
cachedPoints :: Element -> (Int, M.Map Int Text)
cachedPoints source =
  case listToMaybe (mapMaybe cache (elChildren source)) of
    Just c ->
      ( fromMaybe 0 $ child "ptCount" c >>= findAttr (unqual "val") >>=
                        readMaybe . T.unpack
      , M.fromList
          [ (i, maybe "" strContent (child "v" pt))
          | pt <- children "pt" c
          , Just i <- [findAttr (unqual "idx") pt >>= readMaybe . T.unpack]
          ]
      )
    Nothing -> (0, M.empty)
  where
    cache el = case qName (elName el) of
      name | name `elem` ["strLit", "numLit"] -> Just el
           | name `elem` ["strRef", "numRef"] ->
               find ((`elem` ["strCache", "numCache"]) . qName . elName)
                    (elChildren el)
           | name == "multiLvlStrRef" ->
               child "multiLvlStrCache" el >>= child "lvl"
           | otherwise -> Nothing

-- | The cached values of a text source such as a series name (@c:tx@),
-- which is either a cell reference with its cache or a literal @c:v@
cachedValues :: Element -> [Text]
cachedValues tx =
  case child "v" tx of
    Just v -> [strContent v]
    Nothing -> M.elems (snd (cachedPoints tx))

-- | A chart as a table, in a @chart@ div with the chart types as its
-- @type@ attribute. Series that share their categories go in one table,
-- one column per series; otherwise each series gets its own table.
chartToBlocks :: Chart -> [Block]
chartToBlocks chart =
  [Div ("", ["chart"], [("type", T.unwords (chartTypes chart))])
       (map table groups)]
  where
    series = chartSeries chart
    categoriesOf = map fst . seriesPoints
    groups = case series of
      s:rest | all ((== categoriesOf s) . categoriesOf) rest -> [series]
      _ -> map (: []) series
    caption = if T.null (chartTitle chart)
                 then Caption Nothing []
                 else Caption Nothing [Plain (B.toList (B.text
                                               (chartTitle chart)))]
    table group =
      let cell t = Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1)
                     [Plain (B.toList (B.text t))]
          headerRow = Row nullAttr $ cell "" : map (cell . seriesName) group
          categories = maybe [] categoriesOf (listToMaybe group)
          values = transpose (map (map snd . seriesPoints) group)
          bodyRows = zipWith (\category vs -> Row nullAttr
                                              (map cell (category : vs)))
                             categories values
       in Table nullAttr caption
            (replicate (length group + 1) (AlignDefault, ColWidthDefault))
            (TableHead nullAttr [headerRow])
            [TableBody nullAttr 0 [] bodyRows]
            (TableFoot nullAttr [])

-- Chart elements are matched by local name, as the c prefix can vary.
child :: Text -> Element -> Maybe Element
child name = find ((== name) . qName . elName) . elChildren

children :: Text -> Element -> [Element]
children name = filter ((== name) . qName . elName) . elChildren
