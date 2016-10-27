{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright (C) 2013-2016 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.MIME
   Copyright   : Copyright (C) 2013-2016 John MacFarlane
   License     : GNU GPL, version 2 or later

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Portability : portable

YAML block parser utils.
-}
module Text.Pandoc.YAML ( yamlMetaBlock ) where

import qualified Text.Pandoc.Builder as B
import           Text.Pandoc.Definition
import           Text.Pandoc.Error
import           Text.Pandoc.Options
import           Text.Pandoc.Parsing
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import           Data.Scientific (coefficient, base10Exponent)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import           Data.Yaml (ParseException(..), YamlException(..), YamlMark(..))


yamlMetaBlock :: ( Stream [Char] m Char, HasReaderOptions st, HasWarnings st )
              => (ReaderOptions -> String -> Either PandocError Pandoc)
              -> ParserT [Char] st m Meta
yamlMetaBlock formatReader = try $ do
  guardEnabled Ext_yaml_metadata_block
  pos <- getPosition
  string "---"
  blankline
  notFollowedBy blankline  -- if --- is followed by a blank it's an HRULE
  rawYamlLines <- manyTill anyLine stopLine
  -- by including --- and ..., we allow yaml blocks with just comments:
  let rawYaml = unlines ("---" : (rawYamlLines ++ ["..."]))
  optional blanklines
  opts <- extractReaderOptions <$> getState
  let formatTextReader = formatReader (noHeaderBlockExtensions opts) . T.unpack
  case Yaml.decodeEither' $ UTF8.fromString rawYaml of
    Right (Yaml.Object hashmap) -> return $
      H.foldrWithKey (\k v m ->
           if ignorable k
              then m
              else case yamlToMeta formatTextReader v of
                     Left _  -> m
                     Right v' -> B.setMeta (T.unpack k) v' m)
        nullMeta hashmap
    Right Yaml.Null -> return nullMeta
    Right _ -> do
      addWarning (Just pos) "YAML header is not an object"
      return nullMeta
    Left err' -> do
      case err' of
         InvalidYaml (Just YamlParseException{
                     yamlProblem = problem
                   , yamlContext = _ctxt
                   , yamlProblemMark = Yaml.YamlMark {
                         yamlLine = yline
                       , yamlColumn = ycol
                   }}) ->
              addWarning (Just $ setSourceLine
                 (setSourceColumn pos
                    (sourceColumn pos + ycol))
                 (sourceLine pos + 1 + yline))
                 $ "Could not parse YAML header: " ++ problem
         _ -> addWarning (Just pos)
                 $ "Could not parse YAML header: " ++ show err'
      return nullMeta
 where
    noHeaderBlockExtensions o =
      o { readerExtensions = readerExtensions o `Set.difference` meta_exts }
    meta_exts = Set.fromList
      [ Ext_pandoc_title_block
      , Ext_mmd_title_block
      , Ext_yaml_metadata_block
      ]

-- ignore fields ending with _
ignorable :: Text -> Bool
ignorable t = (T.pack "_") `T.isSuffixOf` t

toMetaValue :: (Text -> Either PandocError Pandoc)
            -> Text
            -> Either PandocError MetaValue
toMetaValue formatReader x = toMeta <$> formatReader x
  where
    toMeta p =
      case p of
        Pandoc _ [Plain xs]  -> MetaInlines xs
        Pandoc _ [Para xs]
         | endsWithNewline x -> MetaBlocks [Para xs]
         | otherwise         -> MetaInlines xs
        Pandoc _ bs          -> MetaBlocks bs
    endsWithNewline t = T.pack "\n" `T.isSuffixOf` t

yamlToMeta :: (Text -> Either PandocError Pandoc)
           -> Yaml.Value -> Either PandocError MetaValue
yamlToMeta fp (Yaml.String t) = toMetaValue fp t
yamlToMeta _  (Yaml.Number n)
  -- avoid decimal points for numbers that don't need them:
  | base10Exponent n >= 0     = return $ MetaString $ show
                                $ coefficient n * (10 ^ base10Exponent n)
  | otherwise                 = return $ MetaString $ show n
yamlToMeta _  (Yaml.Bool b)   = return $ MetaBool b
yamlToMeta fp (Yaml.Array xs) = B.toMetaValue <$> mapM (yamlToMeta fp)
                                                  (V.toList xs)
yamlToMeta fp (Yaml.Object o) = MetaMap <$> H.foldrWithKey (\k v m ->
                                if ignorable k
                                   then m
                                   else (do
                                    v' <- yamlToMeta fp v
                                    m' <- m
                                    return (M.insert (T.unpack k) v' m')))
                                (return M.empty) o
yamlToMeta _ _ = return $ MetaString ""

stopLine :: Stream s m Char => ParserT s st m ()
stopLine = try $ (string "---" <|> string "...") >> blankline >> return ()
