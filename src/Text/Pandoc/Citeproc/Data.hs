{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Text.Pandoc.Citeproc.Data
  (biblatexStringMap)
where
import Data.FileEmbed
import Data.ByteString (ByteString)
import qualified Data.Map as M
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.Text (Text)
import Text.Pandoc.Citeproc.Util (toIETF)
import Text.Collate.Lang (Lang(..), parseLang)

biblatexLocalizations :: [(FilePath, ByteString)]
biblatexLocalizations = $(embedDir "citeproc/biblatex-localization")

-- biblatex localization keys, from files at
-- http://github.com/plk/biblatex/tree/master/tex/latex/biblatex/lbx
biblatexStringMap :: M.Map Text (M.Map Text (Text, Text))
biblatexStringMap = foldr go mempty biblatexLocalizations
 where
  go (fp, bs) =
    let ls = T.lines $ TE.decodeUtf8 bs
     in case parseLang (toIETF $ T.takeWhile (/= '.') $ T.pack fp) of
          Right lang | length ls > 4
            -> M.insert (langLanguage lang)
                        (toStringMap $ map (T.splitOn "|") ls)
          _ -> id
  toStringMap = foldr go' mempty
  go' [term, x, y] = M.insert term (x, y)
  go' _ = id
