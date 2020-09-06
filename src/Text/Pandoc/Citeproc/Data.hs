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
import Citeproc (Lang(..), parseLang)

biblatexLocalizations :: [(FilePath, ByteString)]
biblatexLocalizations = $(embedDir "citeproc/biblatex-localization")

-- biblatex localization keys, from files at
-- http://github.com/plk/biblatex/tree/master/tex/latex/biblatex/lbx
biblatexStringMap :: M.Map Text (M.Map Text (Text, Text))
biblatexStringMap = foldr go mempty biblatexLocalizations
 where
  go (fp, bs) =
    let Lang lang _ = parseLang (toIETF $ T.takeWhile (/= '.') $ T.pack fp)
        ls = T.lines $ TE.decodeUtf8 bs
     in if length ls > 4
           then M.insert lang (toStringMap $ map (T.splitOn "|") ls)
           else id
  toStringMap = foldr go' mempty
  go' [term, x, y] = M.insert term (x, y)
  go' _ = id
