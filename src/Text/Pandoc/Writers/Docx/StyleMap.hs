{-# LANGUAGE FlexibleContexts  #-}
{- |
   Module : Text.Pandoc.Writers.Docx.StyleMap
   Copyright   : © 2014-2020 Jesse Rosenthal <jrosenthal@jhu.edu>,
                   2014-2024 John MacFarlane <jgm@berkeley.edu>,
                   2015-2019 Nikolay Yakimov <root@livid.pp.ru>
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Mappings of element styles (word to pandoc-internal).
-}

module Text.Pandoc.Writers.Docx.StyleMap ( StyleMaps(..)
                                         , ParaStyleName
                                         , CharStyleName
                                         , TableStyleName
                                         , getStyleMaps
                                         , getStyleIdFromName
                                         , hasStyleName
                                         , fromStyleId
                                         , fromStyleName
                                         ) where

import Text.Pandoc.Readers.Docx.Parse.Styles
import Codec.Archive.Zip
import qualified Data.Map as M
import qualified Data.Text as T
import Data.String
import Data.Char (isSpace)

data StyleMaps = StyleMaps { smCharStyle  :: CharStyleNameMap
                           , smParaStyle  :: ParaStyleNameMap
                           , smTableStyle :: TableStyleNameMap }
type TableStyleNameMap = M.Map TableStyleName TableStyle
type ParaStyleNameMap = M.Map ParaStyleName ParStyle
type CharStyleNameMap = M.Map CharStyleName CharStyle

getStyleIdFromName :: (Ord sn, FromStyleName sn, IsString (StyleId sty), HasStyleId sty)
                   => sn -> M.Map sn sty -> StyleId sty
getStyleIdFromName s = maybe (fallback s) getStyleId . M.lookup s
  where fallback = fromString . T.unpack . T.filter (not . isSpace) . fromStyleName

hasStyleName :: (Ord sn, HasStyleId sty)
             => sn -> M.Map sn sty -> Bool
hasStyleName styleName = M.member styleName

getStyleMaps :: Archive -> StyleMaps
getStyleMaps arch =
  let (s1, s2, s3) = archiveToStyles' getStyleName getStyleName getStyleName arch
  in  StyleMaps s1 s2 s3
