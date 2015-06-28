{-# LANGUAGE CPP #-}
{-
Copyright (C) 2013-2015 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.ManPages
   Copyright   : Copyright (C) 2013-2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions to build pandoc's man pages (pandoc.1 and pandoc_markdown.5)
from pandoc's README.
-}
module Text.Pandoc.ManPages (
  manPandoc1,
  manPandocMarkdown5
  ) where
import Text.Pandoc
import Text.Pandoc.Error (handleError)
import Data.Char (toUpper)
import System.FilePath
import Text.Pandoc.Shared (normalize, readDataFileUTF8)

manPandoc1 :: IO String
manPandoc1 = do
    readme <- readDataFileUTF8 Nothing "README"
    let (Pandoc meta blocks) = normalize $ handleError
                                         $ readMarkdown def readme
    let manBlocks = removeSect [Str "Wrappers"]
                  $ removeSect [Str "Pandoc's",Space,Str "markdown"] blocks
    makeManPage "pandoc.1" meta manBlocks

manPandocMarkdown5 :: IO String
manPandocMarkdown5 = do
    readme <- readDataFileUTF8 Nothing "README"
    let (Pandoc meta blocks) = normalize $ handleError
                                         $ readMarkdown def readme
    let syntaxBlocks = extractSect [Str "Pandoc's",Space,Str "markdown"] blocks
    makeManPage "pandoc_markdown.5" meta syntaxBlocks

makeManPage :: String -> Meta -> [Block] -> IO String
makeManPage page meta blocks = do
  let templ = page <.> "template"
  manTemplate <- readDataFileUTF8 Nothing templ
  return $ writeManPage manTemplate (Pandoc meta blocks)

writeManPage :: String -> Pandoc -> String
writeManPage templ doc =
  writeMan def{ writerStandalone = True
              , writerTemplate = templ
              , writerVariables = [("version", pandocVersion)] } $
                    bottomUp (concatMap removeLinks) $
                    bottomUp  capitalizeHeaders doc

removeLinks :: Inline -> [Inline]
removeLinks (Link l _) = l
removeLinks x = [x]

capitalizeHeaders :: Block -> Block
capitalizeHeaders (Header 1 attr xs) = Header 1 attr $ bottomUp capitalize xs
capitalizeHeaders x = x

capitalize :: Inline -> Inline
capitalize (Str xs) = Str $ map toUpper xs
capitalize x = x

removeSect :: [Inline] -> [Block] -> [Block]
removeSect ils (Header 1 _ x:xs) | x == ils =
  dropWhile (not . isHeader1) xs
removeSect ils (x:xs) = x : removeSect ils xs
removeSect _ [] = []

extractSect :: [Inline] -> [Block] -> [Block]
extractSect ils (Header 1 _ z:xs) | z == ils =
  bottomUp promoteHeader $ takeWhile (not . isHeader1) xs
    where promoteHeader (Header n attr x) = Header (n-1) attr x
          promoteHeader x            = x
extractSect ils (_:xs) = extractSect ils xs
extractSect _ [] = []

isHeader1 :: Block -> Bool
isHeader1 (Header 1 _ _ ) = True
isHeader1 _               = False
