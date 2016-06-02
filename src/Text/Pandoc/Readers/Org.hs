{-
Copyright (C) 2014-2016 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

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
   Module      : Text.Pandoc.Readers.Org
   Copyright   : Copyright (C) 2014-2016 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Conversion of org-mode formatted plain text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Org ( readOrg ) where

import           Text.Pandoc.Readers.Org.Blocks ( blockList, meta )
import           Text.Pandoc.Readers.Org.Parsing ( OrgParser, readWithM )
import           Text.Pandoc.Readers.Org.ParserState ( optionsToParserState )

import           Text.Pandoc.Definition
import           Text.Pandoc.Error
import           Text.Pandoc.Options

import           Control.Monad.Reader ( runReader )


-- | Parse org-mode string and return a Pandoc document.
readOrg :: ReaderOptions -- ^ Reader options
        -> String        -- ^ String to parse (assuming @'\n'@ line endings)
        -> Either PandocError Pandoc
readOrg opts s = flip runReader def $
                 readWithM parseOrg (optionsToParserState opts) (s ++ "\n\n")

--
-- Parser
--
parseOrg :: OrgParser Pandoc
parseOrg = do
  blocks' <- blockList
  meta'   <- meta
  return . Pandoc meta' $ removeUnwantedBlocks blocks'
 where
   removeUnwantedBlocks :: [Block] -> [Block]
   removeUnwantedBlocks = dropCommentTrees . filter (/= Null)

-- | Drop COMMENT headers and the document tree below those headers.
dropCommentTrees :: [Block] -> [Block]
dropCommentTrees [] = []
dropCommentTrees (b:bs) =
  maybe (b:dropCommentTrees bs)
        (dropCommentTrees . flip dropUntilHeaderAboveLevel bs)
        (commentHeaderLevel b)

-- | Return the level of a header starting a comment or :noexport: tree and
--  Nothing otherwise.
commentHeaderLevel :: Block -> Maybe Int
commentHeaderLevel blk =
   case blk of
     (Header level _ ((Str "COMMENT"):_))          -> Just level
     (Header level _ title) | hasNoExportTag title -> Just level
     _                                             -> Nothing
 where
   hasNoExportTag :: [Inline] -> Bool
   hasNoExportTag = any isNoExportTag

   isNoExportTag :: Inline -> Bool
   isNoExportTag (Span ("", ["tag"], [("data-tag-name", "noexport")]) []) = True
   isNoExportTag _ = False

-- | Drop blocks until a header on or above the given level is seen
dropUntilHeaderAboveLevel :: Int -> [Block] -> [Block]
dropUntilHeaderAboveLevel n = dropWhile (not . isHeaderLevelLowerEq n)

isHeaderLevelLowerEq :: Int -> Block -> Bool
isHeaderLevelLowerEq n blk =
  case blk of
    (Header level _ _) -> n >= level
    _                  -> False
