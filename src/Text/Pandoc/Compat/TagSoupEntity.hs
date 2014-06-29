{-# LANGUAGE CPP #-}
module Text.Pandoc.Compat.TagSoupEntity (lookupEntity
                          ) where

import qualified Text.HTML.TagSoup.Entity as TE

lookupEntity :: String -> Maybe Char
#if MIN_VERSION_tagsoup(0,13,0)
lookupEntity = str2chr . TE.lookupEntity
  where str2chr :: Maybe String -> Maybe Char
        str2chr (Just [c]) = Just c
        str2chr _ = Nothing
#else
lookupEntity = TE.lookupEntity
#endif
