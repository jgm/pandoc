{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2014-2019 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

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
   Module      : Text.Pandoc.Readers.Org.Shared
   Copyright   : Copyright (C) 2014-2019 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Utility functions used in other Pandoc Org modules.
-}
module Text.Pandoc.Readers.Org.Shared
  ( cleanLinkString
  , isImageFilename
  , originalLang
  , translateLang
  , exportsCode
  ) where

import Prelude
import Data.Char (isAlphaNum)
import Data.List (isPrefixOf)
import System.FilePath (isValid, takeExtension)


-- | Check whether the given string looks like the path to of URL of an image.
isImageFilename :: String -> Bool
isImageFilename fp = hasImageExtension && (isValid fp || isKnownProtocolUri)
 where
   hasImageExtension = takeExtension fp `elem` imageExtensions
   isKnownProtocolUri = any (\x -> (x ++ "://") `isPrefixOf` fp) protocols

   imageExtensions = [ ".jpeg", ".jpg", ".png", ".gif", ".svg" ]
   protocols = [ "file", "http", "https" ]

-- | Cleanup and canonicalize a string describing a link.  Return @Nothing@ if
-- the string does not appear to be a link.
cleanLinkString :: String -> Maybe String
cleanLinkString s =
  case s of
    '/':_                  -> Just $ "file://" ++ s  -- absolute path
    '.':'/':_              -> Just s                 -- relative path
    '.':'.':'/':_          -> Just s                 -- relative path
    -- Relative path or URL (file schema)
    'f':'i':'l':'e':':':s' -> Just $ if "//" `isPrefixOf` s' then s else s'
    _                      -> if isUrl s then Just s else Nothing
 where
   isUrl :: String -> Bool
   isUrl cs =
     let (scheme, path) = break (== ':') cs
     in all (\c -> isAlphaNum c || c `elem` (".-"::String)) scheme
          && not (null path)

-- | Creates an key-value pair marking the original language name specified for
-- a piece of source code.

-- | Creates an key-value attributes marking the original language name
-- specified for a piece of source code.
originalLang :: String -> [(String, String)]
originalLang lang =
  let transLang = translateLang lang
  in if transLang == lang
     then []
     else [("org-language", lang)]

-- | Translate from Org-mode's programming language identifiers to those used
-- by Pandoc.  This is useful to allow for proper syntax highlighting in
-- Pandoc output.
translateLang :: String -> String
translateLang cs =
  case cs of
    "C"          -> "c"
    "C++"        -> "cpp"
    "emacs-lisp" -> "commonlisp" -- emacs lisp is not supported
    "js"         -> "javascript"
    "lisp"       -> "commonlisp"
    "R"          -> "r"
    "sh"         -> "bash"
    "sqlite"     -> "sql"
    _            -> cs

exportsCode :: [(String, String)] -> Bool
exportsCode = maybe True (`elem` ["code", "both"]) . lookup "exports"
