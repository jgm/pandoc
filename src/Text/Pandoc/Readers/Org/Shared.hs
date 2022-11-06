{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Org.Shared
   Copyright   : Copyright (C) 2014-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Utility functions used in other Pandoc Org modules.
-}
module Text.Pandoc.Readers.Org.Shared
  ( cleanLinkText
  , isImageFilename
  , originalLang
  , translateLang
  , exportsCode
  ) where

import Control.Applicative ((<|>))
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (isValid, takeExtension)
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.Windows as Windows

-- | Check whether the given string looks like the path to of URL of an image.
isImageFilename :: Text -> Bool
isImageFilename fp = hasImageExtension && (isValid (T.unpack fp) || isKnownProtocolUri)
 where
   hasImageExtension = takeExtension (T.unpack $ T.toLower fp)
                       `elem` imageExtensions
   isKnownProtocolUri = any (\x -> (x <> "://") `T.isPrefixOf` fp) protocols

   imageExtensions = [ ".jpeg", ".jpg", ".png", ".gif", ".svg", ".webp", ".jxl" ]
   protocols = [ "file", "http", "https" ]

-- | Cleanup and canonicalize a string describing a link.  Return @Nothing@ if
-- the string does not appear to be a link.
cleanLinkText :: Text -> Maybe Text
cleanLinkText s
  | Just f <- toFileSchema s           = Just f                -- absolute path
  | Just _ <- T.stripPrefix "./" s     = Just s                -- relative path
  | Just _ <- T.stripPrefix "../" s    = Just s                -- relative path
  -- Relative path or URL (file schema)
  | Just s' <- T.stripPrefix "file:" s = if "//" `T.isPrefixOf` s'
                                         then Just s
                                         else  toFileSchema s' <|> Just s'
  | isUrl s                            = Just s
  | otherwise                          = Nothing
  where
    toFileSchema :: Text -> Maybe Text
    toFileSchema t
      | Windows.isAbsolute (T.unpack t) = Just ("file:///" <> t)
      | Posix.isAbsolute (T.unpack t)   = Just ("file://" <> t)
      | otherwise                       = Nothing
    isUrl :: Text -> Bool
    isUrl cs =
      let (scheme, path) = T.break (== ':') cs
      in T.all (\c -> isAlphaNum c || T.any (== c) ".-") scheme
         && not (T.null path)

-- | Creates an key-value pair marking the original language name specified for
-- a piece of source code.

-- | Creates an key-value attributes marking the original language name
-- specified for a piece of source code.
originalLang :: Text -> [(Text, Text)]
originalLang lang =
  let transLang = translateLang lang
  in [("org-language", lang) | transLang /= lang]

-- | Translate from Org-mode's programming language identifiers to those used
-- by Pandoc.  This is useful to allow for proper syntax highlighting in
-- Pandoc output.
translateLang :: Text -> Text
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

exportsCode :: [(Text, Text)] -> Bool
exportsCode = maybe True (`elem` ["code", "both"]) . lookup "exports"
