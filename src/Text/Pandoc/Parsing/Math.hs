{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{- |
Module      : Text.Pandoc.Parsing.Math
Copyright   : © 2006-2024 John MacFarlane
License     : GPL-2.0-or-later
Maintainer  : John MacFarlane <jgm@berkeley.edu>

Parsing of LaTeX math.
-}

module Text.Pandoc.Parsing.Math
  ( mathDisplay
  , mathInline
  )
where

import Control.Monad (mzero, when)
import Data.Text (Text)
import Text.Parsec ((<|>), ParsecT, Stream(..), notFollowedBy, many1, try)
import Text.Pandoc.Options
  ( Extension(Ext_tex_math_dollars, Ext_tex_math_single_backslash,
              Ext_tex_math_double_backslash) )
import Text.Pandoc.Parsing.Capabilities (HasReaderOptions, guardEnabled)
import Text.Pandoc.Parsing.General
import Text.Pandoc.Shared (trimMath)
import Text.Pandoc.Sources
  (UpdateSourcePos, anyChar, char, digit, newline, satisfy, space, string)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB

mathInlineWith :: (Stream s m Char, UpdateSourcePos s Char)  => Text -> Text -> ParsecT s st m Text
mathInlineWith op cl = try $ do
  textStr op
  when (op == "$") $ notFollowedBy space
  words' <- many1Till (
                       (T.singleton <$>
                          satisfy (\c -> not (isSpaceChar c || c == '\\')))
                   <|> (char '\\' >>
                           -- This next clause is needed because \text{..} can
                           -- contain $, \(\), etc.
                           (try (string "text" >>
                                 (("\\text" <>) <$> inBalancedBraces 0 ""))
                            <|>  (\c -> T.pack ['\\',c]) <$> anyChar))
                   <|> ("\n" <$ blankline <* notFollowedBy' blankline <* notFollowedBy (char '$'))
                   <|> (T.pack <$> many1 spaceChar <* notFollowedBy (char '$'))
                    ) (try $ textStr cl)
  notFollowedBy digit  -- to prevent capture of $5
  return $ trimMath $ T.concat words'
 where
  inBalancedBraces :: (Stream s m Char, UpdateSourcePos s Char)
                   => Int -> Text -> ParsecT s st m Text
  inBalancedBraces n t =
    TL.toStrict . TB.toLazyText <$>
      go n False (TB.fromText t) (not (T.null t))

  -- go depth lastWasBackslash accumulator started
  go :: (Stream s m Char, UpdateSourcePos s Char)
     => Int -> Bool -> TB.Builder -> Bool -> ParsecT s st m TB.Builder
  go 0 _ acc False = do
    c <- anyChar
    if c == '{'
       then go 1 False (acc <> TB.singleton '{') True
       else mzero
  go 0 _ acc True = return acc
  go depth True acc _ = do
    c <- anyChar
    go depth False (acc <> TB.singleton c) True
  go depth False acc _ = do
    c <- anyChar
    let acc' = acc <> TB.singleton c
    case c of
         '\\' -> go depth True acc' True
         '}'  -> go (depth - 1) False acc' True
         '{'  -> go (depth + 1) False acc' True
         _    -> go depth False acc' True

mathDisplayWith :: (Stream s m Char, UpdateSourcePos s Char) => Text -> Text -> ParsecT s st m Text
mathDisplayWith op cl = try $ fmap T.pack $ do
  textStr op
  many1Till (satisfy (/= '\n') <|> (newline <* notFollowedBy' blankline))
            (try $ textStr cl)

mathDisplay :: (HasReaderOptions st, Stream s m Char, UpdateSourcePos s Char)
            => ParsecT s st m Text
mathDisplay =
      (guardEnabled Ext_tex_math_dollars >> mathDisplayWith "$$" "$$")
  <|> (guardEnabled Ext_tex_math_single_backslash >>
       mathDisplayWith "\\[" "\\]")
  <|> (guardEnabled Ext_tex_math_double_backslash >>
       mathDisplayWith "\\\\[" "\\\\]")

mathInline :: (HasReaderOptions st, Stream s m Char, UpdateSourcePos s Char)
           => ParsecT s st m Text
mathInline =
      (guardEnabled Ext_tex_math_dollars >> mathInlineWith "$" "$")
  <|> (guardEnabled Ext_tex_math_single_backslash >>
       mathInlineWith "\\(" "\\)")
  <|> (guardEnabled Ext_tex_math_double_backslash >>
       mathInlineWith "\\\\(" "\\\\)")
