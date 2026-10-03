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

import Control.Monad (when, guard)
import Data.Text (Text)
import Text.Parsec ((<|>), ParsecT, Stream(..), notFollowedBy, many, many1, try)
import Text.Pandoc.Options
  ( Extension(Ext_tex_math_dollars, Ext_tex_math_single_backslash,
              Ext_tex_math_double_backslash) )
import Text.Pandoc.Parsing.Capabilities (HasReaderOptions, guardEnabled)
import Text.Pandoc.Parsing.General
import Text.Pandoc.Shared (trimMath)
import Text.Pandoc.Sources
  (UpdateSourcePos, anyChar, char, digit, newline, satisfy, space)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB

mathInlineWith :: (Stream s m Char, UpdateSourcePos s Char)  => Text -> Text -> ParsecT s st m Text
mathInlineWith op cl = try $ do
  textStr op
  when (op == "$") $ notFollowedBy space
  words' <- many1Till (
                       mathComment (newline <* notFollowedBy' blankline)
                   <|> mathGroup anyChar
                   <|> (T.singleton <$>
                          satisfy (\c -> not (isSpaceChar c ||
                                             c == '\\' || c == '{')))
                   <|> (char '\\' >> (\c -> T.pack ['\\',c]) <$> anyChar)
                   <|> ("\n" <$ blankline <* notFollowedBy' blankline <*
                          (guard (op /= "$") <|> notFollowedBy (char '$')))
                   <|> (T.pack <$> many1 spaceChar <*
                          (guard (op /= "$") <|> notFollowedBy (char '$')))
                    ) (try $ textStr cl)
  notFollowedBy digit  -- to prevent capture of $5
  return $ trimMath $ T.concat words'

-- Preserve comments, but do not interpret their braces or math delimiters.
-- Consume the newline too: it terminates the comment, not the math content.
mathComment :: (Stream s m Char, UpdateSourcePos s Char)
            => ParsecT s st m Char -> ParsecT s st m Text
mathComment lineEnd = do
  char '%'
  content <- many (satisfy (/= '\n'))
  end <- lineEnd
  return $ T.pack ('%' : content ++ [end])

-- Consume a whole TeX group so delimiters in command arguments (for example,
-- \text{hi $x$ bye} or \colorbox{aqua}{$x$}) cannot close the outer math.
-- Escaped braces do not change the nesting depth.
mathGroup :: (Stream s m Char, UpdateSourcePos s Char)
          => ParsecT s st m Char -> ParsecT s st m Text
mathGroup mathChar = do
  char '{'
  TL.toStrict . TB.toLazyText <$> go (1 :: Int) False (TB.singleton '{')
 where
  -- go depth lastWasBackslash accumulator
  go 0 _ acc = return acc
  go depth True acc = do
    c <- mathChar
    go depth False (acc <> TB.singleton c)
  go depth False acc =
    (do comment <- mathComment mathChar
        go depth False (acc <> TB.fromText comment))
    <|> do
      c <- mathChar
      let acc' = acc <> TB.singleton c
      case c of
           '\\' -> go depth True acc'
           '}'  -> go (depth - 1) False acc'
           '{'  -> go (depth + 1) False acc'
           _    -> go depth False acc'

mathDisplayWith :: (Stream s m Char, UpdateSourcePos s Char) => Text -> Text -> ParsecT s st m Text
mathDisplayWith op cl = try $ fmap T.concat $ do
  textStr op
  many1Till (mathComment mathChar
            <|> mathGroup mathChar
            <|> (char '\\' >> (\c -> T.pack ['\\',c]) <$> mathChar)
            <|> (T.singleton <$> satisfy (`notElem` ['\n', '\\', '{']))
            <|> ("\n" <$ newline <* notFollowedBy' blankline))
            (try $ textStr cl)
 where
  mathChar = satisfy (/= '\n') <|>
                (newline <* notFollowedBy' blankline)

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
