{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Readers.CommonMark
   Copyright   : Copyright (C) 2015-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of CommonMark-formatted plain text to 'Pandoc' document.

CommonMark is a strongly specified variant of Markdown: http://commonmark.org.
-}
module Text.Pandoc.Readers.CommonMark (readCommonMark)
where

import Commonmark
import Commonmark.Extensions
import Commonmark.Pandoc
import Data.Text (Text)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Builder as B
import Text.Pandoc.Options
import Text.Pandoc.Error
import Text.Pandoc.Readers.Metadata (yamlMetaBlock)
import Control.Monad.Except
import Data.Functor.Identity (runIdentity)
import Data.Typeable
import Text.Pandoc.Parsing (runParserT, getInput, getPosition,
                            runF, defaultParserState, option, many1, anyChar,
                            Sources(..), ToSources(..), ParserT, Future,
                            sourceName, sourceLine, incSourceLine)
import Text.Pandoc.Walk (walk)
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A
import Control.Applicative ((<|>))

-- | Parse a CommonMark formatted string into a 'Pandoc' structure.
readCommonMark :: (PandocMonad m, ToSources a)
               => ReaderOptions -> a -> m Pandoc
readCommonMark opts s
  | isEnabled Ext_yaml_metadata_block opts = do
    let sources = toSources s
    let firstSourceName = case unSources sources of
                               ((pos,_):_) -> sourceName pos
                               _ -> ""
    let toks = concatMap sourceToToks (unSources sources)
    res <- runParserT (do meta <- yamlMetaBlock (metaValueParser opts)
                          pos <- getPosition
                          rest <- getInput
                          let rest' = case rest of
                                -- update position of first source (#7863):
                                Sources ((_,t):xs) -> Sources ((pos,t):xs)
                                _ -> rest
                          return (meta, rest'))
                      defaultParserState firstSourceName sources
    case res of
      Left _ -> readCommonMarkBody opts sources toks
      Right (meta, rest) -> do
        -- strip off metadata section and parse body
        let body = concatMap sourceToToks (unSources rest)
        Pandoc _ bs <- readCommonMarkBody opts sources body
        return $ Pandoc (runF meta defaultParserState) bs
  | otherwise = do
    let sources = toSources s
    let toks = concatMap sourceToToks (unSources sources)
    readCommonMarkBody opts sources toks

sourceToToks :: (SourcePos, Text) -> [Tok]
sourceToToks (pos, s) = map adjust $ tokenize (sourceName pos) s
 where
   adjust = case sourceLine pos of
              1 -> id
              n -> \tok -> tok{ tokPos =
                                  incSourceLine (tokPos tok) (n - 1) }


metaValueParser :: Monad m
                => ReaderOptions -> ParserT Sources st m (Future st MetaValue)
metaValueParser opts = do
  inp <- option "" $ T.pack <$> many1 anyChar
  let toks = concatMap sourceToToks (unSources (toSources inp))
  case runIdentity (parseCommonmarkWith (specFor opts) toks) of
     Left _ -> mzero
     Right (Cm bls :: Cm () Blocks) -> return $ return $ B.toMetaValue bls

readCommonMarkBody :: PandocMonad m => ReaderOptions -> Sources -> [Tok] -> m Pandoc
readCommonMarkBody opts s toks =
  (if readerStripComments opts
      then walk stripBlockComments . walk stripInlineComments
      else id) <$>
  if isEnabled Ext_sourcepos opts
     then case runIdentity (parseCommonmarkWith (specFor opts) toks) of
            Left err -> throwError $ PandocParsecError s err
            Right (Cm bls :: Cm SourceRange Blocks) -> return $ B.doc bls
     else case runIdentity (parseCommonmarkWith (specFor opts) toks) of
            Left err -> throwError $ PandocParsecError s err
            Right (Cm bls :: Cm () Blocks) -> return $ B.doc bls

stripBlockComments :: Block -> Block
stripBlockComments (RawBlock (B.Format "html") s) =
  RawBlock (B.Format "html") (removeComments s)
stripBlockComments x = x

stripInlineComments :: Inline -> Inline
stripInlineComments (RawInline (B.Format "html") s) =
  RawInline (B.Format "html") (removeComments s)
stripInlineComments x = x

removeComments :: Text -> Text
removeComments s =
  either (const s) id $ A.parseOnly pRemoveComments s
 where
  pRemoveComments = mconcat <$> A.many'
    ("" <$ (A.string "<!--" *> A.scan (0 :: Int) scanChar <* A.char '>') <|>
     (A.takeWhile1 (/= '<')) <|>
     (A.string "<"))
  scanChar st c =
    case c of
      '-' -> Just (st + 1)
      '>' | st >= 2 -> Nothing
      _ -> Just 0

specFor :: (Monad m, Typeable m, Typeable a,
            Rangeable (Cm a Inlines), Rangeable (Cm a Blocks))
        => ReaderOptions -> SyntaxSpec m (Cm a Inlines) (Cm a Blocks)
specFor opts = foldr ($) defaultSyntaxSpec exts
 where
  exts = [ (hardLineBreaksSpec <>) | isEnabled Ext_hard_line_breaks opts ] ++
         [ (smartPunctuationSpec <>) | isEnabled Ext_smart opts ] ++
         [ (strikethroughSpec <>) | isEnabled Ext_strikeout opts ] ++
         [ (superscriptSpec <>) | isEnabled Ext_superscript opts ] ++
         [ (subscriptSpec <>) | isEnabled Ext_subscript opts ] ++
         [ (mathSpec <>) | isEnabled Ext_tex_math_dollars opts ] ++
         [ (fancyListSpec <>) | isEnabled Ext_fancy_lists opts ] ++
         [ (fencedDivSpec <>) | isEnabled Ext_fenced_divs opts ] ++
         [ (bracketedSpanSpec <>) | isEnabled Ext_bracketed_spans opts ] ++
         [ (rawAttributeSpec <>) | isEnabled Ext_raw_attribute opts ] ++
         [ (attributesSpec <>) | isEnabled Ext_attributes opts ] ++
         [ (<> pipeTableSpec) | isEnabled Ext_pipe_tables opts ] ++
            -- see #6739
         [ (autolinkSpec <>) | isEnabled Ext_autolink_bare_uris opts ] ++
         [ (emojiSpec <>) | isEnabled Ext_emoji opts ] ++
         [ (autoIdentifiersSpec <>)
           | isEnabled Ext_gfm_auto_identifiers opts
           , not (isEnabled Ext_ascii_identifiers opts) ] ++
         [ (autoIdentifiersAsciiSpec <>)
           | isEnabled Ext_gfm_auto_identifiers opts
           , isEnabled Ext_ascii_identifiers opts ] ++
         [ (implicitHeadingReferencesSpec <>)
           | isEnabled Ext_implicit_header_references opts ] ++
         [ (footnoteSpec <>) | isEnabled Ext_footnotes opts ] ++
         [ (definitionListSpec <>) | isEnabled Ext_definition_lists opts ] ++
         [ (taskListSpec <>) | isEnabled Ext_task_lists opts ] ++
         [ (rebaseRelativePathsSpec <>)
           | isEnabled Ext_rebase_relative_paths opts ]
