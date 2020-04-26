{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns      #-}
{- |
   Module      : Text.Pandoc.Readers.CommonMark
   Copyright   : Copyright (C) 2015-2020 John MacFarlane
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
import Control.Monad.Except
import Data.Functor.Identity (runIdentity)

-- | Parse a CommonMark formatted string into a 'Pandoc' structure.
readCommonMark :: PandocMonad m => ReaderOptions -> Text -> m Pandoc
readCommonMark opts s = do
  let res = runIdentity $
              commonmarkWith (foldr (<>) defaultSyntaxSpec exts) "input" s
  case res of
    Left err -> throwError $ PandocParsecError s err
    Right (Cm bls :: Cm () Blocks) -> return $ B.doc bls
 where
  exts = [ hardLineBreaksSpec | isEnabled Ext_hard_line_breaks opts ] ++
         [ smartPunctuationSpec | isEnabled Ext_smart opts ] ++
         [ strikethroughSpec | isEnabled Ext_strikeout opts ] ++
         [ superscriptSpec | isEnabled Ext_superscript opts ] ++
         [ subscriptSpec | isEnabled Ext_subscript opts ] ++
         [ mathSpec | isEnabled Ext_tex_math_dollars opts ] ++
         [ fancyListSpec | isEnabled Ext_fancy_lists opts ] ++
         [ fencedDivSpec | isEnabled Ext_fenced_divs opts ] ++
         [ bracketedSpanSpec | isEnabled Ext_bracketed_spans opts ] ++
         [ rawAttributeSpec | isEnabled Ext_raw_attribute opts ] ++
         [ attributesSpec | isEnabled Ext_attributes opts ] ++
         [ pipeTableSpec | isEnabled Ext_pipe_tables opts ] ++
         [ autolinkSpec | isEnabled Ext_autolink_bare_uris opts ] ++
         [ emojiSpec | isEnabled Ext_emoji opts ] ++
         [ autoIdentifiersSpec
           | isEnabled Ext_gfm_auto_identifiers opts
           , not (isEnabled Ext_ascii_identifiers opts) ] ++
         [ autoIdentifiersAsciiSpec
           | isEnabled Ext_gfm_auto_identifiers opts
           , isEnabled Ext_ascii_identifiers opts ] ++
         [ implicitHeadingReferencesSpec
           | isEnabled Ext_implicit_header_references opts ] ++
         [ footnoteSpec | isEnabled Ext_footnotes opts ] ++
         [ definitionListSpec | isEnabled Ext_definition_lists opts ] ++
         [ taskListSpec | isEnabled Ext_task_lists opts ]

