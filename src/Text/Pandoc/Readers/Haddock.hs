{-# LANGUAGE CPP #-}
{- |
   Module      : Text.Pandoc.Readers.Haddock
   Copyright   : Copyright (C) 2013 David Lazar
   License     : GNU GPL, version 2 or above

   Maintainer  : David Lazar <lazar6@illinois.edu>,
                 John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha

Conversion of Haddock markup to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Haddock
    ( readHaddock
    ) where

import Text.Pandoc.Builder (Blocks, Inlines)
import qualified Text.Pandoc.Builder as B
import Data.Monoid ((<>))
import Text.Pandoc.Shared (trim, splitBy)
import Data.List (intersperse, stripPrefix)
import Data.Maybe (fromMaybe)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Documentation.Haddock.Parser
import Documentation.Haddock.Types
import Debug.Trace (trace)

import Text.Pandoc.Error

-- | Parse Haddock markup and return a 'Pandoc' document.
readHaddock :: ReaderOptions -- ^ Reader options
            -> String        -- ^ String to parse
            -> Either PandocError Pandoc
readHaddock opts =
#if MIN_VERSION_haddock_library(1,2,0)
  Right . B.doc . docHToBlocks . trace' . _doc . parseParas
#else
  Right .  B.doc . docHToBlocks . trace' . parseParas
#endif
  where trace' x = if readerTrace opts
                      then trace (show x) x
                      else x

docHToBlocks :: DocH String Identifier -> Blocks
docHToBlocks d' =
  case d' of
    DocEmpty -> mempty
    DocAppend (DocParagraph (DocHeader h)) (DocParagraph (DocAName ident)) ->
         B.headerWith (ident,[],[]) (headerLevel h)
            (docHToInlines False $ headerTitle h)
    DocAppend d1 d2 -> mappend (docHToBlocks d1) (docHToBlocks d2)
    DocString _ -> inlineFallback
    DocParagraph (DocAName h) -> B.plain $ docHToInlines False $ DocAName h
    DocParagraph x -> B.para $ docHToInlines False x
    DocIdentifier _ -> inlineFallback
    DocIdentifierUnchecked _ -> inlineFallback
    DocModule s -> B.plain $ docHToInlines False $ DocModule s
    DocWarning _ -> mempty -- TODO
    DocEmphasis _ -> inlineFallback
    DocMonospaced _ -> inlineFallback
    DocBold _ -> inlineFallback
#if MIN_VERSION_haddock_library(1,4,0)
    DocMathInline _ -> inlineFallback
    DocMathDisplay _ -> inlineFallback
#endif
    DocHeader h -> B.header (headerLevel h)
                           (docHToInlines False $ headerTitle h)
    DocUnorderedList items -> B.bulletList (map docHToBlocks items)
    DocOrderedList items -> B.orderedList (map docHToBlocks items)
    DocDefList items -> B.definitionList (map (\(d,t) ->
                               (docHToInlines False d,
                                [consolidatePlains $ docHToBlocks t])) items)
    DocCodeBlock (DocString s) -> B.codeBlockWith ("",[],[]) s
    DocCodeBlock d -> B.para $ docHToInlines True d
    DocHyperlink _ -> inlineFallback
    DocPic _ -> inlineFallback
    DocAName _ -> inlineFallback
    DocProperty s -> B.codeBlockWith ("",["property","haskell"],[]) (trim s)
    DocExamples es -> mconcat $ map (\e ->
       makeExample ">>>" (exampleExpression e) (exampleResult e)) es

  where inlineFallback = B.plain $ docHToInlines False d'
        consolidatePlains = B.fromList . consolidatePlains' . B.toList
        consolidatePlains' zs@(Plain _ : _) =
          let (xs, ys) = span isPlain zs in
          Para (concatMap extractContents xs) : consolidatePlains' ys
        consolidatePlains' (x : xs) = x : consolidatePlains' xs
        consolidatePlains' [] = []
        isPlain (Plain _) = True
        isPlain _         = False
        extractContents (Plain xs) = xs
        extractContents _ = []

docHToInlines :: Bool -> DocH String Identifier -> Inlines
docHToInlines isCode d' =
  case d' of
    DocEmpty -> mempty
    DocAppend d1 d2 -> mappend (docHToInlines isCode d1)
                               (docHToInlines isCode d2)
    DocString s
      | isCode -> mconcat $ intersperse B.linebreak
                              $ map B.code $ splitBy (=='\n') s
      | otherwise  -> B.text s
    DocParagraph _ -> mempty
    DocIdentifier (_,s,_) -> B.codeWith ("",["haskell","identifier"],[]) s
    DocIdentifierUnchecked s -> B.codeWith ("",["haskell","identifier"],[]) s
    DocModule s -> B.codeWith ("",["haskell","module"],[]) s
    DocWarning _ -> mempty -- TODO
    DocEmphasis d -> B.emph (docHToInlines isCode d)
    DocMonospaced (DocString s) -> B.code s
    DocMonospaced d -> docHToInlines True d
    DocBold d -> B.strong (docHToInlines isCode d)
#if MIN_VERSION_haddock_library(1,4,0)
    DocMathInline s -> B.math s
    DocMathDisplay s -> B.displayMath s
#endif
    DocHeader _ -> mempty
    DocUnorderedList _ -> mempty
    DocOrderedList _ -> mempty
    DocDefList _ -> mempty
    DocCodeBlock _ -> mempty
    DocHyperlink h -> B.link (hyperlinkUrl h) (hyperlinkUrl h)
             (maybe (B.text $ hyperlinkUrl h) B.text $ hyperlinkLabel h)
    DocPic p -> B.image (pictureUri p) (fromMaybe (pictureUri p) $ pictureTitle p)
                        (maybe mempty B.text $ pictureTitle p)
    DocAName s -> B.spanWith (s,["anchor"],[]) mempty
    DocProperty _ -> mempty
    DocExamples _ -> mempty

-- | Create an 'Example', stripping superfluous characters as appropriate
makeExample :: String -> String -> [String] -> Blocks
makeExample prompt expression result =
    B.para $ B.codeWith ("",["prompt"],[]) prompt
        <> B.space
        <> B.codeWith ([], ["haskell","expr"], []) (trim expression)
        <> B.linebreak
        <> (mconcat $ intersperse B.linebreak $ map coder result')
  where
    -- 1. drop trailing whitespace from the prompt, remember the prefix
    prefix = takeWhile (`elem` " \t") prompt

    -- 2. drop, if possible, the exact same sequence of whitespace
    -- characters from each result line
    --
    -- 3. interpret lines that only contain the string "<BLANKLINE>" as an
    -- empty line
    result' = map (substituteBlankLine . tryStripPrefix prefix) result
      where
        tryStripPrefix xs ys = fromMaybe ys $ stripPrefix xs ys

        substituteBlankLine "<BLANKLINE>" = ""
        substituteBlankLine line          = line
    coder = B.codeWith ([], ["result"], [])
