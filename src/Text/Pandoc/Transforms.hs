{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE StrictData          #-}
{- |
   Module      : Text.Pandoc.Transforms
   Copyright   : © 2006-2024 John MacFarlane
   License     : GPL-2.0-or-later
   Maintainer  : John MacFarlane <jgm@berkeley@edu>

Transformation of a Pandoc document post-parsing
-}
module Text.Pandoc.Transforms
  ( Transform
  , applyTransforms
  , adjustLinksAndIds
  , eastAsianLineBreakFilter
  , filterIpynbOutput
  , headerShift
  ) where

import Data.List (sortOn)
import Data.Text (Text)
import Network.URI (unEscapeString)
import Text.DocLayout (charWidth)
import Text.Pandoc.Definition
  ( Pandoc (..), Attr, Block (..), Format (..), Inline (..) )
import Text.Pandoc.Generic (bottomUp)
import Text.Pandoc.Options (Extensions)
import Text.Pandoc.Shared (stringify, textToIdentifier)
import Text.Pandoc.Walk (walk)
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B

-- | Transformation of a Pandoc document post-parsing
type Transform = Pandoc -> Pandoc

-- | Apply a list of transforms to a document, in order.
applyTransforms :: Monad m => [Transform] -> Pandoc -> m Pandoc
applyTransforms transforms d = return $ foldr ($) d transforms

-- | Prefixes identifiers with a string derived from the filepath of
-- @thisfile@; fixes links to targets in @allfiles@ accordingly.
adjustLinksAndIds :: Extensions        -- ^ defines how IDs are generated
                  -> Text              -- ^ thisfile
                  -> [Text]            -- ^ allfiles
                  -> Transform
adjustLinksAndIds exts thisfile allfiles
  | length allfiles > 1 = walk fixInline . walk fixBlock
  | otherwise           = id
 where
  -- fix ids in blocks
  fixBlock :: Block -> Block
  fixBlock (CodeBlock attr t) = CodeBlock (fixAttrs attr) t
  fixBlock (Header lev attr ils) = Header lev (fixAttrs attr) ils
  fixBlock (Table attr cap cols th tbs tf) =
     Table (fixAttrs attr) cap cols th tbs tf
  fixBlock (Div attr bs) = Div (fixAttrs attr) bs
  fixBlock x = x

  -- fix ids and links in inlines
  fixInline :: Inline -> Inline
  fixInline (Code attr t) = Code (fixAttrs attr) t
  fixInline (Link attr ils (url,tit)) =
    Link (fixAttrs attr) ils (fixURL url,tit)
  fixInline (Image attr ils (url,tit)) =
    Image (fixAttrs attr) ils (fixURL url,tit)
  fixInline (Span attr ils) = Span (fixAttrs attr) ils
  fixInline x = x

  -- add thisfile as prefix of identifier
  fixAttrs :: Attr -> Attr
  fixAttrs (i,cs,kvs)
    | T.null i = (i,cs,kvs)
    | otherwise =
        (T.intercalate "__"
          (filter (not . T.null) [toIdent thisfile, i]),
        cs, kvs)

  -- turns a filepath into an identifier
  toIdent :: Text -> Text
  toIdent = textToIdentifier exts . T.intercalate "__" .
            T.split (\c -> c == '/' || c == '\\')

  -- if URL begins with file from allfiles, convert to
  -- an internal link with the appropriate identifier
  fixURL :: Text -> Text
  fixURL u =
    let (a,b) = T.break (== '#') $ T.pack . unEscapeString . T.unpack $ u
        filepart = if T.null a
                      then toIdent thisfile
                      else toIdent a
        fragpart = T.dropWhile (== '#') b
     in if T.null a || a `elem` allfiles
           then "#" <> T.intercalate "__"
                         (filter (not . T.null) [filepart, fragpart])
           else u

-- | Process ipynb output cells.  If mode is Nothing,
-- remove all output.  If mode is Just format, select
-- best output for the format.  If format is not ipynb,
-- strip out ANSI escape sequences from CodeBlocks (see #5633).
filterIpynbOutput :: Maybe Format -> Pandoc -> Pandoc
filterIpynbOutput mode = walk go
  where go (Div (ident, "output":os, kvs) bs) =
          case mode of
            Nothing  -> Div (ident, "output":os, kvs) []
            -- "best" for ipynb includes all formats:
            Just fmt
              | fmt == Format "ipynb"
                          -> Div (ident, "output":os, kvs) bs
              | otherwise -> Div (ident, "output":os, kvs) $
                              walk removeANSI $
                              take 1 $ sortOn rank bs
                 where
                  rank (RawBlock (Format "html") _)
                    | fmt == Format "html" = 1 :: Int
                    | fmt == Format "markdown" = 3
                    | otherwise = 4
                  rank (RawBlock (Format "latex") _)
                    | fmt == Format "latex" = 1
                    | fmt == Format "markdown" = 3
                    | otherwise = 4
                  rank (RawBlock f _)
                    | fmt == f = 1
                    | otherwise = 4
                  rank (Para [Image{}]) = 2
                  rank _ = 3
                  removeANSI (CodeBlock attr code) =
                    CodeBlock attr (removeANSIEscapes code)
                  removeANSI x = x
                  removeANSIEscapes t
                    | Just cs <- T.stripPrefix "\x1b[" t =
                        removeANSIEscapes $ T.drop 1 $ T.dropWhile (/='m') cs
                    | Just (c, cs) <- T.uncons t = T.cons c $ removeANSIEscapes cs
                    | otherwise = ""
        go x = x

-- | Remove soft breaks between East Asian characters.
eastAsianLineBreakFilter :: Pandoc -> Pandoc
eastAsianLineBreakFilter = bottomUp go
  where go (x:SoftBreak:y:zs)
          | Just (_, b) <- T.unsnoc $ stringify x
          , Just (c, _) <- T.uncons $ stringify y
          , charWidth b == 2
          , charWidth c == 2
          = x:y:zs
          | otherwise
          = x:SoftBreak:y:zs
        go xs
          = xs

-- | Shift header levels up or down.
headerShift :: Int -> Pandoc -> Pandoc
headerShift n (Pandoc meta (Header m _ ils : bs))
  | n < 0
  , m + n == 0 = headerShift n $
                 B.setTitle (B.fromList ils) $ Pandoc meta bs
-- for this case, see #10459:
headerShift n (Pandoc meta (Div attr@(_,"section":_,_) (Header m _ ils : as) : bs))
  | n < 0
  , m + n == 0 = headerShift n $
                 B.setTitle (B.fromList ils) $ Pandoc meta (Div attr as : bs)
headerShift n (Pandoc meta bs) = Pandoc meta (walk shift bs)

 where
   shift :: Block -> Block
   shift (Header level attr inner)
     | level + n > 0  = Header (level + n) attr inner
     | otherwise      = Para inner
   shift x            = x
