{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.Blaze
   Copyright   : Copyright (C) 2021-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Render blaze-html Html to DocLayout document (so it can be wrapped).
-}
module Text.Pandoc.Writers.Blaze ( layoutMarkup )
where
import Text.Blaze
import qualified Data.ByteString as S
import Data.List (isInfixOf)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import Data.Text (Text)
import Text.DocLayout hiding (Text, Empty)
import Text.Blaze.Internal (ChoiceString(..), getText, MarkupM(..))

layoutMarkup :: Markup -> Doc T.Text
layoutMarkup = go True mempty
  where
    go :: Bool -> Doc T.Text -> MarkupM b -> Doc T.Text
    go wrap attrs (Parent _ open close content) =
      let open' = getText open
       in literal open'
            <> attrs
            <> char '>'
            <> (case open' of
                  "<code" -> go False mempty content
                  t | t == "<pre" ||
                      t == "<style" ||
                      t == "<script" ||
                      t == "<textarea" -> flush $ go False mempty content
                    | otherwise -> go wrap mempty content)
            <> literal (getText close)
    go wrap attrs (CustomParent tag content) =
        char '<'
            <> fromChoiceString wrap tag
            <> attrs
            <> char '>'
            <> go wrap mempty content
            <> literal "</"
            <> fromChoiceString wrap tag
            <> char '>'
    go _wrap attrs (Leaf _ begin end _) =
        literal (getText begin)
            <> attrs
            <> literal (getText end)
    go wrap attrs (CustomLeaf tag close _) =
        char '<'
            <> fromChoiceString wrap tag
            <> attrs
            <> (if close then literal " />" else char '>')
    go wrap attrs (AddAttribute rawkey _ value h) =
        go wrap
          (space' wrap
            <> literal (getText rawkey)
            <> char '='
            <> doubleQuotes (fromChoiceString False value)
            <> attrs) h
    go wrap attrs (AddCustomAttribute key value h) =
        go wrap
          (space' wrap
            <> fromChoiceString wrap key
            <> char '='
            <> doubleQuotes (fromChoiceString False value)
            <> attrs) h
    go wrap _ (Content content _) = fromChoiceString wrap content
    go wrap _ (Comment comment _) =
        literal "<!--"
            <> space' wrap
            <> fromChoiceString False comment
            <> space' wrap
            <> "-->"
    go wrap attrs (Append h1 h2) = go wrap attrs h1 <> go wrap attrs h2
    go _ _ (Empty _) = mempty
    space' wrap = if wrap then space else char ' '


fromChoiceString :: Bool                  -- ^ Allow wrapping
                 -> ChoiceString          -- ^ String to render
                 -> Doc Text              -- ^ Resulting builder
fromChoiceString wrap (Static s)     = withWrap wrap $ getText s
fromChoiceString wrap (String s)     = withWrap wrap $
                                         escapeMarkupEntities $ T.pack s
fromChoiceString wrap (Text s)       = withWrap wrap $ escapeMarkupEntities s
fromChoiceString wrap (ByteString s) = withWrap wrap $ decodeUtf8 s
fromChoiceString _wrap (PreEscaped x) = -- don't wrap!
  case x of
    String s -> literal $ T.pack s
    Text   s -> literal s
    s        -> fromChoiceString False s
fromChoiceString wrap (External x) = case x of
    -- Check that the sequence "</" is *not* in the external data.
    String s     -> if "</" `isInfixOf` s then mempty else withWrap wrap (T.pack s)
    Text   s     -> if "</" `T.isInfixOf` s then mempty else withWrap wrap s
    ByteString s -> if "</" `S.isInfixOf` s then mempty else withWrap wrap (decodeUtf8 s)
    s            -> fromChoiceString wrap s
fromChoiceString wrap (AppendChoiceString x y) =
    fromChoiceString wrap x <> fromChoiceString wrap y
fromChoiceString _ EmptyChoiceString = mempty

withWrap :: Bool -> Text -> Doc Text
withWrap wrap
  | wrap = mconcat . toChunks
  | otherwise = literal

toChunks :: Text -> [Doc Text]
toChunks = map toDoc . T.groupBy sameStatus
  where
   toDoc t
     | t == " " = space
     | t == "\n" = cr
     | otherwise         = literal t
   sameStatus c d =
     (c == ' ' && d == ' ') ||
     (c == '\n' && d == '\n') ||
     (c /= ' ' && d /= ' ' && c /= '\n' && d /= '\n')


-- | Escape predefined XML entities in a text value
--
escapeMarkupEntities :: Text     -- ^ Text to escape
                     -> Text -- ^ Resulting Doc
escapeMarkupEntities = T.concatMap escape
  where
    escape :: Char -> Text
    escape '<'  = "&lt;"
    escape '>'  = "&gt;"
    escape '&'  = "&amp;"
    escape '"'  = "&quot;"
    escape '\'' = "&#39;"
    escape x    = T.singleton x
