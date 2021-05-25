{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Org
   Copyright   : © 2019-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Conversion of jira wiki formatted plain text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Jira ( readJira ) where

import Control.Monad.Except (throwError)
import Data.Text (Text, append, pack, singleton, unpack)
import Text.HTML.TagSoup.Entity (lookupEntity)
import Text.Jira.Parser (parse)
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Builder hiding (cell)
import Text.Pandoc.Error (PandocError (PandocParseError))
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Sources (ToSources(..), sourcesToText)
import qualified Text.Jira.Markup as Jira

-- | Read Jira wiki markup.
readJira :: (PandocMonad m, ToSources a)
         => ReaderOptions
         -> a
         -> m Pandoc
readJira _opts inp = do
  let sources = toSources inp
  case parse (sourcesToText sources) of
    Right d -> return $ jiraToPandoc d
    Left e  -> throwError . PandocParseError $
               "Jira parse error" `append` pack (show e)

jiraToPandoc :: Jira.Doc -> Pandoc
jiraToPandoc (Jira.Doc blks) = doc $ foldMap jiraToPandocBlocks blks

--
-- Blocks
--

-- | Converts a Jira block to a Pandoc block.
jiraToPandocBlocks :: Jira.Block -> Blocks
jiraToPandocBlocks = \case
  Jira.BlockQuote blcks -> blockQuote $ foldMap jiraToPandocBlocks blcks
  Jira.Code lang ps txt -> toPandocCodeBlocks (Just lang) ps txt
  Jira.Color c blcks    -> divWith (mempty, mempty, [("color", colorName c)]) $
                           foldMap jiraToPandocBlocks blcks
  Jira.Header lvl inlns -> header lvl $ foldMap jiraToPandocInlines inlns
  Jira.HorizontalRule   -> horizontalRule
  Jira.List style items -> toPandocList style items
  Jira.NoFormat ps txt  -> toPandocCodeBlocks Nothing ps txt
  Jira.Panel ps blcks   -> toPandocDiv ps blcks
  Jira.Para inlns       -> para $ foldMap jiraToPandocInlines inlns
  Jira.Table rows       -> toPandocTable rows

-- | Create a pandoc list – either to a @'BulletList'@ or an @'OrderedList'@.
toPandocList :: Jira.ListStyle -> [[Jira.Block]] -> Blocks
toPandocList style items =
  let items' = map (foldMap jiraToPandocBlocks) items
  in if style == Jira.Enumeration
     then orderedList items'
     else bulletList items'

-- | Create a pandoc @'CodeBlock'@
toPandocCodeBlocks :: Maybe Jira.Language -> [Jira.Parameter] -> Text -> Blocks
toPandocCodeBlocks langMay params txt =
  let classes = case langMay of
                  Just (Jira.Language lang) -> [lang]
                  Nothing                   -> []
  in codeBlockWith ("", classes, map paramToPair params) txt

-- | Create a pandoc @'Div'@ from a panel.
toPandocDiv :: [Jira.Parameter] -> [Jira.Block] -> Blocks
toPandocDiv params =
  divWith ("", ["panel"], map paramToPair params) . foldMap jiraToPandocBlocks

paramToPair :: Jira.Parameter -> (Text, Text)
paramToPair (Jira.Parameter key value) = (key, value)

-- | Give textual representation of a color.
colorName :: Jira.ColorName -> Text
colorName (Jira.ColorName name) = name

-- | Create a pandoc @'Table'@.
-- This relies on 'simpleTable' to sanitize the table.
toPandocTable :: [Jira.Row] -> Blocks
toPandocTable rows =
  let (headerRow, bodyRows) = splitIntoHeaderAndBody rows
  in simpleTable
       (rowToBlocksList headerRow)
       (map rowToBlocksList bodyRows)

rowToBlocksList :: Jira.Row -> [Blocks]
rowToBlocksList (Jira.Row cells) =
  map cellContent cells
  where
    cellContent cell = let content = case cell of
                             Jira.HeaderCell x -> x
                             Jira.BodyCell x   -> x
                       in foldMap jiraToPandocBlocks content

splitIntoHeaderAndBody :: [Jira.Row] -> (Jira.Row, [Jira.Row])
splitIntoHeaderAndBody [] = (Jira.Row [], [])
splitIntoHeaderAndBody rows@(first@(Jira.Row cells) : rest) =
  let isHeaderCell Jira.HeaderCell{} = True
      isHeaderCell Jira.BodyCell{}   = False
  in if all isHeaderCell cells
     then (first, rest)
     else (Jira.Row [], rows)

--
-- Inlines
--

-- | Converts a Jira inline to a Pandoc block.
jiraToPandocInlines :: Jira.Inline -> Inlines
jiraToPandocInlines = \case
  Jira.Anchor t          -> spanWith (t, [], []) mempty
  Jira.AutoLink url      -> link (Jira.fromURL url) "" (str (Jira.fromURL url))
  Jira.Citation ils      -> str "—" <> space <> emph (fromInlines ils)
  Jira.ColorInline c ils -> spanWith ("", [], [("color", colorName c)]) $
                                     fromInlines ils
  Jira.Emoji icon        -> str . iconUnicode $ icon
  Jira.Entity entity     -> str . fromEntity $ entity
  Jira.Image params url  -> let (title, attr) = imgParams params
                            in imageWith attr (Jira.fromURL url) title mempty
  Jira.Link lt alias url -> jiraLinkToPandoc lt alias url
  Jira.Linebreak         -> linebreak
  Jira.Monospaced inlns  -> code . stringify . toList . fromInlines $ inlns
  Jira.Space             -> space
  Jira.SpecialChar c     -> str (Data.Text.singleton c)
  Jira.Str t             -> str t
  Jira.Styled style inlns -> fromStyle style $ fromInlines inlns
  where
    fromInlines  = foldMap jiraToPandocInlines
    fromEntity e = case lookupEntity (unpack e ++ ";") of
                     Nothing -> "&" `append` e `append` ";"
                     Just cs -> pack cs

    fromStyle = \case
      Jira.Emphasis    -> emph
      Jira.Insert      -> underline
      Jira.Strikeout   -> strikeout
      Jira.Strong      -> strong
      Jira.Subscript   -> subscript
      Jira.Superscript -> superscript

    imgParams :: [Jira.Parameter] -> (Text, Attr)
    imgParams = foldr addImgParam ("", ("", [], []))

    addImgParam :: Jira.Parameter -> (Text, Attr) -> (Text, Attr)
    addImgParam p (title, attr@(ident, classes, kvs)) =
      case Jira.parameterKey p of
        "title"     -> (Jira.parameterValue p, attr)
        "thumbnail" -> (title, (ident, "thumbnail":classes, kvs))
        _           -> let kv = (Jira.parameterKey p, Jira.parameterValue p)
                       in (title, (ident, classes, kv:kvs))

-- | Convert a Jira link to pandoc inlines.
jiraLinkToPandoc :: Jira.LinkType -> [Jira.Inline] -> Jira.URL -> Inlines
jiraLinkToPandoc linkType alias url =
  let url' = (if linkType == Jira.User then ("~" <>) else id) $ Jira.fromURL url
      alias' = case alias of
                 [] -> str url'
                 _  -> foldMap jiraToPandocInlines alias
  in case linkType of
    Jira.External   -> link url' "" alias'
    Jira.Email      -> link ("mailto:" <> url') "" alias'
    Jira.Attachment -> linkWith ("", ["attachment"], []) url' "" alias'
    Jira.User       -> linkWith ("", ["user-account"], []) url' "" alias'
    Jira.SmartCard  -> linkWith ("", ["smart-card"], []) url' "" alias'
    Jira.SmartLink  -> linkWith ("", ["smart-link"], []) url' "" alias'

-- | Get unicode representation of a Jira icon.
iconUnicode :: Jira.Icon -> Text
iconUnicode = \case
  Jira.IconSlightlySmiling -> "🙂"
  Jira.IconFrowning        -> "🙁"
  Jira.IconTongue          -> "😛"
  Jira.IconSmiling         -> "😃"
  Jira.IconWinking         -> "😉"
  Jira.IconThumbsUp        -> "👍"
  Jira.IconThumbsDown      -> "👎"
  Jira.IconInfo            -> "ℹ"
  Jira.IconCheckmark       -> "✔"
  Jira.IconX               -> "❌"
  Jira.IconAttention       -> "❗"
  Jira.IconPlus            -> "➕"
  Jira.IconMinus           -> "➖"
  Jira.IconQuestionmark    -> "❓"
  Jira.IconOn              -> "💡"
  Jira.IconOff             -> "🌙"
  Jira.IconStar            -> "⭐"
  Jira.IconStarRed         -> "⭐"
  Jira.IconStarGreen       -> "⭐"
  Jira.IconStarBlue        -> "⭐"
  Jira.IconStarYellow      -> "⭐"
  Jira.IconFlag            -> "⚑"
  Jira.IconFlagOff         -> "⚐"
