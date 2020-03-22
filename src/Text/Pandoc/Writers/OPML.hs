{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
{- |
   Module      : Text.Pandoc.Writers.OPML
   Copyright   : Copyright (C) 2013-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to OPML XML.
-}
module Text.Pandoc.Writers.OPML ( writeOPML) where
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Data.Time
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.DocLayout
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Writers.Markdown (writeMarkdown)
import Text.Pandoc.Writers.Shared
import Text.Pandoc.XML

-- | Convert Pandoc document to string in OPML format.
writeOPML :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeOPML opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
      meta' = B.setMeta "date" (B.str $ convertDate $ docDate meta) meta
  metadata <- metaToContext opts
              (fmap literal . writeMarkdown def . Pandoc nullMeta)
              (\ils -> literal . T.stripEnd <$>
                writeMarkdown def (Pandoc nullMeta [Plain ils]))
              meta'
  let blocks' = makeSections False (Just 1) blocks
  main <- (render colwidth . vcat) <$>
             mapM (blockToOPML opts) blocks'
  let context = defField "body" main metadata
  return $
    (if writerPreferAscii opts then toEntities else id) $
    case writerTemplate opts of
       Nothing  -> main
       Just tpl -> render colwidth $ renderTemplate tpl context


writeHtmlInlines :: PandocMonad m => [Inline] -> m Text
writeHtmlInlines ils =
  T.strip <$> writeHtml5String def (Pandoc nullMeta [Plain ils])

-- date format: RFC 822: Thu, 14 Jul 2005 23:41:05 GMT
showDateTimeRFC822 :: UTCTime -> Text
showDateTimeRFC822 = T.pack . formatTime defaultTimeLocale "%a, %d %b %Y %X %Z"

convertDate :: [Inline] -> Text
convertDate ils = maybe "" showDateTimeRFC822 $
  parseTimeM True defaultTimeLocale "%F" . T.unpack =<< normalizeDate (stringify ils)

-- | Convert a Block to OPML.
blockToOPML :: PandocMonad m => WriterOptions -> Block -> m (Doc Text)
blockToOPML opts (Div (_,"section":_,_) (Header _ _ title : xs)) = do
  let isSect (Div (_,"section":_,_) (Header{}:_)) = True
      isSect _ = False
  let (blocks, rest) = break isSect xs
  htmlIls <- writeHtmlInlines title
  md <- if null blocks
        then return mempty
        else writeMarkdown def $ Pandoc nullMeta blocks
  let attrs = ("text", htmlIls) :
              [("_note", T.stripEnd md) | not (null blocks)]
  rest' <- vcat <$> mapM (blockToOPML opts) rest
  return $ inTags True "outline" attrs rest'
blockToOPML _ _ = return empty
