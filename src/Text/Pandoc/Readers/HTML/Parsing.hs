{-# LANGUAGE OverloadedStrings     #-}
{- |
   Module      : Text.Pandoc.Readers.HTML.Parsing
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Parsing functions and utilities.
-}
module Text.Pandoc.Readers.HTML.Parsing
  ( pInTags
  , pInTags'
  , pInTag
  , pAny
  , pCloses
  , pSatisfy
  , pBlank
  , matchTagClose
  , matchTagOpen
  , isSpace
  , toAttr
  , toStringAttr
  )
where

import Control.Monad (guard, void, mzero)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.HTML.TagSoup
  ( Tag (..), (~==), isTagText, isTagPosition, isTagOpen, isTagClose )
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Definition (Attr)
import Text.Pandoc.Parsing
  ( (<|>), eof, getPosition, lookAhead, manyTill, newPos, optional
  , skipMany, setPosition, token, try)
import Text.Pandoc.Readers.HTML.TagCategories
import Text.Pandoc.Readers.HTML.Types
import Text.Pandoc.Shared (tshow)
import Text.Pandoc.XML (html5Attributes, html4Attributes, rdfaAttributes)
import qualified Data.Set as Set
import qualified Data.Text as T

pInTags :: (PandocMonad m, Monoid a) => Text -> TagParser m a -> TagParser m a
pInTags tagtype parser = pInTags' tagtype (const True) parser

pInTags' :: (PandocMonad m, Monoid a)
         => Text
         -> (Tag Text -> Bool)
         -> TagParser m a
         -> TagParser m a
pInTags' tagtype tagtest parser = try $ do
  pSatisfy (\t -> t ~== TagOpen tagtype [] && tagtest t)
  mconcat <$> manyTill parser (pCloses tagtype <|> eof)

-- parses p, preceded by an opening tag (optional if tagsOptional)
-- and followed by a closing tag (optional if tagsOptional)
pInTag :: PandocMonad m => Bool -> Text -> TagParser m a -> TagParser m a
pInTag tagsOptional tagtype p = try $ do
  skipMany pBlank
  (if tagsOptional then optional else void) $ pSatisfy (matchTagOpen tagtype [])
  skipMany pBlank
  x <- p
  skipMany pBlank
  (if tagsOptional then optional else void) $ pSatisfy (matchTagClose tagtype)
  skipMany pBlank
  return x

pCloses :: PandocMonad m => Text -> TagParser m ()
pCloses tagtype = try $ do
  t <- lookAhead $ pSatisfy $ \tag -> isTagClose tag || isTagOpen tag
  case t of
       (TagClose t') | t' == tagtype -> void pAny
       (TagOpen t' _) | t' `closes` tagtype -> return ()
       (TagClose "ul") | tagtype == "li" -> return ()
       (TagClose "ol") | tagtype == "li" -> return ()
       (TagClose "dl") | tagtype == "dd" -> return ()
       (TagClose "table") | tagtype == "td" -> return ()
       (TagClose "table") | tagtype == "th" -> return ()
       (TagClose "table") | tagtype == "tr" -> return ()
       (TagClose "td") | tagtype `Set.member` blockHtmlTags -> return ()
       (TagClose "th") | tagtype `Set.member` blockHtmlTags -> return ()
       (TagClose t') | tagtype == "p" && t' `Set.member` blockHtmlTags
                                            -> return () -- see #3794
       _ -> mzero

pBlank :: PandocMonad m => TagParser m ()
pBlank = try $ do
  (TagText str) <- pSatisfy isTagText
  guard $ T.all isSpace str

pLocation :: PandocMonad m => TagParser m ()
pLocation = do
  (TagPosition r c) <- pSat isTagPosition
  setPosition $ newPos "input" r c

pSat :: PandocMonad m => (Tag Text -> Bool) -> TagParser m (Tag Text)
pSat f = do
  pos <- getPosition
  token tshow (const pos) (\x -> if f x then Just x else Nothing)

pSatisfy :: PandocMonad m => (Tag Text -> Bool) -> TagParser m (Tag Text)
pSatisfy f = try $ optional pLocation >> pSat f

matchTagClose :: Text -> (Tag Text -> Bool)
matchTagClose t = (~== TagClose t)

matchTagOpen :: Text -> [(Text, Text)] -> (Tag Text -> Bool)
matchTagOpen t as = (~== TagOpen t as)

pAny :: PandocMonad m => TagParser m (Tag Text)
pAny = pSatisfy (const True)

isSpace :: Char -> Bool
isSpace ' '  = True
isSpace '\t' = True
isSpace '\n' = True
isSpace '\r' = True
isSpace _    = False

-- taken from HXT and extended
-- See http://www.w3.org/TR/html5/syntax.html sec 8.1.2.4 optional tags
closes :: Text -> Text -> Bool
_ `closes` "body" = False
_ `closes` "html" = False
"body" `closes` "head" = True
"a" `closes` "a" = True
"li" `closes` "li" = True
"th" `closes` t | t `elem` ["th","td"] = True
"td" `closes` t | t `elem` ["th","td"] = True
"tr" `closes` t | t `elem` ["th","td","tr"] = True
"dd" `closes` t | t `elem` ["dt", "dd"] = True
"dt" `closes` t | t `elem` ["dt","dd"] = True
"rt" `closes` t | t `elem` ["rb", "rt", "rtc"] = True
"optgroup" `closes` "optgroup" = True
"optgroup" `closes` "option" = True
"option" `closes` "option" = True
-- https://html.spec.whatwg.org/multipage/syntax.html#optional-tags
x `closes` "p" | x `elem` ["address", "article", "aside", "blockquote",
   "dir", "div", "dl", "fieldset", "footer", "form", "h1", "h2", "h3", "h4",
   "h5", "h6", "header", "hr", "main", "menu", "nav", "ol", "p", "pre", "section",
   "table", "ul"] = True
_ `closes` "meta" = True
"form" `closes` "form" = True
"label" `closes` "label" = True
"map" `closes` "map" = True
"object" `closes` "object" = True
_ `closes` t | t `elem` ["option","style","script","textarea","title"] = True
t `closes` "select" | t /= "option" = True
"thead" `closes` "colgroup" = True
"tfoot" `closes` t | t `elem` ["thead","colgroup"] = True
"tbody" `closes` t | t `elem` ["tbody","tfoot","thead","colgroup"] = True
t `closes` t2 |
   t `elem` ["h1","h2","h3","h4","h5","h6","dl","ol","ul","table","div","main","p"] &&
   t2 `elem` ["h1","h2","h3","h4","h5","h6","p" ] = True -- not "div" or "main"
t1 `closes` t2 |
   t1 `Set.member` blockTags &&
   t2 `Set.notMember` blockTags &&
   t2 `Set.notMember` eitherBlockOrInline = True
_ `closes` _ = False

toStringAttr :: [(Text, Text)] -> [(Text, Text)]
toStringAttr = map go
  where
   go (x,y) =
     case T.stripPrefix "data-" x of
       Just x' | x' `Set.notMember` (html5Attributes <>
                                     html4Attributes <> rdfaAttributes)
         -> (x',y)
       _ -> (x,y)

mkAttr :: [(Text, Text)] -> Attr
mkAttr attr = (attribsId, attribsClasses, attribsKV)
  where attribsId = fromMaybe "" $ lookup "id" attr
        attribsClasses = T.words (fromMaybe "" $ lookup "class" attr) <> epubTypes
        attribsKV = filter (\(k,_) -> k /= "class" && k /= "id") attr
        epubTypes = T.words $ fromMaybe "" $ lookup "epub:type" attr

toAttr :: [(Text, Text)] -> Attr
toAttr = mkAttr . toStringAttr
