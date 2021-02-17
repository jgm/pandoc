{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.XML.Light.Output
   Copyright   : Copyright (C) 2007 Galois, Inc., 2021 John MacFarlane
   License     : GNU GPL, version 2 or above


   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

   This code is based on code from xml-light, released under the BSD3 license.
   We use a text Builder instead of ShowS.
-}
module Text.Pandoc.XML.Light.Output
  ( -- * Replacement for xml-light's Text.XML.Output
    ppTopElement
  , ppElement
  , ppContent
  , ppcElement
  , ppcContent
  , showTopElement
  , showElement
  , showContent
  , useShortEmptyTags
  , defaultConfigPP
  , ConfigPP(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, singleton, fromText, toLazyText)
import Text.Pandoc.XML.Light.Types

--
-- duplicates functinos from Text.XML.Output
--

-- | The XML 1.0 header
xmlHeader :: Text
xmlHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"


--------------------------------------------------------------------------------
data ConfigPP = ConfigPP
  { shortEmptyTag :: QName -> Bool
  , prettify      :: Bool
  }

-- | Default pretty orinting configuration.
--  * Always use abbreviate empty tags.
defaultConfigPP :: ConfigPP
defaultConfigPP = ConfigPP { shortEmptyTag = const True
                           , prettify      = False
                           }

-- | The predicate specifies for which empty tags we should use XML's
-- abbreviated notation <TAG />.  This is useful if we are working with
-- some XML-ish standards (such as certain versions of HTML) where some
-- empty tags should always be displayed in the <TAG></TAG> form.
useShortEmptyTags :: (QName -> Bool) -> ConfigPP -> ConfigPP
useShortEmptyTags p c = c { shortEmptyTag = p }


-- | Specify if we should use extra white-space to make document more readable.
-- WARNING: This adds additional white-space to text elements,
-- and so it may change the meaning of the document.
useExtraWhiteSpace :: Bool -> ConfigPP -> ConfigPP
useExtraWhiteSpace p c  = c { prettify = p }

-- | A configuration that tries to make things pretty
-- (possibly at the cost of changing the semantics a bit
-- through adding white space.)
prettyConfigPP     :: ConfigPP
prettyConfigPP      = useExtraWhiteSpace True defaultConfigPP


--------------------------------------------------------------------------------


-- | Pretty printing renders XML documents faithfully,
-- with the exception that whitespace may be added\/removed
-- in non-verbatim character data.
ppTopElement       :: Element -> Text
ppTopElement        = ppcTopElement prettyConfigPP

-- | Pretty printing elements
ppElement          :: Element -> Text
ppElement           = ppcElement prettyConfigPP

-- | Pretty printing content
ppContent          :: Content -> Text
ppContent           = ppcContent prettyConfigPP

-- | Pretty printing renders XML documents faithfully,
-- with the exception that whitespace may be added\/removed
-- in non-verbatim character data.
ppcTopElement      :: ConfigPP -> Element -> Text
ppcTopElement c e   = T.unlines [xmlHeader,ppcElement c e]

-- | Pretty printing elements
ppcElement         :: ConfigPP -> Element -> Text
ppcElement c        = TL.toStrict . toLazyText . ppElementS c mempty

-- | Pretty printing content
ppcContent         :: ConfigPP -> Content -> Text
ppcContent c        = TL.toStrict . toLazyText . ppContentS c mempty

ppcCData           :: ConfigPP -> CData -> Text
ppcCData c         = TL.toStrict . toLazyText . ppCDataS c mempty

type Indent = Builder

-- | Pretty printing content using ShowT
ppContentS         :: ConfigPP -> Indent -> Content -> Builder
ppContentS c i x = case x of
                     Elem e -> ppElementS c i e
                     Text t -> ppCDataS c i t
                     CRef r -> showCRefS r

ppElementS         :: ConfigPP -> Indent -> Element -> Builder
ppElementS c i e = i <> tagStart (elName e) (elAttribs e) <>
  (case elContent e of
    [] | "?" `T.isPrefixOf` qName name -> fromText " ?>"
       | shortEmptyTag c name  -> fromText " />"
    [Text t] -> singleton '>' <> ppCDataS c mempty t <> tagEnd name
    cs -> singleton '>' <> nl <>
          mconcat (map ((<> nl) . ppContentS c (sp <> i)) cs) <>
          i <> tagEnd name
      where (nl,sp)  = if prettify c then ("\n","  ") else ("","")
  )
  where name = elName e

ppCDataS           :: ConfigPP -> Indent -> CData -> Builder
ppCDataS c i t     = i <> if cdVerbatim t /= CDataText || not (prettify c)
                             then showCDataS t
                             else foldr cons mempty (T.unpack (showCData t))
  where cons         :: Char -> Builder -> Builder
        cons '\n' ys  = singleton '\n' <> i <> ys
        cons y ys     = singleton y <> ys



--------------------------------------------------------------------------------

-- | Adds the <?xml?> header.
showTopElement     :: Element -> Text
showTopElement c    = xmlHeader <> showElement c

showContent        :: Content -> Text
showContent         = ppcContent defaultConfigPP

showElement        :: Element -> Text
showElement         = ppcElement defaultConfigPP

showCData          :: CData -> Text
showCData           = ppcCData defaultConfigPP

-- Note: crefs should not contain '&', ';', etc.
showCRefS          :: Text -> Builder
showCRefS r         = singleton '&' <> fromText r <> singleton ';'

-- | Convert a text element to characters.
showCDataS         :: CData -> Builder
showCDataS cd =
 case cdVerbatim cd of
   CDataText     -> escStr (cdData cd)
   CDataVerbatim -> fromText "<![CDATA[" <> escCData (cdData cd) <>
                    fromText "]]>"
   CDataRaw      -> fromText (cdData cd)

--------------------------------------------------------------------------------
escCData           :: Text -> Builder
escCData t
  | "]]>" `T.isPrefixOf` t =
     fromText "]]]]><![CDATA[>" <> fromText (T.drop 3 t)
escCData t
  = case T.uncons t of
      Nothing     -> mempty
      Just (c,t') -> singleton c <> escCData t'

escChar            :: Char -> Builder
escChar c = case c of
  '<'   -> fromText "&lt;"
  '>'   -> fromText "&gt;"
  '&'   -> fromText "&amp;"
  '"'   -> fromText "&quot;"
  -- we use &#39 instead of &apos; because IE apparently has difficulties
  -- rendering &apos; in xhtml.
  -- Reported by Rohan Drape <rohan.drape@gmail.com>.
  '\''  -> fromText "&#39;"
  _     -> singleton c

  {- original xml-light version:
  -- NOTE: We escape '\r' explicitly because otherwise they get lost
  -- when parsed back in because of then end-of-line normalization rules.
  _ | isPrint c || c == '\n' -> singleton c
    | otherwise -> showText "&#" . showsT oc . singleton ';'
      where oc = ord c
  -}

escStr             :: Text -> Builder
escStr cs          = if T.any needsEscape cs
                        then mconcat (map escChar (T.unpack cs))
                        else fromText cs
 where
  needsEscape '<' = True
  needsEscape '>' = True
  needsEscape '&' = True
  needsEscape '"' = True
  needsEscape '\'' = True
  needsEscape _ = False

tagEnd             :: QName -> Builder
tagEnd qn           = fromText "</" <> showQName qn <> singleton '>'

tagStart           :: QName -> [Attr] -> Builder
tagStart qn as      = singleton '<' <> showQName qn <> as_str
 where as_str       = if null as
                         then mempty
                         else mconcat (map showAttr as)

showAttr           :: Attr -> Builder
showAttr (Attr qn v) = singleton ' ' <> showQName qn <>
                       singleton '=' <>
                       singleton '"' <> escStr v <> singleton '"'

showQName          :: QName -> Builder
showQName q         =
  case qPrefix q of
    Nothing -> fromText (qName q)
    Just p  -> fromText p <> singleton ':' <> fromText (qName q)
