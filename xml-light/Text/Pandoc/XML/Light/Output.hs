{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.XML.Light.Output
   Copyright   : Copyright (C) 2007 Galois, Inc., 2021-2024 John MacFarlane
   License     : GNU GPL, version 2 or above


   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

   This code is based on code from xml-light, released under the BSD3 license.
   We use a TextBuilder (from the text-builder package) instead of ShowS.
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

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import TextBuilder (TextBuilder, char, text, toText)
import Text.Pandoc.XML.Light.Types

--
-- duplicates functions from Text.XML.Output
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
ppcElement c        = toText . ppElementS c mempty

-- | Pretty printing content
ppcContent         :: ConfigPP -> Content -> Text
ppcContent c        = toText . ppContentS c mempty

type Indent = TextBuilder

-- | Pretty printing content using ShowT
ppContentS         :: ConfigPP -> Indent -> Content -> TextBuilder
ppContentS c i x = case x of
                     Elem e -> ppElementS c i e
                     Text t -> ppCDataS c i t
                     CRef r -> showCRefS r

ppElementS         :: ConfigPP -> Indent -> Element -> TextBuilder
ppElementS c i e = i <> tagStart (elName e) (elAttribs e) <>
  (case elContent e of
    [] | "?" `T.isPrefixOf` qName name -> text " ?>"
       | shortEmptyTag c name  -> text " />"
    [Text t] -> char '>' <> ppCDataS c mempty t <> tagEnd name
    cs -> char '>' <> nl <>
          mconcat (map ((<> nl) . ppContentS c (sp <> i)) cs) <>
          i <> tagEnd name
      where (nl,sp)  = if prettify c
                          then (text "\n", text "  ")
                          else (mempty, mempty)
  )
  where name = elName e

ppCDataS           :: ConfigPP -> Indent -> CData -> TextBuilder
ppCDataS c i t     = i <> if cdVerbatim t /= CDataText || not (prettify c)
                             then showCDataS t
                             -- add indentation after newlines; escaping
                             -- neither adds nor removes newlines, so we
                             -- can split the unescaped text
                             else mconcat
                                  (intersperse (char '\n' <> i)
                                    (map escStr
                                      (T.split (=='\n') (cdData t))))



--------------------------------------------------------------------------------

-- | Adds the <?xml?> header.
showTopElement     :: Element -> Text
showTopElement c    = xmlHeader <> showElement c

showContent        :: Content -> Text
showContent         = ppcContent defaultConfigPP

showElement        :: Element -> Text
showElement         = ppcElement defaultConfigPP

-- Note: crefs should not contain '&', ';', etc.
showCRefS          :: Text -> TextBuilder
showCRefS r         = char '&' <> text r <> char ';'

-- | Convert a text element to characters.
showCDataS         :: CData -> TextBuilder
showCDataS cd =
 case cdVerbatim cd of
   CDataText     -> escStr (cdData cd)
   CDataVerbatim -> text "<![CDATA[" <> escCData (cdData cd) <>
                    text "]]>"
   CDataRaw      -> text (cdData cd)

--------------------------------------------------------------------------------
escCData           :: Text -> TextBuilder
escCData t =
  case T.breakOn "]]>" t of
    (chunk, rest)
      | T.null rest -> text chunk
      | otherwise   -> text chunk <> text "]]]]><![CDATA[>" <>
                       escCData (T.drop 3 rest)

escChar            :: Char -> TextBuilder
escChar c = case c of
  '<'   -> text "&lt;"
  '>'   -> text "&gt;"
  '&'   -> text "&amp;"
  '"'   -> text "&quot;"
  -- we use &#39 instead of &apos; because IE apparently has difficulties
  -- rendering &apos; in xhtml.
  -- Reported by Rohan Drape <rohan.drape@gmail.com>.
  '\''  -> text "&#39;"
  _     -> char c

  {- original xml-light version:
  -- NOTE: We escape '\r' explicitly because otherwise they get lost
  -- when parsed back in because of then end-of-line normalization rules.
  _ | isPrint c || c == '\n' -> singleton c
    | otherwise -> showText "&#" . showsT oc . singleton ';'
      where oc = ord c
  -}

escStr             :: Text -> TextBuilder
escStr cs          = case T.break needsEscape cs of
                       (chunk, rest) ->
                         case T.uncons rest of
                           Nothing -> text chunk
                           Just (c, rest') ->
                             text chunk <> escChar c <> escStr rest'
 where
  needsEscape '<' = True
  needsEscape '>' = True
  needsEscape '&' = True
  needsEscape '"' = True
  needsEscape '\'' = True
  needsEscape _ = False

tagEnd             :: QName -> TextBuilder
tagEnd qn           = text "</" <> showQName qn <> char '>'

tagStart           :: QName -> [Attr] -> TextBuilder
tagStart qn as      = char '<' <> showQName qn <> as_str
 where as_str       = if null as
                         then mempty
                         else mconcat (map showAttr as)

showAttr           :: Attr -> TextBuilder
showAttr (Attr qn v) = char ' ' <> showQName qn <>
                       char '=' <>
                       char '"' <> escStr v <> char '"'

showQName          :: QName -> TextBuilder
showQName q         =
  case qPrefix q of
    Nothing -> text (qName q)
    Just p  -> text p <> char ':' <> text (qName q)
