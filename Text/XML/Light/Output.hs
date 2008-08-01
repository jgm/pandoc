--------------------------------------------------------------------
-- |
-- Module    : Text.XML.Light.Output
-- Copyright : (c) Galois, Inc. 2007
-- License   : BSD3
--
-- Maintainer: Iavor S. Diatchki <diatchki@galois.com>
-- Stability : provisional
-- Portability:
--
-- Output handling for the lightweight XML lib.
--

module Text.XML.Light.Output
  ( showTopElement, showContent, showElement, showCData, showQName, showAttr
  , ppTopElement, ppContent, ppElement
  , tagEnd, xml_header
  ) where

import Text.XML.Light.Types
import Data.Char
import Data.List ( isPrefixOf )

-- | The XML 1.0 header
xml_header :: String
xml_header = "<?xml version='1.0' ?>"

-- | Pretty printing renders XML documents faithfully,
-- with the exception that whitespace may be added\/removed
-- in non-verbatim character data.
ppTopElement       :: Element -> String
ppTopElement e      = unlines [xml_header,ppElement e]

-- | Pretty printing elements
ppElement          :: Element -> String
ppElement e         = ppElementS "" e ""

-- | Pretty printing content
ppContent          :: Content -> String
ppContent x         = ppContentS "" x ""

-- | Pretty printing content using ShowS
ppContentS         :: String -> Content -> ShowS
ppContentS i x xs   = case x of
                        Elem e -> ppElementS i e xs
                        Text c -> ppCData i c xs
                        CRef r -> showCRefS r xs

ppElementS         :: String -> Element -> ShowS
ppElementS i e xs   = i ++ (tagStart (elName e) (elAttribs e) $
  case elContent e of
    [] 
     | not ("?xml" `isPrefixOf` (qName $ elName e)) -> " />" ++ xs
     | otherwise -> " ?>" ++ xs
    [Text t] -> ">" ++ ppCData "" t (tagEnd (elName e) xs)
    cs -> ">\n" ++ foldr ppSub (i ++ tagEnd (elName e) xs) cs
      where ppSub e1 = ppContentS ("  " ++ i) e1 . showChar '\n'
  )

ppCData            :: String -> CData -> ShowS
ppCData i c xs      = i ++ if (cdVerbatim c /= CDataText )
                              then showCDataS c xs
                              else foldr cons xs (showCData c)

  where cons         :: Char -> String -> String
        cons '\n' ys  = "\n" ++ i ++ ys
        cons y ys     = y : ys



--------------------------------------------------------------------------------
-- | Adds the <?xml?> header.
showTopElement     :: Element -> String
showTopElement c    = xml_header ++ showElement c

showContent        :: Content -> String
showContent c       = showContentS c ""

showElement        :: Element -> String
showElement c       = showElementS c ""

showCData          :: CData -> String
showCData c         = showCDataS c ""

-- Note: crefs should not contain '&', ';', etc.
showCRefS          :: String -> ShowS
showCRefS r xs      = '&' : r ++ ';' : xs

-- | Good for transmition (no extra white space etc.) but less readable.
showContentS           :: Content -> ShowS
showContentS (Elem e)   = showElementS e
showContentS (Text cs)  = showCDataS cs
showContentS (CRef cs)  = showCRefS cs

-- | Good for transmition (no extra white space etc.) but less readable.
showElementS       :: Element -> ShowS
showElementS e xs =
  tagStart (elName e) (elAttribs e)
    $ case elContent e of
        [] -> " />" ++ xs
        ch -> '>' : foldr showContentS (tagEnd (elName e) xs) ch

-- | Convert a text element to characters.
showCDataS         :: CData -> ShowS
showCDataS cd = 
 case cdVerbatim cd of
   CDataText     -> escStr (cdData cd)
   CDataVerbatim -> showString "<![CDATA[" . escCData (cdData cd) . showString "]]>"
   CDataRaw      -> \ xs -> cdData cd ++ xs

--------------------------------------------------------------------------------
escCData           :: String -> ShowS
escCData (']' : ']' : '>' : cs) = showString "]]]]><![CDATA[>" . escCData cs
escCData (c : cs)               = showChar c . escCData cs
escCData []                     = id

escChar            :: Char -> ShowS
escChar c = case c of
  '<'   -> showString "&lt;"
  '>'   -> showString "&gt;"
  '&'   -> showString "&amp;"
  '"'   -> showString "&quot;"
  '\''  -> showString "&apos;"
  -- XXX: Is this really wortherd?
  -- We could deal with these issues when we convert characters to bytes.
  _ | (oc <= 0x7f && isPrint c) || c == '\n' || c == '\r' -> showChar c
    | otherwise -> showString "&#" . shows oc . showChar ';'
      where oc = ord c

escStr             :: String -> ShowS
escStr cs rs        = foldr escChar rs cs

tagEnd             :: QName -> ShowS
tagEnd qn rs        = '<':'/':showQName qn ++ '>':rs

tagStart           :: QName -> [Attr] -> ShowS
tagStart qn as rs   = '<':showQName qn ++ as_str ++ rs
 where as_str       = if null as then "" else ' ' : unwords (map showAttr as)

showAttr           :: Attr -> String
showAttr (Attr qn v) = showQName qn ++ '=' : '"' : escStr v "\""

showQName          :: QName -> String
showQName q         = pre ++ qName q
  where pre = case qPrefix q of
                Nothing -> ""
                Just p  -> p ++ ":"



