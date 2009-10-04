{-
Copyright (C) 2007 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Readers.TeXMath
   Copyright   : Copyright (C) 2007 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of TeX math to a list of 'Pandoc' inline elements.
-}
module Text.Pandoc.Readers.TeXMath ( 
                                     readTeXMath 
                                   ) where

import Text.ParserCombinators.Parsec
import Text.Pandoc.Definition

-- | Converts a string of raw TeX math to a list of 'Pandoc' inlines. 
readTeXMath :: String    -- ^ String to parse (assumes @'\n'@ line endings)
            -> [Inline]
readTeXMath inp = case parse teXMath ("formula: " ++ inp) inp of
   Left _    -> [Str inp]  -- if unparseable, just include original
   Right res -> res

teXMath :: GenParser Char st [Inline]
teXMath = manyTill mathPart eof >>= return . concat

mathPart :: GenParser Char st [Inline]
mathPart = whitespace <|> superscript <|> subscript <|> symbol <|> 
           argument <|> digits <|> letters <|> misc

whitespace :: GenParser Char st [Inline]
whitespace = many1 space >> return []

symbol :: GenParser Char st [Inline]
symbol = try $ do
  char '\\'
  res <- many1 letter
  case lookup res teXsymbols of
    Just m  -> return [Str m]
    Nothing -> return [Str $ "\\" ++ res]

argument :: GenParser Char st [Inline]
argument = try $ do
  char '{'
  res <- many mathPart
  char '}'
  return $ if null res 
              then [Str " "]
              else [Str "{"] ++ concat res ++ [Str "}"]

digits :: GenParser Char st [Inline]
digits = do
  res <- many1 digit
  return [Str res]

letters :: GenParser Char st [Inline]
letters = do
  res <- many1 letter
  return [Emph [Str res]]

misc :: GenParser Char st [Inline]
misc = do
  res <- noneOf "}"
  return [Str [res]] 

scriptArg :: GenParser Char st [Inline]
scriptArg = try $ do
  (try (do{char '{'; r <- many mathPart; char '}'; return $ concat r}))
   <|> symbol
   <|> (do{c <- (letter <|> digit); return [Str [c]]})
  
superscript :: GenParser Char st [Inline]
superscript = try $ do
  char '^'
  arg <- scriptArg
  return [Superscript arg]

subscript :: GenParser Char st [Inline]
subscript = try $ do
  char '_'
  arg <- scriptArg
  return [Subscript arg]
 
withThinSpace :: String -> String
withThinSpace str = "\x2009" ++ str ++ "\x2009"

teXsymbols :: [(String, String)]
teXsymbols = 
 [("alpha","\x3B1")
 ,("beta", "\x3B2")
 ,("chi", "\x3C7")
 ,("delta", "\x3B4")
 ,("Delta", "\x394")
 ,("epsilon", "\x3B5")
 ,("varepsilon", "\x25B")
 ,("eta", "\x3B7")
 ,("gamma", "\x3B3")
 ,("Gamma", "\x393")
 ,("iota", "\x3B9")
 ,("kappa", "\x3BA")
 ,("lambda", "\x3BB")
 ,("Lambda", "\x39B")
 ,("mu", "\x3BC")
 ,("nu", "\x3BD")
 ,("omega", "\x3C9")
 ,("Omega", "\x3A9")
 ,("phi", "\x3C6")
 ,("varphi", "\x3D5")
 ,("Phi", "\x3A6")
 ,("pi", "\x3C0")
 ,("Pi", "\x3A0")
 ,("psi", "\x3C8")
 ,("Psi", "\x3A8")
 ,("rho", "\x3C1")
 ,("sigma", "\x3C3")
 ,("Sigma", "\x3A3")
 ,("tau", "\x3C4")
 ,("theta", "\x3B8")
 ,("vartheta", "\x3D1")
 ,("Theta", "\x398")
 ,("upsilon", "\x3C5")
 ,("xi", "\x3BE")
 ,("Xi", "\x39E")
 ,("zeta", "\x3B6")
 ,("ne", "\x2260")
 ,("lt", withThinSpace "<")
 ,("le", withThinSpace "\x2264")
 ,("leq", withThinSpace "\x2264")
 ,("ge", withThinSpace "\x2265")
 ,("geq", withThinSpace "\x2265")
 ,("prec", withThinSpace "\x227A")
 ,("succ", withThinSpace "\x227B")
 ,("preceq", withThinSpace "\x2AAF")
 ,("succeq", withThinSpace "\x2AB0")
 ,("in", withThinSpace "\x2208")
 ,("notin", withThinSpace "\x2209")
 ,("subset", withThinSpace "\x2282")
 ,("supset", withThinSpace "\x2283")
 ,("subseteq", withThinSpace "\x2286")
 ,("supseteq", withThinSpace "\x2287")
 ,("equiv", withThinSpace "\x2261")
 ,("cong", withThinSpace "\x2245")
 ,("approx", withThinSpace "\x2248")
 ,("propto", withThinSpace "\x221D")
 ,("cdot", withThinSpace "\x22C5")
 ,("star", withThinSpace "\x22C6")
 ,("backslash", "\\")
 ,("times", withThinSpace "\x00D7")
 ,("divide", withThinSpace "\x00F7")
 ,("circ", withThinSpace "\x2218")
 ,("oplus", withThinSpace "\x2295")
 ,("otimes", withThinSpace "\x2297")
 ,("odot", withThinSpace "\x2299")
 ,("sum", "\x2211")
 ,("prod", "\x220F")
 ,("wedge", withThinSpace "\x2227")
 ,("bigwedge", withThinSpace "\x22C0")
 ,("vee", withThinSpace "\x2228")
 ,("bigvee", withThinSpace "\x22C1")
 ,("cap", withThinSpace "\x2229")
 ,("bigcap", withThinSpace "\x22C2")
 ,("cup", withThinSpace "\x222A")
 ,("bigcup", withThinSpace "\x22C3")
 ,("neg", "\x00AC")
 ,("implies", withThinSpace "\x21D2")
 ,("iff", withThinSpace "\x21D4")
 ,("forall", "\x2200")
 ,("exists", "\x2203")
 ,("bot", "\x22A5")
 ,("top", "\x22A4")
 ,("vdash", "\x22A2")
 ,("models", withThinSpace "\x22A8")
 ,("uparrow", "\x2191")
 ,("downarrow", "\x2193")
 ,("rightarrow", withThinSpace "\x2192")
 ,("to", withThinSpace "\x2192")
 ,("rightarrowtail", "\x21A3")
 ,("twoheadrightarrow", withThinSpace "\x21A0")
 ,("twoheadrightarrowtail", withThinSpace "\x2916")
 ,("mapsto", withThinSpace "\x21A6")
 ,("leftarrow", withThinSpace "\x2190")
 ,("leftrightarrow", withThinSpace "\x2194")
 ,("Rightarrow", withThinSpace "\x21D2")
 ,("Leftarrow", withThinSpace "\x21D0")
 ,("Leftrightarrow", withThinSpace "\x21D4")
 ,("partial", "\x2202")
 ,("nabla", "\x2207")
 ,("pm", "\x00B1")
 ,("emptyset", "\x2205")
 ,("infty", "\x221E")
 ,("aleph", "\x2135")
 ,("ldots", "...")
 ,("therefore", "\x2234")
 ,("angle", "\x2220")
 ,("quad", "\x00A0\x00A0")
 ,("cdots", "\x22EF")
 ,("vdots", "\x22EE")
 ,("ddots", "\x22F1")
 ,("diamond", "\x22C4")
 ,("Box", "\x25A1")
 ,("lfloor", "\x230A")
 ,("rfloor", "\x230B")
 ,("lceiling", "\x2308")
 ,("rceiling", "\x2309")
 ,("langle", "\x2329")
 ,("rangle", "\x232A")
 ,("{", "{")
 ,("}", "}")
 ,("[", "[")
 ,("]", "]")
 ,("|", "|")
 ,("||", "||")
 ]

