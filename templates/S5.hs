{-
Copyright (C) 2006-7 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.S5
   Copyright   : Copyright (C) 2006-7 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Definitions for creation of S5 powerpoint-like HTML.
(See <http://meyerweb.com/eric/tools/s5/>.)
-}
module Text.Pandoc.Writers.S5 (
                -- * Strings
                s5Meta,
                s5Javascript,
                s5CSS,
                s5Links,
                -- * Functions
                writeS5,
                writeS5String,
                insertS5Structure
                ) where
import Text.Pandoc.Shared ( joinWithSep, WriterOptions )
import Text.Pandoc.Writers.HTML ( writeHtml, writeHtmlString )
import Text.Pandoc.Definition
import Text.XHtml.Strict

s5Meta :: String
s5Meta = "<!-- configuration parameters -->\n<meta name=\"defaultView\" content=\"slideshow\" />\n<meta name=\"controlVis\" content=\"hidden\" />\n"

s5Javascript :: String
s5Javascript = "<script type=\"text/javascript\">\n" ++ @slides.js.comment@ ++ @slides.js.packed@ ++ "</script>\n" 

s5CoreCSS :: String
s5CoreCSS = @s5-core.css@

s5FramingCSS :: String
s5FramingCSS = @framing.css@

s5PrettyCSS :: String
s5PrettyCSS = @pretty.css@

s5OperaCSS :: String
s5OperaCSS = @opera.css@

s5OutlineCSS :: String
s5OutlineCSS = @outline.css@

s5PrintCSS :: String
s5PrintCSS = @print.css@

s5CSS :: String
s5CSS = "<style type=\"text/css\" media=\"projection\" id=\"slideProj\">\n" ++ s5CoreCSS ++ "\n" ++ s5FramingCSS ++ "\n" ++ s5PrettyCSS ++ "\n</style>\n<style type=\"text/css\" media=\"projection\" id=\"operaFix\">\n" ++ s5OperaCSS ++ "\n</style>\n<style type=\"text/css\" media=\"screen\" id=\"outlineStyle\">\n" ++ s5OutlineCSS ++ "\n</style>\n<style type=\"text/css\" media=\"print\" id=\"slidePrint\">\n" ++ s5PrintCSS ++ "\n</style>\n"

s5Links :: String
s5Links = "<!-- style sheet links -->\n<link rel=\"stylesheet\" href=\"ui/default/slides.css\" type=\"text/css\" media=\"projection\" id=\"slideProj\" />\n<link rel=\"stylesheet\" href=\"ui/default/outline.css\" type=\"text/css\" media=\"screen\" id=\"outlineStyle\" />\n<link rel=\"stylesheet\" href=\"ui/default/print.css\" type=\"text/css\" media=\"print\" id=\"slidePrint\" />\n<link rel=\"stylesheet\" href=\"ui/default/opera.css\" type=\"text/css\" media=\"projection\" id=\"operaFix\" />\n<!-- S5 JS -->\n<script src=\"ui/default/slides.js\" type=\"text/javascript\"></script>\n"

-- | Converts Pandoc document to an S5 HTML presentation (Html structure).
writeS5 :: WriterOptions -> Pandoc -> Html
writeS5 options = (writeHtml options) . insertS5Structure

-- | Converts Pandoc document to an S5 HTML presentation (string).
writeS5String :: WriterOptions -> Pandoc -> String
writeS5String options = (writeHtmlString options) . insertS5Structure

-- | Inserts HTML needed for an S5 presentation (e.g. around slides).
layoutDiv :: [Inline]  -- ^ Title of document (for header or footer)
          -> String    -- ^ Date of document (for header or footer)
          -> [Block]   -- ^ List of block elements returned
layoutDiv title' date = [(RawHtml "<div class=\"layout\">\n<div id=\"controls\"></div>\n<div id=\"currentSlide\"></div>\n<div id=\"header\"></div>\n<div id=\"footer\">\n"), (Header 1 [Str date]), (Header 2 title'), (RawHtml "</div>\n</div>\n")]

presentationStart :: Block
presentationStart = RawHtml "<div class=\"presentation\">\n\n"

presentationEnd :: Block
presentationEnd = RawHtml "</div>\n"

slideStart :: Block
slideStart = RawHtml "<div class=\"slide\">\n"

slideEnd :: Block
slideEnd = RawHtml "</div>\n"

-- | Returns 'True' if block is a Header 1.
isH1 :: Block -> Bool
isH1 (Header 1 _) = True
isH1 _ = False 

-- | Insert HTML around sections to make individual slides.
insertSlides :: Bool -> [Block] -> [Block]
insertSlides beginning blocks = 
    let (beforeHead, rest) = break isH1 blocks in
    if (null rest) then 
        if beginning then
            beforeHead 
        else
            beforeHead ++ [slideEnd]
    else
        if beginning then
            beforeHead ++ 
            slideStart:(head rest):(insertSlides False (tail rest))
        else
            beforeHead ++ 
            slideEnd:slideStart:(head rest):(insertSlides False (tail rest)) 

-- | Insert blocks into 'Pandoc' for slide structure.
insertS5Structure :: Pandoc -> Pandoc
insertS5Structure (Pandoc meta' []) = Pandoc meta' []
insertS5Structure (Pandoc (Meta title' authors date) blocks) = 
    let slides     = insertSlides True blocks 
        firstSlide = if not (null title')
                        then [slideStart, (Header 1 title'), 
                              (Header 3 [Str (joinWithSep ", " authors)]),
                              (Header 4 [Str date]), slideEnd]
                        else []
        newBlocks  = (layoutDiv title' date) ++ presentationStart:firstSlide ++
                     slides ++ [presentationEnd]
    in  Pandoc (Meta title' authors date) newBlocks
