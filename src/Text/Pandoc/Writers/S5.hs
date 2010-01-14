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
                -- * Header includes
                s5HeaderIncludes,
                s5Links,
                -- * Functions
                writeS5,
                writeS5String,
                insertS5Structure
                ) where
import Text.Pandoc.Shared ( WriterOptions, readDataFile )
import Text.Pandoc.Writers.HTML ( writeHtml, writeHtmlString )
import Text.Pandoc.Definition
import Text.XHtml.Strict
import System.FilePath ( (</>) )
import Data.List ( intercalate )

s5HeaderIncludes :: FilePath -> IO String
s5HeaderIncludes datadir = do
  c <- s5CSS datadir
  j <- s5Javascript datadir
  return $ s5Meta ++ c ++ j

s5Meta :: String
s5Meta = "<!-- configuration parameters -->\n<meta name=\"defaultView\" content=\"slideshow\" />\n<meta name=\"controlVis\" content=\"hidden\" />\n"

s5Javascript :: FilePath -> IO String
s5Javascript datadir = do
  jsCom <- readDataFile datadir $ "s5" </> "default" </> "slides.js.comment"
  jsPacked <- readDataFile datadir $ "s5" </> "default" </> "slides.js.packed"
  return $ "<script type=\"text/javascript\">\n" ++ jsCom ++ jsPacked ++
           "</script>\n"

s5CSS :: FilePath -> IO String
s5CSS datadir = do
  s5CoreCSS <- readDataFile datadir $ "s5" </> "default" </> "s5-core.css"
  s5FramingCSS <- readDataFile datadir $ "s5" </> "default" </> "framing.css"
  s5PrettyCSS <- readDataFile datadir $ "s5" </> "default" </> "pretty.css"
  s5OperaCSS <- readDataFile datadir $ "s5" </> "default" </> "opera.css"
  s5OutlineCSS <- readDataFile datadir $ "s5" </> "default" </> "outline.css"
  s5PrintCSS <- readDataFile datadir $ "s5" </> "default" </> "print.css"
  return $ "<style type=\"text/css\" media=\"projection\" id=\"slideProj\">\n" ++ s5CoreCSS ++ "\n" ++ s5FramingCSS ++ "\n" ++ s5PrettyCSS ++ "\n</style>\n<style type=\"text/css\" media=\"projection\" id=\"operaFix\">\n" ++ s5OperaCSS ++ "\n</style>\n<style type=\"text/css\" media=\"screen\" id=\"outlineStyle\">\n" ++ s5OutlineCSS ++ "\n</style>\n<style type=\"text/css\" media=\"print\" id=\"slidePrint\">\n" ++ s5PrintCSS ++ "\n</style>\n"

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
          -> [Inline]  -- ^ Date of document (for header or footer)
          -> [Block]   -- ^ List of block elements returned
layoutDiv title' date = [(RawHtml "<div class=\"layout\">\n<div id=\"controls\"></div>\n<div id=\"currentSlide\"></div>\n<div id=\"header\"></div>\n<div id=\"footer\">\n"), (Header 1 date), (Header 2 title'), (RawHtml "</div>\n</div>\n")]

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
                              (Header 3 (intercalate [LineBreak] authors)),
                              (Header 4 date), slideEnd]
                        else []
        newBlocks  = (layoutDiv title' date) ++ presentationStart:firstSlide ++
                     slides ++ [presentationEnd]
    in  Pandoc (Meta title' authors date) newBlocks
