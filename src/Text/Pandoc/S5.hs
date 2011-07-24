{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.S5
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Definitions for creation of S5 powerpoint-like HTML.
(See <http://meyerweb.com/eric/tools/s5/>.)
-}
module Text.Pandoc.S5 ( s5HeaderIncludes) where
import Text.Pandoc.Shared ( readDataFile )
import System.FilePath ( (</>) )

s5HeaderIncludes :: Maybe FilePath -> IO String
s5HeaderIncludes datadir = do
  c <- s5CSS datadir
  j <- s5Javascript datadir
  return $ c ++ j

inCDATA :: String -> String
inCDATA s = "/*<![CDATA[*/\n" ++ s ++ "\n/*]]>*/\n"

s5Javascript :: Maybe FilePath -> IO String
s5Javascript datadir = do
  js <- readDataFile datadir $ "s5" </> "default" </> "slides.min.js"
  return $ "<script type=\"text/javascript\">\n" ++ inCDATA js ++ "</script>\n"

s5CSS :: Maybe FilePath -> IO String
s5CSS datadir = do
  s5CoreCSS <- readDataFile datadir $ "s5" </> "default" </> "s5-core.css"
  s5FramingCSS <- readDataFile datadir $ "s5" </> "default" </> "framing.css"
  s5PrettyCSS <- readDataFile datadir $ "s5" </> "default" </> "pretty.css"
  s5OperaCSS <- readDataFile datadir $ "s5" </> "default" </> "opera.css"
  s5OutlineCSS <- readDataFile datadir $ "s5" </> "default" </> "outline.css"
  s5PrintCSS <- readDataFile datadir $ "s5" </> "default" </> "print.css"
  return $ "<style type=\"text/css\" media=\"projection\" id=\"slideProj\">\n" ++
          inCDATA (s5CoreCSS ++ "\n" ++ s5FramingCSS ++ "\n" ++ s5PrettyCSS) ++
          "</style>\n<style type=\"text/css\" media=\"screen\" id=\"outlineStyle\">\n" ++
          inCDATA s5OutlineCSS ++
          "</style>\n<style type=\"text/css\" media=\"print\" id=\"slidePrint\">\n" ++
          inCDATA s5PrintCSS ++
          "</style>\n<style type=\"text/css\" media=\"projection\" id=\"operaFix\">\n" ++
          inCDATA s5OperaCSS ++ "</style>\n"
