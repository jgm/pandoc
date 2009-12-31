{-# LANGUAGE CPP, TemplateHaskell #-}
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
   Module      : Text.Pandoc.DefaultTemplates
   Copyright   : Copyright (C) 2006-7 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Default templates for Pandoc writers.
-}
module Text.Pandoc.DefaultTemplates ( defaultLaTeXTemplate,
                                      defaultConTeXtTemplate,
                                      defaultDocbookTemplate,
                                      defaultOpenDocumentTemplate,
                                      defaultS5Template,
                                      defaultRTFTemplate
                                    ) where
import Text.Pandoc.Writers.S5
import Text.Pandoc.Shared
import System.FilePath ( (</>) )
import Text.Pandoc.TH ( contentsOf )

defaultLaTeXTemplate :: String
#ifndef __HADDOCK__
defaultLaTeXTemplate = $(contentsOf $  "data" </> "templates" </> "LaTeX.template")
#endif

defaultConTeXtTemplate :: String
#ifndef __HADDOCK__
defaultConTeXtTemplate = $(contentsOf $  "data" </> "templates" </> "ConTeXt.template")
#endif

defaultDocbookTemplate :: String
#ifndef __HADDOCK__
defaultDocbookTemplate = $(contentsOf $  "data" </> "templates" </> "Docbook.template")
#endif

defaultOpenDocumentTemplate :: String
#ifndef __HADDOCK__
defaultOpenDocumentTemplate = $(contentsOf $  "data" </> "templates" </> "OpenDocument.template")
#endif

defaultS5Template :: String
defaultS5Template = substitute "$" "$$" $ s5Meta ++ s5CSS ++ s5Javascript

defaultRTFTemplate :: String
#ifndef __HADDOCK__
defaultRTFTemplate = $(contentsOf $ "data" </> "templates" </> "RTF.template")
#endif
