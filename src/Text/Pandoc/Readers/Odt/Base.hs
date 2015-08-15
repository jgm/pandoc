{-# LANGUAGE PatternGuards #-}

{-
Copyright (C) 2015 Martin Linnemann <theCodingMarlin@googlemail.com>

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
   Module      : Text.Pandoc.Readers.Odt.Base
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

Core types of the odt reader.
-}

module Text.Pandoc.Readers.Odt.Base where

import           Text.Pandoc.Readers.Odt.Generic.XMLConverter
import           Text.Pandoc.Readers.Odt.Namespaces

type OdtConverterState s = XMLConverterState Namespace s

type XMLReader     s a b = FallibleXMLConverter Namespace s a b

type XMLReaderSafe s a b =         XMLConverter Namespace s a b

