{- |
   Module      : Text.Pandoc.Readers.ODT.Base
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

Core types of the odt reader.
-}

module Text.Pandoc.Readers.ODT.Base where

import Text.Pandoc.Readers.ODT.Generic.XMLConverter
import Text.Pandoc.Readers.ODT.Namespaces

type ODTConverterState s = XMLConverterState Namespace s

type XMLReader     s a b = FallibleXMLConverter Namespace s a b

type XMLReaderSafe s a b =         XMLConverter Namespace s a b
