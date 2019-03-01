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

import Text.Pandoc.Readers.Odt.Generic.XMLConverter
import Text.Pandoc.Readers.Odt.Namespaces

type OdtConverterState s = XMLConverterState Namespace s

type XMLReader     s a b = FallibleXMLConverter Namespace s a b

type XMLReaderSafe s a b =         XMLConverter Namespace s a b
