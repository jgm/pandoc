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
   Module      : Text.Pandoc.Reader.Odt.Namespaces
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

Namespaces used in odt files.
-}

module Text.Pandoc.Readers.Odt.Namespaces ( Namespace (..)
                                          ) where

import           Data.List       ( isPrefixOf )
import           Data.Maybe      ( fromMaybe, listToMaybe )
import qualified Data.Map   as M ( empty, insert )

import           Text.Pandoc.Readers.Odt.Generic.Namespaces


instance NameSpaceID Namespace where

  getInitialIRImap     = nsIDmap

  getNamespaceID ""  m = Just(m, NsXML)
  getNamespaceID iri m = asPair $ fromMaybe (NsOther iri) (findID iri)
    where asPair nsID = Just (M.insert nsID iri m, nsID)


findID :: NameSpaceIRI -> Maybe Namespace
findID iri = listToMaybe [nsID | (iri',~nsID) <- nsIDs, iri' `isPrefixOf` iri]

nsIDmap :: NameSpaceIRIs Namespace
nsIDmap = foldr (uncurry $ flip M.insert) M.empty nsIDs

data Namespace = -- Open Document core
                 NsOffice | NsStyle  | NsText   | NsTable  | NsForm
               | NsDraw   | Ns3D     | NsAnim   | NsChart  | NsConfig
               | NsDB     | NsMeta   | NsNumber | NsScript | NsManifest
               | NsPresentation
                 -- Metadata
               | NsODF
                 -- Compatible elements
               | NsXSL_FO  | NsSVG    | NsSmil
                 -- External standards
               | NsMathML | NsXForms | NsXLink  | NsXHtml  | NsGRDDL
               | NsDublinCore
                 -- Metadata manifest
               | NsPKG
                 -- Others
               | NsOpenFormula
                 -- Core XML (basically only for the 'id'-attribute)
               | NsXML
                 -- Fallback
               | NsOther String
  deriving ( Eq, Ord, Show )

-- | Not the actual iri's, but large prefixes of them - this way there are
-- less versioning problems and the like.
nsIDs :: [(String,Namespace)]
nsIDs = [
  ("urn:oasis:names:tc:opendocument:xmlns:animation"        , NsAnim         ),
  ("urn:oasis:names:tc:opendocument:xmlns:chart"            , NsChart        ),
  ("urn:oasis:names:tc:opendocument:xmlns:config"           , NsConfig       ),
  ("urn:oasis:names:tc:opendocument:xmlns:database"         , NsDB           ),
  ("urn:oasis:names:tc:opendocument:xmlns:dr3d"             , Ns3D           ),
  ("urn:oasis:names:tc:opendocument:xmlns:drawing"          , NsDraw         ),
  ("urn:oasis:names:tc:opendocument:xmlns:form"             , NsForm         ),
  ("urn:oasis:names:tc:opendocument:xmlns:manifest"         , NsManifest     ),
  ("urn:oasis:names:tc:opendocument:xmlns:meta"             , NsMeta         ),
  ("urn:oasis:names:tc:opendocument:xmlns:datastyle"        , NsNumber       ),
  ("urn:oasis:names:tc:opendocument:xmlns:of"               , NsOpenFormula  ),
  ("urn:oasis:names:tc:opendocument:xmlns:office:1.0"       , NsOffice       ),
  ("urn:oasis:names:tc:opendocument:xmlns:presentation"     , NsPresentation ),
  ("urn:oasis:names:tc:opendocument:xmlns:script"           , NsScript       ),
  ("urn:oasis:names:tc:opendocument:xmlns:style"            , NsStyle        ),
  ("urn:oasis:names:tc:opendocument:xmlns:table"            , NsTable        ),
  ("urn:oasis:names:tc:opendocument:xmlns:text"             , NsText         ),
  ("urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible", NsXSL_FO       ),
  ("urn:oasis:names:tc:opendocument:xmlns:smil-compatible"  , NsSmil         ),
  ("urn:oasis:names:tc:opendocument:xmlns:svg-compatible"   , NsSVG          ),
  ("http://docs.oasis-open.org/ns/office/1.2/meta/odf"      , NsODF          ),
  ("http://docs.oasis-open.org/ns/office/1.2/meta/pkg"      , NsPKG          ),
  ("http://purl.org/dc/elements"                            , NsDublinCore   ),
  ("http://www.w3.org/2003/g/data-view"                     , NsGRDDL        ),
  ("http://www.w3.org/1998/Math/MathML"                     , NsMathML       ),
  ("http://www.w3.org/1999/xhtml"                           , NsXHtml        ),
  ("http://www.w3.org/2002/xforms"                          , NsXForms       ),
  ("http://www.w3.org/1999/xlink"                           , NsXLink        )
  ]
