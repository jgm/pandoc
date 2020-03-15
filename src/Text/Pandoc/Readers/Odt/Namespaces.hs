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

import Data.List (isPrefixOf)
import qualified Data.Map as M (empty, insert)
import Data.Maybe (fromMaybe, listToMaybe)

import Text.Pandoc.Readers.Odt.Generic.Namespaces


instance NameSpaceID Namespace where

  getInitialIRImap     = nsIDmap

  getNamespaceID ""  m = Just(m, NsXML)
  getNamespaceID iri m = asPair $ fromMaybe (NsOther iri) (findID iri)
    where asPair nsID = Just (M.insert nsID iri m, nsID)


findID :: NameSpaceIRI -> Maybe Namespace
findID iri = listToMaybe [nsID | (iri',nsID) <- nsIDs, iri' `isPrefixOf` iri]

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
