{- |
   Module      : Text.Pandoc.Readers.Odt.Generic.Namespaces
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

A class containing a set of namespace identifiers. Used to convert between
typesafe Haskell namespace identifiers and unsafe "real world" namespaces.
-}

module Text.Pandoc.Readers.Odt.Generic.Namespaces where

import qualified Data.Map as M

--
type NameSpaceIRI          = String

--
type NameSpaceIRIs     nsID = M.Map nsID NameSpaceIRI

--
class (Eq nsID, Ord nsID) => NameSpaceID nsID where

  -- | Given a IRI, possibly update the map and return the id of the namespace.
  -- May fail if the namespace is unknown and the application does not
  -- allow unknown namespaces.
  getNamespaceID   :: NameSpaceIRI
                      -> NameSpaceIRIs nsID
                      -> Maybe (NameSpaceIRIs nsID, nsID)
  -- | Given a namespace id, lookup its IRI. May be overridden for performance.
  getIRI           :: nsID
                      -> NameSpaceIRIs nsID
                      -> Maybe NameSpaceIRI
  -- | The root element of an XML document has a namespace, too, and the
  -- "XML.Light-parser" is eager to remove the corresponding namespace
  -- attribute.
  -- As a result, at least this root namespace must be provided.
  getInitialIRImap :: NameSpaceIRIs nsID

  getIRI           = M.lookup
  getInitialIRImap = M.empty
