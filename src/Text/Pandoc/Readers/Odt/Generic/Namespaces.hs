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
  -- | Given a namespace id, lookup its IRI. May be overriden for performance.
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
