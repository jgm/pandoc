{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Mdoc.Macros
   Copyright   : © 2024 Evan Silberman
   License     : GNU GPL, version 2 or above

   Maintainer  : Evan Silberman <evan@jklol.net>
   Stability   : WIP
   Portability : portable

-}
module Text.Pandoc.Readers.Mdoc.Macros (isParsedMacro, isCallableMacro) where

import Data.Set (member, fromList, Set)
import Data.Text

isParsedMacro :: Text -> Bool
isParsedMacro a = member a parsedMacros

isCallableMacro :: Text -> Bool
isCallableMacro a = member a callableMacros

parsedMacros :: Set Text
parsedMacros = fromList [
  "Ac",
  "Ad",
  "An",
  "Ao",
  "Ap",
  "Aq",
  "Ar",
  "At",
  "Bc",
  "Bo",
  "Bq",
  "Brc",
  "Bro",
  "Brq",
  "Bsx",
  "Bx",
  "Cd",
  "Cm",
  "D1",
  "Dc",
  "Dl",
  "Do",
  "Dq",
  "Dv",
  "Dx",
  "Ec",
  "Em",
  "En",
  "Eo",
  "Er",
  "Es",
  "Ev",
  "Fa",
  "Fc",
  "Fl",
  "Fn",
  "Fr",
  "Ft",
  "Fx",
  "Ic",
  "In",
  "It",
  "Li",
  "Lk",
  "Ms",
  "Mt",
  "Nm",
  "No",
  "Ns",
  "Nx",
  "Oc",
  "Oo",
  "Op",
  "Ot",
  "Ox",
  "Pa",
  "Pc",
  "Pf",
  "Po",
  "Pq",
  "Qc",
  "Ql",
  "Qo",
  "Qq",
  "Sc",
  "Sh",
  "So",
  "Sq",
  "Ss",
  "St",
  "Sx",
  "Sy",
  "Ta",
  "Tn",
  "Ux",
  "Va",
  "Vt",
  "Xc",
  "Xo",
  "Xr"]

callableMacros :: Set Text
callableMacros = fromList [
  "Ac",
  "Ad",
  "An",
  "Ao",
  "Ap",
  "Aq",
  "Ar",
  "At",
  "Bc",
  "Bo",
  "Bq",
  "Brc",
  "Bro",
  "Brq",
  "Bsx",
  "Bx",
  "Cd",
  "Cm",
  "Dc",
  "Do",
  "Dq",
  "Dv",
  "Dx",
  "Ec",
  "Em",
  "En",
  "Eo",
  "Er",
  "Es",
  "Ev",
  "Fa",
  "Fc",
  "Fl",
  "Fn",
  "Fo",
  "Fr",
  "Ft",
  "Fx",
  "Ic",
  "In",
  "Li",
  "Lk",
  "Ms",
  "Mt",
  "Nm",
  "No",
  "Ns",
  "Nx",
  "Oc",
  "Oo",
  "Op",
  "Ot",
  "Ox",
  "Pa",
  "Pc",
  "Pf",
  "Po",
  "Pq",
  "Qc",
  "Ql",
  "Qo",
  "Qq",
  "Sc",
  "So",
  "Sq",
  "St",
  "Sx",
  "Sy",
  "Ta",
  "Tn",
  "Ux",
  "Va",
  "Vt",
  "Xc",
  "Xo",
  "Xr"]
