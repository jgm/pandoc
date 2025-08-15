{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Mdoc.Standards
   Copyright   : © 2024 Evan Silberman
   License     : GNU GPL, version 2 or above

   Maintainer  : Evan Silberman <evan@jklol.net>
   Stability   : WIP
   Portability : portable

-}
module Text.Pandoc.Readers.Mdoc.Standards (standard) where

import Data.Map (fromList, Map)
import qualified Data.Map as M
import Data.Text

standard :: Text -> Maybe Text
standard = flip M.lookup standards

standards :: Map Text Text
standards = fromList [
  ("-p1003.1-88",    "IEEE Std 1003.1-1988 (“POSIX.1”)"),
  ("-p1003.1-90",    "IEEE Std 1003.1-1990 (“POSIX.1”)"),
  ("-p1003.1-96",    "ISO/IEC 9945-1:1996 (“POSIX.1”)"),
  ("-p1003.1-2001",  "IEEE Std 1003.1-2001 (“POSIX.1”)"),
  ("-p1003.1-2004",  "IEEE Std 1003.1-2004 (“POSIX.1”)"),
  ("-p1003.1-2008",  "IEEE Std 1003.1-2008 (“POSIX.1”)"),
  ("-p1003.1-2024",  "IEEE Std 1003.1-2024 (“POSIX.1”)"),
  ("-p1003.1",       "IEEE Std 1003.1 (“POSIX.1”)"),
  ("-p1003.1b",      "IEEE Std 1003.1b (“POSIX.1b”)"),
  ("-p1003.1b-93",   "IEEE Std 1003.1b-1993 (“POSIX.1b”)"),
  ("-p1003.1c-95",   "IEEE Std 1003.1c-1995 (“POSIX.1c”)"),
  ("-p1003.1g-2000", "IEEE Std 1003.1g-2000 (“POSIX.1g”)"),
  ("-p1003.1i-95",   "IEEE Std 1003.1i-1995 (“POSIX.1i”)"),
  ("-p1003.2",       "IEEE Std 1003.2 (“POSIX.2”)"),
  ("-p1003.2-92",    "IEEE Std 1003.2-1992 (“POSIX.2”)"),
  ("-p1003.2a-92",   "IEEE Std 1003.2a-1992 (“POSIX.2”)"),
  ("-isoC",          "ISO/IEC 9899:1990 (“ISO C90”)"),
  ("-isoC-90",       "ISO/IEC 9899:1990 (“ISO C90”)"),
  ("-isoC-amd1",     "ISO/IEC 9899/AMD1:1995 (“ISO C90, Amendment 1”)"),
  ("-isoC-tcor1",    "ISO/IEC 9899/TCOR1:1994 (“ISO C90, Technical Corrigendum 1”)"),
  ("-isoC-tcor2",    "ISO/IEC 9899/TCOR2:1995 (“ISO C90, Technical Corrigendum 2”)"),
  ("-isoC-99",       "ISO/IEC 9899:1999 (“ISO C99”)"),
  ("-isoC-2011",     "ISO/IEC 9899:2011 (“ISO C11”)"),
  ("-isoC-2023",     "ISO/IEC 9899:2024 (“ISO C23”)"),
  ("-iso9945-1-90",  "ISO/IEC 9945-1:1990 (“POSIX.1”)"),
  ("-iso9945-1-96",  "ISO/IEC 9945-1:1996 (“POSIX.1”)"),
  ("-iso9945-2-93",  "ISO/IEC 9945-2:1993 (“POSIX.2”)"),
  ("-ansiC",         "ANSI X3.159-1989 (“ANSI C89”)"),
  ("-ansiC-89",      "ANSI X3.159-1989 (“ANSI C89”)"),
  ("-ieee754",       "IEEE Std 754-1985"),
  ("-iso8802-3",     "ISO 8802-3: 1989"),
  ("-iso8601",       "ISO 8601"),
  ("-ieee1275-94",   "IEEE Std 1275-1994 (“Open Firmware”)"),
  ("-xpg3",          "X/Open Portability Guide Issue 3 (“XPG3”)"),
  ("-xpg4",          "X/Open Portability Guide Issue 4 (“XPG4”)"),
  ("-xpg4.2",        "X/Open Portability Guide Issue 4, Version 2 (“XPG4.2”)"),
  ("-xbd5",          "X/Open Base Definitions Issue 5 (“XBD5”)"),
  ("-xcu5",          "X/Open Commands and Utilities Issue 5 (“XCU5”)"),
  ("-xsh5",          "X/Open System Interfaces and Headers Issue 5 (“XSH5”)"),
  ("-xns5",          "X/Open Networking Services Issue 5 (“XNS5”)"),
  ("-xns5.2",        "X/Open Networking Services Issue 5.2 (“XNS5.2”)"),
  ("-xcurses4.2",    "X/Open Curses Issue 4, Version 2 (“XCURSES4.2”)"),
  ("-susv1",         "Version 1 of the Single UNIX Specification (“SUSv1”)"),
  ("-susv2",         "Version 2 of the Single UNIX Specification (“SUSv2”)"),
  ("-susv3",         "Version 3 of the Single UNIX Specification (“SUSv3”)"),
  ("-susv4",         "Version 4 of the Single UNIX Specification (“SUSv4”)"),
  ("-svid4",         "System V Interface Definition, Fourth Edition (“SVID4”)")
  ]
