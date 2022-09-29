{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{- |
   Module      : Text.Pandoc.Lua.Orphans
   Copyright   : © 2012-2022 John MacFarlane
                 © 2017-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Orphan instances for Lua's Pushable and Peekable type classes.
-}
module Text.Pandoc.Lua.Orphans () where

import Data.Version (Version)
import HsLua
import HsLua.Module.Version (peekVersionFuzzy)
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Marshal.AST
import Text.Pandoc.Lua.Marshal.CommonState ()
import Text.Pandoc.Lua.Marshal.Context ()
import Text.Pandoc.Lua.Marshal.PandocError()
import Text.Pandoc.Lua.Marshal.ReaderOptions ()
import Text.Pandoc.Lua.Marshal.Sources (pushSources)
import Text.Pandoc.Lua.ErrorConversion ()
import Text.Pandoc.Sources (Sources)

instance Pushable Pandoc where
  push = pushPandoc

instance Pushable Meta where
  push = pushMeta

instance Pushable MetaValue where
  push = pushMetaValue

instance Pushable Block where
  push = pushBlock

instance {-# OVERLAPPING #-} Pushable [Block] where
  push = pushBlocks

instance Pushable Alignment where
  push = pushString . show

instance Pushable CitationMode where
  push = pushCitationMode

instance Pushable Format where
  push = pushFormat

instance Pushable ListNumberDelim where
  push = pushString . show

instance Pushable ListNumberStyle where
  push = pushString . show

instance Pushable MathType where
  push = pushMathType

instance Pushable QuoteType where
  push = pushQuoteType

instance Pushable Cell where
  push = pushCell

instance Pushable Inline where
  push = pushInline

instance {-# OVERLAPPING #-} Pushable [Inline] where
  push = pushInlines

instance Pushable Citation where
  push = pushCitation

instance Pushable Row where
  push = pushRow

instance Pushable TableBody where
  push = pushTableBody

instance Pushable TableFoot where
  push = pushTableFoot

instance Pushable TableHead where
  push = pushTableHead

-- These instances exist only for testing. It's a hack to avoid making
-- the marshalling modules public.
instance Peekable Inline where
  safepeek = peekInline

instance Peekable Block where
  safepeek = peekBlock

instance Peekable Cell where
  safepeek = peekCell

instance Peekable Meta where
  safepeek = peekMeta

instance Peekable Pandoc where
  safepeek = peekPandoc

instance Peekable Row where
  safepeek = peekRow

instance Peekable Version where
  safepeek = peekVersionFuzzy

instance {-# OVERLAPPING #-} Peekable Attr where
  safepeek = peekAttr

instance Pushable Sources where
  push = pushSources
