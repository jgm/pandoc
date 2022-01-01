{- |
   Module      : Text.Pandoc.Writers.Markdown.Inline
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable
-}
module Text.Pandoc.Network.HTTP (
  urlEncode
  ) where
import qualified Network.HTTP.Types as HTTP
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.Text as T

urlEncode :: T.Text -> T.Text
urlEncode = UTF8.toText . HTTP.urlEncode True . UTF8.fromText
