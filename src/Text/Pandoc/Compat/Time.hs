{-# LANGUAGE CPP #-}

{-
This compatibility module is needed because, in time 1.5, the
`defaultTimeLocale` function was moved from System.Locale (in the
old-locale library) into Data.Time.

We support both behaviors because time 1.4 is a boot library for GHC
7.8. time 1.5 is a boot library for GHC 7.10.

When support is dropped for GHC 7.8, this module may be obsoleted.
-}

#if MIN_VERSION_time(1,5,0)
module Text.Pandoc.Compat.Time (
  module Data.Time
)
where
import Data.Time

#else
module Text.Pandoc.Compat.Time (
  module Data.Time,
  defaultTimeLocale
)
where
import Data.Time
import System.Locale ( defaultTimeLocale )

#endif
