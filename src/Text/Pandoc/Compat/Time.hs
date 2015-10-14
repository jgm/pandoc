{-# LANGUAGE CPP #-}
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
