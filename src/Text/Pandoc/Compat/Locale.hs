{-# LANGUAGE CPP #-}
module Text.Pandoc.Compat.Locale ( defaultTimeLocale )
where

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format ( defaultTimeLocale )
#else
import System.Locale ( defaultTimeLocale )
#endif
