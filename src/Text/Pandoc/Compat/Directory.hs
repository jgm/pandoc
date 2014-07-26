{-# LANGUAGE CPP #-}
module Text.Pandoc.Compat.Directory ( getModificationTime )
       where

#if MIN_VERSION_directory(1,2,0)
import System.Directory


#else
import qualified System.Directory as S
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX
import System.Time

getModificationTime :: FilePath -> IO UTCTime
getModificationTime fp = convert `fmap` S.getModificationTime fp
    where
      convert (TOD x _) = posixSecondsToUTCTime (realToFrac x)

#endif

