{-# LANGUAGE CPP #-}
module Text.Pandoc.Compat.Except ( ExceptT
                                 , Error(..)
                                 , runExceptT
                                 , throwError
                                 , catchError )
       where

#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except

class Error a where
  noMsg  :: a
  strMsg :: String -> a

  noMsg    = strMsg ""
  strMsg _ = noMsg

#else
import Control.Monad.Error
type ExceptT = ErrorT

runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT = runErrorT
#endif


