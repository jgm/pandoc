{-# LANGUAGE CPP #-}
module Text.Pandoc.Compat.Except ( ExceptT
                                 , Except
                                 , Error(..)
                                 , runExceptT
                                 , runExcept
                                 , MonadError
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
import Control.Monad.Identity (Identity, runIdentity)

type ExceptT = ErrorT

type Except s a = ErrorT s Identity a

runExceptT ::  ExceptT e m a -> m (Either e a)
runExceptT = runErrorT

runExcept :: ExceptT e Identity a -> Either e a
runExcept = runIdentity . runExceptT
#endif


