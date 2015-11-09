{-# LANGUAGE CPP #-}
module Text.Pandoc.Compat.Monoid ( (<>) )
       where

#if MIN_VERSION_base(4,5,0)
import Data.Monoid ((<>))

#else
import Data.Monoid

infixr 6 <>

--- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
#endif
