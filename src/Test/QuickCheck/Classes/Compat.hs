{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE QuantifiedConstraints #-}
#endif

module Test.QuickCheck.Classes.Compat
  ( isTrue#
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
  , eq1
#endif
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
  , eq2
#endif
  ) where

#if MIN_VERSION_base(4,7,0)
import GHC.Exts (isTrue#)
#endif

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import qualified Data.Functor.Classes as C
#endif

#if !MIN_VERSION_base(4,7,0)
isTrue# :: Bool -> Bool
isTrue# b = b
#endif

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
#if MIN_VERSION_base(4,12,0)
eq1 :: (forall a. Eq a => Eq (f a), Eq a) => f a -> f a -> Bool
eq1 = (==)
#else
eq1 :: (C.Eq1 f, Eq a) => f a -> f a -> Bool
eq1 = C.eq1
#endif
#endif

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
#if MIN_VERSION_base(4,12,0)
eq2 :: (forall a. (Eq a, Eq b) => Eq (f a b), Eq a, Eq b) => f a b -> f a b -> Bool
eq2 = (==)
#else
eq2 :: (C.Eq2 f, Eq a, Eq b) => f a b -> f a b -> Bool
eq2 = C.eq2
#endif
#endif

