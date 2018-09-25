{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE QuantifiedConstraints #-}
#endif

module Test.QuickCheck.Classes.Compat
  ( isTrue#
  , eq1
  , eq2
  ) where

#if MIN_VERSION_base(4,7,0)
import GHC.Exts (isTrue#)
#endif

import qualified Data.Functor.Classes

#if !MIN_VERSION_base(4,7,0)
isTrue# :: Bool -> Bool
isTrue# b = b
#endif

#if MIN_VERSION_base(4,12,0)
eq1 :: (forall a. Eq a => Eq (f a), Eq a) => f a -> f a -> Bool
eq1 = (==)
#else
eq1 :: (Eq1 f, Eq a) => f a -> f a -> Bool
eq1 = Data.Functor.Classes.eq1
#endif

#if MIN_VERSION_base(4,12,0)
eq2 :: (forall a. (Eq a, Eq b) => Eq (f a b), Eq a, Eq b) => f a b -> f a b -> Bool
eq2 = (==)
#else
eq2 :: (Eq2 f, Eq a, Eq b) => f a b -> f a b -> Bool
eq2 = Data.Functor.Classes.eq2
#endif

