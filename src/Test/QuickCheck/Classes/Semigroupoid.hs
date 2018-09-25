{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Semigroupoid
  (
#if MIN_VERSION_QuickCheck(2,10,0)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
#if defined(VERSION_semigroupoids)
    semigroupoidLaws
  , commutativeSemigroupoidLaws
#endif
#endif
#endif
  ) where

import Prelude hiding (id, (.))
#if defined(VERSION_semigroupoids)
import Data.Semigroupoid (Semigroupoid(..))
#endif
import Test.QuickCheck hiding ((.&.))
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes (Eq2,Show2)
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common
import Test.QuickCheck.Classes.Compat (eq2)

#if MIN_VERSION_QuickCheck(2,10,0)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)

#if defined (VERSION_semigroupoids)
-- | Tests the following 'Semigroupoid' properties:
--
-- [/Associativity/]
--   @f `'o'` (g `'o'` h) ≡ (f `'o'` g) `'o'` h@
--
-- /Note/: This property test is only available when this package is built with
-- @base-4.9+@ or @transformers-0.5+@.
semigroupoidLaws :: forall proxy s.
#if MIN_VERSION_base(4,12,0)
  (Semigroupoid s, forall a b. (Eq a, Eq b) => Eq (s a b), forall a b. (Show a, Show b) => Show (s a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (s a b))
#else
  (Semigroupoid s, Eq2 s, Show2 s, Arbitrary2 s)
#endif
  => proxy s -> Laws
semigroupoidLaws p = Laws "Semigroupoid"
  [ ("Associativity", semigroupoidAssociativity p)
  ]

-- | Tests everything from 'semigroupoidLaws' plus the following:
--
-- [/Commutative/]
--   @f `'o'` g ≡ g `'o'` f@
--
-- /Note/: This property test is only available when this package is built with
-- @base-4.9+@ or @transformers-0.5+@.
commutativeSemigroupoidLaws :: forall proxy s.
#if MIN_VERSION_base(4,12,0)
  (Semigroupoid s, forall a b. (Eq a, Eq b) => Eq (s a b), forall a b. (Show a, Show b) => Show (s a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (s a b))
#else
  (Semigroupoid s, Eq2 s, Show2 s, Arbitrary2 s)
#endif
  => proxy s -> Laws
commutativeSemigroupoidLaws p = Laws "Commutative Semigroupoid" $ lawsProperties (semigroupoidLaws p) ++
  [ ("Commutative", semigroupoidCommutativity p)
  ]

semigroupoidAssociativity :: forall proxy s.
#if MIN_VERSION_base(4,12,0)
  (Semigroupoid s, forall a b. (Eq a, Eq b) => Eq (s a b), forall a b. (Show a, Show b) => Show (s a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (s a b))
#else
  (Semigroupoid s, Eq2 s, Show2 s, Arbitrary2 s)
#endif
  => proxy s -> Property
semigroupoidAssociativity _ = property $ \(Apply2 (f :: s Integer Integer)) (Apply2 (g :: s Integer Integer)) (Apply2 (h :: s Integer Integer)) -> eq2 (f `o` (g `o` h)) ((f `o` g) `o` h)

semigroupoidCommutativity :: forall proxy s.
#if MIN_VERSION_base(4,12,0)
  (Semigroupoid s, forall a b. (Eq a, Eq b) => Eq (s a b), forall a b. (Show a, Show b) => Show (s a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (s a b))
#else
  (Semigroupoid s, Eq2 s, Show2 s, Arbitrary2 s)
#endif
  => proxy s -> Property
semigroupoidCommutativity _ = property $ \(Apply2 (f :: s Integer Integer)) (Apply2 (g :: s Integer Integer)) -> eq2 (f `o` g) (g `o` f)

#endif

#endif

#endif
