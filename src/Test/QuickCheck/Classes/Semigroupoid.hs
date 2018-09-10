{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Functor.Classes
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common

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
semigroupoidLaws :: (Semigroupoid sem, Eq2 sem, Show2 sem, Arbitrary2 sem) => proxy sem -> Laws
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
commutativeSemigroupoidLaws :: (Semigroupoid sem, Eq2 sem, Show2 sem, Arbitrary2 sem) => proxy sem -> Laws
commutativeSemigroupoidLaws p = Laws "Commutative Semigroupoid" $ lawsProperties (semigroupoidLaws p) ++
  [ ("Commutative", semigroupoidCommutativity p)
  ]

semigroupoidAssociativity :: forall proxy sem. (Semigroupoid sem, Eq2 sem, Show2 sem, Arbitrary2 sem) => proxy sem -> Property
semigroupoidAssociativity _ = property $ \(Apply2 (f :: sem Integer Integer)) (Apply2 (g :: sem Integer Integer)) (Apply2 (h :: sem Integer Integer)) -> eq2 (f `o` (g `o` h)) ((f `o` g) `o` h)

semigroupoidCommutativity :: forall proxy sem. (Semigroupoid sem, Eq2 sem, Show2 sem, Arbitrary2 sem) => proxy sem -> Property
semigroupoidCommutativity _ = property $ \(Apply2 (f :: sem Integer Integer)) (Apply2 (g :: sem Integer Integer)) -> eq2 (f `o` g) (g `o` f)

#endif

#endif

#endif
