{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Apply
  (
#if MIN_VERSION_QuickCheck(2,10,0)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
#if defined(VERSION_semigroupoids)
    applyLaws
#endif
#endif
#endif
) where

import Data.Functor

#if defined(VERSION_semigroupoids)
import qualified Data.Functor.Apply as FunctorApply
#endif

import Test.QuickCheck hiding ((.&.))
#if MIN_VERSION_QuickCheck(2,10,0)
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes (Eq1,Show1)
#endif
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common
import Test.QuickCheck.Classes.Compat (eq1)

#if MIN_VERSION_QuickCheck(2,10,0)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)

-- | Tests the following alt properties:
--
-- [/LiftF2 (1)/]
--   @('FunctorApply.<.>') ≡ 'liftF2' 'id'@
#if defined(VERSION_semigroupoids)
applyLaws ::
#if MIN_VERSION_base(4,12,0)
  (FunctorApply.Apply f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (FunctorApply.Apply f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
applyLaws p = Laws "Apply"
  [ ("LiftF2 part 1", applyLiftF2_1 p)
  ]

applyLiftF2_1 :: forall proxy f. 
#if MIN_VERSION_base(4,12,0)
  (FunctorApply.Apply f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (FunctorApply.Apply f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
applyLiftF2_1 _ = property $ \(Apply (f' :: f QuadraticEquation)) (Apply (x :: f Integer)) ->
  let f = fmap runQuadraticEquation f'
  in eq1 (FunctorApply.liftF2 id f x) (f FunctorApply.<.> x)
#endif
#endif
#endif

