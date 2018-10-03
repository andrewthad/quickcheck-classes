{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Apply
  (
#if HAVE_UNARY_LAWS
#if defined(VERSION_semigroupoids)
    applyLaws
#endif
#endif
) where

import Data.Functor

#if defined(VERSION_semigroupoids)
import qualified Data.Functor.Apply as FunctorApply
#endif

import Test.QuickCheck hiding ((.&.))
#if HAVE_UNARY_LAWS
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
import Data.Functor.Classes (Eq1,Show1)
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common
#if HAVE_UNARY_LAWS
import Test.QuickCheck.Classes.Compat (eq1)
#endif

#if HAVE_UNARY_LAWS

-- | Tests the following alt properties:
--
-- [/LiftF2 (1)/]
--   @('FunctorApply.<.>') ≡ 'liftF2' 'id'@
#if defined(VERSION_semigroupoids)
applyLaws ::
#if HAVE_QUANTIFIED_CONSTRAINTS
  (FunctorApply.Apply f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (FunctorApply.Apply f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
applyLaws p = Laws "Apply"
  [ ("LiftF2 part 1", applyLiftF2_1 p)
  ]

applyLiftF2_1 :: forall proxy f. 
#if HAVE_QUANTIFIED_CONSTRAINTS
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
