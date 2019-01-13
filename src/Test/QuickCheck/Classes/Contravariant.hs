{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Contravariant
  (
#if HAVE_UNARY_LAWS
    contravariantLaws
#endif
  ) where

import Data.Functor.Contravariant
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

-- | Tests the following contravariant properties:
--
-- [/Identity/]
--   @'contramap' 'id' ≡ 'id'@
-- [/Composition/]
--   @'contramap' f '.' 'contramap' g ≡ 'contramap' (g '.' f)@
contravariantLaws ::
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Contravariant f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Contravariant f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f
  -> Laws
contravariantLaws p = Laws "Contravariant"
  [ ("Identity", contravariantIdentity p)
  , ("Composition", contravariantComposition p)
  ]

contravariantIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Contravariant f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Contravariant f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
contravariantIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (contramap id a) a

contravariantComposition :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Contravariant f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Contravariant f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
contravariantComposition _ = property $ \(Apply (a :: f Integer)) (f' :: QuadraticEquation) (g' :: QuadraticEquation) -> do
  let f = runQuadraticEquation f'
      g = runQuadraticEquation g'
  eq1 (contramap f (contramap g a)) (contramap (g . f) a)

#endif
