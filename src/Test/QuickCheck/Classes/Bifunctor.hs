{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Bifunctor
  (
#if MIN_VERSION_QuickCheck(2,10,0)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
    bifunctorLaws
#endif
#endif
  ) where

import Data.Bifunctor(Bifunctor(..))
import Test.QuickCheck hiding ((.&.))
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
import Data.Functor.Classes (Eq2,Show2)
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
import Test.QuickCheck.Classes.Compat (eq2)
#endif

#if MIN_VERSION_QuickCheck(2,10,0)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)

-- | Tests the following 'Bifunctor' properties:
--
-- [/Identity/]
--   @'bimap' 'id' 'id' ≡ 'id'@
-- [/First Identity/]
--   @'first' 'id' ≡ 'id'@
-- [/Second Identity/] 
--   @'second' 'id' ≡ 'id'@
-- [/Bifunctor Composition/]
--   @'bimap' f g ≡ 'first' f '.' 'second' g@ 
--
-- /Note/: This property test is only available when this package is built with
-- @base-4.9+@ or @transformers-0.5+@.
bifunctorLaws :: forall proxy f.
#if MIN_VERSION_base(4,12,0)
  (Bifunctor f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Laws
bifunctorLaws p = Laws "Bifunctor"
  [ ("Identity", bifunctorIdentity p)
  , ("First Identity", bifunctorFirstIdentity p)
  , ("Second Identity", bifunctorSecondIdentity p)
  , ("Bifunctor Composition", bifunctorComposition p)
  ]

bifunctorIdentity :: forall proxy f.
#if MIN_VERSION_base(4,12,0)
  (Bifunctor f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Property
bifunctorIdentity _ = property $ \(Apply2 (x :: f Integer Integer)) -> eq2 (bimap id id x) x

bifunctorFirstIdentity :: forall proxy f.
#if MIN_VERSION_base(4,12,0)
  (Bifunctor f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Property
bifunctorFirstIdentity _ = property $ \(Apply2 (x :: f Integer Integer)) -> eq2 (first id x) x

bifunctorSecondIdentity :: forall proxy f.
#if MIN_VERSION_base(4,12,0)
  (Bifunctor f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Property
bifunctorSecondIdentity _ = property $ \(Apply2 (x :: f Integer Integer)) -> eq2 (second id x) x

bifunctorComposition :: forall proxy f. 
#if MIN_VERSION_base(4,12,0)
  (Bifunctor f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Property
bifunctorComposition _ = property $ \(Apply2 (z :: f Integer Integer)) -> eq2 (bimap id id z) ((first id . second id) z)
#endif

#endif

