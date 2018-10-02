{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Category
  (
#if MIN_VERSION_QuickCheck(2,10,0)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
    categoryLaws
  , commutativeCategoryLaws
#endif
#endif
  ) where

import Prelude hiding (id, (.))
import Control.Category (Category(..))
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

-- | Tests the following 'Category' properties:
--
-- [/Right Identity/]
--   @f '.' 'id' ≡ f@
-- [/Left Identity/]
--   @'id' '.' f ≡ f@
-- [/Associativity/]
--   @f '.' (g '.' h) ≡ (f '.' g) '.' h@
--
-- /Note/: This property test is only available when this package is built with
-- @base-4.9+@ or @transformers-0.5+@.
categoryLaws :: forall proxy c.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Category c, forall a b. (Eq a, Eq b) => Eq (c a b), forall a b. (Show a, Show b) => Show (c a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (c a b))
#else
  (Category c, Eq2 c, Show2 c, Arbitrary2 c)
#endif
  => proxy c -> Laws
categoryLaws p = Laws "Category"
  [ ("Right Identity", categoryRightIdentity p)
  , ("Left Identity", categoryLeftIdentity p)
  , ("Associativity", categoryAssociativity p)
  ]

-- | Test everything from 'categoryLaws' plus the following:
--
-- [/Commutative/]
--   @f '.' g ≡ g '.' f@
--
-- /Note/: This property test is only available when this package is built with
-- @base-4.9+@ or @transformers-0.5+@.
commutativeCategoryLaws :: forall proxy c.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Category c, forall a b. (Eq a, Eq b) => Eq (c a b), forall a b. (Show a, Show b) => Show (c a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (c a b))
#else
  (Category c, Eq2 c, Show2 c, Arbitrary2 c)
#endif
  => proxy c -> Laws
commutativeCategoryLaws p = Laws "Commutative Category" $ lawsProperties (categoryLaws p) ++
  [ ("Commutative", categoryCommutativity p)
  ]

categoryRightIdentity :: forall proxy c.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Category c, forall a b. (Eq a, Eq b) => Eq (c a b), forall a b. (Show a, Show b) => Show (c a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (c a b))
#else
  (Category c, Eq2 c, Show2 c, Arbitrary2 c)
#endif
  => proxy c -> Property
categoryRightIdentity _ = property $ \(Apply2 (x :: c Integer Integer)) -> eq2 (x . id) x

categoryLeftIdentity :: forall proxy c.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Category c, forall a b. (Eq a, Eq b) => Eq (c a b), forall a b. (Show a, Show b) => Show (c a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (c a b))
#else
  (Category c, Eq2 c, Show2 c, Arbitrary2 c)
#endif
  => proxy c -> Property
categoryLeftIdentity _ = property $ \(Apply2 (x :: c Integer Integer)) -> eq2 (id . x) x

categoryAssociativity :: forall proxy c.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Category c, forall a b. (Eq a, Eq b) => Eq (c a b), forall a b. (Show a, Show b) => Show (c a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (c a b))
#else
  (Category c, Eq2 c, Show2 c, Arbitrary2 c)
#endif
  => proxy c -> Property
categoryAssociativity _ = property $ \(Apply2 (f :: c Integer Integer)) (Apply2 (g :: c Integer Integer)) (Apply2 (h :: c Integer Integer)) -> eq2 (f . (g . h)) ((f . g) . h)

categoryCommutativity :: forall proxy c.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Category c, forall a b. (Eq a, Eq b) => Eq (c a b), forall a b. (Show a, Show b) => Show (c a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (c a b))
#else
  (Category c, Eq2 c, Show2 c, Arbitrary2 c)
#endif
  => proxy c -> Property
categoryCommutativity _ = property $ \(Apply2 (f :: c Integer Integer)) (Apply2 (g :: c Integer Integer)) -> eq2 (f . g) (g . f)

#endif

#endif

