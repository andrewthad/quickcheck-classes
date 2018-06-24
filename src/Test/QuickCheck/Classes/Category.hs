{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Category
  (
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
    categoryLaws
  , commutativeCategoryLaws
#endif  
  ) where

import Prelude hiding (id, (.))
import Control.Category (Category(..))
import Test.QuickCheck hiding ((.&.))
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common

#if MIN_VERSION_QuickCheck(2,10,0)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)

-- | Tests the following 'Category' properties:
--
-- [/Right Identity/]
--   @f '.' 'id' ≡ f
-- [/Left Identity/]
--   @'id' '.' f ≡ f@
-- [/Associativity/]
--   @f '.' (g '.' h) ≡ (f '.' g) '.' h@
--
-- /Note/: This property test is only available when this package is built with
-- @base-4.9+@ or @transformers-0.5+@.
categoryLaws :: (Category cat, Eq2 cat, Show2 cat, Arbitrary2 cat) => proxy cat -> Laws
categoryLaws p = Laws "Category"
  [ ("Right Identity", categoryRightIdentity p)
  , ("Left Identity", categoryLeftIdentity p)
  , ("Second Identity", categoryAssociativity p)
  ]

-- | Test everything from 'categoryLaws' plus the following:
--
-- [/Commutative/]
--   @f '.' g ≡ g '.' f@
--
-- /Note/: This property test is only available when this package is built with
-- @base-4.9+@ or @transformers-0.5+@.
commutativeCategoryLaws :: (Category cat, Eq2 cat, Show2 cat, Arbitrary2 cat) => proxy cat -> Laws
commutativeCategoryLaws p = Laws "Commutative Category" $ lawsProperties (categoryLaws p) ++
  [ ("Commutative", categoryCommutativity p)
  ]

categoryRightIdentity :: forall proxy cat. (Category cat, Eq2 cat, Show2 cat, Arbitrary2 cat) => proxy cat -> Property
categoryRightIdentity _ = property $ \(Apply2 (x :: cat Integer Integer)) -> eq2 (x . id) x

categoryLeftIdentity :: forall proxy cat. (Category cat, Eq2 cat, Show2 cat, Arbitrary2 cat) => proxy cat -> Property
categoryLeftIdentity _ = property $ \(Apply2 (x :: cat Integer Integer)) -> eq2 (id . x) x

categoryAssociativity :: forall proxy cat. (Category cat, Eq2 cat, Show2 cat, Arbitrary2 cat) => proxy cat -> Property
categoryAssociativity _ = property $ \(Apply2 (f :: cat Integer Integer)) (Apply2 (g :: cat Integer Integer)) (Apply2 (h :: cat Integer Integer)) -> eq2 (f . (g . h)) ((f . g) . h)

categoryCommutativity :: forall proxy cat. (Category cat, Eq2 cat, Show2 cat, Arbitrary2 cat) => proxy cat -> Property
categoryCommutativity _ = property $ \(Apply2 (f :: cat Integer Integer)) (Apply2 (g :: cat Integer Integer)) -> eq2 (f . g) (g . f)

#endif

#endif

