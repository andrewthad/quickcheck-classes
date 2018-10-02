{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Plus
  (
#if MIN_VERSION_QuickCheck(2,10,0)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
#if defined(VERSION_semigroupoids)
    plusLaws
  , extendedPlusLaws
#endif
#endif
#endif
  ) where

import Data.Functor

#if defined(VERSION_semigroupoids)
import Data.Functor.Alt (Alt)
import Data.Functor.Plus (Plus)
import qualified Data.Functor.Alt as Alt
import qualified Data.Functor.Plus as Plus
#endif

import Test.QuickCheck hiding ((.&.))
#if MIN_VERSION_QuickCheck(2,10,0)
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes (Eq1,Show1)
import qualified Control.Applicative as Alternative
#endif
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Test.QuickCheck.Classes.Compat (eq1)
#endif

#if MIN_VERSION_QuickCheck(2,10,0)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)

-- | Tests the following alt properties:
--
-- [/Left Identity/]
--   @'Plus.zero' 'Alt.<!>' m ≡ m@
-- [/Right Identity/]
--   @m 'Alt.<!>' 'Plus.zero' ≡ m@
#if defined(VERSION_semigroupoids)
plusLaws :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Plus f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Plus f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
plusLaws p = Laws "Plus"
  [ ("Left Identity", plusLeftIdentity p)
  , ("Right Identity", plusRightIdentity p)
  ]

-- | Tests everything from 'altLaws', plus the following:
--
-- [/Congruency/]
--   @'Plus.zero' ≡ 'Alternative.empty'@
extendedPlusLaws :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Plus f, Alternative.Alternative f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Plus f, Alternative.Alternative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
extendedPlusLaws p = Laws "Plus extended to Alternative" $ lawsProperties (plusLaws p) ++
  [ ("Congruency", extendedPlusLaw p)
  ]

extendedPlusLaw :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Plus f, Alternative.Alternative f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Plus f, Alternative.Alternative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
extendedPlusLaw _ = property $ eq1 (Plus.zero :: f Integer) (Alternative.empty :: f Integer)

plusLeftIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Plus f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Plus f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
plusLeftIdentity _ = property $ \(Apply (m :: f Integer)) -> eq1 (Plus.zero Alt.<!> m) m

plusRightIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Plus f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Plus f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
plusRightIdentity _ = property $ \(Apply (m :: f Integer)) -> eq1 (m Alt.<!> Plus.zero) m

#endif
#endif
#endif

