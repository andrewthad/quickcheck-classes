{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Alt
  (
#if HAVE_UNARY_LAWS
#if defined(VERSION_semigroupoids)
    altLaws
#endif
#endif
) where

import Data.Functor

#if defined(VERSION_semigroupoids)
import Data.Functor.Alt (Alt)
import qualified Data.Functor.Alt as Alt
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
-- [/Associativity/]
--   @(a 'Alt.<!>' b) 'Alt.<!>' c ≡ a 'Alt.<!>' (b 'Alt.<!>' c)@
-- [/Left Distributivity/]
--   @f '<$>' (a 'Alt.<!>' b) ≡ (f '<$>' a) 'Alt.<!>' (f '<$>' b)@
#if defined(VERSION_semigroupoids)
altLaws :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Alt f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Alt f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
altLaws p = Laws "Alt"
  [ ("Associativity", altAssociative p)
  , ("Left Distributivity", altLeftDistributive p)
  ]

altAssociative :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Alt f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Alt f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
altAssociative _ = property $ \(Apply (a :: f Integer)) (Apply (b :: f Integer)) (Apply (c :: f Integer)) -> eq1 ((a Alt.<!> b) Alt.<!> c) (a Alt.<!> (b Alt.<!> c))

altLeftDistributive :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Alt f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Alt f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
altLeftDistributive _ = property $ \(Apply (a :: f Integer)) (Apply (b :: f Integer)) -> eq1 (id <$> (a Alt.<!> b)) ((id <$> a) Alt.<!> (id <$> b))
#endif
#endif
