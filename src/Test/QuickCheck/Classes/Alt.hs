{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Alt
  (
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
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
#if MIN_VERSION_QuickCheck(2,10,0)
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes
#endif
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common

#if MIN_VERSION_QuickCheck(2,10,0)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)

-- | Tests the following alt properties:
--
-- [/Associativity/]
--   @(a '<!>' b) '<!>' c ≡ a '<!>' (b '<!>' c)@
-- [/Left Distributivity/]
--   @f '<$>' (a '<!>' b) = (f '<$>' a) '<!>' (f '<$>' b)
#if defined(VERSION_semigroupoids)
altLaws :: (Alt f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
altLaws p = Laws "Alt"
  [ ("Associativity", altAssociative p)
  , ("Left Distributivity", altLeftDistributive p)
  ]

altAssociative :: forall proxy f. (Alt f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
altAssociative _ = property $ \(Apply (a :: f Integer)) (Apply (b :: f Integer)) (Apply (c :: f Integer)) -> eq1 ((a Alt.<!> b) Alt.<!> c) (a Alt.<!> (b Alt.<!> c))

altLeftDistributive :: forall proxy f. (Alt f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
altLeftDistributive _ = property $ \(Apply (a :: f Integer)) (Apply (b :: f Integer)) -> eq1 (id <$> (a Alt.<!> b)) ((id <$> a) Alt.<!> (id <$> b))
#endif
#endif
#endif

