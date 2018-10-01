{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE QuantifiedConstraints #-}
#endif
{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Generic
  (
#if MIN_VERSION_base(4,6,0)
    genericLaws
  , genericLawsU
#endif
#if MIN_VERSION_base(4,12,0) 
--  , generic1Laws
#endif
  ) where

#if MIN_VERSION_base(4,6,0)
import Control.Applicative
import Data.Semigroup
import Data.Monoid
import GHC.Generics
#if MIN_VERSION_base(4,9,0)
import Data.Functor.Classes
#endif
import Data.Proxy (Proxy(Proxy))
import Test.QuickCheck
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common (Laws(..), Apply(..))

-- | Tests the following properties:
--
-- [/Conjunction Idempotence/]
--   @n .&. n ≡ n@
-- [/Disjunction Idempotence/]
--   @n .|. n ≡ n@
-- [/Double Complement/]
--   @complement (complement n) ≡ n@
--
-- /Note:/ This property test is only available when
-- using @base-4.6@ or newer.
genericLaws :: (Generic a, Eq a, Arbitrary a, Show a, Show (Rep a x), Arbitrary (Rep a x), Eq (Rep a x)) => Proxy a -> Proxy x -> Laws
genericLaws pa px = Laws "Generic"
  [ ("From-To idempotence", fromToIdempotence pa px)
  , ("To-From idempotence", toFromIdempotence pa)
  ]

genericLawsU :: (Generic a, Eq a, Arbitrary a, Show a, Show (Rep a ()), Arbitrary (Rep a ()), Eq (Rep a ())) => Proxy a -> Laws
genericLawsU pa = Laws "Generic"
  [ ("From-To idempotence", fromToIdempotenceU pa)
  , ("To-From idempotence", toFromIdempotence pa)
  ]


toFromIdempotence :: forall proxy a. (Generic a, Eq a, Arbitrary a, Show a) => proxy a -> Property
toFromIdempotence _ = property $ \(v :: a) -> (to . from $ v) == v

fromToIdempotence ::
     forall proxy a x.
     (Generic a, Show (Rep a x), Arbitrary (Rep a x), Eq (Rep a x))
  => proxy a
  -> proxy x
  -> Property
fromToIdempotence _ _ = property $ \(r :: Rep a x) -> r == (from (to r :: a)) 

fromToIdempotenceU ::
     forall a.
     (Generic a, Show (Rep a ()), Arbitrary (Rep a ()), Eq (Rep a ()))
  => Proxy a
  -> Property
fromToIdempotenceU pa = fromToIdempotence pa (Proxy :: Proxy ())
#if MIN_VERSION_base(4,12,0)

generic1Laws :: (Generic1 f, Eq1 f, Arbitrary1 f, Show1 f, Eq1 (Rep1 f), Show1 (Rep1 f), Arbitrary1 (Rep1 f))
  => Proxy f -> Laws
generic1Laws p = Laws "Generic1"
  [ ("From1-To1 idempotence", fromToIdempotence1 p)
  , ("To1-From1 idempotence", toFromIdempotence1 p)
  ]

-- hack for quantified constraints: under base >= 4.12,
-- our usual 'Apply' wrapper has a {Eq,Show,Arbitrary}
-- instances that are incompatible.
newtype GApply f a = GApply { getGApply :: f a }

instance (Applicative f, Semigroup a) => Semigroup (GApply f a) where
  GApply x <> GApply y = GApply $ liftA2 (<>) x y

instance (Applicative f, Monoid a) => Monoid (GApply f a) where
  mempty = GApply $ pure mempty
  mappend = (<>)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
instance (Eq1 f, Eq a) => Eq (GApply f a) where
  GApply a == GApply b = eq1 a b

instance (Show1 f, Show a) => Show (GApply f a) where
  showsPrec p = showsPrec1 p . getGApply

#if MIN_VERSION_QuickCheck(2,10,0)
instance (Arbitrary1 f, Arbitrary a) => Arbitrary (GApply f a) where
  arbitrary = fmap GApply arbitrary1
  shrink = map GApply . shrink1 . getGApply
#endif
#endif

toFromIdempotence1 :: forall proxy f. (Generic1 f, Eq1 f, Arbitrary1 f, Show1 f) => proxy f -> Property
toFromIdempotence1 _ = property $ \(GApply (v :: f Integer)) -> eq1 v (to1 . from1 $ v)

fromToIdempotence1 :: forall proxy f. (Generic1 f, Eq1 (Rep1 f), Arbitrary1 (Rep1 f), Show1 (Rep1 f)) => proxy f -> Property
fromToIdempotence1 _ = property $ \(GApply (r :: Rep1 f Integer)) -> eq1 r (from1 ((to1 $ r) :: f Integer))

#endif

#endif
