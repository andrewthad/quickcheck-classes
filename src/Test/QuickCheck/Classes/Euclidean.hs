{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

#if !HAVE_SEMIRINGS || !MIN_VERSION_semirings(0,4,2)
module Test.QuickCheck.Classes.Euclidean where
#else

module Test.QuickCheck.Classes.Euclidean
  ( gcdDomainLaws
  , euclideanLaws
  ) where

import Prelude hiding (quotRem, quot, rem, gcd, lcm)
import Data.Maybe
import Data.Proxy (Proxy)
import Data.Euclidean
import Data.Semiring (Semiring(..))

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common (Laws(..))

-- | Test that a 'GcdDomain' instance obey several laws.
gcdDomainLaws :: (Eq a, GcdDomain a, Arbitrary a, Show a) => Proxy a -> Laws
gcdDomainLaws p = Laws "GcdDomain"
  [ ("divide1", divideLaw1 p)
  , ("divide2", divideLaw2 p)
  , ("gcd1", gcdLaw1 p)
  , ("gcd2", gcdLaw2 p)
  , ("lcm1", lcmLaw1 p)
  , ("lcm2", lcmLaw2 p)
  , ("coprime", coprimeLaw p)
  ]

divideLaw1 :: forall a. (Eq a, GcdDomain a, Arbitrary a, Show a) => Proxy a -> Property
divideLaw1 _ = property $ \(x :: a) y ->
  y /= zero ==> (x `times` y) `divide` y === Just x

divideLaw2 :: forall a. (Eq a, GcdDomain a, Arbitrary a, Show a) => Proxy a -> Property
divideLaw2 _ = property $ \(x :: a) y ->
  y /= zero ==> maybe (property True) (\z -> x === z `times` y) (x `divide` y)

gcdLaw1 :: forall a. (Eq a, GcdDomain a, Arbitrary a, Show a) => Proxy a -> Property
gcdLaw1 _ = property $ \(x :: a) y ->
  x /= zero || y /= zero ==> isJust (x `divide` gcd x y) .&&. isJust (y `divide` gcd x y)

gcdLaw2 :: forall a. (Eq a, GcdDomain a, Arbitrary a, Show a) => Proxy a -> Property
gcdLaw2 _ = property $ \(x :: a) y z ->
  z /= zero ==> isJust (gcd (x `times` z) (y `times` z) `divide` z)

lcmLaw1 :: forall a. (Eq a, GcdDomain a, Arbitrary a, Show a) => Proxy a -> Property
lcmLaw1 _ = property $ \(x :: a) y ->
  x /= zero && y /= zero ==> isJust (lcm x y `divide` x) .&&. isJust (lcm x y `divide` y)

lcmLaw2 :: forall a. (Eq a, GcdDomain a, Arbitrary a, Show a) => Proxy a -> Property
lcmLaw2 _ = property $ \(x :: a) y z ->
  x /= zero && y /= zero ==> isNothing (z `divide` x) .||. isNothing (z `divide` y) .||. isJust (z `divide` lcm x y)

coprimeLaw :: forall a. (Eq a, GcdDomain a, Arbitrary a, Show a) => Proxy a -> Property
coprimeLaw _ = property $ \(x :: a) y ->
  y /= zero ==> coprime x y === isJust (one `divide` gcd x y)

-- | Test that a 'Euclidean' instance obey several laws.
euclideanLaws :: (Eq a, Euclidean a, Arbitrary a, Show a) => Proxy a -> Laws
euclideanLaws p = Laws "Euclidean"
  [ ("degree", degreeLaw p)
  , ("quotRem", quotRemLaw p)
  , ("quot", quotLaw p)
  , ("rem", remLaw p)
  ]

degreeLaw :: forall a. (Eq a, Euclidean a, Arbitrary a, Show a) => Proxy a -> Property
degreeLaw _ = property $ \(x :: a) y ->
  y /= zero ==> let (_, r) = x `quotRem` y in (r === zero .||. degree r < degree y)

quotRemLaw :: forall a. (Eq a, Euclidean a, Arbitrary a, Show a) => Proxy a -> Property
quotRemLaw _ = property $ \(x :: a) y ->
  y /= zero ==> let (q, r) = x `quotRem` y in x === (q `times` y) `plus` r

quotLaw :: forall a. (Eq a, Euclidean a, Arbitrary a, Show a) => Proxy a -> Property
quotLaw _ = property $ \(x :: a) y ->
  y /= zero ==> quot x y === fst (quotRem x y)

remLaw :: forall a. (Eq a, Euclidean a, Arbitrary a, Show a) => Proxy a -> Property
remLaw _ = property $ \(x :: a) y ->
  y /= zero ==> rem x y === snd (quotRem x y)

#endif
