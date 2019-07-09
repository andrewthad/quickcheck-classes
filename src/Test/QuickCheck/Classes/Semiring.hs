{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Semiring
  (
#if HAVE_SEMIRINGS
    semiringLaws
#endif
  ) where

#if HAVE_SEMIRINGS
import Data.Semiring
import Prelude hiding (Num(..))
#if MIN_VERSION_semirings(0,4,0)
import Prelude (fromInteger)
#endif
#endif

import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common (Laws(..), myForAllShrink)

#if HAVE_SEMIRINGS
-- | Tests the following properties:
--
-- [/Additive Commutativity/]
--   @a + b ≡ b + a@
-- [/Additive Left Identity/]
--   @0 + a ≡ a@
-- [/Additive Right Identity/]
--   @a + 0 ≡ a@
-- [/Multiplicative Associativity/]
--   @a * (b * c) ≡ (a * b) * c@
-- [/Multiplicative Left Identity/]
--   @1 * a ≡ a@
-- [/Multiplicative Right Identity/]
--   @a * 1 ≡ a@
-- [/Multiplication Left Distributes Over Addition/]
--   @a * (b + c) ≡ (a * b) + (a * c)@
-- [/Multiplication Right Distributes Over Addition/]
--   @(a + b) * c ≡ (a * c) + (b * c)@
-- [/Multiplicative Left Annihilation/]
--   @0 * a ≡ 0@
-- [/Multiplicative Right Annihilation/]
--   @a * 0 ≡ 0@
--
-- Also tests that 'fromNatural' is a homomorphism of semirings:
--
-- [/FromNatural Maps Zero/]
--   'fromNatural' 0 = 'zero'
-- [/FromNatural Maps One/]
--   'fromNatural' 1 = 'one'
-- [/FromNatural Maps Plus/]
--   'fromNatural' (@a@ + @b@) = 'fromNatural' @a@ + 'fromNatural' @b@
-- [/FromNatural Maps Times/]
--   'fromNatural' (@a@ * @b@) = 'fromNatural' @a@ * 'fromNatural' @b@
semiringLaws :: (Semiring a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
semiringLaws p = Laws "Semiring"
  [ ("Additive Commutativity", semiringCommutativePlus p)
  , ("Additive Left Identity", semiringLeftIdentityPlus p)
  , ("Additive Right Identity", semiringRightIdentityPlus p)
  , ("Multiplicative Associativity", semiringAssociativeTimes p)
  , ("Multiplicative Left Identity", semiringLeftIdentityTimes p)
  , ("Multiplicative Right Identity", semiringRightIdentityTimes p)
  , ("Multiplication Left Distributes Over Addition", semiringLeftMultiplicationDistributes p)
  , ("Multiplication Right Distributes Over Addition", semiringRightMultiplicationDistributes p)
  , ("Multiplicative Left Annihilation", semiringLeftAnnihilation p)
  , ("Multiplicative Right Annihilation", semiringRightAnnihilation p)
#if MIN_VERSION_semirings(0,4,0)
  , ("FromNatural Maps Zero", semiringFromNaturalMapsZero p)
  , ("FromNatural Maps One", semiringFromNaturalMapsOne p)
  , ("FromNatural Maps Plus", semiringFromNaturalMapsPlus p)
  , ("FromNatural Maps Times", semiringFromNaturalMapsTimes p)
#endif
  ]

semiringLeftMultiplicationDistributes :: forall a. (Semiring a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semiringLeftMultiplicationDistributes _ = myForAllShrink True (const True)
  (\(a :: a,b,c) -> ["a = " ++ show a, "b = " ++ show b, "c = " ++ show c])
  "a * (b + c)"
  (\(a,b,c) -> a * (b + c))
  "(a * b) + (a * c)"
  (\(a,b,c) -> (a * b) + (a * c))

semiringRightMultiplicationDistributes :: forall a. (Semiring a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semiringRightMultiplicationDistributes _ = myForAllShrink True (const True)
  (\(a :: a,b,c) -> ["a = " ++ show a, "b = " ++ show b, "c = " ++ show c])
  "(a + b) * c"
  (\(a,b,c) -> (a + b) * c)
  "(a * c) + (b * c)"
  (\(a,b,c) -> (a * c) + (b * c))

semiringLeftIdentityPlus :: forall a. (Semiring a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semiringLeftIdentityPlus _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "0 + a"
  (\a -> zero + a)
  "a"
  (\a -> a)

semiringRightIdentityPlus :: forall a. (Semiring a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semiringRightIdentityPlus _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "a + 0"
  (\a -> a + zero)
  "a"
  (\a -> a)

semiringRightIdentityTimes :: forall a. (Semiring a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semiringRightIdentityTimes _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "a * 1"
  (\a -> a * one)
  "a"
  (\a -> a)

semiringLeftIdentityTimes :: forall a. (Semiring a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semiringLeftIdentityTimes _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "1 * a"
  (\a -> one * a)
  "a"
  (\a -> a)

semiringLeftAnnihilation :: forall a. (Semiring a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semiringLeftAnnihilation _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "0 * a"
  (\a -> zero * a)
  "0"
  (\_ -> zero)

semiringRightAnnihilation :: forall a. (Semiring a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semiringRightAnnihilation _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "a * 0"
  (\a -> a * zero)
  "0"
  (\_ -> zero)

semiringCommutativePlus :: forall a. (Semiring a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semiringCommutativePlus _ = myForAllShrink True (const True)
  (\(a :: a,b) -> ["a = " ++ show a, "b = " ++ show b])
  "a + b"
  (\(a,b) -> a + b)
  "b + a"
  (\(a,b) -> b + a)

semiringAssociativeTimes :: forall a. (Semiring a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semiringAssociativeTimes _ = myForAllShrink True (const True)
  (\(a :: a,b,c) -> ["a = " ++ show a, "b = " ++ show b, "c = " ++ show c])
  "a * (b * c)"
  (\(a,b,c) -> a * (b * c))
  "(a * b) * c"
  (\(a,b,c) -> (a * b) * c)

#if MIN_VERSION_semirings(0,4,0)

semiringFromNaturalMapsZero :: forall a. (Semiring a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semiringFromNaturalMapsZero _ = myForAllShrink False (const True)
  (\_ -> [""])
  "fromNatural 0"
  (\() -> fromNatural 0 :: a)
  "zero"
  (\() -> zero)

semiringFromNaturalMapsOne :: forall a. (Semiring a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semiringFromNaturalMapsOne _ = myForAllShrink False (const True)
  (\_ -> [""])
  "fromNatural 1"
  (\() -> fromNatural 1 :: a)
  "one"
  (\() -> one)

-- | There is no Arbitrary instance for Natural in QuickCheck,
-- so we use NonNegative Integer instead.
semiringFromNaturalMapsPlus :: forall a. (Semiring a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semiringFromNaturalMapsPlus _ = myForAllShrink True (const True)
  (\(NonNegative a, NonNegative b) -> ["a = " ++ show a, "b = " ++ show b])
  "fromNatural (a + b)"
  (\(NonNegative a, NonNegative b) -> fromNatural (fromInteger (a + b)) :: a)
  "fromNatural a + fromNatural b"
  (\(NonNegative a, NonNegative b) -> fromNatural (fromInteger a) + fromNatural (fromInteger b))

semiringFromNaturalMapsTimes :: forall a. (Semiring a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semiringFromNaturalMapsTimes _ = myForAllShrink True (const True)
  (\(NonNegative a, NonNegative b) -> ["a = " ++ show a, "b = " ++ show b])
  "fromNatural (a * b)"
  (\(NonNegative a, NonNegative b) -> fromNatural (fromInteger (a * b)) :: a)
  "fromNatural a * fromNatural b"
  (\(NonNegative a, NonNegative b) -> fromNatural (fromInteger a) * fromNatural (fromInteger b))

#endif

#endif
