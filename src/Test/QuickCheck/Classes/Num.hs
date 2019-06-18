{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Num
  ( numLaws
  ) where

import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common (Laws(..), myForAllShrink)

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
-- [/Additive Inverse/]
--   @'negate' a '+' a ≡ 0@
numLaws :: (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
numLaws p = Laws "Num"
  [ ("Additive Commutativity", numCommutativePlus p)
  , ("Additive Left Identity", numLeftIdentityPlus p)
  , ("Additive Right Identity", numRightIdentityPlus p)
  , ("Multiplicative Associativity", numAssociativeTimes p)
  , ("Multiplicative Left Identity", numLeftIdentityTimes p)
  , ("Multiplicative Right Identity", numRightIdentityTimes p)
  , ("Multiplication Left Distributes Over Addition", numLeftMultiplicationDistributes p)
  , ("Multiplication Right Distributes Over Addition", numRightMultiplicationDistributes p)
  , ("Multiplicative Left Annihilation", numLeftAnnihilation p)
  , ("Multiplicative Right Annihilation", numRightAnnihilation p)
  , ("Additive Inverse", numAdditiveInverse p)
  ]

numLeftMultiplicationDistributes :: forall a. (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
numLeftMultiplicationDistributes _ = myForAllShrink True (const True)
  (\(a :: a,b,c) -> ["a = " ++ show a, "b = " ++ show b, "c = " ++ show c])
  "a * (b + c)"
  (\(a,b,c) -> a * (b + c))
  "(a * b) + (a * c)"
  (\(a,b,c) -> (a * b) + (a * c))

numRightMultiplicationDistributes :: forall a. (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
numRightMultiplicationDistributes _ = myForAllShrink True (const True)
  (\(a :: a,b,c) -> ["a = " ++ show a, "b = " ++ show b, "c = " ++ show c])
  "(a + b) * c"
  (\(a,b,c) -> (a + b) * c)
  "(a * c) + (b * c)"
  (\(a,b,c) -> (a * c) + (b * c))

numLeftIdentityPlus :: forall a. (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
numLeftIdentityPlus _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "0 + a"
  (\a -> 0 + a)
  "a"
  (\a -> a)

numRightIdentityPlus :: forall a. (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
numRightIdentityPlus _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "a + 0"
  (\a -> a + 0)
  "a"
  (\a -> a)

numRightIdentityTimes :: forall a. (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
numRightIdentityTimes _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "a * 1"
  (\a -> a * 1)
  "a"
  (\a -> a)

numLeftIdentityTimes :: forall a. (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
numLeftIdentityTimes _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "1 * a"
  (\a -> 1 * a)
  "a"
  (\a -> a)

numLeftAnnihilation :: forall a. (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
numLeftAnnihilation _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "0 * a"
  (\a -> 0 * a)
  "0"
  (\_ -> 0)

numRightAnnihilation :: forall a. (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
numRightAnnihilation _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "a * 0"
  (\a -> a * 0)
  "0"
  (\_ -> 0)

numCommutativePlus :: forall a. (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
numCommutativePlus _ = myForAllShrink True (const True)
  (\(a :: a,b) -> ["a = " ++ show a, "b = " ++ show b])
  "a + b"
  (\(a,b) -> a + b)
  "b + a"
  (\(a,b) -> b + a)

numAssociativeTimes :: forall a. (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
numAssociativeTimes _ = myForAllShrink True (const True)
  (\(a :: a,b,c) -> ["a = " ++ show a, "b = " ++ show b, "c = " ++ show c])
  "a * (b * c)"
  (\(a,b,c) -> a * (b * c))
  "(a * b) * c"
  (\(a,b,c) -> (a * b) * c)

numAdditiveInverse :: forall a. (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
numAdditiveInverse _ = myForAllShrink True (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "negate a + a"
  (\a -> (-a) + a)
  "0"
  (const 0)
