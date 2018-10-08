{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Semigroup
  ( -- * Laws
    semigroupLaws
  , commutativeSemigroupLaws
  , specialSemigroupLaws
    -- * Special Semigroups
  , SpecialSemigroup
  , commutative
  , exponential
  , idempotent
  , rectangularBand
  ) where

import Prelude hiding (foldr1)
import Data.Semigroup (Semigroup(..))
import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common (Laws(..), myForAllShrink)

import Data.Foldable (foldr1,toList)
import Data.List.NonEmpty (NonEmpty((:|)))

import qualified Data.List as L

-- | Tests the following properties:
--
-- [/Associative/]
--   @a '<>' (b '<>' c) ≡ (a '<>' b) '<>' c@
-- [/Concatenation/]
--   @'sconcat' as ≡ 'foldr1' ('<>') as@
-- [/Times/]
--   @'stimes' n a ≡ 'foldr1' ('<>') (replicate n a)@
semigroupLaws :: (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
semigroupLaws p = Laws "Semigroup"
  [ ("Associative", semigroupAssociative p)
  , ("Concatenation", semigroupConcatenation p)
  , ("Times", semigroupTimes p)
  ]

-- | Tests everything from 'semigroupLaws', plus the following:
--
-- [/Commutative/]
--   @a '<>' b ≡ b '<>' a@
commutativeSemigroupLaws :: (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
commutativeSemigroupLaws p = Laws "Commutative Semigroup" $ lawsProperties (semigroupLaws p) ++
  [ ("Commutative", semigroupCommutative p)
  ]

specialSemigroupLaws :: (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> [SpecialSemigroup] -> Laws
specialSemigroupLaws p xs = Laws "Special Semigroup" $ concat
  [ lawsProperties (semigroupLaws p)
  , map (\x -> (specialName x, specialProperty p x)) (L.nub xs)
  ]

semigroupAssociative :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupAssociative _ = myForAllShrink True (const True)
  (\(a :: a,b,c) -> ["a = " ++ show a, "b = " ++ show b, "c = " ++ show c])
  "a <> (b <> c)"
  (\(a,b,c) -> a <> (b <> c))
  "(a <> b) <> c"
  (\(a,b,c) -> (a <> b) <> c)

semigroupCommutative :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupCommutative _ = myForAllShrink True (const True)
  (\(a :: a,b) -> ["a = " ++ show a, "b = " ++ show b])
  "a <> b"
  (\(a,b) -> a <> b)
  "b <> a"
  (\(a,b) -> b <> a)

semigroupConcatenation :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupConcatenation _ = myForAllShrink True (const True)
  (\(a, SmallList (as :: [a])) -> ["as = " ++ show (a :| as)])
  "sconcat as"
  (\(a, SmallList as) -> sconcat (a :| as))
  "foldr1 (<>) as"
  (\(a, SmallList as) -> foldr1 (<>) (a :| as))

semigroupTimes :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupTimes _ = myForAllShrink True (\(_,n) -> n > 0)
  (\(a :: a, n :: Int) -> ["a = " ++ show a, "n = " ++ show n])
  "stimes n a"
  (\(a,n) -> stimes n a)
  "foldr1 (<>) (replicate n a)"
  (\(a,n) -> foldr1 (<>) (replicate n a))

semigroupExponential :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupExponential _ = myForAllShrink True (\(_,_,n) -> n > 0)
  (\(a :: a, b, n :: Int) -> ["a = " ++ show a, "b = " ++ show b, "n = " ++ show n])
  "stimes n (a <> b)"
  (\(a,b,n) -> stimes n (a <> b))
  "stimes n a <> stimes n b"
  (\(a,b,n) -> stimes n a <> stimes n b)

semigroupIdempotent :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupIdempotent _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "a <> a"
  (\a -> a <> a)
  "a"
  (\a -> a)

semigroupRectangularBand :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupRectangularBand _ = myForAllShrink False (const True)
  (\(a :: a, b) -> ["a = " ++ show a, "b = " ++ show b])
  "a <> b <> a"
  (\(a,b) -> a <> b <> a)
  "a"
  (\(a,_) -> a)

newtype SmallList a = SmallList { getSmallList :: [a] }
  deriving (Eq,Show)

instance Arbitrary a => Arbitrary (SmallList a) where
  arbitrary = do
    n <- choose (0,6)
    xs <- vector n
    return (SmallList xs)
  shrink = map SmallList . shrink . getSmallList

-- | Additional properties that a semigroup may have.
data SpecialSemigroup
  = Commutative
  | Idempotent
  | RectangularBand
  | Exponential
  deriving (Eq)

-- | @a '<>' b ≡ b '<>' a@
commutative :: SpecialSemigroup
commutative = Commutative

-- | @stimes n (a '<>' b) ≡ 'stimes' n a '<>' 'stimes' n b@
exponential :: SpecialSemigroup
exponential = Exponential

-- | @a '<>' a ≡ a@
idempotent :: SpecialSemigroup
idempotent = Idempotent

-- | @a '<>' b ' <> 'a' ≡ a@
rectangularBand :: SpecialSemigroup
rectangularBand = RectangularBand

-- Consider adding some approximation of a reductive semigroup law later.
-- @(∀ x. x '<>' a ≡ x '<>' b) ⇒ (a ≡ b)@
-- reductive :: SpecialSemigroup
-- reductive = Reductive

specialName :: SpecialSemigroup -> String
specialName x = case x of
  Commutative -> "Commutative"
  Idempotent -> "Idempotent"
  RectangularBand -> "Rectangular Band"
  Exponential -> "Exponential"

specialProperty :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> SpecialSemigroup -> Property
specialProperty p x = case x of
  Commutative -> semigroupCommutative p
  Idempotent -> semigroupIdempotent p
  RectangularBand -> semigroupRectangularBand p
  Exponential -> semigroupExponential p


