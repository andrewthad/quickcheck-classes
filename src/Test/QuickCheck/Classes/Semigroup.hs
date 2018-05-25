{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Semigroup
  ( semigroupLaws
  ) where

import Prelude hiding (foldr1)
import Data.Semigroup (Semigroup(..))
import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common (Laws(..), myForAllShrink)

import Data.Foldable (foldr1,toList)
import Data.List.NonEmpty (NonEmpty((:|)))

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

semigroupAssociative :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupAssociative _ = property $ \(a :: a) b c -> a <> (b <> c) == (a <> b) <> c

semigroupConcatenation :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupConcatenation _ = myForAllShrink True (const True)
  (\(a, as :: [a]) -> ["as = " ++ show (a :| as)])
  "sconcat as"
  (\(a,as) -> sconcat (a :| as))
  "foldr1 (<>) as"
  (\(a,as) -> foldr1 (<>) (a :| as))

semigroupTimes :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupTimes _ = myForAllShrink True (\(_,n) -> n > 0)
  (\(a :: a, n :: Int) -> ["a = " ++ show a, "n = " ++ show n])
  "stimes n a"
  (\(a,n) -> stimes n a)
  "foldr1 (<>) (replicate n a)"
  (\(a,n) -> foldr1 (<>) (replicate n a))

