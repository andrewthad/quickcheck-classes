{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Eq
  ( eqLaws
  , substitutiveEqLaws
  ) where

import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Function

import Test.QuickCheck.Classes.Internal (Laws(..))

-- | Tests the following properties:
--
-- [/Transitive/]
--   @a '==' b ∧ b '==' c ⇒ a '==' c@
-- [/Symmetric/]
--   @a '==' b ⇒ b '==' a@
-- [/Reflexive/]
--   @a '==' a@
-- [/Negation/]
--   @x '/=' y '==' 'not' (x '==' y)@
--
-- Some of these properties involve implication. In the case that
-- the left hand side of the implication arrow does not hold, we
-- do not retry. Consequently, these properties only end up being
-- useful when the data type has a small number of inhabitants.
eqLaws :: (Eq a, Arbitrary a, Show a) => Proxy a -> Laws
eqLaws p = Laws "Eq"
  [ ("Transitive", eqTransitive p)
  , ("Symmetric", eqSymmetric p)
  , ("Reflexive", eqReflexive p)
  ]

eqTransitive :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqTransitive _ = property $ \(a :: a) b c -> case a == b of
  True -> case b == c of
    True -> a == c
    False -> a /= c
  False -> case b == c of
    True -> a /= c
    False -> True

eqSymmetric :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqSymmetric _ = property $ \(a :: a) b -> case a == b of
  True -> b == a
  False -> b /= a

eqReflexive :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqReflexive _ = property $ \(a :: a) -> a == a

eqNegation :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqNegation _ = property $ \(x :: a) y -> (x /= y) == not (x == y)

-- | Tests the following properties:
--
-- [/Substitutive/]
--   @x '==' y ⇒ f x '==' f y@
--
-- /Note/: This does not test `eqLaws`.
-- If you want to use this, You should use it in addition to `eqLaws`.
substitutiveEqLaws :: forall a. (Eq a, Arbitrary a, CoArbitrary a, Function a, Show a) => Proxy a -> Laws
substitutiveEqLaws _ = Laws "Eq"
  [ ("Substitutivity"
    , property $ \(x :: a) y (f :: Fun a Integer) ->
        x == y ==> applyFun f x == applyFun f y
    )
  ]
