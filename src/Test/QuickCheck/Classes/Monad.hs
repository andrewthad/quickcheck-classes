{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Monad
  (
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
    monadLaws
#endif  
  ) where

import Control.Applicative
import Test.QuickCheck hiding ((.&.))
#if MIN_VERSION_QuickCheck(2,10,0)
import Control.Monad (ap)
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes
#endif
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common

#if MIN_VERSION_QuickCheck(2,10,0)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)

-- | Tests the following monadic properties:
--
-- [/Left Identity/]
--   @'return' a '>>=' k ≡ k a@
-- [/Right Identity/]
--   @m '>>=' 'return' ≡ m@
-- [/Associativity/]
--   @m '>>=' (\\x -> k x '>>=' h) ≡ (m '>>=' k) '>>=' h@
-- [/Return/]
--   @'pure' ≡ 'return'@
-- [/Ap/]
--   @('<*>') ≡ 'ap'@
monadLaws :: (Monad f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
monadLaws p = Laws "Monad"
  [ ("Left Identity", monadLeftIdentity p)
  , ("Right Identity", monadRightIdentity p)
  , ("Associativity", monadAssociativity p)
  , ("Return", monadReturn p)
  , ("Ap", monadAp p)
  ]

monadLeftIdentity :: forall proxy f. (Monad f, Functor f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadLeftIdentity _ = property $ \(k' :: LinearEquationM f) (a :: Integer) ->
  let k = runLinearEquationM k'
   in eq1 (return a >>= k) (k a)

monadRightIdentity :: forall proxy f. (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadRightIdentity _ = property $ \(Apply (m :: f Integer)) ->
  eq1 (m >>= return) m

monadAssociativity :: forall proxy f. (Monad f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadAssociativity _ = property $ \(Apply (m :: f Integer)) (k' :: LinearEquationM f) (h' :: LinearEquationM f) ->
  let k = runLinearEquationM k'
      h = runLinearEquationM h'
   in eq1 (m >>= (\x -> k x >>= h)) ((m >>= k) >>= h)

monadReturn :: forall proxy f. (Monad f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadReturn _ = property $ \(x :: Integer) ->
  eq1 (return x) (pure x :: f Integer)

monadAp :: forall proxy f. (Monad f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadAp _ = property $ \(Apply (f' :: f QuadraticEquation)) (Apply (x :: f Integer)) ->
  let f = fmap runQuadraticEquation f'
   in eq1 (ap f x) (f <*> x)

#endif

#endif

