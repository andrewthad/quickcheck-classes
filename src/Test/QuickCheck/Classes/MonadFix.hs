{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.MonadFix
  (
#if HAVE_UNARY_LAWS
    monadFixLaws
#endif
  ) where

import Control.Applicative
import Test.QuickCheck hiding ((.&.))
import Control.Monad (liftM)
import Control.Monad.Fix (MonadFix(..))
import Data.Function (fix)
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

-- | Tests the following 'MonadFix' properties:
--
-- [/Purity/]
--   @'mfix' ('return' '.' h) ≡ 'return' ('fix' h)
-- [/Left Shrinking (or Tightening)/]
--   @'mfix' (\x -> a '>>=' \y -> f x y) ≡ a '>>=' \y -> 'mfix' (\x -> f x y)@
-- [/Sliding/]
--   @'mfix' ('liftM' h '.' f) ≡ 'liftM' h ('mfix' (f '.' h))@, for strict @h@. 
-- [/Nesting/]
--   @'mfix' (\x -> 'mfix' (\y -> f x y)) ≡ 'mfix' (\x -> f x x)@ 
monadFixLaws ::
#if HAVE_QUANTIFIED_CONSTRAINTS
  (MonadFix f, Applicative f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (MonadFix f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
monadFixLaws p = Laws "MonadFix"
  [ ("Purity", monadFixPurity p)
  , ("Left Shrinking (or Tightening)", monadFixLeftShrinking p)
  , ("Sliding", monadFixSliding p)
  , ("Nesting", monadFixNesting p)
  ]

type MonadFixProp proxy f =
  ( MonadFix f
#if HAVE_QUANTIFIED_CONSTRAINTS
  , forall x. Eq x => Eq (f x)
  , forall x. Show x => Show (f x)
  , forall x. Arbitrary x => Arbitrary (f x)
#else
  , Eq1 f
  , Show1 f
  , Arbitrary1 f
#endif
  ) => proxy f -> Property

monadFixPurity :: forall proxy f. MonadFixProp proxy f
monadFixPurity _ = property $ \(h' :: QuadraticEquation) ->
  let h = runQuadraticEquation h'
      x = mfix (return . h) :: f Integer
      y = return (fix h) :: f Integer
  in x == y

monadFixLeftShrinking :: forall proxy f. MonadFixProp proxy f
monadFixLeftShrinking _ = property $ \(Apply (a :: f Integer)) (f' :: LinearEquationTwo) ->
  let f a' b' = return $ runLinearEquationTwo f' a' b'
      x' = mfix (\x -> a >>= \y -> f x y) :: f Integer
      y' = a >>= \y -> mfix (\x -> f x y) :: f Integer
  in x' == y'

monadFixSliding :: forall proxy f. MonadFixProp proxy f
monadFixSliding _ = property $ \(f' :: QuadraticEquation) ->
  let f :: Integer -> f Integer
      f = return . runQuadraticEquation f'
      h !i = let !x = i * i + 7 in x
      x' = mfix (liftM h . f) :: f Integer
      y' = liftM h (mfix (f . h)) :: f Integer
  in x' == y'

monadFixNesting :: forall proxy f. MonadFixProp proxy f
monadFixNesting _ = property $ \(f' :: LinearEquationTwo) ->
  let f a' b' = return $ runLinearEquationTwo f' a' b'
      x' = mfix (\x -> mfix (\y -> f x y)) :: f Integer
      y' = mfix (\x -> f x x) :: f Integer
  in x' == y'

#endif
