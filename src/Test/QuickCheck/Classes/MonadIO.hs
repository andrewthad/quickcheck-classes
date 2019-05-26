{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.MonadIO
  (
#if HAVE_UNARY_LAWS
    monadIOLaws
#endif
  ) where

import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative
import Test.QuickCheck hiding ((.&.))
import Control.Monad.IO.Class (MonadIO(..))
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

-- | Tests the following 'MonadIO' properties:
--
-- [/Return/]
--   @'liftIO' '.' 'return' ≡ 'return'@
-- [/LiftIO Transforms/]
--   @'liftIO' (m '>>=' f) ≡ 'liftIO' m '>>=' ('liftIO' '.' f)@
monadIOLaws ::
#if HAVE_QUANTIFIED_CONSTRAINTS
  (MonadIO f, Applicative f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (MonadIO f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
monadIOLaws p = Laws "Monad"
  [ ("Return", monadIOReturn p)
  , ("LiftIO Transform", monadIOTransform p)
  ]

type MonadIOProp proxy f =
  ( MonadIO f
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

monadIOReturn :: forall proxy f. MonadIOProp proxy f
monadIOReturn _ = property $ \(x :: Integer) -> liftIO (pure x) == (pure x :: f Integer)

monadIOTransform :: forall proxy f. MonadIOProp proxy f
monadIOTransform _ = property $ \(m' :: ShowIO Integer) (f' :: LinearEquation) ->
  let m = getShowIO m'
      f = pure . runLinearEquation f'
      x = liftIO (m >>= f) :: f Integer
      y = liftIO m >>= (liftIO . f) :: f Integer 
  in x == y

newtype ShowIO a = ShowIO { getShowIO :: IO a }

instance Show a => Show (ShowIO a) where
  show = unsafePerformIO . fmap (\x -> "IO <val: " ++ show x ++ ">") . getShowIO

instance Arbitrary a => Arbitrary (ShowIO a) where
  arbitrary = fmap (ShowIO . pure) arbitrary

#endif
