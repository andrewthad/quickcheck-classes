{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.MonadZip
  (
#if MIN_VERSION_QuickCheck(2,10,0)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
    monadZipLaws
#endif
#endif
  ) where

import Control.Applicative
import Control.Arrow (Arrow(..))
import Control.Monad.Zip (MonadZip(mzip))
import Test.QuickCheck hiding ((.&.))
#if MIN_VERSION_QuickCheck(2,10,0)
import Control.Monad (liftM)
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes (Eq1,Show1)
#endif
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Test.QuickCheck.Classes.Compat (eq1)
#endif

#if MIN_VERSION_QuickCheck(2,10,0)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)

-- | Tests the following monadic zipping properties:
--
-- [/Naturality/]
--   @'liftM' (f '***' g) ('mzip' ma mb) = 'mzip' ('liftM' f ma) ('liftM' g mb)@
--
-- In the laws above, the infix function @'***'@ refers to a typeclass
-- method of 'Arrow'.
monadZipLaws ::
#if MIN_VERSION_base(4,12,0)
  (MonadZip f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (MonadZip f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
monadZipLaws p = Laws "MonadZip"
  [ ("Naturality", monadZipNaturality p)
  ]

monadZipNaturality :: forall proxy f.
#if MIN_VERSION_base(4,12,0)
  (MonadZip f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (MonadZip f, Functor f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
monadZipNaturality _ = property $ \(f' :: LinearEquation) (g' :: LinearEquation) (Apply (ma :: f Integer)) (Apply (mb :: f Integer)) ->
  let f = runLinearEquation f'
      g = runLinearEquation g'
   in eq1 (liftM (f *** g) (mzip ma mb)) (mzip (liftM f ma) (liftM g mb))

#endif

#endif

