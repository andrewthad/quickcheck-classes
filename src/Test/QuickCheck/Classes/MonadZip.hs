{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.MonadZip
  (
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
    monadZipLaws
#endif  
  ) where

import Control.Applicative
import Control.Arrow ((***))
import Control.Monad.Zip (MonadZip(mzip))
import Test.QuickCheck hiding ((.&.))
#if MIN_VERSION_QuickCheck(2,10,0)
import Control.Monad (liftM)
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes
#endif
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common

#if MIN_VERSION_QuickCheck(2,10,0)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)

-- | Tests the following monadic zipping properties:
--
-- [/Naturality/]
--   @liftM (f *** g) (mzip ma mb) = mzip (liftM f ma) (liftM g mb)@
--
-- In the laws above, the infix function @***@ refers to a typeclass
-- method of 'Arrow'.
monadZipLaws :: (MonadZip f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
monadZipLaws p = Laws "MonadZip"
  [ ("Naturality", monadZipNaturality p)
  ]

monadZipNaturality :: forall proxy f. (MonadZip f, Functor f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadZipNaturality _ = property $ \(f' :: LinearEquation) (g' :: LinearEquation) (Apply (ma :: f Integer)) (Apply (mb :: f Integer)) ->
  let f = runLinearEquation f'
      g = runLinearEquation g'
   in eq1 (liftM (f *** g) (mzip ma mb)) (mzip (liftM f ma) (liftM g mb))

#endif

#endif

