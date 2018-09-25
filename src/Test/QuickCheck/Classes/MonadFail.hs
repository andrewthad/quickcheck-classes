{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.MonadFail
  (
#if MIN_VERSION_QuickCheck(2,10,0)
#if MIN_VERSION_base(4,9,0) && MIN_VERSION_transformers(0,4,0)
    monadFailLaws
#endif
#endif
  ) where

import Control.Applicative
import Test.QuickCheck hiding ((.&.))
#if MIN_VERSION_QuickCheck(2,10,0)
import Control.Monad (ap)
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
#if MIN_VERSION_base(4,9,0) && MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes (Eq1,Show1)
import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
#endif
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Test.QuickCheck.Classes.Compat (eq1)
#endif

#if MIN_VERSION_QuickCheck(2,10,0)

#if MIN_VERSION_base(4,9,0) && MIN_VERSION_transformers(0,4,0)

-- | Tests the following 'MonadFail' properties:
-- 
-- [/Left Zero/]
-- @'fail' s '>>=' f ≡ 'fail' s@
monadFailLaws :: forall proxy f.
#if MIN_VERSION_base(4,12,0)
  (MonadFail f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (MonadFail f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
monadFailLaws p = Laws "Monad"
  [ ("Left Zero", monadFailLeftZero p)
  ]
 
monadFailLeftZero :: forall proxy f.
#if MIN_VERSION_base(4,12,0)
  (MonadFail f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (MonadFail f, Functor f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
monadFailLeftZero _ = property $ \(k' :: LinearEquationM f) (s :: String) ->
  let k = runLinearEquationM k'
  in eq1 (fail s >>= k) (fail s)

#endif

#endif

