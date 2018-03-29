{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.MonadPlus
  (
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
    monadPlusLaws
#endif  
  ) where

import Test.QuickCheck hiding ((.&.))
#if MIN_VERSION_QuickCheck(2,10,0)
import Control.Monad (MonadPlus(mzero,mplus))
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes
#endif
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common

#if MIN_VERSION_QuickCheck(2,10,0)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)

-- | Tests the following monad plus properties:
--
-- [/Left Identity/]
--   @'mplus' 'empty' x ≡ x@
-- [/Right Identity/]
--   @'mplus' x 'empty' ≡ x@
-- [/Associativity/]
--   @'mplus' a ('mplus' b c) ≡ 'mplus' ('mplus' a b) c)@ 
-- [/Left Zero/]
--   @'mzero' '>>=' f ≡ 'mzero'@
-- [/Right Zero/]
--   @m >> 'mzero' ≡ 'mzero'@
monadPlusLaws :: (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
monadPlusLaws p = Laws "MonadPlus"
  [ ("Left Identity", monadPlusLeftIdentity p)
  , ("Right Identity", monadPlusRightIdentity p)
  , ("Associativity", monadPlusAssociativity p)
  , ("Left Zero", monadPlusLeftZero p)
  , ("Right Zero", monadPlusRightZero p)
  ]

monadPlusLeftIdentity :: forall proxy f. (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadPlusLeftIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (mplus mzero a) a

monadPlusRightIdentity :: forall proxy f. (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadPlusRightIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (mplus a mzero) a

monadPlusAssociativity :: forall proxy f. (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadPlusAssociativity _ = property $ \(Apply (a :: f Integer)) (Apply (b :: f Integer)) (Apply (c :: f Integer)) -> eq1 (mplus a (mplus b c)) (mplus (mplus a b) c)

monadPlusLeftZero :: forall proxy f. (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadPlusLeftZero _ = property $ \(k' :: LinearEquationM f) -> eq1 (mzero >>= runLinearEquationM k') mzero

monadPlusRightZero :: forall proxy f. (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadPlusRightZero _ = property $ \(Apply (a :: f Integer)) -> eq1 (a >> (mzero :: f Integer)) mzero

#endif

#endif

