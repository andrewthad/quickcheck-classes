{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Ring
  ( 
#if HAVE_SEMIRINGS
    ringLaws
#endif
  ) where

#if HAVE_SEMIRINGS
import Data.Semiring
import Prelude hiding (Num(..))
#endif

import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Internal (Laws(..), myForAllShrink)

#if HAVE_SEMIRINGS
-- | Tests the following properties:
--
-- [/Additive Inverse/]
--   @'negate' a '+' a ≡ 0@
--
-- Note that this does not test any of the laws tested by 'Test.QuickCheck.Classes.Semiring.semiringLaws'.
ringLaws :: (Ring a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
ringLaws p = Laws "Ring"
  [ ("Additive Inverse", ringAdditiveInverse p)
  ]

ringAdditiveInverse :: forall a. (Ring a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
ringAdditiveInverse _ = myForAllShrink True (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "negate a + a"
  (\a -> negate a + a)
  "0"
  (const zero)
#endif
