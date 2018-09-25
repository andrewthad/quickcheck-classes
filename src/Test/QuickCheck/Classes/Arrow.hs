{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

-- N.B.: This module is not currently built.
module Test.QuickCheck.Classes.Arrow
  (
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
    arrowLaws
#endif  
  ) where

import Prelude hiding (id, (.))
import Control.Applicative
import Control.Arrow (Arrow(..))
import Control.Category (Category(..), (>>>), (<<<))
import Test.QuickCheck hiding ((.&.))
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes (Eq2,Show2)
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Test.QuickCheck.Classes.Compat (eq2)
#endif

#if MIN_VERSION_QuickCheck(2,10,0)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)

-- | Tests the following 'Arrow' properties:
-- [/Law1/]
--   @'arr' 'id' ≡ 'id'@
-- [/Law2/]
--   @'arr' (f '>>>' g) ≡ 'arr' f '>>>' 'arr' g@
-- [/Law3/]
--   @'first' ('arr' f) ≡ 'arr' ('first' f)@
-- [/Law4/]
--   @'first' (f '>>>' g) ≡ 'first' f >>> 'first' g@
-- [/Law5/]
--   @'first' f '>>>' 'arr' 'fst' ≡ 'arr' 'fst' '>>>' f
-- [/Law6/]
--   @'first' f '>>>' 'arr' ('id' '***' g) ≡ 'arr' ('id' '***' g) '>>>' 'first' f@
-- [/Law7/]
--   @'first' ('first' f) '>>>' 'arr' assoc ≡ 'arr' assoc '>>>' 'first' f@
--
--   where
--   @assoc ((a,b),c) = (a,(b,c))
--
-- /Note/: This property test is only available when this package is built with
-- @base-4.9+@ or @transformers-0.5+@.
arrowLaws :: forall proxy f.
#if MIN_VERSION_base(4,12,0)
  (Arrow f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Arrow f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Laws
arrowLaws p = Laws "Arrow"
  [ ("Law1", arrowLaw1 p)
  ]

arrowLaw1 :: forall proxy f.
#if MIN_VERSION_base(4,12,0)
  (Arrow f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Arrow f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Property
arrowLaw1 _ = property $ \(Apply2 (x :: f Integer Integer)) -> eq2 (arr id x) (id x)

#endif

#endif

