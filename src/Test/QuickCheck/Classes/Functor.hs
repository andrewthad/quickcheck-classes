{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Functor
  (
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
    functorLaws
#endif  
  ) where

import Test.QuickCheck hiding ((.&.))
#if MIN_VERSION_QuickCheck(2,10,0)
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes
#endif
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common

#if MIN_VERSION_QuickCheck(2,10,0)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)

-- | Tests the following functor properties:
--
-- [/Identity/]
--   @'fmap' 'id' ≡ 'id'@
-- [/Composition/]
--   @fmap (f . g) ≡ 'fmap' f . 'fmap' g@
-- [/Const/]
--   @(<$) ≡ 'fmap' 'const'@
functorLaws :: (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
functorLaws p = Laws "Functor"
  [ ("Identity", functorIdentity p)
  , ("Composition", functorComposition p)
  , ("Const", functorConst p)
  ]

functorIdentity :: forall proxy f. (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
functorIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (fmap id a) a

functorComposition :: forall proxy f. (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
functorComposition _ = property $ \(Apply (a :: f Integer)) ->
  eq1 (fmap func2 (fmap func1 a)) (fmap (func2 . func1) a)

functorConst :: forall proxy f. (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
functorConst _ = property $ \(Apply (a :: f Integer)) ->
  eq1 (fmap (const 'X') a) ('X' <$ a)

#endif

#endif

