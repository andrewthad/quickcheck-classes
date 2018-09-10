{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Alternative
  (
#if MIN_VERSION_QuickCheck(2,10,0)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
    alternativeLaws
#endif
#endif
  ) where

import Control.Applicative (Alternative(..))
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

-- | Tests the following alternative properties:
--
-- [/Left Identity/]
--   @'empty' '<|>' x ≡ x@
-- [/Right Identity/]
--   @x '<|>' 'empty' ≡ x@
-- [/Associativity/]
--   @a '<|>' (b '<|>' c) ≡ (a '<|>' b) '<|>' c)@
alternativeLaws :: (Alternative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
alternativeLaws p = Laws "Alternative"
  [ ("Left Identity", alternativeLeftIdentity p)
  , ("Right Identity", alternativeRightIdentity p)
  , ("Associativity", alternativeAssociativity p)
  ]

alternativeLeftIdentity :: forall proxy f. (Alternative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
alternativeLeftIdentity _ = property $ \(Apply (a :: f Integer)) -> (eq1 (empty <|> a) a)

alternativeRightIdentity :: forall proxy f. (Alternative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
alternativeRightIdentity _ = property $ \(Apply (a :: f Integer)) -> (eq1 a (empty <|> a))

alternativeAssociativity :: forall proxy f. (Alternative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
alternativeAssociativity _ = property $ \(Apply (a :: f Integer)) (Apply (b :: f Integer)) (Apply (c :: f Integer)) -> eq1 (a <|> (b <|> c)) ((a <|> b) <|> c)

#endif

#endif

