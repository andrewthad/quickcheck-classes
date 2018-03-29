{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Alternative
  (
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
    alternativeLaws
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
-- [/Identity/]
--   @'empty' '<|>' x ≡ x@
--   @x '<|>' 'empty' ≡ x@
-- [/Associativity/]
--   @a '<|>' (b '<|>' c) ≡ (a '<|>' b) '<|>' c)@
alternativeLaws :: (Alternative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
alternativeLaws p = Laws "Alternative"
  [ ("Identity", alternativeIdentity p)
  , ("Associativity", alternativeAssociativity p)
  ]

alternativeIdentity :: forall proxy f. (Alternative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
alternativeIdentity _ = property $ \(Apply (a :: f Integer)) -> (eq1 (empty <|> a) a) && (eq1 a (empty <|> a))

alternativeAssociativity :: forall proxy f. (Alternative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
alternativeAssociativity _ = property $ \(Apply (a :: f Integer)) (Apply (b :: f Integer)) (Apply (c :: f Integer)) -> eq1 (a <|> (b <|> c)) ((a <|> b) <|> c)

#endif

#endif

