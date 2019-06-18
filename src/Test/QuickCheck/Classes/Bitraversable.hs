{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Bitraversable
  (
#if HAVE_BINARY_LAWS
    bitraversableLaws
#endif
  ) where

import Data.Bitraversable(Bitraversable(..))
import Test.QuickCheck hiding ((.&.))
#if HAVE_BINARY_LAWS
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Classes (Eq2,Show2)
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common
#if HAVE_BINARY_LAWS
import Test.QuickCheck.Classes.Compat (eq2)
#endif

#if HAVE_BINARY_LAWS

-- | Tests the following 'Bitraversable' properties:
--
-- [/Naturality/]
--   @'bitraverse' (t '.' f) (t '.' g) ≡ t '.' 'bitraverse' f g@ for every applicative transformation @t@
-- [/Identity/]
--   @'bitraverse' 'Identity' 'Identity' ≡ 'Identity'
-- [/Composition/] 
--   @'Compose' '.' 'fmap' ('bitraverse' g1 g2) '.' 'bitraverse' f1 f2 ≡ 'bitraverse' ('Compose' '.' 'fmap' g1 g2 '.' f1) ('Compose' '.' 'fmap' g2 '.' f2)
--
-- /Note/: This property test is only available when this package is built with
-- @base-4.9+@ or @transformers-0.5+@.
bitraversableLaws :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Bitraversable f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bitraversable f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Laws
bitraversableLaws p = Laws "Bitraversable"
  [ ("Naturality", bitraversableIdentity p)
  , ("Identity", bitraversableIdentity p)
  , ("Composition", bitraversableComposition p)
  ]

bitraversableNaturality :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Bitraversable f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bitraversable f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Property
bitraversableNaturality _ = property $ \(Apply2 (x :: f Integer Integer)) ->
  let t = apTrans
      f = func4
      g = func4
      x' = bitraverse (t . f) (t . g) x
      y' = t (bitraverse f g x)
  in x' == y'

bitraversableIdentity :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Bitraversable f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bitraversable f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Property
bitraversableIdentity _ = property $ \(Apply2 (x :: f Integer Integer)) -> (bitraverse Identity Identity x) == (Identity x)

bitraversableComposition :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Bitraversable f, forall a b. (Eq a, Eq b) => Eq (f a b), forall a b. (Show a, Show b) => Show (f a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (f a b))
#else
  (Bitraversable f, Eq2 f, Show2 f, Arbitrary2 f)
#endif
  => proxy f -> Property
bitraversableComposition _ = property $ \(Apply2 (x :: f Integer Integer)) ->
  let f1 = func6
      f2 = func5
      g1 = func4
      g2 = func4
      x' = Compose . fmap (bitraverse g1 g2) . bitraverse f1 f2 $ x
      y' = bitraverse (Compose . fmap g1 . f1) (Compose . fmap g2 . f2) x
  in x' == y'

#endif
