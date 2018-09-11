{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Traversable
  (
#if MIN_VERSION_QuickCheck(2,10,0)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
    traversableLaws
#endif
#endif
  ) where

import Data.Foldable (foldMap)
import Data.Traversable (Traversable,fmapDefault,foldMapDefault,sequenceA,traverse)
import Test.QuickCheck hiding ((.&.))
#if MIN_VERSION_QuickCheck(2,10,0)
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Identity
#endif
#endif

import qualified Data.Set as S

import Test.QuickCheck.Classes.Common

#if MIN_VERSION_QuickCheck(2,10,0)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)

-- | Tests the following 'Traversable' properties:
--
-- [/Naturality/]
--   @t '.' 'traverse' f ≡ 'traverse' (t '.' f)@
--   for every applicative transformation @t@
-- [/Identity/]
--   @'traverse' 'Identity' ≡ 'Identity'@
-- [/Composition/]
--   @'traverse' ('Compose' '.' 'fmap' g '.' f) ≡ 'Compose' '.' 'fmap' ('traverse' g) '.' 'traverse' f@
-- [/Sequence Naturality/]
--   @t '.' 'sequenceA' ≡ 'sequenceA' '.' 'fmap' t@
--   for every applicative transformation @t@
-- [/Sequence Identity/]
--   @'sequenceA' '.' 'fmap' 'Identity' ≡ 'Identity'@
-- [/Sequence Composition/]
--   @'sequenceA' '.' 'fmap' 'Compose' ≡ 'Compose' '.' 'fmap' 'sequenceA' '.' 'sequenceA'@
-- [/foldMap/]
--   @'foldMap' ≡ 'foldMapDefault'@
-- [/fmap/]
--   @'fmap' ≡ 'fmapDefault'@
--
-- Where an /applicative transformation/ is a function
--
-- @t :: (Applicative f, Applicative g) => f a -> g a@
--
-- preserving the 'Applicative' operations, i.e.
--
-- * Identity: @t ('pure' x) ≡ 'pure' x@
-- * Distributivity: @t (x '<*>' y) ≡ t x '<*>' t y@
traversableLaws :: (Traversable f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
traversableLaws = traversableLawsInternal

traversableLawsInternal :: forall proxy f. (Traversable f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
traversableLawsInternal _ = Laws "Traversable"
  [ (,) "Naturality" $ property $ \(Apply (a :: f Integer)) ->
      propNestedEq1 (apTrans (traverse func4 a)) (traverse (apTrans . func4) a)
  , (,) "Identity" $ property $ \(Apply (t :: f Integer)) ->
      nestedEq1 (traverse Identity t) (Identity t)
  , (,) "Composition" $ property $ \(Apply (t :: f Integer)) ->
      nestedEq1 (traverse (Compose . fmap func5 . func6) t) (Compose (fmap (traverse func5) (traverse func6 t)))
  , (,) "Sequence Naturality" $ property $ \(Apply (x :: f (Compose Triple ((,) (S.Set Integer)) Integer))) ->
      let a = fmap toSpecialApplicative x in
      propNestedEq1 (apTrans (sequenceA a)) (sequenceA (fmap apTrans a))
  , (,) "Sequence Identity" $ property $ \(Apply (t :: f Integer)) ->
      nestedEq1 (sequenceA (fmap Identity t)) (Identity t)
  , (,) "Sequence Composition" $ property $ \(Apply (t :: f (Triple (Triple Integer)))) ->
      nestedEq1 (sequenceA (fmap Compose t)) (Compose (fmap sequenceA (sequenceA t)))
  , (,) "foldMap" $ property $ \(Apply (t :: f Integer)) ->
      foldMap func3 t == foldMapDefault func3 t
  , (,) "fmap" $ property $ \(Apply (t :: f Integer)) ->
      eq1 (fmap func3 t) (fmapDefault func3 t)
  ]


#endif

#endif

