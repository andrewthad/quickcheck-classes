{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Applicative
  (
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
    applicativeLaws
#endif  
  ) where

import Control.Applicative
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

-- | Tests the following applicative properties:
--
-- [/Identity/]
--   @'pure' 'id' '<*>' v ≡ v@
-- [/Composition/]
--   @'pure' (.) '<*>' u '<*>' v '<*>' w ≡ u '<*>' (v '<*>' w)@
-- [/Homomorphism/]
--   @'pure' f '<*>' 'pure' x ≡ 'pure' (f x)@
-- [/Interchange/]
--   @u '<*>' 'pure' y ≡ 'pure' ('$' y) '<*>' u@
-- [/LiftA2 (1)/]
--   @('<*>') ≡ 'liftA2' 'id'@
applicativeLaws :: (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
applicativeLaws p = Laws "Applicative"
  [ ("Identity", applicativeIdentity p)
  , ("Composition", applicativeComposition p)
  , ("Homomorphism", applicativeHomomorphism p)
  , ("Interchange", applicativeInterchange p)
  , ("LiftA2 Part 1", applicativeLiftA2_1 p)
    -- todo: liftA2 part 2, we need an equation of two variables for this
  ]

applicativeIdentity :: forall proxy f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
applicativeIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (pure id <*> a) a

applicativeComposition :: forall proxy f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
applicativeComposition _ = property $ \(Apply (u' :: f Equation)) (Apply (v' :: f Equation)) (Apply (w :: f Integer)) ->
  let u = fmap runEquation u'
      v = fmap runEquation v'
   in eq1 (pure (.) <*> u <*> v <*> w) (u <*> (v <*> w))

applicativeHomomorphism :: forall proxy f. (Applicative f, Eq1 f, Show1 f) => proxy f -> Property
applicativeHomomorphism _ = property $ \(e :: Equation) (a :: Integer) ->
  let f = runEquation e
   in eq1 (pure f <*> pure a) (pure (f a) :: f Integer)

applicativeInterchange :: forall proxy f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
applicativeInterchange _ = property $ \(Apply (u' :: f Equation)) (y :: Integer) ->
  let u = fmap runEquation u'
   in eq1 (u <*> pure y) (pure ($ y) <*> u)

applicativeLiftA2_1 :: forall proxy f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
applicativeLiftA2_1 _ = property $ \(Apply (f' :: f Equation)) (Apply (x :: f Integer)) ->
  let f = fmap runEquation f'
   in eq1 (liftA2 id f x) (f <*> x)

#endif

#endif

