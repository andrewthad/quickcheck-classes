{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Arrow
  (
#if HAVE_BINARY_LAWS
    arrowLaws
#endif
  ) where

import Prelude hiding (id, (.))
import qualified Prelude
import Control.Category (Category(..))
import Control.Arrow (Arrow(..), (>>>))
import Test.QuickCheck hiding ((.&.))
#if HAVE_BINARY_LAWS
import Data.Functor.Classes (Eq2,Show2)
#endif
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common
#if HAVE_BINARY_LAWS
import Test.QuickCheck.Classes.Compat (eq2)
#endif

#if HAVE_BINARY_LAWS

-- | Tests the following 'Arrow' properties:
--   
--  * @'arr' id ≡ 'id'@
--
--  * @'arr' (f '>>>' g) ≡ 'arr' f '>>>' 'arr' g@
--
--  * @'first' ('arr' f) ≡ 'arr' ('first' f)@
--
--  * @'first' (f '>>>' g) ≡ 'first' f '>>>' 'first' g@
--
--  * @'first' f '>>>' 'arr' 'fst' ≡ 'arr' 'fst' '>>>' f@
--
--  * @'first' f '>>>' 'arr' ('id' '***' g) ≡ 'arr' ('id' '***' g) '>>>' 'first' f@
--
--  * @'first' ('first' f) '>>>' 'arr' assoc ≡ 'arr' assoc '>>>' 'first' f@
--
--  where
--  
--  > assoc ((a,b),c) = (a,(b,c))
--  
-- /Note/: This property test is only available when this package is built with
-- @base-4.9+@ or @transformers-0.5+@.
arrowLaws :: forall proxy c.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Arrow c, forall a b. (Eq a, Eq b) => Eq (c a b), forall a b. (Show a, Show b) => Show (c a b), forall a b. (Arbitrary a, Arbitrary b) => Arbitrary (c a b))
#else
  (Arrow c, Eq2 c, Show2 c, Arbitrary2 c)
#endif
  => proxy c -> Laws
arrowLaws p = Laws "Arrow"
  [ ("Arr Identity", arrowArrIdentity p)
  , ("Arr Composition", arrowArrComposition p)
  , ("Arr . First === First . Arr", arrowArrFirstFirstArr p)
  , ("First Composition", arrowFirstComposition p)
  , ("Arrow Law 5", arrowLaw5 p)
  , ("Arrow Law 6", arrowLaw6 p)
  , ("Arrow Law 7", arrowLaw7 p)
  ]

type ArrowProp proxy f =
  ( Arrow f
#if HAVE_QUANTIFIED_CONSTRAINTS
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  , forall x y. (Arbitrary x, Arbitrary y) => Arbitrary (f x y)
#else
  , Eq2 f
  , Show2 f
  , Arbitrary2 f 
#endif
  ) => proxy f -> Property

arrowArrIdentity :: forall proxy f. ArrowProp proxy f
arrowArrIdentity _ = property $ do
  arr Prelude.id == (id :: f Integer Integer)

arrowArrComposition :: forall proxy f. ArrowProp proxy f
arrowArrComposition _ = property $ \(f' :: QuadraticEquation) (g' :: QuadraticEquation) ->
  let f = runQuadraticEquation f'
      g = runQuadraticEquation g'
  in (arr (f >>> g) :: f Integer Integer) == (arr f >>> arr g)

arrowArrFirstFirstArr :: forall proxy f. ArrowProp proxy f
arrowArrFirstFirstArr _ = property $ \(f' :: QuadraticEquation) ->
  let f = runQuadraticEquation f'
      x = first (arr f) :: f (Integer, Integer) (Integer, Integer)
      y = arr (first f) :: f (Integer, Integer) (Integer, Integer)
  in x == y

arrowFirstComposition :: forall proxy f. ArrowProp proxy f
arrowFirstComposition _ = property $ \(Apply2 (f :: f Integer Integer)) (Apply2 (g :: f Integer Integer)) ->
  let x = first (f >>> g) :: f (Integer, Integer) (Integer, Integer)
      y = first f >>> first g :: f (Integer, Integer) (Integer, Integer)
  in x == y

arrowLaw5 :: forall proxy f. ArrowProp proxy f
arrowLaw5 _ = property $ \(Apply2 (f :: f Integer Integer)) ->
  let x = first f >>> arr fst :: f (Integer, Integer) Integer
      y = arr fst >>> f :: f (Integer, Integer) Integer
  in x == y 

arrowLaw6 :: forall proxy f. ArrowProp proxy f
arrowLaw6 _ = property $ \(Apply2 (f :: f Integer Integer)) (g' :: QuadraticEquation) ->
  let g = runQuadraticEquation g'
      x = ((first f) >>> (arr (Prelude.id *** g))) :: f (Integer, Integer) (Integer, Integer)
      y = arr (id *** g) >>> first f :: f (Integer, Integer) (Integer, Integer)
  in x == y

arrowLaw7 :: forall proxy f. ArrowProp proxy f
arrowLaw7 _ = property $ \(Apply2 (f :: f Integer Integer)) ->
  let x = first (first f) >>> arr assoc :: f ((Integer, Integer), Integer) (Integer, (Integer, Integer))
      y = arr assoc >>> first f :: f ((Integer, Integer), Integer) (Integer, (Integer, Integer))
  in x == y

assoc :: ((a,b),c) -> (a,(b,c))
assoc ((a,b),c) = (a,(b,c))
#endif
