{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Foldable
  (
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
    foldableLaws
#endif  
  ) where

import Data.Monoid
import Data.Foldable (foldMap,Foldable)
import Test.QuickCheck hiding ((.&.))
#if MIN_VERSION_QuickCheck(2,10,0)
import Control.Exception (ErrorCall,try,evaluate)
import Control.Monad.Trans.Class (lift)
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
import Test.QuickCheck.Monadic (monadicIO)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes
#endif
#endif
import Test.QuickCheck.Property (Property)

import qualified Data.Foldable as F
import qualified Data.Semigroup as SG

import Test.QuickCheck.Classes.Common

#if MIN_VERSION_QuickCheck(2,10,0)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)

-- | Tests the following 'Foldable' properties:
--
-- [/fold/]
--   @'fold' ≡ 'foldMap' 'id'@
-- [/foldMap/]
--   @'foldMap' f ≡ 'foldr' ('mappend' . f) 'mempty'@
-- [/foldr/]
--   @'foldr' f z t ≡ 'appEndo' ('foldMap' ('Endo' . f) t ) z@
-- [/foldr'/]
--   @'foldr'' f z0 xs = let f\' k x z = k '$!' f x z in 'foldl' f\' 'id' xs z0@
-- [/foldr1/]
--   @'foldr1' f t ≡ let Just (xs,x) = unsnoc ('toList' t) in 'foldr' f x xs@
-- [/foldl/]
--   @'foldl' f z t ≡ 'appEndo' ('getDual' ('foldMap' ('Dual' . 'Endo' . 'flip' f) t)) z@
-- [/foldl'/]
--   @'foldl'' f z0 xs ≡ let f' x k z = k '$!' f z x in 'foldr' f\' 'id' xs z0@
-- [/foldl1/]
--   @'foldl1' f t ≡ let x : xs = 'toList' t in 'foldl' f x xs@
-- [/toList/]
--   @'F.toList' ≡ 'foldr' (:) []@
-- [/null/]
--   @'null' ≡ 'foldr' ('const' ('const' 'False')) 'True'@
-- [/length/]
--   @'length' ≡ getSum . foldMap ('const' ('Sum' 1))@
--
-- Note that this checks to ensure that @foldl\'@ and @foldr\'@
-- are suitably strict.
foldableLaws :: (Foldable f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
foldableLaws = foldableLawsInternal

foldableLawsInternal :: forall proxy f. (Foldable f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
foldableLawsInternal p = Laws "Foldable"
  [ (,) "fold" $ property $ \(Apply (a :: f (SG.Sum Integer))) ->
      F.fold a == F.foldMap id a
  , (,) "foldMap" $ property $ \(Apply (a :: f Integer)) (e :: Equation) ->
      let f = SG.Sum . runEquation e
       in F.foldMap f a == F.foldr (mappend . f) mempty a
  , (,) "foldr" $ property $ \(e :: EquationTwo) (z :: Integer) (Apply (t :: f Integer)) ->
      let f = runEquationTwo e
       in F.foldr f z t == SG.appEndo (foldMap (SG.Endo . f) t) z
  , (,) "foldr'" (foldableFoldr' p)
  , (,) "foldl" $ property $ \(e :: EquationTwo) (z :: Integer) (Apply (t :: f Integer)) ->
      let f = runEquationTwo e
       in F.foldl f z t == SG.appEndo (SG.getDual (F.foldMap (SG.Dual . SG.Endo . flip f) t)) z
  , (,) "foldl'" (foldableFoldl' p)
  , (,) "foldl1" $ property $ \(e :: EquationTwo) (Apply (t :: f Integer)) ->
      case compatToList t of
        [] -> True
        x : xs ->
          let f = runEquationTwo e
           in F.foldl1 f t == F.foldl f x xs
  , (,) "foldr1" $ property $ \(e :: EquationTwo) (Apply (t :: f Integer)) ->
      case unsnoc (compatToList t) of
        Nothing -> True
        Just (xs,x) ->
          let f = runEquationTwo e
           in F.foldr1 f t == F.foldr f x xs
  , (,) "toList" $ property $ \(Apply (t :: f Integer)) ->
      eq1 (F.toList t) (F.foldr (:) [] t)
#if MIN_VERSION_base(4,8,0)
  , (,) "null" $ property $ \(Apply (t :: f Integer)) ->
      null t == F.foldr (const (const False)) True t
  , (,) "length" $ property $ \(Apply (t :: f Integer)) ->
      F.length t == SG.getSum (F.foldMap (const (SG.Sum 1)) t)
#endif
  ]

unsnoc :: [a] -> Maybe ([a],a)
unsnoc [] = Nothing
unsnoc [x] = Just ([],x)
unsnoc (x:y:xs) = fmap (\(bs,b) -> (x:bs,b)) (unsnoc (y : xs))

compatToList :: Foldable f => f a -> [a]
compatToList = foldMap (\x -> [x])

foldableFoldl' :: forall proxy f. (Foldable f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
foldableFoldl' _ = property $ \(_ :: ChooseSecond) (_ :: LastNothing) (Apply (xs :: f (Bottom Integer))) ->
  monadicIO $ do
    let f :: Integer -> Bottom Integer -> Integer
        f a b = case b of
          BottomUndefined -> error "foldableFoldl' example"
          BottomValue v -> if even v
            then a
            else v
        z0 = 0
    r1 <- lift $ do
      let f' x k z = k $! f z x
      e <- try (evaluate (F.foldr f' id xs z0))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    r2 <- lift $ do
      e <- try (evaluate (F.foldl' f z0 xs))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    return (r1 == r2)

foldableFoldr' :: forall proxy f. (Foldable f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
foldableFoldr' _ = property $ \(_ :: ChooseFirst) (_ :: LastNothing) (Apply (xs :: f (Bottom Integer))) ->
  monadicIO $ do
    let f :: Bottom Integer -> Integer -> Integer
        f a b = case a of
          BottomUndefined -> error "foldableFoldl' example"
          BottomValue v -> if even v
            then v
            else b
        z0 = 0
    r1 <- lift $ do
      let f' k x z = k $! f x z
      e <- try (evaluate (F.foldl f' id xs z0))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    r2 <- lift $ do
      e <- try (evaluate (F.foldr' f z0 xs))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    return (r1 == r2)

#endif

#endif

