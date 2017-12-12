{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

{-|

This library provides lists of properties that should hold for common typeclasses.
All of these take a 'Proxy' argument that is used to nail down the type for which
the typeclass dictionaries should be tested. For example, at GHCi:

>>> lawsCheck (monoidLaws (Proxy :: Proxy Ordering))
Monoid: Associative +++ OK, passed 100 tests.
Monoid: Left Identity +++ OK, passed 100 tests.
Monoid: Right Identity +++ OK, passed 100 tests.

Assuming that the 'Arbitrary' instance for 'Ordering' is good, we now
have confidence that the 'Monoid' instance for 'Ordering' satisfies
the monoid laws. We can check multiple typeclasses with:

>>> foldMap (lawsCheck . ($ (Proxy :: Proxy Word))) [jsonLaws,showReadLaws]
ToJSON/FromJSON: Encoding Equals Value +++ OK, passed 100 tests.
ToJSON/FromJSON: Partial Isomorphism +++ OK, passed 100 tests.
Show/Read: Partial Isomorphism +++ OK, passed 100 tests.

-}
module Test.QuickCheck.Classes
  ( -- * Running
    lawsCheck
    -- * Properties
    -- ** Ground Types
  , semigroupLaws
  , monoidLaws
  , commutativeMonoidLaws
  , eqLaws
  , ordLaws
  , showReadLaws
  , jsonLaws
  , isListLaws
  , primLaws
  , storableLaws
#if MIN_VERSION_QuickCheck(2,10,0)
    -- ** Higher-Kinded Types
  , functorLaws
  , applicativeLaws
  , monadLaws
  , foldableLaws
#endif
    -- * Types
  , Laws(..)
  ) where

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO)
import Data.Primitive hiding (sizeOf,newArray,copyArray)
import Data.Primitive.PrimArray
import Data.Proxy
import Control.Monad.ST
import Control.Monad
import Data.Monoid (Endo(..),Sum(..),Dual(..))
import GHC.Ptr (Ptr(..))
import Data.Primitive.Addr (Addr(..))
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import Data.Semigroup (Semigroup)
import GHC.Exts (IsList(fromList,toList,fromListN),Item)
import Foreign.Marshal.Array
import Foreign.Storable
import Text.Read (readMaybe)
import Data.Aeson (FromJSON(..),ToJSON(..))
import Data.Functor.Classes
import Control.Applicative
import Data.Foldable (foldlM,fold,foldMap,foldl',foldr')
import Control.Exception (ErrorCall,evaluate,try)
import Control.Monad.Trans.Class (lift)
import qualified Data.Foldable as F
import qualified Data.Aeson as AE
import qualified Data.Primitive as P
import qualified Data.Semigroup as SG
import qualified GHC.OldList as L

#if MIN_VERSION_QuickCheck(2,10,0)
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
#endif

-- | A set of laws associated with a typeclass.
data Laws = Laws
  { lawsTypeclass :: String
    -- ^ Name of the typeclass whose laws are tested
  , lawsProperties :: [(String,Property)]
    -- ^ Pairs of law name and property
  }

-- | A convenience for working testing properties in GHCi.
--   See the test suite of this library for an example of how to
--   integrate multiple properties into larger test suite.
lawsCheck :: Laws -> IO ()
lawsCheck (Laws className properties) = do
  flip foldlMapM properties $ \(name,p) -> do
    putStr (className ++ ": " ++ name ++ " ")
    quickCheck p

foldlMapM :: (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldlMapM f = foldlM (\b a -> fmap (mappend b) (f a)) mempty

jsonLaws :: (ToJSON a, FromJSON a, Show a, Arbitrary a, Eq a) => Proxy a -> Laws
jsonLaws p = Laws "ToJSON/FromJSON"
  [ ("Encoding Equals Value", jsonEncodingEqualsValue p)
  , ("Partial Isomorphism", jsonEncodingPartialIsomorphism p)
  ]

-- | Tests the following properties:
--
-- [/Partial Isomorphism/]
--   @fromList . toList ≡ id@
-- [/Length Preservation/]
--   @fromList xs ≡ fromListN (length xs) xs@
isListLaws :: (IsList a, Show a, Show (Item a), Arbitrary a, Arbitrary (Item a), Eq a) => Proxy a -> Laws
isListLaws p = Laws "IsList"
  [ ("Partial Isomorphism", isListPartialIsomorphism p)
  , ("Length Preservation", isListLengthPreservation p)
  ]

showReadLaws :: (Show a, Read a, Eq a, Arbitrary a) => Proxy a -> Laws
showReadLaws p = Laws "Show/Read"
  [ ("Partial Isomorphism", showReadPartialIsomorphism p)
  ]

-- | Tests the following properties:
--
-- [/Associative/]
--   @a <> (b <> c) ≡ (a <> b) <> c@
semigroupLaws :: (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
semigroupLaws p = Laws "Semigroup"
  [ ("Associative", semigroupAssociative p)
  ]

-- | Tests the following properties:
--
-- [/Transitive/]
--   @a == b ∧ b == c ⇒ a == c@
-- [/Symmetric/]
--   @a == b ⇒ b == a@
-- [/Reflexive/]
--   @a == a@
--
-- Some of these properties involve implication. In the case that
-- the left hand side of the implication arrow does not hold, we
-- do not retry. Consequently, these properties only end up being
-- useful when the data type has a small number of inhabitants.
eqLaws :: (Eq a, Arbitrary a, Show a) => Proxy a -> Laws
eqLaws p = Laws "Eq"
  [ ("Transitive", eqTransitive p)
  , ("Symmetric", eqSymmetric p)
  , ("Reflexive", eqReflexive p)
  ]

-- | Tests the following properties:
--
-- [/Transitive/]
--   @a ≤ b ∧ b ≤ c ⇒ a ≤ c@
-- [/Comparable/]
--   @a ≤ b ∨ a > b@
ordLaws :: (Ord a, Arbitrary a, Show a) => Proxy a -> Laws
ordLaws p = Laws "Ord"
  [ ("Transitive", ordTransitive p)
  , ("Comparable", ordComparable p)
  ]

-- | Tests the following properties:
--
-- [/Associative/]
--   @mappend a (mappend b c) ≡ mappend (mappend a b) c@
-- [/Left Identity/]
--   @mappend mempty a ≡ a@
-- [/Right Identity/]
--   @mappend a mempty ≡ a@
monoidLaws :: (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
monoidLaws p = Laws "Monoid"
  [ ("Associative", monoidAssociative p)
  , ("Left Identity", monoidLeftIdentity p)
  , ("Right Identity", monoidRightIdentity p)
  ]

-- | Tests everything from 'monoidProps' plus the following:
--
-- [/Commutative/]
--   @mappend a b ≡ mappend b a@
commutativeMonoidLaws :: (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
commutativeMonoidLaws p = Laws "Commutative Monoid" $ lawsProperties (monoidLaws p) ++
  [ ("Commutative", monoidCommutative p)
  ]

-- | Test that a 'Prim' instance obey the several laws.
primLaws :: (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
primLaws p = Laws "Prim"
  [ ("ByteArray Set-Get (you get back what you put in)", primSetGetByteArray p)
  , ("ByteArray Get-Set (putting back what you got out has no effect)", primGetSetByteArray p)
  , ("ByteArray Set-Set (setting twice is same as setting once)", primSetSetByteArray p)
  , ("ByteArray List Conversion Roundtrips", primListByteArray p)
  , ("Addr Set-Get (you get back what you put in)", primSetGetAddr p)
  , ("Addr Get-Set (putting back what you got out has no effect)", primGetSetAddr p)
  , ("Addr List Conversion Roundtrips", primListAddr p)
  ]

storableLaws :: (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
storableLaws p = Laws "Storable"
  [ ("Set-Get (you get back what you put in)", storableSetGet p)
  , ("Get-Set (putting back what you got out has no effect)", storableGetSet p)
  , ("List Conversion Roundtrips", storableList p)
  ]

isListPartialIsomorphism :: forall a. (IsList a, Show a, Arbitrary a, Eq a) => Proxy a -> Property
isListPartialIsomorphism _ = property $ \(a :: a) ->
  fromList (toList a) == a

isListLengthPreservation :: forall a. (IsList a, Show (Item a), Arbitrary (Item a), Eq a) => Proxy a -> Property
isListLengthPreservation _ = property $ \(xs :: [Item a]) ->
  (fromList xs :: a) == fromListN (length xs) xs

showReadPartialIsomorphism :: forall a. (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showReadPartialIsomorphism _ = property $ \(a :: a) ->
  readMaybe (show a) == Just a

-- TODO: improve the quality of the error message if
-- something does not pass this test.
jsonEncodingEqualsValue :: forall a. (ToJSON a, Show a, Arbitrary a) => Proxy a -> Property
jsonEncodingEqualsValue _ = property $ \(a :: a) ->
  case AE.decode (AE.encode a) of
    Nothing -> False
    Just (v :: AE.Value) -> v == toJSON a

jsonEncodingPartialIsomorphism :: forall a. (ToJSON a, FromJSON a, Show a, Eq a, Arbitrary a) => Proxy a -> Property
jsonEncodingPartialIsomorphism _ = property $ \(a :: a) ->
  AE.decode (AE.encode a) == Just a

eqTransitive :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqTransitive _ = property $ \(a :: a) b c -> case a == b of
  True -> case b == c of
    True -> a == c
    False -> a /= c
  False -> case b == c of
    True -> a /= c
    False -> True

-- Technically, this tests something a little stronger than it is supposed to.
-- But that should be alright since this additional strength is implied by
-- the rest of the Ord laws.
ordTransitive :: forall a. (Show a, Ord a, Arbitrary a) => Proxy a -> Property
ordTransitive _ = property $ \(a :: a) b c -> case (compare a b, compare b c) of
  (LT,LT) -> a < c
  (LT,EQ) -> a < c
  (LT,GT) -> True
  (EQ,LT) -> a < c
  (EQ,EQ) -> a == c
  (EQ,GT) -> a > c
  (GT,LT) -> True
  (GT,EQ) -> a > c
  (GT,GT) -> a > c

ordComparable :: forall a. (Show a, Ord a, Arbitrary a) => Proxy a -> Property
ordComparable _ = property $ \(a :: a) b -> a > b || b >= a

eqSymmetric :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqSymmetric _ = property $ \(a :: a) b -> case a == b of
  True -> b == a
  False -> b /= a

eqReflexive :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqReflexive _ = property $ \(a :: a) -> a == a

semigroupAssociative :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupAssociative _ = property $ \(a :: a) b c -> a SG.<> (b SG.<> c) == (a SG.<> b) SG.<> c

monoidAssociative :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidAssociative _ = property $ \(a :: a) b c -> mappend a (mappend b c) == mappend (mappend a b) c

monoidLeftIdentity :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidLeftIdentity _ = property $ \(a :: a) -> mappend mempty a == a

monoidRightIdentity :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidRightIdentity _ = property $ \(a :: a) -> mappend a mempty == a

monoidCommutative :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidCommutative _ = property $ \(a :: a) b -> mappend a b == mappend b a

primListByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primListByteArray _ = property $ \(as :: [a]) ->
  as == toList (fromList as :: PrimArray a)

primListAddr :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primListAddr _ = property $ \(as :: [a]) -> unsafePerformIO $ do
  let len = L.length as
  ptr@(Ptr addr#) :: Ptr a <- mallocBytes (len * P.sizeOf (undefined :: a))
  let addr = Addr addr#
  let go :: Int -> [a] -> IO ()
      go !ix xs = case xs of
        [] -> return ()
        (x : xsNext) -> do
          writeOffAddr addr ix x
          go (ix + 1) xsNext
  go 0 as
  let rebuild :: Int -> IO [a]
      rebuild !ix = if ix < len
        then (:) <$> readOffAddr addr ix <*> rebuild (ix + 1)
        else return []
  asNew <- rebuild 0
  free ptr
  return (as == asNew)

primSetGetByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primSetGetByteArray _ = property $ \(a :: a) len -> (len > 0) ==> do
  ix <- choose (0,len - 1)
  return $ runST $ do
    arr <- newPrimArray len
    writePrimArray arr ix a
    a' <- readPrimArray arr ix
    return (a == a')

primGetSetByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primGetSetByteArray _ = property $ \(as :: [a]) -> (not (L.null as)) ==> do
  let arr1 = fromList as :: PrimArray a
      len = L.length as
  ix <- choose (0,len - 1)
  arr2 <- return $ runST $ do
    marr <- newPrimArray len
    copyPrimArray marr 0 arr1 0 len
    a <- readPrimArray marr ix
    writePrimArray marr ix a
    unsafeFreezePrimArray marr
  return (arr1 == arr2)

primSetSetByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primSetSetByteArray _ = property $ \(a :: a) (as :: [a]) -> (not (L.null as)) ==> do
  let arr1 = fromList as :: PrimArray a
      len = L.length as
  ix <- choose (0,len - 1)
  (arr2,arr3) <- return $ runST $ do
    marr2 <- newPrimArray len
    copyPrimArray marr2 0 arr1 0 len
    writePrimArray marr2 ix a
    marr3 <- newPrimArray len
    copyMutablePrimArray marr3 0 marr2 0 len
    arr2 <- unsafeFreezePrimArray marr2
    writePrimArray marr3 ix a
    arr3 <- unsafeFreezePrimArray marr3
    return (arr2,arr3)
  return (arr2 == arr3)

primSetGetAddr :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primSetGetAddr _ = property $ \(a :: a) len -> (len > 0) ==> do
  ix <- choose (0,len - 1)
  return $ unsafePerformIO $ do
    ptr@(Ptr addr#) :: Ptr a <- mallocBytes (len * P.sizeOf (undefined :: a))
    let addr = Addr addr#
    writeOffAddr addr ix a
    a' <- readOffAddr addr ix
    free ptr
    return (a == a')

primGetSetAddr :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primGetSetAddr _ = property $ \(as :: [a]) -> (not (L.null as)) ==> do
  let arr1 = fromList as :: PrimArray a
      len = L.length as
  ix <- choose (0,len - 1)
  arr2 <- return $ unsafePerformIO $ do
    ptr@(Ptr addr#) :: Ptr a <- mallocBytes (len * P.sizeOf (undefined :: a))
    let addr = Addr addr#
    copyPrimArrayToPtr ptr arr1 0 len
    a :: a <- readOffAddr addr ix
    writeOffAddr addr ix a
    marr <- newPrimArray len
    copyPtrToMutablePrimArray marr 0 ptr len
    free ptr
    unsafeFreezePrimArray marr
  return (arr1 == arr2)

storableSetGet :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storableSetGet _ = property $ \(a :: a) len -> (len > 0) ==> do
  ix <- choose (0,len - 1)
  return $ unsafePerformIO $ do
    ptr :: Ptr a <- mallocArray len
    pokeElemOff ptr ix a
    a' <- peekElemOff ptr ix
    free ptr
    return (a == a')

storableGetSet :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storableGetSet _ = property $ \(as :: [a]) -> (not (L.null as)) ==> do
  let len = L.length as
  ix <- choose (0,len - 1)
  return $ unsafePerformIO $ do
    ptrA <- newArray as
    ptrB <- mallocArray len
    copyArray ptrB ptrA len
    a <- peekElemOff ptrA ix
    pokeElemOff ptrA ix a
    res <- arrayEq ptrA ptrB len
    free ptrA
    free ptrB
    return res

storableList :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storableList _ = property $ \(as :: [a]) -> unsafePerformIO $ do
  let len = L.length as
  ptr <- newArray as
  let rebuild :: Int -> IO [a]
      rebuild !ix = if ix < len
        then (:) <$> peekElemOff ptr ix <*> rebuild (ix + 1)
        else return []
  asNew <- rebuild 0
  free ptr
  return (as == asNew)

arrayEq :: forall a. (Storable a, Eq a) => Ptr a -> Ptr a -> Int -> IO Bool
arrayEq ptrA ptrB len = go 0 where
  go !i = if i < len
    then do
      a <- peekElemOff ptrA i
      b <- peekElemOff ptrB i
      if a == b
        then go (i + 1)
        else return False
    else return True

#if MIN_VERSION_QuickCheck(2,10,0)
-- | Tests the following applicative properties:
--
-- [/Identity/]
--   @'fmap' 'id' ≡ 'id'@
-- [/Composition/]
--   @fmap (f . g) ≡ 'fmap' f . 'fmap' g@
-- [/Const/]
--   @(<$) ≡ 'fmap' 'const'@
functorLaws :: (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Laws
functorLaws p = Laws "Functor"
  [ ("Identity", functorIdentity p)
  , ("Composition", functorComposition p)
  , ("Const", functorConst p)
  ]

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
applicativeLaws :: (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Laws
applicativeLaws p = Laws "Applicative"
  [ ("Identity", applicativeIdentity p)
  , ("Composition", applicativeComposition p)
  , ("Homomorphism", applicativeHomomorphism p)
  , ("Interchange", applicativeInterchange p)
  , ("LiftA2 Part 1", applicativeLiftA2_1 p)
    -- todo: liftA2 part 2, we need an equation of two variables for this
  ]


-- | Tests the following monadic properties:
--
-- [/Left Identity/]
--   @'return' a '>>=' k ≡ k a@
-- [/Right Identity/]
--   @m '>>=' 'return' ≡ m@
-- [/Associativity/]
--   @m '>>=' (\\x -> k x '>>=' h) ≡ (m '>>=' k) '>>=' h@
-- [/Return/]
--   @'pure' ≡ 'return'@
-- [/Ap/]
--   @('<*>') ≡ 'ap'@
monadLaws :: (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Laws
monadLaws p = Laws "Monad"
  [ ("Left Identity", monadLeftIdentity p)
  , ("Right Identity", monadRightIdentity p)
  , ("Associativity", monadAssociativity p)
  , ("Return", monadReturn p)
  , ("Ap", monadAp p)
  ]

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
-- [/foldl/]
--   @'foldl' f z t ≡ 'appEndo' ('getDual' ('foldMap' ('Dual' . 'Endo' . 'flip' f) t)) z@
-- [/foldl'/]
--   @'foldl'' f z0 xs = let f' x k z = k '$!' f z x in 'foldr' f\' 'id' xs z0@
-- [/toList/]
--   @'F.toList' ≡ 'foldr' (:) []@
-- [/null/]
--   @'null' ≡ 'foldr' ('const' ('const' 'False')) 'True'@
-- [/length/]
--   @'length' ≡ getSum . foldMap ('const' ('Sum' 1))@
--
-- Note that this checks to ensure that @foldl\'@ and @foldr\'@
-- are suitably strict.
foldableLaws :: (Foldable f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Laws
foldableLaws = foldableLawsInternal

foldableLawsInternal :: forall f. (Foldable f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Laws
foldableLawsInternal p = Laws "Foldable"
  [ (,) "fold" $ property $ \(Apply (a :: f (Sum Integer))) ->
      fold a == foldMap id a
  , (,) "foldMap" $ property $ \(Apply (a :: f Integer)) (e :: Equation) ->
      let f = Sum . runEquation e
       in foldMap f a == foldr (mappend . f) mempty a
  , (,) "foldr" $ property $ \(e :: EquationTwo) (z :: Integer) (Apply (t :: f Integer)) ->
      let f = runEquationTwo e
       in foldr f z t == appEndo (foldMap (Endo . f) t) z
  , (,) "foldr'" (foldableFoldr' p)
  , (,) "foldl" $ property $ \(e :: EquationTwo) (z :: Integer) (Apply (t :: f Integer)) ->
      let f = runEquationTwo e
       in foldl f z t == appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
  , (,) "foldl'" (foldableFoldl' p)
  , (,) "toList" $ property $ \(Apply (t :: f Integer)) ->
      eq1 (F.toList t) (foldr (:) [] t)
  , (,) "null" $ property $ \(Apply (t :: f Integer)) ->
      null t == foldr (const (const False)) True t
  , (,) "length" $ property $ \(Apply (t :: f Integer)) ->
      length t == getSum (foldMap (const (Sum 1)) t)
  ]

foldableFoldl' :: forall f. (Foldable f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
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
      e <- try (evaluate (foldr f' id xs z0))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    r2 <- lift $ do
      e <- try (evaluate (foldl' f z0 xs))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    return (r1 == r2)

foldableFoldr' :: forall f. (Foldable f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
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
      e <- try (evaluate (foldl f' id xs z0))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    r2 <- lift $ do
      e <- try (evaluate (foldr' f z0 xs))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    return (r1 == r2)

data ChooseSecond = ChooseSecond
  deriving (Eq)

data ChooseFirst = ChooseFirst
  deriving (Eq)

data LastNothing = LastNothing
  deriving (Eq)

data Bottom a = BottomUndefined | BottomValue a
  deriving (Eq)

instance Show ChooseFirst where
  show ChooseFirst = "\\a b -> if even a then a else b"

instance Show ChooseSecond where
  show ChooseSecond = "\\a b -> if even b then a else b"

instance Show LastNothing where
  show LastNothing = "0"

instance Show a => Show (Bottom a) where
  show x = case x of
    BottomUndefined -> "undefined"
    BottomValue a -> show a

instance Arbitrary ChooseSecond where
  arbitrary = pure ChooseSecond

instance Arbitrary ChooseFirst where
  arbitrary = pure ChooseFirst

instance Arbitrary LastNothing where
  arbitrary = pure LastNothing

instance Arbitrary a => Arbitrary (Bottom a) where
  arbitrary = fmap maybeToBottom arbitrary
  shrink x = map maybeToBottom (shrink (bottomToMaybe x))

bottomToMaybe :: Bottom a -> Maybe a
bottomToMaybe BottomUndefined = Nothing
bottomToMaybe (BottomValue a) = Just a

maybeToBottom :: Maybe a -> Bottom a
maybeToBottom Nothing = BottomUndefined
maybeToBottom (Just a) = BottomValue a

data Apply f a = Apply { getApply :: f a }

instance (Eq1 f, Eq a) => Eq (Apply f a) where
  Apply a == Apply b = eq1 a b

data LinearEquation = LinearEquation
  { _linearEquationLinear :: Integer
  , _linearEquationConstant :: Integer
  } deriving (Eq)

data LinearEquationM m = LinearEquationM (m LinearEquation) (m LinearEquation)

runLinearEquation :: Integer -> LinearEquation -> Integer
runLinearEquation x (LinearEquation a b) = a * x + b

runLinearEquationM :: Functor m => LinearEquationM m -> Integer -> m Integer
runLinearEquationM (LinearEquationM e1 e2) i = if odd i
  then fmap (runLinearEquation i) e1
  else fmap (runLinearEquation i) e2

instance Eq1 m => Eq (LinearEquationM m) where
  LinearEquationM a1 b1 == LinearEquationM a2 b2 = eq1 a1 a2 && eq1 b1 b2

showLinear :: Int -> LinearEquation -> ShowS
showLinear _ (LinearEquation a b) = shows a . showString " * x + " . shows b

showLinearList :: [LinearEquation] -> ShowS
showLinearList xs = appEndo $ mconcat
   $ [Endo (showChar '[')]
  ++ L.intersperse (Endo (showChar ',')) (map (Endo . showLinear 0) xs)
  ++ [Endo (showChar ']')]

instance Show1 m => Show (LinearEquationM m) where
  show (LinearEquationM a b) = (\f -> f "")
    $ showString "\\x -> if odd x then "
    . liftShowsPrec showLinear showLinearList 0 a
    . showString " else "
    . liftShowsPrec showLinear showLinearList 0 b

instance Arbitrary1 m => Arbitrary (LinearEquationM m) where
  arbitrary = liftA2 LinearEquationM arbitrary1 arbitrary1
  shrink (LinearEquationM a b) = concat
    [ map (\x -> LinearEquationM x b) (shrink1 a)
    , map (\x -> LinearEquationM a x) (shrink1 b)
    ]

instance Arbitrary LinearEquation where
  arbitrary = do
    (a,b) <- arbitrary
    return (LinearEquation (abs a) (abs b))
  shrink (LinearEquation a b) =
    let xs = shrink (a,b)
     in map (\(x,y) -> LinearEquation (abs x) (abs y)) xs

-- this is a quadratic equation
data Equation = Equation Integer Integer Integer
  deriving (Eq)

-- This show instance is does not actually provide a
-- way to create an equation. Instead, it makes it look
-- like a lambda.
instance Show Equation where
  show (Equation a b c) = "\\x -> " ++ show a ++ " * x ^ 2 + " ++ show b ++ " * x + " ++ show c

instance Arbitrary Equation where
  arbitrary = do
    (a,b,c) <- arbitrary
    return (Equation (abs a) (abs b) (abs c))
  shrink (Equation a b c) =
    let xs = shrink (a,b,c)
     in map (\(x,y,z) -> Equation (abs x) (abs y) (abs z)) xs

runEquation :: Equation -> Integer -> Integer
runEquation (Equation a b c) x = a * x ^ (2 :: Integer) + b * x + c

-- linear equation of two variables
data EquationTwo = EquationTwo Integer Integer
  deriving (Eq)

-- This show instance is does not actually provide a
-- way to create an EquationTwo. Instead, it makes it look
-- like a lambda that takes two variables.
instance Show EquationTwo where
  show (EquationTwo a b) = "\\x y -> " ++ show a ++ " * x + " ++ show b ++ " * y"

instance Arbitrary EquationTwo where
  arbitrary = do
    (a,b) <- arbitrary
    return (EquationTwo (abs a) (abs b))
  shrink (EquationTwo a b) =
    let xs = shrink (a,b)
     in map (\(x,y) -> EquationTwo (abs x) (abs y)) xs

runEquationTwo :: EquationTwo -> Integer -> Integer -> Integer
runEquationTwo (EquationTwo a b) x y = a * x + b * y

-- This show instance is intentionally a little bit wrong.
-- We don't wrap the result in Apply since the end user
-- should not be made aware of the Apply wrapper anyway.
instance (Show1 f, Show a) => Show (Apply f a) where
  showsPrec p = showsPrec1 p . getApply

instance (Arbitrary1 f, Arbitrary a) => Arbitrary (Apply f a) where
  arbitrary = fmap Apply arbitrary1
  shrink = map Apply . shrink1 . getApply

functorIdentity :: forall f. (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
functorIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (fmap id a) a

func1 :: Integer -> (Integer,Integer)
func1 i = (div (i + 5) 3, i * i - 2 * i + 1)

func2 :: (Integer,Integer) -> (Bool,Either Ordering Integer)
func2 (a,b) = (odd a, if even a then Left (compare a b) else Right (b + 2))

functorComposition :: forall f. (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
functorComposition _ = property $ \(Apply (a :: f Integer)) ->
  eq1 (fmap func2 (fmap func1 a)) (fmap (func2 . func1) a)

functorConst :: forall f. (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
functorConst _ = property $ \(Apply (a :: f Integer)) ->
  eq1 (fmap (const 'X') a) ('X' <$ a)

applicativeIdentity :: forall f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
applicativeIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (pure id <*> a) a

applicativeComposition :: forall f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
applicativeComposition _ = property $ \(Apply (u' :: f Equation)) (Apply (v' :: f Equation)) (Apply (w :: f Integer)) ->
  let u = fmap runEquation u'
      v = fmap runEquation v'
   in eq1 (pure (.) <*> u <*> v <*> w) (u <*> (v <*> w))

applicativeHomomorphism :: forall f. (Applicative f, Eq1 f, Show1 f) => Proxy f -> Property
applicativeHomomorphism _ = property $ \(e :: Equation) (a :: Integer) ->
  let f = runEquation e
   in eq1 (pure f <*> pure a) (pure (f a) :: f Integer)

applicativeInterchange :: forall f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
applicativeInterchange _ = property $ \(Apply (u' :: f Equation)) (y :: Integer) ->
  let u = fmap runEquation u'
   in eq1 (u <*> pure y) (pure ($ y) <*> u)

applicativeLiftA2_1 :: forall f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
applicativeLiftA2_1 _ = property $ \(Apply (f' :: f Equation)) (Apply (x :: f Integer)) -> 
  let f = fmap runEquation f'
   in eq1 (liftA2 id f x) (f <*> x)

monadLeftIdentity :: forall f. (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
monadLeftIdentity _ = property $ \(k' :: LinearEquationM f) (a :: Integer) -> 
  let k = runLinearEquationM k'
   in eq1 (return a >>= k) (k a)

monadRightIdentity :: forall f. (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
monadRightIdentity _ = property $ \(Apply (m :: f Integer)) -> 
  eq1 (m >>= return) m

monadAssociativity :: forall f. (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
monadAssociativity _ = property $ \(Apply (m :: f Integer)) (k' :: LinearEquationM f) (h' :: LinearEquationM f) -> 
  let k = runLinearEquationM k'
      h = runLinearEquationM h'
   in eq1 (m >>= (\x -> k x >>= h)) ((m >>= k) >>= h)

monadReturn :: forall f. (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
monadReturn _ = property $ \(x :: Integer) ->
  eq1 (return x) (pure x :: f Integer)

monadAp :: forall f. (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
monadAp _ = property $ \(Apply (f' :: f Equation)) (Apply (x :: f Integer)) -> 
  let f = fmap runEquation f'
   in eq1 (ap f x) (f <*> x)

#endif
