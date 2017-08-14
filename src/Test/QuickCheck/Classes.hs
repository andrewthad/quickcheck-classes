{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.QuickCheck.Classes
  ( primProps
  , storableProps
  , semigroupProps
  , monoidProps
  ) where

import Test.QuickCheck
import Data.Primitive hiding (sizeOf,newArray,copyArray)
import Data.Primitive.PrimArray
import Data.Proxy
import Control.Monad.ST
import GHC.Ptr (Ptr(..))
import Data.Primitive.Addr (Addr(..))
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import Data.Semigroup (Semigroup)
import GHC.Exts (fromList,toList)
import Foreign.Marshal.Array
import Foreign.Storable
import qualified Data.Primitive as P
import qualified Data.Semigroup as SG
import qualified GHC.OldList as L

semigroupProps :: (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> [(String,Property)]
semigroupProps p =
  [ ("Associative", semigroupAssociative p)
  ]

monoidProps :: (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> [(String,Property)]
monoidProps p =
  [ ("Associative", monoidAssociative p)
  , ("Left Identity", monoidLeftIdentity p)
  , ("Right Identity", monoidRightIdentity p)
  ]

primProps :: (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> [(String,Property)]
primProps p =
  [ ("ByteArray Set-Get (you get back what you put in)", primSetGetByteArray p)
  , ("ByteArray Get-Set (putting back what you got out has no effect)", primGetSetByteArray p)
  , ("ByteArray Set-Set (setting twice is same as setting once)", primSetSetByteArray p)
  , ("ByteArray List Conversion Roundtrips", primListByteArray p)
  , ("Addr Set-Get (you get back what you put in)", primSetGetAddr p)
  , ("Addr Get-Set (putting back what you got out has no effect)", primGetSetAddr p)
  , ("Addr List Conversion Roundtrips", primListAddr p)
  ]

storableProps :: (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> [(String,Property)]
storableProps p =
  [ ("Set-Get (you get back what you put in)", storableSetGet p)
  , ("Get-Set (putting back what you got out has no effect)", storableGetSet p)
  , ("List Conversion Roundtrips", storableList p)
  ]

semigroupAssociative :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupAssociative _ = property $ \(a :: a) b c -> a SG.<> (b SG.<> c) == (a SG.<> b) SG.<> c

monoidAssociative :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidAssociative _ = property $ \(a :: a) b c -> mappend a (mappend b c) == mappend (mappend a b) c

monoidLeftIdentity :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidLeftIdentity _ = property $ \(a :: a) -> mappend mempty a == a

monoidRightIdentity :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidRightIdentity _ = property $ \(a :: a) -> mappend a mempty == a

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

