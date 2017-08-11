{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.QuickCheck.Classes
  ( primProps
  , monoidProps
  ) where

import Test.QuickCheck
import Data.Primitive
import Data.Primitive.PrimArray
import Data.Proxy
import Control.Monad.ST
import GHC.Ptr (Ptr(..))
import Data.Primitive.Addr (Addr(..))
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import Data.Semigroup (Semigroup)
import GHC.Exts (fromList)
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
  [ ("Set-Get ByteArray (you get back what you put in)", primSetGetByteArray p)
  , ("Get-Set ByteArray (putting back what you got out has no effect)", primGetSet p)
  , ("Set-Set ByteArray (setting twice is same as setting once)", primSetSet p)
  , ("Set-Get Addr (you get back what you put in)", primSetGetAddr p)
  ]

semigroupAssociative :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupAssociative _ = property $ \(a :: a) b c -> a SG.<> (b SG.<> c) == (a SG.<> b) SG.<> c

monoidAssociative :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidAssociative _ = property $ \(a :: a) b c -> mappend a (mappend b c) == mappend (mappend a b) c

monoidLeftIdentity :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidLeftIdentity _ = property $ \(a :: a) -> mappend mempty a == a

monoidRightIdentity :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidRightIdentity _ = property $ \(a :: a) -> mappend a mempty == a

primSetGetByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primSetGetByteArray _ = property $ \(a :: a) len -> (len > 0) ==> do
  ix <- choose (0,len - 1)
  return $ runST $ do
    arr <- newPrimArray len
    writePrimArray arr ix a
    a' <- readPrimArray arr ix
    return (a == a')

primGetSet :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primGetSet _ = property $ \(as :: [a]) -> (not (L.null as)) ==> do
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

primSetSet :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primSetSet _ = property $ \(a :: a) (as :: [a]) -> (not (L.null as)) ==> do
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
    ptr@(Ptr addr#) :: Ptr a <- mallocBytes (len * sizeOf (undefined :: a))
    let addr = Addr addr#
    writeOffAddr addr ix a
    a' <- readOffAddr addr ix
    free ptr
    return (a == a')

