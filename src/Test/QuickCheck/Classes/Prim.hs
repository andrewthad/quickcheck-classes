{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Prim
  ( primLaws
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState,primitive,primitive_)
import Control.Monad.ST
import Data.Proxy (Proxy)
import Data.Primitive hiding (sizeOf, newArray, copyArray)
import Data.Primitive.Addr (Addr(..))
import Foreign.Marshal.Alloc
import GHC.Exts
  (Int(I#),(*#),newByteArray#,unsafeFreezeByteArray#,copyMutableByteArray#
  ,copyByteArray#,quotInt#,sizeofByteArray#)

#if MIN_VERSION_base(4,7,0)
import GHC.Exts (IsList(fromList,toList,fromListN),Item,
  copyByteArrayToAddr#,copyAddrToByteArray#)
#endif

import GHC.Ptr (Ptr(..))
import System.IO.Unsafe
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

import qualified Data.List as L
import qualified Data.Primitive as P

import Test.QuickCheck.Classes.Common (Laws(..))

-- | Test that a 'Prim' instance obey the several laws.
primLaws :: (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
primLaws p = Laws "Prim"
  [ ("ByteArray Set-Get (you get back what you put in)", primSetGetByteArray p)
  , ("ByteArray Get-Set (putting back what you got out has no effect)", primGetSetByteArray p)
  , ("ByteArray Set-Set (setting twice is same as setting once)", primSetSetByteArray p)
#if MIN_VERSION_base(4,7,0)
  , ("ByteArray List Conversion Roundtrips", primListByteArray p)
#endif
  , ("Addr Set-Get (you get back what you put in)", primSetGetAddr p)
  , ("Addr Get-Set (putting back what you got out has no effect)", primGetSetAddr p)
  , ("Addr List Conversion Roundtrips", primListAddr p)
  ]

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
  let arr1 = primArrayFromList as :: PrimArray a
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
  let arr1 = primArrayFromList as :: PrimArray a
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
  let arr1 = primArrayFromList as :: PrimArray a
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


-- byte array with phantom variable that specifies element type
data PrimArray a = PrimArray ByteArray#
data MutablePrimArray s a = MutablePrimArray (MutableByteArray# s)

instance (Eq a, Prim a) => Eq (PrimArray a) where
  a1 == a2 = sizeofPrimArray a1 == sizeofPrimArray a2 && loop (sizeofPrimArray a1 - 1)
    where
    loop !i | i < 0 = True
            | otherwise = indexPrimArray a1 i == indexPrimArray a2 i && loop (i-1)

#if MIN_VERSION_base(4,7,0)
instance Prim a => IsList (PrimArray a) where
  type Item (PrimArray a) = a
  fromList = primArrayFromList
  fromListN = primArrayFromListN
  toList = primArrayToList
#endif

indexPrimArray :: forall a. Prim a => PrimArray a -> Int -> a
indexPrimArray (PrimArray arr#) (I# i#) = indexByteArray# arr# i#

sizeofPrimArray :: forall a. Prim a => PrimArray a -> Int
sizeofPrimArray (PrimArray arr#) = I# (quotInt# (sizeofByteArray# arr#) (sizeOf# (undefined :: a)))

newPrimArray :: forall m a. (PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
newPrimArray (I# n#)
  = primitive (\s# ->
      case newByteArray# (n# *# sizeOf# (undefined :: a)) s# of
        (# s'#, arr# #) -> (# s'#, MutablePrimArray arr# #)
    )

readPrimArray :: (Prim a, PrimMonad m) => MutablePrimArray (PrimState m) a -> Int -> m a
readPrimArray (MutablePrimArray arr#) (I# i#)
  = primitive (readByteArray# arr# i#)

writePrimArray ::
     (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> Int
  -> a
  -> m ()
writePrimArray (MutablePrimArray arr#) (I# i#) x
  = primitive_ (writeByteArray# arr# i# x)

unsafeFreezePrimArray
  :: PrimMonad m => MutablePrimArray (PrimState m) a -> m (PrimArray a)
unsafeFreezePrimArray (MutablePrimArray arr#)
  = primitive (\s# -> case unsafeFreezeByteArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, PrimArray arr'# #))


copyPrimArrayToPtr :: forall m a. (PrimMonad m, Prim a)
  => Ptr a       -- ^ destination pointer
  -> PrimArray a -- ^ source array
  -> Int         -- ^ offset into source array
  -> Int         -- ^ number of prims to copy
  -> m ()
copyPrimArrayToPtr (Ptr addr#) (PrimArray ba#) (I# soff#) (I# n#) =
#if MIN_VERSION_base(4,7,0)
  primitive (\ s# ->
      let s'# = copyByteArrayToAddr# ba# (soff# *# siz#) addr# (n# *# siz#) s#
      in (# s'#, () #))
  where siz# = sizeOf# (undefined :: a)
#else
  generateM_ n $ \ix -> writeOffAddr (ptrToAddr addr) ix (indexPrimArray ba (ix + soff))
#endif

--ptrToAddr :: Ptr a -> Addr
--ptrToAddr (Ptr x) = Addr x

copyPtrToMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a
  -> Int
  -> Ptr a
  -> Int
  -> m ()
copyPtrToMutablePrimArray (MutablePrimArray ba#) (I# doff#) (Ptr addr#) (I# n#) =
#if MIN_VERSION_base(4,7,0)
  primitive (\ s# ->
      let s'# = copyAddrToByteArray# addr# ba# (doff# *# siz#) (n# *# siz#) s#
      in (# s'#, () #))
  where siz# = sizeOf# (undefined :: a)
#else
  generateM_ n $ \ix -> do
    x <- readOffAddr (ptrToAddr addr) ix
    writePrimArray ba (doff + ix) x
#endif

copyMutablePrimArray :: forall m a.
     (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> MutablePrimArray (PrimState m) a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of bytes to copy
  -> m ()
copyMutablePrimArray (MutablePrimArray dst#) (I# doff#) (MutablePrimArray src#) (I# soff#) (I# n#)
  = primitive_ (copyMutableByteArray#
      src#
      (soff# *# (sizeOf# (undefined :: a)))
      dst#
      (doff# *# (sizeOf# (undefined :: a)))
      (n# *# (sizeOf# (undefined :: a)))
    )

copyPrimArray :: forall m a.
     (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> PrimArray a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of bytes to copy
  -> m ()
copyPrimArray (MutablePrimArray dst#) (I# doff#) (PrimArray src#) (I# soff#) (I# n#)
  = primitive_ (copyByteArray#
      src#
      (soff# *# (sizeOf# (undefined :: a)))
      dst#
      (doff# *# (sizeOf# (undefined :: a)))
      (n# *# (sizeOf# (undefined :: a)))
    )

primArrayFromList :: Prim a => [a] -> PrimArray a
primArrayFromList xs = primArrayFromListN (L.length xs) xs

primArrayFromListN :: forall a. Prim a => Int -> [a] -> PrimArray a
primArrayFromListN len vs = runST run where
  run :: forall s. ST s (PrimArray a)
  run = do
    arr <- newPrimArray len
    let go :: [a] -> Int -> ST s ()
        go !xs !ix = case xs of
          [] -> return ()
          a : as -> do
            writePrimArray arr ix a
            go as (ix + 1)
    go vs 0
    unsafeFreezePrimArray arr

primArrayToList :: forall a. Prim a => PrimArray a -> [a]
primArrayToList arr = go 0 where
  !len = sizeofPrimArray arr
  go :: Int -> [a]
  go !ix = if ix < len
    then indexPrimArray arr ix : go (ix + 1)
    else []


#if MIN_VERSION_base(4,7,0)
primListByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primListByteArray _ = property $ \(as :: [a]) ->
  as == toList (fromList as :: PrimArray a)
#endif
