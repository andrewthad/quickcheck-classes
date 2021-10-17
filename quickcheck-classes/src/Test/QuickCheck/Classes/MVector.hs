-- |
-- Module:      Test.QuickCheck.Classes.MVector
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

#if !HAVE_VECTOR
module Test.QuickCheck.Classes.MVector where
#else

module Test.QuickCheck.Classes.MVector
  ( muvectorLaws
  ) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.ST
import Data.Functor
import Data.Proxy (Proxy)
import qualified Data.Vector.Generic.Mutable as MU (basicInitialize)
import qualified Data.Vector.Unboxed.Mutable as MU

import Test.QuickCheck hiding ((.&.))

import Test.QuickCheck.Classes.Internal (Laws(..))

-- | Test that a 'Vector.Unboxed.MVector' instance obey several laws.
muvectorLaws :: (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Laws
muvectorLaws p = Laws "Vector.Unboxed.MVector"
  [ ("New-Length", newLength p)
  , ("Replicate-Length", replicateLength p)
  , ("Slice-Length", sliceLength p)
  , ("Grow-Length", growLength p)

  , ("Write-Read", writeRead p)
  , ("Set-Read", setRead p)
  , ("Sliced-Set-Read", slicedSetRead p)
  , ("Replicate-Read", replicateRead p)

  , ("Slice-Overlaps", sliceOverlaps p)
  , ("Slice-Copy", sliceCopy p)
  , ("Slice-Move", sliceMove p)

  , ("Write-Copy-Read", writeCopyRead p)
  , ("Write-Move-Read", writeMoveRead p)
  , ("Write-Grow-Read", writeGrowRead p)
  , ("Sliced-Write-Copy-Read", slicedWriteCopyRead p)
  , ("Sliced-Write-Move-Read", slicedWriteMoveRead p)
  , ("Sliced-Write-Grow-Read", slicedWriteGrowRead p)

  , ("Write-InitializeAround-Read", writeInitializeAroundRead p)
  , ("Write-ClearAround-Read", writeClearAroundRead p)
  , ("Write-SetAround-Read", writeSetAroundRead p)
  , ("Write-WriteAround-Read", writeWriteAroundRead p)
  , ("Write-CopyAround-Read", writeCopyAroundRead p)
  , ("Write-MoveAround-Read", writeMoveAroundRead p)

  , ("Write-InitializeBetween-Read", writeInitializeBetweenRead p)
  , ("Write-ClearBetween-Read", writeClearBetweenRead p)
  , ("Write-SetBetween-Read", writeSetBetweenRead p)
  , ("Write-CopyBetween-Read", writeCopyBetweenRead p)
  , ("Write-MoveBetween-Read", writeMoveBetweenRead p)
  ]

-------------------------------------------------------------------------------
-- Length

newLength :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
newLength _ = property $ \(NonNegative len) -> do
  (=== len) (runST $ MU.length <$> (MU.new len :: ST s (MU.MVector s a)))

replicateLength :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
replicateLength _ = property $ \(a :: a) (NonNegative len) -> do
  (=== len) (runST $ MU.length <$> MU.replicate len a)

sliceLength :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
sliceLength _ = property $ \(NonNegative ix) (NonNegative subLen) (Positive excess) -> do
  (=== subLen) (runST $ MU.length . MU.slice ix subLen <$> (MU.new (ix + subLen + excess) :: ST s (MU.MVector s a)))

growLength :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
growLength _ = property $ \(Positive len) (Positive by) -> do
  (=== len + by) $ runST $ do
    arr <- MU.new len :: ST s (MU.MVector s a)
    MU.length <$> MU.grow arr by

-------------------------------------------------------------------------------
-- Read

writeRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
writeRead _ = property $ \(a :: a) (NonNegative ix) (Positive excess) -> do
  (=== a) $ runST $ do
    arr <- MU.new (ix + excess)
    MU.write arr ix a
    MU.read arr ix

setRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
setRead _ = property $ \(a :: a) (NonNegative ix) (Positive excess) -> do
  (=== a) $ runST $ do
    arr <- MU.new (ix + excess)
    MU.set arr a
    MU.read arr ix

slicedSetRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
slicedSetRead _ = property $ \(a :: a) (NonNegative ix) (Positive excess) before after -> do
  (=== a) $ runST $ do
    arr <- newSlice before after (ix + excess)
    MU.set arr a
    MU.read arr ix

replicateRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
replicateRead _ = property $ \(a :: a) (NonNegative ix) (Positive excess) -> do
  (=== a) $ runST $ do
    arr <- MU.replicate (ix + excess) a
    MU.read arr ix

-------------------------------------------------------------------------------
-- Overlaps

sliceOverlaps :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
sliceOverlaps _ = property $ \(NonNegative i) (NonNegative ij) (NonNegative jk) (NonNegative kl) (NonNegative lm) -> do
  let j = i + ij
      k = j + jk
      l = k + kl
      m = l + lm
  property $ runST $ do
    arr <- MU.new (m + 1) :: ST s (MU.MVector s a)
    let slice1 = MU.slice i (k - i + 1) arr
        slice2 = MU.slice j (l - j + 1) arr
    pure $ MU.overlaps slice1 slice2

sliceCopy :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
sliceCopy _ = property $ \(a :: a) (NonNegative i) (NonNegative ix) (Positive excess) (NonNegative ij) (NonNegative jk) -> do
  let j = i + ix + excess + ij
      k = j + ix + excess + jk
  runST $ do
    arr <- MU.new k :: ST s (MU.MVector s a)
    let src = MU.slice i (ix + excess) arr
        dst = MU.slice j (ix + excess) arr
    if MU.overlaps src dst then pure (property True) else do
      MU.write src ix a
      MU.copy dst src
      valSrc <- MU.read src ix
      valDst <- MU.read dst ix
      pure (valSrc === a .&&. valDst === a)

sliceMove :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
sliceMove _ = property $ \(a :: a) (NonNegative i) (NonNegative ix) (Positive excess) (NonNegative ij) (NonNegative jk) -> do
  let j = i + ix + excess + ij
      k = j + ix + excess + jk
  (=== a) $ runST $ do
    arr <- MU.new k :: ST s (MU.MVector s a)
    let src = MU.slice i (ix + excess) arr
        dst = MU.slice j (ix + excess) arr
    MU.write src ix a
    MU.move dst src
    MU.read dst ix

-------------------------------------------------------------------------------
-- Write + copy/move/grow

writeCopyRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
writeCopyRead _ = property $ \(a :: a) (NonNegative ix) (Positive excess) -> do
  (=== a) $ runST $ do
    src <- MU.new (ix + excess)
    MU.write src ix a
    dst <- MU.new (ix + excess)
    MU.copy dst src
    MU.clear src
    MU.read dst ix

writeMoveRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
writeMoveRead _ = property $ \(a :: a) (NonNegative ix) (Positive excess) -> do
  (=== a) $ runST $ do
    src <- MU.new (ix + excess)
    MU.write src ix a
    dst <- MU.new (ix + excess)
    MU.move dst src
    MU.clear src
    MU.read dst ix

writeGrowRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
writeGrowRead _ = property $ \(a :: a) (NonNegative ix) (Positive excess) (Positive by) -> do
  (=== a) $ runST $ do
    src <- MU.new (ix + excess)
    MU.write src ix a
    dst <- MU.grow src by
    MU.clear src
    MU.read dst ix

slicedWriteCopyRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
slicedWriteCopyRead _ = property $ \(a :: a) (NonNegative ix) (Positive excess) beforeSrc afterSrc beforeDst afterDst -> do
  (=== a) $ runST $ do
    src <- newSlice beforeSrc afterSrc (ix + excess)
    MU.write src ix a
    dst <- newSlice beforeDst afterDst (ix + excess)
    MU.copy dst src
    MU.clear src
    MU.read dst ix

slicedWriteMoveRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
slicedWriteMoveRead _ = property $ \(a :: a) (NonNegative ix) (Positive excess) beforeSrc afterSrc beforeDst afterDst -> do
  (=== a) $ runST $ do
    src <- newSlice beforeSrc afterSrc (ix + excess)
    MU.write src ix a
    dst <- newSlice beforeDst afterDst (ix + excess)
    MU.move dst src
    MU.clear src
    MU.read dst ix

slicedWriteGrowRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
slicedWriteGrowRead _ = property $ \(a :: a) (NonNegative ix) (Positive excess) (Positive by) beforeSrc afterSrc -> do
  (=== a) $ runST $ do
    src <- newSlice beforeSrc afterSrc (ix + excess)
    MU.write src ix a
    dst <- MU.grow src by
    MU.clear src
    MU.read dst ix

-------------------------------------------------------------------------------
-- Write + overwrite around

writeInitializeAroundRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
writeInitializeAroundRead _ = property $ \(a :: a) (NonNegative ix) (Positive excess) -> do
  (=== a) $ runST $ do
    arr <- MU.new (ix + excess)
    MU.write arr ix a
    when (ix > 0) $
      MU.basicInitialize (MU.slice 0 ix arr)
    when (excess > 1) $
      MU.basicInitialize (MU.slice (ix + 1) (excess - 1) arr)
    MU.read arr ix

writeClearAroundRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
writeClearAroundRead _ = property $ \(a :: a) (NonNegative ix) (Positive excess) -> do
  (=== a) $ runST $ do
    arr <- MU.new (ix + excess)
    MU.write arr ix a
    when (ix > 0) $
      MU.clear (MU.slice 0 ix arr)
    when (excess > 1) $
      MU.clear (MU.slice (ix + 1) (excess - 1) arr)
    MU.read arr ix

writeSetAroundRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
writeSetAroundRead _ = property $ \(a :: a) (b :: a) (NonNegative ix) (Positive excess) -> do
  (=== a) $ runST $ do
    arr <- MU.new (ix + excess)
    MU.write arr ix a
    when (ix > 0) $
      MU.set (MU.slice 0 ix arr) b
    when (excess > 1) $
      MU.set (MU.slice (ix + 1) (excess - 1) arr) b
    MU.read arr ix

writeWriteAroundRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
writeWriteAroundRead _ = property $ \(a :: a) (b :: a) (NonNegative ix) (Positive excess) -> do
  (=== a) $ runST $ do
    arr <- MU.new (ix + excess)
    MU.write arr ix a
    when (ix > 0) $
      MU.write arr (ix - 1) b
    when (excess > 1) $
      MU.write arr (ix + 1) b
    MU.read arr ix

writeCopyAroundRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
writeCopyAroundRead _ = property $ \(a :: a) (NonNegative ix) (Positive excess) -> do
  (=== a) $ runST $ do
    src <- MU.new (ix + excess)
    dst <- MU.new (ix + excess)
    MU.write dst ix a
    when (ix > 0) $
      MU.copy (MU.slice 0 ix dst) (MU.slice 0 ix src)
    when (excess > 1) $
      MU.copy (MU.slice (ix + 1) (excess - 1) dst) (MU.slice (ix + 1) (excess - 1) src)
    MU.read dst ix

writeMoveAroundRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
writeMoveAroundRead _ = property $ \(a :: a) (NonNegative ix) (Positive excess) -> do
  (=== a) $ runST $ do
    src <- MU.new (ix + excess)
    dst <- MU.new (ix + excess)
    MU.write dst ix a
    when (ix > 0) $
      MU.move (MU.slice 0 ix dst) (MU.slice 0 ix src)
    when (excess > 1) $
      MU.move (MU.slice (ix + 1) (excess - 1) dst) (MU.slice (ix + 1) (excess - 1) src)
    MU.read dst ix

-------------------------------------------------------------------------------
-- Two writes + overwrite in between

writeInitializeBetweenRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
writeInitializeBetweenRead _ = property $ \(a :: a) (b :: a) (NonNegative ix) (Positive dix) (Positive excess) -> do
  (=== (a, b)) $ runST $ do
    arr <- MU.new (ix + dix + excess)
    MU.write arr ix a
    MU.write arr (ix + dix) b
    MU.basicInitialize (MU.slice (ix + 1) (dix - 1) arr)
    (,) <$> MU.read arr ix <*> MU.read arr (ix + dix)

writeClearBetweenRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
writeClearBetweenRead _ = property $ \(a :: a) (b :: a) (NonNegative ix) (Positive dix) (Positive excess) -> do
  (=== (a, b)) $ runST $ do
    arr <- MU.new (ix + dix + excess)
    MU.write arr ix a
    MU.write arr (ix + dix) b
    MU.clear (MU.slice (ix + 1) (dix - 1) arr)
    (,) <$> MU.read arr ix <*> MU.read arr (ix + dix)

writeSetBetweenRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
writeSetBetweenRead _ = property $ \(a :: a) (b :: a) (c :: a) (NonNegative ix) (Positive dix) (Positive excess) -> do
  (=== (a, b)) $ runST $ do
    arr <- MU.new (ix + dix + excess)
    MU.write arr ix a
    MU.write arr (ix + dix) b
    MU.set (MU.slice (ix + 1) (dix - 1) arr) c
    (,) <$> MU.read arr ix <*> MU.read arr (ix + dix)

writeCopyBetweenRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
writeCopyBetweenRead _ = property $ \(a :: a) (b :: a) (NonNegative ix) (Positive dix) (Positive excess) -> do
  (=== (a, b)) $ runST $ do
    src <- MU.new (ix + dix + excess)
    dst <- MU.new (ix + dix + excess)
    MU.write dst ix a
    MU.write dst (ix + dix) b
    MU.copy (MU.slice (ix + 1) (dix - 1) dst) (MU.slice (ix + 1) (dix - 1) src)
    (,) <$> MU.read dst ix <*> MU.read dst (ix + dix)

writeMoveBetweenRead :: forall a. (Eq a, MU.Unbox a, Arbitrary a, Show a) => Proxy a -> Property
writeMoveBetweenRead _ = property $ \(a :: a) (b :: a) (NonNegative ix) (Positive dix) (Positive excess) -> do
  (=== (a, b)) $ runST $ do
    src <- MU.new (ix + dix + excess)
    dst <- MU.new (ix + dix + excess)
    MU.write dst ix a
    MU.write dst (ix + dix) b
    MU.move (MU.slice (ix + 1) (dix - 1) dst) (MU.slice (ix + 1) (dix - 1) src)
    (,) <$> MU.read dst ix <*> MU.read dst (ix + dix)

-------------------------------------------------------------------------------
-- Utils

newSlice :: MU.Unbox a => NonNegative Int -> NonNegative Int -> Int -> ST s (MU.MVector s a)
newSlice (NonNegative before) (NonNegative after) len = do
  arr <- MU.new (before + len + after)
  pure $ MU.slice before len arr

#endif
