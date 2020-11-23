{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Storable
  ( storableLaws
  ) where

import Control.Applicative
import Control.Monad
import Data.Proxy (Proxy)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.Ptr (Ptr(..), plusPtr)
import Test.QuickCheck hiding ((.&.))

import Test.QuickCheck.Classes.Internal (Laws(..))

-- | Tests the following 'Storable' properties:
--
-- [/Set-Get/]
--   @('pokeElemOff' ptr ix a >> 'peekElemOff' ptr ix') ≡ 'pure' a@
-- [/Get-Set/]
--   @('peekElemOff' ptr ix >> 'pokeElemOff' ptr ix a) ≡ 'pure' a@
storableLaws :: (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
storableLaws p = Laws "Storable"
  [ ("Set-Get (you get back what you put in)", storableSetGet p)
  , ("Get-Set (putting back what you got out has no effect)", storableGetSet p)
  , ("Set-Set (if you set something twice, the first set is inconsequential", storableSetSet p)
  , ("List Conversion Roundtrips", storableList p)
  , ("peekElemOff a i ≡ peek (plusPtr a (i * sizeOf undefined))", storablePeekElem p)
  , ("peekElemOff a i x ≡ poke (plusPtr a (i * sizeOf undefined)) x ≡ id ", storablePokeElem p)
  , ("peekByteOff a i ≡ peek (plusPtr a i)", storablePeekByte p)
  , ("peekByteOff a i x ≡ poke (plusPtr a i) x ≡ id ", storablePokeByte p)
  ]

arrayArbitrary :: forall a. (Arbitrary a, Storable a) => Int -> IO (Ptr a)
arrayArbitrary = newArray <=< generate . vector

storablePeekElem :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storablePeekElem _ = property $ \(Positive len) ix' -> ioProperty $ do
  let ix = ix' `mod` len
  addr :: Ptr a <- arrayArbitrary len
  x <- peekElemOff addr ix
  y <- peek (addr `advancePtr` ix)
  free addr
  return (x ==== y)

storablePokeElem :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storablePokeElem _ = property $ \(Positive len) (x :: a) ix' -> ioProperty $ do
  let ix = ix' `mod` len
  addr <- arrayArbitrary len
  pokeElemOff addr ix x
  u <- peekElemOff addr ix
  poke (addr `advancePtr` ix) x
  v <- peekElemOff addr ix
  free addr
  return (u ==== v)

storablePeekByte :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storablePeekByte _ = property $ \(Positive len) off' -> ioProperty $ do
  let off = (off' `mod` len) * sizeOf (undefined :: a)
  addr :: Ptr a <- arrayArbitrary len
  x :: a <- peekByteOff addr off
  y :: a <- peek (addr `plusPtr` off)
  free addr
  return (x ==== y)

storablePokeByte :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storablePokeByte _ = property $ \(Positive len) (x :: a) off' -> ioProperty $ do
  let off = (off' `mod` len) * sizeOf (undefined :: a)
  addr :: Ptr a <- arrayArbitrary len
  pokeByteOff addr off x
  u :: a <- peekByteOff addr off
  poke (addr `plusPtr` off) x
  v :: a <- peekByteOff addr off
  free addr
  return (u ==== v)

storableSetGet :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storableSetGet _ = property $ \(a :: a) (Positive len) ix' -> ioProperty $ do
  let ix = ix' `mod` len
  ptr <- arrayArbitrary len
  pokeElemOff ptr ix a
  a' <- peekElemOff ptr ix
  free ptr
  return (a ==== a')

storableGetSet :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storableGetSet _ = property $ \(NonEmpty (as :: [a])) ix' -> ioProperty $ do
  let len = length as
      ix = ix' `mod` len
  ptrA <- newArray as
  ptrB <- arrayArbitrary len
  copyArray ptrB ptrA len
  a <- peekElemOff ptrA ix
  pokeElemOff ptrA ix a

  arrA <- peekArray len ptrA
  arrB <- peekArray len ptrB
  free ptrA
  free ptrB
  return $ conjoin $ zipWith (===) arrA arrB

storableSetSet :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storableSetSet _ = property $ \(x :: a) (y :: a) (Positive len) ix' -> ioProperty $ do
  let ix = ix' `mod` len
  ptr <- arrayArbitrary len
  pokeElemOff ptr ix x
  pokeElemOff ptr ix y
  atIx <- peekElemOff ptr ix
  free ptr
  return $ atIx ==== y

storableList :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storableList _ = property $ \(as :: [a]) -> ioProperty $ do
  let len = length as
  ptr <- newArray as
  let rebuild !ix = if ix < len
        then (:) <$> peekElemOff ptr ix <*> rebuild (ix + 1)
        else return []
  asNew <- rebuild 0
  free ptr
  return (as ==== asNew)

(====) :: (Eq a, Show a) => a -> a -> Property
x ==== y
  | x /= x && y /= y = discard
  | otherwise = x === y
