{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Ix
  ( ixLaws
  ) where

import Data.Ix (Ix(..))
import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Internal (Laws(..))

-- | Tests the various 'Ix' properties:
--
--   @'inRange' (l,u) i '==' 'elem' i ('range' (l,u))@
--
--   @'range' (l,u) '!!' 'index' (l,u) i '==' i@, when @'inRange' (l,u) i@
--
--   @'map' ('index' (l,u)) ('range' (l,u)) '==' [0 .. 'rangeSize' (l,u) - 1]@
--   
--   @'rangeSize' (l,u) '==' 'length' ('range' (l,u))@
ixLaws :: (Ix a, Arbitrary a, Show a) => Proxy a -> Laws
ixLaws p = Laws "Ix"
  [ ("InRange", ixInRange p)
  , ("RangeIndex", ixRangeIndex p)
  , ("MapIndexRange", ixMapIndexRange p)
  , ("RangeSize", ixRangeSize p)
  ]

ixInRange :: forall a. (Show a, Ix a, Arbitrary a) => Proxy a -> Property
ixInRange _ = property $ \(l :: a) (u :: a) (i :: a) -> (l <= u) ==> do
  inRange (l,u) i == elem i (range (l,u))

ixRangeIndex :: forall a. (Show a, Ix a, Arbitrary a) => Proxy a -> Property
ixRangeIndex _ = property $ \(l :: a) (u :: a) (i :: a) -> ((l <= u) && (i >= l && i <= u)) ==> do
  range (l,u) !! index (l,u) i == i

ixMapIndexRange :: forall a. (Show a, Ix a, Arbitrary a) => Proxy a -> Property
ixMapIndexRange _ = property $ \(l :: a) (u :: a) -> (l <= u) ==> do
  map (index (l,u)) (range (l,u)) == [0 .. rangeSize (l,u) - 1]

ixRangeSize :: forall a. (Show a, Ix a, Arbitrary a) => Proxy a -> Property
ixRangeSize _ = property $ \(l :: a) (u :: a) -> (l <= u) ==> do
  rangeSize (l,u) == length (range (l,u))


