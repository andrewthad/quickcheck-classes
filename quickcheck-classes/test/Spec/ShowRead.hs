{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

module Spec.ShowRead where

import Control.Applicative (liftA2)
import Data.Complex (Complex)
import Data.Fixed (E0, E1, E12, Fixed, HasResolution)
import Data.Int (Int64, Int8)
import Data.Orphans ()
import Data.Proxy (Proxy(Proxy))
import Data.Ratio (Ratio)
import Data.Word
import Test.QuickCheck (Arbitrary(arbitrary), elements)
#if MIN_VERSION_QuickCheck(2,8,2)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
#endif
#if MIN_VERSION_QuickCheck(2,9,0)
import Control.Applicative (Const, ZipList)
import Data.Functor.Constant (Constant)
import Data.Functor.Identity (Identity)
import Data.Version (Version)
#endif
#if MIN_VERSION_QuickCheck(2,10,0)
import Data.Functor.Compose (Compose)
import Data.Functor.Product (Product)
#endif

import Test.QuickCheck.Classes

data Prefix = Prefix | Prefix' | Prefix_
  deriving (Eq, Read, Show)

instance Arbitrary Prefix where
  arbitrary = elements [Prefix, Prefix', Prefix_]

data WeirdRecord = (:*) { left :: Int, right :: Int }
  deriving (Eq, Read, Show)

instance Arbitrary WeirdRecord where
  arbitrary = liftA2 (:*) arbitrary arbitrary

lawsApplied :: [(String,[Laws])]
lawsApplied =
  [ -- local
    ("Prefix",         allShowReadLaws (Proxy :: Proxy Prefix))
  , ("WeirdRecord",    allShowReadLaws (Proxy :: Proxy WeirdRecord))

    -- base
  , ("()",             allShowReadLaws (Proxy :: Proxy ()))
  , ("Bool",           allShowReadLaws (Proxy :: Proxy Bool))
  , ("Char",           allShowReadLaws (Proxy :: Proxy Char))
  , ("Complex Float",  allShowReadLaws (Proxy :: Proxy (Complex Float)))
  , ("Complex Double", allShowReadLaws (Proxy :: Proxy (Complex Double)))
  , ("Double",         allShowReadLaws (Proxy :: Proxy Double))
  , ("Either",         allShowReadLaws (Proxy :: Proxy (Either Int Int)))
  , ("Fixed E12",      allFixedLaws (Proxy :: Proxy (Fixed E12)))
  -- , ("Fixed E9",       allFixedLaws (Proxy :: Proxy (Fixed E9)))
  -- , ("Fixed E6",       allFixedLaws (Proxy :: Proxy (Fixed E6)))
  -- , ("Fixed E3",       allFixedLaws (Proxy :: Proxy (Fixed E3)))
  -- , ("Fixed E2",       allFixedLaws (Proxy :: Proxy (Fixed E2)))
  , ("Fixed E1",       allFixedLaws (Proxy :: Proxy (Fixed E1)))
  , ("Fixed E0",       allFixedLaws (Proxy :: Proxy (Fixed E0)))
  , ("Float",          allShowReadLaws (Proxy :: Proxy Float))
  , ("Int",            allShowReadLaws (Proxy :: Proxy Int))
  -- , ("Int16",          allShowReadLaws (Proxy :: Proxy Int16))
  -- , ("Int32",          allShowReadLaws (Proxy :: Proxy Int32))
  , ("Int64",          allShowReadLaws (Proxy :: Proxy Int64))
  , ("Int8",           allShowReadLaws (Proxy :: Proxy Int8))
  , ("Integer",        allShowReadLaws (Proxy :: Proxy Integer))
  , ("List",           allShowReadLaws (Proxy :: Proxy [Int]))
  , ("Maybe",          allShowReadLaws (Proxy :: Proxy (Maybe Int)))
  , ("Ordering",       allShowReadLaws (Proxy :: Proxy Ordering))
  , ("Ratio",          allShowReadLaws (Proxy :: Proxy (Ratio Int)))
  , ("Tuple2",         allShowReadLaws (Proxy :: Proxy (Int,Int)))
  , ("Tuple3",         allShowReadLaws (Proxy :: Proxy (Int,Int,Int)))
  , ("Word",           allShowReadLaws (Proxy :: Proxy Word))
  -- , ("Word16",         allShowReadLaws (Proxy :: Proxy Word16))
  -- , ("Word32",         allShowReadLaws (Proxy :: Proxy Word32))
  , ("Word64",         allShowReadLaws (Proxy :: Proxy Word64))
  , ("Word8",          allShowReadLaws (Proxy :: Proxy Word8))
#if MIN_VERSION_QuickCheck(2,9,0)
  , ("Const",          allShowReadLaws (Proxy :: Proxy (Const Int Int)))
  , ("Constant",       allShowReadLaws (Proxy :: Proxy (Constant Int Int)))
  , ("Identity",       allShowReadLaws (Proxy :: Proxy (Identity Int)))
  , ("Version",        allShowReadLaws (Proxy :: Proxy Version))
  , ("ZipList",        allShowReadLaws (Proxy :: Proxy (ZipList Int)))
#endif
#if MIN_VERSION_QuickCheck(2,10,0)
  , ("Compose",        allShowReadLaws (Proxy :: Proxy (Compose [] Maybe Int)))
  , ("Product",        allShowReadLaws (Proxy :: Proxy (Product [] Maybe Int)))
#endif

  -- containers
#if MIN_VERSION_QuickCheck(2,8,2)
  , ("IntMap",         allShowReadLaws (Proxy :: Proxy (IntMap Int)))
  , ("IntSet",         allShowReadLaws (Proxy :: Proxy IntSet))
  , ("Map",            allShowReadLaws (Proxy :: Proxy (Map Int Int)))
  , ("Seq",            allShowReadLaws (Proxy :: Proxy (Seq Int)))
  , ("Set",            allShowReadLaws (Proxy :: Proxy (Set Int)))
#endif
  ]

allShowReadLaws :: (Show a, Read a, Eq a, Arbitrary a) => Proxy a -> [Laws]
allShowReadLaws p = map ($p)
  [ showLaws
  , showReadLaws
  ]

allFixedLaws :: HasResolution e => Proxy (Fixed e) -> [Laws]
allFixedLaws p = map ($p)
  [ showLaws
#if MIN_VERSION_base(4,7,0)
  -- Earlier versions of base have a buggy read instance.
  , showReadLaws
#endif
  ]
