{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.ShowRead
  ( showReadLaws
  ) where

import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

#if MIN_VERSION_base(4,6,0)
import Text.Read (readMaybe)
#endif

import Test.QuickCheck.Classes.Common (Laws(..))

showReadLaws :: (Show a, Read a, Eq a, Arbitrary a) => Proxy a -> Laws
showReadLaws p = Laws "Show/Read"
  [ ("Partial Isomorphism", showReadPartialIsomorphism p)
  ]

showReadPartialIsomorphism :: forall a. (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showReadPartialIsomorphism _ = property $ \(a :: a) ->
#if MIN_VERSION_base(4,6,0)
  readMaybe (show a) == Just a
#else
  read (show a) == a
#endif

