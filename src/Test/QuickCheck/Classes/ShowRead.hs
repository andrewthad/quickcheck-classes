{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.ShowRead
  ( showReadLaws
  ) where

import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

import Test.QuickCheck.Classes.Common (Laws(..))
import Test.QuickCheck.Classes.Compat (readMaybe)

-- | Tests the following properties:
--
-- [/Partial Isomorphism/]
--   @'readMaybe' ('show' a) == 'Just' a@
--
-- /Note:/ When using @base-4.5@ or older, a shim implementation
-- of 'readMaybe' is used.
--
showReadLaws :: (Show a, Read a, Eq a, Arbitrary a) => Proxy a -> Laws
showReadLaws p = Laws "Show/Read"
  [ ("Partial Isomorphism", showReadPartialIsomorphism p)
  ]

showReadPartialIsomorphism :: forall a. (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showReadPartialIsomorphism _ = property $ \(a :: a) ->
  readMaybe (show a) == Just a

