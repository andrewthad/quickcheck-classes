{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Json
  (
#if defined(VERSION_aeson)
    jsonLaws
#endif  
  ) where

import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property)

#if defined(VERSION_aeson)
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as AE
#endif

import Test.QuickCheck.Classes.Common.Ground (Laws(..))

-- | Tests the following properties:
--
-- [/Partial Isomorphism/]
--   @decode . encode ≡ Just@
-- [/Encoding Equals Value/]
--   @decode . encode ≡ Just . toJSON@
--
-- Note that in the second property, the type of decode is @ByteString -> Value@,
-- not @ByteString -> a@
#if defined(VERSION_aeson)
jsonLaws :: (ToJSON a, FromJSON a, Show a, Arbitrary a, Eq a) => Proxy a -> Laws
jsonLaws p = Laws "ToJSON/FromJSON"
  [ ("Partial Isomorphism", jsonEncodingPartialIsomorphism p)
  , ("Encoding Equals Value", jsonEncodingEqualsValue p)
  ]

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

#endif
