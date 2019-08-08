{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Json
  (
#if HAVE_AESON
    jsonLaws
#endif  
  ) where

import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property(..))

#if HAVE_AESON
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as AE
#endif

import Test.QuickCheck.Classes.Internal (Laws(..))

-- | Tests the following properties:
--
-- [/Partial Isomorphism/]
--   @decode . encode ≡ Just@
-- [/Encoding Equals Value/]
--   @decode . encode ≡ Just . toJSON@
--
-- Note that in the second property, the type of decode is @ByteString -> Value@,
-- not @ByteString -> a@
#if HAVE_AESON
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
jsonEncodingPartialIsomorphism _ =
#if MIN_VERSION_QuickCheck(2,9,0)
  again $
#endif
  MkProperty $
    arbitrary >>= \(x :: a) ->
      unProperty $
      shrinking shrink x $ \x' ->
        let desc1 = "Just"
            desc2 = "Data.Aeson.decode . Data.Aeson.encode"
            name1 = "Data.Aeson.encode a"
            name2 = "Data.Aeson.decode (Data.Aeson.encode a)"
            b1  = AE.encode x'
            b2  = AE.decode (AE.encode x')
            sb1 = show b1
            sb2 = show b2
            description = "  Description: " ++ desc1 ++ " == " ++ desc2
            err = description ++ "\n" ++ unlines (map ("  " ++) (["a = " ++ show x'])) ++ "  " ++ name1 ++ " = " ++ sb1 ++ "\n  " ++ name2 ++ " = " ++ sb2
        in counterexample err (Just x' == b2)
#endif
