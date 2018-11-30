{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Json
  (
#if HAVE_AESON
    jsonLaws
#endif  
  ) where

import Data.String (fromString)
import Data.Proxy (Proxy)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property(..))

#if HAVE_AESON
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as AE
#endif

import Test.QuickCheck.Classes.Common (Laws(..), myForAllShrink)

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
        let name1 = "Just a"
            name2 = "AE.decode (AE.encode a)"
            b1  = AE.encode x'
            b2  = AE.decode (AE.encode x')
            sb1 = show b1
            sb2 = show b2
            description = "  Description: " ++ name1 ++ " = " ++ name2
            err = description ++ "\n" ++ unlines (map ("  " ++) (["a = " ++ show x'])) ++ "  " ++ name1 ++ " = " ++ sb1 ++ "\n  " ++ name2 ++ " = " ++ sb2
        in counterexample err (Just x' == b2)
{-
myForAllShrink True (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "encode a"
  (\(a :: a) -> Just $ AE.encode a)
  "decode (encode a)"
  (\(a :: a) -> (fromString . show) <$> (AE.decode (AE.encode a) :: Maybe a)) --AE.decode (AE.encode a))
-}

#endif
