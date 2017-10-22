{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.QuickCheck
import Data.Proxy
import Data.Word
import Data.Int
import Control.Monad
import Data.Primitive
import Data.Foldable
import Data.Monoid (Sum)
import Foreign.Storable
import Data.Aeson (ToJSON,FromJSON)

import Test.QuickCheck.Classes

main :: IO ()
main = do
  putStrLn "Testing properties for common typeclasses"
  r <- flip foldlMapM allPropsApplied $ \(typeName,properties) -> do
    putStrLn $ "------------"
    putStrLn $ "-- " ++ typeName
    putStrLn $ "------------"
    flip foldlMapM properties $ \(name,p) -> do
      putStrLn name
      r <- quickCheckResult p
      return $ case r of
        Success _ _ _ -> Good
        _ -> Bad
  putStrLn ""
  case r of
    Good -> putStrLn "All tests succeeded"
    Bad -> putStrLn "One or more tests failed"

data Status = Bad | Good

instance Monoid Status where
  mempty = Good
  mappend Good x = x
  mappend Bad _ = Bad

allPropsApplied :: [(String,[(String,Property)])]
allPropsApplied = 
  [ ("Word8",allProps (Proxy :: Proxy Word8))
  , ("Int16",allProps (Proxy :: Proxy Int16))
  , ("Int",allProps (Proxy :: Proxy Int))
  , ("Word",allProps (Proxy :: Proxy Word))
  , ("Int64",allProps (Proxy :: Proxy Int64))
  ]

allProps :: forall a. (Num a, Prim a, Storable a, Eq a, Arbitrary a, Show a, Read a, ToJSON a, FromJSON a) => Proxy a -> [(String,Property)]
allProps p = concat
  [ primProps p
  , storableProps p
  , monoidProps (Proxy :: Proxy (Sum a))
  , showReadProps p
  , jsonProps p
  ]

foldlMapM :: (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldlMapM f = foldlM (\b a -> fmap (mappend b) (f a)) mempty

