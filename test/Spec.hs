{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Functor.Classes
import Data.Aeson (ToJSON,FromJSON)
import Data.Vector (Vector)

import qualified Data.Vector as V

import Test.QuickCheck.Classes

main :: IO ()
main = lawsCheckMany allPropsApplied

allPropsApplied :: [(String,[Laws])]
allPropsApplied = 
  [ ("Int",allLaws (Proxy :: Proxy Int))
  , ("Int64",allLaws (Proxy :: Proxy Int64))
  , ("Word",allLaws (Proxy :: Proxy Word))
#if MIN_VERSION_QuickCheck(2,10,0)
  , ("Maybe",allHigherLaws (Proxy :: Proxy Maybe))
  , ("List",allHigherLaws (Proxy :: Proxy []))
#endif
  , ("Vector",[isListLaws (Proxy :: Proxy (Vector Word))])
  ]

allLaws :: forall a. (Integral a, Prim a, Storable a, Ord a, Arbitrary a, Show a, Read a, ToJSON a, FromJSON a) => Proxy a -> [Laws]
allLaws p = 
  [ primLaws p
  , storableLaws p
  , monoidLaws (Proxy :: Proxy (Sum a))
  , showReadLaws p
  , jsonLaws p
  , eqLaws p
  , ordLaws p
  , integralLaws p
  ]

foldlMapM :: (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldlMapM f = foldlM (\b a -> fmap (mappend b) (f a)) mempty

#if MIN_VERSION_QuickCheck(2,10,0)
allHigherLaws :: (Foldable f, Monad f, Eq1 f, Arbitrary1 f, Show1 f) => Proxy f -> [Laws]
allHigherLaws p = 
  [ functorLaws p
  , applicativeLaws p
  , monadLaws p
  , foldableLaws p
  ]
#endif

-- This type is fails the laws for the strict functions
-- in Foldable. It is used just to confirm that
-- those property tests actually work.
newtype Rouge a = Rouge [a]
  deriving (Eq,Show,Arbitrary,Arbitrary1,Eq1,Show1)

instance Foldable Rouge where
  foldMap f (Rouge xs) = foldMap f xs
  foldl f x (Rouge xs) = foldl f x xs
  foldl' f x (Rouge xs) = foldl f x xs
  foldr' f x (Rouge xs) = foldr f x xs

-------------------
-- Orphan Instances
-------------------

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = V.fromList <$> arbitrary
  shrink v = map V.fromList (shrink (V.toList v))

