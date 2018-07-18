{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Monad
import Control.Monad.Zip (MonadZip)
import Control.Applicative
#if defined(VERSION_aeson)
import Data.Aeson (ToJSON,FromJSON)
#endif
import Data.Bits
import Data.Foldable
#if defined(VERSION_containers)
import Data.Map (Map)
#endif
#if MIN_VERSION_containers(0,5,9)
import qualified Data.Map.Merge.Strict as MM
#endif
import Data.Traversable
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
#if defined(VERSION_semigroupoids)
import Data.Functor.Apply (Apply((<.>)))
#endif
#endif
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes
#endif
import Data.Int
import Data.Monoid (Sum,Monoid,mappend,mconcat,mempty)
import Data.Primitive
import Data.Proxy
import Data.Vector (Vector)
import Data.Word
import Foreign.Storable
import Test.QuickCheck
import Text.Show.Functions

import qualified Data.Vector as V
import qualified Data.Foldable as F

import Test.QuickCheck.Classes

main :: IO ()
main = do
  quickCheck prop_map_apply_equals
  lawsCheckMany allPropsApplied

allPropsApplied :: [(String,[Laws])]
allPropsApplied = 
  [ ("Int",allLaws (Proxy :: Proxy Int))
  , ("Int64",allLaws (Proxy :: Proxy Int64))
  , ("Word",allLaws (Proxy :: Proxy Word))
#if MIN_VERSION_QuickCheck(2,10,0)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
  , ("Maybe",allHigherLaws (Proxy1 :: Proxy1 Maybe))
  , ("List",allHigherLaws (Proxy1 :: Proxy1 []))
#endif
#endif
#if MIN_VERSION_QuickCheck(2,10,0)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
#if defined(VERSION_semigroupoids)
  , ("Map", someHigherLaws (Proxy1 :: Proxy1 (Map Int)))
  , ("Pound", someHigherLaws (Proxy1 :: Proxy1 (Pound Int)))
#endif
#endif
#endif
#if MIN_VERSION_base(4,7,0)
  , ("Vector",[isListLaws (Proxy :: Proxy (Vector Word))])
#endif
  ]

allLaws :: forall a.
  ( Integral a
  , Prim a
  , Storable a
  , Ord a
  , Arbitrary a
  , Show a
  , Read a
  , Enum a
  , Bounded a
#if defined(VERSION_aeson)
  , ToJSON a
  , FromJSON a
#endif
#if MIN_VERSION_base(4,7,0)
  , FiniteBits a
#endif
  ) => Proxy a -> [Laws]
allLaws p = 
  [ primLaws p
  , storableLaws p
  , semigroupLaws (Proxy :: Proxy (Sum a))
  , monoidLaws (Proxy :: Proxy (Sum a))
  , showReadLaws p
  , boundedEnumLaws p
#if defined(VERSION_aeson)
  , jsonLaws p
#endif
  , eqLaws p
  , ordLaws p
  , integralLaws p
#if MIN_VERSION_base(4,7,0)
  , bitsLaws p
#endif
  ]

foldlMapM :: (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldlMapM f = foldlM (\b a -> liftM (mappend b) (f a)) mempty

#if MIN_VERSION_QuickCheck(2,10,0)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
allHigherLaws :: (Traversable f, MonadZip f, MonadPlus f, Applicative f, Eq1 f, Arbitrary1 f, Show1 f) => proxy f -> [Laws]
allHigherLaws p = 
  [ functorLaws p
  , applicativeLaws p
  , monadLaws p
  , monadPlusLaws p
  , monadZipLaws p
  , foldableLaws p
  , traversableLaws p
  ]
#endif
#endif

#if MIN_VERSION_QuickCheck(2,10,0)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
#if defined(VERSION_semigroupoids)
someHigherLaws :: (Apply f, Eq1 f, Arbitrary1 f, Show1 f) => proxy f -> [Laws]
someHigherLaws p = 
  [ applyLaws p
  ]
#endif
#endif
#endif

-- This type fails the laws for the strict functions
-- in Foldable. It is used just to confirm that
-- those property tests actually work.
newtype Rouge a = Rouge [a]
#if MIN_VERSION_QuickCheck(2,10,0) && (MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0))
  deriving (Eq,Show,Arbitrary,Arbitrary1,Eq1,Show1)
#else
  deriving (Eq,Show,Arbitrary)
#endif

-- Note: when using base < 4.6, the Rouge type does
-- not really test anything. 
instance Foldable Rouge where
  foldMap f (Rouge xs) = F.foldMap f xs
  foldl f x (Rouge xs) = F.foldl f x xs
#if MIN_VERSION_base(4,6,0)
  foldl' f x (Rouge xs) = F.foldl f x xs
  foldr' f x (Rouge xs) = F.foldr f x xs
#endif

newtype Pound k v = Pound { getPound :: Map k v }
#if MIN_VERSION_QuickCheck(2,10,0) && (MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0))
  deriving (Eq,Functor,Show,Arbitrary,Arbitrary1,Eq1,Show1)
#else
  deriving (Eq,Show,Arbitrary)
#endif

#if defined(VERSION_semigroupoids)
#if MIN_VERSION_containers(0,5,9)
instance Ord k => Apply (Pound k) where
  Pound m1 <.> Pound m2 = Pound $
    MM.merge
      MM.dropMissing
      MM.dropMissing
      (MM.zipWithMatched (\_ f a -> f a))
      m1
      m2
#endif
#endif

#if defined(VERSION_semigroupoids)
#if MIN_VERSION_containers(0,5,9)
prop_map_apply_equals :: Map Int (Int -> Int)
                      -> Map Int Int
                      -> Bool
prop_map_apply_equals mf ma =
  let pf = Pound mf
      pa = Pound ma
      m = mf <.> ma
      p = pf <.> pa
  in m == (getPound p)
#endif
#endif

-------------------
-- Orphan Instances
-------------------

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = V.fromList <$> arbitrary
  shrink v = map V.fromList (shrink (V.toList v))

