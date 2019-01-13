{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

import Control.Monad
import Control.Monad.Zip (MonadZip)
import Control.Applicative
#if defined(VERSION_aeson)
import Data.Aeson (ToJSON,FromJSON)
#endif
import Data.Bits
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
#if MIN_VERSION_containers(0,5,9)
import qualified Data.Map.Merge.Strict as MM
#endif
import Data.Traversable
#if HAVE_SEMIGROUPOIDS
import Data.Functor.Apply (Apply((<.>)))
#endif
#if HAVE_UNARY_LAWS
import Data.Functor.Classes
#endif
import Data.Int
import Data.Monoid (Sum(..),Monoid,mappend,mconcat,mempty)
import Data.Orphans ()
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
import qualified Spec.ShowRead

main :: IO ()
main = do
#if HAVE_SEMIGROUPOIDS
#if MIN_VERSION_containers(0,5,9)
  quickCheck prop_map_apply_equals
#endif
#endif
  lawsCheckMany allPropsApplied

allPropsApplied :: [(String,[Laws])]
allPropsApplied = M.toList . M.fromListWith (++) $
  [ ("Int",allLaws (Proxy :: Proxy Int))
  , ("Int64",allLaws (Proxy :: Proxy Int64))
  , ("Word",allLaws (Proxy :: Proxy Word))
#if HAVE_UNARY_LAWS
  , ("Maybe",allHigherLaws (Proxy1 :: Proxy1 Maybe))
  , ("List",allHigherLaws (Proxy1 :: Proxy1 []))
#endif
#if defined(HAVE_SEMIGROUPOIDS) && defined(HAVE_UNARY_LAWS)
#if MIN_VERSION_base(4,9,0) && MIN_VERSION_containers(0,5,9)
  , ("Map", someHigherLaws (Proxy1 :: Proxy1 (Map Int)))
  , ("Pound", someHigherLaws (Proxy1 :: Proxy1 (Pound Int)))
#endif
#endif
#if MIN_VERSION_base(4,7,0)
  , ("Vector",
    [ isListLaws (Proxy :: Proxy (Vector Word))
#if HAVE_VECTOR
    , muvectorLaws (Proxy :: Proxy Word8)
    , muvectorLaws (Proxy :: Proxy (Int, Word))
#endif
    ])
#endif
  ]
  ++ Spec.ShowRead.lawsApplied

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

#if HAVE_UNARY_LAWS
allHigherLaws ::
  (Traversable f, MonadZip f, MonadPlus f, Applicative f,
#if HAVE_QUANTIFIED_CONSTRAINTS
   forall a. Eq a => Eq (f a), forall a. Arbitrary a => Arbitrary (f a),
   forall a. Show a => Show (f a)
#else
   Eq1 f, Arbitrary1 f, Show1 f
#endif
  ) => proxy f -> [Laws]
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

#if defined(HAVE_SEMIGROUPOIDS) && defined(HAVE_UNARY_LAWS)
someHigherLaws ::
  (Apply f,
#if HAVE_QUANTIFIED_CONSTRAINTS
   forall a. Eq a => Eq (f a), forall a. Arbitrary a => Arbitrary (f a),
   forall a. Show a => Show (f a)
#else
   Eq1 f, Arbitrary1 f, Show1 f
#endif
  ) => proxy f -> [Laws]
someHigherLaws p =
  [ applyLaws p
  ]
#endif

-- This type fails the laws for the strict functions
-- in Foldable. It is used just to confirm that
-- those property tests actually work.
newtype Rogue a = Rogue [a]
  deriving
  ( Eq, Show, Arbitrary
#if HAVE_UNARY_LAWS
  , Arbitrary1
  , Eq1
  , Show1
#endif
  )

-- Note: when using base < 4.6, the Rogue type does
-- not really test anything. 
instance Foldable Rogue where
  foldMap f (Rogue xs) = F.foldMap f xs
  foldl f x (Rogue xs) = F.foldl f x xs
#if MIN_VERSION_base(4,6,0)
  foldl' f x (Rogue xs) = F.foldl f x xs
  foldr' f x (Rogue xs) = F.foldr f x xs
#endif

newtype Pound k v = Pound { getPound :: Map k v }
  deriving
  ( Eq, Functor, Show, Arbitrary
#if HAVE_UNARY_LAWS
  , Arbitrary1
  -- The following instances are only available for the variants
  -- of the type classes in base, not for those in transformers.
#if MIN_VERSION_base(4,9,0) && MIN_VERSION_containers(0,5,9)
  , Eq1
  , Show1
#endif
#endif
  )

#if HAVE_SEMIGROUPOIDS
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

#if HAVE_SEMIGROUPOIDS
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

#if !MIN_VERSION_QuickCheck(2,8,2)
instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
  arbitrary = M.fromList <$> arbitrary
  shrink m = map M.fromList (shrink (M.toList m))
#endif

#if !MIN_VERSION_QuickCheck(2,9,0)
instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary
  shrink = map Sum . shrink . getSum
#endif
