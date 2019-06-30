{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}

{-# OPTIONS_GHC -Wall #-}

{-| This library provides sets of properties that should hold for common
    typeclasses.

    /Note:/ on GHC < 8.6, this library uses the higher-kinded typeclasses
    ('Data.Functor.Classes.Show1', 'Data.Functor.Classes.Eq1', 'Data.Functor.Classes.Ord1', etc.),
    but on GHC >= 8.6, it uses @-XQuantifiedConstraints@ to express these
    constraints more cleanly.
-}
module Test.QuickCheck.Classes
  ( -- * Running
    lawsCheck
  , lawsCheckMany
  , lawsCheckOne
    -- * Properties
    -- ** Ground types
#if MIN_VERSION_base(4,7,0)
  , bitsLaws
#endif
  , eqLaws
  , numLaws
  , integralLaws
  , ixLaws
#if MIN_VERSION_base(4,7,0)
  , isListLaws
#endif
#if HAVE_AESON
  , jsonLaws
#endif
  , monoidLaws
  , commutativeMonoidLaws
  , semigroupMonoidLaws
  , ordLaws
  , enumLaws
  , boundedEnumLaws
  , primLaws
  , semigroupLaws
  , commutativeSemigroupLaws
  , exponentialSemigroupLaws
  , idempotentSemigroupLaws
  , rectangularBandSemigroupLaws
#if HAVE_SEMIRINGS
  , semiringLaws
  , ringLaws
  , gcdDomainLaws
  , euclideanLaws
#endif
  , showLaws
  , showReadLaws
  , storableLaws
#if MIN_VERSION_base(4,5,0)
  , genericLaws
  , generic1Laws
#endif
#if HAVE_UNARY_LAWS
    -- ** Unary type constructors
  , alternativeLaws
#if HAVE_SEMIGROUPOIDS
  , altLaws
  , applyLaws
#endif
  , applicativeLaws
  , contravariantLaws
  , foldableLaws
  , functorLaws
  , monadLaws
  , monadPlusLaws
  , monadZipLaws
#if HAVE_SEMIGROUPOIDS
  , plusLaws
  , extendedPlusLaws
#endif
  , traversableLaws
#endif
#if HAVE_BINARY_LAWS
    -- ** Binary type constructors
  , bifoldableLaws
  , bifunctorLaws
  , bitraversableLaws 
  , categoryLaws
  , commutativeCategoryLaws
#if HAVE_SEMIGROUPOIDS
  , semigroupoidLaws
  , commutativeSemigroupoidLaws
#endif
#if HAVE_VECTOR
  , muvectorLaws
#endif
#endif
    -- * Types
  , Laws(..)
  , Proxy1(..)
  , Proxy2(..)
  ) where

--
-- re-exports
--

-- Ground Types
import Test.QuickCheck.Classes.Bits
import Test.QuickCheck.Classes.Enum
import Test.QuickCheck.Classes.Eq
import Test.QuickCheck.Classes.Num
import Test.QuickCheck.Classes.Integral
import Test.QuickCheck.Classes.Ix
#if MIN_VERSION_base(4,7,0)
import Test.QuickCheck.Classes.IsList
#endif
#if HAVE_AESON
import Test.QuickCheck.Classes.Json
#endif
import Test.QuickCheck.Classes.Monoid
import Test.QuickCheck.Classes.Ord
import Test.QuickCheck.Classes.Prim
import Test.QuickCheck.Classes.Semigroup
#if HAVE_SEMIRINGS
import Test.QuickCheck.Classes.Euclidean
import Test.QuickCheck.Classes.Semiring
import Test.QuickCheck.Classes.Ring
#endif
import Test.QuickCheck.Classes.Show
import Test.QuickCheck.Classes.ShowRead
import Test.QuickCheck.Classes.Storable
#if MIN_VERSION_base(4,5,0)
import Test.QuickCheck.Classes.Generic
#endif
-- Unary type constructors
#if HAVE_UNARY_LAWS
import Test.QuickCheck.Classes.Alternative
#if HAVE_SEMIGROUPOIDS
import Test.QuickCheck.Classes.Alt
import Test.QuickCheck.Classes.Apply
#endif
import Test.QuickCheck.Classes.Applicative
import Test.QuickCheck.Classes.Contravariant
import Test.QuickCheck.Classes.Foldable
import Test.QuickCheck.Classes.Functor
import Test.QuickCheck.Classes.Monad
import Test.QuickCheck.Classes.MonadPlus
import Test.QuickCheck.Classes.MonadZip
#if HAVE_SEMIGROUPOIDS
import Test.QuickCheck.Classes.Plus
#endif
import Test.QuickCheck.Classes.Traversable
#endif

-- Binary type constructors
#if HAVE_BINARY_LAWS
import Test.QuickCheck.Classes.Bifunctor
import Test.QuickCheck.Classes.Bifoldable
import Test.QuickCheck.Classes.Bitraversable
import Test.QuickCheck.Classes.Category
#if HAVE_SEMIGROUPOIDS
import Test.QuickCheck.Classes.Semigroupoid
#endif
#endif

#if HAVE_VECTOR
import Test.QuickCheck.Classes.MVector
#endif

--
-- used below
--
import Test.QuickCheck
import Test.QuickCheck.Classes.Common (foldMapA, Laws(..))
import Control.Monad
import Data.Foldable
import Data.Monoid (Monoid(..))
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup)
import System.Exit (exitFailure)
import qualified Data.List as List
import qualified Data.Semigroup as SG

-- | A convenience function for testing properties in GHCi.
-- For example, at GHCi:
--
-- >>> lawsCheck (monoidLaws (Proxy :: Proxy Ordering))
-- Monoid: Associative +++ OK, passed 100 tests.
-- Monoid: Left Identity +++ OK, passed 100 tests.
-- Monoid: Right Identity +++ OK, passed 100 tests.
--
-- Assuming that the 'Arbitrary' instance for 'Ordering' is good, we now
-- have confidence that the 'Monoid' instance for 'Ordering' satisfies
-- the monoid laws.
lawsCheck :: Laws -> IO ()
lawsCheck (Laws className properties) = do
  flip foldMapA properties $ \(name,p) -> do
    putStr (className ++ ": " ++ name ++ " ")
    quickCheck p

-- | A convenience function that allows one to check many typeclass
-- instances of the same type.
--
-- >>> specialisedLawsCheckMany (Proxy :: Proxy Word) [jsonLaws, showReadLaws]
-- ToJSON/FromJSON: Encoding Equals Value +++ OK, passed 100 tests.
-- ToJSON/FromJSON: Partial Isomorphism +++ OK, passed 100 tests.
-- Show/Read: Partial Isomorphism +++ OK, passed 100 tests.
lawsCheckOne :: Proxy a -> [Proxy a -> Laws] -> IO ()
lawsCheckOne p ls = foldlMapM (lawsCheck . ($ p)) ls

-- | A convenience function for checking multiple typeclass instances
--   of multiple types. Consider the following Haskell source file:
--
-- @
-- import Data.Proxy (Proxy(..))
-- import Data.Map (Map)
-- import Data.Set (Set)
--
-- -- A 'Proxy' for 'Set' 'Int'.
-- setInt :: Proxy (Set Int)
-- setInt = Proxy
--
-- -- A 'Proxy' for 'Map' 'Int' 'Int'.
-- mapInt :: Proxy (Map Int Int)
-- mapInt = Proxy
--
-- myLaws :: Proxy a -> [Laws]
-- myLaws p = [eqLaws p, monoidLaws p]
--
-- namedTests :: [(String, [Laws])]
-- namedTests =
--   [ ("Set Int", myLaws setInt)
--   , ("Map Int Int", myLaws mapInt)
--   ]
-- @
--
-- Now, in GHCi:
--
-- >>> lawsCheckMany namedTests
--
-- @
-- Testing properties for common typeclasses
-- -------------
-- -- Set Int --
-- -------------
--
-- Eq: Transitive +++ OK, passed 100 tests.
-- Eq: Symmetric +++ OK, passed 100 tests.
-- Eq: Reflexive +++ OK, passed 100 tests.
-- Monoid: Associative +++ OK, passed 100 tests.
-- Monoid: Left Identity +++ OK, passed 100 tests.
-- Monoid: Right Identity +++ OK, passed 100 tests.
-- Monoid: Concatenation +++ OK, passed 100 tests.
--
-- -----------------
-- -- Map Int Int --
-- -----------------
--
-- Eq: Transitive +++ OK, passed 100 tests.
-- Eq: Symmetric +++ OK, passed 100 tests.
-- Eq: Reflexive +++ OK, passed 100 tests.
-- Monoid: Associative +++ OK, passed 100 tests.
-- Monoid: Left Identity +++ OK, passed 100 tests.
-- Monoid: Right Identity +++ OK, passed 100 tests.
-- Monoid: Concatenation +++ OK, passed 100 tests.
-- @
--
-- In the case of a failing test, the program terminates with
-- exit code 1.
lawsCheckMany ::
     [(String,[Laws])] -- ^ Element is type name paired with typeclass laws
  -> IO ()
lawsCheckMany xs = do
  putStrLn "Testing properties for common typeclasses"
  r <- flip foldMapA xs $ \(typeName,laws) -> do
    putStrLn $ List.replicate (length typeName + 6) '-'
    putStrLn $ "-- " ++ typeName ++ " --"
    putStrLn $ List.replicate (length typeName + 6) '-'
    flip foldMapA laws $ \(Laws typeClassName properties) -> do
      flip foldMapA properties $ \(name,p) -> do
        putStr (typeClassName ++ ": " ++ name ++ " ")
        r <- quickCheckResult p
        return $ case r of
          Success{} -> Good
          _ -> Bad
  putStrLn ""
  case r of
    Good -> putStrLn "All tests succeeded"
    Bad -> do
      putStrLn "One or more tests failed"
      exitFailure

data Status = Bad | Good

instance Semigroup Status where
  Good <> x = x
  Bad <> _ = Bad

instance Monoid Status where
  mempty = Good
  mappend = (SG.<>)

-- | In older versions of GHC, Proxy is not poly-kinded,
--   so we provide Proxy1.
data Proxy1 (f :: * -> *) = Proxy1

-- | In older versions of GHC, Proxy is not poly-kinded,
--   so we provide Proxy2.
data Proxy2 (f :: * -> * -> *) = Proxy2

-- This is used internally to work around a missing Monoid
-- instance for IO on older GHCs.
foldlMapM :: (Foldable t, Monoid b, Monad m) => (a -> m b) -> t a -> m b
foldlMapM f = foldlM (\b a -> liftM (mappend b) (f a)) mempty
