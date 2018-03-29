{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wall #-}

{-|
This library provides sets of properties that should hold for common typeclasses.
All of these take a 'Proxy' argument that is used to nail down the type for which
the typeclass dictionaries should be tested. For example, at GHCi:
>>> lawsCheck (monoidLaws (Proxy :: Proxy Ordering))
Monoid: Associative +++ OK, passed 100 tests.
Monoid: Left Identity +++ OK, passed 100 tests.
Monoid: Right Identity +++ OK, passed 100 tests.
Assuming that the 'Arbitrary' instance for 'Ordering' is good, we now
have confidence that the 'Monoid' instance for 'Ordering' satisfies
the monoid laws. We can check multiple typeclasses with:
>>> foldMap (lawsCheck . ($ (Proxy :: Proxy Word))) [jsonLaws,showReadLaws]
ToJSON/FromJSON: Encoding Equals Value +++ OK, passed 100 tests.
ToJSON/FromJSON: Partial Isomorphism +++ OK, passed 100 tests.
Show/Read: Partial Isomorphism +++ OK, passed 100 tests.
-}
module Test.QuickCheck.Classes
  ( -- * Running 
    lawsCheck
  , lawsCheckMany
    -- * Properties
    -- ** Ground types
#if MIN_VERSION_base(4,7,0)
  , bitsLaws
#endif
  , commutativeMonoidLaws 
  , eqLaws
  , integralLaws
#if MIN_VERSION_base(4,7,0)
  , isListLaws
#endif
#if defined(VERSION_aeson)
  , jsonLaws
#endif
  , monoidLaws
  , ordLaws
  , primLaws
  , semigroupLaws
  , showReadLaws
  , storableLaws
#if MIN_VERSION_QuickCheck(2,10,0) && (MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0))
    -- ** Higher-Kinded Types
  , alternativeLaws
#if defined(VERSION_semigroupoids)
  , altLaws
#endif
  , applicativeLaws
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
  , bifunctorLaws 
#endif
  , foldableLaws
  , functorLaws
  , monadLaws
  , monadPlusLaws
  , monadZipLaws
  , traversableLaws
#endif
    -- * Types
  , Laws(..)
  ) where

--
-- re-exports
--

-- Ground Types
import Test.QuickCheck.Classes.Bits
import Test.QuickCheck.Classes.Eq
import Test.QuickCheck.Classes.Integral
#if MIN_VERSION_base(4,7,0)
import Test.QuickCheck.Classes.IsList
#endif
#if defined(VERSION_aeson)
import Test.QuickCheck.Classes.Json
#endif
import Test.QuickCheck.Classes.Monoid
import Test.QuickCheck.Classes.Ord
import Test.QuickCheck.Classes.Prim
import Test.QuickCheck.Classes.Semigroup
import Test.QuickCheck.Classes.ShowRead
import Test.QuickCheck.Classes.Storable

-- Higher-Kinded Types

#if MIN_VERSION_QuickCheck(2,10,0)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Test.QuickCheck.Classes.Alternative
#if defined(VERSION_semigroupoids)
import Test.QuickCheck.Classes.Alt
#endif
import Test.QuickCheck.Classes.Applicative
#if MIN_VERSION_transformers(0,5,0)
import Test.QuickCheck.Classes.Bifunctor
#endif
import Test.QuickCheck.Classes.Foldable
import Test.QuickCheck.Classes.Functor
import Test.QuickCheck.Classes.Monad
import Test.QuickCheck.Classes.MonadPlus
import Test.QuickCheck.Classes.MonadZip
import Test.QuickCheck.Classes.Traversable
#endif
#endif

-- used below
import Test.QuickCheck
import Test.QuickCheck.Classes.Common (foldMapA, Laws(..))
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as SG

-- | A convenience function for working testing properties in GHCi.
--   See the test suite of this library for an example of how to
--   integrate multiple properties into larger test suite.
lawsCheck :: Laws -> IO ()
lawsCheck (Laws className properties) = do
  flip foldMapA properties $ \(name,p) -> do
    putStr (className ++ ": " ++ name ++ " ")
    quickCheck p

-- | A convenience function for checking multiple typeclass instances
--   of multiple types.
lawsCheckMany ::
     [(String,[Laws])] -- ^ Element is type name paired with typeclass laws
  -> IO ()
lawsCheckMany xs = do
  putStrLn "Testing properties for common typeclasses"
  r <- flip foldMapA xs $ \(typeName,laws) -> do
    putStrLn $ "------------"
    putStrLn $ "-- " ++ typeName
    putStrLn $ "------------"
    flip foldMapA laws $ \(Laws typeClassName properties) -> do
      flip foldMapA properties $ \(name,p) -> do
        putStr (typeClassName ++ ": " ++ name ++ " ")
        r <- quickCheckResult p
        return $ case r of
          Success _ _ _ -> Good
          _ -> Bad
  putStrLn ""
  case r of
    Good -> putStrLn "All tests succeeded"
    Bad -> putStrLn "One or more tests failed"

data Status = Bad | Good

instance Semigroup Status where
  Good <> x = x
  Bad <> _ = Bad

instance Monoid Status where
  mempty = Good
  mappend = (SG.<>)
