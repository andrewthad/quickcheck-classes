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
module Test.QuickCheck.Classes.Common.Ground
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
#if defined(VERSION_aeson)
  , jsonLaws
#endif
  , monoidLaws
  , ordLaws
  , primLaws
  , semigroupLaws
  , showReadLaws
  , storableLaws
#if MIN_VERSION_QuickCheck(2,10,0)
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

-- Ground Types
import Test.QuickCheck.Classes.Bits
import Test.QuickCheck.Classes.Eq
import Test.QuickCheck.Classes.Integral
#if MIN_VERSION_BASE(4,7,0)
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

--import Test.QuickCheck.Classes.Bits
--import Test.QuickCheck.Classes.Bits
--import Test.QuickCheck.Classes.Bits
--import Test.QuickCheck.Classes.Bits
--mport Test.QuickCheck.Classes.Bits
--import Test.QuickCheck.Classes.Bits
--import Test.QuickCheck.Classes.Bits
