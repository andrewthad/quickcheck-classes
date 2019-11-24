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
    QCB.lawsCheck
  , QCB.lawsCheckMany
  , QCB.lawsCheckOne
    -- * Properties
    -- ** Ground types
#if MIN_VERSION_base(4,7,0)
  , QCB.bitsLaws
#endif
  , QCB.eqLaws
  , QCB.substitutiveEqLaws
  , QCB.numLaws
  , QCB.integralLaws
  , QCB.ixLaws
#if MIN_VERSION_base(4,7,0)
  , QCB.isListLaws
#endif
#if HAVE_AESON
  , jsonLaws
#endif
  , QCB.monoidLaws
  , QCB.commutativeMonoidLaws
  , QCB.semigroupMonoidLaws
  , QCB.ordLaws
  , QCB.enumLaws
  , QCB.boundedEnumLaws
  , primLaws
  , QCB.semigroupLaws
  , QCB.commutativeSemigroupLaws
  , QCB.exponentialSemigroupLaws
  , QCB.idempotentSemigroupLaws
  , QCB.rectangularBandSemigroupLaws
#if HAVE_SEMIRINGS
  , semiringLaws
  , ringLaws
  , gcdDomainLaws
  , euclideanLaws
#endif
  , QCB.showLaws
  , QCB.showReadLaws
  , QCB.storableLaws
#if MIN_VERSION_base(4,5,0)
  , QCB.genericLaws
  , QCB.generic1Laws
#endif
#if HAVE_UNARY_LAWS
    -- ** Unary type constructors
  , QCB.alternativeLaws
#if HAVE_SEMIGROUPOIDS
  , altLaws
  , applyLaws
#endif
  , QCB.applicativeLaws
  , QCB.contravariantLaws
  , QCB.foldableLaws
  , QCB.functorLaws
  , QCB.monadLaws
  , QCB.monadPlusLaws
  , QCB.monadZipLaws
#if HAVE_SEMIGROUPOIDS
  , plusLaws
  , extendedPlusLaws
#endif
  , QCB.traversableLaws
#endif
#if HAVE_BINARY_LAWS
    -- ** Binary type constructors
  , QCB.bifoldableLaws
  , QCB.bifunctorLaws
  , QCB.bitraversableLaws
  , QCB.categoryLaws
  , QCB.commutativeCategoryLaws
#if HAVE_SEMIGROUPOIDS
  , semigroupoidLaws
  , commutativeSemigroupoidLaws
#endif
#if HAVE_VECTOR
  , muvectorLaws
#endif
#endif
    -- * Types
  , QCB.Laws(..)
  , QCB.Proxy1(..)
  , QCB.Proxy2(..)
  ) where

--
-- re-exports
--

-- Ground Types
#if MIN_VERSION_base(4,7,0)
import Test.QuickCheck.Classes.IsList
#endif
#if HAVE_AESON
import Test.QuickCheck.Classes.Json
#endif
import Test.QuickCheck.Classes.Prim
#if HAVE_SEMIRINGS
import Test.QuickCheck.Classes.Euclidean
import Test.QuickCheck.Classes.Semiring
import Test.QuickCheck.Classes.Ring
#endif
-- Unary type constructors
#if HAVE_UNARY_LAWS
#if HAVE_SEMIGROUPOIDS
import Test.QuickCheck.Classes.Alt
import Test.QuickCheck.Classes.Apply
#endif
#if HAVE_SEMIGROUPOIDS
import Test.QuickCheck.Classes.Plus
#endif
#endif

-- Binary type constructors
#if HAVE_BINARY_LAWS
#if HAVE_SEMIGROUPOIDS
import Test.QuickCheck.Classes.Semigroupoid
#endif
#endif

#if HAVE_VECTOR
import Test.QuickCheck.Classes.MVector
#endif

import qualified Test.QuickCheck.Classes.Base as QCB
