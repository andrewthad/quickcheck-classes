{-# language DerivingStrategies #-}
{-# language DerivingVia #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

import Test.Tasty (TestTree,defaultMain,testGroup,adjustOption)
import Test.QuickCheck (Arbitrary)
import Data.Proxy (Proxy(..))
import Data.Set (Set)
import Data.Primitive (Array)
import Control.Monad (forM_,replicateM)
import Data.Monoid (All(..))
import Test.QuickCheck.Classes (eqLaws,ordLaws)
import Data.Typeable (Typeable,typeRep)
import Data.Coerce (coerce)
import Data.Set (Set)

import qualified Data.Set as S
import qualified Data.List as L
import qualified GHC.Exts as E
import qualified Test.QuickCheck as QC
import qualified Test.Tasty.QuickCheck as TQC
import qualified Test.QuickCheck.Classes as QCC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "universe"
  [ testGroup "deriving"
    [ testGroup "strict"
      [ laws @A [eqLaws,ordLaws]
      , laws @B [eqLaws,ordLaws]
      , laws @C [eqLaws,ordLaws]
      , laws @D [eqLaws,ordLaws]
      , laws @E [eqLaws,ordLaws]
      , laws @F [eqLaws,ordLaws]
      , laws @G [eqLaws,ordLaws]
      , laws @H [eqLaws,ordLaws]
      , laws @I [eqLaws,ordLaws]
      , laws @K [eqLaws,ordLaws]
      ]
    , testGroup "thunk"
      [ laws @(Thunk A) [eqLaws,ordLaws]
      , laws @(Thunk B) [eqLaws,ordLaws]
      , laws @(Thunk C) [eqLaws,ordLaws]
      , laws @(Thunk D) [eqLaws,ordLaws]
      , laws @(Thunk E) [eqLaws,ordLaws]
      , laws @(Thunk F) [eqLaws,ordLaws]
      , laws @(Thunk G) [eqLaws,ordLaws]
      , laws @(Thunk H) [eqLaws,ordLaws]
      , laws @(Thunk I) [eqLaws,ordLaws]
      , laws @(Thunk K) [eqLaws,ordLaws]
      ]
    , testGroup "lazy"
      [ laws @(Lazy A) [eqLaws,ordLaws]
      , laws @(Lazy B) [eqLaws,ordLaws]
      , laws @(Lazy C) [eqLaws,ordLaws]
      , laws @(Lazy D) [eqLaws,ordLaws]
      , laws @(Lazy E) [eqLaws,ordLaws]
      , laws @(Lazy F) [eqLaws,ordLaws]
      , laws @(Lazy G) [eqLaws,ordLaws]
      , laws @(Lazy H) [eqLaws,ordLaws]
      , laws @(Lazy I) [eqLaws,ordLaws]
      , laws @(Lazy K) [eqLaws,ordLaws]
      ]
    ]
  , testGroup "containers"
    [ testGroup "strict"
      [ laws @(Set A) [eqLaws,ordLaws]
      , laws @(Set B) [eqLaws,ordLaws]
      , laws @(Set C) [eqLaws,ordLaws]
      , laws @(Set D) [eqLaws,ordLaws]
      , laws @(Set E) [eqLaws,ordLaws]
      , laws @(Set F) [eqLaws,ordLaws]
      , laws @(Set G) [eqLaws,ordLaws]
      , laws @(Set H) [eqLaws,ordLaws]
      , laws @(Set I) [eqLaws,ordLaws]
      , laws @(Set K) [eqLaws,ordLaws]
      ]
    , testGroup "lazy"
      [ laws @(SmallLazySet A) [eqLaws,ordLaws]
      , laws @(SmallLazySet B) [eqLaws,ordLaws]
      , laws @(SmallLazySet C) [eqLaws,ordLaws]
      , laws @(SmallLazySet D) [eqLaws,ordLaws]
      , laws @(SmallLazySet E) [eqLaws,ordLaws]
      , laws @(SmallLazySet F) [eqLaws,ordLaws]
      , laws @(SmallLazySet G) [eqLaws,ordLaws]
      , laws @(SmallLazySet H) [eqLaws,ordLaws]
      , laws @(SmallLazySet I) [eqLaws,ordLaws]
      , laws @(SmallLazySet K) [eqLaws,ordLaws]
      ]
    ]
  ]

data A = A0
  deriving stock (Eq,Ord,Show,Read,Bounded,Enum)
  deriving Arbitrary via (Enumeration A)

data B = B0 | B1
  deriving stock (Eq,Ord,Show,Read,Bounded,Enum)
  deriving Arbitrary via (Enumeration B)

data C = C0 | C1 | C2
  deriving stock (Eq,Ord,Show,Read,Bounded,Enum)
  deriving Arbitrary via (Enumeration C)

data D = D0 | D1 | D2 | D3
  deriving stock (Eq,Ord,Show,Read,Bounded,Enum)
  deriving Arbitrary via (Enumeration D)

data E = E0 | E1 | E2 | E3 | E4
  deriving stock (Eq,Ord,Show,Read,Bounded,Enum)
  deriving Arbitrary via (Enumeration E)

data F = F0 | F1 | F2 | F3 | F4 | F5
  deriving stock (Eq,Ord,Show,Read,Bounded,Enum)
  deriving Arbitrary via (Enumeration F)

data G = G0 | G1 | G2 | G3 | G4 | G5 | G6
  deriving stock (Eq,Ord,Show,Read,Bounded,Enum)
  deriving Arbitrary via (Enumeration G)

data H = H0 | H1 | H2 | H3 | H4 | H5 | H6 | H7
  deriving stock (Eq,Ord,Show,Read,Bounded,Enum)
  deriving Arbitrary via (Enumeration H)

data I = I0 | I1 | I2 | I3 | I4 | I5 | I6 | I7 | I8
  deriving stock (Eq,Ord,Show,Read,Bounded,Enum)
  deriving Arbitrary via (Enumeration I)

data J = J0 | J1 | J2 | J3 | J4 | J5 | J6 | J7 | J8 | J9
  deriving stock (Eq,Ord,Show,Read,Bounded,Enum)
  deriving Arbitrary via (Enumeration J)

data K = K0 | K1 | K2 | K3 | K4 | K5 | K6 | K7 | K8 | K9 | K10
  deriving stock (Eq,Ord,Show,Read,Bounded,Enum)
  deriving Arbitrary via (Enumeration K)

laws :: forall a. Typeable a => [Proxy a -> QCC.Laws] -> TestTree
laws = testGroup (show (typeRep (Proxy :: Proxy a))) . map
  ( \f -> let QCC.Laws name pairs = f (Proxy :: Proxy a) in
    testGroup name (map (uncurry TQC.testProperty) pairs)
  )

newtype Enumeration a = Enumeration a

instance (Bounded a, Enum a, Eq a) => Arbitrary (Enumeration a) where
  arbitrary = fmap Enumeration TQC.arbitraryBoundedEnum
  shrink (Enumeration x) = if x == minBound
    then []
    else [Enumeration (pred x)]

data Thunk a = Thunk a
  deriving stock (Eq,Ord,Show,Read)

newtype Lazy a = Lazy a
  deriving newtype (Eq,Ord,Show,Read)

newtype SmallLazySet a = SmallLazySet (Set a)
  deriving newtype (Eq,Ord,Show,Read)

instance Arbitrary a => Arbitrary (Thunk a) where
  arbitrary = do
    a <- TQC.arbitrary
    let {-# NOINLINE b #-}
        b () = a
    pure (Thunk (b ()))
  shrink (Thunk x) = map Thunk (TQC.shrink x)

instance Arbitrary a => Arbitrary (Lazy a) where
  arbitrary = do
    a <- TQC.arbitrary
    let {-# NOINLINE b #-}
        b () = a
    pure (Lazy (b ()))
  shrink (Lazy x) = map Lazy (TQC.shrink x)

instance (Arbitrary a, Ord a) => Arbitrary (SmallLazySet a) where
  arbitrary = do
    a <- TQC.arbitrary
    b <- TQC.arbitrary
    c <- TQC.arbitrary
    let {-# NOINLINE a' #-}
        a' () = a
    let {-# NOINLINE b' #-}
        b' () = b
    let {-# NOINLINE c' #-}
        c' () = c
    pure (SmallLazySet (S.fromList [a' (), b' (), c' (), a' (), b' (), c' ()]))

