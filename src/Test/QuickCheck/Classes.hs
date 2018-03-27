{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wall #-}

{-|

This library provides lists of properties that should hold for common typeclasses.
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
    -- ** Ground Types
  , commutativeMonoidLaws
  , eqLaws
  , ordLaws
  , showReadLaws
#if defined(VERSION_aeson)
  , jsonLaws
#endif
  , integralLaws
  , monoidLaws
  , ordLaws
  , primLaws
  , semigroupLaws
  , showReadLaws
  , storableLaws
  , integralLaws
#if MIN_VERSION_base(4,7,0)
  , bitsLaws
  , isListLaws
#endif
#if MIN_VERSION_QuickCheck(2,10,0)
    -- ** Higher-Kinded Types
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
#if defined(VERSION_semigroupoids)
  , altLaws 
#endif
  , alternativeLaws 
  , applicativeLaws
  , foldableLaws
  , traversableLaws
  , functorLaws
  , monadLaws
  , monadPlusLaws 
  , monadZipLaws
#endif
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
  , bifunctorLaws 
#endif
#endif
    -- * Types
  , Laws(..)
  ) where

import Data.Functor ((<$))
import Control.Applicative (liftA2,(<*>),pure,Applicative,(<$>),Alternative(..))
import Control.Monad.ST
import Data.Bifunctor (Bifunctor(..))
import Data.Bits
import Data.Foldable (foldMap,Foldable)
import Data.Traversable (Traversable,fmapDefault,foldMapDefault,sequenceA,traverse)
import Data.Monoid (Monoid,mconcat,mempty,mappend)
import Data.Primitive hiding (sizeOf,newArray,copyArray)
import Data.Primitive.Addr (Addr(..))
import Data.Proxy
import Data.Semigroup (Semigroup)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import GHC.Exts (Int(I#),(*#),newByteArray#,unsafeFreezeByteArray#,
  copyMutableByteArray#,copyByteArray#,quotInt#,sizeofByteArray#)
import GHC.Ptr (Ptr(..))
import System.IO.Unsafe
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property(..))
import Control.Monad.Primitive (PrimMonad,PrimState,primitive,primitive_)
import Control.Monad.Zip (MonadZip(mzip))
import Control.Arrow ((***))
import qualified Control.Monad.Trans.Writer.Lazy as WL
import qualified Data.Primitive as P
import qualified Data.Semigroup as SG
import qualified Data.Monoid as MND
import qualified Data.List as L
import qualified Data.Set as S

#if defined(VERSION_semigroupoids)
import Data.Functor.Alt (Alt)
import qualified Data.Functor.Alt as Alt
#endif

#if defined(VERSION_aeson)
import Data.Aeson (FromJSON(..),ToJSON(..))
import qualified Data.Aeson as AE
#endif

#if MIN_VERSION_base(4,6,0)
import Text.Read (readMaybe)
#endif

#if MIN_VERSION_base(4,7,0)
import GHC.Exts (IsList(fromList,toList,fromListN),Item,
  copyByteArrayToAddr#,copyAddrToByteArray#)
#endif

#if MIN_VERSION_QuickCheck(2,10,0)
import Control.Exception (ErrorCall,try,evaluate)
import Control.Monad (ap,liftM,MonadPlus(mzero,mplus))
import Control.Monad.Trans.Class (lift)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes
import Data.Functor.Identity
import Data.Functor.Compose
#endif
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
import Test.QuickCheck.Monadic (monadicIO)
import qualified Data.Foldable as F
#endif

-- | A set of laws associated with a typeclass.
data Laws = Laws
  { lawsTypeclass :: String
    -- ^ Name of the typeclass whose laws are tested
  , lawsProperties :: [(String,Property)]
    -- ^ Pairs of law name and property
  }

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

newtype Ap f a = Ap { getAp :: f a }

instance (Applicative f, Semigroup a) => Semigroup (Ap f a) where
  Ap x <> Ap y = Ap $ liftA2 (SG.<>) x y

instance (Applicative f, Monoid a, Semigroup a) => Monoid (Ap f a) where
  mempty = Ap $ pure mempty
  mappend = (SG.<>)

foldMapA :: (Foldable t, Monoid m, Semigroup m, Applicative f) => (a -> f m) -> t a -> f m
foldMapA f = getAp . foldMap (Ap . f)

-- | Tests the following properties:
--
-- [/Partial Isomorphism/]
--   @decode . encode ≡ Just@
-- [/Encoding Equals Value/]
--   @decode . encode ≡ Just . toJSON@
--
-- Note that in the second propertiy, the type of decode is @ByteString -> Value@,
-- not @ByteString -> a@
#if defined(VERSION_aeson)
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
jsonEncodingPartialIsomorphism _ = property $ \(a :: a) ->
  AE.decode (AE.encode a) == Just a

#endif

-- | Tests the following properties:
--
-- [/Partial Isomorphism/]
--   @fromList . toList ≡ id@
-- [/Length Preservation/]
--   @fromList xs ≡ fromListN (length xs) xs@
--
-- /Note:/ This property test is only available when
-- using @base-4.7@ or newer.
#if MIN_VERSION_base(4,7,0)
isListLaws :: (IsList a, Show a, Show (Item a), Arbitrary a, Arbitrary (Item a), Eq a) => Proxy a -> Laws
isListLaws p = Laws "IsList"
  [ ("Partial Isomorphism", isListPartialIsomorphism p)
  , ("Length Preservation", isListLengthPreservation p)
  ]
#endif

showReadLaws :: (Show a, Read a, Eq a, Arbitrary a) => Proxy a -> Laws
showReadLaws p = Laws "Show/Read"
  [ ("Partial Isomorphism", showReadPartialIsomorphism p)
  ]

-- | Tests the following properties:
--
-- [/Associative/]
--   @a <> (b <> c) ≡ (a <> b) <> c@
semigroupLaws :: (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
semigroupLaws p = Laws "Semigroup"
  [ ("Associative", semigroupAssociative p)
  ]

-- | Tests the following properties:
--
-- [/Transitive/]
--   @a == b ∧ b == c ⇒ a == c@
-- [/Symmetric/]
--   @a == b ⇒ b == a@
-- [/Reflexive/]
--   @a == a@
--
-- Some of these properties involve implication. In the case that
-- the left hand side of the implication arrow does not hold, we
-- do not retry. Consequently, these properties only end up being
-- useful when the data type has a small number of inhabitants.
eqLaws :: (Eq a, Arbitrary a, Show a) => Proxy a -> Laws
eqLaws p = Laws "Eq"
  [ ("Transitive", eqTransitive p)
  , ("Symmetric", eqSymmetric p)
  , ("Reflexive", eqReflexive p)
  ]

-- | Tests the following properties:
--
-- [/Antisymmetry/]
--   @a ≤ b ∧ b ≤ a ⇒ a = b  
-- [/Transitivity/]
--   @a ≤ b ∧ b ≤ c ⇒ a ≤ c@
-- [/Totality/]
--   @a ≤ b ∨ a > b@
ordLaws :: (Ord a, Arbitrary a, Show a) => Proxy a -> Laws
ordLaws p = Laws "Ord"
  [ ("Antisymmetry", ordAntisymmetric p)
  , ("Transitivity", ordTransitive p)
  , ("Totality", ordTotal p)
  ]

-- | Tests the following properties:
--
-- [/Associative/]
--   @mappend a (mappend b c) ≡ mappend (mappend a b) c@
-- [/Left Identity/]
--   @mappend mempty a ≡ a@
-- [/Right Identity/]
--   @mappend a mempty ≡ a@
monoidLaws :: (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
monoidLaws p = Laws "Monoid"
  [ ("Associative", monoidAssociative p)
  , ("Left Identity", monoidLeftIdentity p)
  , ("Right Identity", monoidRightIdentity p)
  ]

-- | Tests everything from 'monoidProps' plus the following:
--
-- [/Commutative/]
--   @mappend a b ≡ mappend b a@
commutativeMonoidLaws :: (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
commutativeMonoidLaws p = Laws "Commutative Monoid" $ lawsProperties (monoidLaws p) ++
  [ ("Commutative", monoidCommutative p)
  ]

-- | Tests the following properties:
--
-- [/Quotient Remainder/]
--   @(quot x y) * y + (rem x y) ≡ x@
-- [/Division Modulus/]
--   @(div x y) * y + (mod x y) ≡ x@
-- [/Integer Roundtrip/]
--   @fromInteger (toInteger x) ≡ x@
integralLaws :: (Integral a, Arbitrary a, Show a) => Proxy a -> Laws
integralLaws p = Laws "Integral"
  [ ("Quotient Remainder", integralQuotientRemainder p)
  , ("Division Modulus", integralDivisionModulus p)
  , ("Integer Roundtrip", integralIntegerRoundtrip p)
  ]

-- | Tests the following properties:
--
-- [/Conjunction Idempotence/]
--   @n .&. n ≡ n@
-- [/Disjunction Idempotence/]
--   @n .|. n ≡ n@
-- [/Double Complement/]
--   @complement (complement n) ≡ n@
-- [/Set Bit/]
--   @setBit n i ≡ n .|. bit i@
-- [/Clear Bit/]
--   @clearBit n i ≡ n .&. complement (bit i)@
-- [/Complement Bit/]
--   @complementBit n i ≡ xor n (bit i)@
-- [/Clear Zero/]
--   @clearBit zeroBits i ≡ zeroBits@
-- [/Set Zero/]
--   @setBit zeroBits i ≡ bit i@
-- [/Test Zero/]
--   @testBit zeroBits i ≡ False@
-- [/Pop Zero/]
--   @popCount zeroBits ≡ 0@
-- [/Count Leading Zeros of Zero/]
--   @countLeadingZeros zeroBits ≡ finiteBitSize ⊥@
-- [/Count Trailing Zeros of Zero/]
--   @countTrailingZeros zeroBits ≡ finiteBitSize ⊥@
--
-- All of the useful instances of the 'Bits' typeclass
-- also have 'FiniteBits' instances, so these property
-- tests actually require that instance as well.
--
-- /Note:/ This property test is only available when
-- using @base-4.7@ or newer.
#if MIN_VERSION_base(4,7,0)
bitsLaws :: (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Laws
bitsLaws p = Laws "Bits"
  [ ("Conjunction Idempotence", bitsConjunctionIdempotence p)
  , ("Disjunction Idempotence", bitsDisjunctionIdempotence p)
  , ("Double Complement", bitsDoubleComplement p)
  , ("Set Bit", bitsSetBit p)
  , ("Clear Bit", bitsClearBit p)
  , ("Complement Bit", bitsComplementBit p)
  , ("Clear Zero", bitsClearZero p)
  , ("Set Zero", bitsSetZero p)
  , ("Test Zero", bitsTestZero p)
  , ("Pop Zero", bitsPopZero p)
#if MIN_VERSION_base(4,8,0)
  , ("Count Leading Zeros of Zero", bitsCountLeadingZeros p)
  , ("Count Trailing Zeros of Zero", bitsCountTrailingZeros p)
#endif
  ]
#endif

-- | Test that a 'Prim' instance obey the several laws.
primLaws :: (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
primLaws p = Laws "Prim"
  [ ("ByteArray Set-Get (you get back what you put in)", primSetGetByteArray p)
  , ("ByteArray Get-Set (putting back what you got out has no effect)", primGetSetByteArray p)
  , ("ByteArray Set-Set (setting twice is same as setting once)", primSetSetByteArray p)
#if MIN_VERSION_base(4,7,0)
  , ("ByteArray List Conversion Roundtrips", primListByteArray p)
#endif
  , ("Addr Set-Get (you get back what you put in)", primSetGetAddr p)
  , ("Addr Get-Set (putting back what you got out has no effect)", primGetSetAddr p)
  , ("Addr List Conversion Roundtrips", primListAddr p)
  ]

storableLaws :: (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Laws
storableLaws p = Laws "Storable"
  [ ("Set-Get (you get back what you put in)", storableSetGet p)
  , ("Get-Set (putting back what you got out has no effect)", storableGetSet p)
  , ("List Conversion Roundtrips", storableList p)
  ]

#if MIN_VERSION_base(4,7,0)
isListPartialIsomorphism :: forall a. (IsList a, Show a, Arbitrary a, Eq a) => Proxy a -> Property
isListPartialIsomorphism _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "fromList (toList a)"
  (\a -> fromList (toList a))
  "a"
  (\a -> a)

isListLengthPreservation :: forall a. (IsList a, Show (Item a), Arbitrary (Item a), Eq a) => Proxy a -> Property
isListLengthPreservation _ = property $ \(xs :: [Item a]) ->
  (fromList xs :: a) == fromListN (length xs) xs
#endif

showReadPartialIsomorphism :: forall a. (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showReadPartialIsomorphism _ = property $ \(a :: a) ->
#if MIN_VERSION_base(4,6,0)
  readMaybe (show a) == Just a
#else
  read (show a) == a
#endif

eqTransitive :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqTransitive _ = property $ \(a :: a) b c -> case a == b of
  True -> case b == c of
    True -> a == c
    False -> a /= c
  False -> case b == c of
    True -> a /= c
    False -> True

ordAntisymmetric :: forall a. (Show a, Ord a, Arbitrary a) => Proxy a -> Property
ordAntisymmetric _ = property $ \(a :: a) b -> ((a <= b) && (b <= a)) == (a == b)

ordTotal :: forall a. (Show a, Ord a, Arbitrary a) => Proxy a -> Property
ordTotal _ = property $ \(a :: a) b -> ((a <= b) || (b <= a)) == True

-- Technically, this tests something a little stronger than it is supposed to.
-- But that should be alright since this additional strength is implied by
-- the rest of the Ord laws.
ordTransitive :: forall a. (Show a, Ord a, Arbitrary a) => Proxy a -> Property
ordTransitive _ = property $ \(a :: a) b c -> case (compare a b, compare b c) of
  (LT,LT) -> a < c
  (LT,EQ) -> a < c
  (LT,GT) -> True
  (EQ,LT) -> a < c
  (EQ,EQ) -> a == c
  (EQ,GT) -> a > c
  (GT,LT) -> True
  (GT,EQ) -> a > c
  (GT,GT) -> a > c

--ordComparable :: forall a. (Show a, Ord a, Arbitrary a) => Proxy a -> Property
--ordComparable _ = property $ \(a :: a) b -> a > b || b >= a

eqSymmetric :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqSymmetric _ = property $ \(a :: a) b -> case a == b of
  True -> b == a
  False -> b /= a

eqReflexive :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqReflexive _ = property $ \(a :: a) -> a == a

semigroupAssociative :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupAssociative _ = property $ \(a :: a) b c -> a SG.<> (b SG.<> c) == (a SG.<> b) SG.<> c

monoidAssociative :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidAssociative _ = myForAllShrink True (const True)
  (\(a :: a,b,c) -> ["a = " ++ show a, "b = " ++ show b, "c = " ++ show c])
  "mappend a (mappend b c)"
  (\(a,b,c) -> mappend a (mappend b c))
  "mappend (mappend a b) c"
  (\(a,b,c) -> mappend (mappend a b) c)

monoidLeftIdentity :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidLeftIdentity _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "mappend mempty a"
  (\a -> mappend mempty a)
  "a"
  (\a -> a)

monoidRightIdentity :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidRightIdentity _ = myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  "mappend a mempty"
  (\a -> mappend a mempty)
  "a"
  (\a -> a)

#if MIN_VERSION_base(4,7,0)
bitsConjunctionIdempotence :: forall a. (Bits a, Arbitrary a, Show a) => Proxy a -> Property
bitsConjunctionIdempotence _ = myForAllShrink False (const True)
  (\(n :: a) -> ["n = " ++ show n])
  "n .&. n"
  (\n -> n .&. n)
  "n"
  (\n -> n)

bitsDisjunctionIdempotence :: forall a. (Bits a, Arbitrary a, Show a) => Proxy a -> Property
bitsDisjunctionIdempotence _ = myForAllShrink False (const True)
  (\(n :: a) -> ["n = " ++ show n])
  "n .|. n"
  (\n -> n .|. n)
  "n"
  (\n -> n)

bitsDoubleComplement :: forall a. (Bits a, Arbitrary a, Show a) => Proxy a -> Property
bitsDoubleComplement _ = myForAllShrink False (const True)
  (\(n :: a) -> ["n = " ++ show n])
  "complement (complement n)"
  (\n -> complement (complement n))
  "n"
  (\n -> n)

bitsSetBit :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsSetBit _ = myForAllShrink True (const True)
  (\(n :: a, BitIndex i :: BitIndex a) -> ["n = " ++ show n, "i = " ++ show i])
  "setBit n i"
  (\(n,BitIndex i) -> setBit n i)
  "n .|. bit i"
  (\(n,BitIndex i) -> n .|. bit i)

bitsClearBit :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsClearBit _ = myForAllShrink True (const True)
  (\(n :: a, BitIndex i :: BitIndex a) -> ["n = " ++ show n, "i = " ++ show i])
  "clearBit n i"
  (\(n,BitIndex i) -> clearBit n i)
  "n .&. complement (bit i)"
  (\(n,BitIndex i) -> n .&. complement (bit i))

bitsComplementBit :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsComplementBit _ = myForAllShrink True (const True)
  (\(n :: a, BitIndex i :: BitIndex a) -> ["n = " ++ show n, "i = " ++ show i])
  "complementBit n i"
  (\(n,BitIndex i) -> complementBit n i)
  "xor n (bit i)"
  (\(n,BitIndex i) -> xor n (bit i))

bitsClearZero :: forall a. (Bits a, Arbitrary a, Show a) => Proxy a -> Property
bitsClearZero _ = myForAllShrink False (const True)
  (\(n :: a) -> ["n = " ++ show n])
  "complement (complement n)"
  (\n -> complement (complement n))
  "n"
  (\n -> n)

bitsSetZero :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsSetZero _ = myForAllShrink True (const True)
  (\(BitIndex i :: BitIndex a) -> ["i = " ++ show i])
  "setBit zeroBits i"
  (\(BitIndex i) -> setBit (zeroBits :: a) i)
  "bit i"
  (\(BitIndex i) -> bit i)

bitsTestZero :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsTestZero _ = myForAllShrink True (const True)
  (\(BitIndex i :: BitIndex a) -> ["i = " ++ show i])
  "testBit zeroBits i"
  (\(BitIndex i) -> testBit (zeroBits :: a) i)
  "False"
  (\_ -> False)

bitsPopZero :: forall a. (Bits a, Arbitrary a, Show a) => Proxy a -> Property
bitsPopZero _ = myForAllShrink True (const True)
  (\() -> [])
  "popCount zeroBits"
  (\() -> popCount (zeroBits :: a))
  "0"
  (\() -> 0)
#endif

#if MIN_VERSION_base(4,8,0)
bitsCountLeadingZeros :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsCountLeadingZeros _ = myForAllShrink True (const True)
  (\() -> [])
  "countLeadingZeros zeroBits"
  (\() -> countLeadingZeros (zeroBits :: a))
  "finiteBitSize undefined"
  (\() -> finiteBitSize (undefined :: a))

bitsCountTrailingZeros :: forall a. (FiniteBits a, Arbitrary a, Show a) => Proxy a -> Property
bitsCountTrailingZeros _ = myForAllShrink True (const True)
  (\() -> [])
  "countTrailingZeros zeroBits"
  (\() -> countTrailingZeros (zeroBits :: a))
  "finiteBitSize undefined"
  (\() -> finiteBitSize (undefined :: a))
#endif

integralQuotientRemainder :: forall a. (Integral a, Arbitrary a, Show a) => Proxy a -> Property
integralQuotientRemainder _ = myForAllShrink False (\(_,y) -> y /= 0)
  (\(x :: a, y) -> ["x = " ++ show x, "y = " ++ show y])
  "(quot x y) * y + (rem x y)"
  (\(x,y) -> (quot x y) * y + (rem x y))
  "x"
  (\(x,_) -> x)

integralDivisionModulus :: forall a. (Integral a, Arbitrary a, Show a) => Proxy a -> Property
integralDivisionModulus _ = myForAllShrink False (\(_,y) -> y /= 0)
  (\(x :: a, y) -> ["x = " ++ show x, "y = " ++ show y])
  "(div x y) * y + (mod x y)"
  (\(x,y) -> (div x y) * y + (mod x y))
  "x"
  (\(x,_) -> x)

integralIntegerRoundtrip :: forall a. (Integral a, Arbitrary a, Show a) => Proxy a -> Property
integralIntegerRoundtrip _ = myForAllShrink False (const True)
  (\(x :: a) -> ["x = " ++ show x])
  "fromInteger (toInteger x)"
  (\x -> fromInteger (toInteger x))
  "x"
  (\x -> x)

monoidCommutative :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidCommutative _ = myForAllShrink True (const True)
  (\(a :: a,b) -> ["a = " ++ show a, "b = " ++ show b])
  "mappend a b"
  (\(a,b) -> mappend a b)
  "mappend b a"
  (\(a,b) -> mappend b a)

#if MIN_VERSION_base(4,7,0)
primListByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primListByteArray _ = property $ \(as :: [a]) ->
  as == toList (fromList as :: PrimArray a)
#endif

primListAddr :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primListAddr _ = property $ \(as :: [a]) -> unsafePerformIO $ do
  let len = L.length as
  ptr@(Ptr addr#) :: Ptr a <- mallocBytes (len * P.sizeOf (undefined :: a))
  let addr = Addr addr#
  let go :: Int -> [a] -> IO ()
      go !ix xs = case xs of
        [] -> return ()
        (x : xsNext) -> do
          writeOffAddr addr ix x
          go (ix + 1) xsNext
  go 0 as
  let rebuild :: Int -> IO [a]
      rebuild !ix = if ix < len
        then (:) <$> readOffAddr addr ix <*> rebuild (ix + 1)
        else return []
  asNew <- rebuild 0
  free ptr
  return (as == asNew)

primSetGetByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primSetGetByteArray _ = property $ \(a :: a) len -> (len > 0) ==> do
  ix <- choose (0,len - 1)
  return $ runST $ do
    arr <- newPrimArray len
    writePrimArray arr ix a
    a' <- readPrimArray arr ix
    return (a == a')

primGetSetByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primGetSetByteArray _ = property $ \(as :: [a]) -> (not (L.null as)) ==> do
  let arr1 = primArrayFromList as :: PrimArray a
      len = L.length as
  ix <- choose (0,len - 1)
  arr2 <- return $ runST $ do
    marr <- newPrimArray len
    copyPrimArray marr 0 arr1 0 len
    a <- readPrimArray marr ix
    writePrimArray marr ix a
    unsafeFreezePrimArray marr
  return (arr1 == arr2)

primSetSetByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primSetSetByteArray _ = property $ \(a :: a) (as :: [a]) -> (not (L.null as)) ==> do
  let arr1 = primArrayFromList as :: PrimArray a
      len = L.length as
  ix <- choose (0,len - 1)
  (arr2,arr3) <- return $ runST $ do
    marr2 <- newPrimArray len
    copyPrimArray marr2 0 arr1 0 len
    writePrimArray marr2 ix a
    marr3 <- newPrimArray len
    copyMutablePrimArray marr3 0 marr2 0 len
    arr2 <- unsafeFreezePrimArray marr2
    writePrimArray marr3 ix a
    arr3 <- unsafeFreezePrimArray marr3
    return (arr2,arr3)
  return (arr2 == arr3)

primSetGetAddr :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primSetGetAddr _ = property $ \(a :: a) len -> (len > 0) ==> do
  ix <- choose (0,len - 1)
  return $ unsafePerformIO $ do
    ptr@(Ptr addr#) :: Ptr a <- mallocBytes (len * P.sizeOf (undefined :: a))
    let addr = Addr addr#
    writeOffAddr addr ix a
    a' <- readOffAddr addr ix
    free ptr
    return (a == a')

primGetSetAddr :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primGetSetAddr _ = property $ \(as :: [a]) -> (not (L.null as)) ==> do
  let arr1 = primArrayFromList as :: PrimArray a
      len = L.length as
  ix <- choose (0,len - 1)
  arr2 <- return $ unsafePerformIO $ do
    ptr@(Ptr addr#) :: Ptr a <- mallocBytes (len * P.sizeOf (undefined :: a))
    let addr = Addr addr#
    copyPrimArrayToPtr ptr arr1 0 len
    a :: a <- readOffAddr addr ix
    writeOffAddr addr ix a
    marr <- newPrimArray len
    copyPtrToMutablePrimArray marr 0 ptr len
    free ptr
    unsafeFreezePrimArray marr
  return (arr1 == arr2)

storableSetGet :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storableSetGet _ = property $ \(a :: a) len -> (len > 0) ==> do
  ix <- choose (0,len - 1)
  return $ unsafePerformIO $ do
    ptr :: Ptr a <- mallocArray len
    pokeElemOff ptr ix a
    a' <- peekElemOff ptr ix
    free ptr
    return (a == a')

storableGetSet :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storableGetSet _ = property $ \(as :: [a]) -> (not (L.null as)) ==> do
  let len = L.length as
  ix <- choose (0,len - 1)
  return $ unsafePerformIO $ do
    ptrA <- newArray as
    ptrB <- mallocArray len
    copyArray ptrB ptrA len
    a <- peekElemOff ptrA ix
    pokeElemOff ptrA ix a
    res <- arrayEq ptrA ptrB len
    free ptrA
    free ptrB
    return res

storableList :: forall a. (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
storableList _ = property $ \(as :: [a]) -> unsafePerformIO $ do
  let len = L.length as
  ptr <- newArray as
  let rebuild :: Int -> IO [a]
      rebuild !ix = if ix < len
        then (:) <$> peekElemOff ptr ix <*> rebuild (ix + 1)
        else return []
  asNew <- rebuild 0
  free ptr
  return (as == asNew)

arrayEq :: forall a. (Storable a, Eq a) => Ptr a -> Ptr a -> Int -> IO Bool
arrayEq ptrA ptrB len = go 0 where
  go !i = if i < len
    then do
      a <- peekElemOff ptrA i
      b <- peekElemOff ptrB i
      if a == b
        then go (i + 1)
        else return False
    else return True

#if MIN_VERSION_QuickCheck(2,10,0)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
-- | Tests the following functor properties:
--
-- [/Identity/]
--   @'fmap' 'id' ≡ 'id'@
-- [/Composition/]
--   @fmap (f . g) ≡ 'fmap' f . 'fmap' g@
-- [/Const/]
--   @(<$) ≡ 'fmap' 'const'@
functorLaws :: (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
functorLaws p = Laws "Functor"
  [ ("Identity", functorIdentity p)
  , ("Composition", functorComposition p)
  , ("Const", functorConst p)
  ]

-- | Tests the following alternative properties:
--
-- [/Identity/]
--   @'empty' '<|>' x ≡ x@
--   @x '<|>' 'empty' ≡ x@
-- [/Associativity/]
--   @a '<|>' (b '<|>' c) ≡ (a '<|>' b) '<|>' c)@ 
alternativeLaws :: (Alternative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
alternativeLaws p = Laws "Alternative"
  [ ("Identity", alternativeIdentity p)
  , ("Associativity", alternativeAssociativity p)
  ]

-- | Tests the following monad plus properties:
--
-- [/Left Identity/]
--   @'mplus' 'empty' x ≡ x@
-- [/Right Identity/]
--   @'mplus' x 'empty' ≡ x@
-- [/Associativity/]
--   @'mplus' a ('mplus' b c) ≡ 'mplus' ('mplus' a b) c)@ 
-- [/Left Zero/]
--   @'mzero' '>>=' f ≡ 'mzero'@
-- [/Right Zero/]
--   @m >> 'mzero' ≡ 'mzero'@
monadPlusLaws :: (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
monadPlusLaws p = Laws "MonadPlus"
  [ ("Left Identity", monadPlusLeftIdentity p)
  , ("Right Identity", monadPlusRightIdentity p)
  , ("Associativity", monadPlusAssociativity p)
  , ("Left Zero", monadPlusLeftZero p)
  , ("Right Zero", monadPlusRightZero p)
  ]

-- | Tests the following applicative properties:
--
-- [/Identity/]
--   @'pure' 'id' '<*>' v ≡ v@
-- [/Composition/]
--   @'pure' (.) '<*>' u '<*>' v '<*>' w ≡ u '<*>' (v '<*>' w)@
-- [/Homomorphism/]
--   @'pure' f '<*>' 'pure' x ≡ 'pure' (f x)@
-- [/Interchange/]
--   @u '<*>' 'pure' y ≡ 'pure' ('$' y) '<*>' u@
-- [/LiftA2 (1)/]
--   @('<*>') ≡ 'liftA2' 'id'@
applicativeLaws :: (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
applicativeLaws p = Laws "Applicative"
  [ ("Identity", applicativeIdentity p)
  , ("Composition", applicativeComposition p)
  , ("Homomorphism", applicativeHomomorphism p)
  , ("Interchange", applicativeInterchange p)
  , ("LiftA2 Part 1", applicativeLiftA2_1 p)
    -- todo: liftA2 part 2, we need an equation of two variables for this
  ]

-- | Tests the following alt properties:
--
-- [/Associativity/]
--   @(a '<!>' b) '<!>' c ≡ a '<!>' (b '<!>' c)@
-- [/Left Distributivity/]
--   @f '<$>' (a '<!>' b) = (f '<$>' a) '<!>' (f '<$>' b)
#if defined(VERSION_semigroupoids)
altLaws :: (Alt f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
altLaws p = Laws "Alt"
  [ ("Associativity", altAssociative p)
  , ("Left Distributivity", altLeftDistributive p)
  ]

altAssociative :: forall proxy f. (Alt f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
altAssociative _ = property $ \(Apply (a :: f Integer)) (Apply (b :: f Integer)) (Apply (c :: f Integer)) -> eq1 ((a Alt.<!> b) Alt.<!> c) (a Alt.<!> (b Alt.<!> c))

altLeftDistributive :: forall proxy f. (Alt f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
altLeftDistributive _ = property $ \(Apply (a :: f Integer)) (Apply (b :: f Integer)) -> eq1 (id <$> (a Alt.<!> b)) ((id <$> a) Alt.<!> (id <$> b))
#endif


-- | Tests the following monadic properties:
--
-- [/Left Identity/]
--   @'return' a '>>=' k ≡ k a@
-- [/Right Identity/]
--   @m '>>=' 'return' ≡ m@
-- [/Associativity/]
--   @m '>>=' (\\x -> k x '>>=' h) ≡ (m '>>=' k) '>>=' h@
-- [/Return/]
--   @'pure' ≡ 'return'@
-- [/Ap/]
--   @('<*>') ≡ 'ap'@
monadLaws :: (Monad f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
monadLaws p = Laws "Monad"
  [ ("Left Identity", monadLeftIdentity p)
  , ("Right Identity", monadRightIdentity p)
  , ("Associativity", monadAssociativity p)
  , ("Return", monadReturn p)
  , ("Ap", monadAp p)
  ]

-- | Tests the following monadic zipping properties:
--
-- [/Naturality/]
--   @liftM (f *** g) (mzip ma mb) = mzip (liftM f ma) (liftM g mb)@
--
-- In the laws above, the infix function @***@ refers to a typeclass
-- method of 'Arrow'.
monadZipLaws :: (MonadZip f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
monadZipLaws p = Laws "Monad"
  [ ("Naturality", monadZipNaturality p)
  ]

-- | Tests the following 'Foldable' properties:
--
-- [/fold/]
--   @'fold' ≡ 'foldMap' 'id'@
-- [/foldMap/]
--   @'foldMap' f ≡ 'foldr' ('mappend' . f) 'mempty'@
-- [/foldr/]
--   @'foldr' f z t ≡ 'appEndo' ('foldMap' ('Endo' . f) t ) z@
-- [/foldr'/]
--   @'foldr'' f z0 xs = let f\' k x z = k '$!' f x z in 'foldl' f\' 'id' xs z0@
-- [/foldr1/]
--   @'foldr1' f t ≡ let Just (xs,x) = unsnoc ('toList' t) in 'foldr' f x xs@
-- [/foldl/]
--   @'foldl' f z t ≡ 'appEndo' ('getDual' ('foldMap' ('Dual' . 'Endo' . 'flip' f) t)) z@
-- [/foldl'/]
--   @'foldl'' f z0 xs ≡ let f' x k z = k '$!' f z x in 'foldr' f\' 'id' xs z0@
-- [/foldl1/]
--   @'foldl1' f t ≡ let x : xs = 'toList' t in 'foldl' f x xs@
-- [/toList/]
--   @'F.toList' ≡ 'foldr' (:) []@
-- [/null/]
--   @'null' ≡ 'foldr' ('const' ('const' 'False')) 'True'@
-- [/length/]
--   @'length' ≡ getSum . foldMap ('const' ('Sum' 1))@
--
-- Note that this checks to ensure that @foldl\'@ and @foldr\'@
-- are suitably strict.
foldableLaws :: (Foldable f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
foldableLaws = foldableLawsInternal

foldableLawsInternal :: forall proxy f. (Foldable f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
foldableLawsInternal p = Laws "Foldable"
  [ (,) "fold" $ property $ \(Apply (a :: f (SG.Sum Integer))) ->
      F.fold a == F.foldMap id a
  , (,) "foldMap" $ property $ \(Apply (a :: f Integer)) (e :: Equation) ->
      let f = SG.Sum . runEquation e
       in F.foldMap f a == F.foldr (mappend . f) mempty a
  , (,) "foldr" $ property $ \(e :: EquationTwo) (z :: Integer) (Apply (t :: f Integer)) ->
      let f = runEquationTwo e
       in F.foldr f z t == SG.appEndo (foldMap (SG.Endo . f) t) z
  , (,) "foldr'" (foldableFoldr' p)
  , (,) "foldl" $ property $ \(e :: EquationTwo) (z :: Integer) (Apply (t :: f Integer)) ->
      let f = runEquationTwo e
       in F.foldl f z t == SG.appEndo (SG.getDual (F.foldMap (SG.Dual . SG.Endo . flip f) t)) z
  , (,) "foldl'" (foldableFoldl' p)
  , (,) "foldl1" $ property $ \(e :: EquationTwo) (Apply (t :: f Integer)) ->
      case compatToList t of
        [] -> True
        x : xs ->
          let f = runEquationTwo e
           in F.foldl1 f t == F.foldl f x xs
  , (,) "foldr1" $ property $ \(e :: EquationTwo) (Apply (t :: f Integer)) ->
      case unsnoc (compatToList t) of
        Nothing -> True
        Just (xs,x) ->
          let f = runEquationTwo e
           in F.foldr1 f t == F.foldr f x xs
  , (,) "toList" $ property $ \(Apply (t :: f Integer)) ->
      eq1 (F.toList t) (F.foldr (:) [] t)
#if MIN_VERSION_base(4,8,0)
  , (,) "null" $ property $ \(Apply (t :: f Integer)) ->
      null t == F.foldr (const (const False)) True t
  , (,) "length" $ property $ \(Apply (t :: f Integer)) ->
      F.length t == SG.getSum (F.foldMap (const (SG.Sum 1)) t)
#endif
  ]

unsnoc :: [a] -> Maybe ([a],a)
unsnoc [] = Nothing
unsnoc [x] = Just ([],x)
unsnoc (x:y:xs) = fmap (\(bs,b) -> (x:bs,b)) (unsnoc (y : xs))

compatToList :: Foldable f => f a -> [a]
compatToList = foldMap (\x -> [x])

foldableFoldl' :: forall proxy f. (Foldable f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
foldableFoldl' _ = property $ \(_ :: ChooseSecond) (_ :: LastNothing) (Apply (xs :: f (Bottom Integer))) ->
  monadicIO $ do
    let f :: Integer -> Bottom Integer -> Integer
        f a b = case b of
          BottomUndefined -> error "foldableFoldl' example"
          BottomValue v -> if even v
            then a
            else v
        z0 = 0
    r1 <- lift $ do
      let f' x k z = k $! f z x
      e <- try (evaluate (F.foldr f' id xs z0))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    r2 <- lift $ do
      e <- try (evaluate (F.foldl' f z0 xs))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    return (r1 == r2)

foldableFoldr' :: forall proxy f. (Foldable f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
foldableFoldr' _ = property $ \(_ :: ChooseFirst) (_ :: LastNothing) (Apply (xs :: f (Bottom Integer))) ->
  monadicIO $ do
    let f :: Bottom Integer -> Integer -> Integer
        f a b = case a of
          BottomUndefined -> error "foldableFoldl' example"
          BottomValue v -> if even v
            then v
            else b
        z0 = 0
    r1 <- lift $ do
      let f' k x z = k $! f x z
      e <- try (evaluate (F.foldl f' id xs z0))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    r2 <- lift $ do
      e <- try (evaluate (F.foldr' f z0 xs))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    return (r1 == r2)

-- | Tests the following 'Traversable' properties:
--
-- [/Naturality/]
--   @t . 'traverse' f = 'traverse' (t . f)@
--   for every applicative transformation @t@
-- [/Identity/]
--   @'traverse' Identity = Identity@
-- [/Composition/]
--   @'traverse' (Compose . 'fmap' g . f) = Compose . 'fmap' ('traverse' g) . 'traverse' f@
-- [/Sequence Naturality/]
--   @t . 'sequenceA' = 'sequenceA' . 'fmap' t@
--   for every applicative transformation @t@
-- [/Sequence Identity/]
--   @'sequenceA' . 'fmap' Identity = Identity@
-- [/Sequence Composition/]
--   @'sequenceA' . 'fmap' Compose = Compose . 'fmap' 'sequenceA' . 'sequenceA'@
-- [/foldMap/]
--   @'foldMap' = 'foldMapDefault'@
-- [/fmap/]
--   @'fmap' = 'fmapDefault'@
--
-- Where an /applicative transformation/ is a function
--
-- @t :: (Applicative f, Applicative g) => f a -> g a@
--
-- preserving the 'Applicative' operations, i.e.
--
-- * Identity: @t ('pure' x) = 'pure' x@
-- * Distributivity: @t (x '<*>' y) = t x '<*>' t y@
traversableLaws :: (Traversable f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
traversableLaws = traversableLawsInternal

traversableLawsInternal :: forall proxy f. (Traversable f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Laws
traversableLawsInternal p = Laws "Traversable"
  [ (,) "Naturality" $ property $ \(Apply (a :: f Integer)) ->
      propNestedEq1 (apTrans (traverse func4 a)) (traverse (apTrans . func4) a)
  , (,) "Identity" $ property $ \(Apply (t :: f Integer)) ->
      nestedEq1 (traverse Identity t) (Identity t)
  , (,) "Composition" $ property $ \(Apply (t :: f Integer)) ->
      nestedEq1 (traverse (Compose . fmap func5 . func6) t) (Compose (fmap (traverse func5) (traverse func6 t)))
  , (,) "Sequence Naturality" $ property $ \(Apply (x :: f (Compose Triple ((,) (S.Set Integer)) Integer))) ->
      let a = fmap toSpecialApplicative x in
      propNestedEq1 (apTrans (sequenceA a)) (sequenceA (fmap apTrans a))
  , (,) "Sequence Identity" $ property $ \(Apply (t :: f Integer)) ->
      nestedEq1 (sequenceA (fmap Identity t)) (Identity t)
  , (,) "Sequence Composition" $ property $ \(Apply (t :: f (Triple (Triple Integer)))) ->
      nestedEq1 (sequenceA (fmap Compose t)) (Compose (fmap sequenceA (sequenceA t)))
  , (,) "foldMap" $ property $ \(Apply (t :: f Integer)) ->
      foldMap func3 t == foldMapDefault func3 t
  , (,) "fmap" $ property $ \(Apply (t :: f Integer)) ->
      eq1 (fmap func3 t) (fmapDefault func3 t)
  ]

-- the Functor constraint is needed for transformers-0.4
nestedEq1 :: (Eq1 f, Eq1 g, Eq a, Functor f) => f (g a) -> f (g a) -> Bool
nestedEq1 x y = eq1 (Compose x) (Compose y)

propNestedEq1 :: (Eq1 f, Eq1 g, Eq a, Show1 f, Show1 g, Show a, Functor f)
  => f (g a) -> f (g a) -> Property
propNestedEq1 x y = Compose x === Compose y

toSpecialApplicative ::
     Compose Triple ((,) (S.Set Integer)) Integer
  -> Compose Triple (WL.Writer (S.Set Integer)) Integer
toSpecialApplicative (Compose (Triple a b c)) =
  Compose (Triple (WL.writer (flipPair a)) (WL.writer (flipPair b)) (WL.writer (flipPair c)))

flipPair :: (a,b) -> (b,a)
flipPair (x,y) = (y,x)

-- Reverse the list and accumulate the writers. We cannot
-- use Sum or Product or else it wont actually be a valid
-- applicative transformation.
apTrans :: 
     Compose Triple (WL.Writer (S.Set Integer)) a
  -> Compose (WL.Writer (S.Set Integer)) Triple a
apTrans (Compose xs) = Compose (sequenceA (reverseTriple xs))

func3 :: Integer -> SG.Sum Integer
func3 i = SG.Sum (3 * i * i - 7 * i + 4)

func4 :: Integer -> Compose Triple (WL.Writer (S.Set Integer)) Integer
func4 i = Compose $ Triple
  (WL.writer (i * i, S.singleton (i * 7 + 5)))
  (WL.writer (i + 2, S.singleton (i * i + 3)))
  (WL.writer (i * 7, S.singleton 4))

func5 :: Integer -> Triple Integer
func5 i = Triple (i + 2) (i * 3) (i * i)

func6 :: Integer -> Triple Integer
func6 i = Triple (i * i * i) (4 * i - 7) (i * i * i)

data Triple a = Triple a a a
  deriving (Show,Eq)

tripleLiftEq :: (a -> b -> Bool) -> Triple a -> Triple b -> Bool
tripleLiftEq p (Triple a1 b1 c1) (Triple a2 b2 c2) =
  p a1 a2 && p b1 b2 && p c1 c2

instance Eq1 Triple where
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
  liftEq = tripleLiftEq
#else
  eq1 = tripleLiftEq (==)
#endif

tripleLiftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Triple a -> ShowS
tripleLiftShowsPrec elemShowsPrec elemShowList p (Triple a b c) = showParen (p > 10)
  $ showString "Triple "
  . elemShowsPrec 11 a
  . showString " "
  . elemShowsPrec 11 b
  . showString " "
  . elemShowsPrec 11 c

instance Show1 Triple where
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
  liftShowsPrec = tripleLiftShowsPrec
#else
  showsPrec1 = tripleLiftShowsPrec showsPrec showList
#endif

instance Arbitrary1 Triple where
  liftArbitrary x = Triple <$> x <*> x <*> x

instance Arbitrary a => Arbitrary (Triple a) where
  arbitrary = liftArbitrary arbitrary

instance Functor Triple where
  fmap f (Triple a b c) = Triple (f a) (f b) (f c)

instance Applicative Triple where
  pure a = Triple a a a
  Triple f g h <*> Triple a b c = Triple (f a) (g b) (h c)

instance Foldable Triple where
  foldMap f (Triple a b c) = f a MND.<> f b MND.<> f c

instance Traversable Triple where
  traverse f (Triple a b c) = Triple <$> f a <*> f b <*> f c

reverseTriple :: Triple a -> Triple a
reverseTriple (Triple a b c) = Triple c b a

data ChooseSecond = ChooseSecond
  deriving (Eq)

data ChooseFirst = ChooseFirst
  deriving (Eq)

data LastNothing = LastNothing
  deriving (Eq)

data Bottom a = BottomUndefined | BottomValue a
  deriving (Eq)

instance Show ChooseFirst where
  show ChooseFirst = "\\a b -> if even a then a else b"

instance Show ChooseSecond where
  show ChooseSecond = "\\a b -> if even b then a else b"

instance Show LastNothing where
  show LastNothing = "0"

instance Show a => Show (Bottom a) where
  show x = case x of
    BottomUndefined -> "undefined"
    BottomValue a -> show a

instance Arbitrary ChooseSecond where
  arbitrary = pure ChooseSecond

instance Arbitrary ChooseFirst where
  arbitrary = pure ChooseFirst

instance Arbitrary LastNothing where
  arbitrary = pure LastNothing

instance Arbitrary a => Arbitrary (Bottom a) where
  arbitrary = fmap maybeToBottom arbitrary
  shrink x = map maybeToBottom (shrink (bottomToMaybe x))

bottomToMaybe :: Bottom a -> Maybe a
bottomToMaybe BottomUndefined = Nothing
bottomToMaybe (BottomValue a) = Just a

maybeToBottom :: Maybe a -> Bottom a
maybeToBottom Nothing = BottomUndefined
maybeToBottom (Just a) = BottomValue a

newtype Apply f a = Apply { getApply :: f a }

newtype Apply2 f a b = Apply2 { getApply2 :: f a b }

instance (Eq1 f, Eq a) => Eq (Apply f a) where
  Apply a == Apply b = eq1 a b

instance (Applicative f, Monoid a) => Semigroup (Apply f a) where
  Apply x <> Apply y = Apply $ liftA2 mappend x y

instance (Applicative f, Monoid a) => Monoid (Apply f a) where
  mempty = Apply $ pure mempty
  mappend = (SG.<>)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
instance (Eq2 f, Eq a, Eq b) => Eq (Apply2 f a b) where
  Apply2 a == Apply2 b = eq2 a b

instance (Show2 f, Show a, Show b) => Show (Apply2 f a b) where
  showsPrec p = showsPrec2 p . getApply2
#endif

instance (Arbitrary2 f, Arbitrary a, Arbitrary b) => Arbitrary (Apply2 f a b) where
  arbitrary = fmap Apply2 arbitrary2
  shrink = fmap Apply2 . shrink2 . getApply2


data LinearEquation = LinearEquation
  { _linearEquationLinear :: Integer
  , _linearEquationConstant :: Integer
  } deriving (Eq)

instance Show LinearEquation where
  showsPrec = showLinear
  showList = showLinearList

data LinearEquationM m = LinearEquationM (m LinearEquation) (m LinearEquation)

runLinearEquation :: LinearEquation -> Integer -> Integer
runLinearEquation (LinearEquation a b) x = a * x + b

runLinearEquationM :: Functor m => LinearEquationM m -> Integer -> m Integer
runLinearEquationM (LinearEquationM e1 e2) i = if odd i
  then fmap (flip runLinearEquation i) e1
  else fmap (flip runLinearEquation i) e2

instance Eq1 m => Eq (LinearEquationM m) where
  LinearEquationM a1 b1 == LinearEquationM a2 b2 = eq1 a1 a2 && eq1 b1 b2

showLinear :: Int -> LinearEquation -> ShowS
showLinear _ (LinearEquation a b) = shows a . showString " * x + " . shows b

showLinearList :: [LinearEquation] -> ShowS
showLinearList xs = SG.appEndo $ mconcat
   $ [SG.Endo (showChar '[')]
  ++ L.intersperse (SG.Endo (showChar ',')) (map (SG.Endo . showLinear 0) xs)
  ++ [SG.Endo (showChar ']')]

instance Show1 m => Show (LinearEquationM m) where
  show (LinearEquationM a b) = (\f -> f "")
    $ showString "\\x -> if odd x then "
    . showsPrec1 0 a
    . showString " else "
    . showsPrec1 0 b

instance Arbitrary1 m => Arbitrary (LinearEquationM m) where
  arbitrary = liftA2 LinearEquationM arbitrary1 arbitrary1
  shrink (LinearEquationM a b) = concat
    [ map (\x -> LinearEquationM x b) (shrink1 a)
    , map (\x -> LinearEquationM a x) (shrink1 b)
    ]

instance Arbitrary LinearEquation where
  arbitrary = do
    (a,b) <- arbitrary
    return (LinearEquation (abs a) (abs b))
  shrink (LinearEquation a b) =
    let xs = shrink (a,b)
     in map (\(x,y) -> LinearEquation (abs x) (abs y)) xs

-- this is a quadratic equation
data Equation = Equation Integer Integer Integer
  deriving (Eq)

-- This show instance is does not actually provide a
-- way to create an equation. Instead, it makes it look
-- like a lambda.
instance Show Equation where
  show (Equation a b c) = "\\x -> " ++ show a ++ " * x ^ 2 + " ++ show b ++ " * x + " ++ show c

instance Arbitrary Equation where
  arbitrary = do
    (a,b,c) <- arbitrary
    return (Equation (abs a) (abs b) (abs c))
  shrink (Equation a b c) =
    let xs = shrink (a,b,c)
     in map (\(x,y,z) -> Equation (abs x) (abs y) (abs z)) xs

runEquation :: Equation -> Integer -> Integer
runEquation (Equation a b c) x = a * x ^ (2 :: Integer) + b * x + c

-- linear equation of two variables
data EquationTwo = EquationTwo Integer Integer
  deriving (Eq)

-- This show instance does not actually provide a
-- way to create an EquationTwo. Instead, it makes it look
-- like a lambda that takes two variables.
instance Show EquationTwo where
  show (EquationTwo a b) = "\\x y -> " ++ show a ++ " * x + " ++ show b ++ " * y"

instance Arbitrary EquationTwo where
  arbitrary = do
    (a,b) <- arbitrary
    return (EquationTwo (abs a) (abs b))
  shrink (EquationTwo a b) =
    let xs = shrink (a,b)
     in map (\(x,y) -> EquationTwo (abs x) (abs y)) xs

runEquationTwo :: EquationTwo -> Integer -> Integer -> Integer
runEquationTwo (EquationTwo a b) x y = a * x + b * y

-- This show instance is intentionally a little bit wrong.
-- We don't wrap the result in Apply since the end user
-- should not be made aware of the Apply wrapper anyway.
instance (Show1 f, Show a) => Show (Apply f a) where
  showsPrec p = showsPrec1 p . getApply

instance (Arbitrary1 f, Arbitrary a) => Arbitrary (Apply f a) where
  arbitrary = fmap Apply arbitrary1
  shrink = map Apply . shrink1 . getApply

functorIdentity :: forall proxy f. (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
functorIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (fmap id a) a

func1 :: Integer -> (Integer,Integer)
func1 i = (div (i + 5) 3, i * i - 2 * i + 1)

func2 :: (Integer,Integer) -> (Bool,Either Ordering Integer)
func2 (a,b) = (odd a, if even a then Left (compare a b) else Right (b + 2))

functorComposition :: forall proxy f. (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
functorComposition _ = property $ \(Apply (a :: f Integer)) ->
  eq1 (fmap func2 (fmap func1 a)) (fmap (func2 . func1) a)

functorConst :: forall proxy f. (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
functorConst _ = property $ \(Apply (a :: f Integer)) ->
  eq1 (fmap (const 'X') a) ('X' <$ a)

alternativeIdentity :: forall proxy f. (Alternative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
alternativeIdentity _ = property $ \(Apply (a :: f Integer)) -> (eq1 (empty <|> a) a) && (eq1 a (empty <|> a))

alternativeAssociativity :: forall proxy f. (Alternative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
alternativeAssociativity _ = property $ \(Apply (a :: f Integer)) (Apply (b :: f Integer)) (Apply (c :: f Integer)) -> eq1 (a <|> (b <|> c)) ((a <|> b) <|> c)

monadPlusLeftIdentity :: forall proxy f. (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadPlusLeftIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (mplus mzero a) a

monadPlusRightIdentity :: forall proxy f. (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadPlusRightIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (mplus a mzero) a

monadPlusAssociativity :: forall proxy f. (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadPlusAssociativity _ = property $ \(Apply (a :: f Integer)) (Apply (b :: f Integer)) (Apply (c :: f Integer)) -> eq1 (mplus a (mplus b c)) (mplus (mplus a b) c)

monadPlusLeftZero :: forall proxy f. (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadPlusLeftZero _ = property $ \(k' :: LinearEquationM f) -> eq1 (mzero >>= runLinearEquationM k') mzero

monadPlusRightZero :: forall proxy f. (MonadPlus f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadPlusRightZero _ = property $ \(Apply (a :: f Integer)) -> eq1 (a >> (mzero :: f Integer)) mzero

applicativeIdentity :: forall proxy f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
applicativeIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (pure id <*> a) a

applicativeComposition :: forall proxy f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
applicativeComposition _ = property $ \(Apply (u' :: f Equation)) (Apply (v' :: f Equation)) (Apply (w :: f Integer)) ->
  let u = fmap runEquation u'
      v = fmap runEquation v'
   in eq1 (pure (.) <*> u <*> v <*> w) (u <*> (v <*> w))

applicativeHomomorphism :: forall proxy f. (Applicative f, Eq1 f, Show1 f) => proxy f -> Property
applicativeHomomorphism _ = property $ \(e :: Equation) (a :: Integer) ->
  let f = runEquation e
   in eq1 (pure f <*> pure a) (pure (f a) :: f Integer)

applicativeInterchange :: forall proxy f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
applicativeInterchange _ = property $ \(Apply (u' :: f Equation)) (y :: Integer) ->
  let u = fmap runEquation u'
   in eq1 (u <*> pure y) (pure ($ y) <*> u)

applicativeLiftA2_1 :: forall proxy f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
applicativeLiftA2_1 _ = property $ \(Apply (f' :: f Equation)) (Apply (x :: f Integer)) -> 
  let f = fmap runEquation f'
   in eq1 (liftA2 id f x) (f <*> x)

monadLeftIdentity :: forall proxy f. (Monad f, Functor f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadLeftIdentity _ = property $ \(k' :: LinearEquationM f) (a :: Integer) -> 
  let k = runLinearEquationM k'
   in eq1 (return a >>= k) (k a)

monadZipNaturality :: forall proxy f. (MonadZip f, Functor f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadZipNaturality _ = property $ \(f' :: LinearEquation) (g' :: LinearEquation) (Apply (ma :: f Integer)) (Apply (mb :: f Integer)) -> 
  let f = runLinearEquation f'
      g = runLinearEquation g'
   in eq1 (liftM (f *** g) (mzip ma mb)) (mzip (liftM f ma) (liftM g mb))

monadRightIdentity :: forall proxy f. (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadRightIdentity _ = property $ \(Apply (m :: f Integer)) -> 
  eq1 (m >>= return) m

monadAssociativity :: forall proxy f. (Monad f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadAssociativity _ = property $ \(Apply (m :: f Integer)) (k' :: LinearEquationM f) (h' :: LinearEquationM f) -> 
  let k = runLinearEquationM k'
      h = runLinearEquationM h'
   in eq1 (m >>= (\x -> k x >>= h)) ((m >>= k) >>= h)

monadReturn :: forall proxy f. (Monad f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadReturn _ = property $ \(x :: Integer) ->
  eq1 (return x) (pure x :: f Integer)

monadAp :: forall proxy f. (Monad f, Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => proxy f -> Property
monadAp _ = property $ \(Apply (f' :: f Equation)) (Apply (x :: f Integer)) -> 
  let f = fmap runEquation f'
   in eq1 (ap f x) (f <*> x)
#endif

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
-- | Tests the following 'Bifunctor' properties:
--
-- [/Identity/]
--   @'bimap' 'id' 'id' ≡ 'id'@
-- [/First Identity/]
--   @'first' 'id' ≡ 'id'@
-- [/Second Identity/] 
--   @'second' 'id' ≡ 'id'@
-- [/Bifunctor Composition/]
--   @'bimap' f g ≡ 'first' f . 'second' g@ 
--
-- /Note/: This property test is only available when this package is built with
-- @base-4.9+@ or @transformers-0.5+@.
bifunctorLaws :: (Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f) => proxy f -> Laws
bifunctorLaws p = Laws "Bifunctor"
  [ ("Identity", bifunctorIdentity p)
  , ("First Identity", bifunctorFirstIdentity p)
  , ("Second Identity", bifunctorSecondIdentity p)
  , ("Bifunctor Composition", bifunctorComposition p)
  ]

bifunctorIdentity :: forall proxy f. (Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f) => proxy f -> Property
bifunctorIdentity _ = property $ \(Apply2 (x :: f Integer Integer)) -> eq2 (bimap id id x) x

bifunctorFirstIdentity :: forall proxy f. (Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f) => proxy f -> Property
bifunctorFirstIdentity _ = property $ \(Apply2 (x :: f Integer Integer)) -> eq2 (first id x) x

bifunctorSecondIdentity :: forall proxy f. (Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f) => proxy f -> Property
bifunctorSecondIdentity _ = property $ \(Apply2 (x :: f Integer Integer)) -> eq2 (second id x) x

bifunctorComposition
  :: forall proxy f.
     (Bifunctor f, Eq2 f, Show2 f, Arbitrary2 f)
  => proxy f -> Property
bifunctorComposition _ = property $ \(Apply2 (z :: f Integer Integer)) -> eq2 (bimap id id z) ((first id . second id) z)
#endif

#endif

myForAllShrink :: (Arbitrary a, Show b, Eq b) => Bool -> (a -> Bool) -> (a -> [String]) -> String -> (a -> b) -> String -> (a -> b) -> Property
myForAllShrink displayRhs isValid showInputs name1 calc1 name2 calc2 =
  again $
  MkProperty $
  arbitrary >>= \x ->
    unProperty $
    shrinking shrink x $ \x' ->
      let b1 = calc1 x'
          b2 = calc2 x'
          sb1 = show b1
          sb2 = show b2
          description = "  Description: " ++ name1 ++ " = " ++ name2
          err = description ++ "\n" ++ unlines (map ("  " ++) (showInputs x')) ++ "  " ++ name1 ++ " = " ++ sb1 ++ (if displayRhs then "\n  " ++ name2 ++ " = " ++ sb2 else "")
       in isValid x' ==> counterexample err (b1 == b2)

#if MIN_VERSION_base(4,7,0)
newtype BitIndex a = BitIndex Int

instance FiniteBits a => Arbitrary (BitIndex a) where
  arbitrary = let n = finiteBitSize (undefined :: a) in if n > 0
    then fmap BitIndex (choose (0,n - 1))
    else return (BitIndex 0)
  shrink (BitIndex x) = if x > 0 then map BitIndex (S.toList (S.fromList [x - 1, div x 2, 0])) else []
#endif

-- byte array with phantom variable that specifies element type
data PrimArray a = PrimArray ByteArray#
data MutablePrimArray s a = MutablePrimArray (MutableByteArray# s)

instance (Eq a, Prim a) => Eq (PrimArray a) where
  a1 == a2 = sizeofPrimArray a1 == sizeofPrimArray a2 && loop (sizeofPrimArray a1 - 1)
    where 
    loop !i | i < 0 = True
            | otherwise = indexPrimArray a1 i == indexPrimArray a2 i && loop (i-1)

#if MIN_VERSION_base(4,7,0)
instance Prim a => IsList (PrimArray a) where
  type Item (PrimArray a) = a
  fromList = primArrayFromList
  fromListN = primArrayFromListN
  toList = primArrayToList
#endif

indexPrimArray :: forall a. Prim a => PrimArray a -> Int -> a
indexPrimArray (PrimArray arr#) (I# i#) = indexByteArray# arr# i#

sizeofPrimArray :: forall a. Prim a => PrimArray a -> Int
sizeofPrimArray (PrimArray arr#) = I# (quotInt# (sizeofByteArray# arr#) (sizeOf# (undefined :: a)))

newPrimArray :: forall m a. (PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
newPrimArray (I# n#)
  = primitive (\s# -> 
      case newByteArray# (n# *# sizeOf# (undefined :: a)) s# of
        (# s'#, arr# #) -> (# s'#, MutablePrimArray arr# #)
    )

readPrimArray :: (Prim a, PrimMonad m) => MutablePrimArray (PrimState m) a -> Int -> m a
readPrimArray (MutablePrimArray arr#) (I# i#)
  = primitive (readByteArray# arr# i#)

writePrimArray ::
     (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> Int
  -> a
  -> m ()
writePrimArray (MutablePrimArray arr#) (I# i#) x
  = primitive_ (writeByteArray# arr# i# x)

unsafeFreezePrimArray
  :: PrimMonad m => MutablePrimArray (PrimState m) a -> m (PrimArray a)
unsafeFreezePrimArray (MutablePrimArray arr#)
  = primitive (\s# -> case unsafeFreezeByteArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, PrimArray arr'# #))


copyPrimArrayToPtr :: forall m a. (PrimMonad m, Prim a)
  => Ptr a       -- ^ destination pointer
  -> PrimArray a -- ^ source array
  -> Int         -- ^ offset into source array
  -> Int         -- ^ number of prims to copy
  -> m ()
copyPrimArrayToPtr addr@(Ptr addr#) ba@(PrimArray ba#) soff@(I# soff#) n@(I# n#) =
#if MIN_VERSION_base(4,7,0)
  primitive (\ s# ->
      let s'# = copyByteArrayToAddr# ba# (soff# *# siz#) addr# (n# *# siz#) s#
      in (# s'#, () #))
  where siz# = sizeOf# (undefined :: a)
#else
  generateM_ n $ \ix -> writeOffAddr (ptrToAddr addr) ix (indexPrimArray ba (ix + soff))
#endif

ptrToAddr :: Ptr a -> Addr
ptrToAddr (Ptr x) = Addr x

generateM_ :: Monad m => Int -> (Int -> m a) -> m ()
generateM_ n f = go 0 where
  go !ix = if ix < n
    then f ix >> go (ix + 1)
    else return ()

copyPtrToMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a
  -> Int
  -> Ptr a
  -> Int
  -> m ()
copyPtrToMutablePrimArray ba@(MutablePrimArray ba#) doff@(I# doff#) addr@(Ptr addr#) n@(I# n#) = 
#if MIN_VERSION_base(4,7,0)
  primitive (\ s# ->
      let s'# = copyAddrToByteArray# addr# ba# (doff# *# siz#) (n# *# siz#) s#
      in (# s'#, () #))
  where siz# = sizeOf# (undefined :: a)
#else
  generateM_ n $ \ix -> do
    x <- readOffAddr (ptrToAddr addr) ix
    writePrimArray ba (doff + ix) x
#endif

copyMutablePrimArray :: forall m a.
     (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> MutablePrimArray (PrimState m) a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of bytes to copy
  -> m ()
copyMutablePrimArray (MutablePrimArray dst#) (I# doff#) (MutablePrimArray src#) (I# soff#) (I# n#)
  = primitive_ (copyMutableByteArray#
      src# 
      (soff# *# (sizeOf# (undefined :: a)))
      dst#
      (doff# *# (sizeOf# (undefined :: a)))
      (n# *# (sizeOf# (undefined :: a)))
    )

copyPrimArray :: forall m a.
     (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a -- ^ destination array
  -> Int -- ^ offset into destination array
  -> PrimArray a -- ^ source array
  -> Int -- ^ offset into source array
  -> Int -- ^ number of bytes to copy
  -> m ()
copyPrimArray (MutablePrimArray dst#) (I# doff#) (PrimArray src#) (I# soff#) (I# n#)
  = primitive_ (copyByteArray#
      src# 
      (soff# *# (sizeOf# (undefined :: a)))
      dst#
      (doff# *# (sizeOf# (undefined :: a)))
      (n# *# (sizeOf# (undefined :: a)))
    )

primArrayFromList :: Prim a => [a] -> PrimArray a
primArrayFromList xs = primArrayFromListN (L.length xs) xs

primArrayFromListN :: forall a. Prim a => Int -> [a] -> PrimArray a
primArrayFromListN len vs = runST run where
  run :: forall s. ST s (PrimArray a)
  run = do
    arr <- newPrimArray len
    let go :: [a] -> Int -> ST s ()
        go !xs !ix = case xs of
          [] -> return ()
          a : as -> do
            writePrimArray arr ix a
            go as (ix + 1)
    go vs 0
    unsafeFreezePrimArray arr

primArrayToList :: forall a. Prim a => PrimArray a -> [a]
primArrayToList arr = go 0 where
  !len = sizeofPrimArray arr
  go :: Int -> [a]
  go !ix = if ix < len
    then indexPrimArray arr ix : go (ix + 1)
    else []

