{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.QuickCheck.Classes
  ( primProps
  , storableProps
  , semigroupProps
  , monoidProps
  , communativeMonoidProps
  , showReadProps
  , jsonProps
  , eqProps
#if MIN_VERSION_QuickCheck(2,10,0)
  , functorProps
  , applicativeProps
  , monadProps
#endif
  ) where

import Test.QuickCheck
import Data.Primitive hiding (sizeOf,newArray,copyArray)
import Data.Primitive.PrimArray
import Data.Proxy
import Control.Monad.ST
import Control.Monad
import Data.Monoid (Endo(..))
import GHC.Ptr (Ptr(..))
import Data.Primitive.Addr (Addr(..))
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import Data.Semigroup (Semigroup)
import GHC.Exts (fromList,toList)
import Foreign.Marshal.Array
import Foreign.Storable
import Text.Read (readMaybe)
import Data.Aeson (FromJSON(..),ToJSON(..))
import Data.Functor.Classes
import Control.Applicative
import qualified Data.Aeson as AE
import qualified Data.Primitive as P
import qualified Data.Semigroup as SG
import qualified GHC.OldList as L

#if MIN_VERSION_QuickCheck(2,10,0)
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
#endif

jsonProps :: (ToJSON a, FromJSON a, Show a, Arbitrary a, Eq a) => Proxy a -> [(String,Property)]
jsonProps p =
  [ ("Encoding Equals Value", jsonEncodingEqualsValue p)
  , ("Partial Isomorphism", jsonEncodingPartialIsomorphism p)
  ]

showReadProps :: (Show a, Read a, Eq a, Arbitrary a) => Proxy a -> [(String,Property)]
showReadProps p =
  [ ("Partial Isomorphism", showReadPartialIsomorphism p)
  ]

-- | Tests the following properties:
--
-- [/Associative/]
--   @a <> (b <> c) ≡ (a <> b) <> c@
semigroupProps :: (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> [(String,Property)]
semigroupProps p =
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
eqProps :: (Eq a, Arbitrary a, Show a) => Proxy a -> [(String,Property)]
eqProps p =
  [ ("Transitive", eqTransitive p)
  , ("Symmetric", eqSymmetric p)
  , ("Reflexive", eqReflexive p)
  ]

-- | Tests the following properties:
--
-- [/Associative/]
--   @mappend a (mappend b c) ≡ mappend (mappend a b) c@
-- [/Left Identity/]
--   @mappend mempty a ≡ a@
-- [/Right Identity/]
--   @mappend a mempty ≡ a@
monoidProps :: (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> [(String,Property)]
monoidProps p =
  [ ("Associative", monoidAssociative p)
  , ("Left Identity", monoidLeftIdentity p)
  , ("Right Identity", monoidRightIdentity p)
  ]

-- | Tests everything from 'monoidProps' plus the following:
--
-- [/Commutative/]
--   @mappend a b ≡ mappend b a@
communativeMonoidProps :: (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> [(String,Property)]
communativeMonoidProps p = monoidProps p ++
  [ ("Commutative", monoidCommutative p)
  ]

primProps :: (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> [(String,Property)]
primProps p =
  [ ("ByteArray Set-Get (you get back what you put in)", primSetGetByteArray p)
  , ("ByteArray Get-Set (putting back what you got out has no effect)", primGetSetByteArray p)
  , ("ByteArray Set-Set (setting twice is same as setting once)", primSetSetByteArray p)
  , ("ByteArray List Conversion Roundtrips", primListByteArray p)
  , ("Addr Set-Get (you get back what you put in)", primSetGetAddr p)
  , ("Addr Get-Set (putting back what you got out has no effect)", primGetSetAddr p)
  , ("Addr List Conversion Roundtrips", primListAddr p)
  ]

storableProps :: (Storable a, Eq a, Arbitrary a, Show a) => Proxy a -> [(String,Property)]
storableProps p =
  [ ("Set-Get (you get back what you put in)", storableSetGet p)
  , ("Get-Set (putting back what you got out has no effect)", storableGetSet p)
  , ("List Conversion Roundtrips", storableList p)
  ]

showReadPartialIsomorphism :: forall a. (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showReadPartialIsomorphism _ = property $ \(a :: a) ->
  readMaybe (show a) == Just a

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

eqTransitive :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqTransitive _ = property $ \(a :: a) b c -> case a == b of
  True -> case b == c of
    True -> a == c
    False -> a /= c
  False -> case b == c of
    True -> a /= c
    False -> True

eqSymmetric :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqSymmetric _ = property $ \(a :: a) b -> case a == b of
  True -> b == a
  False -> b /= a

eqReflexive :: forall a. (Show a, Eq a, Arbitrary a) => Proxy a -> Property
eqReflexive _ = property $ \(a :: a) -> a == a

semigroupAssociative :: forall a. (Semigroup a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
semigroupAssociative _ = property $ \(a :: a) b c -> a SG.<> (b SG.<> c) == (a SG.<> b) SG.<> c

monoidAssociative :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidAssociative _ = property $ \(a :: a) b c -> mappend a (mappend b c) == mappend (mappend a b) c

monoidLeftIdentity :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidLeftIdentity _ = property $ \(a :: a) -> mappend mempty a == a

monoidRightIdentity :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidRightIdentity _ = property $ \(a :: a) -> mappend a mempty == a

monoidCommutative :: forall a. (Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
monoidCommutative _ = property $ \(a :: a) b -> mappend a b == mappend b a

primListByteArray :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> Property
primListByteArray _ = property $ \(as :: [a]) ->
  as == toList (fromList as :: PrimArray a)

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
  let arr1 = fromList as :: PrimArray a
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
  let arr1 = fromList as :: PrimArray a
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
  let arr1 = fromList as :: PrimArray a
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
-- | Tests the following applicative properties:
--
-- [/Identity/]
--   @'fmap' 'id' ≡ 'id'@
-- [/Composition/]
--   @fmap (f . g) ≡ 'fmap' f . 'fmap' g@
-- [/Const/]
--   @(<$) ≡ 'fmap' 'const'@
functorProps :: (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> [(String,Property)]
functorProps p =
  [ ("Identity", functorIdentity p)
  , ("Composition", functorComposition p)
  , ("Const", functorConst p)
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
applicativeProps :: (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> [(String,Property)]
applicativeProps p =
  [ ("Identity", applicativeIdentity p)
  , ("Composition", applicativeComposition p)
  , ("Homomorphism", applicativeHomomorphism p)
  , ("Interchange", applicativeInterchange p)
  , ("LiftA2 Part 1", applicativeLiftA2_1 p)
    -- todo: liftA2 part 2, we need an equation of two variables for this
  ]


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
monadProps :: (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> [(String,Property)]
monadProps p =
  [ ("Left Identity", monadLeftIdentity p)
  , ("Right Identity", monadRightIdentity p)
  , ("Associativity", monadAssociativity p)
  , ("Return", monadReturn p)
  , ("Ap", monadAp p)
  ]

data Apply f a = Apply { getApply :: f a }

instance (Eq1 f, Eq a) => Eq (Apply f a) where
  Apply a == Apply b = eq1 a b

data LinearEquation = LinearEquation
  { linearEquationLinear :: Integer
  , linearEquationConstant :: Integer
  } deriving (Eq)

data LinearEquationM m = LinearEquationM (m LinearEquation) (m LinearEquation)

runLinearEquation :: Integer -> LinearEquation -> Integer
runLinearEquation x (LinearEquation a b) = a * x + b

runLinearEquationM :: Functor m => LinearEquationM m -> Integer -> m Integer
runLinearEquationM (LinearEquationM e1 e2) i = if odd i
  then fmap (runLinearEquation i) e1
  else fmap (runLinearEquation i) e2

instance Eq1 m => Eq (LinearEquationM m) where
  LinearEquationM a1 b1 == LinearEquationM a2 b2 = eq1 a1 a2 && eq1 b1 b2

showLinear :: Int -> LinearEquation -> ShowS
showLinear _ (LinearEquation a b) = shows a . showString " * x + " . shows b

showLinearList :: [LinearEquation] -> ShowS
showLinearList xs = appEndo $ mconcat
   $ [Endo (showChar '[')]
  ++ L.intersperse (Endo (showChar ',')) (map (Endo . showLinear 0) xs)
  ++ [Endo (showChar ']')]

instance Show1 m => Show (LinearEquationM m) where
  show (LinearEquationM a b) = (\f -> f "")
    $ showString "\\x -> if odd x then "
    . liftShowsPrec showLinear showLinearList 0 a
    . showString " else "
    . liftShowsPrec showLinear showLinearList 0 b

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
runEquation (Equation a b c) x = a * x ^ 2 + b * x + c

-- This show instance is intentionally a little bit wrong.
-- We don't wrap the result in Apply since the end user
-- should not be made aware of the Apply wrapper anyway.
instance (Show1 f, Show a) => Show (Apply f a) where
  showsPrec p = showsPrec1 p . getApply

instance (Arbitrary1 f, Arbitrary a) => Arbitrary (Apply f a) where
  arbitrary = fmap Apply arbitrary1
  shrink = map Apply . shrink1 . getApply

functorIdentity :: forall f. (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
functorIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (fmap id a) a

func1 :: Integer -> (Integer,Integer)
func1 i = (div (i + 5) 3, i * i - 2 * i + 1)

func2 :: (Integer,Integer) -> (Bool,Either Ordering Integer)
func2 (a,b) = (odd a, if even a then Left (compare a b) else Right (b + 2))

functorComposition :: forall f. (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
functorComposition _ = property $ \(Apply (a :: f Integer)) ->
  eq1 (fmap func2 (fmap func1 a)) (fmap (func2 . func1) a)

functorConst :: forall f. (Functor f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
functorConst _ = property $ \(Apply (a :: f Integer)) ->
  eq1 (fmap (const 'X') a) ('X' <$ a)

applicativeIdentity :: forall f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
applicativeIdentity _ = property $ \(Apply (a :: f Integer)) -> eq1 (pure id <*> a) a

applicativeComposition :: forall f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
applicativeComposition _ = property $ \(Apply (u' :: f Equation)) (Apply (v' :: f Equation)) (Apply (w :: f Integer)) ->
  let u = fmap runEquation u'
      v = fmap runEquation v'
   in eq1 (pure (.) <*> u <*> v <*> w) (u <*> (v <*> w))

applicativeHomomorphism :: forall f. (Applicative f, Eq1 f, Show1 f) => Proxy f -> Property
applicativeHomomorphism _ = property $ \(e :: Equation) (a :: Integer) ->
  let f = runEquation e
   in eq1 (pure f <*> pure a) (pure (f a) :: f Integer)

applicativeInterchange :: forall f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
applicativeInterchange _ = property $ \(Apply (u' :: f Equation)) (y :: Integer) ->
  let u = fmap runEquation u'
   in eq1 (u <*> pure y) (pure ($ y) <*> u)

applicativeLiftA2_1 :: forall f. (Applicative f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
applicativeLiftA2_1 _ = property $ \(Apply (f' :: f Equation)) (Apply (x :: f Integer)) -> 
  let f = fmap runEquation f'
   in eq1 (liftA2 id f x) (f <*> x)

monadLeftIdentity :: forall f. (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
monadLeftIdentity _ = property $ \(k' :: LinearEquationM f) (a :: Integer) -> 
  let k = runLinearEquationM k'
   in eq1 (return a >>= k) (k a)

monadRightIdentity :: forall f. (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
monadRightIdentity _ = property $ \(Apply (m :: f Integer)) -> 
  eq1 (m >>= return) m

monadAssociativity :: forall f. (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
monadAssociativity _ = property $ \(Apply (m :: f Integer)) (k' :: LinearEquationM f) (h' :: LinearEquationM f) -> 
  let k = runLinearEquationM k'
      h = runLinearEquationM h'
   in eq1 (m >>= (\x -> k x >>= h)) ((m >>= k) >>= h)

monadReturn :: forall f. (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
monadReturn _ = property $ \(x :: Integer) ->
  eq1 (return x) (pure x :: f Integer)

monadAp :: forall f. (Monad f, Eq1 f, Show1 f, Arbitrary1 f) => Proxy f -> Property
monadAp _ = property $ \(Apply (f' :: f Equation)) (Apply (x :: f Integer)) -> 
  let f = fmap runEquation f'
   in eq1 (ap f x) (f <*> x)

#endif
