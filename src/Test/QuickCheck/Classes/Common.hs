{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Common
  ( Laws(..)
  , foldMapA 
  , myForAllShrink 
  
  -- only used for higher-kinded types
  , Apply(..)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
  , Apply2(..)
#endif
  , Triple(..)
  , ChooseFirst(..)
  , ChooseSecond(..)
  , LastNothing(..)
  , Bottom(..)
  , LinearEquation(..)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
  , LinearEquationM(..)
#endif
  , QuadraticEquation(..)
  , LinearEquationTwo(..)
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
  , nestedEq1
  , propNestedEq1
  , toSpecialApplicative
#endif
  , flipPair
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
  , apTrans
#endif
  , func1
  , func2
  , func3
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
  , func4
#endif
  , func5
  , func6
  , reverseTriple
  , runLinearEquation
#if MIN_VERSION_base(4,8,0) || MIN_VERSION_transformers(0,5,0)
  , runLinearEquationM
#endif
  , runQuadraticEquation
  , runLinearEquationTwo
  ) where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Traversable
import Data.Monoid
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
import Data.Functor.Classes
import Data.Functor.Compose
#endif
import Data.Semigroup (Semigroup)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property(..))

import qualified Control.Monad.Trans.Writer.Lazy as WL
import qualified Data.List as L
import qualified Data.Monoid as MND
import qualified Data.Semigroup as SG
import qualified Data.Set as S

-- | A set of laws associated with a typeclass.
data Laws = Laws
  { lawsTypeclass :: String
    -- ^ Name of the typeclass whose laws are tested
  , lawsProperties :: [(String,Property)]
    -- ^ Pairs of law name and property
  }

myForAllShrink :: (Arbitrary a, Show b, Eq b)
  => Bool -- Should we show the RHS. It's better not to show it
          -- if the RHS is equal to the input.
  -> (a -> Bool) -- is the value a valid input
  -> (a -> [String])
  -> String
  -> (a -> b)
  -> String
  -> (a -> b)
  -> Property
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

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
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
#endif

flipPair :: (a,b) -> (b,a)
flipPair (x,y) = (y,x)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
-- Reverse the list and accumulate the writers. We cannot
-- use Sum or Product or else it wont actually be a valid
-- applicative transformation.
apTrans ::
     Compose Triple (WL.Writer (S.Set Integer)) a
  -> Compose (WL.Writer (S.Set Integer)) Triple a
apTrans (Compose xs) = Compose (sequenceA (reverseTriple xs))
#endif

func1 :: Integer -> (Integer,Integer)
func1 i = (div (i + 5) 3, i * i - 2 * i + 1)

func2 :: (Integer,Integer) -> (Bool,Either Ordering Integer)
func2 (a,b) = (odd a, if even a then Left (compare a b) else Right (b + 2))

func3 :: Integer -> SG.Sum Integer
func3 i = SG.Sum (3 * i * i - 7 * i + 4)

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
func4 :: Integer -> Compose Triple (WL.Writer (S.Set Integer)) Integer
func4 i = Compose $ Triple
  (WL.writer (i * i, S.singleton (i * 7 + 5)))
  (WL.writer (i + 2, S.singleton (i * i + 3)))
  (WL.writer (i * 7, S.singleton 4))
#endif

func5 :: Integer -> Triple Integer
func5 i = Triple (i + 2) (i * 3) (i * i)

func6 :: Integer -> Triple Integer
func6 i = Triple (i * i * i) (4 * i - 7) (i * i * i)

data Triple a = Triple a a a
  deriving (Show,Eq)

tripleLiftEq :: (a -> b -> Bool) -> Triple a -> Triple b -> Bool
tripleLiftEq p (Triple a1 b1 c1) (Triple a2 b2 c2) =
  p a1 a2 && p b1 b2 && p c1 c2

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
instance Eq1 Triple where
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
  liftEq = tripleLiftEq
#else
  eq1 = tripleLiftEq (==)
#endif
#endif

tripleLiftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Triple a -> ShowS
tripleLiftShowsPrec elemShowsPrec _ p (Triple a b c) = showParen (p > 10)
  $ showString "Triple "
  . elemShowsPrec 11 a
  . showString " "
  . elemShowsPrec 11 b
  . showString " "
  . elemShowsPrec 11 c

#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
instance Show1 Triple where
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
  liftShowsPrec = tripleLiftShowsPrec
#else
  showsPrec1 = tripleLiftShowsPrec showsPrec showList
#endif
#endif

#if MIN_VERSION_QuickCheck(2,10,0)
instance Arbitrary1 Triple where
  liftArbitrary x = Triple <$> x <*> x <*> x

instance Arbitrary a => Arbitrary (Triple a) where
  arbitrary = liftArbitrary arbitrary
#else
instance Arbitrary a => Arbitrary (Triple a) where
  arbitrary = Triple <$> arbitrary <*> arbitrary <*> arbitrary
#endif

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

instance (Applicative f, Monoid a) => Semigroup (Apply f a) where
  Apply x <> Apply y = Apply $ liftA2 mappend x y

instance (Applicative f, Monoid a) => Monoid (Apply f a) where
  mempty = Apply $ pure mempty
  mappend = (SG.<>)

#if MIN_VERSION_base(4,8,0) || MIN_VERSION_transformers(0,5,0)
instance (Eq1 f, Eq a) => Eq (Apply f a) where
  Apply a == Apply b = eq1 a b

-- This show instance is intentionally a little bit wrong.
-- We don't wrap the result in Apply since the end user
-- should not be made aware of the Apply wrapper anyway.
instance (Show1 f, Show a) => Show (Apply f a) where
  showsPrec p = showsPrec1 p . getApply

#if MIN_VERSION_QuickCheck(2,10,0)
instance (Arbitrary1 f, Arbitrary a) => Arbitrary (Apply f a) where
  arbitrary = fmap Apply arbitrary1
  shrink = map Apply . shrink1 . getApply
#endif
#endif

foldMapA :: (Foldable t, Monoid m, Semigroup m, Applicative f) => (a -> f m) -> t a -> f m
foldMapA f = getApply . foldMap (Apply . f)


#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,5,0)
newtype Apply2 f a b = Apply2 { getApply2 :: f a b }

instance (Eq2 f, Eq a, Eq b) => Eq (Apply2 f a b) where
  Apply2 a == Apply2 b = eq2 a b

instance (Show2 f, Show a, Show b) => Show (Apply2 f a b) where
  showsPrec p = showsPrec2 p . getApply2

#if MIN_VERSION_QuickCheck(2,10,0)
instance (Arbitrary2 f, Arbitrary a, Arbitrary b) => Arbitrary (Apply2 f a b) where
  arbitrary = fmap Apply2 arbitrary2
  shrink = fmap Apply2 . shrink2 . getApply2
#endif
#endif

data LinearEquation = LinearEquation
  { _linearEquationLinear :: Integer
  , _linearEquationConstant :: Integer
  } deriving (Eq)

instance Show LinearEquation where
  showsPrec = showLinear
  showList = showLinearList

runLinearEquation :: LinearEquation -> Integer -> Integer
runLinearEquation (LinearEquation a b) x = a * x + b

showLinear :: Int -> LinearEquation -> ShowS
showLinear _ (LinearEquation a b) = shows a . showString " * x + " . shows b

showLinearList :: [LinearEquation] -> ShowS
showLinearList xs = SG.appEndo $ mconcat
   $ [SG.Endo (showChar '[')]
  ++ L.intersperse (SG.Endo (showChar ',')) (map (SG.Endo . showLinear 0) xs)
  ++ [SG.Endo (showChar ']')]

#if MIN_VERSION_base(4,8,0) || MIN_VERSION_transformers(0,5,0)
data LinearEquationM m = LinearEquationM (m LinearEquation) (m LinearEquation)

runLinearEquationM :: Monad m => LinearEquationM m -> Integer -> m Integer
runLinearEquationM (LinearEquationM e1 e2) i = if odd i
  then liftM (flip runLinearEquation i) e1
  else liftM (flip runLinearEquation i) e2

instance Eq1 m => Eq (LinearEquationM m) where
  LinearEquationM a1 b1 == LinearEquationM a2 b2 = eq1 a1 a2 && eq1 b1 b2

instance Show1 m => Show (LinearEquationM m) where
  show (LinearEquationM a b) = (\f -> f "")
    $ showString "\\x -> if odd x then "
    . showsPrec1 0 a
    . showString " else "
    . showsPrec1 0 b

#if MIN_VERSION_QuickCheck(2,10,0)
instance Arbitrary1 m => Arbitrary (LinearEquationM m) where
  arbitrary = liftA2 LinearEquationM arbitrary1 arbitrary1
  shrink (LinearEquationM a b) = L.concat
    [ map (\x -> LinearEquationM x b) (shrink1 a)
    , map (\x -> LinearEquationM a x) (shrink1 b)
    ]
#endif
#endif

instance Arbitrary LinearEquation where
  arbitrary = do
    (a,b) <- arbitrary
    return (LinearEquation (abs a) (abs b))
  shrink (LinearEquation a b) =
    let xs = shrink (a,b)
     in map (\(x,y) -> LinearEquation (abs x) (abs y)) xs

-- this is a quadratic equation
data QuadraticEquation = QuadraticEquation
  { _quadraticEquationQuadratic :: Integer
  , _quadraticEquationLinear :: Integer
  , _quadraticEquationConstant :: Integer
  }
  deriving (Eq)

-- This show instance is does not actually provide a
-- way to create an equation. Instead, it makes it look
-- like a lambda.
instance Show QuadraticEquation where
  show (QuadraticEquation a b c) = "\\x -> " ++ show a ++ " * x ^ 2 + " ++ show b ++ " * x + " ++ show c

instance Arbitrary QuadraticEquation where
  arbitrary = do
    (a,b,c) <- arbitrary
    return (QuadraticEquation (abs a) (abs b) (abs c))
  shrink (QuadraticEquation a b c) =
    let xs = shrink (a,b,c)
     in map (\(x,y,z) -> QuadraticEquation (abs x) (abs y) (abs z)) xs

runQuadraticEquation :: QuadraticEquation -> Integer -> Integer
runQuadraticEquation (QuadraticEquation a b c) x = a * x ^ (2 :: Integer) + b * x + c

data LinearEquationTwo = LinearEquationTwo
  { _linearEquationTwoX :: Integer
  , _linearEquationTwoY :: Integer
  }
  deriving (Eq)

-- This show instance does not actually provide a
-- way to create a LinearEquationTwo. Instead, it makes it look
-- like a lambda that takes two variables.
instance Show LinearEquationTwo where
  show (LinearEquationTwo a b) = "\\x y -> " ++ show a ++ " * x + " ++ show b ++ " * y"

instance Arbitrary LinearEquationTwo where
  arbitrary = do
    (a,b) <- arbitrary
    return (LinearEquationTwo (abs a) (abs b))
  shrink (LinearEquationTwo a b) =
    let xs = shrink (a,b)
     in map (\(x,y) -> LinearEquationTwo (abs x) (abs y)) xs

runLinearEquationTwo :: LinearEquationTwo -> Integer -> Integer -> Integer
runLinearEquationTwo (LinearEquationTwo a b) x y = a * x + b * y
