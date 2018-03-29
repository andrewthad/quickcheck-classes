{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Common
  ( Laws(..)
  , myForAllShrink
  
  -- only used for higher-kinded types
  , Apply(..)
  , Apply2(..)
  , Triple(..)
  , ChooseFirst(..)
  , ChooseSecond(..)
  , LastNothing(..)
  , Bottom(..)
  , LinearEquation(..)
  , LinearEquationM(..)
  , Equation(..)
  , EquationTwo(..)
  , nestedEq1
  , propNestedEq1
  , toSpecialApplicative
  , flipPair
  , apTrans
  , func3
  , func4
  , func5
  , func6
  , reverseTriple
  , runLinearEquation
  , runLinearEquationM
  , runEquation
  , runEquationTwo
  ) where

import Control.Applicative (liftA2)
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
tripleLiftShowsPrec elemShowsPrec _ p (Triple a b c) = showParen (p > 10)
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
