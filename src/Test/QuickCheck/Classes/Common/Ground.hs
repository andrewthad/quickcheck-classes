{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Common.Ground
  ( Laws(..)
  , lawsCheck
  , lawsCheckMany
  , myForAllShrink
  ) where

import Control.Applicative (liftA2)
import Data.Semigroup (Semigroup)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Property (Property(..))

import qualified Data.Semigroup as SG

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
  Bad  <> _ = Bad

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

