{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

{-| Module      : Test.QuickCheck.Classes.ShowRead
    Description : Properties for testing the interaction between the Show and Read
                  type classes.
-}
module Test.QuickCheck.Classes.ShowRead
  ( showReadLaws
  , showReadExpressionLaws
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Char (chr, isSpace)
import Data.Proxy (Proxy)
import Test.QuickCheck
import Text.Read (readListDefault)
import Text.Show (showListWith)

import Test.QuickCheck.Classes.Common (Laws(..), ShowReadPrecedence(..),
  SmallList(..), myForAllShrink)
import Test.QuickCheck.Classes.Compat (readMaybe)

-- | Tests the following properties:
--
-- [/Partial Isomorphism: 'show' \/ 'read'/]
--   @'readMaybe' ('show' a) ≡ 'Just' a@
-- [/Partial Isomorphism: 'show' \/ 'read' with initial space/]
--   @'readMaybe' (" " ++ 'show' a) ≡ 'Just' a@
-- [/Partial Isomorphism: 'show' \/ 'read' in context/]
--   @'readMaybe' (" " ++ 'show' __E__[a]) ≡ 'Just' __E__[a]@
-- [/Partial Isomorphism: 'showsPrec' \/ 'readsPrec'/]
--   @(a,"") \`elem\` 'readsPrec' p ('showsPrec' p a "")@
-- [/Partial Isomorphism: 'showList' \/ 'readList'/]
--   @(as,"") \`elem\` 'readList' ('showList' as "")@
-- [/Partial Isomorphism: 'showListWith' 'shows' \/ 'readListDefault'/]
--   @(as,"") \`elem\` 'readListDefault' ('showListWith' 'shows' as "")@
--
-- /Note:/ When using @base-4.5@ or older, a shim implementation
-- of 'readMaybe' is used.
--
showReadLaws :: (Show a, Read a, Eq a, Arbitrary a) => Proxy a -> Laws
showReadLaws p = Laws "Show/Read"
  [ ("Partial Isomorphism: show/read", showReadPartialIsomorphism p)
  , ("Partial Isomorphism: show/read with initial space", showReadSpacePartialIsomorphism p)
  , ("Partial Isomorphism: show/read with context", showReadCtxPartialIsomorphism p)
  , ("Partial Isomorphism: showsPrec/readsPrec", showsPrecReadsPrecPartialIsomorphism p)
  , ("Partial Isomorphism: showList/readList", showListReadListPartialIsomorphism p)
  , ("Partial Isomorphism: showListWith shows / readListDefault",
     showListWithShowsReadListDefaultPartialIsomorphism p)
  ]

data Ctx a
  = CtxList [a]
  | CtxTuple (a,a)
  | CtxRecord { field :: a }
  | CtxContList a [()]
  | CtxContInt a Int
  | CtxContBool a Bool
  | CtxContUnit a ()
  | CtxContChar a Char
  | CtxContString a String
  deriving (Eq,Show,Read)

instance Arbitrary a => Arbitrary (Ctx a) where
  arbitrary = oneof
    [ CtxList . getSmallList <$> arbitrary
    , CtxTuple <$> arbitrary
    -- Silence unused binding warning
    , do { x <- arbitrary; return $ CtxRecord { field = x } }
    , CtxContList <$> arbitrary <*> (getSmallList <$> arbitrary)
    , CtxContInt <$> arbitrary <*> arbitrary
    , CtxContBool <$> arbitrary <*> arbitrary
    , CtxContUnit <$> arbitrary <*> arbitrary
    , CtxContChar <$> arbitrary <*> arbitrary
    , CtxContString <$> arbitrary <*> (getSmallList <$> arbitrary)
    ]
  shrink c = case c of
    CtxList x         -> CtxList <$> shrink x
    CtxTuple x        -> CtxTuple <$> shrink x
    CtxRecord x       -> CtxRecord <$> shrink x
    CtxContList x y   -> uncurry CtxContList <$> shrink (x,y)
    CtxContInt x y    -> uncurry CtxContInt <$> shrink (x,y)
    CtxContBool x y   -> uncurry CtxContBool <$> shrink (x,y)
    CtxContUnit x y   -> uncurry CtxContUnit <$> shrink (x,y)
    CtxContChar x y   -> uncurry CtxContChar <$> shrink (x,y)
    CtxContString x y -> uncurry CtxContString <$> shrink (x,y)

showReadPartialIsomorphism :: forall a.
  (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showReadPartialIsomorphism _ =
  myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  ("readMaybe (show a)")
  (\a -> readMaybe (show a))
  ("Just a")
  (\a -> Just a)

showReadSpacePartialIsomorphism :: forall a.
  (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showReadSpacePartialIsomorphism _ =
  myForAllShrink False (const True)
  (\(a :: a) -> ["a = " ++ show a])
  ("readMaybe (\" \" ++ show a)")
  (\a -> readMaybe (" " ++ show a))
  ("Just a")
  (\a -> Just a)

showReadCtxPartialIsomorphism :: forall a.
  (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showReadCtxPartialIsomorphism _ =
  myForAllShrink True (const True)
  (\(a :: Ctx a) -> ["a = " ++ show a])
  ("readMaybe (show a)")
  (\a -> readMaybe (show a))
  ("Just a")
  (\a -> Just a)

showsPrecReadsPrecPartialIsomorphism :: forall a.
  (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showsPrecReadsPrecPartialIsomorphism _ =
  property $ \(a :: a) (ShowReadPrecedence p) ->
    (a,"") `elem` readsPrec p (showsPrec p a "")

showListReadListPartialIsomorphism :: forall a.
  (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showListReadListPartialIsomorphism _ =
  property $ \(SmallList (as :: [a])) ->
    (as,"") `elem` readList (showList as "")

showListWithShowsReadListDefaultPartialIsomorphism :: forall a.
  (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showListWithShowsReadListDefaultPartialIsomorphism _ =
  property $ \(SmallList (as :: [a])) ->
    (as,"") `elem` readListDefault (showListWith shows as "")

-- Expressions
--------------------------------------------------------------------------------

-- | Tests the following properties:
--
-- TODO
showReadExpressionLaws :: (Show a, Read a, Eq a, Arbitrary a) => Proxy a -> Laws
showReadExpressionLaws p = Laws "Show/Read Expression"
  [ ("Partial Isomorphism with mods", showReadModPartialIsomorphism p)
  ]

showReadModPartialIsomorphism :: forall a.
  (Show a, Read a, Arbitrary a, Eq a) => Proxy a -> Property
showReadModPartialIsomorphism _ =
  property $ \(a :: a) -> do
    ms <- getSmallList <$> arbitrary
    return $
      readMaybe (foldr runMod (show a++) ms "") == Just a

data Mod = Prepend Char | Append Char | Parens

runMod :: Mod -> ShowS -> ShowS
runMod m s = case m of
  Prepend c -> (c:) . s
  Append c  -> s . (c:)
  Parens    -> ('(':) . s . (')':)

instance Arbitrary Mod where
  arbitrary = oneof
    [ Prepend <$> elements whitespace
    , Append <$> elements whitespace
    , return Parens
    ]
    where
      -- Control characters considered whitespace
      controlWhite = "\t\n\v\f\r"
      -- Unicode 11.0.0 whitespace characters extracted using
      --  curl -s https://www.unicode.org/Public/11.0.0/ucd/UnicodeData.txt \
      --    | egrep '([^;]+;){2}Zs'
      unicodeWhite = map chr
        [ 0x0020, 0x00A0, 0x1680, 0x2000, 0x2001, 0x2002
        , 0x2003, 0x2004, 0x2005, 0x2006, 0x2007, 0x2008
        , 0x2009, 0x200A, 0x202F, 0x205F, 0x3000
        ]
      -- Filter with isSpace to make sure base also considers it whitespace
      whitespace = filter isSpace (controlWhite ++ unicodeWhite)
