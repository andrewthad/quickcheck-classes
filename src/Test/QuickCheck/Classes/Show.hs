{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

{-| Module      : Test.QuickCheck.Classes.Show
    Description : Properties for testing the properties of the Show type class.
-}
module Test.QuickCheck.Classes.Show
  ( showLaws
  , showExpressionLaws
  ) where

import Control.Monad (guard, void)
import Data.Char (isAlphaNum, isLower, isUpper)
import Data.Maybe (isJust)
import Data.Proxy (Proxy)
import Test.QuickCheck (Arbitrary, Property, property)
import Text.ParserCombinators.ReadP (ReadP, char, eof, manyTill, munch, optional,
  readP_to_S, satisfy, sepBy1, skipSpaces, (+++))
import Text.Read.Lex (Lexeme(..), lex)

import Test.QuickCheck.Classes.Common (Laws(..), ShowReadPrecedence(..))

-- | Tests the following properties:
--
-- [/Show/]
-- @'show' a ≡ 'showsPrec' 0 a ""@
-- [/Equivariance: 'showsPrec'/]
-- @'showsPrec' p a r '++' s ≡ 'showsPrec' p a (r '++' s)@
-- [/Equivariance: 'showList'/]
-- @'showList' as r '++' s ≡ 'showList' as (r '++' s)@
--
showLaws :: (Show a, Arbitrary a) => Proxy a -> Laws
showLaws p = Laws "Show"
  [ ("Show", showShowsPrecZero p)
  , ("Equivariance: showsPrec", equivarianceShowsPrec p)
  , ("Equivariance: showList", equivarianceShowList p)
  ]

showShowsPrecZero :: forall a. (Show a, Arbitrary a) => Proxy a -> Property
showShowsPrecZero _ =
  property $ \(a :: a) ->
    show a == showsPrec 0 a ""

equivarianceShowsPrec :: forall a.
  (Show a, Arbitrary a) => Proxy a -> Property
equivarianceShowsPrec _ =
  property $ \(ShowReadPrecedence p) (a :: a) (r :: String) (s :: String) ->
    showsPrec p a r ++ s == showsPrec p a (r ++ s)

equivarianceShowList :: forall a.
  (Show a, Arbitrary a) => Proxy a -> Property
equivarianceShowList _ =
  property $ \(as :: [a]) (r :: String) (s :: String) ->
    showList as r ++ s == showList as (r ++ s)

-- Show Expression Laws
--------------------------------------------------------------------------------

-- | Tests the following properties:
--
-- TODO
showExpressionLaws :: (Show a, Arbitrary a) => Proxy a -> Laws
showExpressionLaws p = Laws "Show Expression"
  [ ("show is lexable", showIsLexable p)
  , ("showList is lexable", showListIsLexable p)
  , ("showsPrec 11 is atomic expression", showsPrec11Atomic p)
  ]

tokenizeMaybe :: String -> Maybe [Lexeme]
tokenizeMaybe s
  | [(ts,"")] <- readP_to_S (manyTill Text.Read.Lex.lex eof) s = Just ts
  | otherwise                                                  = Nothing

showIsLexable :: forall a. (Show a, Arbitrary a) => Proxy a -> Property
showIsLexable _ = property $ \(a :: a) -> isJust (tokenizeMaybe (show a))

showListIsLexable :: forall a. (Show a, Arbitrary a) => Proxy a -> Property
showListIsLexable _ = property $ \(as :: [a]) ->
  isJust (tokenizeMaybe (showList as ""))

-- This tests whether the result of showing in precedence context 11 is an
-- atomic expression, which includes parenthesized expressions. Only the
-- outermost lexical structure is tested.
--
-- According to the Haskell 2010 Language report,
-- http://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5
-- the atomic expressions are defined as
--
-- aexp    → qvar                                (variable)
--         | gcon                                (general constructor)
--         | literal
--         | ( exp )                             (parenthesized expression)
--         | ( exp1 , … , expk )	         (tuple, k ≥ 2)
--         | ( infixexp qop )                    (left section)
--         | ( qop⟨-⟩ infixexp )                 (right section)
--         | [ exp1 , … , expk ]	         (list, k ≥ 1)
--         | [ exp1 [, exp2] .. [exp3] ]         (arithmetic sequence)
--         | [ exp | qual1 , … , qualn ]         (list comprehension, n ≥ 1)
--         | qcon { fbind1 , … , fbindn }        (labeled construction, n ≥ 0)
--         | aexp⟨qcon⟩ { fbind1 , … , fbindn }  (labeled update, n  ≥  1)
-- gcon	   → ()
--         | []
--         | (,{,})
--         | qcon
-- qvar    → qvarid | ( qvarsym )                (qualified variable)
-- qcon    → qconid | ( gconsym )                (qualified constructor)
-- literal → integer | float | char | string
--
-- The lexeme parser from Text.Read.Lex has two shortcomings when it comes to
-- recognizing atomic expressions:
--
-- 1. Qualified constructors (qconid) and qualified variables (qvarid) are not
--    properly recognized and are lexed as identifiers seperated by dots
--    instead:
--
--      λ> tokenizeMaybe "MyModuleId.MyConId"
--      Just [Ident "MyModuleId",Symbol ".",Ident "MyConId"]
--
--    This should of course be remedied by using a fully compliant lexer, e.g.
--    http://hackage.haskell.org/package/haskell-lexer . To avoid the extra
--    dependency, we implement a lexer for qualified identifiers here.
--
-- 2. Labeled construction and labeled update are not recognized as atomic.
--
--      λ> tokenizeMaybe "K { f = x }"
--      Just [Ident "K",Punc "{",Ident "f",Punc "=",Ident "x",Punc "}"]
--
--    At least handling labeled updates correctly would need a full blown
--    expression parser. However, since the default derived Show instances
--    for records emit parentheses, and this is more readable, we content
--    ourselves with a falsely failing property for records without parens.
showsPrec11Atomic :: forall a. (Show a, Arbitrary a) => Proxy a -> Property
showsPrec11Atomic _ = property $ \(a :: a) ->
  let str = showsPrec 11 a "" in
  case tokenizeMaybe str of
    Nothing -> False
    Just ts -> case ts of
      [x] -> case x of
               -- conid, varid
               Ident{}  -> True
               -- literal
               Char{}   -> True
               String{} -> True
#if MIN_VERSION_base(4,6,0)
               Number{} -> True
#else
               Int{}    -> True
               Rat{}    -> True
#endif
               -- consym, varsym
               Symbol{} -> False
               -- special: ,;()[]{}`
               Punc{}   -> False
               EOF{}    -> False
      -- parenthesized expression, tuple, left section, right section,
      -- unit constructor, tuple constructor, general symbol constructor
      -- general symbol variable
      Punc "(":_ | Punc ")":_ <- reverse ts -> True
      -- empty list, non-empty list, arithmetic sequence, list comprehension
      Punc "[":_ | Punc "]":_ <- reverse ts -> True
      _                                     -> isQualId str

-- Haskell 2010 identifiers
-- https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4
--
-- varid       →  (small {small | large | digit | ' })⟨reservedid⟩
-- conid       →  large {small | large | digit | ' }
-- reservedid  →  case | class | data | default | deriving | do | else
--             |  foreign | if | import | in | infix | infixl
--             |  infixr | instance | let | module | newtype | of
--             |  then | type | where | _
-- modid       →  {conid .} conid
-- qvarid      →  [modid .] varid
-- qconid      →  [modid .] conid

isSmall, isLarge, isFollow :: Char -> Bool
isSmall c  = isLower c || c == '_'
isLarge    = isUpper
isFollow c = isAlphaNum c || c `elem` "_'"

isReserved :: String -> Bool
isReserved x = x `elem`
  [ "case", "class", "data", "default", "deriving", "do", "else", "foreign"
  , "if", "import", "in", "infix", "infixl", "infixr", "instance", "let"
  , "module", "newtype", "of", "then", "type", "where", "_"
  ]

pLexVarId, pLexConId, pLexQConId, pLexQVarId :: ReadP ()
pLexVarId  = void $ do
                      c <- satisfy isSmall
                      s <- munch isFollow
                      guard (not (isReserved (c:s)))
pLexConId  = void $ satisfy isLarge >> munch isFollow
pLexQConId = void $ sepBy1 pLexConId (char '.')             -- inlined modid
pLexQVarId = optional (pLexQConId >> char '.') >> pLexVarId -- inlined modid

pQConId, pQVarId :: ReadP ()
pQConId = skipSpaces >> pLexQConId
pQVarId = skipSpaces >> pLexQVarId

isQualId :: String -> Bool
isQualId s = ((),"") `elem` readP_to_S (pQConId +++ pQVarId >> eof) s
