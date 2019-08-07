module Test.QuickCheck.Classes.IsList
  ( module Test.QuickCheck.Classes.Base.IsList
  ) where

-- It would be better to do this with Cabal's module reexport feature,
-- but that would break compatibility with older GHCs.

import Test.QuickCheck.Classes.Base.IsList
