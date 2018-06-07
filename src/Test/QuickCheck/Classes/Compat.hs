{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module Test.QuickCheck.Classes.Compat
  ( isTrue#
  ) where

#if MIN_VERSION_base(4,7,0)
import GHC.Exts (isTrue#)
#endif

#if !MIN_VERSION_base(4,7,0)
isTrue# :: Bool -> Bool
isTrue# b = b
#endif
