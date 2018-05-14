{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

{- | This module contains a bunch of pre-defined proxies
     so that users don't /always/ have to write (Proxy :: Proxy Word8), 
     etc.
-}

module Test.QuickCheck.Classes.Proxy
  (
#if !(MIN_VERSION_base(4,7,0))
    Proxy1(..)
  , Proxy2(..)
  , bool
#else
    bool
#endif
  , char
#if MIN_VERSION_base(4,7,0)
  , complex
#endif
#if MIN_VERSION_base(4,9,0)
  , compose
#endif
#if MIN_VERSION_base(4,7,0) 
  , const
#endif
  , constr 
  , datarep
  , datatype
  , dynamic
  , e0
  , e1
  , e2
  , e3
  , e6
  , e9
  , e12
#if MIN_VERSION_base(4,7,0)
  , either
#endif
#if MIN_VERSION_base(4,7,0)
  , fixed 
#endif 
  , fixity
#if MIN_VERSION_base(4,10,0)
  , hrefl
#endif
#if MIN_VERSION_base(4,8,0)
  , identity
#endif
#if MIN_VERSION_base(4,7,0)
  , ioref
#endif
  , int
  , int8
  , int16
  , int32
  , int64
  , ordering 
#if MIN_VERSION_base(4,9,0)
  , functorproduct
#endif
#if MIN_VERSION_base(4,7,0)
  , refl 
#endif 
  , string 
#if MIN_VERSION_base(4,9,0)
  , functorsum
#endif
  , word
  , word8
  , word16
  , word32
  , word64
  , unique
  , version
#if MIN_VERSION_base(4,8,0)
  , void
#endif
  ) where

import Data.Complex (Complex)
import Data.Data (Constr, DataType, DataRep, Fixity)
import Data.Dynamic (Dynamic)
#if MIN_VERSION_base(4,7,0)
import Data.Fixed (Fixed, E0, E1, E2, E3, E6, E9, E12)
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Functor.Compose (Compose)
#endif
#if MIN_VERSION_base(4,7,0)
import Data.Functor.Const (Const)
#endif
#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity
#endif
#if MIN_VERSION_base(4,9,0)
import qualified Data.Functor.Product as Functor
#endif
#if MIN_VERSION_base(4,9,0)
import qualified Data.Functor.Sum as Functor
#endif
#if MIN_VERSION_base(4,7,0)
import Data.IORef (IORef)
#endif
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word16, Word32, Word64)
#if MIN_VERSION_base(4,7,0)
import Data.Type.Equality
  ( (:~:)
#if MIN_VERSION_base(4,10,0)
  , (:~~:)
#endif
  )
#endif
import Data.Unique (Unique)
import Data.Version (Version)
#if MIN_VERSION_base(4,8,0)
import Data.Void (Void)
#endif

import Prelude hiding (const, either, product, sum)

#if !(MIN_VERSION_base(4,7,0))

-- | In older versions of GHC, Proxy is not poly-kinded,
--   so we provide Proxy1.
data Proxy1 (f :: * -> *) = Proxy1

-- | In older versions of GHC, Proxy is not poly-kinded,
--   so we provide Proxy2.
data Proxy2 (f :: * -> * -> *) = Proxy2

#endif

bool :: Proxy Bool
bool = Proxy

char :: Proxy Char
char = Proxy

#if MIN_VERSION_base(4,7,0)
complex :: Proxy Complex
complex = Proxy
#endif

#if MIN_VERSION_base(4,9,0)
compose :: Proxy Compose
compose = Proxy
#endif

#if MIN_VERSION_base(4,7,0)
const :: Proxy Const
const = Proxy
#endif

constr :: Proxy Constr
constr = Proxy

datarep :: Proxy DataRep
datarep = Proxy

datatype :: Proxy DataType
datatype = Proxy

dynamic :: Proxy Dynamic
dynamic = Proxy

#if MIN_VERSION_base(4,7,0)
either :: Proxy Either
either = Proxy
#endif

#if MIN_VERSION_base(4,7,0)
fixed :: Proxy Fixed
fixed = Proxy
#endif

e0 :: Proxy E0
e0 = Proxy

e1 :: Proxy E1
e1 = Proxy

e2 :: Proxy E2
e2 = Proxy

e3 :: Proxy E3
e3 = Proxy

e6 :: Proxy E6
e6 = Proxy

e9 :: Proxy E9
e9 = Proxy

e12 :: Proxy E12
e12 = Proxy

fixity :: Proxy Fixity
fixity = Proxy

#if MIN_VERSION_base(4,10,0)
hrefl :: Proxy (:~~:)
hrefl = Proxy
#endif

#if MIN_VERSION_base(4,8,0)
identity :: Proxy Identity
identity = Proxy
#endif

#if MIN_VERSION_base(4,7,0)
ioref :: Proxy IORef
ioref = Proxy
#endif

int :: Proxy Int
int = Proxy

int8 :: Proxy Int8
int8 = Proxy

int16 :: Proxy Int16
int16 = Proxy

int32 :: Proxy Int32
int32 = Proxy

int64 :: Proxy Int64
int64 = Proxy

#if MIN_VERSION_base(4,9,0)
functorproduct :: Proxy Functor.Product
functorproduct = Proxy
#endif

#if MIN_VERSION_base(4,7,0)
refl :: Proxy (:~:)
refl = Proxy
#endif

string :: Proxy String
string = Proxy

#if MIN_VERSION_base(4,9,0)
functorsum :: Proxy Functor.Sum
functorsum = Proxy
#endif

word :: Proxy Word
word = Proxy

word8 :: Proxy Word8
word8 = Proxy

word16 :: Proxy Word16
word16 = Proxy

word32 :: Proxy Word32
word32 = Proxy

word64 :: Proxy Word64
word64 = Proxy

ordering :: Proxy Ordering
ordering = Proxy

unique :: Proxy Unique
unique = Proxy

version :: Proxy Version
version = Proxy

#if MIN_VERSION_base(4,8,0)
void :: Proxy Void
void = Proxy
#endif

