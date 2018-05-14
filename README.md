# quickcheck-classes

This library provides sets of properties that should hold for common typeclasses,
along with three simple functions that you can use to test them.

## `lawsCheck`:

A convenience function for testing properties in GHCi.
For example, at GHCi:

```bash
>>> lawsCheck (monoidLaws (Proxy :: Proxy Ordering))
Monoid: Associative +++ OK, passed 100 tests.
Monoid: Left Identity +++ OK, passed 100 tests.
Monoid: Right Identity +++ OK, passed 100 tests.
```

Assuming that the 'Arbitrary' instance for 'Ordering' is good, we now
have confidence that the 'Monoid' instance for 'Ordering' satisfies
the monoid laws.

## `lawsCheckMany`:

A convenience function for checking multiple typeclass instances
of multiple types. Consider the following Haskell source file:

```haskell
import Data.Proxy (Proxy(..))
import Data.Map (Map)
import Data.Set (Set)

-- A 'Proxy' for 'Set' 'Int'. 
setInt :: Proxy (Set Int)
setInt = Proxy

-- A 'Proxy' for 'Map' 'Int' 'Int'.
mapInt :: Proxy (Map Int Int)
mapInt = Proxy

myLaws :: Proxy a -> [Laws]
myLaws p = [eqLaws p, monoidLaws p]

namedTests :: [(String, [Laws])]
namedTests =
  [ ("Set Int", myLaws setInt)
  , ("Map Int Int", myLaws mapInt)
  ]
```

Now, in GHCi:

```bash
>>> lawsCheckMany namedTests

Testing properties for common typeclasses
-------------
-- Set Int --
-------------

Eq: Transitive +++ OK, passed 100 tests.
Eq: Symmetric +++ OK, passed 100 tests.
Eq: Reflexive +++ OK, passed 100 tests.
Monoid: Associative +++ OK, passed 100 tests.
Monoid: Left Identity +++ OK, passed 100 tests.
Monoid: Right Identity +++ OK, passed 100 tests.
Monoid: Concatenation +++ OK, passed 100 tests.

-----------------
-- Map Int Int --
-----------------

Eq: Transitive +++ OK, passed 100 tests.
Eq: Symmetric +++ OK, passed 100 tests.
Eq: Reflexive +++ OK, passed 100 tests.
Monoid: Associative +++ OK, passed 100 tests.
Monoid: Left Identity +++ OK, passed 100 tests.
Monoid: Right Identity +++ OK, passed 100 tests.
Monoid: Concatenation +++ OK, passed 100 tests.

```

## `specialisedLawsCheckMany`

A convenience function that allows one to check many typeclass
instances of the same type.

For example, in GHCi:

```bash
>>> specialisedLawsCheckMany (Proxy :: Proxy Word) [jsonLaws, showReadLaws]
ToJSON/FromJSON: Encoding Equals Value +++ OK, passed 100 tests.
ToJSON/FromJSON: Partial Isomorphism +++ OK, passed 100 tests.
Show/Read: Partial Isomorphism +++ OK, passed 100 tests.
```
