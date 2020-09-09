# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

Note that since `quickcheck-classes` reexports larges parts of
`quickcheck-classes-base`, changelog entries that deal with any of the
classes from `base` are duplicated across the two changelogs.

## [0.6.5.0] - 2020-09-09
### Added
- Laws for `abs` and `signum`
- Storable Set-Set Law (resolves issue 101).
- Add laws for `quotRem` and `divMod`.
- Use non-commutative monoid for bifoldable tests (resolves issue 98)
- `substitutiveEqLaws`, which tests for Eq substitutivity.
- Negation law check for `Eq`.
- Document that users can provide their own `Laws`.

## [0.6.4.0] - 2019-09-13
### Changed
- Use newer semirings

## [0.6.3.0] - 2019-08-08
### Added
- `gcdDomainLaws`
- `euclideanLaws`
### Changed
- Replaces 0.6.2.2. That release should have been a minor version
  bump since it added new features.
- Support `primitive-0.6.4.0`.
- Extend `semiringLaws` to cover `fromNatural`
- Factor out a subset of laws tests into `quickcheck-classes-base`
  and depend on this library.

## [0.6.2.2] - 2019-06-18
### Added
- `numLaws`
- `bitraversableLaws`

## [0.6.2.1] - 2019-05-23
### Fixed
- Removal of BadList test that was causing the test suite to fail

## [0.6.2.0] - 2019-05-23
### Added
- `ixLaws`
- `contravariantLaws`
- `semigroupMonoidLaws`
### Changed
- extend `mvectorLaws`
- extend `applyLaws` to include associativity
### Fixed
- bug in `foldableLaws` which could fail to catch implementations of `foldMap` or `fold`
  that evaluate in the wrong order

## [0.6.1.0] - 2019-01-12
### Change
- `genericLaws` and `generic1Laws` were not exported. Now they are.
### Added
- Add `muvectorLaws`.

## [0.6.0.0] - 2018-12-24
### Change
- Support QuickCheck 2.7 and 2.8. This adds `Arbitrary` orphan instances
  to the test suite.
- Fix CPP that caused build failures on GHC 7.10 and some old
  package versions.
- Fix compiling the test suite without semigroupoids and compiling with old
  versions of transformers.
- Add lower bound for semigroups to make sure the `stimes` method is available.
- The laws `commutativeSemigroupLaws` and `commutativeMonoidLaws` no longer
  check any property other than commutativity. They must now be used in conjunction
  with, rather than in place of, `semigroupLaws` and `monoidLaws`. This is a breaking
  change.
- Fix the right distribution law for semirings.
- The function `lawsCheckMany` now terminates with exit code 1 if a
  test fails.
- Extend `showReadLaws` with new properties for `showsPrec`, `readsPrec`,
  `showList` and `readList`.
- Prettify JSON partial isomorphism test failure.
### Added
- Add `genericLaws` and `generic1Laws`
- Add property tests for special classes of semigroups. This includes:
  commutative, idempotent, rectangular band, and exponential.
- `bifoldableLaws`, `bifoldableFunctorLaws`
- Add `showLaws`.

## [0.5.0.0] - 2018-09-25
### Change
- When compiling with GHC 8.6 and newer, use `QuantifiedConstraints` instead
  of `Eq1`, `Show1`, `Arbitrary1`, `Eq2`, `Show`, and `Arbitrary2`.

## [0.4.14.3] - 2018-09-21
### Change
- Fix a CPP conditional import problem that caused build failures on GHC 7.10
- Set an explicit lower bound for containers

## [0.4.14.2] - 2018-09-12
### Change
- Support QuickCheck-2.12
- Fix compilation for containers<0.5.9
- Fix compilation with QuickCheck-2.9

## [0.4.14.1] - 2018-07-24
### Change
- Build correctly when dependency on semigroupoids is disabled.

## [0.4.14] - 2018-07-23
### Added
- commutativeSemigroupLaws
- the following typeclasses:
    `Data.Semigroupoid.Semigroupoid` (semigroupoids)
    `Data.Functor.Plus.Plus` (semigroupoids)

### Change
- semiringLaws were never exported, we now export them.
- make documentation for `MonadPlus` and `Alternative` consistent.
- bump semirings to 0.2.0.0
- deprecate `Test.QuickCheck.Classes.specialisedLawsCheckMany`
  in favour of `Test.QuickCheck.Classes.lawsCheckOne`

## [0.4.13] - 2018-07-18
### Added
- Laws for `Enum` typeclass.
- Laws for `Category` typeclass.

## [0.4.12] - 2018-06-07
### Added
- Remaining laws for `Storable` typeclass.
- Laws for `Prim` typeclass requiring `setByteArray` and `setOffAddr` to
  match the behavior that would result from manually iterating over the
  array and writing the value element-by-element.
### Change
- Correct the law from the `Bits` typeclass that relates `clearBit`
  and `zeroBits`.
- Limit the size of the lists that are used when testing that
  `mconcat` and `sconcat` have behaviors that match their default
  implementations. For some data structures, concatenating the
  elements in a list of several dozen arbitrary values does not
  finish in a reasonable amount of time. So, the size of these
  has been limited to 6.
- Make library build against `primitive-0.6.1.0`.

## [0.4.11.1] - 2018-05-25
### Change
- Fix compatibility with older GHCs when `semigroupoids` support
  is disabled.

## [0.4.11] - 2018-05-24
### Added
- Greatly improved documentation
- `specialisedLawsCheckMany` function, a shorter way for the user
  to use `lawsCheckMany` on a single type.

### Change
- Some internal names, making it more clear what it is that they do.

## [0.4.10] - 2018-05-03
### Added
- Property tests for `mconcat`, `sconcat`, and `stimes`. It isn't
  common to override the defaults for these, but when you do, it's
  nice to check that they agree with what they are supposed to do.

## [0.4.9] - 2018-04-06
### Change
- Be more careful with import of `Data.Primitive`. There is a
  branch of `primitive` that adds `PrimArray`. The implementation
  of `PrimArray` in this library should eventually be removed, but
  for now it will be sufficient to ensure that it does not create
  a conflicting import problem with the one in the branch.

## [0.4.8] - 2018-03-29
### Change
- Fix compilation regression for older versions of transformers.

## [0.4.7] - 2018-03-29
### Change
- Split up monolithic module into hidden internal modules.
- Fix compilation regression for older GHCs.

## [0.4.6] - 2018-03-29
### Added
- Property test the naturality law for `MonadZip`. There is another law
  that instances should satisfy (the Information Preservation law), but
  it's more difficult to write a test for. It has been omitted for now.
- Property tests for all `MonadPlus` laws.
- Several additional property tests for list-like containers: mapMaybe,
  replicate, filter.

## [0.4.5] - 2018-03-26
### Added
- Property tests for list-like containers that have `IsList` instances.
  These are useful for things that are nearly `Foldable` or nearly `Traversable`
  but are either constrained in their element type or totally monomorphic
  in it.

## [0.4.4] - 2018-03-23
### Added
- Cabal flags for controlling whether or not `aeson` and `semigroupoids`
  are used. These are mostly provided to accelerate builds `primitive`'s
  test suite.

## [0.4.3] - 2018-03-23
### Added
- Property tests for `foldl1` and `foldr1`.
- Property tests for `Traversable`.

## [0.4.2] - 2018-03-22
### Changed
- Made compatible with `transformers-0.3`. Tests for higher-kinded
  typeclasses are unavailable when built with a sufficiently old
  version of both `transformers` and `base`. This is because `Eq1`
  and `Show1` are unavailable in this situation.

## [0.4.1] - 2018-03-21
### Changed
- Made compatible with `transformers-0.4`.

## [0.4.0] - 2018-03-20
### Added
- Property tests for `Bifunctor` and `Alternative`.
### Changed
- Made compatible with older GHCs all the way back to 7.8.4.
- Lower dependency footprint. Eliminate the dependency on `prim-array`
  and inline the relevant functions and types from it into
  `Test.QuickCheck.Classes`. None of these are exported.
