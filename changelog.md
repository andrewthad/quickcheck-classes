# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

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
