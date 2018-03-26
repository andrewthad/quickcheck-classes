# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

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
