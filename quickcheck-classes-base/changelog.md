# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

Note that since `quickcheck-classes` reexports larges parts of
`quickcheck-classes-base`, changelog entries that deal with any of the
classes from `base` are duplicated across the two changelogs.

## [0.6.2.0] - 2021-04-12

- Storable Set-Set Law (resolves issue 101).
- Trim unneeded dependencies (tagged, base-orphans)
- Trim unneeded dependencies on newer GHCs (bifunctors, contravariant)

## [0.6.1.0] - 2020-09-09
### Added
- Laws for `abs` and `signum`
- Storable Set-Set Law (resolves issue 101).
- Add laws for `quotRem` and `divMod`.
- Use non-commutative monoid for bifoldable tests (resolves issue 98)
- `substitutiveEqLaws`, which tests for Eq substitutivity.
- Negation law check for `Eq`.
- Document that users can provide their own `Laws`.

## [0.6.0.0] - 2019-08-08
### Added
- Initial release. This factor out a subset of laws tests
  from `quickcheck-classes` and depend on this library that
  have a more minimal dependency footprint.
- Add laws for left rotate and right rotate.
- Add law that checks that subtraction is the same thing as
  adding the negation of a number.
