# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.6.0.0] - 2019-08-08
### Added
- Initial release. This factor out a subset of laws tests
  from `quickcheck-classes` and depend on this library that
  have a more minimal dependency footprint.
- Add laws for left rotate and right rotate.
- Add law that checks that subtraction is the same thing as
  adding the negation of a number.
