# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.3.0] - 2026-07-17

### Added

- `Error.prototype.stack`
  ([#203](https://codeberg.org/kiesel-js/kiesel/issues/203))
- Explicit resource management (`DisposableStack`, `AsyncDisposableStack`,
  `using` declarations)
  ([#191](https://codeberg.org/kiesel-js/kiesel/issues/191))
- Support for destructuring assignment patterns
- CI builds for `loongarch64-linux-musl`

### Changed

- Normalize invalid UTF-8 input using replacement characters instead of
  rejecting it outright, thus allowing arbitrary byte sequences in comments
- Complete `Intl.supportedValuesOf()` implementation
- Update `Iterator.zip()` and `Iterator.zipKeyed()` to stage 4 spec
  ([#160](https://codeberg.org/kiesel-js/kiesel/issues/160))
- Update `Atomics.pause()` to stage 4 spec
  ([#153](https://codeberg.org/kiesel-js/kiesel/issues/153))
- Minor improvements to pretty printing
- Various performance improvements, resulting in the
  [v8-v7 benchmark](https://github.com/mozilla/arewefastyet/tree/master/benchmarks/v8-v7)
  to run 33% faster and use 42% less memory
  ([#205](https://codeberg.org/kiesel-js/kiesel/pulls/205),
  [#207](https://codeberg.org/kiesel-js/kiesel/pulls/207),
  [#209](https://codeberg.org/kiesel-js/kiesel/pulls/209),
  [#210](https://codeberg.org/kiesel-js/kiesel/pulls/210),
  [#217](https://codeberg.org/kiesel-js/kiesel/pulls/217),
  [#218](https://codeberg.org/kiesel-js/kiesel/pulls/218))

### Fixed

- Bytecode use-after-free in exception printing
- Logic typo in `String.prototype.split()`
- Date time string parsing bugs
- BigInt comparison bugs
- Typo in `DataView` constructor error message
- `Map` and `Set` deletion bugs
  ([#212](https://codeberg.org/kiesel-js/kiesel/issues/212)), also improving Set
  performance by using tombstones
  ([#11](https://codeberg.org/kiesel-js/kiesel/issues/11))
- `length` property of built-in setters to be 1 instead of 0
- `const` declarations without initializer not being rejected
- Numeric literal with leading zero not being rejected in strict mode

## [0.2.0] - 2026-05-11

Initial tagged release, containing all changes since the initial commit until
this point.

## 0.1.0 - N/A

Placeholder version used during the first few years of development. No release
exists.

[unreleased]: https://codeberg.org/kiesel-js/kiesel/compare/0.3.0...HEAD
[0.3.0]: https://codeberg.org/kiesel-js/kiesel/compare/0.2.0...0.3.0
[0.2.0]:
  https://codeberg.org/kiesel-js/kiesel/compare/929bb044ffcd1776c850a5b924f9992915277e58...0.2.0
