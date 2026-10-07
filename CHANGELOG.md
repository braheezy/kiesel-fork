# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.4.0] - 2026-10-07

### Added

- `Iterator.prototype.chunks()` and `Iterator.prototype.windows()`
  ([#199](https://codeberg.org/kiesel-js/kiesel/issues/199))
- `Iterator.prototype.includes()`
  ([#200](https://codeberg.org/kiesel-js/kiesel/issues/200))
- `Iterator.prototype.join()`
  ([#201](https://codeberg.org/kiesel-js/kiesel/issues/201))
- `Promise.allKeyed()` and `Promise.allSettledKeyed()`
  ([#219](https://codeberg.org/kiesel-js/kiesel/issues/219))
- Intl legacy constructor mode for `Intl.DateTimeFormat` and `Intl.NumberFormat`
- `Intl.DisplayNames.prototype.of()` support for the `"script"` type
- Named time zone offset calculation for `Date` and `Intl.DateTimeFormat`
- `Kiesel.args` and `Kiesel.env` CLI globals
- Rust version in `--version` output for Intl/Temporal builds
- CI builds for OpenBSD (new) and NetBSD (re-enabled)
  ([#164](https://codeberg.org/kiesel-js/kiesel/issues/164),
  [#208](https://codeberg.org/kiesel-js/kiesel/issues/208))
- SerenityOS port
  ([SerenityOS/serenity#26931](https://github.com/SerenityOS/serenity/pull/26931))

### Changed

- Update to Zig 0.17
- Stop requiring nightly Rust for cross-compilation
- Improve `Intl.DateTimeFormat` implementation, including time zone handling,
  `hourCycle`, `resolvedOptions`, and `formatRange`
- Use Number semantics for counters in `Iterator` methods, and throw a
  `RangeError` when `Iterator.prototype.{take,drop}()` limits exceed 2^53-1
- Make `Promise.try()` use `PromiseResolve` in the non-error case
- Minor performance improvements, including
  polymorphic/megamorphic/computed/prototype inline caches and more constant/
  instruction folding

### Removed

- UEFI port

### Fixed

- Tagged template raw/cooked mixup
- Incorrect branch target for `for`-loop `continue` statements
- Missing binding creation in `for`-`in`/`for`-`of` head evaluation
- Implement more early errors, including accessor parameter mismatches and
  disallowed `arguments` bindings

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

[unreleased]: https://codeberg.org/kiesel-js/kiesel/compare/0.4.0...HEAD
[0.4.0]: https://codeberg.org/kiesel-js/kiesel/compare/0.3.0...0.4.0
[0.3.0]: https://codeberg.org/kiesel-js/kiesel/compare/0.2.0...0.3.0
[0.2.0]:
  https://codeberg.org/kiesel-js/kiesel/compare/929bb044ffcd1776c850a5b924f9992915277e58...0.2.0
