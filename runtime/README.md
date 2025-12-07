# Kiesel Runtime

> 🌐 A web compatible runtime for the Kiesel JavaScript engine

[![justforfunnoreally.dev badge](https://img.shields.io/badge/justforfunnoreally-dev-9ff)](https://justforfunnoreally.dev)
[![License](https://img.shields.io/badge/License-MIT-d63e97)](https://codeberg.org/kiesel-js/runtime/src/branch/main/LICENSE)

## Introduction

The goal for this project roughly aligns with the WinterTC
[Minimum Common Web Platform API](https://common-min-api.proposal.wintertc.org).
Kiesel itself only provides an implementation of ECMA-262 and ECMA-402, other
commonly available APIs like `console` will be implemented here.

## Build

This is included in the `kiesel` CLI by default, follow the instructions in the
[main repo](https://codeberg.org/kiesel-js/kiesel).

To include this runtime in your own project using Kiesel for JS evaluation:

1. Add the dependency to your `build.zig.zon`
2. Supply the `kiesel` module import:

   ```zig
   const kiesel = b.dependency("kiesel");
   const kiesel_runtime = b.dependency("kiesel-runtime");
   kiesel_runtime.module("kiesel-runtime").addImport("kiesel", kiesel.module("kiesel"));
   ```

3. Add bindings to an object (usually the global object) as follows:

   ```zig
   const kiesel_runtime = @import("kiesel-runtime");
   // ...
   try kiesel_runtime.addBindings(realm, global_object);
   ```

## Available APIs

- `Blob()`
- `Blob.prototype.arrayBuffer()`
- `Blob.prototype.bytes()`
- `Blob.prototype.size`
- `Blob.prototype.text()`
- `Blob.prototype.type`
- `console.assert()`
- `console.debug()`
- `console.error()`
- `console.info()`
- `console.log()`
- `console.warn()`
- `crypto.getRandomValues()`
- `crypto.randomUUID()`
- `fetch()` (using synchronous networking for now. sue me)
- `navigator.userAgent`
- `performance.now()`
- `performance.timeOrigin`
- `queueMicrotask()`
- `TextDecoder()`
- `TextDecoder.prototype.decode()`
- `TextDecoder.prototype.encoding`
- `TextDecoder.prototype.fatal`
- `TextDecoder.prototype.ignoreBOM`
- `TextEncoder()`
- `TextEncoder.prototype.encode()`
- `TextEncoder.prototype.encoding`
