# Running test262

There is no custom test262 runner for Kiesel yet, but
[CanadaHonk](https://github.com/CanadaHonk/) kindly added support to their forks
of various popular existing tools!

## Installation

Clone test262 - we always use the upstream `main` branch, there is no manually
updated fork or submodule:

```console
git clone https://github.com/tc39/test262
```

Install [`eshost`](https://github.com/CanadaHonk/eshost) and
[`test262-harness`](https://github.com/CanadaHonk/test262-harness):

```console
npm install -g github:CanadaHonk/eshost
npm install -g github:CanadaHonk/test262-harness
```

Optionally you can also use `CanadaHonk/esvu` to automatically install a recent
build of Kiesel for your platform, but for the following we'll assume a local
build is available.

## Usage

To run all tests (this takes a long time!):

```console
test262-harness --host-type=kiesel --host-path=/path/to/zig-out/bin/kiesel 'test/**/*.js'
```

To only run tests for e.g. `Object.groupBy()` pass
`test/built-ins/Object/groupBy/**/*.js` instead - in this case the recursive
glob is not necessary but tests often have subdirectories.

Make sure to quote the last argument so that your shell doesn't expand the glob
and passes individual file paths as arguments.
