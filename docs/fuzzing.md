# Fuzzing

## With Fuzzilli

In the Kiesel repo:

```console
zig build fuzzilli
```

In the Fuzzilli repo:

```console
git apply /path/to/kiesel/tools/fuzzilli/fuzzilli.patch
swift build -c release
swift run -c release FuzzilliCli --profile=kiesel /path/to/kiesel/zig-out/bin/kiesel-fuzzilli
```
