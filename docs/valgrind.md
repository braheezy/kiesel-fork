# Valgrind

Kiesel aims to be Valgrind-clean, but bdwgc will cause errors by default.

A [suppression file](../tools/valgrind/kiesel.supp) is provided to remove the noise:

```console
valgrind --suppressions=tools/valgrind/kiesel.supp ./zig-out/bin/kiesel
```
