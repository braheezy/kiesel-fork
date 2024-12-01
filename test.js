function foo(i) {
    var j = 123;
    return i + j;
}

for (var i = 0; i < 100_000; i++) {
    foo(i);
}
