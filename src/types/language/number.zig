//! 6.1.6.1 The Number Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-number-type

pub const Number = union(enum) {
    f64: f64,
    // OPTIMIZATION: Instead of always storing floats we also have a Number type that stores an
    // i32 internally.
    i32: i32,
};
