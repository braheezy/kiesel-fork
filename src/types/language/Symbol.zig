//! 6.1.5 The Symbol Type
//! https://tc39.es/ecma262/#sec-ecmascript-language-types-symbol-type

const Symbol = @This();

pub const Id = usize;

/// Internal ID used for equality checks
id: Id,

/// [[Description]]
description: ?[]const u8,
