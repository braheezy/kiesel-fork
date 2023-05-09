//! 20.1 Object Objects
//! https://tc39.es/ecma262/#sec-object-objects

const types = @import("../types.zig");

const Object_ = types.Object;

pub const Object = Object_.Factory(.{});
