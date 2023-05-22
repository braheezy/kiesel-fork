//! 6.1.7.4 Well-Known Intrinsic Objects
//! https://tc39.es/ecma262/#sec-well-known-intrinsic-objects

const builtins = @import("../../builtins.zig");
const types = @import("../../types.zig");

const Object = types.Object;
const Realm = @import("../Realm.zig");

const Self = @This();

realm: *Realm,

const lazy_intrinsics = struct {
    var @"%Boolean%": ?Object = null;
    var @"%Boolean.prototype%": ?Object = null;
    var @"%Function.prototype%": ?Object = null;
    var @"%isFinite%": ?Object = null;
    var @"%isNaN%": ?Object = null;
    var @"%Object.prototype%": ?Object = null;
    var @"%ThrowTypeError%": ?Object = null;
};

inline fn lazyIntrinsic(
    self: *Self,
    comptime name: []const u8,
    comptime T: type,
) error{OutOfMemory}!Object {
    const intrinsic = &@field(lazy_intrinsics, name);
    if (intrinsic.* == null)
        intrinsic.* = try T.create(self.realm);
    return intrinsic.*.?;
}

pub fn @"%Boolean%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Boolean%", builtins.BooleanConstructor);
}
pub fn @"%Boolean.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Boolean.prototype%", builtins.BooleanPrototype);
}
pub fn @"%Function.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Function.prototype%", builtins.FunctionPrototype);
}
pub fn @"%isFinite%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%isFinite%", builtins.global_functions.IsFinite);
}
pub fn @"%isNaN%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%isNaN%", builtins.global_functions.IsNaN);
}
pub fn @"%Object.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Object.prototype%", builtins.ObjectPrototype);
}
pub fn @"%ThrowTypeError%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%ThrowTypeError%", builtins.ThrowTypeError);
}
