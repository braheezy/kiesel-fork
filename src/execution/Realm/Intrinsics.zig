//! 6.1.7.4 Well-Known Intrinsic Objects
//! https://tc39.es/ecma262/#sec-well-known-intrinsic-objects

const builtins = @import("../../builtins.zig");
const types = @import("../../types.zig");

const Object = types.Object;
const Realm = @import("../Realm.zig");

const Self = @This();

realm: *Realm,

// Not stored as top-level properties so we can have methods of the same names
lazy_intrinsics: struct {
    @"%Array%": ?Object = null,
    @"%Array.prototype%": ?Object = null,
    @"%BigInt%": ?Object = null,
    @"%BigInt.prototype%": ?Object = null,
    @"%Boolean%": ?Object = null,
    @"%Boolean.prototype%": ?Object = null,
    @"%Error%": ?Object = null,
    @"%Error.prototype%": ?Object = null,
    @"%eval%": ?Object = null,
    @"%Function%": ?Object = null,
    @"%Function.prototype%": ?Object = null,
    @"%isFinite%": ?Object = null,
    @"%isNaN%": ?Object = null,
    @"%Number%": ?Object = null,
    @"%Number.prototype%": ?Object = null,
    @"%Object%": ?Object = null,
    @"%Object.prototype%": ?Object = null,
    @"%String%": ?Object = null,
    @"%String.prototype%": ?Object = null,
    @"%Symbol%": ?Object = null,
    @"%Symbol.prototype%": ?Object = null,
    @"%ThrowTypeError%": ?Object = null,
} = .{},

inline fn lazyIntrinsic(
    self: *Self,
    comptime name: []const u8,
    comptime T: type,
) error{OutOfMemory}!Object {
    const intrinsic = &@field(self.lazy_intrinsics, name);
    if (intrinsic.* == null) {
        // Intrinsics that have a dependency on themselves need to use two-stage initialization
        // when first created, otherwise create() goes into infinite recursion.
        // Once the intrinsic is no longer null, regular create() can be used.
        if (@hasDecl(T, "createNoinit")) {
            intrinsic.* = try T.createNoinit(self.realm);
            try T.init(self.realm, intrinsic.*.?);
        } else {
            intrinsic.* = try T.create(self.realm);
        }
    }
    return intrinsic.*.?;
}

pub fn @"%Array%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Array%", builtins.ArrayConstructor);
}
pub fn @"%Array.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Array.prototype%", builtins.ArrayPrototype);
}
pub fn @"%BigInt%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%BigInt%", builtins.BigIntConstructor);
}
pub fn @"%BigInt.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%BigInt.prototype%", builtins.BigIntPrototype);
}
pub fn @"%Boolean%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Boolean%", builtins.BooleanConstructor);
}
pub fn @"%Boolean.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Boolean.prototype%", builtins.BooleanPrototype);
}
pub fn @"%Error%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Error%", builtins.ErrorConstructor);
}
pub fn @"%Error.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Error.prototype%", builtins.ErrorPrototype);
}
pub fn @"%eval%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%eval%", builtins.global_functions.Eval);
}
pub fn @"%Function%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Function%", builtins.FunctionConstructor);
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
pub fn @"%Number%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Number%", builtins.NumberConstructor);
}
pub fn @"%Number.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Number.prototype%", builtins.NumberPrototype);
}
pub fn @"%Object%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Object%", builtins.ObjectConstructor);
}
pub fn @"%Object.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Object.prototype%", builtins.ObjectPrototype);
}
pub fn @"%String%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%String%", builtins.StringConstructor);
}
pub fn @"%String.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%String.prototype%", builtins.StringPrototype);
}
pub fn @"%Symbol%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Symbol%", builtins.SymbolConstructor);
}
pub fn @"%Symbol.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Symbol.prototype%", builtins.SymbolPrototype);
}
pub fn @"%ThrowTypeError%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%ThrowTypeError%", builtins.ThrowTypeError);
}
