//! 6.1.7.4 Well-Known Intrinsic Objects
//! https://tc39.es/ecma262/#sec-well-known-intrinsic-objects

const builtins = @import("../../builtins.zig");
const types = @import("../../types.zig");

const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Realm = @import("../Realm.zig");

const Self = @This();

realm: *Realm,

// Not stored as top-level properties so we can have methods of the same names
lazy_intrinsics: struct {
    @"%AggregateError%": ?Object = null,
    @"%AggregateError.prototype%": ?Object = null,
    @"%Array%": ?Object = null,
    @"%Array.prototype%": ?Object = null,
    @"%BigInt%": ?Object = null,
    @"%BigInt.prototype%": ?Object = null,
    @"%Boolean%": ?Object = null,
    @"%Boolean.prototype%": ?Object = null,
    @"%Error%": ?Object = null,
    @"%Error.prototype%": ?Object = null,
    @"%eval%": ?Object = null,
    @"%EvalError%": ?Object = null,
    @"%EvalError.prototype%": ?Object = null,
    @"%Function%": ?Object = null,
    @"%Function.prototype%": ?Object = null,
    @"%isFinite%": ?Object = null,
    @"%isNaN%": ?Object = null,
    @"%Math%": ?Object = null,
    @"%Number%": ?Object = null,
    @"%Number.prototype%": ?Object = null,
    @"%Object%": ?Object = null,
    @"%Object.prototype%": ?Object = null,
    @"%Object.prototype.toString%": ?Object = null,
    @"%Proxy%": ?Object = null,
    @"%RangeError%": ?Object = null,
    @"%RangeError.prototype%": ?Object = null,
    @"%ReferenceError%": ?Object = null,
    @"%ReferenceError.prototype%": ?Object = null,
    @"%Reflect%": ?Object = null,
    @"%String%": ?Object = null,
    @"%String.prototype%": ?Object = null,
    @"%Symbol%": ?Object = null,
    @"%Symbol.prototype%": ?Object = null,
    @"%SyntaxError%": ?Object = null,
    @"%SyntaxError.prototype%": ?Object = null,
    @"%ThrowTypeError%": ?Object = null,
    @"%TypeError%": ?Object = null,
    @"%TypeError.prototype%": ?Object = null,
    @"%URIError%": ?Object = null,
    @"%URIError.prototype%": ?Object = null,
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

pub fn @"%AggregateError%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%AggregateError%", builtins.AggregateErrorConstructor);
}
pub fn @"%AggregateError.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%AggregateError.prototype%", builtins.AggregateErrorPrototype);
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
pub fn @"%EvalError%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%EvalError%", builtins.EvalErrorConstructor);
}
pub fn @"%EvalError.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%EvalError.prototype%", builtins.EvalErrorPrototype);
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
pub fn @"%Math%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Math%", builtins.Math);
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
pub fn @"%Object.prototype.toString%"(self: *Self) error{OutOfMemory}!Object {
    const intrinsic = &self.lazy_intrinsics.@"%Object.prototype.toString%";
    if (intrinsic.* == null) {
        const object_prototype = try @"%Object.prototype%"(self);
        const property_descriptor = object_prototype.data.property_storage.get(PropertyKey.from("toString"));
        intrinsic.* = property_descriptor.?.value.?.object;
    }
    return intrinsic.*.?;
}
pub fn @"%Proxy%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Proxy%", builtins.ProxyConstructor);
}
pub fn @"%RangeError%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%RangeError%", builtins.RangeErrorConstructor);
}
pub fn @"%RangeError.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%RangeError.prototype%", builtins.RangeErrorPrototype);
}
pub fn @"%ReferenceError%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%ReferenceError%", builtins.ReferenceErrorConstructor);
}
pub fn @"%ReferenceError.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%ReferenceError.prototype%", builtins.ReferenceErrorPrototype);
}
pub fn @"%Reflect%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%Reflect%", builtins.Reflect);
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
pub fn @"%SyntaxError%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%SyntaxError%", builtins.SyntaxErrorConstructor);
}
pub fn @"%SyntaxError.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%SyntaxError.prototype%", builtins.SyntaxErrorPrototype);
}
pub fn @"%ThrowTypeError%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%ThrowTypeError%", builtins.ThrowTypeError);
}
pub fn @"%TypeError%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%TypeError%", builtins.TypeErrorConstructor);
}
pub fn @"%TypeError.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%TypeError.prototype%", builtins.TypeErrorPrototype);
}
pub fn @"%URIError%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%URIError%", builtins.URIErrorConstructor);
}
pub fn @"%URIError.prototype%"(self: *Self) error{OutOfMemory}!Object {
    return self.lazyIntrinsic("%URIError.prototype%", builtins.URIErrorPrototype);
}
