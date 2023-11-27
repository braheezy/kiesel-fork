//! 6.1.7.4 Well-Known Intrinsic Objects
//! https://tc39.es/ecma262/#sec-well-known-intrinsic-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

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
    @"%Array.prototype.toString%": ?Object = null,
    @"%Array.prototype.values%": ?Object = null,
    @"%ArrayBuffer%": ?Object = null,
    @"%ArrayBuffer.prototype%": ?Object = null,
    @"%ArrayIteratorPrototype%": ?Object = null,
    @"%AsyncFunction%": ?Object = null,
    @"%AsyncFunction.prototype%": ?Object = null,
    @"%AsyncGeneratorFunction%": ?Object = null,
    @"%AsyncGeneratorFunction.prototype%": ?Object = null,
    @"%AsyncGeneratorFunction.prototype.prototype%": ?Object = null,
    @"%AsyncIteratorPrototype%": ?Object = null,
    @"%BigInt%": ?Object = null,
    @"%BigInt.prototype%": ?Object = null,
    @"%BigInt64Array%": ?Object = null,
    @"%BigInt64Array.prototype%": ?Object = null,
    @"%BigUint64Array%": ?Object = null,
    @"%BigUint64Array.prototype%": ?Object = null,
    @"%Boolean%": ?Object = null,
    @"%Boolean.prototype%": ?Object = null,
    @"%DataView%": ?Object = null,
    @"%DataView.prototype%": ?Object = null,
    @"%Date%": ?Object = null,
    @"%Date.prototype%": ?Object = null,
    @"%decodeURI%": ?Object = null,
    @"%decodeURIComponent%": ?Object = null,
    @"%encodeURI%": ?Object = null,
    @"%encodeURIComponent%": ?Object = null,
    @"%Error%": ?Object = null,
    @"%Error.prototype%": ?Object = null,
    @"%eval%": ?Object = null,
    @"%EvalError%": ?Object = null,
    @"%EvalError.prototype%": ?Object = null,
    @"%Float32Array%": ?Object = null,
    @"%Float32Array.prototype%": ?Object = null,
    @"%Float64Array%": ?Object = null,
    @"%Float64Array.prototype%": ?Object = null,
    @"%Function%": ?Object = null,
    @"%Function.prototype%": ?Object = null,
    @"%GeneratorFunction%": ?Object = null,
    @"%GeneratorFunction.prototype%": ?Object = null,
    @"%GeneratorFunction.prototype.prototype%": ?Object = null,
    @"%Int8Array%": ?Object = null,
    @"%Int8Array.prototype%": ?Object = null,
    @"%Int16Array%": ?Object = null,
    @"%Int16Array.prototype%": ?Object = null,
    @"%Int32Array%": ?Object = null,
    @"%Int32Array.prototype%": ?Object = null,
    @"%Intl%": ?Object = null,
    @"%Intl.Locale%": ?Object = null,
    @"%Intl.Locale.prototype%": ?Object = null,
    @"%isFinite%": ?Object = null,
    @"%isNaN%": ?Object = null,
    @"%IteratorPrototype%": ?Object = null,
    @"%JSON%": ?Object = null,
    @"%Map%": ?Object = null,
    @"%Map.prototype%": ?Object = null,
    @"%MapIteratorPrototype%": ?Object = null,
    @"%Math%": ?Object = null,
    @"%Number%": ?Object = null,
    @"%Number.prototype%": ?Object = null,
    @"%Object%": ?Object = null,
    @"%Object.prototype%": ?Object = null,
    @"%Object.prototype.toString%": ?Object = null,
    @"%parseFloat%": ?Object = null,
    @"%parseInt%": ?Object = null,
    @"%Promise%": ?Object = null,
    @"%Promise.prototype%": ?Object = null,
    @"%Proxy%": ?Object = null,
    @"%RangeError%": ?Object = null,
    @"%RangeError.prototype%": ?Object = null,
    @"%ReferenceError%": ?Object = null,
    @"%ReferenceError.prototype%": ?Object = null,
    @"%Reflect%": ?Object = null,
    @"%RegExp%": ?Object = null,
    @"%RegExp.prototype%": ?Object = null,
    @"%RegExpStringIteratorPrototype%": ?Object = null,
    @"%Set%": ?Object = null,
    @"%Set.prototype%": ?Object = null,
    @"%SetIteratorPrototype%": ?Object = null,
    @"%String%": ?Object = null,
    @"%String.prototype%": ?Object = null,
    @"%StringIteratorPrototype%": ?Object = null,
    @"%Symbol%": ?Object = null,
    @"%Symbol.prototype%": ?Object = null,
    @"%SyntaxError%": ?Object = null,
    @"%SyntaxError.prototype%": ?Object = null,
    @"%ThrowTypeError%": ?Object = null,
    @"%TypedArray%": ?Object = null,
    @"%TypedArray.prototype%": ?Object = null,
    @"%TypeError%": ?Object = null,
    @"%TypeError.prototype%": ?Object = null,
    @"%Uint8Array%": ?Object = null,
    @"%Uint8Array.prototype%": ?Object = null,
    @"%Uint8ClampedArray%": ?Object = null,
    @"%Uint8ClampedArray.prototype%": ?Object = null,
    @"%Uint16Array%": ?Object = null,
    @"%Uint16Array.prototype%": ?Object = null,
    @"%Uint32Array%": ?Object = null,
    @"%Uint32Array.prototype%": ?Object = null,
    @"%URIError%": ?Object = null,
    @"%URIError.prototype%": ?Object = null,
} = .{},

inline fn lazyIntrinsic(
    self: *Self,
    comptime name: []const u8,
    comptime T: type,
) Allocator.Error!Object {
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

pub fn @"%AggregateError%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%AggregateError%", builtins.AggregateErrorConstructor);
}
pub fn @"%AggregateError.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%AggregateError.prototype%", builtins.AggregateErrorPrototype);
}
pub fn @"%Array%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Array%", builtins.ArrayConstructor);
}
pub fn @"%Array.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Array.prototype%", builtins.ArrayPrototype);
}
pub fn @"%Array.prototype.toString%"(self: *Self) Allocator.Error!Object {
    const intrinsic = &self.lazy_intrinsics.@"%Array.prototype.toString%";
    if (intrinsic.* == null) {
        const array_prototype = try @"%Array.prototype%"(self);
        const property_descriptor = array_prototype.data.property_storage.get(PropertyKey.from("toString"));
        intrinsic.* = property_descriptor.?.value.?.object;
    }
    return intrinsic.*.?;
}
pub fn @"%Array.prototype.values%"(self: *Self) Allocator.Error!Object {
    const intrinsic = &self.lazy_intrinsics.@"%Array.prototype.values%";
    if (intrinsic.* == null) {
        const array_prototype = try @"%Array.prototype%"(self);
        const property_descriptor = array_prototype.data.property_storage.get(PropertyKey.from("values"));
        intrinsic.* = property_descriptor.?.value.?.object;
    }
    return intrinsic.*.?;
}
pub fn @"%ArrayBuffer%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%ArrayBuffer%", builtins.ArrayBufferConstructor);
}
pub fn @"%ArrayBuffer.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%ArrayBuffer.prototype%", builtins.ArrayBufferPrototype);
}
pub fn @"%ArrayIteratorPrototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%ArrayIteratorPrototype%", builtins.ArrayIteratorPrototype);
}
pub fn @"%AsyncFunction%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%AsyncFunction%", builtins.AsyncFunctionConstructor);
}
pub fn @"%AsyncFunction.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%AsyncFunction.prototype%", builtins.AsyncFunctionPrototype);
}
pub fn @"%AsyncGeneratorFunction%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%AsyncGeneratorFunction%", builtins.AsyncGeneratorFunctionConstructor);
}
pub fn @"%AsyncGeneratorFunction.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%AsyncGeneratorFunction.prototype%", builtins.AsyncGeneratorFunctionPrototype);
}
pub fn @"%AsyncGeneratorFunction.prototype.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%AsyncGeneratorFunction.prototype.prototype%", builtins.AsyncGeneratorPrototype);
}
pub fn @"%AsyncIteratorPrototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%AsyncIteratorPrototype%", builtins.AsyncIteratorPrototype);
}
pub fn @"%BigInt%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%BigInt%", builtins.BigIntConstructor);
}
pub fn @"%BigInt.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%BigInt.prototype%", builtins.BigIntPrototype);
}
pub fn @"%BigInt64Array%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%BigInt%", builtins.BigInt64ArrayConstructor);
}
pub fn @"%BigInt64Array.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%BigInt%", builtins.BigInt64ArrayPrototype);
}
pub fn @"%BigUint64Array%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%BigInt%", builtins.BigUint64ArrayConstructor);
}
pub fn @"%BigUint64Array.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%BigInt%", builtins.BigUint64ArrayPrototype);
}
pub fn @"%Boolean%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Boolean%", builtins.BooleanConstructor);
}
pub fn @"%Boolean.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Boolean.prototype%", builtins.BooleanPrototype);
}
pub fn @"%DataView%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%DataView%", builtins.DataViewConstructor);
}
pub fn @"%DataView.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%DataView.prototype%", builtins.DataViewPrototype);
}
pub fn @"%Date%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Date%", builtins.DateConstructor);
}
pub fn @"%Date.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Date.prototype%", builtins.DatePrototype);
}
pub fn @"%decodeURI%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%decodeURI%", builtins.global_functions.DecodeURI);
}
pub fn @"%decodeURIComponent%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%decodeURIComponent%", builtins.global_functions.DecodeURIComponent);
}
pub fn @"%encodeURI%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%encodeURI%", builtins.global_functions.EncodeURI);
}
pub fn @"%encodeURIComponent%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%encodeURIComponent%", builtins.global_functions.EncodeURIComponent);
}
pub fn @"%Error%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Error%", builtins.ErrorConstructor);
}
pub fn @"%Error.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Error.prototype%", builtins.ErrorPrototype);
}
pub fn @"%eval%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%eval%", builtins.global_functions.Eval);
}
pub fn @"%EvalError%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%EvalError%", builtins.EvalErrorConstructor);
}
pub fn @"%EvalError.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%EvalError.prototype%", builtins.EvalErrorPrototype);
}
pub fn @"%Float32Array%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Float32Array%", builtins.Float32ArrayConstructor);
}
pub fn @"%Float32Array.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Float32Array.prototype%", builtins.Float32ArrayPrototype);
}
pub fn @"%Float64Array%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Float64Array%", builtins.Float64ArrayConstructor);
}
pub fn @"%Float64Array.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Float64Array.prototype%", builtins.Float64ArrayPrototype);
}
pub fn @"%Function%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Function%", builtins.FunctionConstructor);
}
pub fn @"%Function.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Function.prototype%", builtins.FunctionPrototype);
}
pub fn @"%GeneratorFunction%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%GeneratorFunction%", builtins.GeneratorFunctionConstructor);
}
pub fn @"%GeneratorFunction.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%GeneratorFunction.prototype%", builtins.GeneratorFunctionPrototype);
}
pub fn @"%GeneratorFunction.prototype.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%GeneratorFunction.prototype.prototype%", builtins.GeneratorPrototype);
}
pub fn @"%Int8Array%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Int8Array%", builtins.Int8ArrayConstructor);
}
pub fn @"%Int8Array.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Int8Array.prototype%", builtins.Int8ArrayPrototype);
}
pub fn @"%Int16Array%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Int16Array%", builtins.Int16ArrayConstructor);
}
pub fn @"%Int16Array.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Int16Array.prototype%", builtins.Int16ArrayPrototype);
}
pub fn @"%Int32Array%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Int32Array%", builtins.Int32ArrayConstructor);
}
pub fn @"%Int32Array.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Int32Array.prototype%", builtins.Int32ArrayPrototype);
}
pub fn @"%Intl%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl%", builtins.Intl);
}
pub fn @"%Intl.Locale%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.Locale%", builtins.Intl.LocaleConstructor);
}
pub fn @"%Intl.Locale.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.Locale.prototype%", builtins.Intl.LocalePrototype);
}
pub fn @"%isFinite%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%isFinite%", builtins.global_functions.IsFinite);
}
pub fn @"%isNaN%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%isNaN%", builtins.global_functions.IsNaN);
}
pub fn @"%IteratorPrototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%IteratorPrototype%", builtins.IteratorPrototype);
}
pub fn @"%JSON%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%JSON%", builtins.JSON);
}
pub fn @"%Map%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Map%", builtins.MapConstructor);
}
pub fn @"%Map.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Map.prototype%", builtins.MapPrototype);
}
pub fn @"%MapIteratorPrototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%MapIteratorPrototype%", builtins.MapIteratorPrototype);
}
pub fn @"%Math%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Math%", builtins.Math);
}
pub fn @"%Number%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Number%", builtins.NumberConstructor);
}
pub fn @"%Number.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Number.prototype%", builtins.NumberPrototype);
}
pub fn @"%Object%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Object%", builtins.ObjectConstructor);
}
pub fn @"%Object.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Object.prototype%", builtins.ObjectPrototype);
}
pub fn @"%Object.prototype.toString%"(self: *Self) Allocator.Error!Object {
    const intrinsic = &self.lazy_intrinsics.@"%Object.prototype.toString%";
    if (intrinsic.* == null) {
        const object_prototype = try @"%Object.prototype%"(self);
        const property_descriptor = object_prototype.data.property_storage.get(PropertyKey.from("toString"));
        intrinsic.* = property_descriptor.?.value.?.object;
    }
    return intrinsic.*.?;
}
pub fn @"%parseFloat%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%parseFloat%", builtins.global_functions.ParseFloat);
}
pub fn @"%parseInt%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%parseInt%", builtins.global_functions.ParseInt);
}
pub fn @"%Promise%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Promise%", builtins.PromiseConstructor);
}
pub fn @"%Promise.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Promise.prototype%", builtins.PromisePrototype);
}
pub fn @"%Proxy%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Proxy%", builtins.ProxyConstructor);
}
pub fn @"%RangeError%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%RangeError%", builtins.RangeErrorConstructor);
}
pub fn @"%RangeError.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%RangeError.prototype%", builtins.RangeErrorPrototype);
}
pub fn @"%ReferenceError%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%ReferenceError%", builtins.ReferenceErrorConstructor);
}
pub fn @"%ReferenceError.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%ReferenceError.prototype%", builtins.ReferenceErrorPrototype);
}
pub fn @"%Reflect%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Reflect%", builtins.Reflect);
}
pub fn @"%RegExp%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%RegExp%", builtins.RegExpConstructor);
}
pub fn @"%RegExp.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%RegExp.prototype%", builtins.RegExpPrototype);
}
pub fn @"%RegExpStringIteratorPrototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%RegExpStringIteratorPrototype%", builtins.RegExpStringIteratorPrototype);
}
pub fn @"%Set%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Set%", builtins.SetConstructor);
}
pub fn @"%Set.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Set.prototype%", builtins.SetPrototype);
}
pub fn @"%SetIteratorPrototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%SetIteratorPrototype%", builtins.SetIteratorPrototype);
}
pub fn @"%String%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%String%", builtins.StringConstructor);
}
pub fn @"%String.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%String.prototype%", builtins.StringPrototype);
}
pub fn @"%StringIteratorPrototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%StringIteratorPrototype%", builtins.StringIteratorPrototype);
}
pub fn @"%Symbol%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Symbol%", builtins.SymbolConstructor);
}
pub fn @"%Symbol.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Symbol.prototype%", builtins.SymbolPrototype);
}
pub fn @"%SyntaxError%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%SyntaxError%", builtins.SyntaxErrorConstructor);
}
pub fn @"%SyntaxError.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%SyntaxError.prototype%", builtins.SyntaxErrorPrototype);
}
pub fn @"%ThrowTypeError%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%ThrowTypeError%", builtins.ThrowTypeError);
}
pub fn @"%TypedArray%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%TypedArray%", builtins.TypedArrayConstructor);
}
pub fn @"%TypedArray.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%TypedArray.prototype%", builtins.TypedArrayPrototype);
}
pub fn @"%TypeError%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%TypeError%", builtins.TypeErrorConstructor);
}
pub fn @"%TypeError.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%TypeError.prototype%", builtins.TypeErrorPrototype);
}
pub fn @"%Uint8Array%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Uint8Array%", builtins.Uint8ArrayConstructor);
}
pub fn @"%Uint8Array.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Uint8Array.prototype%", builtins.Uint8ArrayPrototype);
}
pub fn @"%Uint8ClampedArray%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Uint8ClampedArray%", builtins.Uint8ClampedArrayConstructor);
}
pub fn @"%Uint8ClampedArray.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Uint8ClampedArray.prototype%", builtins.Uint8ClampedArrayPrototype);
}
pub fn @"%Uint16Array%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Uint16Array%", builtins.Uint16ArrayConstructor);
}
pub fn @"%Uint16Array.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Uint16Array.prototype%", builtins.Uint16ArrayPrototype);
}
pub fn @"%Uint32Array%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Uint32Array%", builtins.Uint32ArrayConstructor);
}
pub fn @"%Uint32Array.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Uint32Array.prototype%", builtins.Uint32ArrayPrototype);
}
pub fn @"%URIError%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%URIError%", builtins.URIErrorConstructor);
}
pub fn @"%URIError.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%URIError.prototype%", builtins.URIErrorPrototype);
}
