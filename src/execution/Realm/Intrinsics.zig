//! 6.1.7.4 Well-Known Intrinsic Objects
//! https://tc39.es/ecma262/#sec-well-known-intrinsic-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const AnyPointer = @import("any-pointer").AnyPointer;

const builtins = @import("../../builtins.zig");
const types = @import("../../types.zig");

const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Realm = @import("../Realm.zig");

const Self = @This();

realm: *Realm,

// Not stored as top-level properties so we can have methods of the same names
lazy_intrinsics: struct {
    // Using a null ptr allows us to avoid using extra memory for optionals.
    const null_intrinsic: Object = .{ .data = undefined, .ptr = AnyPointer.null_pointer, .tag = undefined };

    @"%AggregateError%": Object = null_intrinsic,
    @"%AggregateError.prototype%": Object = null_intrinsic,
    @"%Array%": Object = null_intrinsic,
    @"%Array.prototype%": Object = null_intrinsic,
    @"%Array.prototype.toString%": Object = null_intrinsic,
    @"%Array.prototype.values%": Object = null_intrinsic,
    @"%ArrayBuffer%": Object = null_intrinsic,
    @"%ArrayBuffer.prototype%": Object = null_intrinsic,
    @"%ArrayIteratorPrototype%": Object = null_intrinsic,
    @"%AsyncFromSyncIteratorPrototype%": Object = null_intrinsic,
    @"%AsyncFunction%": Object = null_intrinsic,
    @"%AsyncFunction.prototype%": Object = null_intrinsic,
    @"%AsyncGeneratorFunction%": Object = null_intrinsic,
    @"%AsyncGeneratorFunction.prototype%": Object = null_intrinsic,
    @"%AsyncGeneratorPrototype%": Object = null_intrinsic,
    @"%AsyncIteratorPrototype%": Object = null_intrinsic,
    @"%Atomics%": Object = null_intrinsic,
    @"%BigInt%": Object = null_intrinsic,
    @"%BigInt.prototype%": Object = null_intrinsic,
    @"%BigInt64Array%": Object = null_intrinsic,
    @"%BigInt64Array.prototype%": Object = null_intrinsic,
    @"%BigUint64Array%": Object = null_intrinsic,
    @"%BigUint64Array.prototype%": Object = null_intrinsic,
    @"%Boolean%": Object = null_intrinsic,
    @"%Boolean.prototype%": Object = null_intrinsic,
    @"%DataView%": Object = null_intrinsic,
    @"%DataView.prototype%": Object = null_intrinsic,
    @"%Date%": Object = null_intrinsic,
    @"%Date.prototype%": Object = null_intrinsic,
    @"%decodeURI%": Object = null_intrinsic,
    @"%decodeURIComponent%": Object = null_intrinsic,
    @"%encodeURI%": Object = null_intrinsic,
    @"%encodeURIComponent%": Object = null_intrinsic,
    @"%Error%": Object = null_intrinsic,
    @"%Error.prototype%": Object = null_intrinsic,
    @"%escape%": Object = null_intrinsic,
    @"%eval%": Object = null_intrinsic,
    @"%EvalError%": Object = null_intrinsic,
    @"%EvalError.prototype%": Object = null_intrinsic,
    @"%FinalizationRegistry%": Object = null_intrinsic,
    @"%FinalizationRegistry.prototype%": Object = null_intrinsic,
    @"%Float16Array%": Object = null_intrinsic,
    @"%Float16Array.prototype%": Object = null_intrinsic,
    @"%Float32Array%": Object = null_intrinsic,
    @"%Float32Array.prototype%": Object = null_intrinsic,
    @"%Float64Array%": Object = null_intrinsic,
    @"%Float64Array.prototype%": Object = null_intrinsic,
    @"%ForInIteratorPrototype%": Object = null_intrinsic,
    @"%Function%": Object = null_intrinsic,
    @"%Function.prototype%": Object = null_intrinsic,
    @"%GeneratorFunction%": Object = null_intrinsic,
    @"%GeneratorFunction.prototype%": Object = null_intrinsic,
    @"%GeneratorPrototype%": Object = null_intrinsic,
    @"%Int8Array%": Object = null_intrinsic,
    @"%Int8Array.prototype%": Object = null_intrinsic,
    @"%Int16Array%": Object = null_intrinsic,
    @"%Int16Array.prototype%": Object = null_intrinsic,
    @"%Int32Array%": Object = null_intrinsic,
    @"%Int32Array.prototype%": Object = null_intrinsic,
    @"%Intl%": Object = null_intrinsic,
    @"%Intl.Collator%": Object = null_intrinsic,
    @"%Intl.Collator.prototype%": Object = null_intrinsic,
    @"%Intl.DateTimeFormat%": Object = null_intrinsic,
    @"%Intl.DateTimeFormat.prototype%": Object = null_intrinsic,
    @"%Intl.DisplayNames%": Object = null_intrinsic,
    @"%Intl.DisplayNames.prototype%": Object = null_intrinsic,
    @"%Intl.ListFormat%": Object = null_intrinsic,
    @"%Intl.ListFormat.prototype%": Object = null_intrinsic,
    @"%Intl.Locale%": Object = null_intrinsic,
    @"%Intl.Locale.prototype%": Object = null_intrinsic,
    @"%Intl.PluralRules%": Object = null_intrinsic,
    @"%Intl.PluralRules.prototype%": Object = null_intrinsic,
    @"%Intl.Segmenter%": Object = null_intrinsic,
    @"%Intl.Segmenter.prototype%": Object = null_intrinsic,
    @"%IntlSegmentsPrototype%": Object = null_intrinsic,
    @"%IntlSegmentIteratorPrototype%": Object = null_intrinsic,
    @"%isFinite%": Object = null_intrinsic,
    @"%isNaN%": Object = null_intrinsic,
    @"%IteratorPrototype%": Object = null_intrinsic,
    @"%JSON%": Object = null_intrinsic,
    @"%Map%": Object = null_intrinsic,
    @"%Map.prototype%": Object = null_intrinsic,
    @"%MapIteratorPrototype%": Object = null_intrinsic,
    @"%Math%": Object = null_intrinsic,
    @"%Number%": Object = null_intrinsic,
    @"%Number.prototype%": Object = null_intrinsic,
    @"%Object%": Object = null_intrinsic,
    @"%Object.prototype%": Object = null_intrinsic,
    @"%Object.prototype.toString%": Object = null_intrinsic,
    @"%parseFloat%": Object = null_intrinsic,
    @"%parseInt%": Object = null_intrinsic,
    @"%Promise%": Object = null_intrinsic,
    @"%Promise.prototype%": Object = null_intrinsic,
    @"%Proxy%": Object = null_intrinsic,
    @"%RangeError%": Object = null_intrinsic,
    @"%RangeError.prototype%": Object = null_intrinsic,
    @"%ReferenceError%": Object = null_intrinsic,
    @"%ReferenceError.prototype%": Object = null_intrinsic,
    @"%Reflect%": Object = null_intrinsic,
    @"%RegExp%": Object = null_intrinsic,
    @"%RegExp.prototype%": Object = null_intrinsic,
    @"%RegExpStringIteratorPrototype%": Object = null_intrinsic,
    @"%Set%": Object = null_intrinsic,
    @"%Set.prototype%": Object = null_intrinsic,
    @"%SetIteratorPrototype%": Object = null_intrinsic,
    @"%SharedArrayBuffer%": Object = null_intrinsic,
    @"%SharedArrayBuffer.prototype%": Object = null_intrinsic,
    @"%String%": Object = null_intrinsic,
    @"%String.prototype%": Object = null_intrinsic,
    @"%StringIteratorPrototype%": Object = null_intrinsic,
    @"%Symbol%": Object = null_intrinsic,
    @"%Symbol.prototype%": Object = null_intrinsic,
    @"%SyntaxError%": Object = null_intrinsic,
    @"%SyntaxError.prototype%": Object = null_intrinsic,
    @"%ThrowTypeError%": Object = null_intrinsic,
    @"%TypedArray%": Object = null_intrinsic,
    @"%TypedArray.prototype%": Object = null_intrinsic,
    @"%TypeError%": Object = null_intrinsic,
    @"%TypeError.prototype%": Object = null_intrinsic,
    @"%Uint8Array%": Object = null_intrinsic,
    @"%Uint8Array.prototype%": Object = null_intrinsic,
    @"%Uint8ClampedArray%": Object = null_intrinsic,
    @"%Uint8ClampedArray.prototype%": Object = null_intrinsic,
    @"%Uint16Array%": Object = null_intrinsic,
    @"%Uint16Array.prototype%": Object = null_intrinsic,
    @"%Uint32Array%": Object = null_intrinsic,
    @"%Uint32Array.prototype%": Object = null_intrinsic,
    @"%unescape%": Object = null_intrinsic,
    @"%URIError%": Object = null_intrinsic,
    @"%URIError.prototype%": Object = null_intrinsic,
} = .{},

inline fn lazyIntrinsic(
    self: *Self,
    comptime name: []const u8,
    comptime T: type,
) Allocator.Error!Object {
    const intrinsic = &@field(self.lazy_intrinsics, name);
    if (intrinsic.ptr.isNull()) {
        // Intrinsics that have a dependency on themselves need to use two-stage initialization
        // when first created, otherwise create() goes into infinite recursion.
        // Once the intrinsic is no longer null, regular create() can be used.
        if (@hasDecl(T, "createNoinit")) {
            const object = try T.createNoinit(self.realm);
            // Sanity check to ensure there is no dependency loop - creating the object must not
            // (indirectly) rely on itself. If something within `createNoInit()` assigned the
            // intrinsic it has been created twice and overwriting it would be a mistake.
            std.debug.assert(intrinsic.ptr.isNull());
            intrinsic.* = object;
            try T.init(self.realm, object);
        } else {
            intrinsic.* = try T.create(self.realm);
        }
    }
    return intrinsic.*;
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
    if (intrinsic.ptr.isNull()) {
        const array_prototype = try @"%Array.prototype%"(self);
        const property_descriptor = array_prototype.data.property_storage.get(PropertyKey.from("toString"));
        intrinsic.* = property_descriptor.?.value.?.object;
    }
    return intrinsic.*;
}
pub fn @"%Array.prototype.values%"(self: *Self) Allocator.Error!Object {
    const intrinsic = &self.lazy_intrinsics.@"%Array.prototype.values%";
    if (intrinsic.ptr.isNull()) {
        const array_prototype = try @"%Array.prototype%"(self);
        const property_descriptor = array_prototype.data.property_storage.get(PropertyKey.from("values"));
        intrinsic.* = property_descriptor.?.value.?.object;
    }
    return intrinsic.*;
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
pub fn @"%AsyncFromSyncIteratorPrototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%AsyncFromSyncIteratorPrototype%", builtins.AsyncFromSyncIteratorPrototype);
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
pub fn @"%AsyncGeneratorPrototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%AsyncGeneratorPrototype%", builtins.AsyncGeneratorPrototype);
}
pub fn @"%AsyncIteratorPrototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%AsyncIteratorPrototype%", builtins.AsyncIteratorPrototype);
}
pub fn @"%Atomics%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Atomics%", builtins.Atomics);
}
pub fn @"%BigInt%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%BigInt%", builtins.BigIntConstructor);
}
pub fn @"%BigInt.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%BigInt.prototype%", builtins.BigIntPrototype);
}
pub fn @"%BigInt64Array%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%BigInt64Array%", builtins.BigInt64ArrayConstructor);
}
pub fn @"%BigInt64Array.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%BigInt64Array.prototype%", builtins.BigInt64ArrayPrototype);
}
pub fn @"%BigUint64Array%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%BigUint64Array%", builtins.BigUint64ArrayConstructor);
}
pub fn @"%BigUint64Array.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%BigUint64Array.prototype%", builtins.BigUint64ArrayPrototype);
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
pub fn @"%escape%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%escape%", builtins.global_functions.Escape);
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
pub fn @"%FinalizationRegistry%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%FinalizationRegistry%", builtins.FinalizationRegistryConstructor);
}
pub fn @"%FinalizationRegistry.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%FinalizationRegistry.prototype%", builtins.FinalizationRegistryPrototype);
}
pub fn @"%Float16Array%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Float16Array%", builtins.Float16ArrayConstructor);
}
pub fn @"%Float16Array.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Float16Array.prototype%", builtins.Float16ArrayPrototype);
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
pub fn @"%ForInIteratorPrototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%ForInIteratorPrototype%", builtins.ForInIteratorPrototype);
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
pub fn @"%GeneratorPrototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%GeneratorPrototype%", builtins.GeneratorPrototype);
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
pub fn @"%Intl.Collator%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.Collator%", builtins.Intl.CollatorConstructor);
}
pub fn @"%Intl.Collator.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.Collator.prototype%", builtins.Intl.CollatorPrototype);
}
pub fn @"%Intl.DateTimeFormat%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.DateTimeFormat%", builtins.Intl.DateTimeFormatConstructor);
}
pub fn @"%Intl.DateTimeFormat.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.DateTimeFormat.prototype%", builtins.Intl.DateTimeFormatPrototype);
}
pub fn @"%Intl.DisplayNames%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.DisplayNames%", builtins.Intl.DisplayNamesConstructor);
}
pub fn @"%Intl.DisplayNames.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.DisplayNames.prototype%", builtins.Intl.DisplayNamesPrototype);
}
pub fn @"%Intl.ListFormat%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.ListFormat%", builtins.Intl.ListFormatConstructor);
}
pub fn @"%Intl.ListFormat.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.ListFormat.prototype%", builtins.Intl.ListFormatPrototype);
}
pub fn @"%Intl.Locale%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.Locale%", builtins.Intl.LocaleConstructor);
}
pub fn @"%Intl.Locale.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.Locale.prototype%", builtins.Intl.LocalePrototype);
}
pub fn @"%Intl.PluralRules%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.PluralRules%", builtins.Intl.PluralRulesConstructor);
}
pub fn @"%Intl.PluralRules.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.PluralRules.prototype%", builtins.Intl.PluralRulesPrototype);
}
pub fn @"%Intl.Segmenter%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.Segmenter%", builtins.Intl.SegmenterConstructor);
}
pub fn @"%Intl.Segmenter.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.Segmenter.prototype%", builtins.Intl.SegmenterPrototype);
}
pub fn @"%IntlSegmentsPrototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%IntlSegmentsPrototype%", builtins.Intl.IntlSegmentsPrototype);
}
pub fn @"%IntlSegmentIteratorPrototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%IntlSegmentIteratorPrototype%", builtins.Intl.IntlSegmentIteratorPrototype);
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
    if (intrinsic.ptr.isNull()) {
        const object_prototype = try @"%Object.prototype%"(self);
        const property_descriptor = object_prototype.data.property_storage.get(PropertyKey.from("toString"));
        intrinsic.* = property_descriptor.?.value.?.object;
    }
    return intrinsic.*;
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
pub fn @"%SharedArrayBuffer%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%SharedArrayBuffer%", builtins.SharedArrayBufferConstructor);
}
pub fn @"%SharedArrayBuffer.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%SharedArrayBuffer.prototype%", builtins.SharedArrayBufferPrototype);
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
pub fn @"%unescape%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%unescape%", builtins.global_functions.Unescape);
}
pub fn @"%URIError%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%URIError%", builtins.URIErrorConstructor);
}
pub fn @"%URIError.prototype%"(self: *Self) Allocator.Error!Object {
    return self.lazyIntrinsic("%URIError.prototype%", builtins.URIErrorPrototype);
}
