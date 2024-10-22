//! 6.1.7.4 Well-Known Intrinsic Objects
//! https://tc39.es/ecma262/#sec-well-known-intrinsic-objects

const std = @import("std");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;

const Intrinsics = @This();

const null_object_data: *allowzero Object.Data = @ptrFromInt(0);

realm: *Realm,

// Not stored as top-level properties so we can have methods of the same names
lazy_intrinsics: struct {
    // Using a null ptr allows us to avoid using extra memory for optionals.
    const null_intrinsic: Object = .{ .data = null_object_data };

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
    @"%Iterator%": Object = null_intrinsic,
    @"%IteratorHelperPrototype%": Object = null_intrinsic,
    @"%Iterator.prototype%": Object = null_intrinsic,
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
    @"%WeakMap%": Object = null_intrinsic,
    @"%WeakMap.prototype%": Object = null_intrinsic,
    @"%WeakRef%": Object = null_intrinsic,
    @"%WeakRef.prototype%": Object = null_intrinsic,
    @"%WeakSet%": Object = null_intrinsic,
    @"%WeakSet.prototype%": Object = null_intrinsic,
    @"%WrapForValidIteratorPrototype%": Object = null_intrinsic,
} = .{},

fn lazyIntrinsic(
    self: *Intrinsics,
    comptime name: []const u8,
    comptime T: type,
) std.mem.Allocator.Error!Object {
    const intrinsic = &@field(self.lazy_intrinsics, name);
    if (intrinsic.data == null_object_data) {
        const object = try T.create(self.realm);
        // Sanity check to ensure there is no dependency loop - creating the object must not
        // (indirectly) rely on itself. If something within `create()` assigned the intrinsic it
        // has been created twice and overwriting it would be a mistake.
        std.debug.assert(intrinsic.data == null_object_data);
        intrinsic.* = object;
        try T.init(self.realm, object);
    }
    return intrinsic.*;
}

pub fn @"%AggregateError%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%AggregateError%", builtins.aggregate_error.constructor);
}
pub fn @"%AggregateError.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%AggregateError.prototype%", builtins.aggregate_error.prototype);
}
pub fn @"%Array%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Array%", builtins.array.constructor);
}
pub fn @"%Array.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Array.prototype%", builtins.array.prototype);
}
pub fn @"%Array.prototype.toString%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    const intrinsic = &self.lazy_intrinsics.@"%Array.prototype.toString%";
    if (intrinsic.data == null_object_data) {
        const array_prototype = try @"%Array.prototype%"(self);
        const property_descriptor = array_prototype.propertyStorage().get(PropertyKey.from("toString"));
        intrinsic.* = property_descriptor.?.value.?.asObject();
    }
    return intrinsic.*;
}
pub fn @"%Array.prototype.values%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    const intrinsic = &self.lazy_intrinsics.@"%Array.prototype.values%";
    if (intrinsic.data == null_object_data) {
        const array_prototype = try @"%Array.prototype%"(self);
        const property_descriptor = array_prototype.propertyStorage().get(PropertyKey.from("values"));
        intrinsic.* = property_descriptor.?.value.?.asObject();
    }
    return intrinsic.*;
}
pub fn @"%ArrayBuffer%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%ArrayBuffer%", builtins.array_buffer.constructor);
}
pub fn @"%ArrayBuffer.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%ArrayBuffer.prototype%", builtins.array_buffer.prototype);
}
pub fn @"%ArrayIteratorPrototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%ArrayIteratorPrototype%", builtins.array_iterator.prototype);
}
pub fn @"%AsyncFromSyncIteratorPrototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%AsyncFromSyncIteratorPrototype%", builtins.async_from_sync_iterator.prototype);
}
pub fn @"%AsyncFunction%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%AsyncFunction%", builtins.async_function.constructor);
}
pub fn @"%AsyncFunction.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%AsyncFunction.prototype%", builtins.async_function.prototype);
}
pub fn @"%AsyncGeneratorFunction%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%AsyncGeneratorFunction%", builtins.async_generator_function.constructor);
}
pub fn @"%AsyncGeneratorFunction.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%AsyncGeneratorFunction.prototype%", builtins.async_generator_function.prototype);
}
pub fn @"%AsyncGeneratorPrototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%AsyncGeneratorPrototype%", builtins.async_generator.prototype);
}
pub fn @"%AsyncIteratorPrototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%AsyncIteratorPrototype%", builtins.async_iterator.prototype);
}
pub fn @"%Atomics%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Atomics%", builtins.atomics.namespace);
}
pub fn @"%BigInt%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%BigInt%", builtins.big_int.constructor);
}
pub fn @"%BigInt.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%BigInt.prototype%", builtins.big_int.prototype);
}
pub fn @"%BigInt64Array%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%BigInt64Array%", builtins.typed_array.big_int64_array.constructor);
}
pub fn @"%BigInt64Array.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%BigInt64Array.prototype%", builtins.typed_array.big_int64_array.prototype);
}
pub fn @"%BigUint64Array%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%BigUint64Array%", builtins.typed_array.big_uint64_array.constructor);
}
pub fn @"%BigUint64Array.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%BigUint64Array.prototype%", builtins.typed_array.big_uint64_array.prototype);
}
pub fn @"%Boolean%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Boolean%", builtins.boolean.constructor);
}
pub fn @"%Boolean.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Boolean.prototype%", builtins.boolean.prototype);
}
pub fn @"%DataView%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%DataView%", builtins.data_view.constructor);
}
pub fn @"%DataView.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%DataView.prototype%", builtins.data_view.prototype);
}
pub fn @"%Date%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Date%", builtins.date.constructor);
}
pub fn @"%Date.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Date.prototype%", builtins.date.prototype);
}
pub fn @"%decodeURI%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%decodeURI%", builtins.global.decode_uri_function);
}
pub fn @"%decodeURIComponent%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%decodeURIComponent%", builtins.global.decode_uri_component_function);
}
pub fn @"%encodeURI%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%encodeURI%", builtins.global.encode_uri_function);
}
pub fn @"%encodeURIComponent%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%encodeURIComponent%", builtins.global.encode_uri_component_function);
}
pub fn @"%Error%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Error%", builtins.@"error".constructor);
}
pub fn @"%Error.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Error.prototype%", builtins.@"error".prototype);
}
pub fn @"%escape%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%escape%", builtins.global.escape_function);
}
pub fn @"%eval%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%eval%", builtins.global.eval_function);
}
pub fn @"%EvalError%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%EvalError%", builtins.@"error".eval_error.constructor);
}
pub fn @"%EvalError.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%EvalError.prototype%", builtins.@"error".eval_error.prototype);
}
pub fn @"%FinalizationRegistry%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%FinalizationRegistry%", builtins.finalization_registry.constructor);
}
pub fn @"%FinalizationRegistry.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%FinalizationRegistry.prototype%", builtins.finalization_registry.prototype);
}
pub fn @"%Float16Array%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Float16Array%", builtins.typed_array.float16_array.constructor);
}
pub fn @"%Float16Array.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Float16Array.prototype%", builtins.typed_array.float16_array.prototype);
}
pub fn @"%Float32Array%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Float32Array%", builtins.typed_array.float32_array.constructor);
}
pub fn @"%Float32Array.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Float32Array.prototype%", builtins.typed_array.float32_array.prototype);
}
pub fn @"%Float64Array%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Float64Array%", builtins.typed_array.float64_array.constructor);
}
pub fn @"%Float64Array.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Float64Array.prototype%", builtins.typed_array.float64_array.prototype);
}
pub fn @"%ForInIteratorPrototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%ForInIteratorPrototype%", builtins.for_in_iterator.prototype);
}
pub fn @"%Function%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Function%", builtins.function.constructor);
}
pub fn @"%Function.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Function.prototype%", builtins.function.prototype);
}
pub fn @"%GeneratorFunction%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%GeneratorFunction%", builtins.generator_function.constructor);
}
pub fn @"%GeneratorFunction.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%GeneratorFunction.prototype%", builtins.generator_function.prototype);
}
pub fn @"%GeneratorPrototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%GeneratorPrototype%", builtins.generator.prototype);
}
pub fn @"%Int8Array%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Int8Array%", builtins.typed_array.int8_array.constructor);
}
pub fn @"%Int8Array.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Int8Array.prototype%", builtins.typed_array.int8_array.prototype);
}
pub fn @"%Int16Array%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Int16Array%", builtins.typed_array.int16_array.constructor);
}
pub fn @"%Int16Array.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Int16Array.prototype%", builtins.typed_array.int16_array.prototype);
}
pub fn @"%Int32Array%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Int32Array%", builtins.typed_array.int32_array.constructor);
}
pub fn @"%Int32Array.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Int32Array.prototype%", builtins.typed_array.int32_array.prototype);
}
pub fn @"%Intl%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl%", builtins.intl.namespace);
}
pub fn @"%Intl.Collator%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.Collator%", builtins.intl.collator.constructor);
}
pub fn @"%Intl.Collator.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.Collator.prototype%", builtins.intl.collator.prototype);
}
pub fn @"%Intl.DateTimeFormat%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.DateTimeFormat%", builtins.intl.date_time_format.constructor);
}
pub fn @"%Intl.DateTimeFormat.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.DateTimeFormat.prototype%", builtins.intl.date_time_format.prototype);
}
pub fn @"%Intl.DisplayNames%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.DisplayNames%", builtins.intl.display_names.constructor);
}
pub fn @"%Intl.DisplayNames.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.DisplayNames.prototype%", builtins.intl.display_names.prototype);
}
pub fn @"%Intl.ListFormat%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.ListFormat%", builtins.intl.list_format.constructor);
}
pub fn @"%Intl.ListFormat.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.ListFormat.prototype%", builtins.intl.list_format.prototype);
}
pub fn @"%Intl.Locale%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.Locale%", builtins.intl.locale.constructor);
}
pub fn @"%Intl.Locale.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.Locale.prototype%", builtins.intl.locale.prototype);
}
pub fn @"%Intl.PluralRules%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.PluralRules%", builtins.intl.plural_rules.constructor);
}
pub fn @"%Intl.PluralRules.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.PluralRules.prototype%", builtins.intl.plural_rules.prototype);
}
pub fn @"%Intl.Segmenter%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.Segmenter%", builtins.intl.segmenter.constructor);
}
pub fn @"%Intl.Segmenter.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Intl.Segmenter.prototype%", builtins.intl.segmenter.prototype);
}
pub fn @"%IntlSegmentsPrototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%IntlSegmentsPrototype%", builtins.intl.segments.prototype);
}
pub fn @"%IntlSegmentIteratorPrototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%IntlSegmentIteratorPrototype%", builtins.intl.segment_iterator.prototype);
}
pub fn @"%isFinite%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%isFinite%", builtins.global.is_finite_function);
}
pub fn @"%isNaN%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%isNaN%", builtins.global.is_nan_function);
}
pub fn @"%Iterator%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Iterator%", builtins.iterator.constructor);
}
pub fn @"%IteratorHelperPrototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%IteratorHelperPrototype%", builtins.iterator_helper.prototype);
}
pub fn @"%Iterator.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Iterator.prototype%", builtins.iterator.prototype);
}
pub fn @"%JSON%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%JSON%", builtins.json.namespace);
}
pub fn @"%Map%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Map%", builtins.map.constructor);
}
pub fn @"%Map.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Map.prototype%", builtins.map.prototype);
}
pub fn @"%MapIteratorPrototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%MapIteratorPrototype%", builtins.map_iterator.prototype);
}
pub fn @"%Math%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Math%", builtins.math.namespace);
}
pub fn @"%Number%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Number%", builtins.number.constructor);
}
pub fn @"%Number.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Number.prototype%", builtins.number.prototype);
}
pub fn @"%Object%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Object%", builtins.object.constructor);
}
pub fn @"%Object.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Object.prototype%", builtins.object.prototype);
}
pub fn @"%Object.prototype.toString%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    const intrinsic = &self.lazy_intrinsics.@"%Object.prototype.toString%";
    if (intrinsic.data == null_object_data) {
        const object_prototype = try @"%Object.prototype%"(self);
        const property_descriptor = object_prototype.propertyStorage().get(PropertyKey.from("toString"));
        intrinsic.* = property_descriptor.?.value.?.asObject();
    }
    return intrinsic.*;
}
pub fn @"%parseFloat%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%parseFloat%", builtins.global.parse_float_function);
}
pub fn @"%parseInt%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%parseInt%", builtins.global.parse_int_function);
}
pub fn @"%Promise%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Promise%", builtins.promise.constructor);
}
pub fn @"%Promise.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Promise.prototype%", builtins.promise.prototype);
}
pub fn @"%Proxy%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Proxy%", builtins.proxy.constructor);
}
pub fn @"%RangeError%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%RangeError%", builtins.@"error".range_error.constructor);
}
pub fn @"%RangeError.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%RangeError.prototype%", builtins.@"error".range_error.prototype);
}
pub fn @"%ReferenceError%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%ReferenceError%", builtins.@"error".reference_error.constructor);
}
pub fn @"%ReferenceError.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%ReferenceError.prototype%", builtins.@"error".reference_error.prototype);
}
pub fn @"%Reflect%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Reflect%", builtins.reflect.namespace);
}
pub fn @"%RegExp%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%RegExp%", builtins.reg_exp.constructor);
}
pub fn @"%RegExp.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%RegExp.prototype%", builtins.reg_exp.prototype);
}
pub fn @"%RegExpStringIteratorPrototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%RegExpStringIteratorPrototype%", builtins.reg_exp_string_iterator.prototype);
}
pub fn @"%Set%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Set%", builtins.set.constructor);
}
pub fn @"%Set.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Set.prototype%", builtins.set.prototype);
}
pub fn @"%SetIteratorPrototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%SetIteratorPrototype%", builtins.set_iterator.prototype);
}
pub fn @"%SharedArrayBuffer%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%SharedArrayBuffer%", builtins.shared_array_buffer.constructor);
}
pub fn @"%SharedArrayBuffer.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%SharedArrayBuffer.prototype%", builtins.shared_array_buffer.prototype);
}
pub fn @"%String%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%String%", builtins.string.constructor);
}
pub fn @"%String.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%String.prototype%", builtins.string.prototype);
}
pub fn @"%StringIteratorPrototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%StringIteratorPrototype%", builtins.string_iterator.prototype);
}
pub fn @"%Symbol%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Symbol%", builtins.symbol.constructor);
}
pub fn @"%Symbol.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Symbol.prototype%", builtins.symbol.prototype);
}
pub fn @"%SyntaxError%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%SyntaxError%", builtins.@"error".syntax_error.constructor);
}
pub fn @"%SyntaxError.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%SyntaxError.prototype%", builtins.@"error".syntax_error.prototype);
}
pub fn @"%ThrowTypeError%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%ThrowTypeError%", builtins.throw_type_error.function);
}
pub fn @"%TypedArray%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%TypedArray%", builtins.typed_array.constructor);
}
pub fn @"%TypedArray.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%TypedArray.prototype%", builtins.typed_array.prototype);
}
pub fn @"%TypeError%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%TypeError%", builtins.@"error".type_error.constructor);
}
pub fn @"%TypeError.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%TypeError.prototype%", builtins.@"error".type_error.prototype);
}
pub fn @"%Uint8Array%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Uint8Array%", builtins.typed_array.uint8_array.constructor);
}
pub fn @"%Uint8Array.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Uint8Array.prototype%", builtins.typed_array.uint8_array.prototype);
}
pub fn @"%Uint8ClampedArray%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Uint8ClampedArray%", builtins.typed_array.uint8_clamped_array.constructor);
}
pub fn @"%Uint8ClampedArray.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Uint8ClampedArray.prototype%", builtins.typed_array.uint8_clamped_array.prototype);
}
pub fn @"%Uint16Array%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Uint16Array%", builtins.typed_array.uint16_array.constructor);
}
pub fn @"%Uint16Array.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Uint16Array.prototype%", builtins.typed_array.uint16_array.prototype);
}
pub fn @"%Uint32Array%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Uint32Array%", builtins.typed_array.uint32_array.constructor);
}
pub fn @"%Uint32Array.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%Uint32Array.prototype%", builtins.typed_array.uint32_array.prototype);
}
pub fn @"%unescape%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%unescape%", builtins.global.unescape_function);
}
pub fn @"%URIError%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%URIError%", builtins.@"error".uri_error.constructor);
}
pub fn @"%URIError.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%URIError.prototype%", builtins.@"error".uri_error.prototype);
}
pub fn @"%WeakMap%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%WeakMap%", builtins.weak_map.constructor);
}
pub fn @"%WeakMap.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%WeakMap.prototype%", builtins.weak_map.prototype);
}
pub fn @"%WeakRef%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%WeakRef%", builtins.weak_ref.constructor);
}
pub fn @"%WeakRef.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%WeakRef.prototype%", builtins.weak_ref.prototype);
}
pub fn @"%WeakSet%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%WeakSet%", builtins.weak_set.constructor);
}
pub fn @"%WeakSet.prototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%WeakSet.prototype%", builtins.weak_set.prototype);
}
pub fn @"%WrapForValidIteratorPrototype%"(self: *Intrinsics) std.mem.Allocator.Error!Object {
    return self.lazyIntrinsic("%WrapForValidIteratorPrototype%", builtins.wrap_for_valid_iterator.prototype);
}
