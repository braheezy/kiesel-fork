//! Fast path implementations for `Array.prototype` methods.

const std = @import("std");

const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Object = types.Object;
const Value = types.Value;
const isStrictlyEqual = types.isStrictlyEqual;
const sameValueZero = types.sameValueZero;

const FindViaPredicateDirection = @import("array.zig").FindViaPredicateDirection;
const FindViaPredicateResult = @import("array.zig").FindViaPredicateResult;

fn lastIndexOfScalarPos(comptime T: type, slice: []const T, start_index: usize, value: T) ?usize {
    var i: usize = start_index;
    while (true) : (i -= 1) {
        if (slice[i] == value) return i;
        if (i == 0) return null;
    }
}

fn toI32(value: Value) ?i32 {
    if (!value.isNumber()) return null;
    switch (value.asNumber()) {
        .i32 => |x| return x,
        .f64 => |x| {
            if (!std.math.isFinite(x) or
                x < std.math.minInt(i32) or
                x > std.math.maxInt(i32) or
                @trunc(x) != x) return null;
            return @intFromFloat(x);
        },
    }
}

fn toF64(value: Value) ?f64 {
    if (!value.isNumber()) return null;
    return value.asNumber().asFloat();
}

fn cb(
    agent: *Agent,
    object: *Object,
    callback: Value,
    this_arg: Value,
    value: Value,
    index: usize,
) Agent.Error!void {
    _ = try callback.callAssumeCallable(
        agent,
        this_arg,
        &.{ value, Value.from(@as(u53, @intCast(index))), Value.from(object) },
    );
}

fn cbToBool(
    agent: *Agent,
    object: *Object,
    callback: Value,
    this_arg: Value,
    value: Value,
    index: usize,
) Agent.Error!bool {
    const result = try callback.callAssumeCallable(
        agent,
        this_arg,
        &.{ value, Value.from(@as(u53, @intCast(index))), Value.from(object) },
    );
    return result.toBoolean();
}

/// Fast path for `Array.prototype.every()`.
///
/// Only applicable to objects that meet the following requirements:
/// - Dense indexed property storage with at least `len` items
/// - Ordinary internal methods: `[[HasProperty]]`, `[[Get]]`
///
/// If the indexed property storage is modified in a way that changes its type or size iteration
/// will continue on the slow path from the returned index.
pub fn every(
    agent: *Agent,
    object: *Object,
    len: u53,
    callback: Value,
    this_arg: Value,
) Agent.Error!?union(enum) {
    done: bool,
    continue_slow: usize,
} {
    const has_ordinary_internal_methods = object.internal_methods.flags.supersetOf(comptime .initMany(&.{
        .ordinary_has_property,
        .ordinary_get,
        // Dependencies of ordinary [[HasProperty]] and [[Get]]
        .ordinary_get_own_property,
        .ordinary_get_prototype_of,
    }));
    if (!has_ordinary_internal_methods or
        object.property_storage.indexed_properties.count() < len) return null;

    const end_index: usize = @intCast(len);
    switch (object.property_storage.indexed_properties.storage) {
        .none => {},
        .dense_i32 => |*dense_i32| for (0..end_index) |index| {
            const value = Value.from(dense_i32.items[index]);
            if (!try cbToBool(agent, object, callback, this_arg, value, index)) {
                return .{ .done = false };
            }
            if (object.property_storage.indexed_properties.storage != .dense_i32 or
                dense_i32.items.len < end_index)
                return .{ .continue_slow = index + 1 };
        },
        .dense_f64 => |*dense_f64| for (0..end_index) |index| {
            const value = Value.from(dense_f64.items[index]);
            if (!try cbToBool(agent, object, callback, this_arg, value, index)) {
                return .{ .done = false };
            }
            if (object.property_storage.indexed_properties.storage != .dense_f64 or
                dense_f64.items.len < end_index)
                return .{ .continue_slow = index + 1 };
        },
        .dense_value => |*dense_value| for (0..end_index) |index| {
            const value = dense_value.items[index];
            if (!try cbToBool(agent, object, callback, this_arg, value, index)) {
                return .{ .done = false };
            }
            if (object.property_storage.indexed_properties.storage != .dense_value or
                dense_value.items.len < end_index)
                return .{ .continue_slow = index + 1 };
        },
        .sparse_value, .sparse_property_descriptor => return null,
    }
    return .{ .done = true };
}

/// Fast path for `Array.prototype.fill()`.
///
/// Only applicable to objects that meet the following requirements:
/// - Dense indexed property storage after migration for the given `value`
/// - Ordinary internal methods: `[[Set]]`
/// - If the length is non-zero and the object has no property storage yet the object must be
///   extensible
pub fn fill(
    allocator: std.mem.Allocator,
    object: *Object,
    len: u53,
    start: u53,
    end: u53,
    value: Value,
) std.mem.Allocator.Error!?void {
    const has_ordinary_internal_methods = object.internal_methods.flags.supersetOf(comptime .initMany(&.{
        .ordinary_set,
        // Dependencies of ordinary [[Set]]
        .ordinary_get_own_property,
        .ordinary_get_prototype_of,
        .ordinary_is_extensible,
        .ordinary_define_own_property,
    }));
    if (!has_ordinary_internal_methods or
        start > std.math.maxInt(Object.IndexedProperties.Index) or
        end > std.math.maxInt(Object.IndexedProperties.Index)) return null;
    if (len > 0 and
        object.property_storage.indexed_properties.storage == .none and
        !object.extensible()) return null;

    if (start >= end) return;
    const start_index: usize = @intCast(start);
    const end_index: usize = @intCast(@min(end, len));
    try object.property_storage.indexed_properties.migrateStorageIfNeeded(allocator, 0, .{
        .value_or_accessor = .{ .value = value },
        .attributes = .all,
    });
    switch (object.property_storage.indexed_properties.storage) {
        .none => unreachable,
        .dense_i32 => |*dense_i32| {
            try dense_i32.ensureTotalCapacity(allocator, end_index);
            if (dense_i32.items.len < end_index) dense_i32.items.len = end_index;
            @memset(dense_i32.items[start_index..end_index], value.__asI32());
        },
        .dense_f64 => |*dense_f64| {
            try dense_f64.ensureTotalCapacity(allocator, end_index);
            if (dense_f64.items.len < end) dense_f64.items.len = end_index;
            @memset(dense_f64.items[start_index..end_index], value.__toF64());
        },
        .dense_value => |*dense_value| {
            try dense_value.ensureTotalCapacity(allocator, end_index);
            if (dense_value.items.len < end) dense_value.items.len = end_index;
            @memset(dense_value.items[start_index..end_index], value);
        },
        .sparse_value, .sparse_property_descriptor => return null,
    }
}

/// Fast path for `findViaPredicate()`, i.e. any of the following:
/// - `Array.prototype.find()`
/// - `Array.prototype.findIndex()`
/// - `Array.prototype.findLast()`
/// - `Array.prototype.findLastIndex()`
///
/// Only applicable to objects that meet the following requirements:
/// - Dense indexed property storage with at least `len` items
/// - Ordinary internal methods: `[[Get]]`
///
/// If the indexed property storage is modified in a way that changes its type or size iteration
/// will continue on the slow path from the returned index.
pub fn findViaPredicate(
    agent: *Agent,
    object: *Object,
    len: u53,
    comptime direction: FindViaPredicateDirection,
    predicate: Value,
    this_arg: Value,
) Agent.Error!?union(enum) {
    done: FindViaPredicateResult,
    continue_slow: ?usize,
} {
    const has_ordinary_internal_methods = object.internal_methods.flags.supersetOf(comptime .initMany(&.{
        .ordinary_get,
        // Dependencies of ordinary [[Get]]
        .ordinary_get_own_property,
        .ordinary_get_prototype_of,
    }));
    if (!has_ordinary_internal_methods or
        object.property_storage.indexed_properties.count() < len) return null;

    const end_index: usize = @intCast(len);
    switch (object.property_storage.indexed_properties.storage) {
        .none => {},
        .dense_i32 => |*dense_i32| switch (direction) {
            .ascending => for (0..end_index) |index| {
                const value = Value.from(dense_i32.items[index]);
                if (try cbToBool(agent, object, predicate, this_arg, value, index)) {
                    return .{ .done = .{ .index = Value.from(@as(u53, @intCast(index))), .value = value } };
                }
                if (object.property_storage.indexed_properties.storage != .dense_i32 or
                    dense_i32.items.len < end_index)
                    return .{ .continue_slow = index + 1 };
            },
            .descending => for (0..end_index) |tmp| {
                const index = end_index - tmp - 1;
                const value = Value.from(dense_i32.items[index]);
                if (try cbToBool(agent, object, predicate, this_arg, value, index)) {
                    return .{ .done = .{ .index = Value.from(@as(u53, @intCast(index))), .value = value } };
                }
                if (object.property_storage.indexed_properties.storage != .dense_i32 or
                    dense_i32.items.len < end_index)
                    return .{ .continue_slow = std.math.sub(usize, index, 1) catch null };
            },
        },
        .dense_f64 => |*dense_f64| switch (direction) {
            .ascending => for (0..end_index) |index| {
                const value = Value.from(dense_f64.items[index]);
                if (try cbToBool(agent, object, predicate, this_arg, value, index)) {
                    return .{ .done = .{ .index = Value.from(@as(u53, @intCast(index))), .value = value } };
                }
                if (object.property_storage.indexed_properties.storage != .dense_f64 or
                    dense_f64.items.len < end_index)
                    return .{ .continue_slow = index + 1 };
            },
            .descending => for (0..end_index) |tmp| {
                const index = end_index - tmp - 1;
                const value = Value.from(dense_f64.items[index]);
                if (try cbToBool(agent, object, predicate, this_arg, value, index)) {
                    return .{ .done = .{ .index = Value.from(@as(u53, @intCast(index))), .value = value } };
                }
                if (object.property_storage.indexed_properties.storage != .dense_f64 or
                    dense_f64.items.len < end_index)
                    return .{ .continue_slow = std.math.sub(usize, index, 1) catch null };
            },
        },
        .dense_value => |*dense_value| switch (direction) {
            .ascending => for (0..end_index) |index| {
                const value = dense_value.items[index];
                if (try cbToBool(agent, object, predicate, this_arg, value, index)) {
                    return .{ .done = .{ .index = Value.from(@as(u53, @intCast(index))), .value = value } };
                }
                if (object.property_storage.indexed_properties.storage != .dense_value or
                    dense_value.items.len < end_index)
                    return .{ .continue_slow = index + 1 };
            },
            .descending => for (0..end_index) |tmp| {
                const index = end_index - tmp - 1;
                const value = dense_value.items[index];
                if (try cbToBool(agent, object, predicate, this_arg, value, index)) {
                    return .{ .done = .{ .index = Value.from(@as(u53, @intCast(index))), .value = value } };
                }
                if (object.property_storage.indexed_properties.storage != .dense_value or
                    dense_value.items.len < end_index)
                    return .{ .continue_slow = std.math.sub(usize, index, 1) catch null };
            },
        },
        .sparse_value, .sparse_property_descriptor => return null,
    }
    return .{ .done = .{ .index = Value.from(-1), .value = .undefined } };
}

/// Fast path for `Array.prototype.forEach()`.
///
/// Only applicable to objects that meet the following requirements:
/// - Dense indexed property storage with at least `len` items
/// - Ordinary internal methods: `[[HasProperty]]`, `[[Get]]`
///
/// If the indexed property storage is modified in a way that changes its type or size iteration
/// will continue on the slow path from the returned index.
pub fn forEach(
    agent: *Agent,
    object: *Object,
    len: u53,
    callback: Value,
    this_arg: Value,
) Agent.Error!?union(enum) {
    done,
    continue_slow: usize,
} {
    const has_ordinary_internal_methods = object.internal_methods.flags.supersetOf(comptime .initMany(&.{
        .ordinary_has_property,
        .ordinary_get,
        // Dependencies of ordinary [[HasProperty]] and [[Get]]
        .ordinary_get_own_property,
        .ordinary_get_prototype_of,
    }));
    if (!has_ordinary_internal_methods or
        object.property_storage.indexed_properties.count() < len) return null;

    const end_index: usize = @intCast(len);
    switch (object.property_storage.indexed_properties.storage) {
        .none => {},
        .dense_i32 => |*dense_i32| for (0..end_index) |index| {
            const value = Value.from(dense_i32.items[index]);
            try cb(agent, object, callback, this_arg, value, index);
            if (object.property_storage.indexed_properties.storage != .dense_i32 or
                dense_i32.items.len < end_index)
                return .{ .continue_slow = index + 1 };
        },
        .dense_f64 => |*dense_f64| for (0..end_index) |index| {
            const value = Value.from(dense_f64.items[index]);
            try cb(agent, object, callback, this_arg, value, index);
            if (object.property_storage.indexed_properties.storage != .dense_f64 or
                dense_f64.items.len < end_index)
                return .{ .continue_slow = index + 1 };
        },
        .dense_value => |*dense_value| for (0..end_index) |index| {
            const value = dense_value.items[index];
            try cb(agent, object, callback, this_arg, value, index);
            if (object.property_storage.indexed_properties.storage != .dense_value or
                dense_value.items.len < end_index)
                return .{ .continue_slow = index + 1 };
        },
        .sparse_value, .sparse_property_descriptor => return null,
    }
    return .done;
}

/// Fast path for `Array.prototype.includes()`.
///
/// Only applicable to objects that meet the following requirements:
/// - Dense indexed property storage with at least `len` items
/// - Ordinary internal methods: `[[Get]]`
pub fn includes(object: *Object, len: u53, from_index: u53, search_element: Value) ?bool {
    const has_ordinary_internal_methods = object.internal_methods.flags.supersetOf(comptime .initMany(&.{
        .ordinary_get,
        // Dependencies of ordinary [[Get]]
        .ordinary_get_own_property,
        .ordinary_get_prototype_of,
    }));
    if (!has_ordinary_internal_methods or
        object.property_storage.indexed_properties.count() < len or
        from_index > std.math.maxInt(Object.IndexedProperties.Index)) return null;

    if (from_index >= len) return false;
    const start_index: usize = @intCast(from_index);
    const end_index: usize = @intCast(len);
    switch (object.property_storage.indexed_properties.storage) {
        .none => {},
        .dense_i32 => |dense_i32| {
            const search_element_i32 = toI32(search_element) orelse return false;
            return std.mem.indexOfScalarPos(
                i32,
                dense_i32.items[0..end_index],
                start_index,
                search_element_i32,
            ) != null;
        },
        .dense_f64 => |dense_f64| {
            const search_element_f64 = toF64(search_element) orelse return false;
            // `sameValueZero()` treats NaN as equal to NaN, so this needs special handling.
            if (std.math.isNan(search_element_f64)) {
                for (dense_f64.items[start_index..end_index]) |value| {
                    if (std.math.isNan(value)) return true;
                }
                return false;
            }
            return std.mem.indexOfScalarPos(
                f64,
                dense_f64.items[0..end_index],
                start_index,
                search_element_f64,
            ) != null;
        },
        .dense_value => |dense_value| {
            for (dense_value.items[start_index..end_index]) |value| {
                if (sameValueZero(search_element, value)) return true;
            }
        },
        .sparse_value, .sparse_property_descriptor => return null,
    }
    return false;
}

/// Fast path for `Array.prototype.indexOf()`.
///
/// Only applicable to objects that meet the following requirements:
/// - Dense indexed property storage with at least `len` items
/// - Ordinary internal methods: `[[HasProperty]]`, `[[Get]]`
pub fn indexOf(object: *Object, len: u53, from_index: u53, search_element: Value) ?Value {
    const has_ordinary_internal_methods = object.internal_methods.flags.supersetOf(comptime .initMany(&.{
        .ordinary_has_property,
        .ordinary_get,
        // Dependencies of ordinary [[HasProperty]] and [[Get]]
        .ordinary_get_own_property,
        .ordinary_get_prototype_of,
    }));
    if (!has_ordinary_internal_methods or
        object.property_storage.indexed_properties.count() < len or
        from_index > std.math.maxInt(Object.IndexedProperties.Index)) return null;

    if (from_index >= len) return Value.from(-1);
    const start_index: usize = @intCast(from_index);
    const end_index: usize = @intCast(len);
    switch (object.property_storage.indexed_properties.storage) {
        .none => {},
        .dense_i32 => |dense_i32| {
            const search_element_i32 = toI32(search_element) orelse return Value.from(-1);
            if (std.mem.indexOfScalarPos(
                i32,
                dense_i32.items[0..end_index],
                start_index,
                search_element_i32,
            )) |index| {
                return Value.from(@as(u53, @intCast(index)));
            }
        },
        .dense_f64 => |dense_f64| {
            const search_element_f64 = toF64(search_element) orelse return Value.from(-1);
            if (std.mem.indexOfScalarPos(
                f64,
                dense_f64.items[0..end_index],
                start_index,
                search_element_f64,
            )) |index| {
                return Value.from(@as(u53, @intCast(index)));
            }
        },
        .dense_value => |dense_value| {
            for (dense_value.items[start_index..end_index], start_index..) |element, index| {
                if (isStrictlyEqual(search_element, element)) {
                    return Value.from(@as(u53, @intCast(index)));
                }
            }
        },
        .sparse_value, .sparse_property_descriptor => return null,
    }
    return Value.from(-1);
}

/// Fast path for `Array.prototype.lastIndexOf()`.
///
/// Only applicable to objects that meet the following requirements:
/// - Dense indexed property storage with at least `len` items
/// - Ordinary internal methods: `[[HasProperty]]`, `[[Get]]`
pub fn lastIndexOf(object: *Object, len: u53, from_index: u53, search_element: Value) ?Value {
    const has_ordinary_internal_methods = object.internal_methods.flags.supersetOf(comptime .initMany(&.{
        .ordinary_has_property,
        .ordinary_get,
        // Dependencies of ordinary [[HasProperty]] and [[Get]]
        .ordinary_get_own_property,
        .ordinary_get_prototype_of,
    }));
    if (!has_ordinary_internal_methods or
        object.property_storage.indexed_properties.count() < len or
        from_index > std.math.maxInt(Object.IndexedProperties.Index)) return null;

    if (from_index >= len) return Value.from(-1);
    const start_index: usize = @intCast(from_index);
    switch (object.property_storage.indexed_properties.storage) {
        .none => {},
        .dense_i32 => |dense_i32| {
            const search_element_i32 = toI32(search_element) orelse return Value.from(-1);
            if (lastIndexOfScalarPos(
                i32,
                dense_i32.items,
                start_index,
                search_element_i32,
            )) |index| {
                return Value.from(@as(u53, @intCast(index)));
            }
        },
        .dense_f64 => |dense_f64| {
            const search_element_f64 = toF64(search_element) orelse return Value.from(-1);
            if (lastIndexOfScalarPos(
                f64,
                dense_f64.items,
                start_index,
                search_element_f64,
            )) |index| {
                return Value.from(@as(u53, @intCast(index)));
            }
        },
        .dense_value => |dense_value| {
            for (0..start_index + 1) |tmp| {
                const index = start_index - tmp;
                const element = dense_value.items[index];
                if (isStrictlyEqual(search_element, element)) {
                    return Value.from(@as(u53, @intCast(index)));
                }
            }
        },
        .sparse_value, .sparse_property_descriptor => return null,
    }
    return Value.from(-1);
}

/// Fast path for `Array.prototype.reverse()`.
///
/// Only applicable to objects that meet the following requirements:
/// - Dense indexed property storage with exactly `len` items
/// - Ordinary internal methods: `[[HasProperty]]`, `[[Get]]`, `[[Set]]`, `[[Delete]]`
pub fn reverse(object: *Object, len: u53) ?void {
    const has_ordinary_internal_methods = object.internal_methods.flags.supersetOf(comptime .initMany(&.{
        .ordinary_has_property,
        .ordinary_get,
        .ordinary_set,
        .ordinary_delete,
        // Dependencies of ordinary [[HasProperty]], [[Get]], [[Set]], and [[Delete]]
        .ordinary_get_own_property,
        .ordinary_get_prototype_of,
        .ordinary_is_extensible,
        .ordinary_define_own_property,
    }));
    if (!has_ordinary_internal_methods or
        object.property_storage.indexed_properties.count() != len) return null;

    switch (object.property_storage.indexed_properties.storage) {
        .none => {},
        .dense_i32 => |dense_i32| {
            std.mem.reverse(i32, dense_i32.items);
        },
        .dense_f64 => |dense_f64| {
            std.mem.reverse(f64, dense_f64.items);
        },
        .dense_value => |dense_value| {
            std.mem.reverse(Value, dense_value.items);
        },
        .sparse_value, .sparse_property_descriptor => return null,
    }
}

/// Fast path for `Array.prototype.some()`.
///
/// Only applicable to objects that meet the following requirements:
/// - Dense indexed property storage with at least `len` items
/// - Ordinary internal methods: `[[HasProperty]]`, `[[Get]]`
///
/// If the indexed property storage is modified in a way that changes its type or size iteration
/// will continue on the slow path from the returned index.
pub fn some(
    agent: *Agent,
    object: *Object,
    len: u53,
    callback: Value,
    this_arg: Value,
) Agent.Error!?union(enum) {
    done: bool,
    continue_slow: usize,
} {
    const has_ordinary_internal_methods = object.internal_methods.flags.supersetOf(comptime .initMany(&.{
        .ordinary_has_property,
        .ordinary_get,
        // Dependencies of ordinary [[HasProperty]] and [[Get]]
        .ordinary_get_own_property,
        .ordinary_get_prototype_of,
    }));
    if (!has_ordinary_internal_methods or
        object.property_storage.indexed_properties.count() < len) return null;

    const end_index: usize = @intCast(len);
    switch (object.property_storage.indexed_properties.storage) {
        .none => {},
        .dense_i32 => |*dense_i32| for (0..end_index) |index| {
            const value = Value.from(dense_i32.items[index]);
            if (try cbToBool(agent, object, callback, this_arg, value, index)) {
                return .{ .done = true };
            }
            if (object.property_storage.indexed_properties.storage != .dense_i32 or
                dense_i32.items.len < end_index)
                return .{ .continue_slow = index + 1 };
        },
        .dense_f64 => |*dense_f64| for (0..end_index) |index| {
            const value = Value.from(dense_f64.items[index]);
            if (try cbToBool(agent, object, callback, this_arg, value, index)) {
                return .{ .done = true };
            }
            if (object.property_storage.indexed_properties.storage != .dense_f64 or
                dense_f64.items.len < end_index)
                return .{ .continue_slow = index + 1 };
        },
        .dense_value => |*dense_value| for (0..end_index) |index| {
            const value = dense_value.items[index];
            if (try cbToBool(agent, object, callback, this_arg, value, index)) {
                return .{ .done = true };
            }
            if (object.property_storage.indexed_properties.storage != .dense_value or
                dense_value.items.len < end_index)
                return .{ .continue_slow = index + 1 };
        },
        .sparse_value, .sparse_property_descriptor => return null,
    }
    return .{ .done = false };
}
