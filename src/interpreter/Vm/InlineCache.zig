const std = @import("std");

const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Value = types.Value;

const InlineCache = @This();

const Entry = struct {
    shape: *Object.Shape,
    offset: Object.Shape.Property.Offset,
    type: Object.Shape.Property.Type,
};

const Entries = struct {
    items: [4]Entry,
    len: u3,
};

state: union(enum) {
    empty,
    monomorphic: Entry,
    polymorphic: Entries,
    megamorphic,
},

pub fn get(
    ic: *const InlineCache,
    agent: *Agent,
    base_object: *Object,
    base_value: Value,
) Agent.Error!?Value {
    const entry: *const Entry = switch (ic.state) {
        .empty, .megamorphic => return null,
        .monomorphic => |*entry| blk: {
            if (base_object.shape == entry.shape) break :blk entry;
            return null;
        },
        .polymorphic => |*entries| blk: {
            for (entries.items[0..entries.len]) |*entry| {
                if (base_object.shape == entry.shape) break :blk entry;
            }
            return null;
        },
    };

    const has_ordinary_internal_methods = base_object.internalMethods().flags.supersetOf(comptime .initMany(&.{
        .ordinary_get,
        .ordinary_get_own_property,
    }));
    // Assert IC update invariants are uphold
    std.debug.assert(has_ordinary_internal_methods);
    std.debug.assert(!base_object.shape.isUnique());

    switch (entry.type) {
        .value => return base_object.getValueAtPropertyOffset(entry.offset),
        .accessor => {
            const getter_value = base_object.getValueAtPropertyOffset(entry.offset);
            if (getter_value.isNull()) {
                @branchHint(.unlikely);
                return .undefined;
            }
            std.debug.assert(getter_value.isObject());
            return try getter_value.callAssumeCallable(agent, base_value, &.{});
        },
    }
}

pub fn set(
    ic: *const InlineCache,
    agent: *Agent,
    base_object: *Object,
    base_value: Value,
    value: Value,
) Agent.Error!bool {
    const entry: *const Entry = switch (ic.state) {
        .empty, .megamorphic => return false,
        .monomorphic => |*entry| blk: {
            if (base_object.shape == entry.shape) break :blk entry;
            return false;
        },
        .polymorphic => |*entries| blk: {
            for (entries.items[0..entries.len]) |*entry| {
                if (base_object.shape == entry.shape) break :blk entry;
            }
            return false;
        },
    };

    const has_ordinary_internal_methods = base_object.internalMethods().flags.supersetOf(comptime .initMany(&.{
        .ordinary_set,
        .ordinary_get_own_property,
    }));
    // Assert IC update invariants are uphold
    std.debug.assert(has_ordinary_internal_methods);
    std.debug.assert(!base_object.shape.isUnique());

    switch (entry.type) {
        .value => {
            base_object.setValueAtPropertyOffset(entry.offset, value);
            return true;
        },
        .accessor => {
            const setter_value = base_object.getValueAtPropertyOffset(
                @enumFromInt(@intFromEnum(entry.offset) + 1),
            );
            if (setter_value.isNull()) {
                @branchHint(.unlikely);
                return false;
            }
            std.debug.assert(setter_value.isObject());
            _ = try setter_value.callAssumeCallable(agent, base_value, &.{value});
            return true;
        },
    }
}

pub fn update(
    ic: *InlineCache,
    base_object: *Object,
    property_key: PropertyKey,
    comptime kind: enum { get, set },
) void {
    const has_ordinary_internal_methods = base_object.internalMethods().flags.supersetOf(comptime .initMany(&.{
        switch (kind) {
            .get => .ordinary_get,
            .set => .ordinary_set,
        },
        .ordinary_get_own_property,
    }));
    if (!has_ordinary_internal_methods) return;
    if (base_object.shape.isUnique()) return;

    const property = base_object.shape.properties.get(property_key) orelse return;
    if (kind == .set and property.type == .value and !property.attributes.writable) return;

    const new_entry: Entry = .{
        .shape = base_object.shape,
        .offset = property.offset,
        .type = property.type,
    };

    switch (ic.state) {
        .empty => {
            ic.state = .{ .monomorphic = new_entry };
        },
        .monomorphic => |*entry| {
            ic.state = .{ .polymorphic = .{
                .items = .{ entry.*, new_entry, undefined, undefined },
                .len = 2,
            } };
        },
        .polymorphic => |*entries| {
            if (entries.len < 4) {
                entries.items[entries.len] = new_entry;
                entries.len += 1;
            } else {
                ic.state = .megamorphic;
            }
        },
        .megamorphic => {},
    }
}
