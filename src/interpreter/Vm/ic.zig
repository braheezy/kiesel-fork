const std = @import("std");

const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Value = types.Value;

fn State(comptime Entry: type, comptime Key: type) type {
    return union(enum) {
        empty,
        monomorphic: Entry,
        polymorphic: Entries,
        megamorphic,

        const max_entries = 4;

        const Entries = struct {
            items: [max_entries]Entry,
            len: std.math.IntFittingRange(0, max_entries),
        };

        fn get(state: *const @This(), key: Key) ?*const Entry {
            switch (state.*) {
                .empty, .megamorphic => return null,
                .monomorphic => |*entry| return if (entry.matches(key)) entry else null,
                .polymorphic => |*entries| {
                    for (entries.items[0..entries.len]) |*entry| {
                        if (entry.matches(key)) return entry;
                    }
                    return null;
                },
            }
        }

        fn add(state: *@This(), new_entry: Entry) void {
            switch (state.*) {
                .empty => {
                    state.* = .{ .monomorphic = new_entry };
                },
                .monomorphic => |*entry| {
                    var items: [max_entries]Entry = undefined;
                    items[0] = entry.*;
                    items[1] = new_entry;
                    state.* = .{ .polymorphic = .{
                        .items = items,
                        .len = 2,
                    } };
                },
                .polymorphic => |*entries| {
                    if (entries.len < max_entries) {
                        entries.items[entries.len] = new_entry;
                        entries.len += 1;
                    } else {
                        state.* = .megamorphic;
                    }
                },
                .megamorphic => {},
            }
        }
    };
}

pub const GetProperty = struct {
    const Entry = struct {
        shape: *const Object.Shape,
        offset: Object.Shape.Property.Offset,
        type: Object.Shape.Property.Type,

        fn matches(entry: *const Entry, shape: Key) bool {
            return entry.shape == shape;
        }
    };
    const Key = *const Object.Shape;

    state: State(Entry, Key),

    pub const empty: GetProperty = .{ .state = .empty };

    pub fn get(
        ic: *const GetProperty,
        agent: *Agent,
        base_object: *Object,
        base_value: Value,
    ) Agent.Error!?Value {
        const entry = ic.state.get(base_object.shape) orelse return null;

        const has_ordinary_internal_methods = base_object.internalMethods().flags.supersetOf(comptime .initMany(&.{
            .ordinary_get,
            .ordinary_get_own_property,
        }));
        // Assert IC update invariants are upheld
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

    pub fn update(
        ic: *GetProperty,
        base_object: *Object,
        property_key: PropertyKey,
    ) void {
        const has_ordinary_internal_methods = base_object.internalMethods().flags.supersetOf(comptime .initMany(&.{
            .ordinary_get,
            .ordinary_get_own_property,
        }));
        if (!has_ordinary_internal_methods) return;
        if (base_object.shape.isUnique()) return;

        const property = base_object.shape.properties.get(property_key) orelse return;

        ic.state.add(.{
            .shape = base_object.shape,
            .offset = property.offset,
            .type = property.type,
        });
    }
};

pub const SetProperty = struct {
    const Entry = struct {
        shape: *const Object.Shape,
        offset: Object.Shape.Property.Offset,
        type: Object.Shape.Property.Type,

        fn matches(entry: *const Entry, shape: Key) bool {
            return entry.shape == shape;
        }
    };
    const Key = *const Object.Shape;

    state: State(Entry, Key),

    pub const empty: SetProperty = .{ .state = .empty };

    pub fn set(
        ic: *const SetProperty,
        agent: *Agent,
        base_object: *Object,
        base_value: Value,
        value: Value,
    ) Agent.Error!bool {
        const entry = ic.state.get(base_object.shape) orelse return false;

        const has_ordinary_internal_methods = base_object.internalMethods().flags.supersetOf(comptime .initMany(&.{
            .ordinary_set,
            .ordinary_get_own_property,
        }));
        // Assert IC update invariants are upheld
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
        ic: *SetProperty,
        base_object: *Object,
        property_key: PropertyKey,
    ) void {
        const has_ordinary_internal_methods = base_object.internalMethods().flags.supersetOf(comptime .initMany(&.{
            .ordinary_set,
            .ordinary_get_own_property,
        }));
        if (!has_ordinary_internal_methods) return;
        if (base_object.shape.isUnique()) return;

        const property = base_object.shape.properties.get(property_key) orelse return;
        if (property.type == .value and !property.attributes.writable) return;

        ic.state.add(.{
            .shape = base_object.shape,
            .offset = property.offset,
            .type = property.type,
        });
    }
};
