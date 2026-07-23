const std = @import("std");

const execution = @import("../../execution.zig");
const types = @import("../../types.zig");
const utils = @import("../../utils.zig");

const Agent = execution.Agent;
const BoundedArray = utils.BoundedArray;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Value = types.Value;

fn State(comptime Entry: type, comptime Key: type) type {
    return union(enum) {
        empty,
        monomorphic: Entry,
        polymorphic: Entries,
        megamorphic,

        const Entries = BoundedArray(Entry, 4);

        fn get(state: *const @This(), key: Key) ?*const Entry {
            switch (state.*) {
                .empty, .megamorphic => return null,
                .monomorphic => |*entry| return if (entry.matches(key)) entry else null,
                .polymorphic => |*entries| {
                    for (entries.slice()) |*entry| {
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
                    var entries: Entries = .empty;
                    entries.append(entry.*) catch unreachable;
                    entries.append(new_entry) catch unreachable;
                    state.* = .{ .polymorphic = entries };
                },
                .polymorphic => |*entries| {
                    entries.append(new_entry) catch {
                        state.* = .megamorphic;
                    };
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

pub const GetPropertyComputed = struct {
    const Entry = struct {
        shape: *const Object.Shape,
        property_key: PropertyKey,
        offset: Object.Shape.Property.Offset,
        type: Object.Shape.Property.Type,

        fn matches(entry: *const Entry, key: Key) bool {
            return entry.shape == key.shape and entry.property_key.eql(key.property_key);
        }
    };
    const Key = struct {
        shape: *const Object.Shape,
        property_key: PropertyKey,
    };

    state: State(Entry, Key),

    pub const empty: GetPropertyComputed = .{ .state = .empty };

    pub fn get(
        ic: *const GetPropertyComputed,
        agent: *Agent,
        base_object: *Object,
        base_value: Value,
        property_key: PropertyKey,
    ) Agent.Error!?Value {
        if (property_key == .integer_index) return null;
        const key: Key = .{ .shape = base_object.shape, .property_key = property_key };
        const entry = ic.state.get(key) orelse return null;

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
        ic: *GetPropertyComputed,
        base_object: *Object,
        property_key: PropertyKey,
    ) void {
        // Indices go through their own indexed property fast path
        if (property_key == .integer_index) return;

        const has_ordinary_internal_methods = base_object.internalMethods().flags.supersetOf(comptime .initMany(&.{
            .ordinary_get,
            .ordinary_get_own_property,
        }));
        if (!has_ordinary_internal_methods) return;
        if (base_object.shape.isUnique()) return;

        const property = base_object.shape.properties.get(property_key) orelse return;

        ic.state.add(.{
            .shape = base_object.shape,
            .property_key = property_key,
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

pub const SetPropertyComputed = struct {
    const Entry = struct {
        shape: *const Object.Shape,
        property_key: PropertyKey,
        offset: Object.Shape.Property.Offset,
        type: Object.Shape.Property.Type,

        fn matches(entry: *const Entry, key: Key) bool {
            return entry.shape == key.shape and entry.property_key.eql(key.property_key);
        }
    };
    const Key = struct {
        shape: *const Object.Shape,
        property_key: PropertyKey,
    };

    state: State(Entry, Key),

    pub const empty: SetPropertyComputed = .{ .state = .empty };

    pub fn set(
        ic: *const SetPropertyComputed,
        agent: *Agent,
        base_object: *Object,
        base_value: Value,
        property_key: PropertyKey,
        value: Value,
    ) Agent.Error!bool {
        if (property_key == .integer_index) return false;
        const key: Key = .{ .shape = base_object.shape, .property_key = property_key };
        const entry = ic.state.get(key) orelse return false;

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
        ic: *SetPropertyComputed,
        base_object: *Object,
        property_key: PropertyKey,
    ) void {
        // Indices go through their own indexed property fast path
        if (property_key == .integer_index) return;

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
            .property_key = property_key,
            .offset = property.offset,
            .type = property.type,
        });
    }
};
