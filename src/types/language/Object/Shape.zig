const std = @import("std");

const build_options = @import("build-options");
const types = @import("../../../types.zig");

const Object = types.Object;
const Attributes = Object.PropertyStorage.Attributes;
const PropertyKey = types.PropertyKey;
const PropertyType = Object.PropertyStorage.PropertyType;

const Shape = @This();

// This value is made up, here's what some other engines use:
// - V8:   1536 - https://source.chromium.org/chromium/chromium/src/+/main:v8/src/objects/transitions.h;l=153;drc=0afc9ac9afcaab79fc54299039f4d27abf3a086d
// - SM: 32-100 - https://searchfox.org/mozilla-central/rev/5f2c1701846a54c484d7dd46a291b796f5a67cac/js/src/vm/PropMap.h#625-626
// - JSC:    64 - https://github.com/WebKit/WebKit/blob/55a405ffb60198aee3fa68ee8af63e352c9bcdda/Source/JavaScriptCore/runtime/Structure.h#L214
// - LibJS:  64 - https://github.com/SerenityOS/serenity/blob/7c710805ec4ef80a1dcc8f5b60fa9964fa010aa9/Userland/Libraries/LibJS/Runtime/Object.cpp#L1218
const max_transition_count = 64;

pub const Transition = union(enum) {
    set_prototype: ?*Object,
    set_non_extensible,
    set_is_htmldda,
    set_property: struct { PropertyKey, Attributes, PropertyType },
    delete_property: PropertyKey,

    pub fn hash(self: Transition) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(std.mem.asBytes(&std.meta.activeTag(self)));
        switch (self) {
            .set_prototype => |prototype| {
                hasher.update(std.mem.asBytes(&prototype));
            },
            .set_non_extensible, .set_is_htmldda => {},
            .set_property => |property| {
                const property_key, const attributes, const property_type = property;
                hasher.update(std.mem.asBytes(&property_key.hash()));
                hasher.update(std.mem.asBytes(&attributes));
                hasher.update(std.mem.asBytes(&property_type));
            },
            .delete_property => |property_key| {
                hasher.update(std.mem.asBytes(&property_key.hash()));
            },
        }
        return hasher.final();
    }

    pub fn eql(a: Transition, b: Transition) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
        return switch (a) {
            .set_prototype => return a.set_prototype == b.set_prototype,
            .set_non_extensible, .set_is_htmldda => true,
            .set_property => a.set_property[0].eql(b.set_property[0]) and
                a.set_property[1].eql(b.set_property[1]),
            .delete_property => a.delete_property.eql(b.delete_property),
        };
    }

    pub fn HashMapUnmanaged(comptime V: type) type {
        return std.HashMapUnmanaged(Transition, V, struct {
            pub fn hash(_: @This(), transition: Transition) u64 {
                return transition.hash();
            }

            pub fn eql(_: @This(), a: Transition, b: Transition) bool {
                return a.eql(b);
            }
        }, std.hash_map.default_max_load_percentage);
    }
};

pub const PropertyLookupCacheEntry = struct {
    shape: *const Shape,
    index: PropertyIndex,
};

const PropertyIndex = union(PropertyType) {
    pub const Value = enum(u32) { _ };
    pub const Accessor = enum(u32) { _ };

    value: PropertyIndex.Value,
    accessor: PropertyIndex.Accessor,
};

const PropertyMetadata = struct {
    index: PropertyIndex,
    attributes: Attributes,
};

const State = enum {
    default,
    detached,
    unique,
};

state: State,
transition_count: u8,
next_value_index: PropertyIndex.Value,
next_accessor_index: PropertyIndex.Accessor,
transitions: Transition.HashMapUnmanaged(*Shape),
properties: PropertyKey.ArrayHashMapUnmanaged(PropertyMetadata),

/// [[Prototype]]
prototype: ?*Object,

/// [[Extensible]]
extensible: bool,

/// [[IsHTMLDDA]]
is_htmldda: if (build_options.enable_annex_b) bool else void,

pub fn init(allocator: std.mem.Allocator) std.mem.Allocator.Error!*Shape {
    const self = try allocator.create(Shape);
    self.* = .{
        .state = .default,
        .transition_count = 0,
        .next_value_index = @enumFromInt(0),
        .next_accessor_index = @enumFromInt(0),
        .transitions = .empty,
        .properties = .empty,
        .prototype = null,
        .extensible = true,
        .is_htmldda = if (build_options.enable_annex_b) false,
    };
    return self;
}

pub fn deinit(self: *Shape, allocator: std.mem.Allocator) void {
    self.transitions.deinit(allocator);
    self.properties.deinit(allocator);
    allocator.destroy(self);
}

pub fn detach(self: *const Shape, allocator: std.mem.Allocator) std.mem.Allocator.Error!*Shape {
    std.debug.assert(self.state == .default);
    const shape = try self.clone(allocator, .detached);
    shape.state = .detached;
    return shape;
}

fn clone(self: *const Shape, allocator: std.mem.Allocator, state: State) std.mem.Allocator.Error!*Shape {
    const shape = try allocator.create(Shape);
    errdefer allocator.destroy(shape);
    shape.* = .{
        .state = state,
        .transition_count = self.transition_count,
        .next_value_index = self.next_value_index,
        .next_accessor_index = self.next_accessor_index,
        .transitions = .empty,
        .properties = try self.properties.clone(allocator),
        .prototype = self.prototype,
        .extensible = self.extensible,
        .is_htmldda = self.is_htmldda,
    };
    return shape;
}

fn getOrCreateShape(
    self: *Shape,
    allocator: std.mem.Allocator,
    transition: ?Transition,
) std.mem.Allocator.Error!*Shape {
    switch (self.state) {
        .default, .detached => {
            if (self.transition_count == max_transition_count) {
                return self.clone(allocator, .unique);
            }
            if (transition == null) {
                if (self.state == .default) {
                    return self.clone(allocator, .detached);
                } else {
                    return self;
                }
            }
            const shape_gop = try self.transitions.getOrPut(allocator, transition.?);
            if (shape_gop.found_existing) return shape_gop.value_ptr.*;
            const shape = try self.clone(allocator, self.state);
            shape.transition_count += 1;
            shape_gop.value_ptr.* = shape;
            return shape;
        },
        .unique => {
            std.debug.assert(self.transition_count == max_transition_count);
            std.debug.assert(self.transitions.count() == 0);
            return self;
        },
    }
}

pub fn setPrototype(
    self: *Shape,
    allocator: std.mem.Allocator,
    prototype: ?*Object,
) std.mem.Allocator.Error!*Shape {
    const transition: Transition = .{ .set_prototype = prototype };
    const shape = try self.getOrCreateShape(allocator, transition);
    shape.prototype = prototype;
    return shape;
}

pub fn setPrototypeWithoutTransition(
    self: *Shape,
    allocator: std.mem.Allocator,
    prototype: ?*Object,
) std.mem.Allocator.Error!*Shape {
    const shape = try self.getOrCreateShape(allocator, null);
    shape.prototype = prototype;
    return shape;
}

pub fn setNonExtensible(
    self: *Shape,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!*Shape {
    const transition: Transition = .set_non_extensible;
    const shape = try self.getOrCreateShape(allocator, transition);
    shape.extensible = false;
    return shape;
}

pub fn setNonExtensibleWithoutTransition(
    self: *Shape,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!*Shape {
    const shape = try self.getOrCreateShape(allocator, null);
    shape.extensible = false;
    return shape;
}

pub fn setIsHTMLDDA(
    self: *Shape,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!*Shape {
    const transition: Transition = .set_is_htmldda;
    const shape = try self.getOrCreateShape(allocator, transition);
    shape.is_htmldda = true;
    return shape;
}

pub fn setIsHTMLDDAWithoutTransition(
    self: *Shape,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!*Shape {
    const shape = try self.getOrCreateShape(allocator, null);
    shape.is_htmldda = true;
    return shape;
}

pub fn setProperty(
    self: *Shape,
    allocator: std.mem.Allocator,
    property_key: PropertyKey,
    attributes: Attributes,
    property_type: PropertyType,
) std.mem.Allocator.Error!*Shape {
    const transition: Transition = .{
        .set_property = .{ property_key, attributes, property_type },
    };
    const shape = try self.getOrCreateShape(allocator, transition);
    const property_gop = try shape.properties.getOrPut(allocator, property_key);
    if (property_gop.found_existing) {
        property_gop.value_ptr.*.attributes = attributes;
        if (property_type == property_gop.value_ptr.index) return shape;
    }
    property_gop.value_ptr.* = .{
        .index = switch (property_type) {
            .value => blk: {
                defer shape.next_value_index = @enumFromInt(@intFromEnum(shape.next_value_index) + 1);
                break :blk .{ .value = shape.next_value_index };
            },
            .accessor => blk: {
                defer shape.next_accessor_index = @enumFromInt(@intFromEnum(shape.next_accessor_index) + 1);
                break :blk .{ .accessor = shape.next_accessor_index };
            },
        },
        .attributes = attributes,
    };
    return shape;
}

pub fn setPropertyWithoutTransition(
    self: *Shape,
    allocator: std.mem.Allocator,
    property_key: PropertyKey,
    attributes: Attributes,
    property_type: PropertyType,
) std.mem.Allocator.Error!*Shape {
    const shape = try self.getOrCreateShape(allocator, null);
    const property_gop = try shape.properties.getOrPut(allocator, property_key);
    if (property_gop.found_existing) {
        property_gop.value_ptr.*.attributes = attributes;
        if (property_type == property_gop.value_ptr.index) return shape;
    }
    property_gop.value_ptr.* = .{
        .index = switch (property_type) {
            .value => blk: {
                defer shape.next_value_index = @enumFromInt(@intFromEnum(shape.next_value_index) + 1);
                break :blk .{ .value = shape.next_value_index };
            },
            .accessor => blk: {
                defer shape.next_accessor_index = @enumFromInt(@intFromEnum(shape.next_accessor_index) + 1);
                break :blk .{ .accessor = shape.next_accessor_index };
            },
        },
        .attributes = attributes,
    };
    return shape;
}

pub fn deleteProperty(
    self: *Shape,
    allocator: std.mem.Allocator,
    property_key: PropertyKey,
) std.mem.Allocator.Error!*Shape {
    const transition: Transition = .{ .delete_property = property_key };
    const shape = try self.getOrCreateShape(allocator, transition);
    // No-op if we got the shape from a previous transition
    _ = shape.properties.orderedRemove(property_key);
    return shape;
}

pub fn deletePropertyWithoutTransition(
    self: *Shape,
    allocator: std.mem.Allocator,
    property_key: PropertyKey,
) std.mem.Allocator.Error!*Shape {
    const shape = try self.getOrCreateShape(allocator, null);
    const removed = shape.properties.orderedRemove(property_key);
    std.debug.assert(removed);
    return shape;
}
