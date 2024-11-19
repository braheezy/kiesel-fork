const std = @import("std");

const types = @import("../../../types.zig");

const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;

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
    set_property: struct { PropertyKey, PropertyMetadata.Attributes },
    delete_property: PropertyKey,

    pub fn hash(self: Transition) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(std.mem.asBytes(&std.meta.activeTag(self)));
        switch (self) {
            .set_prototype => |prototype| {
                hasher.update(std.mem.asBytes(&prototype));
            },
            .set_non_extensible => {},
            .set_property => |property_key_and_attributes| {
                const property_key, const attributes = property_key_and_attributes;
                hasher.update(std.mem.asBytes(&property_key.hash()));
                hasher.update(std.mem.asBytes(&attributes));
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
            .set_non_extensible => true,
            .set_property => a.set_property[0].eql(b.set_property[0]) and
                a.set_property[1].eql(b.set_property[1]),
            .delete_property => a.delete_property.eql(b.delete_property),
        };
    }

    pub fn HashMap(comptime V: type) type {
        return std.HashMap(Transition, V, struct {
            pub fn hash(_: @This(), transition: Transition) u64 {
                return transition.hash();
            }

            pub fn eql(_: @This(), a: Transition, b: Transition) bool {
                return a.eql(b);
            }
        }, std.hash_map.default_max_load_percentage);
    }
};

pub const PropertyMetadata = struct {
    pub const Attributes = packed struct(u3) {
        writable: bool,
        enumerable: bool,
        configurable: bool,

        pub fn eql(a: Attributes, b: Attributes) bool {
            return a.writable == b.writable and
                a.enumerable == b.enumerable and
                a.configurable == b.configurable;
        }

        pub fn fromPropertyDescriptor(property_desciptor: PropertyDescriptor) Attributes {
            return .{
                .writable = property_desciptor.writable orelse false,
                .enumerable = property_desciptor.enumerable orelse false,
                .configurable = property_desciptor.configurable orelse false,
            };
        }
    };

    index: usize,
    attributes: Attributes,
};

pub const PropertyLookupCacheEntry = struct {
    shape: *const Shape,
    index: usize,
};

const State = enum {
    default,
    detached,
    unique,
};

state: State,
transition_count: u8,
next_index: usize,
transitions: Transition.HashMap(*Shape),
properties: PropertyKey.ArrayHashMap(PropertyMetadata),

/// [[Prototype]]
prototype: ?*Object,

/// [[Extensible]]
extensible: bool,

pub fn init(allocator: std.mem.Allocator) std.mem.Allocator.Error!*Shape {
    const self = try allocator.create(Shape);
    self.* = .{
        .state = .default,
        .transition_count = 0,
        .next_index = 0,
        .transitions = .init(allocator),
        .properties = .init(allocator),
        .prototype = null,
        .extensible = true,
    };
    return self;
}

pub fn deinit(self: *Shape, allocator: std.mem.Allocator) void {
    self.transitions.deinit();
    self.properties.deinit();
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
        .next_index = self.next_index,
        .transitions = .init(allocator),
        .properties = try self.properties.clone(),
        .prototype = self.prototype,
        .extensible = self.extensible,
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
            const shape_gop = try self.transitions.getOrPut(transition.?);
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

pub fn setProperty(
    self: *Shape,
    allocator: std.mem.Allocator,
    property_key: PropertyKey,
    attributes: PropertyMetadata.Attributes,
) std.mem.Allocator.Error!*Shape {
    const transition: Transition = .{ .set_property = .{ property_key, attributes } };
    const shape = try self.getOrCreateShape(allocator, transition);
    const property_gop = try shape.properties.getOrPut(property_key);
    if (property_gop.found_existing) {
        property_gop.value_ptr.*.attributes = attributes;
    } else {
        property_gop.value_ptr.* = .{
            .index = shape.next_index,
            .attributes = attributes,
        };
        shape.next_index += 1;
    }
    return shape;
}

pub fn setPropertyWithoutTransition(
    self: *Shape,
    allocator: std.mem.Allocator,
    property_key: PropertyKey,
    attributes: PropertyMetadata.Attributes,
) std.mem.Allocator.Error!*Shape {
    const shape = try self.getOrCreateShape(allocator, null);
    const property_gop = try shape.properties.getOrPut(property_key);
    if (property_gop.found_existing) {
        property_gop.value_ptr.*.attributes = attributes;
    } else {
        property_gop.value_ptr.* = .{
            .index = shape.next_index,
            .attributes = attributes,
        };
        shape.next_index += 1;
    }
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
