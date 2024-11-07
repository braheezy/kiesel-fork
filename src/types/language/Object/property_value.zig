const builtin = @import("builtin");
const std = @import("std");

const execution = @import("../../../execution.zig");
const types = @import("../../../types.zig");

const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;

pub const PropertyValue = union(enum) {
    const LazyIntrinsic = struct {
        realm: *Realm,
        lazyIntrinsicFn: *const fn (*Realm.Intrinsics) std.mem.Allocator.Error!*Object,
    };

    accessor: struct {
        get: ?*Object,
        set: ?*Object,
    },
    value: Value,
    lazy_intrinsic: LazyIntrinsic,

    pub fn fromPropertyDescriptor(property_descriptor: PropertyDescriptor) PropertyValue {
        if (property_descriptor.isAccessorDescriptor()) {
            return .{
                .accessor = .{
                    .get = if (property_descriptor.get.?) |object| object else null,
                    .set = if (property_descriptor.set.?) |object| object else null,
                },
            };
        } else if (property_descriptor.isDataDescriptor()) {
            return .{ .value = property_descriptor.value.? };
        } else unreachable;
    }

    pub fn toPropertyDescriptor(
        self: PropertyValue,
        attributes: Object.Shape.PropertyMetadata.Attributes,
    ) PropertyDescriptor {
        return switch (self) {
            .accessor => |accessor| .{
                .get = if (accessor.get) |object| object else @as(?*Object, null),
                .set = if (accessor.set) |object| object else @as(?*Object, null),
                .enumerable = attributes.enumerable,
                .configurable = attributes.configurable,
            },
            .value => |value| .{
                .value = value,
                .writable = attributes.writable,
                .enumerable = attributes.enumerable,
                .configurable = attributes.configurable,
            },
            .lazy_intrinsic => unreachable,
        };
    }
};

comptime {
    // Let's make sure the size doesn't quietly change
    switch (builtin.target.ptrBitWidth()) {
        // Only some 32-bit platforms have certain bitpacking optimizations applied
        32 => std.debug.assert(@sizeOf(PropertyValue) == 16 or @sizeOf(PropertyValue) == 24),
        64 => std.debug.assert(@sizeOf(PropertyValue) == 24),
        else => unreachable,
    }
}
