const std = @import("std");

const Value = @import("../Value.zig");
const Object = @import("../Object.zig");
const Symbol = @import("../Symbol.zig");

/// A value that does not prevent the underlying data from being garbage
/// collected. Only supports object and symbol types.
pub const Weak = union(enum) {
    const MaskedObjectPtr = MaskedPtr(*allowzero Object.Data);
    const MaskedSymbolPtr = MaskedPtr(*Symbol.Data);

    object: MaskedObjectPtr,
    symbol: MaskedSymbolPtr,

    pub fn init(value: Value) Weak {
        return switch (value.type()) {
            .object => .{ .object = MaskedObjectPtr.init(value.asObject().data) },
            .symbol => .{ .symbol = MaskedSymbolPtr.init(value.asSymbol().data) },
            else => unreachable,
        };
    }

    /// The returned value may have already been garbage collected.
    pub fn get(weak: Weak) Value {
        return switch (weak) {
            .object => |object_ptr| Value.from(Object{ .data = object_ptr.get() }),
            .symbol => |symbol_ptr| Value.from(Symbol{ .data = symbol_ptr.get() }),
        };
    }

    /// The returned pointer may have already been garbage collected.
    pub fn getPtr(weak: Weak) *anyopaque {
        return switch (weak) {
            .object => |object_ptr| object_ptr.get(),
            .symbol => |symbol_ptr| symbol_ptr.get(),
        };
    }
};

fn MaskedPtr(comptime T: type) type {
    return enum(usize) {
        _,

        pub fn init(ptr: T) @This() {
            return @enumFromInt(~@intFromPtr(ptr));
        }

        pub fn get(masked_ptr: @This()) T {
            const masked_bits: usize = @intFromEnum(masked_ptr);
            return @ptrFromInt(~masked_bits);
        }
    };
}
