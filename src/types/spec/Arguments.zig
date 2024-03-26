const types = @import("../../types.zig");

const Value = types.Value;

const Self = @This();

values: []const Value,

pub fn from(values: []const Value) Self {
    return .{ .values = values };
}

pub fn count(self: Self) usize {
    return self.values.len;
}

pub fn get(self: Self, index: usize) Value {
    return self.getOrNull(index) orelse .undefined;
}

pub fn getOrNull(self: Self, index: usize) ?Value {
    return if (self.count() > index) self.values[index] else null;
}
