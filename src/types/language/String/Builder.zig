const std = @import("std");

const Allocator = std.mem.Allocator;

const String = @import("../string.zig").String;

const Self = @This();

allocator: Allocator,
segments: std.ArrayList(Segment),

pub const Segment = union(enum) {
    string: String,
    char: u8,
    code_point: u21,
};

pub fn init(allocator: Allocator) Self {
    return .{
        .allocator = allocator,
        .segments = std.ArrayList(Segment).init(allocator),
    };
}

pub fn deinit(self: Self) void {
    self.segments.deinit();
}

pub fn appendSegment(self: *Self, segment: Segment) Allocator.Error!void {
    try self.segments.append(segment);
}

pub fn appendString(self: *Self, string: String) Allocator.Error!void {
    try self.segments.append(.{ .string = string });
}

pub fn appendChar(self: *Self, char: u8) Allocator.Error!void {
    try self.segments.append(.{ .char = char });
}

pub fn appendCodePoint(self: *Self, code_point: u21) Allocator.Error!void {
    try self.segments.append(.{ .code_point = code_point });
}

pub fn build(self: Self) Allocator.Error!String {
    var result = std.ArrayList(u8).init(self.allocator);
    for (self.segments.items) |segment| switch (segment) {
        .string => |string| try result.appendSlice(string.utf8),
        .char => |char| try result.append(char),
        .code_point => |code_point| {
            var out: [4]u8 = undefined;
            const len = std.unicode.utf8Encode(
                code_point,
                &out,
            ) catch |err| switch (err) {
                error.CodepointTooLarge => unreachable,
                error.Utf8CannotEncodeSurrogateHalf => {
                    try result.appendSlice("\u{fffd}");
                    continue;
                },
            };
            try result.appendSlice(out[0..len]);
        },
    };
    return String.from(try result.toOwnedSlice());
}
