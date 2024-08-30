const std = @import("std");

const Allocator = std.mem.Allocator;

const String = @import("../String.zig");

const Builder = @This();

allocator: Allocator,
segments: std.ArrayList(Segment),

pub const Segment = union(enum) {
    string: String,
    char: u8,
    code_unit: u16,
    code_point: u21,

    pub fn isAscii(self: Segment) bool {
        return switch (self) {
            .string => |string| string.data.slice == .ascii,
            .char => true,
            .code_unit => |code_unit| code_unit <= 0x7F,
            .code_point => |code_point| code_point <= 0x7F,
        };
    }
};

pub fn init(allocator: Allocator) Builder {
    return .{
        .allocator = allocator,
        .segments = std.ArrayList(Segment).init(allocator),
    };
}

pub fn deinit(self: Builder) void {
    self.segments.deinit();
}

pub fn appendSegment(self: *Builder, segment: Segment) Allocator.Error!void {
    try self.segments.append(segment);
}

pub fn appendString(self: *Builder, string: String) Allocator.Error!void {
    try self.segments.append(.{ .string = string });
}

pub fn appendChar(self: *Builder, char: u8) Allocator.Error!void {
    try self.segments.append(.{ .char = char });
}

pub fn appendCodeUnit(self: *Builder, code_unit: u16) Allocator.Error!void {
    try self.segments.append(.{ .code_unit = code_unit });
}

pub fn appendCodePoint(self: *Builder, code_point: u21) Allocator.Error!void {
    try self.segments.append(.{ .code_point = code_point });
}

pub fn build(self: Builder) Allocator.Error!String {
    const is_ascii = for (self.segments.items) |segment| {
        if (!segment.isAscii()) break false;
    } else true;
    if (is_ascii) {
        var result = std.ArrayList(u8).init(self.allocator);
        for (self.segments.items) |segment| switch (segment) {
            .string => |string| switch (string.data.slice) {
                .ascii => |ascii| try result.appendSlice(ascii),
                .utf16 => unreachable,
            },
            .char => |char| try result.append(char),
            .code_unit => |code_unit| try result.append(@intCast(code_unit)),
            .code_point => |code_point| try result.append(@intCast(code_point)),
        };
        return String.fromAscii(self.allocator, try result.toOwnedSlice());
    } else {
        var result = std.ArrayList(u16).init(self.allocator);
        for (self.segments.items) |segment| switch (segment) {
            .string => |string| switch (string.data.slice) {
                .ascii => |ascii| for (ascii) |c| try result.append(c),
                .utf16 => |utf16| try result.appendSlice(utf16),
            },
            .char => |char| try result.append(char),
            .code_unit => |code_unit| try result.append(code_unit),
            .code_point => |code_point| {
                if (code_point < 0x10000) {
                    try result.append(@intCast(code_point));
                } else {
                    try result.append(@intCast(0xd800 | ((code_point - 0x10000) >> 10)));
                    try result.append(@intCast(0xdc00 | ((code_point - 0x10000) & 0x3ff)));
                }
            },
        };
        return String.fromUtf16(self.allocator, try result.toOwnedSlice());
    }
}
