const std = @import("std");

const String = @import("../String.zig");

const Builder = @This();

allocator: std.mem.Allocator,
segments: std.ArrayList(Segment),

pub const Segment = union(enum) {
    string: *const String,
    char: u8,
    code_unit: u16,
    code_point: u21,

    pub fn isAscii(self: Segment) bool {
        return switch (self) {
            .string => |string| string.slice == .ascii,
            .char => true,
            .code_unit => |code_unit| code_unit <= 0x7F,
            .code_point => |code_point| code_point <= 0x7F,
        };
    }
};

pub fn init(allocator: std.mem.Allocator) Builder {
    return .{
        .allocator = allocator,
        .segments = .init(allocator),
    };
}

pub fn initCapacity(
    allocator: std.mem.Allocator,
    capacity: usize,
) std.mem.Allocator.Error!Builder {
    var builder = init(allocator);
    try builder.segments.ensureUnusedCapacity(capacity);
    return builder;
}

pub fn deinit(self: Builder) void {
    self.segments.deinit();
}

pub fn appendSegment(self: *Builder, segment: Segment) std.mem.Allocator.Error!void {
    try self.segments.append(segment);
}

pub fn appendSegmentAssumeCapacity(self: *Builder, segment: Segment) void {
    self.segments.appendAssumeCapacity(segment);
}

pub fn appendString(self: *Builder, string: *const String) std.mem.Allocator.Error!void {
    try self.segments.append(.{ .string = string });
}

pub fn appendStringAssumeCapacity(self: *Builder, string: *const String) void {
    self.segments.appendAssumeCapacity(.{ .string = string });
}

pub fn appendChar(self: *Builder, char: u8) std.mem.Allocator.Error!void {
    try self.segments.append(.{ .char = char });
}

pub fn appendCharAssumeCapacity(self: *Builder, char: u8) void {
    self.segments.appendAssumeCapacity(.{ .char = char });
}

pub fn appendCodeUnit(self: *Builder, code_unit: u16) std.mem.Allocator.Error!void {
    try self.segments.append(.{ .code_unit = code_unit });
}

pub fn appendCodeUnitAssumeCapacity(self: *Builder, code_unit: u16) void {
    self.segments.appendAssumeCapacity(.{ .code_unit = code_unit });
}

pub fn appendCodePoint(self: *Builder, code_point: u21) std.mem.Allocator.Error!void {
    try self.segments.append(.{ .code_point = code_point });
}

pub fn appendCodePointAssumeCapacity(self: *Builder, code_point: u21) void {
    self.segments.appendAssumeCapacity(.{ .code_point = code_point });
}

pub fn build(self: Builder) std.mem.Allocator.Error!*const String {
    const is_ascii = for (self.segments.items) |segment| {
        if (!segment.isAscii()) break false;
    } else true;
    if (is_ascii) {
        var result = std.ArrayList(u8).init(self.allocator);
        for (self.segments.items) |segment| switch (segment) {
            .string => |string| switch (string.slice) {
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
            .string => |string| switch (string.slice) {
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
