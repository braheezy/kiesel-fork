//! 6.1.5.1 Well-Known Symbols
//! https://tc39.es/ecma262/#sec-well-known-symbols

const std = @import("std");

const types = @import("../../types.zig");

const String = types.String;
const Symbol = types.Symbol;

const WellKnownSymbols = @This();

@"%Symbol.asyncIterator%": Symbol,
@"%Symbol.hasInstance%": Symbol,
@"%Symbol.isConcatSpreadable%": Symbol,
@"%Symbol.iterator%": Symbol,
@"%Symbol.match%": Symbol,
@"%Symbol.matchAll%": Symbol,
@"%Symbol.replace%": Symbol,
@"%Symbol.search%": Symbol,
@"%Symbol.species%": Symbol,
@"%Symbol.split%": Symbol,
@"%Symbol.toPrimitive%": Symbol,
@"%Symbol.toStringTag%": Symbol,
@"%Symbol.unscopables%": Symbol,

pub fn init(allocator: std.mem.Allocator) std.mem.Allocator.Error!WellKnownSymbols {
    return .{
        .@"%Symbol.asyncIterator%" = try Symbol.init(allocator, String.fromLiteral("Symbol.asyncIterator")),
        .@"%Symbol.hasInstance%" = try Symbol.init(allocator, String.fromLiteral("Symbol.hasInstance")),
        .@"%Symbol.isConcatSpreadable%" = try Symbol.init(allocator, String.fromLiteral("Symbol.isConcatSpreadable")),
        .@"%Symbol.iterator%" = try Symbol.init(allocator, String.fromLiteral("Symbol.iterator")),
        .@"%Symbol.match%" = try Symbol.init(allocator, String.fromLiteral("Symbol.match")),
        .@"%Symbol.matchAll%" = try Symbol.init(allocator, String.fromLiteral("Symbol.matchAll")),
        .@"%Symbol.replace%" = try Symbol.init(allocator, String.fromLiteral("Symbol.replace")),
        .@"%Symbol.search%" = try Symbol.init(allocator, String.fromLiteral("Symbol.search")),
        .@"%Symbol.species%" = try Symbol.init(allocator, String.fromLiteral("Symbol.species")),
        .@"%Symbol.split%" = try Symbol.init(allocator, String.fromLiteral("Symbol.split")),
        .@"%Symbol.toPrimitive%" = try Symbol.init(allocator, String.fromLiteral("Symbol.toPrimitive")),
        .@"%Symbol.toStringTag%" = try Symbol.init(allocator, String.fromLiteral("Symbol.toStringTag")),
        .@"%Symbol.unscopables%" = try Symbol.init(allocator, String.fromLiteral("Symbol.unscopables")),
    };
}

pub fn deinit(self: WellKnownSymbols, allocator: std.mem.Allocator) void {
    self.@"%Symbol.asyncIterator%".deinit(allocator);
    self.@"%Symbol.hasInstance%".deinit(allocator);
    self.@"%Symbol.isConcatSpreadable%".deinit(allocator);
    self.@"%Symbol.iterator%".deinit(allocator);
    self.@"%Symbol.match%".deinit(allocator);
    self.@"%Symbol.matchAll%".deinit(allocator);
    self.@"%Symbol.replace%".deinit(allocator);
    self.@"%Symbol.search%".deinit(allocator);
    self.@"%Symbol.species%".deinit(allocator);
    self.@"%Symbol.split%".deinit(allocator);
    self.@"%Symbol.toPrimitive%".deinit(allocator);
    self.@"%Symbol.toStringTag%".deinit(allocator);
    self.@"%Symbol.unscopables%".deinit(allocator);
}
