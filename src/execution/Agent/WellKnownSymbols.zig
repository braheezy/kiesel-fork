//! 6.1.5.1 Well-Known Symbols
//! https://tc39.es/ecma262/#sec-well-known-symbols

const std = @import("std");

const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const String = types.String;
const Symbol = types.Symbol;

const Self = @This();

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

pub fn init(agent: *Agent) std.mem.Allocator.Error!Self {
    return .{
        .@"%Symbol.asyncIterator%" = try Symbol.create(agent, String.fromLiteral("Symbol.asyncIterator")),
        .@"%Symbol.hasInstance%" = try Symbol.create(agent, String.fromLiteral("Symbol.hasInstance")),
        .@"%Symbol.isConcatSpreadable%" = try Symbol.create(agent, String.fromLiteral("Symbol.isConcatSpreadable")),
        .@"%Symbol.iterator%" = try Symbol.create(agent, String.fromLiteral("Symbol.iterator")),
        .@"%Symbol.match%" = try Symbol.create(agent, String.fromLiteral("Symbol.match")),
        .@"%Symbol.matchAll%" = try Symbol.create(agent, String.fromLiteral("Symbol.matchAll")),
        .@"%Symbol.replace%" = try Symbol.create(agent, String.fromLiteral("Symbol.replace")),
        .@"%Symbol.search%" = try Symbol.create(agent, String.fromLiteral("Symbol.search")),
        .@"%Symbol.species%" = try Symbol.create(agent, String.fromLiteral("Symbol.species")),
        .@"%Symbol.split%" = try Symbol.create(agent, String.fromLiteral("Symbol.split")),
        .@"%Symbol.toPrimitive%" = try Symbol.create(agent, String.fromLiteral("Symbol.toPrimitive")),
        .@"%Symbol.toStringTag%" = try Symbol.create(agent, String.fromLiteral("Symbol.toStringTag")),
        .@"%Symbol.unscopables%" = try Symbol.create(agent, String.fromLiteral("Symbol.unscopables")),
    };
}
