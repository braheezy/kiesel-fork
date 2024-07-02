//! 6.1.5.1 Well-Known Symbols
//! https://tc39.es/ecma262/#sec-well-known-symbols

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

pub fn init(agent: *Agent) Self {
    return .{
        .@"%Symbol.asyncIterator%" = agent.createSymbol(String.fromLiteral("Symbol.asyncIterator")) catch unreachable,
        .@"%Symbol.hasInstance%" = agent.createSymbol(String.fromLiteral("Symbol.hasInstance")) catch unreachable,
        .@"%Symbol.isConcatSpreadable%" = agent.createSymbol(String.fromLiteral("Symbol.isConcatSpreadable")) catch unreachable,
        .@"%Symbol.iterator%" = agent.createSymbol(String.fromLiteral("Symbol.iterator")) catch unreachable,
        .@"%Symbol.match%" = agent.createSymbol(String.fromLiteral("Symbol.match")) catch unreachable,
        .@"%Symbol.matchAll%" = agent.createSymbol(String.fromLiteral("Symbol.matchAll")) catch unreachable,
        .@"%Symbol.replace%" = agent.createSymbol(String.fromLiteral("Symbol.replace")) catch unreachable,
        .@"%Symbol.search%" = agent.createSymbol(String.fromLiteral("Symbol.search")) catch unreachable,
        .@"%Symbol.species%" = agent.createSymbol(String.fromLiteral("Symbol.species")) catch unreachable,
        .@"%Symbol.split%" = agent.createSymbol(String.fromLiteral("Symbol.split")) catch unreachable,
        .@"%Symbol.toPrimitive%" = agent.createSymbol(String.fromLiteral("Symbol.toPrimitive")) catch unreachable,
        .@"%Symbol.toStringTag%" = agent.createSymbol(String.fromLiteral("Symbol.toStringTag")) catch unreachable,
        .@"%Symbol.unscopables%" = agent.createSymbol(String.fromLiteral("Symbol.unscopables")) catch unreachable,
    };
}
