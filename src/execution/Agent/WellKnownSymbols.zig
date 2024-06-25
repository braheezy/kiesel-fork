//! 6.1.5.1 Well-Known Symbols
//! https://tc39.es/ecma262/#sec-well-known-symbols

const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const String = types.String;
const Symbol = types.Symbol;

const Self = @This();

@"@@asyncIterator": Symbol,
@"@@hasInstance": Symbol,
@"@@isConcatSpreadable": Symbol,
@"@@iterator": Symbol,
@"@@match": Symbol,
@"@@matchAll": Symbol,
@"@@replace": Symbol,
@"@@search": Symbol,
@"@@species": Symbol,
@"@@split": Symbol,
@"@@toPrimitive": Symbol,
@"@@toStringTag": Symbol,
@"@@unscopables": Symbol,

pub fn init(agent: *Agent) Self {
    return .{
        .@"@@asyncIterator" = agent.createSymbol(String.fromLiteral("Symbol.asyncIterator")) catch unreachable,
        .@"@@hasInstance" = agent.createSymbol(String.fromLiteral("Symbol.hasInstance")) catch unreachable,
        .@"@@isConcatSpreadable" = agent.createSymbol(String.fromLiteral("Symbol.isConcatSpreadable")) catch unreachable,
        .@"@@iterator" = agent.createSymbol(String.fromLiteral("Symbol.iterator")) catch unreachable,
        .@"@@match" = agent.createSymbol(String.fromLiteral("Symbol.match")) catch unreachable,
        .@"@@matchAll" = agent.createSymbol(String.fromLiteral("Symbol.matchAll")) catch unreachable,
        .@"@@replace" = agent.createSymbol(String.fromLiteral("Symbol.replace")) catch unreachable,
        .@"@@search" = agent.createSymbol(String.fromLiteral("Symbol.search")) catch unreachable,
        .@"@@species" = agent.createSymbol(String.fromLiteral("Symbol.species")) catch unreachable,
        .@"@@split" = agent.createSymbol(String.fromLiteral("Symbol.split")) catch unreachable,
        .@"@@toPrimitive" = agent.createSymbol(String.fromLiteral("Symbol.toPrimitive")) catch unreachable,
        .@"@@toStringTag" = agent.createSymbol(String.fromLiteral("Symbol.toStringTag")) catch unreachable,
        .@"@@unscopables" = agent.createSymbol(String.fromLiteral("Symbol.unscopables")) catch unreachable,
    };
}
