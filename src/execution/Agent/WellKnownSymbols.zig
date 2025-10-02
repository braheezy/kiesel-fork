//! 6.1.5.1 Well-Known Symbols
//! https://tc39.es/ecma262/#sec-well-known-symbols

const types = @import("../../types.zig");

const String = types.String;
const Symbol = types.Symbol;

const WellKnownSymbols = @This();

@"%Symbol.asyncIterator%": *const Symbol,
@"%Symbol.hasInstance%": *const Symbol,
@"%Symbol.isConcatSpreadable%": *const Symbol,
@"%Symbol.iterator%": *const Symbol,
@"%Symbol.match%": *const Symbol,
@"%Symbol.matchAll%": *const Symbol,
@"%Symbol.replace%": *const Symbol,
@"%Symbol.search%": *const Symbol,
@"%Symbol.species%": *const Symbol,
@"%Symbol.split%": *const Symbol,
@"%Symbol.toPrimitive%": *const Symbol,
@"%Symbol.toStringTag%": *const Symbol,
@"%Symbol.unscopables%": *const Symbol,

pub const init: WellKnownSymbols = .{
    .@"%Symbol.asyncIterator%" = Symbol.initComptime(String.fromLiteral("Symbol.asyncIterator")),
    .@"%Symbol.hasInstance%" = Symbol.initComptime(String.fromLiteral("Symbol.hasInstance")),
    .@"%Symbol.isConcatSpreadable%" = Symbol.initComptime(String.fromLiteral("Symbol.isConcatSpreadable")),
    .@"%Symbol.iterator%" = Symbol.initComptime(String.fromLiteral("Symbol.iterator")),
    .@"%Symbol.match%" = Symbol.initComptime(String.fromLiteral("Symbol.match")),
    .@"%Symbol.matchAll%" = Symbol.initComptime(String.fromLiteral("Symbol.matchAll")),
    .@"%Symbol.replace%" = Symbol.initComptime(String.fromLiteral("Symbol.replace")),
    .@"%Symbol.search%" = Symbol.initComptime(String.fromLiteral("Symbol.search")),
    .@"%Symbol.species%" = Symbol.initComptime(String.fromLiteral("Symbol.species")),
    .@"%Symbol.split%" = Symbol.initComptime(String.fromLiteral("Symbol.split")),
    .@"%Symbol.toPrimitive%" = Symbol.initComptime(String.fromLiteral("Symbol.toPrimitive")),
    .@"%Symbol.toStringTag%" = Symbol.initComptime(String.fromLiteral("Symbol.toStringTag")),
    .@"%Symbol.unscopables%" = Symbol.initComptime(String.fromLiteral("Symbol.unscopables")),
};
