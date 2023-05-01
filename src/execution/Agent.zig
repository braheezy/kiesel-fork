//! 9.7 Agents
//! https://tc39.es/ecma262/#sec-agents

const gc = @import("gc");
const std = @import("std");

const Allocator = std.mem.Allocator;

const types = @import("../types.zig");

const BigInt = types.BigInt;
const Symbol = types.Symbol;
const Value = types.Value;

const Self = @This();

pub const Error = error{
    ExceptionThrown,
    OutOfMemory,
};

const ExceptionType = enum {
    // NativeError types
    eval_error,
    range_error,
    reference_error,
    syntax_error,
    type_error,
    uri_error,

    // Non-standard internal error
    internal_error,

    pub fn typeName(self: @This()) []const u8 {
        return switch (self) {
            .eval_error => "EvalError",
            .range_error => "RangeError",
            .reference_error => "ReferenceError",
            .syntax_error => "SyntaxError",
            .type_error => "TypeError",
            .uri_error => "URIError",
            .internal_error => "InternalError",
        };
    }
};

allocator: Allocator,
pre_allocated: struct {
    pow_2_63: BigInt.Value,
    pow_2_64: BigInt.Value,
},
exception: ?Value = null,
symbol_id: usize = 0,
well_known_symbols: WellKnownSymbols,

/// 6.1.5.1 Well-Known Symbols
/// https://tc39.es/ecma262/#sec-well-known-symbols
pub const WellKnownSymbols = struct {
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
};

pub fn init() !Self {
    var self = Self{
        .allocator = gc.allocator(),
        .pre_allocated = undefined,
        .well_known_symbols = undefined,
    };
    self.pre_allocated = .{
        .pow_2_63 = try BigInt.Value.initSet(self.allocator, std.math.pow(u64, 2, 63)),
        .pow_2_64 = try BigInt.Value.initSet(self.allocator, std.math.pow(u128, 2, 64)),
    };
    self.well_known_symbols = WellKnownSymbols{
        .@"@@asyncIterator" = self.createSymbol("Symbol.asyncIterator") catch unreachable,
        .@"@@hasInstance" = self.createSymbol("Symbol.hasInstance") catch unreachable,
        .@"@@isConcatSpreadable" = self.createSymbol("Symbol.isConcatSpreadable") catch unreachable,
        .@"@@iterator" = self.createSymbol("Symbol.iterator") catch unreachable,
        .@"@@match" = self.createSymbol("Symbol.match") catch unreachable,
        .@"@@matchAll" = self.createSymbol("Symbol.matchAll") catch unreachable,
        .@"@@replace" = self.createSymbol("Symbol.replace") catch unreachable,
        .@"@@search" = self.createSymbol("Symbol.search") catch unreachable,
        .@"@@species" = self.createSymbol("Symbol.species") catch unreachable,
        .@"@@split" = self.createSymbol("Symbol.split") catch unreachable,
        .@"@@toPrimitive" = self.createSymbol("Symbol.toPrimitive") catch unreachable,
        .@"@@toStringTag" = self.createSymbol("Symbol.toStringTag") catch unreachable,
        .@"@@unscopables" = self.createSymbol("Symbol.unscopables") catch unreachable,
    };
    return self;
}

pub fn createSymbol(self: *Self, description: ?[]const u8) !Symbol {
    const id = blk: {
        const ov = @addWithOverflow(self.symbol_id, 1);
        if (ov[1] != 0)
            return error.Overflow;
        defer self.symbol_id = ov[0];
        break :blk self.symbol_id;
    };
    return .{ .id = id, .description = description };
}

/// 5.2.3.2 Throw an Exception
/// https://tc39.es/ecma262/#sec-throw-an-exception
pub fn throwException(
    self: *Self,
    exception_type: ExceptionType,
    message: []const u8,
) error{ExceptionThrown} {
    // TODO: Create an actual error object.
    self.exception = Value.fromString(
        std.fmt.allocPrint(
            self.allocator,
            "{s}: {s}",
            .{ exception_type.typeName(), message },
        ) catch "InternalError: Out of memory",
    );
    return error.ExceptionThrown;
}

test "well_known_symbols" {
    const agent = try init();
    const unscopables = agent.well_known_symbols.@"@@unscopables";
    try std.testing.expectEqual(unscopables.id, 12);
    try std.testing.expectEqualStrings(unscopables.description.?, "Symbol.unscopables");
}
