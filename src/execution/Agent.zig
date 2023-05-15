//! 9.7 Agents
//! https://tc39.es/ecma262/#sec-agents

const gc = @import("gc");
const std = @import("std");

const Allocator = std.mem.Allocator;

const types = @import("../types.zig");

const BigInt = types.BigInt;
const ExecutionContext = @import("ExecutionContext.zig");
const Realm = @import("Realm.zig");
const Symbol = types.Symbol;
const Value = types.Value;

const Self = @This();

allocator: Allocator,
options: Options,
pre_allocated: struct {
    pow_2_63: BigInt.Value,
    pow_2_64: BigInt.Value,
},
exception: ?Value = null,
symbol_id: usize = 0,
well_known_symbols: WellKnownSymbols,
execution_context_stack: std.ArrayList(ExecutionContext),

pub const Options = struct {
    debug: struct {
        print_ast: bool = false,
        print_bytecode: bool = false,
    } = .{},
};

pub const Error = error{
    ExceptionThrown,
    OutOfMemory,
};

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

pub fn init(options: Options) !Self {
    var self = Self{
        .allocator = gc.allocator(),
        .options = options,
        .pre_allocated = undefined,
        .well_known_symbols = undefined,
        .execution_context_stack = undefined,
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
    self.execution_context_stack = std.ArrayList(ExecutionContext).init(self.allocator);
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

/// 5.2.3.2 Throw an Exception
/// https://tc39.es/ecma262/#sec-throw-an-exception
pub fn throwException(
    self: *Self,
    exception_type: ExceptionType,
    message: []const u8,
) error{ExceptionThrown} {
    // TODO: Create an actual error object.
    self.exception = Value.from(
        std.fmt.allocPrint(
            self.allocator,
            "{s}: {s}",
            .{ exception_type.typeName(), message },
        ) catch "InternalError: Out of memory",
    );
    return error.ExceptionThrown;
}

/// https://tc39.es/ecma262/#running-execution-context
pub fn runningExecutionContext(self: Self) *ExecutionContext {
    // At any point in time, there is at most one execution context per agent that is actually
    // executing code. This is known as the agent's running execution context.
    // The running execution context is always the top element of this stack.
    std.debug.assert(self.execution_context_stack.items.len > 0);
    return &self.execution_context_stack.items[self.execution_context_stack.items.len - 1];
}

/// https://tc39.es/ecma262/#current-realm
pub fn currentRealm(self: Self) *Realm {
    // The value of the Realm component of the running execution context is also called the current
    // Realm Record.
    return self.runningExecutionContext().realm;
}

/// 9.4.1 GetActiveScriptOrModule ( )
/// https://tc39.es/ecma262/#sec-getactivescriptormodule
pub fn getActiveScriptOrModule(self: Self) ?*ExecutionContext.ScriptOrModule {
    // 1. If the execution context stack is empty, return null.
    if (self.execution_context_stack.items.len == 0)
        return null;

    // 2. Let ec be the topmost execution context on the execution context stack whose
    //    ScriptOrModule component is not null.
    var execution_context: ?*ExecutionContext = null;
    var i = self.execution_context_stack.items.len;
    while (i > 0) : (i -= 1) {
        execution_context = self.execution_context_stack.items[i - 1];
        if (execution_context.script_or_module != null)
            break;
    }

    // 3. If no such execution context exists, return null. Otherwise, return ec's ScriptOrModule.
    if (execution_context == null)
        return null;
    return execution_context.script_or_module;
}

test "well_known_symbols" {
    const agent = try init(.{});
    const unscopables = agent.well_known_symbols.@"@@unscopables";
    try std.testing.expectEqual(unscopables.id, 12);
    try std.testing.expectEqualStrings(unscopables.description.?, "Symbol.unscopables");
}
